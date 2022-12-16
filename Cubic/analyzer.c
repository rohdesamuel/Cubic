#include "analyzer.h"
#include "ast.h"
#include "memory.h"
#include "symbol_table.h"
#include "object.h"

#include <stdarg.h>
#include <string.h>

typedef void (*AnalysisFn)(AstNode_*);

typedef struct AnalysisRule_ {
  AnalysisFn fn;
} AnalysisRule_;

static AnalysisRule_* get_rule(int type);
static void error(Analyzer_* analyzer, AstNode_* node, const char* message, ...);

Analyzer_* analyzer_;

static void do_analysis(AstNode_* node) {
  get_rule(node->cls)->fn(node);
}

void analyzer_init(Analyzer_* analyzer) {
  memset(analyzer, 0, sizeof(Analyzer_));
  pageallocator_init(&analyzer->allocator, 1 << 14);

  analyzer->frame = frame_root((MemoryAllocator_*)&analyzer->allocator);
  analyzer->scope = analyzer->frame->scope; //scope_create(analyzer->frame, NULL, (MemoryAllocator_*)&analyzer->allocator);
}

void analyze(Analyzer_* analyzer, struct AstProgram_* ast) {
  analyzer_ = analyzer;
  do_analysis((AstNode_*)ast);
}

void analyzer_clear(Analyzer_* analyzer) {
  scope_destroy(&analyzer->scope);
  frame_destroy(&analyzer->frame);
  pageallocator_deinit(&analyzer->allocator);
  analyzer_ = NULL;
}

void program_analysis(AstNode_* node) {
  AstProgram_* program = (AstProgram_*)node;
  do_analysis((AstNode_*)program->block);
}

void block_analysis(AstNode_* node) {
  AstBlock_* block = (AstBlock_*)node;
  for (AstListNode_* n = block->statements.head; n != NULL; n = n->next) {
    do_analysis(n->node);
  }

  if ((int64_t)node->scope->frame->var_count > UINT16_MAX) {
    error(analyzer_, node, "Number of local variables exceeded max of 64K.");
  }
}

void print_analysis(AstNode_* n) {
  AstPrintStmt_* stmt = (AstPrintStmt_*)n;
  do_analysis((AstNode_*)stmt->expr);
}

void unary_analysis(AstNode_* n) {
  AstUnaryExp_* exp = (AstUnaryExp_*)n;
  
  do_analysis(exp->expr);

  exp->base.type = AS_EXPR(exp->expr)->type;

  switch (exp->op) {
    case TK_TILDE:
      if (!ISA_TY_NUMBER(exp->base.type)) {
        error(analyzer_, n, "expression does not have a number type.");
      }
      break;

    case TK_NOT:
      if (!IS_TY_BOOL(exp->base.type)) {
        error(analyzer_, n, "expression does not have a boolean type.");
      }
      break;

    case TK_MINUS:
      if (!ISA_TY_NUMBER(exp->base.type)) {
        error(analyzer_, n, "expression does not have a number type.");
      }
      break;    
  }
}

void binary_analysis(AstNode_* n) {
  AstBinaryExp_* exp = (AstBinaryExp_*)n;

  do_analysis(exp->left);
  do_analysis(exp->right);

  if (IS_TY_UNKNOWN(AS_EXPR(exp->left)->type)) {
    error(analyzer_, n, "left-hand expression type could not be deduced.");
    return;
  }

  if (IS_TY_UNKNOWN(AS_EXPR(exp->right)->type)) {
    error(analyzer_, n, "right-hand expression type could not be deduced.");
    return;
  }

  if (!type_equal(AS_EXPR(exp->left)->type, AS_EXPR(exp->right)->type)) {
    error(analyzer_, n, "left and right expressions are not the same type.");
    return;
  }

  Type_ expected_type = UNKNOWN_TY;
  switch (exp->op) {
    case TK_AND: 
    case TK_OR:
    case TK_XOR:
      exp->base.type = BOOL_TY;
      expected_type = BOOL_TY;
      break;

    case TK_GT:
    case TK_GTE:
    case TK_LT:
    case TK_LTE:
    case TK_EQUAL_EQUAL:
    case TK_BANG_EQUAL:
      exp->base.type = BOOL_TY;
      break;

    case TK_AMPERSAND:
    case TK_PIPE:
    case TK_HAT:
      exp->base.type = AS_EXPR(exp->left)->type;
      if (!ISA_TY_INTEGER(exp->base.type)) {
        error(analyzer_, n, "expected the expression type to be a number.");
      }
      break;

    case TK_PLUS:
    case TK_MINUS:
    case TK_STAR:
    case TK_SLASH:
      exp->base.type = AS_EXPR(exp->left)->type;
      if (!ISA_TY_NUMBER(exp->base.type)) {
        error(analyzer_, n, "expected the expression type to be a number.");
      }
      break;

    // case TK_DOUBLE_SLASH:
    case TK_PERCENT:
    case TK_LSHIFT:
    case TK_RSHIFT:
      exp->base.type = AS_EXPR(exp->left)->type;
      expected_type = INT_TY;
      expected_type.kind = KIND_VAL;
      break;

    default:
      error(analyzer_, n, "unimplemented token type.");
      break;
  }

  if (!IS_TY_UNKNOWN(expected_type)) {
    if ((AS_EXPR(exp->left)->type.ty != expected_type.ty)) {
      error(analyzer_, n, "left-hand expression does not have expected type of %s.", valuetype_str(expected_type));
    }

    if ((AS_EXPR(exp->right)->type.ty != expected_type.ty)) {
      error(analyzer_, n, "right-hand expression does not have expected type of %s.", valuetype_str(expected_type));
    }
  }
}

void primary_analysis(AstNode_* n) {
  AstPrimaryExp_* primary = (AstPrimaryExp_*)n;
  primary->base.type = primary->value.type;
}

void return_analysis(AstNode_* n) {
  AstReturnStmt_* stmt = (AstReturnStmt_*)n;
  Frame_* frame = stmt->base.scope->frame;

  do_analysis(stmt->expr);
  if (!type_equal(AS_EXPR(stmt->expr)->type, frame->fn_symbol->fn.return_type)) {
    error(analyzer_, n, "Return statement type does not match function type.");
  }
}

void if_analysis(AstNode_* n) {
  AstIfStmt_* stmt = (AstIfStmt_*)n;

  do_analysis(stmt->condition_expr);
  if (!IS_TY_BOOL(AS_EXPR(stmt->condition_expr)->type)) {
    error(analyzer_, n, "if expression must have a boolean type.");
  }

  do_analysis(stmt->if_stmt);
  if (stmt->else_stmt) {
    do_analysis(stmt->else_stmt);
  }

  for (AstListNode_* n = stmt->elif_exprs.head; n != NULL; n = n->next) {
    do_analysis(n->node);
    if (!IS_TY_BOOL(AS_EXPR(n->node)->type)) {
      error(analyzer_, n->node, "elif expression must have a boolean type.");
    }
  }
  for (AstListNode_* n = stmt->elif_stmts.head; n != NULL; n = n->next) {
    do_analysis(n->node);
  }
}

void assert_analysis(AstNode_* n) {
  AstAssertStmt_* stmt = (AstAssertStmt_*)n;
  do_analysis(stmt->expr);

  if (!IS_TY_BOOL(AS_EXPR(stmt->expr)->type)) {
    error(analyzer_, n, "assert expression must have a boolean type.");
  }
}

void var_decl_analysis(AstNode_* n) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)n;

  if (stmt->expr) {
    do_analysis((AstNode_*)stmt->expr);
  }

  if (IS_TY_UNKNOWN(stmt->type)) {
    assertf(stmt->expr, "type deduced variable must have an expression");        
    stmt->type = AS_EXPR(stmt->expr)->type;    
  }

  // TODO: implement tuples (and others) for variable declarations.
  VarSymbol_* var = scope_var(n->scope, &stmt->name);
  var->type = stmt->type;

  if (stmt->expr && !type_iscoercible(AS_EXPR(stmt->expr)->type, stmt->type)) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }

  //Symbol_* s = symboltable_addvar(scope->table, &stmt->name, scope, stmt->type, KIND_VAL);
  //scope->frame->var_count += 1;
  //scope->frame->max_stack_offset = max(scope->frame->max_stack_offset, scope->offset + scope->table->vars.count);
}

void var_expr_analysis(AstNode_* n) {
  AstVarExpr_* expr = (AstVarExpr_*)n;
  do_analysis((AstNode_*)expr->expr);
  expr->base.type = expr->expr->type;
}

void id_expr_analysis(AstNode_* n) {
  AstIdExpr_* expr = (AstIdExpr_*)n;
  Symbol_* var = scope_find(n->scope, &expr->name);
  if (var) {
    switch (var->type) {
      case SYMBOL_TYPE_STRUCT: break;
      case SYMBOL_TYPE_FN: expr->base.type = var->fn.return_type; break;
      case SYMBOL_TYPE_VAR: expr->base.type = var->var.type; break;
    }
  } else {
    error(analyzer_, n, "Could not find variable %.*s", expr->name.length, expr->name.start);
    expr->base.type = UNKNOWN_TY;    
  }
}

void assignment_expr_analysis(AstNode_* n) {
  AstAssignmentExpr_* expr = (AstAssignmentExpr_*)n;
  
  // TODO: allow for deconstructing tuples in assignments (and other types).
  do_analysis(expr->left);
  do_analysis(expr->right);

  if (expr->left->cls != AST_CLS(AstVarExpr_)) {
    error(analyzer_, n, "Left-hand side of assignment cannot be assigned to.");
    return;
  }

  AstVarExpr_* lvalue_expr = (AstVarExpr_*)expr->right;
  AstExpr_* rvalue_expr = (AstExpr_*)expr->right;
  if (!type_equal(lvalue_expr->base.type, rvalue_expr->type)) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }
  expr->base.type = lvalue_expr->base.type;
}

void while_stmt_analysis(AstNode_* n) {
  AstWhileStmt_* stmt = (AstWhileStmt_*)n;
  do_analysis(stmt->condition_expr);
  do_analysis(stmt->block_stmt);

  if (!IS_TY_BOOL(AS_EXPR(stmt->condition_expr)->type)) {
    error(analyzer_, n, "while loop conditional must be a bool.");
  }
}

void expression_statement_analysis(AstNode_* n) {
  AstExpressionStmt_* stmt = (AstExpressionStmt_*)n;
  do_analysis(stmt->expr);
}

void function_def_analysis(AstNode_* n) {
  AstFunctionDef_* def = (AstFunctionDef_*)n;
  FunctionSymbol_* s = scope_fn(n->scope, &def->name);
  s->return_type = def->body->return_type;

  do_analysis((AstNode_*)def->body);  
}

void function_body_analysis(AstNode_* n) {
  AstFunctionBody_* body = (AstFunctionBody_*)n;
  
  for (AstListNode_* n = body->function_params.head; n != NULL; n = n->next) {
    do_analysis(n->node);
  }

  do_analysis(body->stmt);
}

void function_param_analysis(AstNode_* n) {
  AstFunctionParam_* param = (AstFunctionParam_*)n;
  
  // TODO: implement parameter type inference.
  if (IS_TY_UNKNOWN(param->type) && !param->opt_expr) {
    error(analyzer_, n, "Parameter type inferrece is unimplemented.");
  }
  
  if (param->opt_expr) {
    do_analysis(param->opt_expr);
    if (!type_iscoercible(param->type, AS_EXPR(param->opt_expr)->type)) {
      error(analyzer_, n, "Parameter type is not coercible.");
    }

    if (IS_TY_UNKNOWN(param->type)) {
      param->type = AS_EXPR(param->opt_expr)->type;
    }
  }

  List_* params = &n->scope->frame->fn_symbol->fn.params;
  for (ListNode_* n = params->head; n != NULL; n = n->next) {
    Symbol_* s = list_val(n, Symbol_*);
    s->var.type = param->type;
  }  
}

void function_call_analysis(AstNode_* n) {
  AstFunctionCall_* call = (AstFunctionCall_*)n;
  do_analysis(call->prefix);
  do_analysis(call->args);

  call->base.type = AS_EXPR(call->prefix)->type;
}

void function_args_analysis(AstNode_* n) {
  AstFunctionArgs_* args = (AstFunctionArgs_*)n;
}

void noop_analysis(AstNode_* n) {}

AnalysisRule_ analysis_rules[] = {
  [AST_CLS(AstProgram_)]        = {program_analysis},
  [AST_CLS(AstBlock_)]          = {block_analysis},
  [AST_CLS(AstPrintStmt_)]      = {print_analysis},
  [AST_CLS(AstUnaryExp_)]       = {unary_analysis},
  [AST_CLS(AstBinaryExp_)]      = {binary_analysis},
  [AST_CLS(AstPrimaryExp_)]     = {primary_analysis},
  [AST_CLS(AstReturnStmt_)]     = {return_analysis},
  [AST_CLS(AstIfStmt_)]         = {if_analysis},
  [AST_CLS(AstAssertStmt_)]     = {assert_analysis},
  [AST_CLS(AstVarDeclStmt_)]    = {var_decl_analysis},
  [AST_CLS(AstVarExpr_)]        = {var_expr_analysis},
  [AST_CLS(AstIdExpr_)]         = {id_expr_analysis},
  [AST_CLS(AstAssignmentExpr_)] = {assignment_expr_analysis},
  [AST_CLS(AstWhileStmt_)]      = {while_stmt_analysis},
  [AST_CLS(AstFunctionDef_)]    = {function_def_analysis},
  [AST_CLS(AstFunctionBody_)]   = {function_body_analysis},
  [AST_CLS(AstFunctionParam_)]  = {function_param_analysis},
  [AST_CLS(AstFunctionCall_)]   = {function_call_analysis},
  [AST_CLS(AstFunctionArgs_)]   = {function_args_analysis},
  [AST_CLS(AstExpressionStmt_)] = {expression_statement_analysis},
  [AST_CLS(AstNoopStmt_)]       = {noop_analysis},
};

// Static assert to make sure that all node types are accounted for.
STATIC_ASSERT(
  sizeof(analysis_rules) / sizeof(AnalysisRule_) == __AST_NODE_COUNT__,
  CHECK_ANALYSIS_COUNT);

static AnalysisRule_* get_rule(int type) {
  AnalysisRule_* ret = &analysis_rules[type];  
  assertf(ret->fn, "Could not find analysis rule for AST class: %d", type);
  return ret;
}

static void error(Analyzer_* analyzer, AstNode_* node, const char* message, ...) {
  if (analyzer->panic_mode) return;
  analyzer->panic_mode = true;
  analyzer->had_error = true;

  va_list args;
  va_start(args, message);
  fprintf(stderr, "[line %d] Error: ", node->line);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
}