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
  pageallocator_init(&analyzer->allocator, (size_t)1 << 14);

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

void stmt_analysis(AstNode_* node) {
  AstStmt_* stmt = (AstStmt_*)node;
  do_analysis(stmt->stmt);
  do_analysis(stmt->cleanup);
}

void expr_analysis(AstNode_* node) {
  AstExpr_* expr = (AstExpr_*)node;
  AS_EXPR(expr->expr)->top_meta = expr->top_meta;
  do_analysis((AstNode_*)expr->expr);
  expr->meta = AS_EXPR(expr->expr)->meta;
}

void print_analysis(AstNode_* n) {
  AstPrintStmt_* stmt = (AstPrintStmt_*)n;
  stmt->expr->top_meta.type.kind = KIND_VAL;
  do_analysis((AstNode_*)stmt->expr);
}

void unary_analysis(AstNode_* n) {
  AstUnaryExp_* expr = (AstUnaryExp_*)n;
  
  expr->expr->top_meta = expr->base.top_meta;
  do_analysis((AstNode_*)expr->expr);
  expr->base.meta = AS_EXPR(expr->expr)->meta;

  switch (expr->op) {
    case TK_TILDE:
      if (!ISA_TY_NUMBER(expr->base.meta.type)) {
        error(analyzer_, n, "expression does not have a number type.");
      }
      break;

    case TK_NOT:
      if (!IS_TY_BOOL(expr->base.meta.type)) {
        error(analyzer_, n, "expression does not have a boolean type.");
      }
      break;

    case TK_MINUS:
      if (!ISA_TY_NUMBER(expr->base.meta.type)) {
        error(analyzer_, n, "expression does not have a number type.");
      }
      break;

    case TK_AMPERSAND:
    {
      SemanticInfo_ meta = expr->base.meta;
      if (meta.type.kind == KIND_UNKNOWN ||
          meta.type.kind == KIND_TMP ||
          meta.type.kind == KIND_REF ||
          meta.type.kind == KIND_WEAK_REF) {
        error(analyzer_, n, "expression is not assignable");
      }

      if (meta.type.kind == KIND_VAL || meta.type.kind == KIND_STATIC) {
        expr->base.meta.type.kind = KIND_WEAK_REF;
      } else {
        expr->base.meta.type.kind = KIND_REF;
      }
      break;
    }
    default:
      error(analyzer_, n, "Unknown unary operator");
      break;
  }
}

void binary_analysis(AstNode_* n) {
  AstBinaryExp_* expr = (AstBinaryExp_*)n;
  expr->left->top_meta = expr->base.top_meta;
  expr->right->top_meta = expr->base.top_meta;

  do_analysis((AstNode_*)expr->left);
  do_analysis((AstNode_*)expr->right);

  SemanticInfo_ lmeta = AS_EXPR(expr->left)->meta;
  SemanticInfo_ rmeta = AS_EXPR(expr->right)->meta;

  if (IS_TY_UNKNOWN(lmeta.type)) {
    error(analyzer_, n, "left-hand expression type could not be deduced.");
    return;
  }

  if (IS_TY_UNKNOWN(rmeta.type)) {
    error(analyzer_, n, "right-hand expression type could not be deduced.");
    return;
  }

  if (!type_iscoercible(rmeta.type, lmeta.type)) {
    error(analyzer_, n, "right-hand expression cannot be coerced to left-hand expression.");
  }

  Type_ expected_type = UNKNOWN_TY;
  switch (expr->op) {
    case TK_AND: 
    case TK_OR:
    case TK_XOR:
      expr->base.meta.type = BOOL_TY;
      expected_type = BOOL_TY;
      break;

    case TK_GT:
    case TK_GTE:
    case TK_LT:
    case TK_LTE:
    case TK_EQUAL_EQUAL:
    case TK_BANG_EQUAL:
      expr->base.meta.type = BOOL_TY;
      break;

    case TK_AMPERSAND:
    case TK_PIPE:
    case TK_HAT:
      expr->base.meta = lmeta;
      if (!ISA_TY_INTEGER(expr->base.meta.type)) {
        error(analyzer_, n, "expected the expression type to be a number.");
      }
      break;

    case TK_PLUS:
    case TK_MINUS:
    case TK_STAR:
    case TK_SLASH:
      expr->base.meta = lmeta;
      if (!ISA_TY_NUMBER(expr->base.meta.type) && !IS_TY_STRING(expr->base.meta.type)) {
        error(analyzer_, n, "expected the expression type to be a number or a string.");
      }
      break;

    // case TK_DOUBLE_SLASH:
    case TK_PERCENT:
    case TK_LSHIFT:
    case TK_RSHIFT:
      expr->base.meta = lmeta;
      expected_type = INT_TY;
      expected_type.kind = KIND_VAL;
      break;

    default:
      error(analyzer_, n, "unimplemented token type.");
      break;
  }

  if (!IS_TY_UNKNOWN(expected_type)) {
    if (lmeta.type.ty != expected_type.ty) {
      error(analyzer_, n, "left-hand expression does not have expected type of %s.", valuetype_str(expected_type));
    }

    if (rmeta.type.ty != expected_type.ty) {
      error(analyzer_, n, "right-hand expression does not have expected type of %s.", valuetype_str(expected_type));
    }
  }

  expr->base.meta.type.kind = KIND_TMP;
}

void primary_analysis(AstNode_* n) {}

void return_analysis(AstNode_* n) {
  AstReturnStmt_* stmt = (AstReturnStmt_*)n;
  Frame_* frame = stmt->base.scope->frame;
  stmt->expr->top_meta.type = frame->fn_symbol->fn.return_type;

  do_analysis((AstNode_*)stmt->expr);
  if (AS_EXPR(stmt->expr)->meta.type.ty != frame->fn_symbol->fn.return_type.ty) {
    error(analyzer_, n, "Return statement type does not match function type.");
  }
}

void if_analysis(AstNode_* n) {
  AstIfStmt_* stmt = (AstIfStmt_*)n;
  stmt->condition_expr->top_meta.type = (Type_){
    .ty = VAL_BOOL,
    .kind = KIND_VAL,
    .obj = OBJ_TYPE_UNKNOWN
  };

  do_analysis((AstNode_*)stmt->condition_expr);
  if (!IS_TY_BOOL(AS_EXPR(stmt->condition_expr)->meta.type)) {
    error(analyzer_, n, "if expression must have a boolean type.");
  }

  do_analysis(stmt->if_stmt);
  if (stmt->else_stmt) {
    do_analysis(stmt->else_stmt);
  }

  for (AstListNode_* n = stmt->elif_exprs.head; n != NULL; n = n->next) {
    do_analysis(n->node);
    if (!IS_TY_BOOL(AS_EXPR(n->node)->meta.type)) {
      error(analyzer_, n->node, "elif expression must have a boolean type.");
    }
  }
  for (AstListNode_* n = stmt->elif_stmts.head; n != NULL; n = n->next) {
    do_analysis(n->node);
  }
}

void assert_analysis(AstNode_* n) {
  AstAssertStmt_* stmt = (AstAssertStmt_*)n;
  stmt->expr->top_meta.type = (Type_){
    .ty = VAL_BOOL,
    .kind = KIND_VAL,
    .obj = OBJ_TYPE_UNKNOWN
  };
  do_analysis((AstNode_*)stmt->expr);

  if (!IS_TY_BOOL(AS_EXPR(stmt->expr)->meta.type)) {
    error(analyzer_, n, "assert expression must have a boolean type.");
  }
}

void var_decl_analysis(AstNode_* n) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)n;
  if (!IS_TY_UNKNOWN(stmt->meta.type)) {
    stmt->expr->top_meta = stmt->meta;
  }

  if (stmt->expr) {
    do_analysis((AstNode_*)stmt->expr);
  }

  if (IS_TY_UNKNOWN(stmt->meta.type)) {
    assertf(stmt->expr, "type deduced variable must have an expression");
    stmt->meta = AS_EXPR(stmt->expr)->meta;
  }

  // The type will be KIND_TMP if created from a PRIMARY_EXP. Only change the kind for
  // non-pointer/reference types.
  if (stmt->meta.type.kind == KIND_UNKNOWN || stmt->meta.type.kind == KIND_TMP) {
    if (stmt->meta.type.ty == VAL_OBJ) {
      stmt->meta.type.kind = KIND_REF;
    } else {
      stmt->meta.type.kind = KIND_VAL;
    }
  }

  // TODO: implement tuples (and others) for variable declarations.
  VarSymbol_* var = scope_var(n->scope, &stmt->name);  
  var->meta = stmt->meta;
  
  if (stmt->expr && !type_iscoercible(AS_EXPR(stmt->expr)->meta.type, stmt->meta.type)) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }
}

void var_expr_analysis(AstNode_* n) {
  AstVarExpr_* expr = (AstVarExpr_*)n;
  expr->expr->top_meta = expr->base.top_meta;
  do_analysis((AstNode_*)expr->expr);
  expr->base.meta = expr->expr->meta;
}

void id_expr_analysis(AstNode_* n) {
  AstIdExpr_* expr = (AstIdExpr_*)n;
  Symbol_* sym = scope_find(n->scope, &expr->name);
  if (sym) {
    switch (sym->type) {
      case SYMBOL_TYPE_STRUCT: break;
      case SYMBOL_TYPE_FN:
        expr->base.meta.type = sym->fn.return_type;
        expr->base.meta.sym = sym;
        break;
      case SYMBOL_TYPE_VAR:
        expr->base.meta.type = sym->var.meta.type;
        expr->base.meta.sym = sym;
        break;

      case SYMBOL_TYPE_CLOSURE:
        expr->base.meta.type = sym->closure.fn->fn.return_type;
        expr->base.meta.sym = sym;
        break;
      default:
        error(analyzer_, n, "Unhandled symbol type: %d", sym->type);
        break;
    }
  } else {
    error(analyzer_, n, "Could not find variable '%.*s'", expr->name.length, expr->name.start);
    expr->base.meta.type = UNKNOWN_TY;
  }
}

void assignment_expr_analysis(AstNode_* n) {
  AstAssignmentExpr_* expr = (AstAssignmentExpr_*)n;
  

  // TODO: allow for deconstructing tuples in assignments (and other types).
  expr->right->top_meta = expr->base.top_meta;
  do_analysis((AstNode_*)expr->left);
  do_analysis((AstNode_*)expr->right);

  if (expr->left->cls != AST_CLS(AstVarExpr_)) {
    error(analyzer_, n, "Left-hand side of assignment cannot be assigned to.");
    return;
  }

  AstVarExpr_* lvalue_expr = (AstVarExpr_*)expr->right;
  AstExpr_* rvalue_expr = (AstExpr_*)expr->right;
  if (!type_equal(lvalue_expr->base.meta.type, rvalue_expr->meta.type)) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }
  expr->base.meta = lvalue_expr->base.meta;
}

void while_stmt_analysis(AstNode_* n) {
  AstWhileStmt_* stmt = (AstWhileStmt_*)n;
  stmt->condition_expr->top_meta.type = (Type_){
    .ty = VAL_BOOL,
    .kind = KIND_VAL,
    .obj = OBJ_TYPE_UNKNOWN
  };

  do_analysis((AstNode_*)stmt->condition_expr);
  do_analysis(stmt->block_stmt);

  if (!IS_TY_BOOL(AS_EXPR(stmt->condition_expr)->meta.type)) {
    error(analyzer_, n, "while loop conditional must be a bool.");
  }
}

void expression_statement_analysis(AstNode_* n) {
  AstExpressionStmt_* stmt = (AstExpressionStmt_*)n;
  do_analysis((AstNode_*)stmt->expr);
}

void function_def_analysis(AstNode_* n) {
  AstFunctionDef_* def = (AstFunctionDef_*)n;  

  FunctionSymbol_* fn = &def->fn_symbol->fn;
  fn->return_type = def->body->return_type;

  ObjFunction_* obj_fn = objfn_create(def->fn_symbol);
  fn->obj_fn = obj_fn;

  def->base.meta = (SemanticInfo_) {
    .type = {
      .ty = fn->return_type.ty,
      .kind = KIND_STATIC,
      .obj = OBJ_TYPE_FUNCTION,
    },
    .sym = def->fn_symbol
  };

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
    param->opt_expr->top_meta.type = param->type;

    do_analysis((AstNode_*)param->opt_expr);
    if (!type_iscoercible(param->type, AS_EXPR(param->opt_expr)->meta.type)) {
      error(analyzer_, n, "Parameter type is not coercible.");
    }

    if (IS_TY_UNKNOWN(param->type)) {
      param->type = AS_EXPR(param->opt_expr)->meta.type;
    }
  }

  Symbol_* s = scope_find(n->scope, &param->name);// &n->scope->frame->fn_symbol->fn.params;
  if (!s) {
    error(analyzer_, n, "Could not find parameter %.*s", param->name.length, param->name.start);
    return;
  }

  if (s->type != SYMBOL_TYPE_VAR) {
    error(analyzer_, n, "Parameter type is unimplemented.");
    return;
  }

  s->var.meta.type = param->type;
}

void function_call_analysis(AstNode_* node) {
  AstFunctionCall_* call = (AstFunctionCall_*)node;
  call->prefix->top_meta = call->base.top_meta;
  do_analysis((AstNode_*)call->prefix);  

  call->base.meta = AS_EXPR(call->prefix)->meta;
  Symbol_* sym = call->base.meta.sym;
  FunctionSymbol_* fn_sym = symbol_ascallable(sym);

  call->args->fn_sym = fn_sym;
  do_analysis((AstNode_*)call->args);

  if (!fn_sym) {
    error(analyzer_, node, "Trying to call variable that is not callable.");
    return;
  }
}

void function_args_analysis(AstNode_* node) {
  AstFunctionArgs_* args = (AstFunctionArgs_*)node;
  FunctionSymbol_* fn_sym = args->fn_sym;

  if (fn_sym->params.count > UINT8_MAX) {
    error(analyzer_, node, "Parameter count exceeded maximum of 255.");
  }

  // TODO: allow for optional arguments
  if (fn_sym->params.count != args->args.count) {
    error(analyzer_, node,
      "Parameter count does not match definition. Expected %d, got %d",
      fn_sym->params.count, args->args.count);
  }

  List_* params = &args->fn_sym->params;
  ListNode_* param_node = params->head;
  for (AstListNode_* n = args->args.head; n != NULL; n = n->next) {
    AS_EXPR(n->node)->top_meta = list_val(param_node, Symbol_*)->var.meta;
    do_analysis(n->node);

    VarSymbol_* param = &list_val(param_node, Symbol_*)->var;
    SemanticInfo_ param_meta = param->meta;
    SemanticInfo_ expr_meta = AS_EXPR(n->node)->meta;

    if (param_meta.type.ty != expr_meta.type.ty ||
      (param_meta.type.kind == KIND_REF && expr_meta.type.kind != KIND_REF && expr_meta.type.kind != KIND_WEAK_REF)) {
      error(analyzer_, n->node, "Expression does not match function parameter type.");
    }

    param_node = param_node->next;
  }
}

void noop_analysis(AstNode_* n) {}

void clean_up_temps_analysis(AstNode_* n) {

}

void ast_tmp_decl_analysis(AstNode_* n) {
  AstTmpDecl_* decl = (AstTmpDecl_*)n;
  decl->expr->top_meta = decl->base.top_meta;
  do_analysis((AstNode_*)decl->expr);
  decl->base.meta = decl->expr->meta;
}

AnalysisRule_ analysis_rules[] = {
  [AST_CLS(AstProgram_)]        = {program_analysis},
  [AST_CLS(AstBlock_)]          = {block_analysis},
  [AST_CLS(AstStmt_)]           = {stmt_analysis},
  [AST_CLS(AstExpr_)]           = {expr_analysis},
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
  [AST_CLS(AstNoopExpr_)]       = {noop_analysis},
  [AST_CLS(AstNoopStmt_)]       = {noop_analysis},
  [AST_CLS(AstCleanUpTemps_)]   = {clean_up_temps_analysis},
  [AST_CLS(AstTmpDecl_)]        = {ast_tmp_decl_analysis},
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