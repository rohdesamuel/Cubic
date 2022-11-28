#include "analyzer.h"
#include "ast.h"
#include "memory.h"
#include "symbol_table.h"

#include <stdarg.h>
#include <string.h>

typedef void (*AnalysisFn)(AstNode_*);

typedef struct AnalysisRule_ {
  AnalysisFn fn;
} AnalysisRule_;

static AnalysisRule_* get_rule(int type);

Analyzer_* analyzer_;

static void do_analysis(AstNode_* node) {
  get_rule(node->cls)->fn(node);
}

void analyzer_init(Analyzer_* analyzer) {
  memset(analyzer, 0, sizeof(Analyzer_));
  pageallocator_init(&analyzer->allocator, 4096);
  analyzer->symbol_table = symboltable_create((MemoryAllocator_*)&analyzer->allocator);
}

void analyze(Analyzer_* analyzer, struct AstProgram_* ast) {
  analyzer_ = analyzer;
  do_analysis((AstNode_*)ast);
}

void analyzer_clear(Analyzer_* analyzer) {
  pageallocator_deinit(&analyzer->allocator);
  analyzer_ = NULL;
}

static void error(Analyzer_* analyzer, AstNode_* node, const char* message, ...);

void program_analysis(AstNode_* node) {
  AstProgram_* program = (AstProgram_*)node;
  do_analysis((AstNode_*)program->block);
}

void block_analysis(AstNode_* node) {
  AstBlock_* block = (AstBlock_*)node;
  for (AstListNode_* n = block->statements.head; n != NULL; n = n->next) {
    do_analysis(n->node);
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
      if (!ISA_NUMBER(exp->base)) {
        error(analyzer_, n, "expression does not have a number type.");
      }
      break;

    case TK_NOT:
      if (exp->base.type != VAL_BOOL) {
        error(analyzer_, n, "expression does not have a boolean type.");
      }
      break;

    case TK_MINUS:
      if (!ISA_NUMBER(exp->base)) {
        error(analyzer_, n, "expression does not have a number type.");
      }
      break;    
  }
}

void binary_analysis(AstNode_* n) {
  AstBinaryExp_* exp = (AstBinaryExp_*)n;

  do_analysis(exp->left);
  do_analysis(exp->right);

  if (AS_EXPR(exp->left)->type == VAL_UNKNOWN) {
    error(analyzer_, n, "left-hand expression type could not be deduced.");
    return;
  }

  if (AS_EXPR(exp->right)->type == VAL_UNKNOWN) {
    error(analyzer_, n, "right-hand expression type could not be deduced.");
    return;
  }

  if (AS_EXPR(exp->left)->type != AS_EXPR(exp->right)->type) {
    error(analyzer_, n, "left and right expressions are not the same type.");
  }

  ValueType expected_type = VAL_UNKNOWN;
  switch (exp->op) {
    case TK_AND: 
    case TK_OR:
    case TK_XOR:
      exp->base.type = VAL_BOOL;
      expected_type = VAL_BOOL;
      break;

    case TK_GT:
    case TK_GTE:
    case TK_LT:
    case TK_LTE:
    case TK_EQUAL_EQUAL:
    case TK_BANG_EQUAL:
      exp->base.type = VAL_BOOL;
      break;

    case TK_AMPERSAND:
    case TK_PIPE:
    case TK_HAT:
      exp->base.type = AS_EXPR(exp->left)->type;
      if (!ISA_INTEGER(exp->base)) {
        error(analyzer_, n, "expected the expression type to be a number.");
      }
      break;

    case TK_PLUS:
    case TK_MINUS:
    case TK_STAR:
    case TK_SLASH:
      exp->base.type = AS_EXPR(exp->left)->type;
      if (!ISA_NUMBER(exp->base)) {
        error(analyzer_, n, "expected the expression type to be a number.");
      }
      break;

    // case TK_DOUBLE_SLASH:
    case TK_PERCENT:
    case TK_LSHIFT:
    case TK_RSHIFT:
      exp->base.type = AS_EXPR(exp->left)->type;
      expected_type = VAL_INT;
      break;

    default:
      error(analyzer_, n, "unimplemented token type.");
      break;
  }

  if (expected_type != VAL_UNKNOWN) {
    if (AS_EXPR(exp->left)->type != expected_type) {
      error(analyzer_, n, "left-hand expression does not have expected type of %s.", valuetype_str(expected_type));
    }

    if (AS_EXPR(exp->right)->type != expected_type) {
      error(analyzer_, n, "right-hand expression does not have expected type of %s.", valuetype_str(expected_type));
    }
  }
}

void primary_analysis(AstNode_* n) {
  AstPrimaryExp_* primary = (AstPrimaryExp_*)n;
  primary->base.type = primary->value.type;
}

void return_analysis(AstNode_* n) {}

void if_analysis(AstNode_* n) {
  AstIfStmt_* stmt = (AstIfStmt_*)n;

  do_analysis(stmt->condition_expr);
  if (AS_EXPR(stmt->condition_expr)->type != VAL_BOOL) {
    error(analyzer_, n, "if expression must have a boolean type.");
  }

  do_analysis(stmt->if_stmt);
  if (stmt->else_stmt) {
    do_analysis(stmt->else_stmt);
  }

  for (AstListNode_* n = stmt->elif_exprs.head; n != NULL; n = n->next) {
    do_analysis(n->node);
    if (AS_EXPR(n->node)->type != VAL_BOOL) {
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

  if (AS_EXPR(stmt->expr)->type != VAL_BOOL) {
    error(analyzer_, n, "assert expression must have a boolean type.");
  }
}

void var_decl_analysis(AstNode_* n) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)n;
  for (AstListNode_* n = stmt->exprs.head; n != NULL; n = n->next) {
    do_analysis(n->node);
  }

  if (stmt->type == VAL_UNKNOWN) {
    assertf(stmt->exprs.head, "type deduced variable must have an expression");
    
    // TODO: implement tuples (and others) for variable declarations.
    VarSymbol_* var = symboltable_var(stmt->base.symbol_table, &stmt->name);
    var->type = stmt->type = AS_EXPR(stmt->exprs.head->node)->type;
  }


  if (stmt->exprs.head && !valuetype_iscoercible(AS_EXPR(stmt->exprs.head->node)->type, stmt->type)) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }

  /*if (stmt->exprs.head && stmt->type != AS_EXPR(stmt->exprs.head->node)->type) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }*/
}

void var_expr_analysis(AstNode_* n) {
  AstVarExpr_* expr = (AstVarExpr_*)n;
  do_analysis((AstNode_*)expr->expr);
  expr->base.type = expr->expr->type;
}

void id_expr_analysis(AstNode_* n) {
  AstIdExpr_* expr = (AstIdExpr_*)n;
  SymbolTable_* table = n->symbol_table;

  VarSymbol_* var = symboltable_var(table, &expr->name);
  if (var) {
    expr->base.type = var->type;
  } else {
    error(analyzer_, n, "Could not find variable %.*s", expr->name.length, expr->name.start);
    expr->base.type = VAL_UNKNOWN;    
  }
}

void assignment_stmt_analysis(AstNode_* n) {
  AstAssignmentStmt_* stmt = (AstAssignmentStmt_*)n;
  
  // TODO: allow for deconstructing tuples in assignments (and other types).
  do_analysis(stmt->vars.head->node);
  do_analysis(stmt->exprs.head->node);

  AstVarExpr_* lvalue_expr = (AstVarExpr_*)stmt->vars.head->node;
  AstExpr_* rvalue_expr = (AstExpr_*)stmt->exprs.head->node;

  if (lvalue_expr->base.type != rvalue_expr->type) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }
}

void while_stmt_analysis(AstNode_* n) {
  AstWhileStmt_* stmt = (AstWhileStmt_*)n;
  do_analysis(stmt->condition_expr);
  do_analysis(stmt->block_stmt);

  if (AS_EXPR(stmt->condition_expr)->type != VAL_BOOL) {
    error(analyzer_, n, "while loop conditional must be a bool.");
  }
}

AnalysisRule_ analysis_rules[] = {
  [AST_CLS(AstProgram_)] =        {program_analysis},
  [AST_CLS(AstBlock_)] =          {block_analysis},
  [AST_CLS(AstPrintStmt_)] =      {print_analysis},
  [AST_CLS(AstUnaryExp_)] =       {unary_analysis},
  [AST_CLS(AstBinaryExp_)] =      {binary_analysis},
  [AST_CLS(AstPrimaryExp_)] =     {primary_analysis},
  [AST_CLS(AstReturnStmt_)] =     {return_analysis},
  [AST_CLS(AstIfStmt_)] =         {if_analysis},
  [AST_CLS(AstAssertStmt_)] =     {assert_analysis},
  [AST_CLS(AstVarDeclStmt_)] =    {var_decl_analysis},
  [AST_CLS(AstVarExpr_)] =        {var_expr_analysis},
  [AST_CLS(AstIdExpr_)] =         {id_expr_analysis},
  [AST_CLS(AstAssignmentStmt_)] = {assignment_stmt_analysis},
  [AST_CLS(AstWhileStmt_)] =      {while_stmt_analysis},
};

// Static assert to make sure that all node types are accounted for.
STATIC_ASSERT(
  sizeof(analysis_rules) / sizeof(AnalysisRule_) == __AST_NODE_COUNT__,
  CHECK_ANALYSIS_COUNT);

static AnalysisRule_* get_rule(int type) {
  AnalysisRule_* ret = &analysis_rules[type];  
  assertf(ret->fn, "Could not find rule for AST class: %d", type);
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