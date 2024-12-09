#if 0
#include "parser.h"
#include "ast.h"
#include "parser.h"
#include "memory.h"
#include "symbol_table.h"
#include "type.h"
#include "object.h"
#include "errors.h"

#include <stdarg.h>
#include <string.h>

typedef void (*ResolverFn)(AstParser_*, AstNode_*);

typedef struct ResolverRule_ {
  ResolverFn fn;
} ResolverRule_;

static void do_resolve(AstParser_* parser, AstNode_* node) {
  get_rule(node->cls)->fn(parser, node);
}

void analyze(AstParser_* parser, struct AstProgram_* ast) {
  do_resolve(parser, (AstNode_*)ast);
}

static void program_resolve(AstParser_* parser, AstNode_* node) {
  AstProgram_* program = (AstProgram_*)node;
  do_resolve(parser, (AstNode_*)program->block);
}

static void block_resolve(AstParser_* parser, AstNode_* node) {
  AstBlock_* block = (AstBlock_*)node;
  for (AstListNode_* n = block->statements.head; n != NULL; n = n->next) {
    do_resolve(parser, n->node);
  }
}

static void stmt_resolve(AstParser_* parser, AstNode_* node) {
  AstStmt_* stmt = (AstStmt_*)node;
  do_resolve(parser, stmt->stmt);
  do_resolve(parser, stmt->cleanup);
}

static void expr_resolve(AstParser_* parser, AstNode_* node) {
  AstExpr_* expr = (AstExpr_*)node;
  if (!expr->expr) {
    return;
  }

  AS_EXPR(expr->expr)->top_type = expr->top_type;
  do_resolve(parser, (AstNode_*)expr->expr);
}

static void print_resolve(AstParser_* parser, AstNode_* n) {
  AstPrintStmt_* stmt = (AstPrintStmt_*)n;
  do_resolve(parser, (AstNode_*)stmt->expr);
}

static void unary_resolve(AstParser_* parser, AstNode_* n) {
  AstUnaryExp_* expr = (AstUnaryExp_*)n;
  do_resolve(parser, (AstNode_*)expr->expr);
}

static void binary_resolve(AstParser_* parser, AstNode_* n) {
  AstBinaryExp_* expr = (AstBinaryExp_*)n;
  do_resolve(parser, (AstNode_*)expr->left);
  do_resolve(parser, (AstNode_*)expr->right);
}

static void primary_resolve(AstParser_* parser, AstNode_* n) {
  //AstPrimaryExp_* exp = AST_CAST(AstPrimaryExp_, n);
}

static void return_resolve(AstParser_* parser, AstNode_* n) {
  AstReturnStmt_* stmt = (AstReturnStmt_*)n;
  do_resolve(parser, (AstNode_*)stmt->expr);
}

static void if_resolve(AstParser_* parser, AstNode_* n) {
  AstIfStmt_* stmt = (AstIfStmt_*)n;

  do_resolve(parser, (AstNode_*)stmt->condition_expr);
  do_resolve(parser, stmt->if_stmt);
  if (stmt->else_stmt) {
    do_resolve(parser, stmt->else_stmt);
  }

  for (AstListNode_* n = stmt->elif_exprs.head; n != NULL; n = n->next) {
    do_resolve(parser, n->node);
  }

  for (AstListNode_* n = stmt->elif_stmts.head; n != NULL; n = n->next) {
    do_resolve(parser, n->node);
  }
}

static void assert_resolve(AstParser_* parser, AstNode_* n) {
  AstAssertStmt_* stmt = (AstAssertStmt_*)n;
  do_resolve(parser, (AstNode_*)stmt->expr);
}

static void var_decl_resolve(AstParser_* parser, AstNode_* n) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)n;

  if (stmt->expr) {
    do_resolve(parser, (AstNode_*)stmt->expr);
  }
}

static void var_expr_resolve(AstParser_* parser, AstNode_* n) {
  AstVarExpr_* expr = (AstVarExpr_*)n;
  do_resolve(parser, (AstNode_*)expr->expr);
}

static void id_expr_resolve(AstParser_* parser, AstNode_* n) {
  AstIdExpr_* expr = (AstIdExpr_*)n;
  Symbol_* sym = scope_find(n->scope, &expr->name);
  if (!sym) {
    error(parser, n, "Could not find variable '%.*s'", expr->name.length, expr->name.start);
    return;
  }

  if (!sym->ty) {
    error(parser, n, "Could not find type for variable '%.*s'", expr->name.length, expr->name.start);
    return;
  }
}

static void index_expr_resolve(AstParser_* parser, AstNode_* n) {
  AstIndexExpr_* expr = (AstIndexExpr_*)n;
  do_resolve(parser, (AstNode_*)expr->prefix);
  do_resolve(parser, (AstNode_*)expr->index);
}

inline static bool node_is_assignable(AstNode_* n) {
  return n->cls == AST_CLS(AstVarExpr_);
}

static void assignment_expr_resolve(AstParser_* parser, AstNode_* n) {
  AstAssignmentExpr_* expr = (AstAssignmentExpr_*)n;

  do_resolve(parser, (AstNode_*)expr->left);
  do_resolve(parser, (AstNode_*)expr->right);
}

static void in_place_binary_stmt_resolve(AstParser_* parser, AstNode_* n) {
  AstInPlaceBinaryStmt_* expr = (AstInPlaceBinaryStmt_*)n;

  // TODO: allow for deconstructing tuples in assignments (and other types).
  do_resolve(parser, (AstNode_*)expr->left);
  do_resolve(parser, (AstNode_*)expr->right);
}

static void while_stmt_resolve(AstParser_* parser, AstNode_* n) {
  AstWhileStmt_* stmt = (AstWhileStmt_*)n;
  do_resolve(parser, (AstNode_*)stmt->condition_expr);
  do_resolve(parser, stmt->block_stmt);
}

static void for_stmt_resolve(AstParser_* parser, AstNode_* n) {
  AstForStmt_* stmt = (AstForStmt_*)n;
  if (stmt->opt_var_decl) {
    do_resolve(parser, stmt->opt_var_decl);
  }

  if (stmt->opt_condition_expr) {
    do_resolve(parser, (AstNode_*)stmt->opt_condition_expr);
  }

  if (stmt->opt_step_expr) {
    do_resolve(parser, (AstNode_*)stmt->opt_step_expr);
  }

  do_resolve(parser, stmt->block_stmt);
}

static void expression_statement_resolve(AstParser_* parser, AstNode_* n) {
  AstExpressionStmt_* stmt = (AstExpressionStmt_*)n;
  do_resolve(parser, (AstNode_*)stmt->expr);
}

static void function_def_resolve(AstParser_* parser, AstNode_* n) {
  AstFunctionDef_* def = (AstFunctionDef_*)n;
  for (AstListNode_* n = def->function_params.head; n != NULL; n = n->next) {
    do_resolve(parser, n->node);
  }

  do_resolve(parser, def->body);

  if (def->base.base.specializations.count) {
    for (AstListNode_* n = def->base.base.specializations.head; n != NULL; n = n->next) {
      do_resolve(parser, n->node);
    }
  }
}

static void generic_function_def_resolve(AstParser_* parser, AstNode_* n) {
  assertf(false, "unimplemented");
}
static void function_param_resolve(AstParser_* parser, AstNode_* n) {
  AstFunctionParam_* param = (AstFunctionParam_*)n;

}

static void function_call_resolve(AstParser_* parser, AstNode_* node) {
  AstFunctionCall_* call = (AstFunctionCall_*)node;
  do_resolve(parser, (AstNode_*)call->prefix);
  do_resolve(parser, (AstNode_*)call->args);
}

static void function_call_args_resolve(AstParser_* parser, AstNode_* node) {
  AstFunctionCallArgs_* args = AST_CAST(AstFunctionCallArgs_, node);
  FunctionType_* fn_type = type_as(FunctionType_, args->fn_type);

  for (AstListNode_* n = args->args.head; n != NULL; n = n->next) {
    do_resolve(parser, n->node);
  }
}

static void noop_resolve(AstParser_* parser, AstNode_* n) {}

static void clean_up_temps_resolve(AstParser_* parser, AstNode_* n) {

}

static void ast_tmp_decl_resolve(AstParser_* parser, AstNode_* n) {
  AstTmpDecl_* decl = (AstTmpDecl_*)n;
  do_resolve(parser, (AstNode_*)decl->expr);
}

static void ast_class_def_resolve(AstParser_* parser, AstNode_* node) {
  AstClassDef_* def = (AstClassDef_*)node;

  for (AstListNode_* n = def->members.head; n != NULL; n = n->next) {
    do_resolve(parser, n->node);
  }

  if (def->base.specializations.count) {
    for (AstListNode_* n = def->base.specializations.head; n != NULL; n = n->next) {
      do_resolve(parser, n->node);
    }
  }
}

static void ast_class_member_decl_resolve(AstParser_* parser, AstNode_* node) {
  AstClassMemberDecl_* decl = (AstClassMemberDecl_*)node;
  Type_* type = decl->field_type;

  if (decl->opt_expr) {
    AstExpr_* expr = decl->opt_expr;
    do_resolve(parser, (AstNode_*)expr);
  }
}

static void ast_dot_expr_resolve(AstParser_* parser, AstNode_* node) {
  AstDotExpr_* expr = AST_CAST(AstDotExpr_, node);

  do_resolve(parser, (AstNode_*)expr->prefix);
}

static void ast_class_constructor_resolve(AstParser_* parser, AstNode_* node) {
  AstClassConstructor_* constructor = (AstClassConstructor_*)node;

  for (AstListNode_* n = constructor->params.head; n != NULL; n = n->next) {
    AstClassConstructorParam_* field = AST_CAST(AstClassConstructorParam_, n->node);
    do_resolve(parser, (AstNode_*)field);
  }
}

static void ast_class_constructor_param_resolve(AstParser_* parser, AstNode_* node) {
  AstClassConstructorParam_* field = (AstClassConstructorParam_*)node;
  do_resolve(parser, (AstNode_*)field->expr);
}

static void array_value_resolve(AstParser_* parser, AstNode_* node) {
  AstArrayValueExpr_* array_expr = (AstArrayValueExpr_*)node;

  for (AstListNode_* n = array_expr->values.head; n != NULL; n = n->next) {
    AstExpr_* expr = (AstExpr_*)n->node;
    do_resolve(parser, n->node);
  }
}

static void range_expr_resolve(AstParser_* parser, AstNode_* node) {
}

static void type_def_resolve(AstParser_* parser, AstNode_* node) {
  AstTypeDef_* type_def = (AstTypeDef_*)node;

  for (AstListNode_* n = type_def->members.head; n != NULL; n = n->next) {
    do_resolve(parser, n->node);
  }
}

static void type_member_decl_resolve(AstParser_* parser, AstNode_* node) {
  TypeMemberDecl_* member_decl = (TypeMemberDecl_*)node;
}

static void generic_params_resolve(AstParser_* parser, AstNode_* node) {
  AstGenericParams_* params = (AstGenericParams_*)node;
}

static void index_or_generic_args_resolve(AstParser_* parser, AstNode_* node) {

}

static void index_or_type_expr_resolve(AstParser_* parser, AstNode_* node) {
  assertf(false, "unimplemented");
  AstVarOrTypeExpr_* expr = (AstVarOrTypeExpr_*)node;
  do_resolve(parser, (AstNode_*)expr->prefix);
}

ResolverRule_ analysis_rules[] = {
  [AST_CLS(AstProgram_)] = {program_resolve},
  [AST_CLS(AstBlock_)] = {block_resolve},
  [AST_CLS(AstStmt_)] = {stmt_resolve},
  [AST_CLS(AstExpr_)] = {expr_resolve},
  [AST_CLS(AstPrintStmt_)] = {print_resolve},
  [AST_CLS(AstUnaryExp_)] = {unary_resolve},
  [AST_CLS(AstBinaryExp_)] = {binary_resolve},
  [AST_CLS(AstPrimaryExp_)] = {primary_resolve},
  [AST_CLS(AstReturnStmt_)] = {return_resolve},
  [AST_CLS(AstIfStmt_)] = {if_resolve},
  [AST_CLS(AstAssertStmt_)] = {assert_resolve},
  [AST_CLS(AstVarDeclStmt_)] = {var_decl_resolve},
  [AST_CLS(AstVarExpr_)] = {var_expr_resolve},
  [AST_CLS(AstIndexExpr_)] = {index_expr_resolve},
  [AST_CLS(AstIdExpr_)] = {id_expr_resolve},
  [AST_CLS(AstAssignmentExpr_)] = {assignment_expr_resolve},
  [AST_CLS(AstInPlaceBinaryStmt_)] = {in_place_binary_stmt_resolve},
  [AST_CLS(AstWhileStmt_)] = {while_stmt_resolve},
  [AST_CLS(AstForStmt_)] = {for_stmt_resolve},
  [AST_CLS(AstFunctionDef_)] = {function_def_resolve},
  [AST_CLS(AstGenericFunctionDef_)] = {generic_function_def_resolve},
  [AST_CLS(AstFunctionParam_)] = {function_param_resolve},
  [AST_CLS(AstFunctionCall_)] = {function_call_resolve},
  [AST_CLS(AstFunctionCallArgs_)] = {function_call_args_resolve},
  [AST_CLS(AstExpressionStmt_)] = {expression_statement_resolve},
  [AST_CLS(AstNoopExpr_)] = {noop_resolve},
  [AST_CLS(AstNoopStmt_)] = {noop_resolve},
  [AST_CLS(AstCleanUpTemps_)] = {clean_up_temps_resolve},
  [AST_CLS(AstTmpDecl_)] = {ast_tmp_decl_resolve},
  [AST_CLS(AstClassDef_)] = {ast_class_def_resolve},
  [AST_CLS(AstClassMemberDecl_)] = {ast_class_member_decl_resolve},
  [AST_CLS(AstClassConstructor_)] = {ast_class_constructor_resolve},
  [AST_CLS(AstClassConstructorParam_)] = {ast_class_constructor_param_resolve},
  [AST_CLS(AstDotExpr_)] = {ast_dot_expr_resolve},
  [AST_CLS(AstTypeExpr_)] = {noop_resolve},
  [AST_CLS(AstArrayValueExpr_)] = {array_value_resolve},
  [AST_CLS(AstRangeExpr_)] = {range_expr_resolve},
  [AST_CLS(AstTypeDef_)] = {type_def_resolve},
  [AST_CLS(TypeMemberDecl_)] = {type_member_decl_resolve},
  [AST_CLS(AstGenericParam_)] = {noop_resolve},
  [AST_CLS(AstGenericParams_)] = {generic_params_resolve},
  [AST_CLS(AstVarOrTypeExpr_)] = {index_or_type_expr_resolve},
  [AST_CLS(AstIndexOrGenericArgs_)] = {index_or_generic_args_resolve},
};

// Static assert to make sure that all node types are accounted for.
STATIC_ASSERT(
  sizeof(analysis_rules) / sizeof(ResolverRule_) == __AST_NODE_COUNT__,
  CHECK_ANALYSIS_COUNT);

static ResolverRule_* get_rule(int info) {
  ResolverRule_* ret = &analysis_rules[info];
  assertf(ret->fn, "Could not find analysis rule for AST class: %d", info);
  return ret;
}

static void error(AstParser_* parser, AstNode_* node, const char* message, ...) {
  if (parser->errors->panic_mode) return;
  parser->errors->panic_mode = true;
  parser->had_error = true;

  va_list args;
  va_start(args, message);
  fprintf(stderr, "[line %d] Error: ", node->line);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
}
#endif