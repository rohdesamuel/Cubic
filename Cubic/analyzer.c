#include "analyzer.h"
#include "ast.h"
#include "memory.h"
#include "symbol_table.h"
#include "type.h"
#include "object.h"

#include <stdarg.h>
#include <string.h>

typedef void (*AnalysisFn)(AstNode_*);

typedef struct AnalysisRule_ {
  AnalysisFn fn;
} AnalysisRule_;

static AnalysisRule_* get_rule(int info);
static void error(Analyzer_* analyzer, AstNode_* node, const char* message, ...);

thread_local Analyzer_* analyzer_;

static void do_analysis(AstNode_* node) {
  get_rule(node->cls)->fn(node);
}

void analyzer_init(Analyzer_* analyzer, MemoryAllocator_* allocator) {
  *analyzer = (Analyzer_){ 0 };
  analyzer->allocator = allocator;
  analyzer->frame = frame_root(analyzer->allocator);
  analyzer->scope = analyzer->frame->scope;
}

void analyze(Analyzer_* analyzer, struct AstProgram_* ast) {
  analyzer_ = analyzer;
  do_analysis((AstNode_*)ast);
}

void analyzer_clear(Analyzer_* analyzer) {
  scope_destroy(&analyzer->scope);
  frame_destroy(&analyzer->frame);
  *analyzer = (Analyzer_){ 0 };
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
  if (!expr->expr) {
    return;
  }

  AS_EXPR(expr->expr)->top_type = expr->top_type;
  do_analysis((AstNode_*)expr->expr);
  expr->type = AS_EXPR(expr->expr)->type;
}

void print_analysis(AstNode_* n) {
  AstPrintStmt_* stmt = (AstPrintStmt_*)n;
  do_analysis((AstNode_*)stmt->expr);
}

void unary_analysis(AstNode_* n) {
  AstUnaryExp_* expr = (AstUnaryExp_*)n;
  
  expr->expr->top_type = expr->base.top_type;
  do_analysis((AstNode_*)expr->expr);
  expr->base.type = AS_EXPR(expr->expr)->type;
  switch (expr->op) {
    case TK_TILDE:
      if (!type_isanumber(expr->base.type)) {
        error(analyzer_, n, "expression does not have a number type.");
      }
      break;

    case TK_NOT:
      if (!type_isabool(expr->base.type)) {
        error(analyzer_, n, "expression does not have a boolean type.");
      }
      break;

    case TK_MINUS:
      if (!type_isanumber(expr->base.type)) {
        error(analyzer_, n, "expression does not have a number type.");
      }
      break;

    case TK_DEL:
      if (!type_is(expr->base.type, VarType_)) {
        error(analyzer_, n, "expression is not a reference and cannot be deleted");
      }
      break;

    default:
      error(analyzer_, n, "Unknown unary operator");
      break;
  }
}

void binary_analysis(AstNode_* n) {
  AstBinaryExp_* expr = (AstBinaryExp_*)n;
  expr->left->top_type = expr->base.top_type;
  expr->right->top_type = expr->base.top_type;

  do_analysis((AstNode_*)expr->left);
  do_analysis((AstNode_*)expr->right);

  Type_* ltype = AS_EXPR(expr->left)->type;
  Type_* rtype = AS_EXPR(expr->right)->type;

  if (type_isunknown(ltype)) {
    error(analyzer_, n, "left-hand expression type could not be deduced.");
    return;
  }

  if (type_isunknown(rtype)) {
    error(analyzer_, n, "right-hand expression type could not be deduced.");
    return;
  }

  if (!type_coercible(rtype, ltype)) {
    error(analyzer_, n, "right-hand expression cannot be coerced to left-hand expression.");
  }

  Type_* lval_type = type_valtype(ltype);
  Type_* rval_type = type_valtype(rtype);
  const Type_* expected_type = NULL;
  switch (expr->op) {
    case TK_AND: 
    case TK_OR:
    case TK_XOR:
      expr->base.type = (Type_*)&Bool_Ty;
      expected_type = (Type_*)&Bool_Ty;
      break;

    case TK_GT:
    case TK_GTE:
    case TK_LT:
    case TK_LTE:
    case TK_EQUAL_EQUAL:
    case TK_BANG_EQUAL:
      expr->base.type = (Type_*)&Bool_Ty;
      break;

    case TK_AMPERSAND:
    case TK_PIPE:
    case TK_HAT:
      expr->base.type = lval_type;
      if (!type_isainteger(lval_type)) {
        error(analyzer_, n, "expected the expression type to be a number.");
      }
      break;

    case TK_PLUS:
      expr->base.type = lval_type;
      if (!type_isanumber(lval_type) && !type_isastring(lval_type)) {
        error(analyzer_, n, "expected the expression type to be a number or a string.");
      }
      break;

    case TK_MINUS:
    case TK_STAR:
    case TK_SLASH:
    case TK_DOUBLE_SLASH:
      expr->base.type = lval_type;
      if (!type_isanumber(lval_type)) {
        error(analyzer_, n, "expected the expression type to be a number.");
      }
      break;

    case TK_PERCENT:
    case TK_LSHIFT:
    case TK_RSHIFT:
      expr->base.type = lval_type;
      expected_type = (const Type_*)&Int_Ty;
      break;

    default:
      error(analyzer_, n, "unimplemented token type.");
      break;
  }

  if (expected_type) {
    if (ltype->cls != expected_type->cls) {
      error(analyzer_, n, "left-hand expression does not have expected type of %s.", type_tostr(expected_type));
    }

    if (rtype->cls != expected_type->cls) {
      error(analyzer_, n, "right-hand expression does not have expected type of %s.", type_tostr(expected_type));
    }
  }
}

void primary_analysis(AstNode_* n) {
  //AstPrimaryExp_* exp = AST_CAST(AstPrimaryExp_, n);
}

void return_analysis(AstNode_* n) {
  AstReturnStmt_* stmt = (AstReturnStmt_*)n;
  Frame_* frame = stmt->base.scope->frame;
  stmt->expr->top_type = type_as(FunctionType_, frame->fn_symbol->ty)->ret_ty;

  do_analysis((AstNode_*)stmt->expr);
  Type_* from = type_valtype(AS_EXPR(stmt->expr)->type);
  Type_* to = type_valtype(type_as(FunctionType_, frame->fn_symbol->ty)->ret_ty);
  if (!type_coercible(from, to)) {
    error(analyzer_, n, "Return statement type does not match function type.");
  }  
}

void if_analysis(AstNode_* n) {
  AstIfStmt_* stmt = (AstIfStmt_*)n;
  stmt->condition_expr->top_type = (Type_*)&Bool_Ty;

  do_analysis((AstNode_*)stmt->condition_expr);
  if (!type_isabool(AS_EXPR(stmt->condition_expr)->type)) {
    error(analyzer_, n, "if expression must have a boolean type.");
  }

  do_analysis(stmt->if_stmt);
  if (stmt->else_stmt) {
    do_analysis(stmt->else_stmt);
  }

  for (AstListNode_* n = stmt->elif_exprs.head; n != NULL; n = n->next) {
    do_analysis(n->node);
    if (!type_isabool(AS_EXPR(n->node)->type)) {
      error(analyzer_, n->node, "elif expression must have a boolean type.");
    }
  }
  for (AstListNode_* n = stmt->elif_stmts.head; n != NULL; n = n->next) {
    do_analysis(n->node);
  }
}

void assert_analysis(AstNode_* n) {
  AstAssertStmt_* stmt = (AstAssertStmt_*)n;
  stmt->expr->top_type = (Type_*)&Bool_Ty;
  do_analysis((AstNode_*)stmt->expr);
  if (!type_isabool(AS_EXPR(stmt->expr)->type)) {
    error(analyzer_, n, "assert expression must have a boolean type.");
  }
}

void var_decl_analysis(AstNode_* n) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)n;

  if (stmt->expr) {
    stmt->expr->top_type = stmt->decl_type;
    do_analysis((AstNode_*)stmt->expr);
  }

  Type_* val_type = type_valtype(stmt->decl_type);
  if (type_isunknown(val_type)) {
    if (!stmt->expr) {
      error(analyzer_, n, "type deduced variable must have an expression");
      return;
    }

    if (type_is(stmt->expr->type, ArrayType_) && type_as(ArrayType_, stmt->expr->type)->count == 0) {
      error(analyzer_, n, "trying to intialize type deduced variable with an empty array");
      return;
    }

    type_set(val_type, stmt->expr->type);
  }
  type_fill(stmt->decl_type, n->scope);

  // An array declared of zero-size, takes the size of the expression.
  if (type_is(val_type, ArrayType_)) {
    ArrayType_* array_ty = type_as(ArrayType_, val_type);

    if (stmt->expr && !type_is(type_valtype(stmt->expr->type), ArrayType_)) {
      error(analyzer_, n, "arrays can only be initialized with an array.");
      return;
    }

    if (array_ty->count == 0) {
      if (!stmt->expr) {
        error(analyzer_, n, "trying to declare a zero-sized array.");
        return;
      }

      if (stmt->expr->base.cls != AST_CLS(AstArrayValueExpr_)) {
        error(analyzer_, n, "undetermined length arrays can only be initialized with an array list.");
        return;
      }

      array_ty->count = AST_CAST(AstArrayValueExpr_, stmt->expr)->values.count;
    }
  }

  // TODO: implement tuples (and others) for variable declarations.
  Symbol_* var = scope_find(n->scope, &stmt->name);
  if (stmt->expr && !type_coercible(AS_EXPR(stmt->expr)->type, val_type)) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }

  type_calcsize(var->ty);
}

void var_expr_analysis(AstNode_* n) {
  AstVarExpr_* expr = (AstVarExpr_*)n;
  expr->expr->top_type = expr->base.top_type;
  do_analysis((AstNode_*)expr->expr);
  expr->base.type = expr->expr->type;
}

void id_expr_analysis(AstNode_* n) {
  AstIdExpr_* expr = (AstIdExpr_*)n;
  Symbol_* sym = scope_find(n->scope, &expr->name);
  if (!sym) {
    error(analyzer_, n, "Could not find variable '%.*s'", expr->name.length, expr->name.start);
    return;
  }

  if (!sym->ty) {
    error(analyzer_, n, "Could not find type for variable '%.*s'", expr->name.length, expr->name.start);
    return;
  }

  expr->base.type = sym->ty;
}

void index_expr_analysis(AstNode_* n) {
  AstIndexExpr_* expr = (AstIndexExpr_*)n;
  expr->prefix->top_type = (Type_*)type_alloc_ty(analyzer_->allocator, RefType_);
  do_analysis((AstNode_*)expr->prefix);
  do_analysis((AstNode_*)expr->index);

  if (!type_isainteger(type_valtype(expr->index->type))) {
    error(analyzer_, n, "Index expression in array does not evaluate to an integer.");
    return;
  }

  if (!type_is(type_valtype(expr->prefix->type), ArrayType_)) {
    error(analyzer_, n, "Prefix expression does not evaluate to an array.");
    return;
  }

  expr->base.type = type_as(ArrayType_, type_valtype(expr->prefix->type))->el_type;
}


void assignment_expr_analysis(AstNode_* n) {
  AstAssignmentExpr_* expr = (AstAssignmentExpr_*)n;

  // TODO: allow for deconstructing tuples in assignments (and other types).
  expr->left->top_type = (Type_*)type_alloc_ty(analyzer_->allocator, RefType_);
  expr->right->top_type = expr->base.top_type;
  do_analysis((AstNode_*)expr->left);
  do_analysis((AstNode_*)expr->right);

  if (expr->left->base.cls != AST_CLS(AstVarExpr_)) {
    error(analyzer_, n, "Left-hand side of assignment cannot be assigned to.");
    return;
  }

  if (type_isconst(expr->left->type)) {
    error(analyzer_, n, "Left-hand side of assignment is const and cannot be assigned to.");
    return;
  }

  AstVarExpr_* lvalue_expr = (AstVarExpr_*)expr->left;
  AstExpr_* rvalue_expr = (AstExpr_*)expr->right;
  if (!type_assignable(rvalue_expr->type, lvalue_expr->base.type)) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }
  expr->base.type = lvalue_expr->base.type;
}

void in_place_binary_stmt_analysis(AstNode_* n) {
  AstInPlaceBinaryStmt_* expr = (AstInPlaceBinaryStmt_*)n;

  // TODO: allow for deconstructing tuples in assignments (and other types).
  expr->left->top_type = (Type_*)type_alloc_ty(analyzer_->allocator, RefType_);
  expr->right->top_type = expr->base.top_type;
  do_analysis((AstNode_*)expr->left);
  do_analysis((AstNode_*)expr->right);

  Type_* val_type = type_valtype(expr->left->type);
  if (!type_isanumber(val_type)) {
    error(analyzer_, n, "expression is not a number type");
  }
  
  if (expr->left->base.cls != AST_CLS(AstVarExpr_)) {
    error(analyzer_, n, "Left-hand side of assignment cannot be assigned to.");
    return;
  }

  if (type_isconst(expr->left->type)) {
    error(analyzer_, n, "Left-hand side of assignment is const and cannot be assigned to.");
    return;
  }

  AstVarExpr_* lvalue_expr = (AstVarExpr_*)expr->left;
  AstExpr_* rvalue_expr = (AstExpr_*)expr->right;
  if (!type_assignable(rvalue_expr->type, lvalue_expr->base.type)) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }

  switch (expr->op) {
    case TK_PLUS_EQUAL:
      expr->bin_op = TK_PLUS;
      break;

    case TK_MINUS_EQUAL:
      expr->bin_op = TK_MINUS;
      break;

    case TK_AMPERSAND_EQUAL:
      expr->bin_op = TK_AMPERSAND;
      break;

    case TK_PIPE_EQUAL:
      expr->bin_op = TK_PIPE;
      break;

    case TK_HAT_EQUAL:
      expr->bin_op = TK_HAT;
      break;

    case TK_TILDE_EQUAL:
      expr->bin_op = TK_TILDE;
      break;

    case TK_STAR_EQUAL:
      expr->bin_op = TK_STAR;
      break;

    case TK_PERCENT_EQUAL:
      expr->bin_op = TK_PERCENT;
      break;

    case TK_SLASH_EQUAL:
      expr->bin_op = TK_SLASH;
      break;

    case TK_SLASH_SLASH_EQUAL:
      expr->bin_op = TK_DOUBLE_SLASH;
      break;

    default:
      error(analyzer_, n, "Unknown in-place binary operator: %d", expr->op);
      break;
  }

  expr->base.type = lvalue_expr->base.type;
}

void while_stmt_analysis(AstNode_* n) {
  AstWhileStmt_* stmt = (AstWhileStmt_*)n;
  stmt->condition_expr->top_type = (Type_*)&Bool_Ty;

  do_analysis((AstNode_*)stmt->condition_expr);
  do_analysis(stmt->block_stmt);
   
  if (!type_isabool(AS_EXPR(stmt->condition_expr)->type)) {
    error(analyzer_, n, "while loop conditional must be a bool.");
  }
}

void expression_statement_analysis(AstNode_* n) {
  AstExpressionStmt_* stmt = (AstExpressionStmt_*)n;
  do_analysis((AstNode_*)stmt->expr);
}

void function_def_analysis(AstNode_* n) {
  AstFunctionDef_* def = (AstFunctionDef_*)n;  

  def->base.type = def->fn_symbol->ty;
  do_analysis((AstNode_*)def->body);  
}

void function_body_analysis(AstNode_* n) {
  AstFunctionBody_* body = (AstFunctionBody_*)n;
  FunctionType_* fn_type = type_as(FunctionType_, body->fn_symbol->ty);
  FunctionSymbol_* fn = &body->fn_symbol->fn;
  fn_type->ret_ty = body->return_type;

  type_fill(fn_type->ret_ty, n->scope);
  type_calcsize(fn_type->ret_ty);

  for (AstListNode_* n = body->function_params.head; n != NULL; n = n->next) {
    do_analysis(n->node);
  }

  do_analysis(body->stmt);
}

void function_param_analysis(AstNode_* n) {
  AstFunctionParam_* param = (AstFunctionParam_*)n;
  
  // TODO: implement parameter type inference.
  Type_* val_type = type_valtype(param->type);
  type_fill(param->type, n->scope);
  type_calcsize(param->type);
  if (type_isunknown(param->type)) {
    error(analyzer_, n, "Parameter type inference is unimplemented.");
  }
  
  Symbol_* s = scope_find(n->scope, &param->name);
  if (!s) {
    error(analyzer_, n, "Could not find parameter %.*s", param->name.length, param->name.start);
    return;
  }
}

void function_call_analysis(AstNode_* node) {
  AstFunctionCall_* call = (AstFunctionCall_*)node;
  call->prefix->top_type = call->base.top_type;
  do_analysis((AstNode_*)call->prefix);

  if (!type_is(call->prefix->type, FunctionType_)) {
    error(analyzer_, node, "Function call is not calling a function");
    return;
  }

  FunctionType_* fn_type = type_as(FunctionType_, call->prefix->type);
  call->base.type = fn_type->ret_ty;
  call->args->fn_type = (Type_*)fn_type;

  do_analysis((AstNode_*)call->args);
}

void function_call_arg_analysis(AstNode_* node) {
  AstFunctionCallArg_* arg = AST_CAST(AstFunctionCallArg_, node);
  
  arg->expr->top_type = arg->base.top_type;
  do_analysis((AstNode_*)arg->expr);
  arg->base.type = arg->expr->type;
}

void function_call_args_analysis(AstNode_* node) {
  AstFunctionCallArgs_* args = AST_CAST(AstFunctionCallArgs_, node);
  FunctionType_* fn_type = type_as(FunctionType_, args->fn_type);

  if (fn_type->params.count > UINT8_MAX) {
    error(analyzer_, node, "Parameter count exceeded maximum of 255.");
  }

  ListNode_* param_node = fn_type->params.head;
  for (AstListNode_* n = args->args.head; n != NULL; n = n->next) {
    Type_* param_type = list_val(param_node, Type_*);

    AS_EXPR(n->node)->top_type = param_type;
    do_analysis(n->node);
    Type_* expr_type = AS_EXPR(n->node)->type;

    if (!type_assignable(expr_type, type_valtype(param_type))) {
      error(analyzer_, n->node, "Expression does not match function parameter type.");
    }
    
    param_node = param_node->next;
  }

  // TODO: allow for optional arguments
  if (fn_type->params.count != args->args.count) {
    error(analyzer_, node,
      "Parameter count does not match definition. Expected %d, got %d",
      fn_type->params.count, args->args.count);
  }
}

void noop_analysis(AstNode_* n) {}

void clean_up_temps_analysis(AstNode_* n) {

}

void ast_tmp_decl_analysis(AstNode_* n) {
  AstTmpDecl_* decl = (AstTmpDecl_*)n;
  decl->expr->top_type = decl->base.top_type;
  do_analysis((AstNode_*)decl->expr);
  decl->base.type = decl->expr->type;
}

void ast_class_def_analysis(AstNode_* node) {
  AstClassDef_* def = (AstClassDef_*)node;

  for (AstListNode_* n = def->members.head; n != NULL; n = n->next) {
    do_analysis(n->node);
  }

  //if (semantictype_hascycle(&def->class_type)) {
  //  error(analyzer_, node, "struct \"%.*s\" has a cycle", cls_sym->name.length, cls_sym->name.start);
  //  return;
  //}

  type_calcsize(def->class_type);
  type_class_calcoffsets(def->class_type);
}

static Value_ fold_constants(Scope_* scope, AstExpr_* expr) {
  switch (expr->base.cls) {
    case AST_CLS(AstPrimaryExp_):
      return AST_CAST(AstPrimaryExp_, expr)->value;
  }
  
  assertf(false, "Unimplemented expression type for constant folding: %d", expr->base.cls);
}

void ast_class_member_decl_analysis(AstNode_* node) {
  AstClassMemberDecl_* decl = (AstClassMemberDecl_*)node;
  Type_* type = decl->field_type;

  if (!type_fill(type, node->scope)) {
    error(analyzer_, node,
      "Could not find type '%.*s' in class member declaration",
      type->opt_name.length, type->opt_name.start);
  }

  if (decl->opt_expr) {
    AstExpr_* expr = decl->opt_expr;
    do_analysis((AstNode_*)expr);

    if (!type_isval(expr->type)) {
      error(analyzer_, node, "struct member field default value must be a value.");
      return;
    }
  }
}

void ast_dot_expr_analysis(AstNode_* node) {
  AstDotExpr_* expr = AST_CAST(AstDotExpr_, node);

  expr->prefix->top_type = expr->base.top_type;
  do_analysis((AstNode_*)expr->prefix);

  Type_* prefix_type = expr->prefix->type;
  Type_* val_type = type_valtype(prefix_type);
  if (!type_is(val_type, ClassType_)) {
    error(analyzer_, (AstNode_*)expr->prefix, "Expected class type for sub-expression.");
    return;
  }
  ClassType_* cls_type = type_as(ClassType_, val_type);

  Type_* found = NULL;
  for (ListNode_* n = cls_type->members.head; n != NULL; n = n->next) {
    ClassTypeField_* field = list_ptr(n, ClassTypeField_);
    if (token_eq(field->name, expr->id)) {
      found = field->type;
      break;
    }
  }

  if (!found) {
    error(analyzer_, node, "Could not find field '%.*s'", expr->id.length, expr->id.start);
    return;
  }
  expr->cls_ty = val_type;
  expr->base.type = found;
}

void ast_class_constructor_analysis(AstNode_* node) {
  AstClassConstructor_* constructor = (AstClassConstructor_*)node;
  Symbol_* cls_sym = scope_search_to_root(node->scope, &constructor->name);

  Type_* type = constructor->base.type;
  if (!type_fill(type, node->scope)) {
    error(analyzer_, node, "Could not find class %.*s", constructor->name.length, constructor->name.start);
    return;
  } else {
    constructor->base.type = cls_sym->ty;
  }

  for (AstListNode_* n = constructor->params.head; n != NULL; n = n->next) {
    AstClassConstructorParam_* field = AST_CAST(AstClassConstructorParam_, n->node);
    field->base.top_type = constructor->base.type;
    do_analysis((AstNode_*)field);
  }
}

void ast_class_constructor_param_analysis(AstNode_* node) {
  AstClassConstructorParam_* field = (AstClassConstructorParam_*)node;
  field->expr->top_type = field->base.top_type;
  do_analysis((AstNode_*)field->expr);

  // Check that the field in the constructor parameter actually exists in the class.
  Type_* cls_type = field->base.top_type;
  if (!type_is(cls_type, ClassType_)) {
    error(analyzer_, node, "Trying to construct a non-class type.");
    return;
  }

  if (field->name.start) {
    bool found = false;
    for (ListNode_* n = type_as(ClassType_, cls_type)->members.head; n != NULL; n = n->next) {
      ClassTypeField_* member = list_ptr(n, ClassTypeField_);
      if (token_eq(member->name, field->name)) {
        found = true;
        break;
      }
    }

    if (!found) {
      error(analyzer_, node, "Constructor field %.*s does not exist in class %.*s.",
        field->name.length, field->name.start,
        cls_type->opt_name.length, cls_type->opt_name.start);
    }
  }

  field->base.type = field->expr->type;
}

void array_value_analysis(AstNode_* node) {
  AstArrayValueExpr_* array_expr = (AstArrayValueExpr_*)node;

  Type_* el_type = NULL;
  int index = 0;
  for (AstListNode_* n = array_expr->values.head; n != NULL; n = n->next) {
    AstExpr_* expr = (AstExpr_*)n->node;
    expr->top_type = array_expr->base.top_type;
    do_analysis(n->node);
    
    if (!el_type) {
      el_type = expr->type;
    } else if (!type_equal(el_type, expr->type)) {
      error(analyzer_, node, "Array element type at index %d does not match array type.", index);
    }
    ++index;
  }

  array_expr->base.type = make_array_ty(el_type, array_expr->values.count, analyzer_->allocator);
  if (el_type) {
    type_calcsize(array_expr->base.type);
  }
}

AnalysisRule_ analysis_rules[] = {
  [AST_CLS(AstProgram_)]                = {program_analysis},
  [AST_CLS(AstBlock_)]                  = {block_analysis},
  [AST_CLS(AstStmt_)]                   = {stmt_analysis},
  [AST_CLS(AstExpr_)]                   = {expr_analysis},
  [AST_CLS(AstPrintStmt_)]              = {print_analysis},
  [AST_CLS(AstUnaryExp_)]               = {unary_analysis},
  [AST_CLS(AstBinaryExp_)]              = {binary_analysis},
  [AST_CLS(AstPrimaryExp_)]             = {primary_analysis},
  [AST_CLS(AstReturnStmt_)]             = {return_analysis},
  [AST_CLS(AstIfStmt_)]                 = {if_analysis},
  [AST_CLS(AstAssertStmt_)]             = {assert_analysis},
  [AST_CLS(AstVarDeclStmt_)]            = {var_decl_analysis},
  [AST_CLS(AstVarExpr_)]                = {var_expr_analysis},
  [AST_CLS(AstIndexExpr_)]              = {index_expr_analysis},
  [AST_CLS(AstIdExpr_)]                 = {id_expr_analysis},
  [AST_CLS(AstAssignmentExpr_)]         = {assignment_expr_analysis},
  [AST_CLS(AstInPlaceBinaryStmt_)]      = {in_place_binary_stmt_analysis},
  [AST_CLS(AstWhileStmt_)]              = {while_stmt_analysis},
  [AST_CLS(AstFunctionDef_)]            = {function_def_analysis},
  [AST_CLS(AstFunctionBody_)]           = {function_body_analysis},
  [AST_CLS(AstFunctionParam_)]          = {function_param_analysis},
  [AST_CLS(AstFunctionCall_)]           = {function_call_analysis},
  [AST_CLS(AstFunctionCallArgs_)]       = {function_call_args_analysis},
  [AST_CLS(AstFunctionCallArg_)]        = {function_call_arg_analysis},
  [AST_CLS(AstExpressionStmt_)]         = {expression_statement_analysis},
  [AST_CLS(AstNoopExpr_)]               = {noop_analysis},
  [AST_CLS(AstNoopStmt_)]               = {noop_analysis},
  [AST_CLS(AstCleanUpTemps_)]           = {clean_up_temps_analysis},
  [AST_CLS(AstTmpDecl_)]                = {ast_tmp_decl_analysis},
  [AST_CLS(AstClassDef_)]               = {ast_class_def_analysis},
  [AST_CLS(AstClassMemberDecl_)]        = {ast_class_member_decl_analysis},
  [AST_CLS(AstClassConstructor_)]       = {ast_class_constructor_analysis},
  [AST_CLS(AstClassConstructorParam_)]  = {ast_class_constructor_param_analysis},
  [AST_CLS(AstDotExpr_)]                = {ast_dot_expr_analysis},
  [AST_CLS(AstTypeExpr_)]               = {noop_analysis},
  [AST_CLS(AstArrayValueExpr_)]         = {array_value_analysis},
};

// Static assert to make sure that all node types are accounted for.
STATIC_ASSERT(
  sizeof(analysis_rules) / sizeof(AnalysisRule_) == __AST_NODE_COUNT__,
  CHECK_ANALYSIS_COUNT);

static AnalysisRule_* get_rule(int info) {
  AnalysisRule_* ret = &analysis_rules[info];  
  assertf(ret->fn, "Could not find analysis rule for AST class: %d", info);
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