#include "ast.h"

#include "cst.h"
#include "symbol_table.h"
#include <string.h>

typedef AstNode_* (*CopyFn)(MemoryAllocator_*, AstNode_*, Scope_*);
typedef struct CopyRule_ {
  CopyFn fn;
} CopyRule_;

static CopyRule_* get_rule(int info);
// static AstNode_* do_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope);
// AstNode_* copy_ast_node(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
//   return do_copy(allocator, node, scope);
// }

AstNode_* make_ast_node(MemoryAllocator_* allocator, int cls, size_t size, struct Scope_* scope, const CstNode_* node) {
  AstNode_* ret = alloc(allocator, size);
  assertf(ret != NULL, "Cannot make AstNode: Allocator OOM");

  memset(ret, 0, size);
  ret->cls = cls;
  ret->scope = scope;
  ret->parent = node;

  if (node) {
    ret->line = ret->parent->line;
  }
  return ret;
}

AstNode_* assert_astnode_is_(AstNode_* node, int cls) {
  assertf(node->cls == cls, "AstNode class does not match %d vs. %d", node->cls, cls);
  return node;
}

#if 0
static AstNode_* alloc_from(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  size_t size = ast_node_sizes[node->cls];
  AstNode_* ret = alloc(allocator, size);
  assertf(ret != NULL, "Cannot make AstNode: Allocator OOM");
  memset(ret, 0, size);

  *ret = *node;
  ret->scope = scope;
  return ret;
}

#define do_copy(ALLOCATOR, NODE, SCOPE) do_copy_(ALLOCATOR, (AstNode_*)(NODE), SCOPE)
static AstNode_* do_copy_(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  if (!node) {
    return NULL;
  }

  return get_rule(node->cls)->fn(node, allocator, scope);
}

static AstNode_* program_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstProgram_* program = (AstProgram_*)node;
  AstProgram_* copy = (AstProgram_*)alloc_from(allocator, node, scope);
  
  copy->base.scope = scope_createfrom(scope);
  copy->block = do_copy(allocator, program->block, copy->base.scope);
  return copy;
}

static AstNode_* block_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstBlock_* block = (AstBlock_*)node;
  AstBlock_* copy = (AstBlock_*)alloc_from(allocator, node, scope);
  copy->base.scope = scope_createfrom(scope);

  astlist_copyto(&copy->statements, &block->statements, allocator, copy->base.scope);

  return copy;
}

static AstNode_* stmt_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstStmt_* stmt = (AstStmt_*)node;
  AstStmt_* copy = (AstStmt_*)alloc_from(allocator, node, scope);
  copy->stmt = do_copy(allocator, stmt->stmt, scope);
  copy->cleanup = do_copy(allocator, stmt->cleanup, scope);

  return copy;
}

static AstType_ copy_type(AstType_ other) {
  return (AstType_) { .expr = other.expr };
}

static AstExpr_ copy_exprbase(const AstExpr_ other) {
  return (AstExpr_) { 
    .base = other.base,
    .type = copy_type(other.type),
    .top_type = copy_type(other.top_type),
  };
}

static AstNode_* expr_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstExpr_* expr = (AstExpr_*)node;
  AstExpr_* copy = (AstExpr_*)alloc_from(allocator, node, scope);
  if (!expr->expr) {
    return copy;
  }

  copy->type = copy_type(expr->type);
  copy->top_type = copy_type(expr->top_type);
  copy->expr = do_copy(allocator, (AstNode_*)expr->expr, scope);

  return copy;
}

static AstNode_* print_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstPrintStmt_* stmt = (AstPrintStmt_*)node;
  AstPrintStmt_* copy = (AstPrintStmt_*)alloc_from(allocator, node, scope);
  copy->expr = do_copy(allocator, (AstNode_*)stmt->expr, scope);
  return copy;
}

static AstNode_* unary_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstUnaryExp_* expr = (AstUnaryExp_*)node;
  AstUnaryExp_* copy = (AstUnaryExp_*)alloc_from(allocator, node, scope);

  copy->base = copy_exprbase(expr->base);
  copy->expr = do_copy(allocator, node, scope);
  return copy;
}

static AstNode_* binary_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstBinaryExp_* expr = (AstBinaryExp_*)node;
  AstBinaryExp_* copy = (AstBinaryExp_*)alloc_from(allocator, node, scope);

  copy->base = copy_exprbase(expr->base);
  copy->op = expr->op;
  copy->left = do_copy(allocator, node, scope);
  copy->right = do_copy(allocator, node, scope);
  
  return copy;
}

static AstNode_* primary_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstPrimaryExp_* expr = (AstPrimaryExp_*)node;
  AstPrimaryExp_* copy = (AstPrimaryExp_*)alloc_from(allocator, node, scope);
  
  copy->base = copy_exprbase(expr->base);
  copy->value = expr->value;

  return copy;
}

static AstNode_* return_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstReturnStmt_* stmt = (AstReturnStmt_*)node;
  AstReturnStmt_* copy = (AstReturnStmt_*)alloc_from(allocator, node, scope);
  
  copy->expr = do_copy(allocator, stmt->expr, scope);

  return copy;
}

static AstNode_* if_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstIfStmt_* stmt = (AstIfStmt_*)node;
  AstIfStmt_* copy = (AstIfStmt_*)alloc_from(allocator, node, scope);

  copy->condition_expr = do_copy(allocator, stmt->condition_expr, scope);
  copy->if_stmt = do_copy(allocator, stmt->if_stmt, scope);
  astlist_copyto(&copy->elif_exprs, &stmt->elif_exprs, allocator, scope);
  astlist_copyto(&copy->elif_stmts, &stmt->elif_stmts, allocator, scope);
  copy->else_stmt = do_copy(allocator, stmt->else_stmt, scope);

  return copy;
}

static AstNode_* assert_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstAssertStmt_* stmt = (AstAssertStmt_*)node;
  AstAssertStmt_* copy = (AstAssertStmt_*)alloc_from(allocator, node, scope);
  copy->expr = do_copy(allocator, stmt->expr, scope);

  return copy;
}

static AstNode_* var_decl_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)node;
  AstVarDeclStmt_* copy = (AstVarDeclStmt_*)alloc_from(allocator, node, scope);

  copy->name = stmt->name;
  copy->decl_type = copy_type(stmt->decl_type);
  copy->expr = do_copy(allocator, stmt->expr, scope);

  frame_addvar(scope->frame, &copy->name, copy->decl_type, scope);

  return copy;
}

static AstNode_* var_expr_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstVarExpr_* expr = (AstVarExpr_*)node;
  AstVarExpr_* copy = (AstVarExpr_*)alloc_from(allocator, node, scope);

  copy->base = copy_exprbase(expr->base);
  copy->expr = do_copy(allocator, expr->expr, scope);

  return copy;
}

static AstNode_* id_expr_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstIdExpr_* expr = (AstIdExpr_*)node;
  AstIdExpr_* copy = (AstIdExpr_*)alloc_from(allocator, node, scope);

  copy->name = expr->name;
  copy->base = copy_exprbase(expr->base);

  return copy;
}

static AstNode_* index_expr_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstIndexExpr_* expr = (AstIndexExpr_*)node;
  AstIndexExpr_* copy = (AstIndexExpr_*)alloc_from(allocator, node, scope);
  
  copy->base = copy_exprbase(expr->base);
  copy->prefix = do_copy(allocator, expr->prefix, scope);
  copy->index = do_copy(allocator, expr->index, scope);
  return copy;
}

static AstNode_* assignment_expr_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstAssignmentExpr_* expr = (AstAssignmentExpr_*)node;
  AstAssignmentExpr_* copy = (AstAssignmentExpr_*)alloc_from(allocator, node, scope);

  copy->base = copy_exprbase(expr->base);
  copy->left = do_copy(allocator, expr->left, scope);
  copy->right = do_copy(allocator, expr->right, scope);
  return copy;
}

static AstNode_* in_place_binary_stmt_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstInPlaceBinaryStmt_* expr = (AstInPlaceBinaryStmt_*)node;
  AstInPlaceBinaryStmt_* copy = (AstInPlaceBinaryStmt_*)alloc_from(allocator, node, scope);

  copy->base = copy_exprbase(expr->base);
  copy->op = expr->op;
  copy->bin_op = expr->bin_op;
  copy->left = do_copy(allocator, expr->left, scope);
  copy->right = do_copy(allocator, expr->right, scope);

  return copy;
}

static AstNode_* while_stmt_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstWhileStmt_* stmt = (AstWhileStmt_*)node;
  AstWhileStmt_* copy = (AstWhileStmt_*)alloc_from(allocator, node, scope);

  copy->condition_expr = do_copy(allocator, stmt->condition_expr, scope);
  copy->block_stmt = do_copy(allocator, stmt->block_stmt, scope);

  return copy;
}

static AstNode_* for_stmt_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstForStmt_* stmt = (AstForStmt_*)node;
  AstForStmt_* copy = (AstForStmt_*)alloc_from(allocator, node, scope);

  copy->opt_var_decl = do_copy(allocator, stmt->opt_var_decl, scope);
  copy->opt_condition_expr = do_copy(allocator, stmt->opt_condition_expr, scope);
  copy->opt_step_expr = do_copy(allocator, stmt->opt_step_expr, scope);
  copy->block_stmt = do_copy(allocator, stmt->block_stmt, scope);

  return copy;
}

static AstNode_* expression_statement_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstExpressionStmt_* stmt = (AstExpressionStmt_*)node;
  AstExpressionStmt_* copy = (AstExpressionStmt_*)alloc_from(allocator, node, scope);
  
  copy->expr = do_copy(allocator, stmt->expr, scope);
  return copy;
}

static AstNode_* function_def_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstFunctionDef_* def = (AstFunctionDef_*)node;
  AstFunctionDef_* copy = (AstFunctionDef_*)alloc_from(allocator, node, scope);

  copy->base = copy_exprbase(def->base);

  def->base.type = def->fn_symbol->ty;
  do_copy(analyzer, (AstNode_*)def->body, scope);
}

static AstNode_* generic_function_def_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  assert(false && "unimplemented");
}

static AstNode_* function_body_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstFunctionBody_* body = (AstFunctionBody_*)node;
  FunctionType_* fn_type = type_as(FunctionType_, body->fn_symbol->ty);
  FunctionSymbol_* fn = &body->fn_symbol->fn;
  fn_type->ret_ty = body->return_type;

  type_resolve(fn_type->ret_ty, n->scope);
  type_calcsize(fn_type->ret_ty);

  for (AstListNode_* n = body->function_params.head; n != NULL; n = n->next) {
    do_copy(analyzer, n->node);
  }

  do_copy(analyzer, body->stmt);
}

static AstNode_* function_param_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstFunctionParam_* param = (AstFunctionParam_*)node;

  // TODO: implement parameter type inference.
  Type_* val_type = type_valtype(param->type);
  type_resolve(param->type, n->scope);
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

static AstNode_* function_call_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstFunctionCall_* call = (AstFunctionCall_*)node;
  call->prefix->top_type = call->base.top_type;
  do_copy(analyzer, (AstNode_*)call->prefix);

  if (!type_is(call->prefix->type, FunctionType_)) {
    error(analyzer_, node, "Function call is not calling a function");
    return;
  }

  FunctionType_* fn_type = type_as(FunctionType_, call->prefix->type);
  call->base.type = fn_type->ret_ty;
  call->args->fn_type = (Type_*)fn_type;

  do_copy(analyzer, (AstNode_*)call->args);
}

static AstNode_* function_call_arg_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstFunctionCallArg_* arg = AST_CAST(AstFunctionCallArg_, node);

  arg->expr->top_type = arg->base.top_type;
  do_copy(analyzer, (AstNode_*)arg->expr);
  arg->base.type = arg->expr->type;
}

static AstNode_* function_call_args_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstFunctionCallArgs_* args = AST_CAST(AstFunctionCallArgs_, node);
  FunctionType_* fn_type = type_as(FunctionType_, args->fn_type);

  if (fn_type->params.count > UINT8_MAX) {
    error(analyzer_, node, "Parameter count exceeded maximum of 255.");
  }

  ListNode_* param_node = fn_type->params.head;
  for (AstListNode_* n = args->args.head; n != NULL; n = n->next) {
    Type_* param_type = list_val(param_node, Type_*);

    AS_EXPR(n->node)->top_type = param_type;
    do_copy(analyzer, n->node);
    Type_* expr_type = AS_EXPR(n->node)->type;

    if (!type_isassignable_to(expr_type, type_valtype(param_type))) {
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

static AstNode_* noop_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {}

static AstNode_* clean_up_temps_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {

}

static AstNode_* ast_tmp_decl_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstTmpDecl_* decl = (AstTmpDecl_*)node;
  decl->expr->top_type = decl->base.top_type;
  do_copy(analyzer, (AstNode_*)decl->expr);
  decl->base.type = decl->expr->type;
}

static AstNode_* ast_class_def_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstClassDef_* def = (AstClassDef_*)node;

  for (AstListNode_* n = def->members.head; n != NULL; n = n->next) {
    do_copy(analyzer, n->node);
  }

  //if (semantictype_hascycle(&def->class_type)) {
  //  error(analyzer_, node, "struct \"%.*s\" has a cycle", cls_sym->name.length, cls_sym->name.start);
  //  return;
  //}

  type_calcsize(def->class_type);
  type_class_calcoffsets(def->class_type);
}

static AstNode_* ast_class_member_decl_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstClassMemberDecl_* decl = (AstClassMemberDecl_*)node;
  Type_* type = decl->field_type;

  if (!type_resolve(type, node->scope)) {
    error(analyzer_, node,
      "Could not find type '%.*s' in class member declaration",
      type->opt_name.length, type->opt_name.start);
  }

  if (decl->opt_expr) {
    AstExpr_* expr = decl->opt_expr;
    do_copy(analyzer, (AstNode_*)expr);

    if (!type_isval(expr->type)) {
      error(analyzer_, node, "struct member field default value must be a value.");
      return;
    }
  }
}

static AstNode_* ast_dot_expr_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstDotExpr_* expr = AST_CAST(AstDotExpr_, node);

  expr->prefix->top_type = expr->base.top_type;
  do_copy(analyzer, (AstNode_*)expr->prefix);

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

static AstNode_* ast_class_constructor_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstClassConstructor_* constructor = (AstClassConstructor_*)node;
  constructor->prefix->top_type = constructor->base.top_type;
  do_copy(analyzer, (AstNode_*)constructor->prefix);

  if (type_isunknown(constructor->prefix->type)) {
    Symbol_* cls_sym = scope_search_to_root(node->scope, &constructor->name);

    Type_* type = constructor->base.type;
    if (!type_resolve(type, node->scope)) {
      error(analyzer_, node, "Could not find class %.*s", type->opt_name.length, type->opt_name.start);
      return;
    } else if (!type) {
      constructor->base.type = cls_sym->ty;
    }
  } else {
    constructor->base.type = constructor->prefix->type;
    type_resolve(constructor->base.type, node->scope);
  }

  for (AstListNode_* n = constructor->params.head; n != NULL; n = n->next) {
    AstClassConstructorParam_* field = AST_CAST(AstClassConstructorParam_, n->node);
    field->base.top_type = constructor->base.type;
    do_copy(analyzer, (AstNode_*)field);
  }
}

static AstNode_* ast_class_constructor_param_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstClassConstructorParam_* field = (AstClassConstructorParam_*)node;
  field->expr->top_type = field->base.top_type;
  do_copy(analyzer, (AstNode_*)field->expr);

  // Check that the field in the constructor parameter actually exists in the class.
  Type_* cls_type = type_valtype(field->base.top_type);
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

static AstNode_* array_value_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstArrayValueExpr_* array_expr = (AstArrayValueExpr_*)node;

  Type_* el_type = NULL;
  int index = 0;
  for (AstListNode_* n = array_expr->values.head; n != NULL; n = n->next) {
    AstExpr_* expr = (AstExpr_*)n->node;
    expr->top_type = array_expr->base.top_type;
    do_copy(analyzer, n->node);

    if (!el_type) {
      el_type = expr->type;
    } else if (!type_isequal(el_type, expr->type)) {
      error(analyzer_, node, "Array element type at index %d does not match array type.", index);
    }
    ++index;
  }

  array_expr->base.type = make_array_ty(el_type, array_expr->values.count, analyzer_->allocator);
  if (el_type) {
    type_calcsize(array_expr->base.type);
  }
}

static AstNode_* range_expr_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
}

static AstNode_* type_def_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstTypeDef_* type_def = (AstTypeDef_*)node;

  for (AstListNode_* n = type_def->members.head; n != NULL; n = n->next) {
    do_copy(analyzer, n->node);
  }

  type_resolve(type_def->type, node->scope);
  printf("%.*s ::= ", type_def->name.length, type_def->name.start);
  print_type(type_def->type);
  printf("\n");
}

static AstNode_* type_member_decl_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  TypeMemberDecl_* member_decl = (TypeMemberDecl_*)node;
  type_resolve(member_decl->type, node->scope);
}

static AstNode_* generic_params_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstGenericParams_* params = (AstGenericParams_*)node;
}

static AstNode_* index_or_generic_args_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstIndexOrGenericArgs_* expr = (AstIndexOrGenericArgs_*)node;
  if (type_is(expr->base.top_type, GenericType_)) {
    Type_* generic_ty = expr->base.top_type;
    ListOf_(TypeArgument_) type_args;
    MemoryAllocator_* allocator = analyzer_->allocator;
    list_of(&type_args, TypeArgument_, allocator);

    for (AstListNode_* n = expr->args.head; n != NULL; n = n->next) {
      AstTypeExpr_* expr = AST_CAST(AstTypeExpr_, n->node);
      do_copy(analyzer, n->node);
      type_resolve(expr->base.type, node->scope);
      TypeArgument_ arg = {
        .is_type = true,
        .type = expr->base.type
      };
      list_push(&type_args, &arg);
    }

    expr->base.type = generictype_specialize(generic_ty, &type_args, generic_ty->opt_name, allocator, node->scope);

    list_clear(&type_args);
  }
}

static AstNode_* index_or_type_expr_copy(MemoryAllocator_* allocator, AstNode_* node, Scope_* scope) {
  AstIndexOrTypeExpr_* expr = (AstIndexOrTypeExpr_*)node;
  expr->prefix->top_type = expr->base.top_type;
  do_copy(analyzer, (AstNode_*)expr->prefix);

  if (type_is(expr->prefix->type, GenericType_)) {
    expr->base.base.cls = AST_CLS(AstTypeExpr_);
    expr->index_args->base.top_type = expr->prefix->type;
    do_copy(analyzer, (AstNode_*)expr->index_args);
    expr->type_expr.base.type = expr->index_args->base.type;
  } else {
    assert_type_is(expr->prefix->type, TYPE_CLS(ArrayType_));
    expr->base.base.cls = AST_CLS(AstVarExpr_);

    AstIndexExpr_* index_expr = MAKE_AST_EXPR(analyzer_->allocator, AstIndexExpr_, node->scope, node->line);
    index_expr->prefix = expr->prefix;

    AstNode_* index = expr->index_args->args.head->node;
    index_expr->index = (AstExpr_*)index;

    expr->var_index_expr.expr = (AstExpr_*)index_expr;
    do_copy(analyzer, node);
  }
}

CopyRule_ copy_rules[] = {
  [AST_CLS(AstProgram_)] = {program_copy},
  [AST_CLS(AstBlock_)] = {block_copy},
  [AST_CLS(AstStmt_)] = {stmt_copy},
  [AST_CLS(AstExpr_)] = {expr_copy},
  [AST_CLS(AstPrintStmt_)] = {print_copy},
  [AST_CLS(AstUnaryExp_)] = {unary_copy},
  [AST_CLS(AstBinaryExp_)] = {binary_copy},
  [AST_CLS(AstPrimaryExp_)] = {primary_copy},
  [AST_CLS(AstReturnStmt_)] = {return_copy},
  [AST_CLS(AstIfStmt_)] = {if_copy},
  [AST_CLS(AstAssertStmt_)] = {assert_copy},
  [AST_CLS(AstVarDeclStmt_)] = {var_decl_copy},
  [AST_CLS(AstVarExpr_)] = {var_expr_copy},
  [AST_CLS(AstIndexExpr_)] = {index_expr_copy},
  [AST_CLS(AstIdExpr_)] = {id_expr_copy},
  [AST_CLS(AstAssignmentExpr_)] = {assignment_expr_copy},
  [AST_CLS(AstInPlaceBinaryStmt_)] = {in_place_binary_stmt_copy},
  [AST_CLS(AstWhileStmt_)] = {while_stmt_copy},
  [AST_CLS(AstForStmt_)] = {for_stmt_copy},
  [AST_CLS(AstFunctionDef_)] = {function_def_copy},
  [AST_CLS(AstGenericFunctionDef_)] = {generic_function_def_copy},
  [AST_CLS(AstFunctionBody_)] = {function_body_copy},
  [AST_CLS(AstFunctionParam_)] = {function_param_copy},
  [AST_CLS(AstFunctionCall_)] = {function_call_copy},
  [AST_CLS(AstFunctionCallArgs_)] = {function_call_args_copy},
  [AST_CLS(AstFunctionCallArg_)] = {function_call_arg_copy},
  [AST_CLS(AstExpressionStmt_)] = {expression_statement_copy},
  [AST_CLS(AstNoopExpr_)] = {noop_copy},
  [AST_CLS(AstNoopStmt_)] = {noop_copy},
  [AST_CLS(AstCleanUpTemps_)] = {clean_up_temps_copy},
  [AST_CLS(AstTmpDecl_)] = {ast_tmp_decl_copy},
  [AST_CLS(AstClassDef_)] = {ast_class_def_copy},
  [AST_CLS(AstClassMemberDecl_)] = {ast_class_member_decl_copy},
  [AST_CLS(AstClassConstructor_)] = {ast_class_constructor_copy},
  [AST_CLS(AstClassConstructorParam_)] = {ast_class_constructor_param_copy},
  [AST_CLS(AstDotExpr_)] = {ast_dot_expr_copy},
  [AST_CLS(AstTypeExpr_)] = {noop_copy},
  [AST_CLS(AstArrayValueExpr_)] = {array_value_copy},
  [AST_CLS(AstRangeExpr_)] = {range_expr_copy},
  [AST_CLS(AstTypeDef_)] = {type_def_copy},
  [AST_CLS(TypeMemberDecl_)] = {type_member_decl_copy},
  [AST_CLS(AstGenericParam_)] = {noop_copy},
  [AST_CLS(AstGenericParams_)] = {generic_params_copy},
  [AST_CLS(AstIndexOrTypeExpr_)] = {index_or_type_expr_copy},
  [AST_CLS(AstIndexOrGenericArgs_)] = {index_or_generic_args_copy},
};

#define AST_SIZEOF(NODE) [AST_CLS(NODE)] = sizeof(NODE)

size_t ast_node_sizes[__AST_NODE_COUNT__] = {
  ASTNODE_LIST(AST_SIZEOF)
};

// Static assert to make sure that all node types are accounted for.
STATIC_ASSERT(
  sizeof(copy_rules) / sizeof(CopyRule_) == __AST_NODE_COUNT__,
  CHECK_COPY_COUNT);

static CopyRule_* get_rule(int info) {
  CopyRule_* ret = &copy_rules[info];
  assertf(ret->fn, "Could not find copy rule for AST class: %d", info);
  return ret;
}
#endif