#include "parser.h"
#include "ast.h"
#include "cst.h"
#include "type_parser.h"
#include "map.h"
#include "memory.h"
#include "symbol_table.h"
#include "object.h"
#include "type_expr.h"
#include "errors.h"

#include <memory.h>
#include <stdarg.h>
#include <string.h>

#pragma warning(3 : 4062)

static bool block_follow(AstParser_* parser, Scanner_* scanner, bool withuntil);
static AstNode_* statement_list(AstParser_* parser, Scanner_* scanner, AstList_* statements, Scope_* scope);

AstList_* astlist_create(struct MemoryAllocator_* allocator) {
  AstList_* ret = alloc(allocator, sizeof(AstList_));
  memset(ret, 0, sizeof(AstList_));
  ret->allocator = allocator;
  return ret;
}

void astlist_init(AstList_* list, struct MemoryAllocator_* allocator) {
  memset(list, 0, sizeof(AstList_));
  list->allocator = allocator;
}

void astlist_clear(AstList_* list) {
  struct MemoryAllocator_* allocator = list->allocator;

  AstListNode_* cur = list->head;
  while (cur) {
    AstListNode_* next = cur->next;
    dealloc(allocator, cur);
    cur = next;
  }

  list->count = 0;
}

void astlist_destroy(AstList_** list) {
  struct MemoryAllocator_* allocator = (*list)->allocator;

  astlist_clear(*list);
  dealloc(allocator, *list);
  *list = NULL;
}

void astlist_append(AstList_* list, AstNode_* node) {
  if (!node) return;

  struct MemoryAllocator_* allocator = list->allocator;
  AstListNode_* n = alloc(allocator, sizeof(AstListNode_));
  n->node = node;

  if (!list->head) {
    list->head = n;
  }

  if (list->tail) {
    list->tail->next = n;
  }

  list->tail = n;
  ++list->count;
}

typedef struct AstParseWork_ {
  Scope_* origin_scope;
  CstNode_* origin_node;
  AstNode_* parsed_node;
  Token_ name;
} AstParseWork_;

typedef AstNode_* (*ParseFn)(AstParser_*, CstNode_*, Scope_*, MemoryAllocator_*);
typedef struct ParseRule_ {
  ParseFn fn;
} ParseRule_;

static ParseRule_* get_rule(int info);

void ast_parser_init(AstParser_* parser, MemoryAllocator_* allocator) {
  *parser = (AstParser_){ 0 };
  list_of(&parser->work_queue, AstParseWork_, NULL);
  parser->generic_nodes = hashmap_create();
  parser->allocator = allocator;
  parser->errors = alloc_ty(allocator, ErrorsContainer_);
  errorscontainer_init(parser->errors, allocator);
}

void ast_parser_clear(AstParser_* parser) {
  errorscontainer_clear(parser->errors);
  dealloc(parser->allocator, parser->errors);
  list_clear(&parser->work_queue);
  *parser = (AstParser_){ 0 };
}

static AstNode_* do_parse_(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  return get_rule(node->cls)->fn(parser, node, scope, allocator);
}
#define do_parse(PARSER, NODE, SCOPE, ALLOCATOR) do_parse_(PARSER, (CstNode_*)(NODE), SCOPE, ALLOCATOR)

static AstClassDef_* init_ast_class_def(ClassType_* class_type, Token_ name, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  AstClassDef_* ret = (AstClassDef_*)MAKE_AST_NODE(allocator, AstClassDef_, scope, node);
  ret->name = name;
  ret->class_type = (Type_*)class_type;
  frame_addclass(scope->frame, ret->class_type, scope);
  astlist_init(&ret->members, allocator);
  astlist_init(&ret->base.specializations, allocator);

  return ret;
}

static AstClassDef_* generate_class_def(AstParser_* parser, ClassType_* class_type, CstClassDef_* class_def, Scope_* scope, MemoryAllocator_* allocator) {
  AstClassDef_* ret = init_ast_class_def(class_type, class_def->name, (CstNode_*)class_def, scope, allocator);

  ListNode_* member_n = class_type->members.head;
  for (CstListNode_* n = class_def->members.head; n != NULL; n = n->next) {
    CstClassMemberDecl_* decl = (CstClassMemberDecl_*)n->node;
    AstClassMemberDecl_* ast = MAKE_AST_NODE(allocator, AstClassMemberDecl_, class_type->scope, decl);
    ast->name = decl->name;
    if (decl->opt_expr) {
      ast->opt_expr = (AstExpr_*)do_parse(parser, decl->opt_expr, scope, allocator);
    }
    ast->field_type = list_val(member_n, Type_*);
    astlist_append(&ret->members, (AstNode_*)ast);

    member_n = member_n->next;
  }

  return ret;
}

static void add_work(AstParser_* parser, AstParseWork_* item) {
  list_push(&parser->work_queue, item);
}

static void do_work(AstParser_* parser, AstParseWork_* item) {
  MemoryAllocator_* allocator = parser->allocator;
  Symbol_* sym = scope_find(item->origin_scope, &item->name);
  assertf(sym, "Could not find symbol: %.*s", item->name.length, item->name.start);
  
  switch (sym->type) {
    case SYMBOL_CLS_FN:
    {
      assert(false && "unimplemented");

      // Find the parent function declaration holding all the generic implementations.
      const TypeExpr_* type = sym->type_expr;
      AstFunctionPrototype_* function_proto = NULL;
      hashmap_get(parser->generic_nodes, sym, sizeof(Symbol_*), (uintptr_t*)&function_proto);

      assert(function_proto && "Could not find function prototype.");
      assert(function_proto->base.cls == AST_CLS(AstFunctionPrototype_));

      // Generate the function generic parameter types.
      {
        ListOf_(TypeExpr_*) type_args = { 0 };
        list_of(&type_args, TypeExpr_*, memallocator_default());

        for (ListNode_* n = type->params.head; n != NULL; n = n->next) {
          const TypeExpr_* type_expr = list_val(n, TypeExpr_*);
          const Type_* arg_type = resolve_typeexpr(type_expr, &type_args, function_proto->base.scope, parser->errors, allocator);
          list_push(&type_args, arg_type);
        }

        //function_type = resolve_typeexpr(type, &type_args, allocator);
        list_clear(&type_args);
      }

      for (AstListNode_* n = function_proto->defs.head; n != NULL; n = n->next) {
        AstFunctionDef_* def = AST_CAST(AstFunctionDef_, n->node);
      }

      Scope_* decl_scope = function_proto->base.scope;
      CstFunctionDef_* parent = CST_CAST(CstFunctionDef_, function_proto->parent);

      AstFunctionDef_* def = MAKE_AST_NODE(parser->allocator, AstFunctionDef_, decl_scope, parent);

      Type_* function_type = NULL;
      {
        ListOf_(TypeExpr_*) type_args = { 0 };
        list_of(&type_args, TypeExpr_*, memallocator_default());

        for (ListNode_* n = type->params.head; n != NULL; n = n->next) {
          list_push(&type_args, list_val(n, TypeExpr_*));
        }

        function_type = (Type_*)resolve_typeexpr(type, &type_args, decl_scope, parser->errors, allocator);
        list_clear(&type_args);
      }

      Frame_* fn_frame = frame_createfrom(decl_scope->frame, decl_scope, function_type);
      Scope_* fn_scope = fn_frame->scope;
      scope_addexisting(decl_scope, fn_frame->fn_symbol);
      def->fn_symbol = fn_frame->fn_symbol;

      // frame_addparam(fn_frame, );

      def->body = do_parse(parser, parent->body, fn_scope, allocator);

      astlist_append(&function_proto->defs, (AstNode_*)def);
      break;
    }

    case SYMBOL_CLS_CLASS:
    {
      ClassType_* class_type = (ClassType_*)sym->ty;
      AstClassDef_* class_def = AST_CAST(AstClassDef_, item->parsed_node);
      ListOf_(Type_*)* specializations = &sym->ty->specializations;
      for (ListNode_* n = specializations->head; n != NULL; n = n->next) {
        ClassType_* specialization = type_as(ClassType_, list_val(n, Type_*));
        AstClassDef_* new_class = generate_class_def(
          parser, specialization, (CstClassDef_*)item->origin_node, specialization->scope->parent, allocator);
        astlist_append(&class_def->base.specializations, (AstNode_*)new_class);
      }

      break;
    }

    default:
      assertf(false, "unimplemented");
  }
}

AstNode_* parse_ast(AstParser_* parser, const struct CstNode_* node, struct Scope_* scope) {
  AstProgram_* root = MAKE_AST_NODE(parser->allocator, AstProgram_, scope, node);
  CstProgram_* cst = CST_CAST(CstProgram_, node);
  root->block = (AstBlock_*)do_parse(parser, cst->block, scope, parser->allocator);

  while (parser->work_queue.count) {
    AstParseWork_ work_item = { 0 };
    list_pop(&parser->work_queue, &work_item, sizeof(AstParseWork_));
    do_work(parser, &work_item);
  }

  return (AstNode_*)root;
}

static AstNode_* cst_block_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstBlock_* block = (CstBlock_*)node;
  AstBlock_* ret = (AstBlock_*)MAKE_AST_NODE(allocator, AstBlock_, scope_createfrom(scope), allocator);
  astlist_init(&ret->statements, allocator);

  for (CstListNode_* n = block->statements.head; n != NULL; n = n->next) {
    astlist_append(&ret->statements, do_parse(parser, n->node, ret->base.scope, allocator));
  }

  if ((int64_t)ret->base.scope->frame->var_count > UINT16_MAX) {
    assert(false && "Number of local variables exceeded max of 64K.");
  }

  return (AstNode_*)ret;
}

static AstNode_* cst_print_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstPrintStmt_* stmt = (CstPrintStmt_*)node;
  AstPrintStmt_* ret = MAKE_AST_NODE(allocator, AstPrintStmt_, scope, node);

  ret->expr = (AstExpr_*)(do_parse(parser, stmt->expr, ret->base.scope, allocator));

  return (AstNode_*)ret;
}

static AstNode_* cst_unary_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstUnaryExp_* expr = (CstUnaryExp_*)node;
  AstUnaryExp_* ret = MAKE_AST_NODE(allocator, AstUnaryExp_, scope, node);
  ret->op = expr->op;
  ret->expr = (AstExpr_*)(do_parse(parser, expr->expr, scope, allocator));

  return (AstNode_*)ret;
}

static AstNode_* cst_binary_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstBinaryExp_* expr = (CstBinaryExp_*)node;
  AstBinaryExp_* ret = MAKE_AST_NODE(allocator, AstBinaryExp_, scope, node);

  ret->op = expr->op;
  ret->left = (AstExpr_*)(do_parse(parser, expr->left, scope, allocator));
  ret->right = (AstExpr_*)(do_parse(parser, expr->right, scope, allocator));

  return (AstNode_*)ret;
}

static AstNode_* cst_primary_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstPrimaryExp_* exp = (CstPrimaryExp_*)node;
  AstPrimaryExp_* ret = MAKE_AST_NODE(allocator, AstPrimaryExp_, scope, node);
  
  ret->value = exp->value;
  
  switch (exp->type) {
    case TK_BOOL:        ret->base.type = (Type_*)Bool_Ty; break;
    case TK_INT:         ret->base.type = (Type_*)Int_Ty; break;
    case TK_UINT:        ret->base.type = (Type_*)Uint_Ty; break;
    case TK_FLOAT:       ret->base.type = (Type_*)Float_Ty; break;
    case TK_DOUBLE:      ret->base.type = (Type_*)Double_Ty; break;
    case TK_STRING_TYPE: ret->base.type = (Type_*)String_Ty; break;
    case TK_NIL:         ret->base.type = (Type_*)Nil_Ty; break;
    default:
      error_panic(parser->errors, node->line, "Unknown type token %d when parsing primary expression.", exp->type);
  }

  return (AstNode_*)ret;
}

static AstNode_* cst_return_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstReturnStmt_* stmt = (CstReturnStmt_*)node;
  AstReturnStmt_* ret = MAKE_AST_NODE(allocator, AstReturnStmt_, scope, node);

  ret->expr = (AstExpr_*)(do_parse(parser, stmt->expr, scope, allocator));

  return (AstNode_*)ret;
}

static AstNode_* cst_if_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstIfStmt_* stmt = (CstIfStmt_*)node;
  AstIfStmt_* ret = MAKE_AST_NODE(allocator, AstIfStmt_, scope, node);

  astlist_init(&ret->elif_exprs, allocator);
  astlist_init(&ret->elif_stmts, allocator);

  ret->condition_expr = (AstExpr_*)(do_parse(parser, stmt->condition_expr, scope, allocator));
  ret->if_stmt = do_parse(parser, stmt->if_stmt, scope, allocator);

  for (CstListNode_* n = stmt->elif_exprs.head; n != NULL; n = n->next) {
    astlist_append(&ret->elif_exprs, do_parse(parser, n->node, scope, allocator));
  }
  for (CstListNode_* n = stmt->elif_stmts.head; n != NULL; n = n->next) {
    astlist_append(&ret->elif_stmts, do_parse(parser, n->node, scope, allocator));
  }

  return (AstNode_*)ret;
}

static AstNode_* cst_assert_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstAssertStmt_* stmt = (CstAssertStmt_*)node;
  AstAssertStmt_* ret = MAKE_AST_NODE(allocator, AstAssertStmt_, scope, node);
  ret->expr = (AstExpr_*)(do_parse(parser, stmt->expr, scope, allocator));

  return (AstNode_*)ret;
}

static AstNode_* cst_var_decl_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstVarDeclStmt_* stmt = (CstVarDeclStmt_*)node;
  AstVarDeclStmt_* ret = MAKE_AST_NODE(allocator, AstVarDeclStmt_, scope, node);

  ret->name = stmt->name;
  
  if (stmt->opt_type) {
    const TypeExpr_* decl_type = parse_type(stmt->opt_type, allocator);
    ret->decl_type = (Type_*)resolve_typeexpr(decl_type, NULL, scope, parser->errors, allocator);
  } else if (!stmt->expr) {
    assertf(ret->expr, "Expected an expression for a deduced type variable.");
    ret->decl_type = ret->expr->type;
  }

  Symbol_* var_sym = frame_addvar(scope->frame, &ret->name, ret->decl_type, scope);
  if (stmt->expr) {
    ret->expr = (AstExpr_*)(do_parse(parser, stmt->expr, scope, allocator));
    if (!stmt->opt_type) {
      var_sym->ty = ret->expr->type;
      ret->decl_type = ret->expr->type;
    }
  }

  return (AstNode_*)ret;
}

static AstNode_* cst_var_expr_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstVarExpr_* expr = (CstVarExpr_*)node;
  AstVarExpr_* ret = MAKE_AST_NODE(allocator, AstVarExpr_, scope, node);
  ret->expr = (AstExpr_*)(do_parse(parser, expr->expr, scope, allocator));
  ret->base.type = ret->expr->type;
  return (AstNode_*)ret;
}

static AstNode_* cst_id_expr_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstIdExpr_* expr = (CstIdExpr_*)node;
  AstIdExpr_* ret = MAKE_AST_NODE(allocator, AstIdExpr_, scope, allocator);
  ret->name = expr->name;

  Symbol_* id_symbol = scope_find(scope, &expr->name);
  if (id_symbol) {
    ret->base.type = id_symbol->ty;
  } else {
    error_add(parser->errors, node->line, "Could not find symbol '%.*s'", expr->name.length, expr->name.start);
  }

  return (AstNode_*)ret;
}

static AstNode_* cst_index_expr_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstIndexExpr_* expr = (CstIndexExpr_*)node;
  AstIndexExpr_* ret = MAKE_AST_NODE(allocator, AstIndexExpr_, scope, node);
  ret->prefix = (AstExpr_*)(do_parse(parser, expr->prefix, scope, allocator));
  ret->index = (AstExpr_*)(do_parse(parser, expr->index, scope, allocator));
  
  return (AstNode_*)ret;
}

static AstNode_* cst_assignment_expr_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstAssignmentExpr_* expr = (CstAssignmentExpr_*)node;
  AstAssignmentExpr_* ret = MAKE_AST_NODE(allocator, AstAssignmentExpr_, scope, node);
  ret->left = (AstExpr_*)(do_parse(parser, expr->left, scope, allocator));
  ret->right = (AstExpr_*)(do_parse(parser, expr->right, scope, allocator));
  ret->base.type = ret->left->type;

  return (AstNode_*)ret;
}

static AstNode_* cst_in_place_binary_stmt_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstInPlaceBinaryStmt_* expr = (CstInPlaceBinaryStmt_*)node;
  AstInPlaceBinaryStmt_* ret = MAKE_AST_NODE(allocator, AstInPlaceBinaryStmt_, scope, node);
  ret->op = expr->op;
  ret->bin_op = expr->bin_op;
  ret->left = (AstExpr_*)(do_parse(parser, expr->left, scope, allocator));
  ret->right = (AstExpr_*)(do_parse(parser, expr->right, scope, allocator));

  return (AstNode_*)ret;
}

static AstNode_* cst_while_stmt_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstWhileStmt_* stmt = (CstWhileStmt_*)node;
  AstWhileStmt_* ret = MAKE_AST_NODE(allocator, AstWhileStmt_, scope, node);
  ret->condition_expr = (AstExpr_*)do_parse(parser, stmt->condition_expr, scope, allocator);
  ret->block_stmt = do_parse(parser, stmt->block_stmt, scope, allocator);
  return (AstNode_*)ret;
}

static AstNode_* cst_for_stmt_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstForStmt_* stmt = (CstForStmt_*)node;
  AstForStmt_* ret = MAKE_AST_NODE(allocator, AstForStmt_, scope_createfrom(scope), node);
  
  if (stmt->opt_var_decl) {
    ret->opt_var_decl = do_parse(parser, stmt->opt_var_decl, ret->base.scope, allocator);
  }

  if (stmt->opt_condition_expr) {
    ret->opt_condition_expr = (AstExpr_*)(do_parse(parser, stmt->opt_condition_expr, ret->base.scope, allocator));
  }

  if (stmt->opt_step_expr) {
    ret->opt_step_expr = (AstExpr_*)(do_parse(parser, stmt->opt_step_expr, ret->base.scope, allocator));
  }

  ret->block_stmt = do_parse(parser, stmt->block_stmt, ret->base.scope, allocator);

  return (AstNode_*)ret;
}

static AstNode_* cst_expression_statement_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstExpressionStmt_* stmt = (CstExpressionStmt_*)node;
  AstExpressionStmt_* ret = MAKE_AST_NODE(allocator, AstExpressionStmt_, scope, node);
  ret->expr = (AstExpr_*)do_parse(parser, stmt->expr, scope, allocator);
  
  return (AstNode_*)ret;
}

static AstNode_* cst_function_def_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstFunctionDef_* def = (CstFunctionDef_*)node;
  AstFunctionPrototype_* ret = MAKE_AST_NODE(allocator, AstFunctionPrototype_, scope, node);

  const TypeExpr_* function_ty = parse_type(node, allocator);

/*
  CstClassDef_* def = (CstClassDef_*)node;
  AstClassDef_* ret = (AstClassDef_*)MAKE_AST_NODE(allocator, AstClassDef_, scope, node);
  ret->name = def->name;

  const TypeExpr_* class_type_expr = parse_type((CstNode_*)def, allocator);
  ret->class_type = (Type_*)resolve_typeexpr(
    class_type_expr, NULL, scope, parser->errors, allocator);
  frame_addclass(scope->frame, ret->class_type, scope);
  astlist_init(&ret->members, allocator);

  AstParseWork_ item = {
    .origin_scope = scope,
    .origin_node = node,
    .parsed_node = (AstNode_*)ret,
    .name = ret->name,
  };

  add_work(parser, &item);

  return (AstNode_*)ret;
  
  */

  // hashmap_set(parser->generic_nodes, function_symbol, sizeof(Symbol_*), ret);
  assert(false && "unimplemented");

  return (AstNode_*)ret;
}

static AstNode_* cst_function_param_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstFunctionParam_* param = (CstFunctionParam_*)node;
  AstFunctionParam_* ret = NULL;
  assert(false && "unimplemented");
  return (AstNode_*)ret;
}

static AstNode_* cst_function_call_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstFunctionCall_* call = (CstFunctionCall_*)node;
  AstFunctionCall_* ret = NULL;
  assert(false && "unimplemented");
  return (AstNode_*)ret;
}

static AstNode_* cst_function_call_args_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstFunctionCallArgs_* args = (CstFunctionCallArgs_*)node;
  AstFunctionCallArgs_* ret = NULL;
  assert(false && "unimplemented");
  return (AstNode_*)ret;
}

static AstNode_* noop_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  return NULL;
}

static AstNode_* cst_class_def_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstClassDef_* def = (CstClassDef_*)node;
  AstClassDef_* ret = (AstClassDef_*)MAKE_AST_NODE(allocator, AstClassDef_, scope, node);
  ret->name = def->name;

  const TypeExpr_* class_type_expr = parse_type((CstNode_*)def, allocator);
  ret->class_type = (Type_*)resolve_typeexpr(
    class_type_expr, NULL, scope, parser->errors, allocator);
  frame_addclass(scope->frame, ret->class_type, scope);
  astlist_init(&ret->members, allocator);

  AstParseWork_ item = {
    .origin_scope = scope,
    .origin_node = node,
    .parsed_node = (AstNode_*)ret,
    .name = ret->name,
  };

  add_work(parser, &item);

  return (AstNode_*)ret;
}

static AstNode_* cst_class_member_decl_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstClassMemberDecl_* decl = (CstClassMemberDecl_*)node;
  AstClassMemberDecl_* ret = (AstClassMemberDecl_*)MAKE_AST_NODE(allocator, AstClassMemberDecl_, scope, node);
  ret->name = decl->name;

  Symbol_* field_sym = scope_find(scope, &ret->name);
  ret->field_type = field_sym->ty;
  return (AstNode_*)ret;
}

static AstNode_* cst_dot_expr_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstDotExpr_* expr = (CstDotExpr_*)node;
  AstDotExpr_* ret = MAKE_AST_NODE(allocator, AstDotExpr_, scope, node);
  ret->prefix = (AstExpr_*)do_parse(parser, expr->prefix, scope, allocator);
  ret->id = expr->id;

  return (AstNode_*)ret;
}

static AstNode_* cst_class_constructor_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstClassConstructor_* constructor = (CstClassConstructor_*)node;
  AstClassConstructor_* ret = MAKE_AST_NODE(allocator, AstClassConstructor_, scope, node);
  ret->name = constructor->name;

  Symbol_* sym = scope_find(scope, &ret->name);
  if (!sym || !type_is(sym->ty, ClassType_)) {
    error_add(parser->errors, node->line, "Could not find type for constructor.");
    return NULL;
  }

  Scope_* cls_scope = type_as(ClassType_, sym->ty)->scope;
  if (constructor->prefix) {
    ret->prefix = (AstExpr_*)do_parse(parser, constructor->prefix, cls_scope, allocator);
  } else {
    ret->prefix = MAKE_AST_NODE(allocator, AstExpr_, cls_scope, node);
  }
  
  astlist_init(&ret->params, allocator);
  
  for (CstListNode_* n = constructor->params.head; n != NULL; n = n->next) {
    AstNode_* param = do_parse(parser, n->node, cls_scope, allocator);
    astlist_append(&ret->params, param);
  }

  return (AstNode_*)ret;
}

static AstNode_* cst_class_constructor_param_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstClassConstructorParam_* field = (CstClassConstructorParam_*)node;
  AstClassConstructorParam_* ret = MAKE_AST_NODE(allocator, AstClassConstructorParam_, scope, node);
  
  ret->name = field->name;
  ret->expr = (AstExpr_*)do_parse(parser, field->expr, scope, allocator);

  return (AstNode_*)ret;
}

static AstNode_* cst_array_value_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstArrayValueExpr_* array_expr = (CstArrayValueExpr_*)node;
  AstArrayValueExpr_* ret = MAKE_AST_NODE(allocator, AstArrayValueExpr_, scope, node);

  astlist_init(&ret->values, allocator);
  
  for (CstListNode_* n = array_expr->values.head; n != NULL; n = n->next) {
    AstNode_* val = do_parse(parser, n->node, scope, allocator);
    astlist_append(&ret->values, val);
  }

  if (ret->values.count) {
    ret->base.type = ((AstExpr_*)ret->values.head)->type;
  }

  return (AstNode_*)ret;
}

static AstNode_* cst_range_expr_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  assert(false && "unimplemented");
  return NULL;
}

static AstNode_* cst_type_def_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstTypeDef_* type_def = (CstTypeDef_*)node;
  AstTypeDef_* ret = MAKE_AST_NODE(allocator, AstTypeDef_, scope, node);
  ret->name = type_def->name;

  const TypeExpr_* type_expr = parse_type((CstNode_*)type_def, allocator);
  ret->type = (Type_*)resolve_typeexpr(
    type_expr, NULL, scope, parser->errors, allocator);
  frame_addclass(scope->frame, ret->type, scope);
  astlist_init(&ret->members, allocator);

  AstParseWork_ item = {
    .origin_scope = scope,
    .origin_node = node,
    .parsed_node = (AstNode_*)ret,
    .name = ret->name,
  };

  add_work(parser, &item);

  return (AstNode_*)ret;
}

static AstNode_* cst_type_member_decl_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstTypeMemberDecl_* member_decl = (CstTypeMemberDecl_*)node;
  TypeMemberDecl_* ret = NULL;
  assert(false && "unimplemented");
  return (AstNode_*)ret;
}

static AstNode_* cst_index_or_generic_args_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstIndexOrGenericArgs_* expr = (CstIndexOrGenericArgs_*)node;
  AstIndexOrGenericArgs_* ret = NULL;
  
  return (AstNode_*)ret;
}

static AstNode_* cst_generic_or_array_type_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstGenericOrArrayType_* cst = (CstGenericOrArrayType_*)node;
  AstNode_* ret = NULL;
  assert(false && "unimplemented");
  return (AstNode_*)ret;
}

static AstNode_* cst_index_or_type_expr_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstIndexOrTypeExpr_* expr = (CstIndexOrTypeExpr_*)node;
  AstIndexOrTypeExpr_* ret = NULL;
  
  AstExpr_* prefix = (AstExpr_*)do_parse(parser, expr->prefix, scope, allocator);

  //AstTypeExpr_

  return (AstNode_*)ret;
}

AstNode_* cst_union_type_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  assert(false && "unimplemented");
  return NULL;
}

AstNode_* cst_tuple_type_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  assert(false && "unimplemented");
  return NULL;
}

AstNode_* cst_primitive_type_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  assert(false && "unimplemented");
  return NULL;
}

AstNode_* cst_id_type_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  assert(false && "unimplemented");
  return NULL;
}

AstNode_* cst_reference_type_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {

  return NULL;
}

AstNode_* cst_generic_type_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  assert(false && "unimplemented");
  return NULL;
}

AstNode_* cst_type_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  assert(false && "unimplemented");
  return NULL;
}

AstNode_* cst_generic_param_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  assert(false && "unimplemented");
  return NULL;
}

typedef AstNode_* (*ParseFn)(AstParser_*, CstNode_*, Scope_*, MemoryAllocator_*);
static ParseRule_ parse_rules[] = {
  [CST_CLS(CstNode_)]                  = {noop_parse},
  [CST_CLS(CstProgram_)]               = {noop_parse},
  [CST_CLS(CstBlock_)]                 = {cst_block_parse},
  [CST_CLS(CstPrintStmt_)]             = {cst_print_parse},
  [CST_CLS(CstUnaryExp_)]              = {cst_unary_parse},
  [CST_CLS(CstBinaryExp_)]             = {cst_binary_parse},
  [CST_CLS(CstPrimaryExp_)]            = {cst_primary_parse},
  [CST_CLS(CstReturnStmt_)]            = {cst_return_parse},
  [CST_CLS(CstIfStmt_)]                = {cst_if_parse},
  [CST_CLS(CstAssertStmt_)]            = {cst_assert_parse},
  [CST_CLS(CstVarDeclStmt_)]           = {cst_var_decl_parse},
  [CST_CLS(CstVarExpr_)]               = {cst_var_expr_parse},
  [CST_CLS(CstIndexExpr_)]             = {cst_index_expr_parse},
  [CST_CLS(CstIdExpr_)]                = {cst_id_expr_parse},
  [CST_CLS(CstAssignmentExpr_)]        = {cst_assignment_expr_parse},
  [CST_CLS(CstInPlaceBinaryStmt_)]     = {cst_in_place_binary_stmt_parse},
  [CST_CLS(CstWhileStmt_)]             = {cst_while_stmt_parse},
  [CST_CLS(CstForStmt_)]               = {cst_for_stmt_parse},
  [CST_CLS(CstFunctionDef_)]           = {cst_function_def_parse},
  [CST_CLS(CstFunctionParam_)]         = {cst_function_param_parse},
  [CST_CLS(CstFunctionCall_)]          = {cst_function_call_parse},
  [CST_CLS(CstFunctionCallArgs_)]      = {cst_function_call_args_parse},
  [CST_CLS(CstExpressionStmt_)]        = {cst_expression_statement_parse},
  [CST_CLS(CstNoopExpr_)]              = {noop_parse},
  [CST_CLS(CstNoopStmt_)]              = {noop_parse},
  [CST_CLS(CstClassDef_)]              = {cst_class_def_parse},
  [CST_CLS(CstClassMemberDecl_)]       = {cst_class_member_decl_parse},
  [CST_CLS(CstClassConstructor_)]      = {cst_class_constructor_parse},
  [CST_CLS(CstClassConstructorParam_)] = {cst_class_constructor_param_parse},
  [CST_CLS(CstDotExpr_)]               = {cst_dot_expr_parse},
  [CST_CLS(CstUnionType_)]             = {noop_parse},
  [CST_CLS(CstTupleType_)]             = {noop_parse},
  [CST_CLS(CstPrimitiveType_)]         = {noop_parse},
  [CST_CLS(CstIdType_)]                = {noop_parse},
  [CST_CLS(CstReferenceType_)]         = {noop_parse},
  [CST_CLS(CstType_)]                  = {cst_type_parse},
  [CST_CLS(CstArrayValueExpr_)]        = {cst_array_value_parse},
  [CST_CLS(CstRangeExpr_)]             = {cst_range_expr_parse},
  [CST_CLS(CstTypeDef_)]               = {cst_type_def_parse},
  [CST_CLS(CstTypeMemberDecl_)]        = {cst_type_member_decl_parse},
  [CST_CLS(CstGenericParam_)]          = {cst_generic_param_parse},
  [CST_CLS(CstIndexOrTypeExpr_)]       = {cst_index_or_type_expr_parse},
  [CST_CLS(CstIndexOrGenericArgs_)]    = {cst_index_or_generic_args_parse},
  [CST_CLS(CstGenericOrArrayType_)]    = {cst_generic_or_array_type_parse},
};

// Static assert to make sure that all node types are accounted for.
STATIC_ASSERT(
  sizeof(parse_rules) / sizeof(ParseRule_) == __CST_NODE_COUNT__,
  CHECK_ANALYSIS_COUNT);

static ParseRule_* get_rule(int info) {
  ParseRule_* ret = &parse_rules[info];
  assertf(ret->fn, "Could not find analysis rule for AST class: %d", info);
  return ret;
}