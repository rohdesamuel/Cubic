#include "parser.h"
#include "ast.h"
#include "ast_internal.h"
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
  const CstNode_* origin_node;
  AstNode_* parsed_node;
  AstList_ args;
  Token_ name;
} AstParseWork_;

typedef struct AstResolutionWork_ {
  Type_* type;
  Scope_* scope;
} AstResolutionWork_;

typedef AstNode_* (*ParseFn)(AstParser_*, CstNode_*, Scope_*, MemoryAllocator_*);
typedef struct ParseRule_ {
  ParseFn fn;
} ParseRule_;

static ParseRule_* get_rule(int info);

void ast_parser_init(AstParser_* parser, MemoryAllocator_* allocator) {
  *parser = (AstParser_){ 0 };
  list_of(&parser->resolution_work_queue, AstResolutionWork_, NULL);
  list_of(&parser->specialization_work_queue, AstParseWork_, NULL);
  list_of(&parser->parse_work_queue, AstParseWork_, NULL);
  parser->allocator = allocator;
  parser->errors = alloc_ty(allocator, ErrorsContainer_);
  errorscontainer_init(parser->errors, allocator);
}

void ast_parser_clear(AstParser_* parser) {
  errorscontainer_clear(parser->errors);
  dealloc(parser->allocator, parser->errors);
  list_clear(&parser->resolution_work_queue);
  list_clear(&parser->specialization_work_queue);
  list_clear(&parser->parse_work_queue);
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
  frame_addclass(scope->frame, &name, ret->class_type, scope);
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

static AstFunctionDef_* init_ast_function_def(FunctionType_* function_type, Token_ name, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  AstFunctionDef_* ret = (AstFunctionDef_*)MAKE_AST_NODE(allocator, AstFunctionDef_, scope, node);

  Frame_* fn_frame = frame_createfrom(scope->frame, scope, (Type_*)function_type);
  Scope_* fn_scope = fn_frame->scope;
  scope_addexisting(scope, fn_frame->fn_symbol);

  ret->fn_symbol = fn_frame->fn_symbol;
  ret->fn_type = (Type_*)function_type;

  return ret;
}

static AstFunctionDef_* generate_function_def(AstParser_* parser, FunctionType_* function_type, CstFunctionDef_* function_def, Scope_* scope, MemoryAllocator_* allocator) {
  AstFunctionDef_* ret = init_ast_function_def(function_type, function_def->name, (CstNode_*)function_def, scope, allocator);
  Scope_* fn_scope = function_type->self.scope;
  ret->return_type = function_type->ret_ty;

  astlist_init(&ret->function_params, allocator);
  for (CstListNode_* n = function_def->function_params.head; n != NULL; n = n->next) {
    AstNode_* fn_param = do_parse(parser, n->node, ret->fn_symbol->ty->scope, allocator);
    astlist_append(&ret->function_params, fn_param);
  }

  ret->body = do_parse(parser, function_def->body, ret->fn_symbol->ty->scope, allocator);

  return ret;
}

Type_* defer_type_resolution(AstParser_* parser, const TypeExpr_* type_expr, Scope_* scope, MemoryAllocator_* allocator) {
  Type_* type = make_placeholder_ty(type_expr, scope, allocator);

  AstResolutionWork_ work = {
    .scope = scope,
    .type = type,
  };
  list_push(&parser->resolution_work_queue, &work);

  return type;
}

static void queue_type_resolution(AstParser_* parser, Type_* type, Scope_* scope) {
  AstResolutionWork_ work = {
    .scope = scope,
    .type = type,
  };
  list_push(&parser->resolution_work_queue, &work);
}

static void defer_ast_parse(AstParser_* parser, const CstNode_* cst_node, AstNode_* ast_node, Scope_* scope, MemoryAllocator_* allocator) {
  AstParseWork_ work = {
    .origin_scope = scope,
    .origin_node = cst_node,
    .parsed_node = (AstNode_*)ast_node,
    .name = (Token_){0},
  };
  list_push(&parser->parse_work_queue, &work);
}

static void defer_ast_parse_args(AstParser_* parser, const CstNode_* cst_node, AstNode_* ast_node, AstList_* args, Scope_* scope, MemoryAllocator_* allocator) {
  AstParseWork_ work = {
    .origin_scope = scope,
    .origin_node = cst_node,
    .parsed_node = (AstNode_*)ast_node,
    .args = *args,
    .name = (Token_){0},
  };
  list_push(&parser->parse_work_queue, &work);
}

static void defer_class_specialization(AstParser_* parser, const CstNode_* cst_node, AstClassDef_* ast_node, Scope_* scope, MemoryAllocator_* allocator) {
  AstParseWork_ item = {
    .origin_scope = scope,
    .origin_node = cst_node,
    .parsed_node = (AstNode_*)ast_node,
    .name = ast_node->name,
  };
  list_push(&parser->specialization_work_queue, &item);
}

static void defer_function_specialization(AstParser_* parser, const CstNode_* cst_node, AstFunctionDef_* ast_node, Scope_* scope, MemoryAllocator_* allocator) {
  AstParseWork_ item = {
    .origin_scope = scope,
    .origin_node = cst_node,
    .parsed_node = (AstNode_*)ast_node,
    .name = ast_node->fn_symbol->name,
  };

  list_push(&parser->specialization_work_queue, &item);
}

static void do_resolution_work(AstParser_* parser, AstResolutionWork_* item) {
  MemoryAllocator_* allocator = parser->allocator;
  type_resolve(item->type, item->scope, parser->errors);
}

static void do_work(AstParser_* parser, AstParseWork_* item) {
  MemoryAllocator_* allocator = parser->allocator;
  Symbol_* sym = scope_find(item->origin_scope, &item->name);
  assertf(sym, "Could not find symbol: %.*s", item->name.length, item->name.start);

  Type_* sym_ty = sym->ty;
  type_resolve(sym->ty, item->origin_scope, parser->errors);

  switch (sym->type) {
    case SYMBOL_CLS_FN:
    {
      FunctionType_* function_type = (FunctionType_*)type_valtype(sym->ty);
      CstFunctionDef_* function_def = (CstFunctionDef_*)item->origin_node;
      AstFunctionDef_* ast = AST_CAST(AstFunctionDef_, item->parsed_node);

      {
        ast->fn_symbol = sym;
        ast->fn_type = (Type_*)function_type;
        ast->return_type = function_type->ret_ty;

        astlist_init(&ast->function_params, allocator);
        for (CstListNode_* n = function_def->function_params.head; n != NULL; n = n->next) {
          AstNode_* fn_param = do_parse(parser, n->node, ast->fn_symbol->ty->scope, allocator);
          astlist_append(&ast->function_params, fn_param);
        }

        ast->body = do_parse(parser, function_def->body, ast->fn_symbol->ty->scope, allocator);
      }

      ListOf_(Type_*)* specializations = &sym->ty->specializations;
      for (ListNode_* n = specializations->head; n != NULL; n = n->next) {
        FunctionType_* specialization = type_as(FunctionType_, list_val(n, Type_*));
        AstFunctionDef_* new_function = generate_function_def(
          parser, specialization, (CstFunctionDef_*)item->origin_node, specialization->self.scope->parent, allocator);
        astlist_append(&ast->base.base.specializations, (AstNode_*)new_function);
      }

      break;
    }

    case SYMBOL_CLS_CLASS:
    {
      ClassType_* class_type = (ClassType_*)type_valtype(sym->ty);
      AstClassDef_* class_def = AST_CAST(AstClassDef_, item->parsed_node);
      class_def->class_type = (Type_*)class_type;
      ListOf_(Type_*)* specializations = &sym->ty->specializations;
      for (ListNode_* n = specializations->head; n != NULL; n = n->next) {
        ClassType_* specialization = type_as(ClassType_, list_val(n, Type_*));
        
        AstClassDef_* new_class = generate_class_def(
          parser, specialization, (CstClassDef_*)item->origin_node, specialization->scope->parent, allocator);
        astlist_append(&class_def->base.specializations, (AstNode_*)new_class);
      }

      break;
    }

    case SYMBOL_CLS_VAR:
      break;

    default:
      assertf(false, "unimplemented");
  }
}


static void do_parse_work(AstParser_* parser, AstParseWork_* item) {
  const CstNode_* cst = item->origin_node;
  AstNode_* ast = item->parsed_node;
  Scope_* scope = item->origin_scope;

  switch (ast->cls) {
    case AST_CLS(AstVarOrTypeExpr_):
    {
      AstVarOrTypeExpr_* expr = (AstVarOrTypeExpr_*)ast;
      break;
    }

    case AST_CLS(AstVarExpr_):
    {
      AstVarExpr_* parsed = (AstVarExpr_*)ast;
      switch (cst->cls) {
        case CST_CLS(CstGenericOrArrayExpr_):
        {
          AstExpr_* new_expr = NULL;
          CstGenericOrArrayExpr_* cst_expr = (CstGenericOrArrayExpr_*)cst;
          AstVarExpr_* prefix = AST_CAST(AstVarExpr_, do_parse(parser, cst_expr->prefix, scope, parser->allocator));
          Type_* type = prefix->base.type;
          type_resolve(type, scope, parser->errors);

          const TypeExpr_* tmpl = type->tmpl;
          // Assume all types with generic parameters are generics.
          if (tmpl->params.count) {
            if (type_is(type, FunctionType_)) {
              parsed->expr = (AstExpr_*)prefix;
            } else {
              assertf(false, "unimplemented");
            }
          } else {
            // Otherwise, assume arrays.

          }
          break;
        }

        default:
          assertf(false, "unimplemented");
          break;
      }
      break;
    }

    default:
      assertf(false, "Unknown parse work item");
      break;
  }
}

AstNode_* parse_ast(AstParser_* parser, const struct CstNode_* node, struct Scope_* scope) {
  AstProgram_* root = MAKE_AST_NODE(parser->allocator, AstProgram_, scope, node);
  CstProgram_* cst = CST_CAST(CstProgram_, node);
  root->block = (AstBlock_*)do_parse(parser, cst->block, scope, parser->allocator);

  do {
    while (parser->resolution_work_queue.count || parser->specialization_work_queue.count) {
      while (parser->resolution_work_queue.count) {
        AstResolutionWork_ work_item = { 0 };
        list_pop(&parser->resolution_work_queue, &work_item, sizeof(AstResolutionWork_));
        do_resolution_work(parser, &work_item);
      }

      while (parser->specialization_work_queue.count) {
        AstParseWork_ work_item = { 0 };
        list_pop(&parser->specialization_work_queue, &work_item, sizeof(AstParseWork_));
        do_work(parser, &work_item);
      }
    }

    for (int i = 0; i < parser->parse_work_queue.count; ++i) {
      AstParseWork_ work_item = { 0 };
      list_pop(&parser->parse_work_queue, &work_item, sizeof(AstParseWork_));
      do_parse_work(parser, &work_item);
    }
  } while (parser->parse_work_queue.count || parser->resolution_work_queue.count || parser->specialization_work_queue.count);

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
  ret->base.type = ret->expr->type;

  return (AstNode_*)ret;
}

static AstNode_* cst_binary_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstBinaryExp_* expr = (CstBinaryExp_*)node;
  AstBinaryExp_* ret = MAKE_AST_NODE(allocator, AstBinaryExp_, scope, node);

  ret->op = expr->op;
  ret->left = (AstExpr_*)(do_parse(parser, expr->left, scope, allocator));
  ret->right = (AstExpr_*)(do_parse(parser, expr->right, scope, allocator));
  
  // TODO: add a TypeExpr_ for this.
  ret->base.type = make_placeholder_ty(NULL, scope, allocator);
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
    ret->decl_type = defer_type_resolution(parser, decl_type, scope, allocator);
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

  if (stmt->decl_token == TK_VAR) {
    ret->decl_type = make_var_ty(ret->decl_type, ret->decl_type->tmpl, scope, allocator);
    var_sym->ty = ret->decl_type;
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
  
  const TypeExpr_* type_expr = parse_type(node, allocator);
  ret->base.type = defer_type_resolution(parser, type_expr, scope, allocator);

  return (AstNode_*)ret;
}

static AstNode_* cst_index_expr_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstIndexExpr_* expr = (CstIndexExpr_*)node;
  AstIndexExpr_* ret = MAKE_AST_NODE(allocator, AstIndexExpr_, scope, node);
  ret->prefix = (AstExpr_*)(do_parse(parser, expr->prefix, scope, allocator));
  ret->index = (AstExpr_*)(do_parse(parser, expr->index, scope, allocator));
  
  const TypeExpr_* type_expr = parse_type(node, allocator);
  ret->base.type = defer_type_resolution(parser, type_expr, scope, allocator);

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
  AstFunctionDef_* ret = MAKE_AST_NODE(allocator, AstFunctionDef_, scope, node);
  const TypeExpr_* type_expr = parse_type(node, allocator);
  
  const TypeExprFunction_* fn_type_expr = (TypeExprFunction_*)type_expr;
  Type_* ret_ty = defer_type_resolution(parser, fn_type_expr->ret_type, scope, allocator);
  Type_* fn_type = make_function_ty(def->name, NULL, ret_ty, type_expr, scope, allocator);

  Frame_* fn_frame = frame_createfrom(scope->frame, scope, fn_type);
  Scope_* fn_scope = fn_frame->scope;
  scope_addexisting(scope, fn_frame->fn_symbol);

  for (ListNode_* n = type_expr->params.head; n != NULL; n = n->next) {
    TypeExprGenericParam_* generic_param_type_expr = list_val(n, TypeExprGenericParam_*);
    Type_* param_ty = defer_type_resolution(parser, (TypeExpr_*)generic_param_type_expr, fn_scope, allocator);
    frame_addtype(fn_frame, &generic_param_type_expr->name, param_ty, fn_scope);
  }

  for (ListNode_* n = fn_type_expr->params.head; n != NULL; n = n->next) {
    TypeExprFunctionParam_* param_type_expr = list_val(n, TypeExprFunctionParam_*);
    Type_* param_ty = defer_type_resolution(parser, (TypeExpr_*)param_type_expr, fn_scope, allocator);
    frame_addparam(fn_frame, &param_type_expr->name, param_ty);
    list_push(&((FunctionType_*)fn_type)->params, &param_ty);
  }

  ret->fn_symbol = fn_frame->fn_symbol;
  ret->fn_type = fn_type;
  defer_function_specialization(parser, node, ret, scope, allocator);
  return (AstNode_*)ret;

  astlist_init(&ret->function_params, allocator);
  for (CstListNode_* n = def->function_params.head; n != NULL; n = n->next) {
    AstFunctionParam_* fn_param_node = (AstFunctionParam_*)do_parse(parser, n->node, fn_scope, allocator);
    astlist_append(&ret->function_params, (AstNode_*)fn_param_node);
  }
  ret->body = do_parse(parser, def->body, fn_scope, allocator);

}

static AstNode_* cst_function_param_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstFunctionParam_* param = (CstFunctionParam_*)node;
  AstFunctionParam_* ret = MAKE_AST_NODE(allocator, AstFunctionParam_, scope, param);

  ret->name = param->name;

  const TypeExpr_* type_expr = parse_type(param->type, allocator);
  ret->type = defer_type_resolution(parser, type_expr, scope, allocator);
  frame_addparam(scope->frame, &ret->name, ret->type);

  return (AstNode_*)ret;
}

static AstNode_* cst_function_call_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstFunctionCall_* call = (CstFunctionCall_*)node;
  AstFunctionCall_* ret = MAKE_AST_NODE(allocator, AstFunctionCall_, scope, node);

  ret->prefix = (AstExpr_*)do_parse(parser, call->prefix, scope, allocator);
  ret->args = (AstFunctionCallArgs_*)do_parse(parser, call->args, scope, allocator);

  const TypeExpr_* type_expr = parse_type(node, allocator);
  ret->base.type = defer_type_resolution(parser, type_expr, scope, allocator);

  return (AstNode_*)ret;
}

static AstNode_* cst_function_call_args_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstFunctionCallArgs_* args = (CstFunctionCallArgs_*)node;
  AstFunctionCallArgs_* ret = MAKE_AST_NODE(allocator, AstFunctionCallArgs_, scope, node);
  astlist_init(&ret->args, allocator);
  for (CstListNode_* n = args->args.head; n != NULL; n = n->next) {
    AstNode_* arg = do_parse(parser, n->node, scope, allocator);
    astlist_append(&ret->args, arg);
  }

  return (AstNode_*)ret;
}

static AstNode_* noop_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  return MAKE_AST_NOOP(allocator);
}

static AstNode_* cst_class_def_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstClassDef_* def = (CstClassDef_*)node;
  AstClassDef_* ret = (AstClassDef_*)MAKE_AST_NODE(allocator, AstClassDef_, scope, node);
  ret->name = def->name;
  astlist_init(&ret->members, allocator);

  const TypeExprClass_* class_type_expr = typeexpr_as(TypeExprClass_, parse_type(node, allocator));
  ClassType_* cls_type = (ClassType_*)make_class_ty(ret->name, (TypeExpr_*)class_type_expr, scope, allocator);

  frame_addclass(scope->frame, &ret->name, (Type_*)cls_type, scope);
  ret->class_type = (Type_*)cls_type;

  ListNode_* member_n = class_type_expr->members.head;
  for (CstListNode_* n = def->members.head; n != NULL; n = n->next) {
    CstClassMemberDecl_* decl = (CstClassMemberDecl_*)n->node;
    AstClassMemberDecl_* ast = MAKE_AST_NODE(allocator, AstClassMemberDecl_, cls_type->scope, decl);
    ast->name = decl->name;
    if (decl->opt_expr) {
      ast->opt_expr = (AstExpr_*)do_parse(parser, decl->opt_expr, scope, allocator);
    }
    const TypeExprClassMember_* field_tmpl = list_val(member_n, TypeExprClassMember_*);

    ast->field_type = defer_type_resolution(parser, (TypeExpr_*)field_tmpl, cls_type->scope, allocator);
    astlist_append(&ret->members, (AstNode_*)ast);

    type_class_addmember((Type_*)cls_type, field_tmpl->name, ast->field_type, ast->opt_expr);

    member_n = member_n->next;
  }

  defer_class_specialization(parser, node, ret, scope, allocator);

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

  const TypeExpr_* type_expr = parse_type(node, allocator);
  ret->base.type = defer_type_resolution(parser, type_expr, scope, allocator);
  return (AstNode_*)ret;
}

static AstNode_* cst_class_constructor_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstClassConstructor_* constructor = (CstClassConstructor_*)node;
  AstClassConstructor_* ret = MAKE_AST_NODE(allocator, AstClassConstructor_, scope, node);
  ret->name = constructor->name;

  if (constructor->prefix) {
    const TypeExpr_* type_expr = parse_type(constructor->prefix, allocator);
    ret->base.type = make_placeholder_ty(type_expr, scope, allocator);
  }

  astlist_init(&ret->params, allocator);

  for (CstListNode_* n = constructor->params.head; n != NULL; n = n->next) {
    AstNode_* param = do_parse(parser, n->node, scope, allocator);
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
  astlist_init(&ret->members, allocator);

  //AstParseWork_ item = {
  //  .origin_scope = scope,
  //  .origin_node = node,
  //  .parsed_node = (AstNode_*)ret,
  //  .name = ret->name,
  //};
  //
  //defer_type_specialization(parser, &item);

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

static AstNode_* cst_generic_or_array_expr_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstGenericOrArrayExpr_* cst = (CstGenericOrArrayExpr_*)node;

  AstNode_* prefix = do_parse(parser, cst->prefix, scope, allocator);
  AstList_ args = { 0 };
  astlist_init(&args, allocator);

  ListOf_(TypeExpr_*) tmpl_args = { 0 };
  list_of(&tmpl_args, TypeExpr_*, allocator);
  for (CstListNode_* n = cst->args.head; n != NULL; n = n->next) {
    AstNode_* arg = do_parse(parser, n->node, scope, allocator);
    astlist_append(&args, arg);

    list_push(&tmpl_args, &((AstExpr_*)arg)->type->tmpl);
  }

  AstVarExpr_* var_prefix = AST_CAST(AstVarExpr_, prefix);
  AstVarExpr_* ret = MAKE_AST_NODE(allocator, AstVarExpr_, scope, node);
  
  const TypeExpr_* type_expr = make_generic_or_array_typeexpr(((AstExpr_*)prefix)->type->tmpl, &tmpl_args, allocator);
  ret->base.type = defer_type_resolution(parser, type_expr, scope, allocator);
  defer_ast_parse_args(parser, node, (AstNode_*)ret, &args, scope, allocator);
  //assert(false && "unimplemented");
  return (AstNode_*)ret;
}

static AstNode_* cst_generic_or_array_type_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  CstGenericOrArrayType_* cst = (CstGenericOrArrayType_*)node;
  AstNode_* ret = NULL;
  assert(false && "unimplemented");
  return (AstNode_*)ret;
}

static AstNode_* cst_var_or_type_expr_parse(AstParser_* parser, CstNode_* node, Scope_* scope, MemoryAllocator_* allocator) {
  assert(false && "This should not be hit. This should have been resolved in the cst_parser.");
  return NULL;
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
  CstType_* cst = (CstType_*)node;
  AstTypeExpr_* ret = MAKE_AST_NODE(allocator, AstTypeExpr_, scope, node);

  const TypeExpr_* type_expr = parse_type(cst->impl, allocator);
  ret->base.type = defer_type_resolution(parser, type_expr, scope, allocator);

  return (AstNode_*)ret;
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
  [CST_CLS(CstVarOrTypeExpr_)]         = {cst_var_or_type_expr_parse},
  [CST_CLS(CstIndexOrGenericArgs_)]    = {cst_index_or_generic_args_parse},
  [CST_CLS(CstGenericOrArrayType_)]    = {cst_generic_or_array_type_parse},
  [CST_CLS(CstGenericOrArrayExpr_)]    = {cst_generic_or_array_expr_parse},
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