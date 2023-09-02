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

static AnalysisRule_* get_rule(int info);
static void error(Analyzer_* analyzer, AstNode_* node, const char* message, ...);

Analyzer_* analyzer_;

static void do_analysis(AstNode_* node) {
  get_rule(node->cls)->fn(node);
}

void analyzer_init(Analyzer_* analyzer, MemoryAllocator_* allocator) {
  *analyzer = (Analyzer_){ 0 };
  analyzer->allocator = allocator;
  analyzer->frame = frame_root(analyzer->allocator);
  analyzer->scope = analyzer->frame->scope; //scope_create(analyzer->frame, NULL, (MemoryAllocator_*)&analyzer->allocator);
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

/*static void analyze_frame(Frame_* frame) {
  int frame_index = 0;
  frame->fn_closure->closure.frame_index = frame_index++;
  analyze_scope(frame, frame->scope, frame_index);

  for (ListNode_* n = frame->children.head; n != NULL; n = n->next) {
    Frame_* f = list_val(n, Frame_*);
    analyze_frame(f);
  }
}

static void analyze_scope(Frame_* frame, Scope_* scope, int frame_index) {
  List_* vars = &scope->table->vars;
  for (ListNode_* n = vars->head; n != NULL; n = n->next) {
    Symbol_* sym = (Symbol_*)n->data;
    if (sym->type != SYMBOL_TYPE_VAR) {
      continue;
    }

    VarSymbol_* var = &sym->var;
    int size = var->sem_type.val == VAL_CLASS ? (int)var->sem_type.size : 1;
    var->frame_index = frame_index;
    frame_index += size;
  }

  frame->max_stack_size = frame_index > frame->max_stack_size ? frame_index : frame->max_stack_size;

  for (ListNode_* n = scope->children.head; n != NULL; n = n->next) {
    Scope_* s = list_val(n, Scope_*);
    analyze_scope(frame, s, frame_index);
  }
}*/

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

  AS_EXPR(expr->expr)->top_sem_type = expr->top_sem_type;
  do_analysis((AstNode_*)expr->expr);
  expr->sem_type = AS_EXPR(expr->expr)->sem_type;
}

void print_analysis(AstNode_* n) {
  AstPrintStmt_* stmt = (AstPrintStmt_*)n;
  do_analysis((AstNode_*)stmt->expr);
}

void unary_analysis(AstNode_* n) {
  AstUnaryExp_* expr = (AstUnaryExp_*)n;
  
  expr->expr->top_sem_type = expr->base.top_sem_type;
  do_analysis((AstNode_*)expr->expr);
  expr->base.sem_type = AS_EXPR(expr->expr)->sem_type;
  switch (expr->op) {
    case TK_TILDE:
      if (!type_isanumber(expr->base.sem_type.ty)) {
        error(analyzer_, n, "expression does not have a number type.");
      }
      break;

    case TK_NOT:
      if (!type_isabool(expr->base.sem_type.ty)) {
        error(analyzer_, n, "expression does not have a boolean type.");
      }
      break;

    case TK_MINUS:
      if (!type_isanumber(expr->base.sem_type.ty)) {
        error(analyzer_, n, "expression does not have a number type.");
      }
      break;

    default:
      error(analyzer_, n, "Unknown unary operator");
      break;
  }
}

void binary_analysis(AstNode_* n) {
  AstBinaryExp_* expr = (AstBinaryExp_*)n;
  expr->left->top_sem_type = expr->base.top_sem_type;
  expr->right->top_sem_type = expr->base.top_sem_type;

  do_analysis((AstNode_*)expr->left);
  do_analysis((AstNode_*)expr->right);

  SemanticType_ lsem_type = AS_EXPR(expr->left)->sem_type;
  SemanticType_ rsem_type = AS_EXPR(expr->right)->sem_type;

  if (type_isunknown(lsem_type.ty)) {
    error(analyzer_, n, "left-hand expression type could not be deduced.");
    return;
  }

  if (type_isunknown(rsem_type.ty)) {
    error(analyzer_, n, "right-hand expression type could not be deduced.");
    return;
  }

  if (!type_coercible(rsem_type.ty, lsem_type.ty)) {
    error(analyzer_, n, "right-hand expression cannot be coerced to left-hand expression.");
  }

  const Type_* expected_type = NULL;
  switch (expr->op) {
    case TK_AND: 
    case TK_OR:
    case TK_XOR:
      expr->base.sem_type = semantictype_as((Type_*)&Bool_Ty);
      expected_type = (Type_*)&Bool_Ty;
      break;

    case TK_GT:
    case TK_GTE:
    case TK_LT:
    case TK_LTE:
    case TK_EQUAL_EQUAL:
    case TK_BANG_EQUAL:
      expr->base.sem_type = semantictype_as((Type_*)&Bool_Ty);
      break;

    case TK_AMPERSAND:
    case TK_PIPE:
    case TK_HAT:
      expr->base.sem_type = lsem_type;
      if (!type_isainteger(expr->base.sem_type.ty)) {
        error(analyzer_, n, "expected the expression type to be a number.");
      }
      break;

    case TK_PLUS:
      expr->base.sem_type = lsem_type;
      if (!type_isanumber(expr->base.sem_type.ty) && !type_isastring(expr->base.sem_type.ty)) {
        error(analyzer_, n, "expected the expression type to be a number or a string.");
      }
      break;

    case TK_MINUS:
    case TK_STAR:
    case TK_SLASH:
    case TK_DOUBLE_SLASH:
      expr->base.sem_type = lsem_type;
      if (!type_isanumber(expr->base.sem_type.ty)) {
        error(analyzer_, n, "expected the expression type to be a number.");
      }
      break;

    case TK_PERCENT:
    case TK_LSHIFT:
    case TK_RSHIFT:
      expr->base.sem_type = lsem_type;
      expected_type = (const Type_*)&Int_Ty;
      break;

    default:
      error(analyzer_, n, "unimplemented token type.");
      break;
  }

  if (expected_type) {
    if (lsem_type.ty->cls != expected_type->cls) {
      error(analyzer_, n, "left-hand expression does not have expected type of %s.", type_tostr(expected_type));
    }

    if (rsem_type.ty->cls != expected_type->cls) {
      error(analyzer_, n, "right-hand expression does not have expected type of %s.", type_tostr(expected_type));
    }
  }

  if (lsem_type.lifetime == LIFETIME_STATIC && rsem_type.lifetime == LIFETIME_STATIC) {
    expr->base.sem_type.lifetime = LIFETIME_STATIC;
  } else {
    expr->base.sem_type.lifetime = LIFETIME_TMP;
  }

  semantictype_size(&expr->base.sem_type);
}

void primary_analysis(AstNode_* n) {
  AstPrimaryExp_* exp = AST_CAST(AstPrimaryExp_, n);

  semantictype_size(&exp->base.sem_type);
  assertf(exp->base.sem_type.ty->size > 0, "");
}

void return_analysis(AstNode_* n) {
  AstReturnStmt_* stmt = (AstReturnStmt_*)n;
  Frame_* frame = stmt->base.scope->frame;
  stmt->expr->top_sem_type.ty = type_as(FunctionType_, frame->fn_symbol->ty)->ret_ty;

  do_analysis((AstNode_*)stmt->expr);
  if (type_coercible(AS_EXPR(stmt->expr)->sem_type.ty, type_as(FunctionType_, frame->fn_symbol->ty)->ret_ty)) {
    error(analyzer_, n, "Return statement type does not match function type.");
  }  
}

void if_analysis(AstNode_* n) {
  AstIfStmt_* stmt = (AstIfStmt_*)n;
  stmt->condition_expr->top_sem_type = semantictype_frominfo((Type_*)&Bool_Ty);

  do_analysis((AstNode_*)stmt->condition_expr);
  if (!type_isabool(AS_EXPR(stmt->condition_expr)->sem_type.ty)) {
    error(analyzer_, n, "if expression must have a boolean type.");
  }

  do_analysis(stmt->if_stmt);
  if (stmt->else_stmt) {
    do_analysis(stmt->else_stmt);
  }

  for (AstListNode_* n = stmt->elif_exprs.head; n != NULL; n = n->next) {
    do_analysis(n->node);
    if (!type_isabool(AS_EXPR(n->node)->sem_type.ty)) {
      error(analyzer_, n->node, "elif expression must have a boolean type.");
    }
  }
  for (AstListNode_* n = stmt->elif_stmts.head; n != NULL; n = n->next) {
    do_analysis(n->node);
  }
}

void assert_analysis(AstNode_* n) {
  AstAssertStmt_* stmt = (AstAssertStmt_*)n;
  stmt->expr->top_sem_type = semantictype_frominfo((Type_*)&Bool_Ty);
  do_analysis((AstNode_*)stmt->expr);
  if (!type_isabool(AS_EXPR(stmt->expr)->sem_type.ty)) {
    error(analyzer_, n, "assert expression must have a boolean type.");
  }
}

void var_decl_analysis(AstNode_* n) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)n;

  if (stmt->expr) {
    stmt->expr->top_sem_type = stmt->sem_type;
    do_analysis((AstNode_*)stmt->expr);
  }

  if (type_isunknown(stmt->sem_type.ty)) {
    if (!stmt->expr) {
      error(analyzer_, n, "type deduced variable must have an expression");
      return;
    }

    if (type_is(stmt->sem_type.ty, VarType_)) {
      stmt->sem_type.ty = make_var_ty(AS_EXPR(stmt->expr)->sem_type.ty, analyzer_->allocator);
    } else {
      stmt->sem_type = AS_EXPR(stmt->expr)->sem_type;
    }
  }

  stmt->sem_type.lifetime = LIFETIME_AUTOMATIC;

  // TODO: implement tuples (and others) for variable declarations.
  VarSymbol_* var = scope_var(n->scope, &stmt->name);
  var->sem_type = stmt->sem_type;

  if (type_is(stmt->sem_type.ty, ClassType_)) {
    ClassType_* ty = type_as(ClassType_, stmt->sem_type.ty);
    const Token_* ty_name = &ty->self.opt_name;
    var->sem_type.sym = scope_search_to_root(n->scope, ty_name);
    if (!var->sem_type.sym) {
      error(analyzer_, n, "could not find symbol \"%.*s\" for variable type.", ty_name->length, ty_name->start);
      return;
    }
  }

  if (stmt->expr && !semantictype_iscoercible(AS_EXPR(stmt->expr)->sem_type, stmt->sem_type)) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }

  semantictype_size(&var->sem_type);
}

void var_expr_analysis(AstNode_* n) {
  AstVarExpr_* expr = (AstVarExpr_*)n;
  expr->expr->top_sem_type = expr->base.top_sem_type;
  do_analysis((AstNode_*)expr->expr);
  expr->base.sem_type = expr->expr->sem_type;
}

void id_expr_analysis(AstNode_* n) {
  AstIdExpr_* expr = (AstIdExpr_*)n;
  Symbol_* sym = scope_find(n->scope, &expr->name);
  if (sym) {
    switch (sym->type) {
      case SYMBOL_TYPE_CLASS:
        expr->base.sem_type = (SemanticType_){
          .ty = sym->ty,
          .sym = sym,
        };
        break;

      case SYMBOL_TYPE_FN:
        expr->base.sem_type = (SemanticType_){
          .ty = type_as(FunctionType_, sym->ty)->ret_ty,
          .sym = sym,
        };
        break;

      case SYMBOL_TYPE_VAR:
        expr->base.sem_type = sym->var.sem_type;
        /*if (expr->base.top_sem_type.kind == KIND_VAR) {
          expr->base.sem_type.kind = KIND_VAR;
          expr->base.sem_type.ref_kind = REF_KIND_WEAK;
        }*/
        break;

      //case SYMBOL_TYPE_CLOSURE:
      //  expr->base.sem_type = sym->closure.fn->fn.return_type;
      //  expr->base.sem_type.sym = sym;
      //  break;

      default:
        error(analyzer_, n, "Unhandled symbol type: %d", sym->type);
        break;
    }
  } else {
    error(analyzer_, n, "Could not find variable '%.*s'", expr->name.length, expr->name.start);
    expr->base.sem_type = SemanticType_Unknown;
  }
}

void assignment_expr_analysis(AstNode_* n) {
  AstAssignmentExpr_* expr = (AstAssignmentExpr_*)n;

  SemanticType_ top_sem_type = SemanticType_Unknown;
  top_sem_type.ty = (Type_*)type_alloc_ty(analyzer_->allocator, RefType_);

  // TODO: allow for deconstructing tuples in assignments (and other types).
  expr->left->top_sem_type = top_sem_type;
  expr->right->top_sem_type = expr->base.top_sem_type;
  do_analysis((AstNode_*)expr->left);
  do_analysis((AstNode_*)expr->right);

  if (expr->left->base.cls != AST_CLS(AstVarExpr_)) {
    error(analyzer_, n, "Left-hand side of assignment cannot be assigned to.");
    return;
  }

  if (type_isconst(expr->left->sem_type.ty)) {
    error(analyzer_, n, "Left-hand side of assignment is const and cannot be assigned to.");
    return;
  }

  AstVarExpr_* lvalue_expr = (AstVarExpr_*)expr->left;
  AstExpr_* rvalue_expr = (AstExpr_*)expr->right;
  if (lvalue_expr->base.sem_type.ty->cls != rvalue_expr->sem_type.ty->cls) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }
  expr->base.sem_type = lvalue_expr->base.sem_type;
}

void while_stmt_analysis(AstNode_* n) {
  AstWhileStmt_* stmt = (AstWhileStmt_*)n;
  stmt->condition_expr->top_sem_type = semantictype_frominfo((Type_*)&Bool_Ty);

  do_analysis((AstNode_*)stmt->condition_expr);
  do_analysis(stmt->block_stmt);
   
  if (!type_isabool(AS_EXPR(stmt->condition_expr)->sem_type.ty)) {
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
  def->base.sem_type = (SemanticType_) {
    .ty = def->fn_symbol->ty,
    .lifetime = LIFETIME_STATIC,
    .sym = def->fn_symbol,
  };

  do_analysis((AstNode_*)def->body);  
}

void function_body_analysis(AstNode_* n) {
  AstFunctionBody_* body = (AstFunctionBody_*)n;
  FunctionType_* fn_type = type_as(FunctionType_, &body->fn_symbol->ty);
  FunctionSymbol_* fn = &body->fn_symbol->fn;
  fn_type->ret_ty = body->return_type.ty;

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
  param->type.sym = scope_search_to_root(n->scope, &param->type.ty->opt_name);
  semantictype_size(&param->type);
  if (type_isunknown(param->type.ty)) {
    error(analyzer_, n, "Parameter type inferrece is unimplemented.");
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

  s->var.sem_type = param->type;
}

void function_call_analysis(AstNode_* node) {
  AstFunctionCall_* call = (AstFunctionCall_*)node;
  call->prefix->top_sem_type = call->base.top_sem_type;
  do_analysis((AstNode_*)call->prefix);  

  call->base.sem_type = AS_EXPR(call->prefix)->sem_type;
  Symbol_* sym = call->base.sem_type.sym;
  Symbol_* fn_sym = symbol_ascallable(sym);

  call->args->fn_sym = fn_sym;
  do_analysis((AstNode_*)call->args);

  if (!fn_sym) {
    error(analyzer_, node, "Trying to call variable that is not callable.");
    return;
  }
}

void function_call_arg_analysis(AstNode_* node) {
  AstFunctionCallArg_* arg = AST_CAST(AstFunctionCallArg_, node);
  
  arg->expr->top_sem_type = arg->base.top_sem_type;
  do_analysis((AstNode_*)arg->expr);
  arg->base.sem_type = arg->expr->sem_type;
}

void function_call_args_analysis(AstNode_* node) {
  AstFunctionCallArgs_* args = AST_CAST(AstFunctionCallArgs_, node);
  FunctionSymbol_* fn_sym = &args->fn_sym->fn;

  if (fn_sym->params.count > UINT8_MAX) {
    error(analyzer_, node, "Parameter count exceeded maximum of 255.");
  }

  List_* params = &args->fn_sym->fn.params;
  ListNode_* param_node = params->head;
  for (AstListNode_* n = args->args.head; n != NULL; n = n->next) {
    Symbol_* param = list_val(param_node, Symbol_*);
    SemanticType_ param_sem_type = SemanticType_Unknown;
    if (param->type == SYMBOL_TYPE_VAR) {
      param_sem_type = param->var.sem_type;
    } else {
      error(analyzer_, node, "Encountered unknown parameter type %d.", param->type);
    }

    AS_EXPR(n->node)->top_sem_type = param_sem_type;
    do_analysis(n->node);
    SemanticType_ expr_sem_type = AS_EXPR(n->node)->sem_type;

    if (type_assignable(expr_sem_type.ty, param_sem_type.ty)) {
      error(analyzer_, n->node, "Expression does not match function parameter type.");
    }
    
    param_node = param_node->next;
  }

  // TODO: allow for optional arguments
  if (fn_sym->params.count != args->args.count) {
    error(analyzer_, node,
      "Parameter count does not match definition. Expected %d, got %d",
      fn_sym->params.count, args->args.count);
  }
}

void noop_analysis(AstNode_* n) {}

void clean_up_temps_analysis(AstNode_* n) {

}

void ast_tmp_decl_analysis(AstNode_* n) {
  AstTmpDecl_* decl = (AstTmpDecl_*)n;
  decl->expr->top_sem_type = decl->base.top_sem_type;
  do_analysis((AstNode_*)decl->expr);
  decl->base.sem_type = decl->expr->sem_type;
}

void ast_class_def_analysis(AstNode_* node) {
  AstClassDef_* def = (AstClassDef_*)node;

  for (AstListNode_* n = def->members.head; n != NULL; n = n->next) {
    do_analysis(n->node);
  }

  Symbol_* cls_sym = def->class_type.sym;
  if (semantictype_hascycle(&def->class_type)) {
    error(analyzer_, node, "struct \"%.*s\" has a cycle", cls_sym->name.length, cls_sym->name.start);
    return;
  }

  semantictype_size(&def->class_type);
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
  FieldSymbol_* field = &decl->sem_type.sym->field;
  Type_* type = decl->sem_type.sym->ty;

  if (!type_fill(type, node->scope)) {
    error(analyzer_, node,
      "Could not find type '%.*s' in class member declaration",
      type->opt_name.length, type->opt_name.start);
  }

  if (decl->opt_expr) {
    AstExpr_* expr = decl->opt_expr;
    do_analysis((AstNode_*)expr);

    if (!type_isval(expr->sem_type.ty) &&
      expr->sem_type.lifetime != LIFETIME_STATIC &&
      expr->sem_type.lifetime != LIFETIME_TMP) {

      error(analyzer_, node, "struct member field default value must be a value.");
      return;
    }

    field->val = fold_constants(node->scope, expr);
    field->has_default_val = true;
  } else {
    field->val = NIL_VAL;
    field->has_default_val = false;
  }
  field->opt_expr = decl->opt_expr;
}

void ast_dot_expr_analysis(AstNode_* node) {
  AstDotExpr_* expr = AST_CAST(AstDotExpr_, node);

  expr->prefix->top_sem_type = expr->base.top_sem_type;
  do_analysis((AstNode_*)expr->prefix);


  SemanticType_ prefix_type = expr->prefix->sem_type;
  if (!type_is(prefix_type.ty, ClassType_)) {
    error(analyzer_, (AstNode_*)expr->prefix, "Expected class type for sub-expression.");
    return;
  }

  if (!prefix_type.sym) {
    error(analyzer_, (AstNode_*)expr->prefix, "Unknown error: no semantic information for prefix.");
    return;
  }

  Symbol_* cls_sym = NULL;

  if (prefix_type.sym->type == SYMBOL_TYPE_VAR) {
    VarSymbol_* sym = &prefix_type.sym->var;
    cls_sym = sym->sem_type.sym;
  } else if (prefix_type.sym->type == SYMBOL_TYPE_CLASS) {
    cls_sym = prefix_type.sym;
  } else {
    error(analyzer_, (AstNode_*)expr->prefix, "Unknown error: unexpected symbol type.");
    return;
  }

  Symbol_* found = NULL;
  for (ListNode_* n = cls_sym->cls.members.head; n != NULL; n = n->next) {
    Symbol_* field = list_val(n, Symbol_*);
    if (token_eq(field->name, expr->id)) {
      found = field;
      break;
    }
  }

  if (!found || found->type != SYMBOL_TYPE_FIELD) {
    error(analyzer_, node, "Could not find field '%.*s'", expr->id.length, expr->id.start);
    return;
  }
  expr->cls_sym = cls_sym;
  expr->base.sem_type.ty = found->ty;
}

void ast_class_constructor_analysis(AstNode_* node) {
  AstClassConstructor_* constructor = (AstClassConstructor_*)node;

  SemanticType_* sem_type = &constructor->base.sem_type;
  Type_* type = sem_type->ty;

  Symbol_* cls_sym = scope_search_to_root(node->scope, &type->opt_name);
  if (!type_fill(type, node->scope)) {
    error(analyzer_, node, "Could not find class %.*s", type->opt_name.length, type->opt_name.start);
    return;
  } else {
    constructor->base.sem_type.ty = cls_sym->ty;
    constructor->base.sem_type.sym = cls_sym;
  }

  for (AstListNode_* n = constructor->params.head; n != NULL; n = n->next) {
    AstClassConstructorParam_* field = AST_CAST(AstClassConstructorParam_, n->node);
    field->base.top_sem_type = constructor->base.sem_type;
    do_analysis((AstNode_*)field);
  }
}

void ast_class_constructor_param_analysis(AstNode_* node) {
  AstClassConstructorParam_* field = (AstClassConstructorParam_*)node;
  field->expr->top_sem_type = field->base.top_sem_type;
  do_analysis((AstNode_*)field->expr);

  // Check that the field in the constructor parameter actually exists in the class.
  SemanticType_* type = &field->base.top_sem_type;
  if (field->name.start) {
    bool found = false;

    ClassSymbol_* cls_sym = &type->sym->cls;
    for (ListNode_* n = cls_sym->members.head; n != NULL; n = n->next) {
      Symbol_* member = list_val(n, Symbol_*);
      if (token_eq(member->name, field->name)) {
        found = true;
        break;
      }
    }

    if (!found) {
      error(analyzer_, node, "Constructor field %.*s does not exist in class %.*s.",
        field->name.length, field->name.start,
        type->ty->opt_name.length, type->ty->opt_name.start);
    }
  }

  field->base.sem_type = field->expr->sem_type;
}

void array_value_analysis(AstNode_* node) {
  /*
  AstArrayValueExpr_* expr = (AstArrayValueExpr_*)node;
  SemanticType_* type = &expr->base.sem_type;
  *type = SemanticType_Unknown;

  type->val = VAL_ARRAY;
  type->kind = KIND_VAL;
  type->size = 0;
  */

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
  [AST_CLS(AstIdExpr_)]                 = {id_expr_analysis},
  [AST_CLS(AstAssignmentExpr_)]         = {assignment_expr_analysis},
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