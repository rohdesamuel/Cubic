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

static void analyze_frame(Frame_* frame);
static void analyze_scope(Frame_* frame, Scope_* scope, int frame_index);

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
  analyze_frame(analyzer->frame);
}

void analyzer_clear(Analyzer_* analyzer) {
  scope_destroy(&analyzer->scope);
  frame_destroy(&analyzer->frame);
  pageallocator_deinit(&analyzer->allocator);
  analyzer_ = NULL;
}

static void analyze_frame(Frame_* frame) {
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
    int size = var->sem_type.info.val == VAL_STRUCT ? (int)var->sem_type.info.size : 1;
    var->frame_index = frame_index;
    frame_index += size;
  }

  frame->max_stack_size = frame_index > frame->max_stack_size ? frame_index : frame->max_stack_size;

  for (ListNode_* n = scope->children.head; n != NULL; n = n->next) {
    Scope_* s = list_val(n, Scope_*);
    analyze_scope(frame, s, frame_index);
  }
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
  AS_EXPR(expr->expr)->top_sem_type = expr->top_sem_type;
  do_analysis((AstNode_*)expr->expr);
  expr->sem_type = AS_EXPR(expr->expr)->sem_type;
}

void print_analysis(AstNode_* n) {
  AstPrintStmt_* stmt = (AstPrintStmt_*)n;
  stmt->expr->top_sem_type.info.kind = KIND_VAL;
  do_analysis((AstNode_*)stmt->expr);
}

void unary_analysis(AstNode_* n) {
  AstUnaryExp_* expr = (AstUnaryExp_*)n;
  
  expr->expr->top_sem_type = expr->base.top_sem_type;
  do_analysis((AstNode_*)expr->expr);
  expr->base.sem_type = AS_EXPR(expr->expr)->sem_type;
  switch (expr->op) {
    case TK_TILDE:
      if (!semantictype_isanumber(expr->base.sem_type)) {
        error(analyzer_, n, "expression does not have a number type.");
      }
      break;

    case TK_NOT:
      if (!semantictype_isabool(expr->base.sem_type)) {
        error(analyzer_, n, "expression does not have a boolean type.");
      }
      break;

    case TK_MINUS:
      if (!semantictype_isanumber(expr->base.sem_type)) {
        error(analyzer_, n, "expression does not have a number type.");
      }
      break;

    case TK_AMPERSAND:
    {
      SemanticType_ sem_type = expr->base.sem_type;
      if (expr->expr->base.cls != AST_CLS(AstVarExpr_)) {
        error(analyzer_, (AstNode_*)expr->expr, "Cannot take address expression");
      }

      if (sem_type.info.kind == KIND_UNKNOWN ||          
          sem_type.info.kind == KIND_REF ||
          sem_type.info.lifetime == LIFETIME_TMP) {
        error(analyzer_, n, "expression is not assignable");
      }

      expr->base.sem_type.info.kind = KIND_REF;
      if (sem_type.info.kind == KIND_VAL) {
        expr->base.sem_type.info.ref_kind = REF_KIND_WEAK;
      } else {
        expr->base.sem_type.info.ref_kind = REF_KIND_STRONG;
      }
      break;
    }

    case TK_NEW:
    {
      SemanticType_* type = &expr->base.sem_type;

      if (type->info.kind == KIND_REF) {
        error(analyzer_, n, "Cannot create allocate references");
      }

      type->info.kind = KIND_REF;
      type->info.ref_kind = REF_KIND_STRONG;
      type->info.lifetime = LIFETIME_AUTOMATIC;

      break;
    }

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

  if (semantictype_isunknown(lsem_type)) {
    error(analyzer_, n, "left-hand expression type could not be deduced.");
    return;
  }

  if (semantictype_isunknown(rsem_type)) {
    error(analyzer_, n, "right-hand expression type could not be deduced.");
    return;
  }

  if (!semantictype_iscoercible(rsem_type, lsem_type)) {
    error(analyzer_, n, "right-hand expression cannot be coerced to left-hand expression.");
  }

  RuntimeType_ expected_type = UNKNOWN_TY;
  switch (expr->op) {
    case TK_AND: 
    case TK_OR:
    case TK_XOR:
      expr->base.sem_type = semantictype_as(VAL_BOOL);
      expected_type = BOOL_TY;
      break;

    case TK_GT:
    case TK_GTE:
    case TK_LT:
    case TK_LTE:
    case TK_EQUAL_EQUAL:
    case TK_BANG_EQUAL:
      expr->base.sem_type = semantictype_as(VAL_BOOL);
      break;

    case TK_AMPERSAND:
    case TK_PIPE:
    case TK_HAT:
      expr->base.sem_type = lsem_type;
      if (!semantictype_isainteger(expr->base.sem_type)) {
        error(analyzer_, n, "expected the expression type to be a number.");
      }
      break;

    case TK_PLUS:
    case TK_MINUS:
    case TK_STAR:
    case TK_SLASH:
      expr->base.sem_type = lsem_type;
      if (!semantictype_isanumber(expr->base.sem_type) && !semantictype_isastring(expr->base.sem_type)) {
        error(analyzer_, n, "expected the expression type to be a number or a string.");
      }
      break;

    // case TK_DOUBLE_SLASH:
    case TK_PERCENT:
    case TK_LSHIFT:
    case TK_RSHIFT:
      expr->base.sem_type = lsem_type;
      expected_type = INT_TY;
      expected_type.kind = KIND_VAL;
      break;

    default:
      error(analyzer_, n, "unimplemented token type.");
      break;
  }

  if (!IS_TY_UNKNOWN(expected_type)) {
    if (lsem_type.info.val != expected_type.ty) {
      error(analyzer_, n, "left-hand expression does not have expected type of %s.", valuetype_str(expected_type));
    }

    if (rsem_type.info.val != expected_type.ty) {
      error(analyzer_, n, "right-hand expression does not have expected type of %s.", valuetype_str(expected_type));
    }
  }

  if (lsem_type.info.lifetime == LIFETIME_STATIC && rsem_type.info.lifetime == LIFETIME_STATIC) {
    expr->base.sem_type.info.lifetime = LIFETIME_STATIC;
  } else {
    expr->base.sem_type.info.lifetime = LIFETIME_TMP;
  }
}

void primary_analysis(AstNode_* n) {}

void return_analysis(AstNode_* n) {
  AstReturnStmt_* stmt = (AstReturnStmt_*)n;
  Frame_* frame = stmt->base.scope->frame;
  stmt->expr->top_sem_type = frame->fn_symbol->fn.return_type;

  do_analysis((AstNode_*)stmt->expr);
  if (AS_EXPR(stmt->expr)->sem_type.info.val != frame->fn_symbol->fn.return_type.info.val) {
    error(analyzer_, n, "Return statement type does not match function type.");
  }
}

void if_analysis(AstNode_* n) {
  AstIfStmt_* stmt = (AstIfStmt_*)n;
  stmt->condition_expr->top_sem_type = semantictype_frominfo(VAL_BOOL, KIND_VAL, OBJ_TYPE_UNKNOWN);

  do_analysis((AstNode_*)stmt->condition_expr);
  if (!semantictype_isabool(AS_EXPR(stmt->condition_expr)->sem_type)) {
    error(analyzer_, n, "if expression must have a boolean type.");
  }

  do_analysis(stmt->if_stmt);
  if (stmt->else_stmt) {
    do_analysis(stmt->else_stmt);
  }

  for (AstListNode_* n = stmt->elif_exprs.head; n != NULL; n = n->next) {
    do_analysis(n->node);
    if (!semantictype_isabool(AS_EXPR(n->node)->sem_type)) {
      error(analyzer_, n->node, "elif expression must have a boolean type.");
    }
  }
  for (AstListNode_* n = stmt->elif_stmts.head; n != NULL; n = n->next) {
    do_analysis(n->node);
  }
}

void assert_analysis(AstNode_* n) {
  AstAssertStmt_* stmt = (AstAssertStmt_*)n;
  stmt->expr->top_sem_type = semantictype_frominfo(VAL_BOOL, KIND_VAL, OBJ_TYPE_UNKNOWN);
  do_analysis((AstNode_*)stmt->expr);

  if (!semantictype_isabool(AS_EXPR(stmt->expr)->sem_type)) {
    error(analyzer_, n, "assert expression must have a boolean type.");
  }
}

void var_decl_analysis(AstNode_* n) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)n;
  if (!semantictype_isunknown(stmt->sem_type)) {
    stmt->expr->top_sem_type = stmt->sem_type;
  }

  if (stmt->expr) {
    do_analysis((AstNode_*)stmt->expr);
  }

  if (semantictype_isunknown(stmt->sem_type)) {
    assertf(stmt->expr, "type deduced variable must have an expression");
    stmt->sem_type = AS_EXPR(stmt->expr)->sem_type;
  }

  // The type will be KIND_TMP if created from a PRIMARY_EXP. Only change the kind for
  // non-pointer/reference types.
  if (stmt->sem_type.info.kind == KIND_UNKNOWN) {
    if (stmt->sem_type.info.val == VAL_OBJ) {
      stmt->sem_type.info.kind = KIND_REF;
    } else {
      stmt->sem_type.info.kind = KIND_VAL;
    }
  }

  stmt->sem_type.info.lifetime = LIFETIME_AUTOMATIC;

  // TODO: implement tuples (and others) for variable declarations.
  VarSymbol_* var = scope_var(n->scope, &stmt->name);  
  var->sem_type = stmt->sem_type;
  
  if (stmt->expr && !semantictype_iscoercible(AS_EXPR(stmt->expr)->sem_type, stmt->sem_type)) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }
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
      case SYMBOL_TYPE_STRUCT:
        expr->base.sem_type = (SemanticType_){
          .info = {
            .val = VAL_STRUCT,
            .kind = KIND_UNKNOWN,
            .obj = OBJ_TYPE_UNKNOWN,
            .sym = sym
          },
          .name = sym->name,
          .sym = sym
        };
        break;

      case SYMBOL_TYPE_FN:
        expr->base.sem_type = sym->fn.return_type;
        expr->base.sem_type.sym = sym;
        break;

      case SYMBOL_TYPE_VAR:
        expr->base.sem_type = sym->var.sem_type;
        expr->base.sem_type.sym = sym;
        break;

      case SYMBOL_TYPE_CLOSURE:
        expr->base.sem_type = sym->closure.fn->fn.return_type;
        expr->base.sem_type.sym = sym;
        break;

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
  

  // TODO: allow for deconstructing tuples in assignments (and other types).
  expr->right->top_sem_type = expr->base.top_sem_type;
  do_analysis((AstNode_*)expr->left);
  do_analysis((AstNode_*)expr->right);

  if (expr->left->cls != AST_CLS(AstVarExpr_)) {
    error(analyzer_, n, "Left-hand side of assignment cannot be assigned to.");
    return;
  }

  AstVarExpr_* lvalue_expr = (AstVarExpr_*)expr->right;
  AstExpr_* rvalue_expr = (AstExpr_*)expr->right;
  if (!semantictype_infoequal(lvalue_expr->base.sem_type, rvalue_expr->sem_type)) {
    error(analyzer_, n, "assignment expression does not match variable type.");
  }
  expr->base.sem_type = lvalue_expr->base.sem_type;
}

void while_stmt_analysis(AstNode_* n) {
  AstWhileStmt_* stmt = (AstWhileStmt_*)n;
  stmt->condition_expr->top_sem_type = semantictype_frominfo(VAL_BOOL, KIND_VAL, OBJ_TYPE_UNKNOWN);

  do_analysis((AstNode_*)stmt->condition_expr);
  do_analysis(stmt->block_stmt);

  if (!semantictype_isabool(AS_EXPR(stmt->condition_expr)->sem_type)) {
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

  def->base.sem_type = (SemanticType_) {
    .info = {
      .val = fn->return_type.info.val,
      .kind = KIND_VAL,
      .obj = OBJ_TYPE_FUNCTION,
      .lifetime = LIFETIME_STATIC,
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
  if (semantictype_isunknown(param->type)) {
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
  FunctionSymbol_* fn_sym = symbol_ascallable(sym);

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
  FunctionSymbol_* fn_sym = args->fn_sym;

  if (fn_sym->params.count > UINT8_MAX) {
    error(analyzer_, node, "Parameter count exceeded maximum of 255.");
  }

  List_* params = &args->fn_sym->params;
  ListNode_* param_node = params->head;
  for (AstListNode_* n = args->args.head; n != NULL; n = n->next) {
    Symbol_* param = list_val(param_node, Symbol_*);
    SemanticType_ param_sem_type = SemanticType_Unknown;
    if (param->type == SYMBOL_TYPE_VAR) {
      param_sem_type = param->var.sem_type;
    } else if (param->type == SYMBOL_TYPE_FIELD) {
      param_sem_type = param->field.sem_type;
    } else {
      error(analyzer_, node, "Encountered unknown parameter type %d.", param->type);
    }
    AS_EXPR(n->node)->top_sem_type = param_sem_type;
    do_analysis(n->node);
    SemanticType_ expr_sem_type = AS_EXPR(n->node)->sem_type;

    if (param_sem_type.info.val != expr_sem_type.info.val ||
      (param_sem_type.info.kind == KIND_REF &&
        expr_sem_type.info.kind != KIND_REF)) {
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

void ast_struct_def_analysis(AstNode_* node) {
  AstStructDef_* def = (AstStructDef_*)node;

  for (AstListNode_* n = def->members.head; n != NULL; n = n->next) {
    do_analysis(n->node);
  }

  Symbol_* struct_sym = def->struct_type.info.sym;
  if (semantictype_hascycle(&def->struct_type)) {
    error(analyzer_, node, "struct \"%.*s\" has a cycle", struct_sym->name.length, struct_sym->name.start);
    return;
  }

  semantictype_size(&def->struct_type);
  def->struct_type.info.sym->strct.self_type = def->struct_type;
}

static Value_ fold_constants(Scope_* scope, AstExpr_* expr) {
  switch (expr->base.cls) {
    case AST_CLS(AstPrimaryExp_):
      return AST_CAST(AstPrimaryExp_, expr)->value;
  }
  
  assertf(false, "Unimplemented expression type for constant folding: %d", expr->base.cls);
}

void ast_struct_member_decl_analysis(AstNode_* node) {
  AstStructMemberDecl_* decl = (AstStructMemberDecl_*)node;
  FieldSymbol_* field = &decl->sem_type.info.sym->field;
  SemanticType_* type = &decl->sem_type.info.sym->field.sem_type;

  if (type->info.val == VAL_UNKNOWN) {
    if (type->name.length == 0) {
      error(analyzer_, node, "Encountered unknown type in struct member declaration");
    } else {
      Symbol_* symbolic_type = scope_find(node->scope, &type->name);
      if (!symbolic_type) {
        error(analyzer_, node,
          "Could not find type '%.*s' in struct member declaration",
          symbolic_type->name.length, symbolic_type->name);
      } else {
        *type = symbolic_type->strct.self_type;
      }
    }
  }

  if (decl->opt_expr) {
    AstExpr_* expr = decl->opt_expr;
    expr->top_sem_type.info.kind = KIND_VAL;
    do_analysis((AstNode_*)expr);

    if (expr->sem_type.info.kind != KIND_VAL &&
      expr->sem_type.info.lifetime != LIFETIME_STATIC &&
      expr->sem_type.info.lifetime != LIFETIME_TMP) {

      error(analyzer_, node, "struct member field default value must be a value.");
      return;
    }

    field->val = fold_constants(node->scope, expr);
    field->has_default_val = true;
  } else {
    field->val = NIL_VAL;
    field->has_default_val = false;
  }

  decl->sem_type = *type;
}

void ast_dot_expr_analysis(AstNode_* node) {
  AstDotExpr_* expr = AST_CAST(AstDotExpr_, node);

  expr->prefix->top_sem_type = expr->base.top_sem_type;
  do_analysis((AstNode_*)expr->prefix);


  SemanticType_ prefix_type = expr->prefix->sem_type;
  if (prefix_type.info.val != VAL_STRUCT) {
    error(analyzer_, (AstNode_*)expr->prefix, "Expected struct type for sub-expression.");
    return;
  }

  if (!prefix_type.sym) {
    error(analyzer_, (AstNode_*)expr->prefix, "Unknown error: no semantic information for prefix.");
    return;
  }

  StructSymbol_* struct_sym = NULL;

  if (prefix_type.sym->type == SYMBOL_TYPE_VAR) {
    VarSymbol_* sym = &prefix_type.sym->var;
    struct_sym = &sym->sem_type.sym->strct;
  } else if (prefix_type.sym->type == SYMBOL_TYPE_STRUCT) {
    struct_sym = &prefix_type.sym->strct;
  } else {
    error(analyzer_, (AstNode_*)expr->prefix, "Unknown error: unexpected symbol type.");
  }

  Symbol_* found = NULL;
  for (ListNode_* n = struct_sym->members.head; n != NULL; n = n->next) {
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

  expr->base.sem_type = found->field.sem_type;
  expr->base.sem_type.info.sym = found;
}

void ast_constructor_analysis(AstNode_* node) {
  AstConstructor_* constructor = (AstConstructor_*)node;

  SemanticType_* type = &constructor->base.sem_type;
  if (type->name.start) {
    Symbol_* struct_sym = scope_find(node->scope, &type->name);

    if (!struct_sym) {
      error(analyzer_, node, "Could not find struct %.*s", type->name.length, type->name.start);
    } else {
      constructor->base.sem_type = struct_sym->strct.self_type;
      constructor->base.sem_type.sym = struct_sym;
    }
  }

  for (AstListNode_* n = constructor->fields.head; n != NULL; n = n->next) {
    AstConstructorField_* field = AST_CAST(AstConstructorField_, n->node);
    field->base.top_sem_type = constructor->base.top_sem_type;
    do_analysis((AstNode_*)field);
  }
}

void ast_constructor_field_analysis(AstNode_* node) {
  AstConstructorField_* field = (AstConstructorField_*)node;
  field->expr->top_sem_type = field->base.top_sem_type;
  do_analysis((AstNode_*)field->expr);
  field->base.sem_type = field->expr->sem_type;
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
  [AST_CLS(AstStructDef_)]              = {ast_struct_def_analysis},
  [AST_CLS(AstStructMemberDecl_)]       = {ast_struct_member_decl_analysis},
  [AST_CLS(AstConstructor_)]            = {ast_constructor_analysis},
  [AST_CLS(AstConstructorField_)]       = {ast_constructor_field_analysis},
  [AST_CLS(AstDotExpr_)]                = {ast_dot_expr_analysis},
  [AST_CLS(AstTypeExpr_)]               = {noop_analysis},
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