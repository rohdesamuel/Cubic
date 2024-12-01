#include "type_parser.h"

#include "cst.h"
#include "memory.h"
#include "type_expr.h"

typedef const TypeExpr_* (*ParseFn)(const CstNode_* node, MemoryAllocator_* allocator);
typedef struct ParseRule_ {
  ParseFn fn;
} ParseRule_;
static ParseRule_* get_rule(int info);

static const TypeExpr_* do_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  return get_rule(node->cls)->fn(node, allocator);
}

const struct TypeExpr_* parse_type(const struct CstNode_* node, MemoryAllocator_* allocator) {
  if (!node) return NULL;

  return do_parse(node, allocator);
}

static const TypeExpr_* cst_union_type_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstUnionType_* cst = (CstUnionType_*)node;
  
  ListOf_(TypeExpr_*) types = { 0 };
  list_of(&types, TypeExpr_*, allocator);

  for (CstListNode_* n = cst->types.head; n != NULL; n = n->next) {
    list_push(&types, parse_type(n->node, allocator));
  }

  return make_union_typeexpr(&types, allocator);
}

static const TypeExpr_* cst_tuple_type_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstTupleType_* cst = (CstTupleType_*)node;

  ListOf_(TypeExpr_*) types = { 0 };
  list_of(&types, TypeExpr_*, allocator);

  for (CstListNode_* n = cst->types.head; n != NULL; n = n->next) {
    list_push(&types, parse_type(n->node, allocator));
  }

  return make_tuple_typeexpr(&types, allocator);
}

static const TypeExpr_* cst_primitive_type_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstPrimitiveType_* cst = (CstPrimitiveType_*)node;
  switch (cst->type) {
    case TK_BOOL:        return Bool_TypeExpr;
    case TK_INT:         return Int_TypeExpr;
    case TK_UINT:        return Uint_TypeExpr;
    case TK_FLOAT:       return Float_TypeExpr;
    case TK_DOUBLE:      return Double_TypeExpr;
    case TK_STRING_TYPE: return String_TypeExpr;
    case TK_NIL:         return Nil_TypeExpr;
  }

  assert(false && "Unknown primitive type");
  return NULL;
}

static const TypeExpr_* cst_id_type_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstIdType_* cst = (CstIdType_*)node;
  return make_symbol_typeexpr(cst->id, allocator);
}

static const TypeExpr_* cst_reference_type_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstReferenceType_* cst = (CstReferenceType_*)node;
  return make_var_typeexpr(do_parse(cst->type, allocator), allocator);
}

static const TypeExpr_* cst_type_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstType_* cst = (CstType_*)node;
  return do_parse(cst->impl, allocator);
}

static const TypeExpr_* cst_generic_param_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstGenericParam_* cst = (CstGenericParam_*)node;

  ListOf_(const TypeExpr_*) constraints = { 0 };
  list_of(&constraints, TypeExpr_*, allocator);

  for (CstListNode_* n = cst->constraints.head; n != NULL; n = n->next) {
    list_push(&constraints, do_parse(n->node, allocator));
  }

  const TypeExpr_* constraints_ty = make_union_typeexpr(&constraints, allocator);
  return make_genericparam_typeexpr(cst->name, constraints_ty, allocator);
}

static const TypeExpr_* cst_var_or_type_expr_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstVarOrTypeExpr_* cst = (CstVarOrTypeExpr_*)node;
  assert(false && "unimplemented");
  return NULL;
}

static const TypeExpr_* cst_generic_or_array_type_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstGenericOrArrayType_* cst = (CstGenericOrArrayType_*)node;

  const TypeExpr_* prefix = do_parse(cst->prefix, allocator);
  ListOf_(TypeExpr_*) args = { 0 };
  list_of(&args, const TypeExpr_*, allocator);

  for (CstListNode_* n = cst->args.head; n != NULL; n = n->next) {
    const TypeExpr_* arg = do_parse(n->node, allocator);
    list_push(&args, &arg);
  }

  return make_generic_or_array_typeexpr(prefix, &args, allocator);
}

static const TypeExpr_* cst_function_def_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstFunctionDef_* cst = (CstFunctionDef_*)node;
  
  ListOf_(TypeExpr_*) type_params = { 0 };
  ListOf_(TypeExpr_*) params = { 0 };
  list_of(&type_params, TypeExpr_*, allocator);
  list_of(&params, TypeExpr_*, allocator);

  for (CstListNode_* n = cst->generic_params.head; n != NULL; n = n->next) {
    const TypeExpr_* param = do_parse(n->node, allocator);
    list_push(&type_params, &param);
  }

  for (CstListNode_* n = cst->function_params.head; n != NULL; n = n->next) {
    const TypeExpr_* param = do_parse(n->node, allocator);
    list_push(&params, &param);
  }

  return make_function_typeexpr(
    /* name = */ cst->name,
    /* ret = */ do_parse(cst->return_type, allocator),
    /* params =  */ &params,
    /* type_params =  */ &type_params,
    allocator);
}

static const TypeExpr_* cst_function_param_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstFunctionParam_* cst = (CstFunctionParam_*)node;
  const TypeExpr_* type = do_parse(cst->type, allocator);

  switch (cst->kind) {
    case 0:
      break;

    case TK_IN:
      type = make_in_typeexpr(type, allocator);
      break;

    case TK_OUT:
      type = make_out_typeexpr(type, allocator);
      break;

    default:
      assertf(false, "Unknown token type.");
      break;
  }

  return type;
}

static const TypeExpr_* cst_class_def_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstClassDef_* cst = (CstClassDef_*)node;
  
  ListOf_(TypeExpr_*) type_params = { 0 };
  ListOf_(TypeExpr_*) members = { 0 };
  list_of(&type_params, TypeExpr_*, allocator);
  list_of(&members, TypeExpr_*, allocator);

  for (CstListNode_* n = cst->generic_params.head; n != NULL; n = n->next) {
    const TypeExpr_* val = do_parse(n->node, allocator);
    list_push(&type_params, &val);
  }

  for (CstListNode_* n = cst->members.head; n != NULL; n = n->next) {
    const TypeExpr_* val = do_parse(n->node, allocator);
    list_push(&members, &val);
  }

  return make_class_typeexpr(
    /* name = */ cst->name,
    /* members =  */ &members,
    /* type_params =  */ &type_params,
    allocator);
}

static const TypeExpr_* cst_class_member_decl_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstClassMemberDecl_* cst = (CstClassMemberDecl_*)node;
  return make_class_member_typeexpr(cst->name, do_parse(cst->field_type, allocator), allocator);
}

static const TypeExpr_* cst_type_def_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstTypeDef_* cst = (CstTypeDef_*)node;
  assert(false && "unimplemented");
  return NULL;
}

static const TypeExpr_* cst_primary_exp_parse(const CstNode_* node, MemoryAllocator_* allocator) {
  CstPrimaryExp_* cst = (CstPrimaryExp_*)node;
  return make_primary_typeexpr(cst->type, cst->value, allocator);
}

static ParseRule_ parse_rules[] = {
  [CST_CLS(CstPrimaryExp_)]         = {cst_primary_exp_parse},
  [CST_CLS(CstFunctionDef_)]        = {cst_function_def_parse},
  [CST_CLS(CstFunctionParam_)]      = {cst_function_param_parse},
  [CST_CLS(CstClassDef_)]           = {cst_class_def_parse},
  [CST_CLS(CstClassMemberDecl_)]    = {cst_class_member_decl_parse},
  [CST_CLS(CstUnionType_)]          = {cst_union_type_parse},
  [CST_CLS(CstTupleType_)]          = {cst_tuple_type_parse},
  [CST_CLS(CstPrimitiveType_)]      = {cst_primitive_type_parse},
  [CST_CLS(CstIdType_)]             = {cst_id_type_parse},
  [CST_CLS(CstReferenceType_)]      = {cst_reference_type_parse},
  [CST_CLS(CstType_)]               = {cst_type_parse},
  [CST_CLS(CstTypeDef_)]            = {cst_type_def_parse},
  [CST_CLS(CstGenericParam_)]       = {cst_generic_param_parse},
  [CST_CLS(CstVarOrTypeExpr_)]    = {cst_var_or_type_expr_parse},
  [CST_CLS(CstGenericOrArrayType_)] = {cst_generic_or_array_type_parse},
};

static ParseRule_* get_rule(int info) {
  ParseRule_* ret = &parse_rules[info];
  assertf(ret->fn, "Could not find analysis rule for CST class: %d", info);
  return ret;
}