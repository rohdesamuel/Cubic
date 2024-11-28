#ifndef TYPE_EXPR__H
#define TYPE_EXPR__H

#include "memory.h"
#include "tokens.h"
#include "value.h"

// A type expression resolves to a type.
#define TYPE_EXPR_CLS(TYPE)  TYPE_EXPR_##TYPE
typedef struct TypeExpr_ {
  enum {
    TYPE_EXPR_CLS(TypeExprError_),
    TYPE_EXPR_CLS(TypeExprPrimitive_),
    TYPE_EXPR_CLS(TypeExprSymbol_),
    TYPE_EXPR_CLS(TypeExprUnion_),
    TYPE_EXPR_CLS(TypeExprTuple_),
    TYPE_EXPR_CLS(TypeExprIn_),
    TYPE_EXPR_CLS(TypeExprOut_),
    TYPE_EXPR_CLS(TypeExprVar_),
    TYPE_EXPR_CLS(TypeExprConstraint_),
    TYPE_EXPR_CLS(TypeExprGenericParam_),
    TYPE_EXPR_CLS(TypeExprFunction_),
    TYPE_EXPR_CLS(TypeExprClass_),
    TYPE_EXPR_CLS(TypeExprClassMember_),
    TYPE_EXPR_CLS(TypeExprGenericOrArrayType_),
    TYPE_EXPR_CLS(TypeExprPrimary_),
  } cls;

  Token_ name;

  // Type parameters, AKA generic parameters.
  ListOf_(const TypeExprGenericParam_*) params;
} TypeExpr_;

void type_expr_init();

typedef struct TypeExprPrimitive_ {
  TypeExpr_ base;
  const struct Type_* primitive_ty;
} TypeExprPrimitive_;

#define DECL_PRIMITIVE_TYPE_EXPR(NAME) extern const TypeExpr_* NAME##_TypeExpr;
DECL_PRIMITIVE_TYPE_EXPR(Nil);
DECL_PRIMITIVE_TYPE_EXPR(Bool);
DECL_PRIMITIVE_TYPE_EXPR(Int);
DECL_PRIMITIVE_TYPE_EXPR(Int8);
DECL_PRIMITIVE_TYPE_EXPR(Int16);
DECL_PRIMITIVE_TYPE_EXPR(Int32);
DECL_PRIMITIVE_TYPE_EXPR(Int64);
DECL_PRIMITIVE_TYPE_EXPR(Uint);
DECL_PRIMITIVE_TYPE_EXPR(Uint8);
DECL_PRIMITIVE_TYPE_EXPR(Uint16);
DECL_PRIMITIVE_TYPE_EXPR(Uint32);
DECL_PRIMITIVE_TYPE_EXPR(Uint64);
DECL_PRIMITIVE_TYPE_EXPR(Float);
DECL_PRIMITIVE_TYPE_EXPR(Double);
DECL_PRIMITIVE_TYPE_EXPR(String);

typedef struct TypeExprError_ {
  TypeExpr_ base;
} TypeExprError_;

typedef struct TypeExprSymbol_ {
  TypeExpr_ base;
  Token_ symbol;
} TypeExprSymbol_;

typedef struct TypeExprUnion_ {
  TypeExpr_ base;
  ListOf_(const TypeExpr_*) types;
} TypeExprUnion_;

typedef struct TypeExprTuple_ {
  TypeExpr_ base;
  ListOf_(const TypeExpr_*) types;
} TypeExprTuple_;

typedef struct TypeExprIn_ {
  TypeExpr_ base;
  const TypeExpr_* type;
} TypeExprIn_;

typedef struct TypeExprOut_ {
  TypeExpr_ base;
  const TypeExpr_* type;
} TypeExprOut_;

typedef struct TypeExprVar_ {
  TypeExpr_ base;
  const TypeExpr_* type;
} TypeExprVar_;

typedef struct TypeExprConstraint_ {
  TypeExpr_ base;
  Token_ name;
  ListOf_(const TypeExpr_*) constraints;
} TypeExprConstraint_;

typedef struct TypeExprFunction_ {
  TypeExpr_ base;
  Token_ name;
  const TypeExpr_* ret_type;
  ListOf_(const TypeExpr_*) params;
} TypeExprFunction_;

typedef struct TypeExprGenericParam_ {
  TypeExpr_ base;
  Token_ name;
  const TypeExpr_* type;
} TypeExprGenericParam_;

typedef struct TypeExprClass_ {
  TypeExpr_ base;
  Token_ name;

  ListOf_(const TypeExprClassMember_*) members;
} TypeExprClass_;

typedef struct TypeExprClassMember_ {
  TypeExpr_ base;
  Token_ name;
  const TypeExpr_* type;
} TypeExprClassMember_;

typedef struct TypeExprGenericOrArrayType_ {
  TypeExpr_ base;
  const TypeExpr_* prefix;
  ListOf_(TypeExpr_*) args;
} TypeExprGenericOrArrayType_;

typedef struct TypeExprPrimary_ {
  TypeExpr_ base;
  TokenType_ type;
  Value_ value;
} TypeExprPrimary_;

const struct Type_* resolve_typeexpr(
  const TypeExpr_* type, ListOf_(const TypeExpr_*)* opt_args, struct Scope_* scope, struct ErrorsContainer_* error, MemoryAllocator_* allocator);

const TypeExpr_* make_symbol_typeexpr(Token_ tk, MemoryAllocator_* allocator);
const TypeExpr_* make_union_typeexpr(ListOf_(const TypeExpr_*)* types, MemoryAllocator_* allocator);
const TypeExpr_* make_tuple_typeexpr(ListOf_(const TypeExpr_*)* types, MemoryAllocator_* allocator);
const TypeExpr_* make_in_typeexpr(const TypeExpr_* type, MemoryAllocator_* allocator);
const TypeExpr_* make_out_typeexpr(const TypeExpr_* type, MemoryAllocator_* allocator);
const TypeExpr_* make_var_typeexpr(const TypeExpr_* type, MemoryAllocator_* allocator);
const TypeExpr_* make_constraint_typeexpr(Token_ name, ListOf_(const TypeExpr_*)* types, MemoryAllocator_* allocator);
const TypeExpr_* make_genericparam_typeexpr(Token_ name, const TypeExpr_* type, MemoryAllocator_* allocator);
const TypeExpr_* make_function_typeexpr(Token_ name, const TypeExpr_* return_type,
  ListOf_(const TypeExpr_*)* params, ListOf_(const TypeExprGenericParam_*)* type_params,
  MemoryAllocator_* allocator);
const TypeExpr_* make_class_typeexpr(Token_ name,
  ListOf_(const TypeExprClass_*)* members, ListOf_(const TypeExprGenericParam_*)* type_params,
  MemoryAllocator_* allocator);
const TypeExpr_* make_class_member_typeexpr(Token_ field_name, const TypeExpr_* field_type,
  MemoryAllocator_* allocator);
const TypeExpr_* make_generic_or_array_typeexpr(const TypeExpr_* prefix, ListOf_(TypeExpr_*)* args, MemoryAllocator_* allocator);
const TypeExpr_* make_primary_typeexpr(TokenType_ type, Value_ val, MemoryAllocator_* allocator);

#endif  // TYPE_EXPR__H