#ifndef TYPE__H
#define TYPE__H

#include <cstdint>
#include <vector>

#include "common.h"
#include "tokens.h"
#include "value.h"

enum ValueKind {
  KIND_UNKNOWN,

  // A named variable that is stored on stack or in a struct.
  // ex. val a := 0
  KIND_VAL,

  // A named variable that is stored on the heap that does not need to be
  // explicitly dereferenced.
  // ex. var a := 0
  KIND_VAR,

  // Non-null pointer that does not need to be dereferenced.
  // ex. function foo (in a : int) ... end
  KIND_REF,
};

enum ConstKind {
  CONST_KIND_UNKNOWN,

  // Mutable. No const restrictions.
  CONST_KIND_NONE,

  // Immutable. Full const restrictions.
  CONST_KIND_WHOLE,
};

enum ValueLifetime {
  LIFETIME_UNKNOWN,

  // Automatic lifetime (will be destructed without user intervention).
  LIFETIME_AUTOMATIC,

  // An intermediate result and used once (rvalue).
  LIFETIME_TMP,

  // Type is a static constant and can be cached for multiple uses.
  LIFETIME_STATIC,
};

#define TYPE_CLS(name) TYPE_##name
#define TYPE_NAME(name) name __##name##impl__

// TODO: implement types as UnionTypes.
#define TYPE_LIST(TYPE_OP, DELIM) \
  TYPE_OP(NilType) DELIM \
  TYPE_OP(BoolType) DELIM \
  TYPE_OP(IntType) DELIM \
  TYPE_OP(Int8Type) DELIM \
  TYPE_OP(Int16Type) DELIM \
  TYPE_OP(Int32Type) DELIM \
  TYPE_OP(Int64Type) DELIM \
  TYPE_OP(UintType) DELIM \
  TYPE_OP(Uint8Type) DELIM \
  TYPE_OP(Uint16Type) DELIM \
  TYPE_OP(Uint32Type_) DELIM \
  TYPE_OP(Uint64Type) DELIM \
  TYPE_OP(FloatType) DELIM \
  TYPE_OP(DoubleType) DELIM \
  TYPE_OP(StringType) DELIM \
  TYPE_OP(FieldType) DELIM \
  TYPE_OP(ClassType) DELIM \
  TYPE_OP(ArrayType) DELIM \
  TYPE_OP(UnionType) DELIM \
  TYPE_OP(TupleType) DELIM \
  TYPE_OP(FunctionType) DELIM \
  TYPE_OP(PrimaryType)


#define GENERATE_TYPE_ENUMS(TYPE) TYPE_CLS(TYPE)
#define GENERATE_TYPE_UNION(TYPE) TYPE_NAME(TYPE)

struct Type {
  enum TypeCls : uint32_t {
    TYPE_LIST(GENERATE_TYPE_ENUMS, CB_COMMA),
    __TYPE_COUNT__,

    __TYPE_PRIMITIVE_START__ = TYPE_CLS(NilType),
    __TYPE_PRIMITIVE_END__ = TYPE_CLS(StringType),

    __TYPE_UNARY_START__ = TYPE_CLS(FieldType),
    __TYPE_UNARY_END__ = TYPE_CLS(FieldType),

    __TYPE_COMPOSITE_START__ = TYPE_CLS(ClassType),
    __TYPE_COMPOSITE_END__ = TYPE_CLS(TupleType),
  } cls = TYPE_CLS(NilType);

  ValueKind val_kind = KIND_UNKNOWN;
  ConstKind const_kind = CONST_KIND_UNKNOWN;
  ValueLifetime val_lifetime = LIFETIME_UNKNOWN;

  // The size of this type in memory, not following pointers.
  size_t size = 0;

  // The scope this type belongs to.
  // struct Scope_* scope;
  // Token opt_name;
  uint64_t id = 0;
};

struct PrimitiveType : public Type { };

#define DEF_PRIMITIVE_TY(NAME) \
PrimitiveType NAME##Type; \
extern const PrimitiveType* NAME##_Ty;

DEF_PRIMITIVE_TY(Nil);
DEF_PRIMITIVE_TY(Bool);
DEF_PRIMITIVE_TY(Int);
DEF_PRIMITIVE_TY(Int8);
DEF_PRIMITIVE_TY(Int16);
DEF_PRIMITIVE_TY(Int32);
DEF_PRIMITIVE_TY(Int64);
DEF_PRIMITIVE_TY(Uint);
DEF_PRIMITIVE_TY(Uint8);
DEF_PRIMITIVE_TY(Uint16);
DEF_PRIMITIVE_TY(Uint32);
DEF_PRIMITIVE_TY(Uint64);
DEF_PRIMITIVE_TY(Float);
DEF_PRIMITIVE_TY(Double);
DEF_PRIMITIVE_TY(String);


struct UnaryType : public Type {
  Type* ty;
};

struct MultiType : public Type {
  std::vector<Type*> types;
};

struct ArrayType : public Type {
  Type* el_type;
  size_t count;
};

struct UnionType : public MultiType {
  Type* selected_type;
};

struct TupleType : public MultiType { };

struct FieldType : public UnaryType {
  Token name;
};

struct ClassTypeField {
  Type* type;

  // Name of the field.
  Token name;

  // Byte frame_offset into the outer-most class.
  size_t offset;

  struct AstExpr_* opt_expr;
};

struct ClassType : public Type {
  const struct FunctionType* constructor;
  std::vector<ClassTypeField> members;
  struct Scope_* scope;
};

struct FunctionType {
  Type* ret_ty;
  std::vector<Type*> params;
};

#if 0
Type* make_placeholder_ty(const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type* make_const_ty(Type* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type* make_var_ty(Type* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type* make_ref_ty(Type* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type* make_in_ty(Type* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type* make_out_ty(Type* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type* make_array_ty(Type* el_type, size_t count, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);

// Class types are singletons. For each class type, there is only one Type*
// instance.
Type* make_class_ty(Token_ name, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type* make_function_ty(Token_ name, ListOf_(Type*)* params, Type* ret_ty, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type* make_tuple_ty(const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator, int n, ...);
Type* make_union_ty(const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator, int n, ...);
Type* make_constraint_ty(const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator, Token_ name, int n, ...);
Type* make_field_ty(Token_ field_name, Type* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type* make_genericparam_ty(Token_ name, Type* constraint, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);

Type* type_alloc(MemoryAllocator_* allocator, struct Scope_* scope, const struct TypeExpr_* tmpl, int type_cls, size_t type_size);

#define type_alloc_ty(ALLOCATOR, SCOPE, TYPE_EXPR, TY) ((TY*)type_alloc(ALLOCATOR, SCOPE, TYPE_EXPR, TYPE_CLS(TY), sizeof(TY)))

// Fills all placeholders in the given type with types found starting at the
// given scope. Returns true if type was resolved successfully, e.g. was able
// to find the class.
const Type* type_resolve(Type* type, struct Scope_* scope, struct ErrorsContainer_* errors);

// Sets the the given type
uint64_t type_id(const Type* ty);

// Calculates the size of the given type.
size_t type_calcsize(Type* type);

const char* type_tostr(const Type* ty);
bool type_isequal(const Type* a, const Type* b);
bool type_isassignable(const Type* ty);
bool type_isassignable_to(const Type* from, const Type* to);
bool type_iscoercible(const Type* from, const Type* to);
bool type_isconst(const Type* ty);
bool type_isval(const Type* ty);
bool type_isaref(const Type* ty);
bool type_isavar(const Type* ty);
Type* type_findspecialization(const Type* base_type, Type** resolved_args);

Type* type_deref(Type* ty);
Type* type_valtype(Type* ty);
void type_class_calcoffsets(Type* ty);
Type* type_class_findmember(const Type* cls_ty, const Token_* name, size_t* offset);
void type_class_addmember(Type* cls_ty, Token_ name, Type* type, struct AstExpr_* opt_expr);

void tupletype_add(Type* ty, Type* new);
void uniontype_add(Type* ty, Type* new);
Type* uniontype_findassignable(const Type* ty, const Type* assign_ty);
bool uniontype_has(const Type* union_ty, const Type* ty);
Type* uniontype_select(const Type* ty, const Type* assign_ty);
Type* generictype_findimpl(Type* generic_ty, struct Scope_* scope);
Type* generictype_findimpl_i(Type* generic_ty, uint64_t type_id);

#define type_is(PTYPE, CLS) type_is_(PTYPE, TYPE_CLS(CLS))
inline bool type_is_(const Type* type, int cls) {
  return type && type->cls == cls;
}

#define type_cast(TYPE, EXPR) ((TYPE*)(EXPR))
#define type_as(TYPE, EXPR) ((TYPE*)assert_type_is((EXPR), TYPE_CLS(TYPE)))
#define assert_type_is(TY, TY_CLS) assert_type_is_((Type*)(TY), TY_CLS)
Type* assert_type_is_(Type* ty, int val);

void print_type(const Type* ty);

bool type_isaprimitive(const Type* ty);
bool type_isunary(const Type* ty);
bool type_isnil(const Type* ty);
bool type_isabool(const Type* ty);
bool type_isanumber(const Type* ty);
bool type_isainteger(const Type* ty);
bool type_issigned(const Type* ty);
bool type_isunsigned(const Type* ty);
bool type_isareal(const Type* ty);
bool type_isastring(const Type* ty);
#endif

#undef COMMA
#undef SEMICOLON
#endif  // TYPE__H