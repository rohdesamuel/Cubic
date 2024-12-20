#ifndef TYPE__H
#define TYPE__H

#include "tokens.h"
#include "value.h"

#if 0
typedef enum {
  VAL_UNKNOWN,

  // Start primitives
  VAL_NIL,
  VAL_BOOL,
  VAL_INT,
  VAL_INT8,
  VAL_INT16,
  VAL_INT32,
  VAL_INT64,
  VAL_UINT,
  VAL_UINT8,
  VAL_UINT16,
  VAL_UINT32,
  VAL_UINT64,
  VAL_FLOAT,
  VAL_DOUBLE,
  VAL_STRING,
  // End primitives

  VAL_CLASS,
  VAL_OBJ,
  VAL_ARRAY,
  VAL_VAR,
  VAL_REF,
  VAL_IN,
  VAL_OUT,

  VAL_PLACEHOLDER,

  __VALUE_TYPE_COUNT__,
} ValueType;
#endif

typedef enum {
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

  // A named variable that is stored on stack or heap, no ownership.
  // ex. ptr a : int
  KIND_PTR,
} ValueKind;

typedef enum {
  CONST_KIND_UNKNOWN,

  // Mutable. No const restrictions.
  CONST_KIND_NONE,

  // Immutable. Full const restrictions.
  CONST_KIND_WHOLE,
} ConstKind;

typedef enum {
  LIFETIME_UNKNOWN,

  // Automatic lifetime (will be destructed without user intervention).
  LIFETIME_AUTOMATIC,

  // An intermediate result and used once (rvalue).
  LIFETIME_TMP,

  // Type is a static constant and can be cached for multiple uses.
  LIFETIME_STATIC,
} ValueLifetime;

#define TYPE_CLS(name) TYPE_##name
#define TYPE_NAME(name) name __##name##impl__

// TODO: implement types as UnionTypes.
#define TYPE_LIST(TYPE_OP, DELIM) \
  TYPE_OP(NilType_) DELIM \
  TYPE_OP(BoolType_) DELIM \
  TYPE_OP(IntType_) DELIM \
  TYPE_OP(Int8Type_) DELIM \
  TYPE_OP(Int16Type_) DELIM \
  TYPE_OP(Int32Type_) DELIM \
  TYPE_OP(Int64Type_) DELIM \
  TYPE_OP(UintType_) DELIM \
  TYPE_OP(Uint8Type_) DELIM \
  TYPE_OP(Uint16Type_) DELIM \
  TYPE_OP(Uint32Type_) DELIM \
  TYPE_OP(Uint64Type_) DELIM \
  TYPE_OP(FloatType_) DELIM \
  TYPE_OP(DoubleType_) DELIM \
  TYPE_OP(StringType_) DELIM \
  TYPE_OP(ConstType_) DELIM \
  TYPE_OP(InType_) DELIM \
  TYPE_OP(OutType_) DELIM \
  TYPE_OP(VarType_) DELIM \
  TYPE_OP(RefType_) DELIM \
  TYPE_OP(FieldType_) DELIM \
  TYPE_OP(ClassType_) DELIM \
  TYPE_OP(ArrayType_) DELIM \
  TYPE_OP(UnionType_) DELIM \
  TYPE_OP(ConstraintType_) DELIM \
  TYPE_OP(TupleType_) DELIM \
  TYPE_OP(GenericParamType_) DELIM \
  TYPE_OP(FunctionType_)

#define GENERATE_TYPE_ENUMS(TYPE) TYPE_CLS(TYPE)
#define GENERATE_TYPE_UNION(TYPE) TYPE_NAME(TYPE)

typedef struct Specialization_ {
  struct Type_* type;
  struct Type_** args;
  size_t count;
} Specialization_;

typedef struct Type_ {
  // The type that is the final result of resolving the symbol.
  // E.g. return value of a function, the type that a pointer is addressing
  // to, the field of a given struct.
  enum TypeCls {
    TYPE_LIST(GENERATE_TYPE_ENUMS, CB_COMMA),

    // Define the PlaceholderType_ outside of the list because this would
    // result in a loop when defining the PlaceholderType_.
    TYPE_CLS(PlaceholderType_),
    __TYPE_COUNT__,

    __TYPE_PRIMITIVE_START__ = TYPE_CLS(NilType_),
    __TYPE_PRIMITIVE_END__ = TYPE_CLS(StringType_),

    __TYPE_UNARY_START__ = TYPE_CLS(ConstType_),
    __TYPE_UNARY_END__ = TYPE_CLS(FieldType_),

    __TYPE_COMPOSITE_START__ = TYPE_CLS(ClassType_),
    __TYPE_COMPOSITE_END__ = TYPE_CLS(GenericParamType_),
  } cls;

  // The TypeExpr_ that created this type.
  const struct TypeExpr_* tmpl;

  // The type arguments that created this type. Should be the same size as
  // the type parameters in the parent TypeExpr_ `tmpl`.
  struct Type_** args;
  int args_count;

  // List of type specializations.
  ListOf_(Type_*) specializations;

  // The size of this type in memory, not following pointers.
  size_t size;

  // The scope this type belongs to.
  struct Scope_* scope;
  Token_ opt_name;
  uint64_t id;
} Type_;

#define DEF_PRIMITIVE_TY(NAME) \
typedef Type_ NAME##Type_; \
extern const Type_* NAME##_Ty;

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

typedef struct PrimitiveType_ {
  Type_ self;
} PrimitiveType_;

typedef struct UnaryType_ {
  Type_ self;
  Type_* ty;
} UnaryType_;

typedef struct MultiType_ {
  Type_ self;
  ListOf_(Type_*) types;
} MultiType_;

typedef struct ArrayType_ {
  Type_ self;
  Type_* el_type;
  size_t count;
} ArrayType_;

typedef struct VarType_ {
  UnaryType_ unary;
} VarType_;

typedef struct InType_ {
  UnaryType_ unary;
} InType_;

typedef struct OutType_ {
  UnaryType_ unary;
} OutType_;

typedef struct RefType_ {
  UnaryType_ unary;
} RefType_;

typedef struct ConstType_ {
  UnaryType_ unary;
} ConstType_;

typedef struct TypeArgument_ {
  union {
    Type_* type;
    Value_ val;
  };

  bool is_type;
  bool is_val;
} TypeArgument_;

typedef struct ConstraintType_ {
  MultiType_ self;
  int index;
} ConstraintType_;

typedef struct GenericImplType_ {
  UnaryType_ unary;

  struct GenericType_* generic_type;
  Type_** args;
  size_t args_count;
  struct Scope_* scope;
} GenericImplType_;

typedef struct GenericType_ {
  UnaryType_ unary;
  Type_* prototype;
  ListOf_(ConstraintType_*) params;
  struct Scope_* scope;
} GenericType_;

typedef struct GenericParamType_ {
  UnaryType_ self;
  Token_ name;
} GenericParamType_;

typedef struct UnionType_ {
  MultiType_ self;
  Type_* selected_type;
} UnionType_;

typedef struct TupleType_ {
  MultiType_ self;
} TupleType_;

typedef struct FieldType_ {
  UnaryType_ self;

  Token_ name;
} FieldType_;

typedef struct ClassTypeField_ {
  Type_* type;

  // Name of the field.
  Token_ name;

  // Byte frame_offset into the outer-most class.
  size_t offset;

  struct AstExpr_* opt_expr;
} ClassTypeField_;

typedef struct ClassType_ {
  Type_ self;
  
  const struct FunctionType_* constructor;
  ListOf_(ClassTypeField_) members;
  struct Scope_* scope;
} ClassType_;

typedef struct FunctionType_ {
  Type_ self;
  Type_* ret_ty;
  ListOf_(Type_*) params;
} FunctionType_;

typedef struct PlaceholderType_ {
  union {
    Type_ self;
    TYPE_LIST(GENERATE_TYPE_UNION, CB_SEMICOLON);
  };
} PlaceholderType_;

void type_init();

Type_* make_placeholder_ty(const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type_* make_const_ty(Type_* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type_* make_var_ty(Type_* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type_* make_ref_ty(Type_* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type_* make_in_ty(Type_* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type_* make_out_ty(Type_* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type_* make_array_ty(Type_* el_type, size_t count, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);

// Class types are singletons. For each class type, there is only one Type_*
// instance.
Type_* make_class_ty(Token_ name, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type_* make_function_ty(Token_ name, ListOf_(Type_*)* params, Type_* ret_ty, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type_* make_tuple_ty(const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator, int n, ...);
Type_* make_union_ty(const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator, int n, ...);
Type_* make_constraint_ty(const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator, Token_ name, int n, ...);
Type_* make_field_ty(Token_ field_name, Type_* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);
Type_* make_genericparam_ty(Token_ name, Type_* constraint, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator);

Type_* type_alloc(MemoryAllocator_* allocator, struct Scope_* scope, const struct TypeExpr_* tmpl, int type_cls, size_t type_size);

#define type_alloc_ty(ALLOCATOR, SCOPE, TYPE_EXPR, TY) ((TY*)type_alloc(ALLOCATOR, SCOPE, TYPE_EXPR, TYPE_CLS(TY), sizeof(TY)))

// Fills all placeholders in the given type with types found starting at the
// given scope. Returns true if type was resolved successfully, e.g. was able
// to find the class.
const Type_* type_resolve(Type_* type, struct Scope_* scope, struct ErrorsContainer_* errors);

// Sets the the given type
uint64_t type_id(const Type_* ty);

// Calculates the size of the given type.
size_t type_calcsize(Type_* type);

const char* type_tostr(const Type_* ty);
bool type_isequal(const Type_* a, const Type_* b);
bool type_isassignable(const Type_* ty);
bool type_isassignable_to(const Type_* from, const Type_* to);
bool type_iscoercible(const Type_* from, const Type_* to);
bool type_isconst(const Type_* ty);
bool type_isval(const Type_* ty);
bool type_isaref(const Type_* ty);
bool type_isavar(const Type_* ty);
Type_* type_findspecialization(const Type_* base_type, Type_** resolved_args);

Type_* type_deref(Type_* ty);
Type_* type_valtype(Type_* ty);
void type_class_calcoffsets(Type_* ty);
Type_* type_class_findmember(const Type_* cls_ty, const Token_* name, size_t* offset);
void type_class_addmember(Type_* cls_ty, Token_ name, Type_* type, struct AstExpr_* opt_expr);

void tupletype_add(Type_* ty, Type_* new);
void uniontype_add(Type_* ty, Type_* new);
Type_* uniontype_findassignable(const Type_* ty, const Type_* assign_ty);
bool uniontype_has(const Type_* union_ty, const Type_* ty);
Type_* uniontype_select(const Type_* ty, const Type_* assign_ty);
Type_* generictype_findimpl(Type_* generic_ty, struct Scope_* scope);
Type_* generictype_findimpl_i(Type_* generic_ty, uint64_t type_id);

#define type_is(PTYPE, CLS) type_is_(PTYPE, TYPE_CLS(CLS))
inline bool type_is_(const Type_* type, int cls) {
  return type && type->cls == cls;
}

#define type_cast(TYPE, EXPR) ((TYPE*)(EXPR))
#define type_as(TYPE, EXPR) ((TYPE*)assert_type_is((EXPR), TYPE_CLS(TYPE)))
#define assert_type_is(TY, TY_CLS) assert_type_is_((Type_*)(TY), TY_CLS)
Type_* assert_type_is_(Type_* ty, int val);

void print_type(const Type_* ty);

bool type_isaprimitive(const Type_* ty);
bool type_isunary(const Type_* ty);
bool type_isnil(const Type_* ty);
bool type_isabool(const Type_* ty);
bool type_isanumber(const Type_* ty);
bool type_isainteger(const Type_* ty);
bool type_issigned(const Type_* ty);
bool type_isunsigned(const Type_* ty);
bool type_isareal(const Type_* ty);
bool type_isastring(const Type_* ty);

#undef COMMA
#undef SEMICOLON
#endif  // TYPE__H