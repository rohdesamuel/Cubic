#ifndef TYPE__H
#define TYPE__H

#include "tokens.h"

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

typedef struct Type_ {
  // The type that is the final result of resolving the symbol.
  // E.g. return value of a function, the type that a pointer is addressing
  // to, the field of a given struct.
  enum TypeCls {
    // Unknown because type has not been parsed.
    TYPE_CLS(UnknownType_),

    // Primitive types.
    TYPE_CLS(NilType_),
    TYPE_CLS(BoolType_),
    TYPE_CLS(IntType_),
    TYPE_CLS(Int8Type_),
    TYPE_CLS(Int16Type_),
    TYPE_CLS(Int32Type_),
    TYPE_CLS(Int64Type_),
    TYPE_CLS(UintType_),
    TYPE_CLS(Uint8Type_),
    TYPE_CLS(Uint16Type_),
    TYPE_CLS(Uint32Type_),
    TYPE_CLS(Uint64Type_),
    TYPE_CLS(FloatType_),
    TYPE_CLS(DoubleType_),
    TYPE_CLS(StringType_),

    // Unary types.    
    TYPE_CLS(ConstType_),
    TYPE_CLS(InType_),
    TYPE_CLS(OutType_),
    TYPE_CLS(VarType_),
    TYPE_CLS(RefType_),
    TYPE_CLS(PlaceholderType_),

    // Compound types.
    TYPE_CLS(ClassType_),
    TYPE_CLS(ArrayType_),

    // Functional types.
    TYPE_CLS(FunctionType_),

    __TYPE_COUNT__,
  } cls;

  // The size of this type in memory, not following pointers.
  size_t size;

  Token_ opt_name;
} Type_;

#define DEF_PRIMITIVE_TY(NAME) typedef struct NAME##Type_ { Type_ self; } NAME##Type_;

DEF_PRIMITIVE_TY(Unknown);
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

typedef struct ArrayType_ {
  Type_ self;
  Type_* el_type;
  size_t count;
} ArrayType_;

typedef struct VarType_ {
  Type_ self;
  Type_* ty;
} VarType_;

typedef struct InType_ {
  Type_ self;
  Type_* ty;
} InType_;

typedef struct OutType_ {
  Type_ self;
  Type_* ty;
} OutType_;

typedef struct RefType_ {
  Type_ self;
  Type_* ty;
} RefType_;

typedef struct ConstType_ {
  Type_ self;
  Type_* ty;
} ConstType_;

typedef struct ClassType_ {
  Type_ self;

  ListOf_(Type_*) members;
  ListOf_(Token_) field_names;
} ClassType_;

typedef struct FunctionType_ {
  Type_ self;

  Type_* ret_ty;
  ListOf_(Type_*) parameters;
} FunctionType_;

typedef struct PlaceholderType_ {
  Type_ self;
  Type_* ty;
} PlaceholderType_;

extern const NilType_ Nil_Ty;
extern const BoolType_ Bool_Ty;
extern const IntType_ Int_Ty;
extern const UintType_ Uint_Ty;
extern const FloatType_ Float_Ty;
extern const DoubleType_ Double_Ty;
extern const StringType_ String_Ty;

Type_* make_const_ty(Type_* sub_type, MemoryAllocator_* allocator);
Type_* make_var_ty(Type_* sub_type, MemoryAllocator_* allocator);
Type_* make_in_ty(Type_* sub_type, MemoryAllocator_* allocator);
Type_* make_out_ty(Type_* sub_type, MemoryAllocator_* allocator);
Type_* make_array_ty(Type_* el_type, size_t count, MemoryAllocator_* allocator);
Type_* make_class_ty(Token_ name, MemoryAllocator_* allocator);
Type_* make_placeholder_ty(Token_ name, MemoryAllocator_* allocator);
Type_* make_function_ty(Token_ name, MemoryAllocator_* allocator);
Type_* type_alloc(MemoryAllocator_* allocator, size_t type_size);

#define type_alloc_ty(ALLOCATOR, TY) ((TY*)type_alloc(ALLOCATOR, sizeof(TY)))

// Fills all placeholders in the given type with types found starting at the
// given scope.
bool type_fill(Type_* type, struct Scope_* scope);

// Calculates the size of the given type.
size_t type_calcsize(Type_* type);

typedef struct RuntimeType_ {
  int ty;
} RuntimeType_;

bool type_iscoercible(RuntimeType_ from, RuntimeType_ to);
uint32_t type_toint(RuntimeType_ info);
RuntimeType_ type_fromint(uint32_t n);

bool type_equal(const Type_* a, const Type_* b);
bool type_assignable(const Type_* from, const Type_* to);
bool type_coercible(const Type_* from, const Type_* to);
bool type_isconst(const Type_* ty);
bool type_isval(const Type_* ty);
bool type_isaref(const Type_* ty);

#define type_is(PTYPE, CLS) type_is_(PTYPE, TYPE_CLS(CLS))
inline bool type_is_(const Type_* type, int cls) { return type->cls == cls; }

inline static bool type_isunknown(const Type_* ty) {
  return ty->cls == TYPE_CLS(UnknownType_);
}

inline static bool type_isnil(const Type_* ty) {
  return ty->cls >= TYPE_CLS(NilType_) && ty->cls <= TYPE_CLS(StringType_);
}

inline static bool type_isaprimitive(const Type_* ty) {
  return ty->cls >= TYPE_CLS(NilType_) && ty->cls <= TYPE_CLS(StringType_);
}

inline static bool type_isabool(const Type_* ty) {
  return type_is(ty, BoolType_);
}

inline static bool type_isanumber(const Type_* ty) {
  return ty->cls >= TYPE_CLS(IntType_) && ty->cls <= TYPE_CLS(DoubleType_);
}

inline static bool type_isainteger(const Type_* ty) {
  return ty->cls >= TYPE_CLS(IntType_) && ty->cls <= TYPE_CLS(Uint64Type_);
}

inline static bool type_issigned(const Type_* ty) {
  return ty->cls >= TYPE_CLS(IntType_) && ty->cls <= TYPE_CLS(Int64Type_);
}

inline static bool type_isunsigned(const Type_* ty) {
  return ty->cls >= TYPE_CLS(UintType_) && ty->cls <= TYPE_CLS(Uint64Type_);
}

inline static bool type_isareal(const Type_* ty) {
  return ty->cls == TYPE_CLS(FloatType_) || ty->cls == TYPE_CLS(DoubleType_);
}

inline static bool type_isastring(const Type_* ty) {
  return type_is(ty, StringType_);
}

#define type_as(TYPE, EXPR) ((TYPE*)assert_type_is((EXPR), TYPE_CLS(TYPE)))
#define assert_type_is(TY, VAL_TY) assert_type_is_((Type_*)(TY), VAL_TY)
Type_* assert_type_is_(Type_* ty, int val);

#endif  // TYPE__H