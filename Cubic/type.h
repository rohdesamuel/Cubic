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
    TYPE_CLS(Type_),

    // Primitive types.
    __TYPE_PRIMITIVE_START__,
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
    __TYPE_PRIMITIVE_END__,

    // Unary types.    
    __TYPE_UNARY_START__,    
    TYPE_CLS(ConstType_),
    TYPE_CLS(InType_),
    TYPE_CLS(OutType_),
    TYPE_CLS(VarType_),
    TYPE_CLS(RefType_),
    __TYPE_UNARY_END__,

    // Composite types.
    __TYPE_COMPOSITE_START__,
    TYPE_CLS(ClassType_),
    TYPE_CLS(ArrayType_),
    __TYPE_COMPOSITE_END__,

    // Functional types.
    TYPE_CLS(FunctionType_),

    __TYPE_COUNT__,
  } cls;


  // The size of this type in memory, not following pointers.
  size_t size;

  struct Type_* impl;

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

typedef struct UnaryType_ {
  Type_ self;
  Type_* ty;
} UnaryType_;

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

  struct FunctionType_* constructor;
  ListOf_(ClassTypeField_*) members;
} ClassType_;

typedef struct FunctionType_ {
  Type_ self;
  struct Symbol_* sym;

  Type_* ret_ty;
  ListOf_(Type_*) params;
} FunctionType_;

extern const UnknownType_ Unknown_Ty;
extern const NilType_ Nil_Ty;
extern const BoolType_ Bool_Ty;
extern const IntType_ Int_Ty;
extern const Int32Type_ Int32_Ty;
extern const Int64Type_ Int64_Ty;
extern const UintType_ Uint_Ty;
extern const FloatType_ Float_Ty;
extern const DoubleType_ Double_Ty;
extern const StringType_ String_Ty;

Type_* make_unknown_ty(MemoryAllocator_* allocator);
Type_* make_const_ty(Type_* sub_type, MemoryAllocator_* allocator);
Type_* make_var_ty(Type_* sub_type, MemoryAllocator_* allocator);
Type_* make_in_ty(Type_* sub_type, MemoryAllocator_* allocator);
Type_* make_out_ty(Type_* sub_type, MemoryAllocator_* allocator);
Type_* make_array_ty(Type_* el_type, size_t count, MemoryAllocator_* allocator);
Type_* make_class_ty(Token_ name, MemoryAllocator_* allocator);
Type_* make_placeholder_ty(Token_ name, MemoryAllocator_* allocator);
Type_* make_function_ty(Token_ name, MemoryAllocator_* allocator);
Type_* make_symbol_ty(Type_* sym_type, struct Symbol_* sym, MemoryAllocator_* allocator);
Type_* type_alloc(MemoryAllocator_* allocator, int type_cls, size_t type_size);

#define type_alloc_ty(ALLOCATOR, TY) ((TY*)type_alloc(ALLOCATOR, TYPE_CLS(TY), sizeof(TY)))

// Fills all placeholders in the given type with types found starting at the
// given scope.
bool type_fill(Type_* type, struct Scope_* scope);

// Sets the the given type
void type_set(Type_* type, Type_* new_type);
void type_wrap(Type_* type, Type_* wrapper);
void type_replace(Type_* type, Type_* new_type);

// Calculates the size of the given type.
size_t type_calcsize(Type_* type);

typedef struct RuntimeType_ {
  int ty;
} RuntimeType_;

bool type_iscoercible(RuntimeType_ from, RuntimeType_ to);
RuntimeType_ type_toruntime(const Type_* ty);
uint32_t type_toint(RuntimeType_ info);
RuntimeType_ type_fromint(uint32_t n);

const char* type_tostr(const Type_* ty);
bool type_equal(const Type_* a, const Type_* b);
bool type_assignable(const Type_* from, const Type_* to);
bool type_coercible(const Type_* from, const Type_* to);
bool type_isconst(const Type_* ty);
bool type_isval(const Type_* ty);
bool type_isaref(const Type_* ty);

Type_* type_deref(Type_* ty);
Type_* type_valtype(Type_* ty);
void type_class_calcoffsets(Type_* ty);
Type_* type_class_findmember(const Type_* cls_ty, const Token_* name, size_t* offset);

#define type_is(PTYPE, CLS) type_is_(PTYPE, TYPE_CLS(CLS))
inline bool type_is_(const Type_* type, int cls) {
  if (type->cls == cls) {
    return true;
  }

  if (type->cls == TYPE_CLS(Type_)) {
    type = ((UnaryType_*)type)->ty;
  }
  
  return false;
}

#define type_cast(TYPE, EXPR) ((TYPE*)(EXPR))
#define type_as(TYPE, EXPR) ((TYPE*)assert_type_is((EXPR), TYPE_CLS(TYPE)))
#define assert_type_is(TY, VAL_TY) assert_type_is_((Type_*)(TY), VAL_TY)
Type_* assert_type_is_(Type_* ty, int val);

void print_type(const Type_* ty);

bool type_isunknown(const Type_* ty);
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

#endif  // TYPE__H