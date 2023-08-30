#ifndef TYPE__H
#define TYPE__H

#include "tokens.h"

#define UNKNOWN_TY       ((RuntimeType_){VAL_UNKNOWN, KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define NIL_TY           ((RuntimeType_){VAL_NIL,     KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define BOOL_TY          ((RuntimeType_){VAL_BOOL,    KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define INT_TY           ((RuntimeType_){VAL_INT,     KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define INT8_TY          ((RuntimeType_){VAL_INT8,    KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define INT16_TY         ((RuntimeType_){VAL_INT16,   KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define INT32_TY         ((RuntimeType_){VAL_INT32,   KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define INT64_TY         ((RuntimeType_){VAL_INT64,   KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define UINT_TY          ((RuntimeType_){VAL_UINT,    KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define UINT8_TY         ((RuntimeType_){VAL_UINT8,   KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define UNT16_TY         ((RuntimeType_){VAL_UINT16,  KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define UNT32_TY         ((RuntimeType_){VAL_UINT32,  KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define UNT64_TY         ((RuntimeType_){VAL_UINT64,  KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define FLOAT_TY         ((RuntimeType_){VAL_FLOAT,   KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define DOUBLE_TY        ((RuntimeType_){VAL_DOUBLE,  KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define OBJ_TY(OBJ_TYPE) ((RuntimeType_){VAL_OBJ,     KIND_UNKNOWN, OBJ_TYPE})
#define STRING_TY        OBJ_TY(OBJ_TYPE_STRING)
#define FUNCTION_TY      OBJ_TY(OBJ_TYPE_FUNCTION)

#define ISA_TY_NUMBER(TYPE)      (type_isanumber(TYPE))
#define ISA_TY_INTEGER(TYPE)     (type_isainteger(TYPE))
#define ISA_TY_INT(TYPE)         (type_isaint(TYPE))
#define ISA_TY_UINT(TYPE)        (type_isauint(TYPE))
#define ISA_TY_REAL(TYPE)        (type_isareal(TYPE))
#define IS_TY_UNKNOWN(TYPE) ((TYPE).ty == VAL_UNKNOWN)
#define IS_TY_NIL(TYPE) ((TYPE).ty == VAL_NIL)
#define IS_TY_BOOL(TYPE) ((TYPE).ty == VAL_BOOL)
#define IS_TY_INT(TYPE) ((TYPE).ty == VAL_INT)
#define IS_TY_INT8(TYPE) ((TYPE).ty == VAL_INT8)
#define IS_TY_INT16(TYPE) ((TYPE).ty == VAL_INT16)
#define IS_TY_INT32(TYPE) ((TYPE).ty == VAL_INT32)
#define IS_TY_INT64(TYPE) ((TYPE).ty == VAL_INT64)
#define IS_TY_UINT(TYPE) ((TYPE).ty == VAL_UINT)
#define IS_TY_UINT8(TYPE) ((TYPE).ty == VAL_UINT8)
#define IS_TY_UNT16(TYPE) ((TYPE).ty == VAL_UINT32)
#define IS_TY_UNT32(TYPE) ((TYPE).ty == VAL_UINT32)
#define IS_TY_UNT64(TYPE) ((TYPE).ty == VAL_UINT64)
#define IS_TY_FLOAT(TYPE) ((TYPE).ty == VAL_FLOAT)
#define IS_TY_DOUBLE(TYPE) ((TYPE).ty == VAL_DOUBLE)
#define IS_TY_OBJ(TYPE, OBJ_TYPE) (type_isobj(TYPE, OBJ_TYPE))
#define IS_TY_STRING(TYPE) IS_TY_OBJ(TYPE, OBJ_TYPE_STRING)
#define IS_TY_FUNCTION(TYPE) IS_TY_OBJ(TYPE, OBJ_TYPE_FUNCTION)

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
  // End primitives

  VAL_CLASS,
  VAL_OBJ,
  VAL_ARRAY,
  VAL_VAR,
  VAL_IN,
  VAL_OUT,

  __VALUE_TYPE_COUNT__,
} ValueType;

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
  REF_KIND_UNKNOWN,

  // A named variable is stored on stack or heap, not reference counted.
  REF_KIND_WEAK,

  // A named variable is stored on heap, reference counted.
  REF_KIND_STRONG,
} ValueRefKind;

typedef enum {
  CONST_KIND_UNKNOWN,

  // Mutable. No const restrictions.
  CONST_KIND_NONE,

  // Immutable. Full const restrictions.
  CONST_KIND_WHOLE,
} ValueConstKind;

typedef enum {
  LIFETIME_UNKNOWN,

  // Automatic lifetime (will be destructed without user intervention).
  LIFETIME_AUTOMATIC,

  // An intermediate result and used once (rvalue).
  LIFETIME_TMP,

  // Type is a static constant and can be cached for multiple uses.
  LIFETIME_STATIC,
} ValueLifetime;

typedef struct Type_ {
  // The type that is the final result of resolving the symbol.
  // E.g. return value of a function, the type that a pointer is addressing
  // to, the field of a given struct.
  enum ValueType val;

  // How to interpret the type, is it a value, a reference?
  enum ValueKind kind;

  // If the type is a composite, e.g. maps, lists, then this will be the
  // sub-types.
  ListOf_(struct Type_*) component_types;

  // The name of the type.
  Token_ opt_name;
} Type_;

extern const Type_ Nil_Ty;
extern const Type_ Bool_Ty;
extern const Type_ Int_Ty;
extern const Type_ Uint_Ty;
extern const Type_ Float_Ty;
extern const Type_ Double_Ty;

Type_* make_var_ty(const Type_* sub_type, MemoryAllocator_* allocator);
Type_* make_in_ty(const Type_* sub_type, MemoryAllocator_* allocator);
Type_* make_out_ty(const Type_* sub_type, MemoryAllocator_* allocator);
Type_* make_array_ty(const Type_* el_type, MemoryAllocator_* allocator);

typedef struct RuntimeType_ {
  enum ValueType ty;
  enum ValueKind kind;
  enum ObjType obj;
} RuntimeType_;

bool type_iscoercible(RuntimeType_ from, RuntimeType_ to);
uint32_t type_toint(RuntimeType_ info);
RuntimeType_ type_fromint(uint32_t n);

// Returns true if both types are equal on all fields.
inline bool type_equal(RuntimeType_ from, RuntimeType_ to) {
  return from.ty == to.ty && from.kind == to.kind && from.obj == to.obj;
}

// Returns true if value and object types are the same.
inline bool type_equiv(RuntimeType_ from, RuntimeType_ to) {
  return from.ty == to.ty && from.obj == to.obj;
}

inline static bool valuetype_isaprimitive(ValueType type) {
  return type >= VAL_NIL && type <= VAL_DOUBLE;
}

inline static bool type_isaprimitive(RuntimeType_ info) {
  return info.ty >= VAL_NIL && info.ty <= VAL_DOUBLE;
}

inline static bool type_isobj(RuntimeType_ info, enum ObjType obj_type) {
  return info.ty == VAL_OBJ && info.obj == obj_type;
}

inline static bool type_isanumber(RuntimeType_ info) {
  return info.ty >= VAL_INT && info.ty <= VAL_DOUBLE;
}

inline static bool type_isainteger(RuntimeType_ info) {
  return info.ty >= VAL_INT && info.ty <= VAL_UINT64;
}

inline static bool type_isaint(RuntimeType_ info) {
  return info.ty >= VAL_INT && info.ty <= VAL_INT64;
}

inline static bool type_isauint(RuntimeType_ info) {
  return info.ty >= VAL_UINT && info.ty <= VAL_UINT64;
}

inline static bool type_isareal(RuntimeType_ info) {
  return info.ty == VAL_FLOAT || info.ty == VAL_DOUBLE;
}

#endif  // TYPE__H