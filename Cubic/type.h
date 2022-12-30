#ifndef TYPE__H
#define TYPE__H

#define UNKNOWN_TY       ((Type_){VAL_UNKNOWN, KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define NIL_TY           ((Type_){VAL_NIL,     KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define BOOL_TY          ((Type_){VAL_BOOL,    KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define INT_TY           ((Type_){VAL_INT,     KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define INT8_TY          ((Type_){VAL_INT8,    KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define INT16_TY         ((Type_){VAL_INT16,   KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define INT32_TY         ((Type_){VAL_INT32,   KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define INT64_TY         ((Type_){VAL_INT64,   KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define UINT_TY          ((Type_){VAL_UINT,    KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define UINT8_TY         ((Type_){VAL_UINT8,   KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define UNT16_TY         ((Type_){VAL_UINT16,  KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define UNT32_TY         ((Type_){VAL_UINT32,  KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define UNT64_TY         ((Type_){VAL_UINT64,  KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define FLOAT_TY         ((Type_){VAL_FLOAT,   KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define DOUBLE_TY        ((Type_){VAL_DOUBLE,  KIND_UNKNOWN, OBJ_TYPE_UNKNOWN})
#define OBJ_TY(OBJ_TYPE) ((Type_){VAL_OBJ,     KIND_UNKNOWN, OBJ_TYPE})
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
  VAL_UNKNOWN = -1,
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
  VAL_OBJ,

  __VALUE_TYPE_COUNT__,
} ValueType;

typedef enum {
  KIND_UNKNOWN,

  // A named variable that is stored on stack.
  KIND_VAL,

  // A named variable that is stored on stack or heap, no ownership.
  KIND_PTR,

  // A named variable is stored on heap, reference counted.
  KIND_REF,

  // An intermediate result and used once (rvalue).
  KIND_TMP,

  // Type is a static constant and can be cached for multiple uses.
  KIND_STATIC,
} ValueKind;

typedef struct Type_ {
  enum ValueType ty;
  enum ValueKind kind;
  enum ObjType obj;
} Type_;

bool type_iscoercible(Type_ from, Type_ to);
uint32_t type_toint(Type_ type);
Type_ type_fromint(uint32_t n);

// Returns true if both types are equal on all fields.
inline bool type_equal(Type_ from, Type_ to) {
  return from.ty == to.ty && from.kind == to.kind && from.obj == to.obj;
}

// Returns true if value and object types are the same.
inline bool type_equiv(Type_ from, Type_ to) {
  return from.ty == to.ty && from.obj == to.obj &&
    ((from.kind && to.kind) ||
      (from.kind == KIND_VAL && to.kind == KIND_TMP));
}

inline static bool type_isobj(Type_ type, enum ObjType obj_type) {
  return type.ty == VAL_OBJ && type.obj == obj_type;
}

inline static bool type_isanumber(Type_ type) {
  return type.ty >= VAL_INT && type.ty <= VAL_DOUBLE;
}

inline static bool type_isainteger(Type_ type) {
  return type.ty >= VAL_INT && type.ty <= VAL_UINT64;
}

inline static bool type_isaint(Type_ type) {
  return type.ty >= VAL_INT && type.ty <= VAL_INT64;
}

inline static bool type_isauint(Type_ type) {
  return type.ty >= VAL_UINT && type.ty <= VAL_UINT64;
}

inline static bool type_isareal(Type_ type) {
  return type.ty == VAL_FLOAT || type.ty == VAL_DOUBLE;
}

#endif  // TYPE__H