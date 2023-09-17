#ifndef VALUE__H
#define VALUE__H

#include "common.h"
#include "type.h"

#define AS_BOOL(value)         ((value).as.b)
#define AS_INT(value)          ((value).as.i)
#define AS_INT8(value)         ((value).as.i8)
#define AS_INT16(value)        ((value).as.i16)
#define AS_INT32(value)        ((value).as.i32)
#define AS_INT64(value)        ((value).as.i64)
#define AS_UINT(value)         ((value).as.u)
#define AS_UINT8(value)        ((value).as.u8)
#define AS_UINT16(value)       ((value).as.u16)
#define AS_UINT32(value)       ((value).as.u32)
#define AS_UINT64(value)       ((value).as.u64)
#define AS_OBJ(value)          ((value).as.obj)

#define AS_FLOAT(value)        ((value).as.f)
#define AS_DOUBLE(value)       ((value).as.d)

#define IS_BOOL(value)         ((value).info.ty == VAL_BOOL)
#define IS_NIL(value)          ((value).info.ty == VAL_NIL)
#define ISA_NUMBER(value)      ((value).info.ty >= VAL_INT && (value).info <= VAL_DOUBLE)
#define ISA_INTEGER(value)     ((value).info.ty >= VAL_INT && (value).info <= VAL_UINT64)
#define ISA_INT(value)         ((value).info.ty >= VAL_INT && (value).info <= VAL_INT64)
#define ISA_UINT(value)        ((value).info.ty >= VAL_UINT && (value).info <= VAL_UINT64)

#define IS_INT(value)          ((value).info.ty == VAL_INT)
#define IS_INTX(value, width)  ((value).info.ty == VAL_INT##width)
#define IS_UINT(value)         ((value).info.ty == VAL_UINT)
#define IS_UINTX(value, width) ((value).info.ty == VAL_UINT##width)
#define IS_FLOAT(value)        ((value).info.ty == VAL_FLOAT)
#define IS_DOUBLE(value)       ((value).info.ty == VAL_DOUBLE)
#define IS_OBJ(value)          ((value).info.ty == VAL_OBJ)
#define IS_VAL(value)          ((value).info.kind == KIND_VAL)
#define IS_VAR(value)          ((value).info.kind == KIND_VAR)
#define IS_PTR(value)          ((value).info.kind == KIND_PTR)
#define IS_REF(value)          ((value).info.kind == KIND_REF)

#define NIL_VAL                ((Value_){{.i = 0}})
#define BOOL_VAL(value)        ((Value_){{.b = value}})
#define TRUE_VAL               BOOL_VAL(true)
#define FALSE_VAL              BOOL_VAL(false)
#define INT_VAL(value)         ((Value_){{.i = value}})
#define INT8_VAL(value)        ((Value_){{.i8 = value}})
#define INT16_VAL(value)       ((Value_){{.i16 = value}})
#define INT32_VAL(value)       ((Value_){{.i32 = value}})
#define INT64_VAL(value)       ((Value_){{.i64 = value}})
#define UINT_VAL(value)        ((Value_){{.u = value}})
#define UINT8_VAL(value)       ((Value_){{.u8 = value}})
#define UNT16_VAL(value)       ((Value_){{.u16 = value}})
#define UNT32_VAL(value)       ((Value_){{.u32 = value}})
#define UNT64_VAL(value)       ((Value_){{.u64 = value}})
#define FLOAT_VAL(value)       ((Value_){{.f = value}})
#define DOUBLE_VAL(value)      ((Value_){{.d = value}})
#define OBJ_VAL(object)        obj_val((Obj_*)object)
#define PTR_VAL(PTR)           ((Value_){{.ptr = ((uintptr_t)(PTR))}})

typedef struct Ref_ {
  struct Value_* pval;
  int* count;
} Ref_;

typedef struct Value_ {
  union {
    bool b;

    int64_t i;
    int8_t i8;
    int16_t i16;
    int32_t i32;
    int64_t i64;

    uint64_t u;
    uint8_t u8;
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;

    float f;
    double d;

    struct Ref_ ref;
    uintptr_t ptr;

    struct Obj_* obj;
  } as;
} Value_;
typedef Value_* Value;

typedef struct TypedValue_ {
  Value_ val;
  RuntimeType_ ty;
} TypedValue_;

typedef struct ValueArray_ {
  int capacity;
  int count;
  Value_* values;
} ValueArray_, *ValueArray;

void valuearray_init(ValueArray value_array);
void valuearray_write(ValueArray value_array, Value_ value);
void valuearray_free(ValueArray value_array);

void value_print(Value_ value, RuntimeType_ info);
void value_set(Value_* l, Value_* r);

#define VALUE_VAL_TYPE(info) ((info & 0x00FF0000) >> 16)
#define VALUE_VAL_KIND(info) ((info & 0x0000FF00) >> 8)
#define VALUE_OBJ_TYPE(info)  (info & 0x000000FF)

#endif  // VALUE__H