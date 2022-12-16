#ifndef VALUE__H
#define VALUE__H

#include "common.h"
#include "type.h"

#define AS_BOOL(value)          ((value).as.b)
#define AS_INT(value)           ((value).as.i)
#define AS_INT8(value)          ((value).as.i8)
#define AS_INT16(value)         ((value).as.i16)
#define AS_INT32(value)         ((value).as.i32)
#define AS_INT64(value)         ((value).as.i64)
#define AS_UINT(value)          ((value).as.u)
#define AS_UINT8(value)         ((value).as.u8)
#define AS_UINT16(value)        ((value).as.u16)
#define AS_UINT32(value)        ((value).as.u32)
#define AS_UINT64(value)        ((value).as.u64)
#define AS_OBJ(value)           ((value).as.obj)

#define AS_FLOAT(value)        ((value).as.f)
#define AS_DOUBLE(value)       ((value).as.d)

#define IS_BOOL(value)         ((value).type.ty == VAL_BOOL)
#define IS_NIL(value)          ((value).type.ty == VAL_NIL)
#define ISA_NUMBER(value)      ((value).type.ty >= VAL_INT && (value).type <= VAL_DOUBLE)
#define ISA_INTEGER(value)     ((value).type.ty >= VAL_INT && (value).type <= VAL_UINT64)
#define ISA_INT(value)         ((value).type.ty >= VAL_INT && (value).type <= VAL_INT64)
#define ISA_UINT(value)        ((value).type.ty >= VAL_UINT && (value).type <= VAL_UINT64)

#define IS_INT(value)          ((value).type.ty == VAL_INT)
#define IS_INTX(value, width)  ((value).type.ty == VAL_INT##width)
#define IS_UINT(value)         ((value).type.ty == VAL_UINT)
#define IS_UINTX(value, width) ((value).type.ty == VAL_UINT##width)
#define IS_FLOAT(value)        ((value).type.ty == VAL_FLOAT)
#define IS_DOUBLE(value)       ((value).type.ty == VAL_DOUBLE)
#define IS_OBJ(value)          ((value).type.ty == VAL_OBJ)
#define IS_VAL(value)          ((value).type.kind == KIND_VAL)
#define IS_PTR(value)          ((value).type.kind == KIND_PTR)
#define IS_REF(value)          ((value).type.kind == KIND_REF)

#define NIL_VAL                    ((Value_){{.i = 0}, VAL_NIL, KIND_VAL})
#define BOOL_VAL(value)            ((Value_){{.b = value}, VAL_BOOL, KIND_VAL})
#define TRUE_VAL BOOL_VAL(true)
#define FALSE_VAL BOOL_VAL(false)
#define INT_VAL(value)          ((Value_){{.i = value},   VAL_INT,    KIND_VAL})
#define INT8_VAL(value)         ((Value_){{.i8 = value},  VAL_INT8,   KIND_VAL})
#define INT16_VAL(value)        ((Value_){{.i16 = value}, VAL_INT16,  KIND_VAL})
#define INT32_VAL(value)        ((Value_){{.i32 = value}, VAL_INT32,  KIND_VAL})
#define INT64_VAL(value)        ((Value_){{.i64 = value}, VAL_INT64,  KIND_VAL})
#define UINT_VAL(value)         ((Value_){{.u = value},   VAL_UINT,   KIND_VAL})
#define UINT8_VAL(value)        ((Value_){{.u8 = value},  VAL_UINT8,  KIND_VAL})
#define UNT16_VAL(value)        ((Value_){{.u16 = value}, VAL_UINT16, KIND_VAL})
#define UNT32_VAL(value)        ((Value_){{.u32 = value}, VAL_UINT32, KIND_VAL})
#define UNT64_VAL(value)        ((Value_){{.u64 = value}, VAL_UINT64, KIND_VAL})
#define FLOAT_VAL(value)        ((Value_){{.f = value},   VAL_FLOAT,  KIND_VAL})
#define DOUBLE_VAL(value)       ((Value_){{.d = value},   VAL_DOUBLE, KIND_VAL})
#define OBJ_VAL(object)         ((Value_){{.obj = (struct Obj_*)object, VAL_OBJ, KIND_VAL}})

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

    uintptr_t ref;
    uintptr_t ptr;

    struct Obj_* obj;
  } as;

  struct Type_ type;
  uint16_t size;
} Value_;

typedef Value_* Value;

typedef struct ValueArray_ {
  int capacity;
  int count;
  Value_* values;
} ValueArray_, *ValueArray;

void valuearray_init(ValueArray value_array);
void valuearray_write(ValueArray value_array, Value_ value);
void valuearray_free(ValueArray value_array);

void value_print(Value_ value);

const char* valuetype_str(Type_ type);
const char* value_typestr(Value_* value);

bool value_iscoercible(Value_ from, Value_ to);

#endif  // VALUE__H