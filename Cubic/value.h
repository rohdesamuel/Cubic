#ifndef VALUE__H
#define VALUE__H

#include "common.h"

typedef enum {
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
  VAL_DOUBLE
} ValueType;

#define AS_BOOL(value)         ((value).as.b)
#define AS_INT(value)          ((value).as.i)
#define AS_INTX(value, width)  ((value).as.i##width)
#define AS_UINT(value)         ((value).as.i)
#define AS_UINTX(value, width) ((value).as.i##width)
#define AS_FLOAT(value)        ((value).as.f)
#define AS_DOUBLE(value)       ((value).as.d)

#define IS_BOOL(value)         ((value).type == VAL_BOOL)
#define IS_NIL(value)          ((value).type == VAL_NIL)
#define ISA_NUMBER(value)       ((value).type >= VAL_INT && (value).type <= VAL_DOUBLE)
#define ISA_INT(value)          ((value).type >= VAL_INT && (value).type <= VAL_INT64)
#define ISA_UINT(value)         ((value).type >= VAL_UINT && (value).type <= VAL_UINT64)

#define IS_INT(value)          ((value).type == VAL_INT)
#define IS_INTX(value, width)  ((value).type == VAL_INT##width)
#define IS_UINT(value)          ((value).type == VAL_UINT)
#define IS_UINTX(value, width) ((value).type == VAL_UINT##width)
#define IS_FLOAT(value)        ((value).type == VAL_FLOAT)
#define IS_DOUBLE(value)       ((value).type == VAL_DOUBLE)

#define NIL_VAL                  ((Value_){{.i = 0}, VAL_NIL})
#define BOOL_VAL(value)          ((Value_){{.b = value}, VAL_BOOL, 0})
#define INT_VAL(value)           ((Value_){{.i = value}, VAL_INT, 0})
#define INT8_VAL(value)           ((Value_){{.i8 = value}, VAL_INT8, 0})
#define INT16_VAL(value)           ((Value_){{.i16 = value}, VAL_INT16, 0})
#define INT32_VAL(value)           ((Value_){{.i32 = value}, VAL_INT32, 0})
#define INT64_VAL(value)           ((Value_){{.i64 = value}, VAL_INT64, 0})

#define UINT_VAL(value)          ((Value_){{.u = value}, VAL_UINT, 0})
#define UNT8_VAL(value)           ((Value_){{.u8 = value}, VAL_UINT8, 0})
#define UNT16_VAL(value)           ((Value_){{.u16 = value}, VAL_UINT16, 0})
#define UNT32_VAL(value)           ((Value_){{.u32 = value}, VAL_UINT32, 0})
#define UNT64_VAL(value)           ((Value_){{.u64 = value}, VAL_UINT64, 0})

#define INTX_VAL(value, width)   ((Value_){{.i = value}, VAL_INT##width, 0})
#define UINTX_VAL(value, width)  ((Value_){{.u = value}, VAL_UINT##width, 0})
#define FLOAT_VAL(value)  ((Value_){{.f = value}, VAL_FLOAT, 0})
#define DOUBLE_VAL(value) ((Value_){{.d = value}, VAL_DOUBLE, 0})

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
  } as;

  ValueType type;
  short size;
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

#endif  // VALUE__H