#include "value.h"

#include "memory.h"
#include <memory.h>

void valuearray_init(ValueArray array) {
  memset(array, 0, sizeof(ValueArray_));
}

void valuearray_write(ValueArray array, Value_ value) {
  if (array->capacity < array->count + 1) {
    int oldCapacity = array->capacity;
    array->capacity = GROW_CAPACITY(oldCapacity);
    array->values = GROW_ARRAY(Value_, array->values,
      oldCapacity, array->capacity);
  }

  array->values[array->count] = value;
  array->count++;
}

void valuearray_free(ValueArray array) {
  FREE_ARRAY(Value_, array->values, array->capacity);
  valuearray_init(array);
}

const char* kTrue = "true";
const char* kFalse = "false";

void value_print(Value_ value) {
  switch (value.type) {
    case VAL_NIL:    printf("nil"); break;
    case VAL_BOOL:   printf("%s", AS_BOOL(value) ? kTrue : kFalse); break;
    case VAL_INT:    printf("%lld", AS_INT(value)); break;
    case VAL_INT8:   printf("%hhd", AS_INT8(value)); break;
    case VAL_INT16:  printf("%hd", AS_INT16(value)); break;
    case VAL_INT32:  printf("%ld", AS_INT32(value)); break;
    case VAL_INT64:  printf("%lld", AS_INT64(value)); break;
    case VAL_UINT:   printf("%llu", AS_INT(value)); break;
    case VAL_UINT8:  printf("%hhu", AS_UINT8(value)); break;
    case VAL_UINT16: printf("%hu", AS_UINT16(value)); break;
    case VAL_UINT32: printf("%lu", AS_UINT32(value)); break;
    case VAL_UINT64: printf("%llu", AS_UINT64(value)); break;
    case VAL_FLOAT:  printf("%f", AS_FLOAT(value)); break;
    case VAL_DOUBLE: printf("%f", AS_DOUBLE(value)); break;
  }
}