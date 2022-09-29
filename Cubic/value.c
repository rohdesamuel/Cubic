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

void value_print(Value_ value) {
  switch (value.type) {
    case VAL_NIL:    printf("nil"); break;
    case VAL_BOOL:   printf("%s", AS_BOOL(value) ? "true" : "false"); break;
    case VAL_INT:    printf("%lld", AS_INT(value)); break;
    case VAL_INT8:   printf("%hhd", AS_INTX(value, 8)); break;
    case VAL_INT16:  printf("%hd", AS_INTX(value, 16)); break;
    case VAL_INT32:  printf("%ld", AS_INTX(value, 32)); break;
    case VAL_INT64:  printf("%lld", AS_INTX(value, 64)); break;
    case VAL_UINT:   printf("%llu", AS_INT(value)); break;
    case VAL_UINT8:  printf("%hhu", AS_UINTX(value, 8)); break;
    case VAL_UINT16: printf("%hu", AS_UINTX(value, 16)); break;
    case VAL_UINT32: printf("%lu", AS_UINTX(value, 32)); break;
    case VAL_UINT64: printf("%llu", AS_UINTX(value, 64)); break;
    case VAL_FLOAT:  printf("%f", AS_FLOAT(value)); break;
    case VAL_DOUBLE: printf("%f", AS_DOUBLE(value)); break;
  }
}