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

const char* valuetype_str(ValueType type) {
  switch (type) {
    case VAL_NIL:    return "nil";
    case VAL_BOOL:   return "bool";
    case VAL_INT:    return "int";
    case VAL_INT8:   return "int8";
    case VAL_INT16:  return "int16";
    case VAL_INT32:  return "int32";
    case VAL_INT64:  return "int64";
    case VAL_UINT:   return "uint";
    case VAL_UINT8:  return "uint8";
    case VAL_UINT16: return "uint16";
    case VAL_UINT32: return "uint32";
    case VAL_UINT64: return "uint64";
    case VAL_FLOAT:  return "float";
    case VAL_DOUBLE: return "double";
  }
  return "unknown";
}

bool value_iscoercible(Value_ from, Value_ to) {
  return

    // 64-bit conversion
    ((IS_UINT(to) || IS_UINTX(to, 64) || IS_INT(to) || IS_INTX(to, 64)) && ISA_INTEGER(to)) ||
    
    // 8-bit conversion
    ((IS_UINTX(to, 8) || IS_INTX(to, 8)) && (IS_UINTX(from, 8) || IS_INTX(from, 8))) ||
    
    // 16-bit conversion
    ((IS_UINTX(to, 16) || IS_INTX(to, 16)) && (IS_UINTX(from, 8) || IS_INTX(from, 8) ||
      IS_UINTX(from, 16) || IS_INTX(from, 16))) ||

    // 32-bit conversion
    ((IS_UINTX(to, 32) || IS_INTX(to, 32)) && (IS_UINTX(from, 8) || IS_INTX(from, 8) ||
      IS_UINTX(from, 16) || IS_INTX(from, 16) ||
      IS_UINTX(from, 32) || IS_INTX(from, 32))) ||

    // Double conversion
    (IS_DOUBLE(to) && ISA_NUMBER(from)) ||
    
    // Float conversion
    (IS_FLOAT(to) && (IS_FLOAT(from) ||
                      from.type >= VAL_INT8 && from.type <= VAL_INT32 ||
                      from.type >= VAL_UINT8 && from.type <= VAL_UINT32));
}

bool valuetype_iscoercible(ValueType from, ValueType to) {
  return
    // 64-bit conversion
    ((to == VAL_UINT || to == VAL_UINT64 || to == VAL_INT || to == VAL_INT64) &&
      (from >= VAL_INT && from <= VAL_UINT64)) ||

    // 8-bit conversion
    ((to == VAL_UINT8 || to == VAL_INT8) && (from == VAL_UINT8 || from == VAL_INT8)) ||

    // 16-bit conversion
    ((to == VAL_UINT16 || to == VAL_INT16) && (from == VAL_UINT8 || from == VAL_INT8 ||
      from == VAL_UINT16 || from == VAL_INT16)) ||

    // 32-bit conversion
    ((to == VAL_UINT32 || to == VAL_INT32) && (from == VAL_UINT8 || from == VAL_INT8 ||
      from == VAL_UINT16 || from == VAL_INT16 ||
      from == VAL_UINT32 || from == VAL_INT32)) ||

    // Double conversion
    (to == VAL_DOUBLE && (from >= VAL_INT && from <= VAL_DOUBLE)) ||

    // Float conversion
    (to == VAL_FLOAT && (from == VAL_FLOAT ||
      from >= VAL_INT8 && from <= VAL_INT32 ||
      from >= VAL_UINT8 && from <= VAL_UINT32));
}