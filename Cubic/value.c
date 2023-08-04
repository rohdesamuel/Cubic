#include "value.h"
#include "object.h"
#include "symbol.h"

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

void value_print(Value_ value, RuntimeType_ info) {
  switch (info.ty) {
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
    case VAL_OBJ:
      switch (OBJ_TYPE(value)) {
        case OBJ_TYPE_STRING:
        {
          ObjString_* str = AS_STRING(value);
          printf("%.*s", str->length, str->chars);
          break;
        }
        default:
          printf("obj");
          break;
      }
  }
}

const char* valuetype_str(RuntimeType_ ty) {
  switch (ty.ty) {
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
    case VAL_OBJ:
      switch (ty.obj) {
        case OBJ_TYPE_STRING:
          return "obj[string]";
        default:
          return "obj";
      }
  }
  return "unknown";
}

void value_set(Value_* l, Value_* r) {

}

uint32_t type_toint(RuntimeType_ info) {
  uint32_t t = info.ty & 0xFF;
  uint32_t k = info.kind & 0xFF;
  uint32_t o = info.obj & 0xFF;
  return (t << 16) | (k << 8) | o;
}

RuntimeType_ type_fromint(uint32_t n) {
  return (RuntimeType_) {
    .ty = ((n & 0x00FF0000) >> 16),
    .kind = ((n & 0x0000FF00) >> 8),
    .obj = ((n & 0x000000FF))
  };
}