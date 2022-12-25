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

void value_print(Value_ value) {
  switch (value.type.ty) {
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

const char* valuetype_str(Type_ ty) {
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

const char* value_typestr(Value_* value) {
  return valuetype_str(value->type);
}

bool value_iscoercible(Value_ from, Value_ to) {
  return type_iscoercible(from.type, to.type);
}

bool value_equal(Value_* l, Value_* r) {
  switch (l->type.ty) {
    case VAL_OBJ:
    {
      ObjString_* l_string = AS_STRING(*l);
      ObjString_* r_string = AS_STRING(*r);
      return l_string == r_string ||
        (l_string->length == r_string->length && memcmp(l_string->chars, r_string->chars, l_string->length) == 0);
    }

    default:
      return l->as.u == r->as.u;
  }
}

void value_set(Value_* l, Value_* r) {

}

bool type_iscoercible(Type_ from, Type_ to) {
  return type_equiv(from, to) ||
    // 64-bit conversion
    ((to.ty == VAL_UINT || to.ty == VAL_UINT64 || to.ty == VAL_INT || to.ty == VAL_INT64) &&
      (from.ty >= VAL_INT && from.ty <= VAL_UINT64)) ||

    // 8-bit conversion
    ((to.ty == VAL_UINT8 || to.ty == VAL_INT8) && (from.ty == VAL_UINT8 || from.ty == VAL_INT8)) ||

    // 16-bit conversion
    ((to.ty == VAL_UINT16 || to.ty == VAL_INT16) && (from.ty == VAL_UINT8 || from.ty == VAL_INT8 ||
      from.ty == VAL_UINT16 || from.ty == VAL_INT16)) ||

    // 32-bit conversion
    ((to.ty == VAL_UINT32 || to.ty == VAL_INT32) && (from.ty == VAL_UINT8 || from.ty == VAL_INT8 ||
      from.ty == VAL_UINT16 || from.ty == VAL_INT16 ||
      from.ty == VAL_UINT32 || from.ty == VAL_INT32)) ||

    // Double conversion
    (to.ty == VAL_DOUBLE && (from.ty >= VAL_INT && from.ty <= VAL_DOUBLE)) ||

    // Float conversion
    (to.ty == VAL_FLOAT && (from.ty == VAL_FLOAT ||
      from.ty >= VAL_INT8 && from.ty <= VAL_INT32 ||
      from.ty >= VAL_UINT8 && from.ty <= VAL_UINT32));
}

uint32_t type_toint(Type_ type) {
  uint32_t t = type.ty & 0xFF;
  uint32_t k = type.kind & 0xFF;
  uint32_t o = type.obj & 0xFF;
  return (t << 16) | (k << 8) | o;
}

Type_ type_fromint(uint32_t n) {
  return (Type_) {
    .ty = ((n & 0x00FF0000) >> 16),
    .kind = ((n & 0x0000FF00) >> 8),
    .obj = ((n & 0x000000FF))
  };
}