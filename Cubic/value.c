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
    case TYPE_CLS(NilType_):    printf("nil"); break;
    case TYPE_CLS(BoolType_):   printf("%s", AS_BOOL(value) ? kTrue : kFalse); break;
    case TYPE_CLS(IntType_):    printf("%lld", AS_INT(value)); break;
    case TYPE_CLS(Int8Type_):   printf("%hhd", AS_INT8(value)); break;
    case TYPE_CLS(Int16Type_):  printf("%hd", AS_INT16(value)); break;
    case TYPE_CLS(Int32Type_):  printf("%ld", AS_INT32(value)); break;
    case TYPE_CLS(Int64Type_):  printf("%lld", AS_INT64(value)); break;
    case TYPE_CLS(UintType_):   printf("%llu", AS_INT(value)); break;
    case TYPE_CLS(Uint8Type_):  printf("%hhu", AS_UINT8(value)); break;
    case TYPE_CLS(Uint16Type_): printf("%hu", AS_UINT16(value)); break;
    case TYPE_CLS(Uint32Type_): printf("%lu", AS_UINT32(value)); break;
    case TYPE_CLS(Uint64Type_): printf("%llu", AS_UINT64(value)); break;
    case TYPE_CLS(FloatType_):  printf("%f", AS_FLOAT(value)); break;
    case TYPE_CLS(DoubleType_): printf("%f", AS_DOUBLE(value)); break;
  }
}

const char* type_tostr(const Type_* ty) {
  switch (ty->cls) {
    case TYPE_CLS(UnknownType_):
      return "<unknown>";

    case TYPE_CLS(NilType_):
      return "nil";

    case TYPE_CLS(IntType_):
      return "int";

    case TYPE_CLS(Int8Type_):
      return "int8";

    case TYPE_CLS(Int16Type_):
      return "int16";

    case TYPE_CLS(Int32Type_):
      return "int32";

    case TYPE_CLS(Int64Type_):
      return "int64";

    case TYPE_CLS(UintType_):
      return "uint";

    case TYPE_CLS(Uint8Type_):
      return "uint8";

    case TYPE_CLS(Uint16Type_):
      return "uint16";

    case TYPE_CLS(Uint32Type_):
      return "uint32";

    case TYPE_CLS(Uint64Type_):
      return "uint64";

    case TYPE_CLS(FloatType_):
      return "float";

    case TYPE_CLS(DoubleType_):
      return "double";

    case TYPE_CLS(StringType_):
      return "string";

    case TYPE_CLS(ClassType_):
      return "class";

    case TYPE_CLS(ArrayType_):
      return "array";

    case TYPE_CLS(VarType_):
      return "var";

    case TYPE_CLS(InType_):
      return "in";

    case TYPE_CLS(OutType_):
      return "out";

    case TYPE_CLS(PlaceholderType_):
      return "placeholder";

  }
  return "unknown";
}

void value_set(Value_* l, Value_* r) {

}

uint32_t type_toint(RuntimeType_ info) {
  return info.ty;
}

RuntimeType_ type_fromint(uint32_t n) {
  return (RuntimeType_) {
    .ty = n,
  };
}