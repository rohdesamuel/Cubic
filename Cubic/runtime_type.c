#include "runtime_type.h"
#include "type.h"

RuntimeType_ type_toruntime(const Type_* ty) {
  ty = type_valtype(type_deref((Type_*)ty));
  return (RuntimeType_) {
    .ty = ty->cls
  };
}

uint32_t type_toint(RuntimeType_ info) {
  return info.ty;
}

RuntimeType_ type_fromint(uint32_t n) {
  return (RuntimeType_) {
    .ty = n,
  };
}