#include "runtime_type.h"
#include "type.h"

#define IS_PTR_MASK 0x80000000
#define ATTRIBUTE_MASK 0xFF000000

RuntimeType_ type_toruntime(const Type_* ty) {
  Type_* sub_ty = type_valtype(type_deref((Type_*)ty));
  return (RuntimeType_) {
    .is_ptr = type_isavar(ty) || type_isaref(ty),
    .ty = sub_ty->cls
  };
}

uint32_t type_toint(RuntimeType_ info) {
  uint32_t ret = info.ty;
  if (info.is_ptr) {
    ret = ret | IS_PTR_MASK;
  }
  return ret;
}

RuntimeType_ type_fromint(uint32_t n) {
  return (RuntimeType_) {
    .is_ptr = (bool)(n & IS_PTR_MASK),
    .ty = n & ~ATTRIBUTE_MASK,
  };
}