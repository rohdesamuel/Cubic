#include "runtime_type.h"
#include "type.h"

#define IS_PTR_MASK 0x80000000
#define ATTRIBUTE_MASK 0xFF000000

RuntimeType type_toruntime(const Type* ty) {
  return RuntimeType{
    .is_ptr = ty->val_kind != KIND_VAL,
    .ty = ty->cls
  };
}

uint32_t type_toint(RuntimeType info) {
  uint32_t ret = info.ty;
  if (info.is_ptr) {
    ret = ret | IS_PTR_MASK;
  }
  return ret;
}

RuntimeType type_fromint(uint32_t n) {
  return RuntimeType{
    .is_ptr = (bool)(n & IS_PTR_MASK),
    .ty = n & ~ATTRIBUTE_MASK,
  };
}