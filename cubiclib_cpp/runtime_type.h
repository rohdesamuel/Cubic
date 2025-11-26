#ifndef RUNTIME_TYPE__H
#define RUNTIME_TYPE__H

#include <cstdint>

struct RuntimeType {
  bool is_ptr;
  uint32_t ty;
};

RuntimeType type_toruntime(const struct Type* ty);
uint32_t type_toint(RuntimeType info);
RuntimeType type_fromint(uint32_t n);

#endif  // RUNTIME_TYPE__H