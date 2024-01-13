#ifndef RUNTIME_TYPE__H
#define RUNTIME_TYPE__H

#include "common.h"

typedef struct RuntimeType_ {
  int ty;
} RuntimeType_;

RuntimeType_ type_toruntime(const struct Type_* ty);
uint32_t type_toint(RuntimeType_ info);
RuntimeType_ type_fromint(uint32_t n);

#endif  // RUNTIME_TYPE__H