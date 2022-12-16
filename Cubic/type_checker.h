#ifndef TYPE_CHECKER__H
#define TYPE_CHECKER__H

#include "common.h"
#include "memory.h"

typedef struct TypeChecker_ {
  PageAllocator_ allocator;
} TypeChecker_;

#endif  // TYPE_CHECKER__H