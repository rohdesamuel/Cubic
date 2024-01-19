#ifndef ERRORS__H
#define ERRORS__H

#include "common.h"

typedef struct Error_ {
  const char* error_str;
  int line;
} Error_;

typedef struct ErrorsContainer_ {
  ListOf_(Error_) errors;
  bool panic_mode;
  bool has_errors;

  MemoryAllocator_* allocator;
} ErrorsContainer_;

void errorscontainer_init(ErrorsContainer_* errors, MemoryAllocator_* allocator);
void errorscontainer_clear(ErrorsContainer_* errors);

void error_add(ErrorsContainer_* errors, int line, const char* format, ...);
void error_panic(ErrorsContainer_* errors, int line, const char* format, ...);

#endif  // ERRORS__H