#include "errors.h"

#include <memory.h>
#include <stdarg.h>

void errorscontainer_init(ErrorsContainer_* errors, MemoryAllocator_* allocator) {
  *errors = (ErrorsContainer_){ 0 };
  errors->allocator = allocator;

  list_of(&errors->errors, Error_, allocator);
}

void errorscontainer_clear(ErrorsContainer_* errors) {
  for (ListNode_* n = errors->errors.head; n != NULL; n = n->next) {
    Error_* err = list_ptr(n, Error_);
    dealloc(errors->allocator, (char*)err->error_str);
  }
  list_clear(&errors->errors);
}

void error_add(ErrorsContainer_* errors, int line, const char* format, ...) {
  if (errors->panic_mode) return;
  errors->has_errors = true;

  char buf[1024] = { 0 };
  int cursor = 0;

  va_list args;
  va_start(args, format);

  cursor = sprintf_s(buf, sizeof(buf), "[line %d] Error: ", line);
  cursor += vsprintf_s(buf, sizeof(buf), format, args);
  assertf(cursor < sizeof(buf), "Could not write error to log (error too long).");

  cursor += sprintf_s(buf, sizeof(buf), "\n");
  assertf(cursor < sizeof(buf), "Could not write error to log (could not append new line).");

  buf[cursor] = '\0';
  cursor += 1;
  assertf(cursor < sizeof(buf), "Could not write error to log (could not make null-terminated).");

  Error_ err = { 0 };
  err.error_str = alloc(errors->allocator, cursor);
  err.line = line;
  memcpy((void*)err.error_str, (void*)buf, cursor);
  printf(err.error_str);
  list_push(&errors->errors, &err);

  va_end(args);
}

void error_panic(ErrorsContainer_* errors, int line, const char* format, ...) {
  if (errors->panic_mode) return;
  errors->panic_mode = true;
  errors->has_errors = true;

  char buf[1024] = { 0 };
  int cursor = 0;

  va_list args;
  va_start(args, format);

  cursor = sprintf_s(buf, sizeof(buf), "[line %d] Error: ", line);
  cursor += vsprintf_s(buf, sizeof(buf), format, args);
  assertf(cursor < sizeof(buf), "Could not write error to log (error too long).");

  cursor += sprintf_s(buf, sizeof(buf), "\n");
  assertf(cursor < sizeof(buf), "Could not write error to log (could not append new line).");

  buf[cursor] = '\0';
  cursor += 1;
  assertf(cursor < sizeof(buf), "Could not write error to log (could not make null-terminated).");

  Error_ err = { 0 };
  err.error_str = alloc(errors->allocator, cursor);
  err.line = line;
  memcpy((void*)err.error_str, (void*)buf, cursor);
  printf(err.error_str);
  list_push(&errors->errors, &err);

  va_end(args);
}