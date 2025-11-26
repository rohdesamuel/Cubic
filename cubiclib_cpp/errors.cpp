#include "errors.h"

#include <iostream>
#include <memory>
#include <stdarg.h>

thread_local ErrorsContainer* cur_errors_;

void ErrorsContainer::Clear() {
  errors_ = {};
}

void ErrorsContainer::Err(int line, const char* format, ...) {
  if (panic_mode_) return;
  has_errors_ = true;

  char buf[1024] = { 0 };
  int cursor = 0;

  va_list args;
  va_start(args, format);

  cursor = sprintf_s(buf, sizeof(buf), "[line %d] Error: ", line);
  cursor += vsprintf_s(buf, sizeof(buf), format, args);
  assertf(cursor > 0 && cursor < sizeof(buf), "Could not write error to log (error too long).");

  cursor += sprintf_s(buf, sizeof(buf), "\n");
  assertf(cursor > 0 && cursor < sizeof(buf), "Could not write error to log (could not append new line).");

  buf[cursor] = '\0';
  cursor += 1;
  assertf(cursor > 0 && cursor < sizeof(buf), "Could not write error to log (could not make null-terminated).");

  Error err = {
    .msg = std::string(buf, cursor),
    .line = line
  };

  std::cerr << err.msg << std::endl;

  va_end(args);
}

void ErrorsContainer::Panic(int line, const char* format, ...) {
  if (panic_mode_) return;
  panic_mode_ = true;
  has_errors_ = true;

  char buf[1024] = { 0 };
  int cursor = 0;

  va_list args;
  va_start(args, format);

  cursor = sprintf_s(buf, sizeof(buf), "[line %d] Error: ", line);
  cursor += vsprintf_s(buf, sizeof(buf), format, args);
  assertf(cursor > 0 && cursor < sizeof(buf), "Could not write error to log (error too long).");

  cursor += sprintf_s(buf, sizeof(buf), "\n");
  assertf(cursor > 0 && cursor < sizeof(buf), "Could not write error to log (could not append new line).");

  buf[cursor] = '\0';
  cursor += 1;
  assertf(cursor > 0 && cursor < sizeof(buf), "Could not write error to log (could not make null-terminated).");

  Error err = {
    .msg = std::string(buf, cursor),
    .line = line
  };

  std::cerr << err.msg << std::endl;

  va_end(args);
}