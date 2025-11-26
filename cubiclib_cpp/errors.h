#ifndef ERRORS__H
#define ERRORS__H

#include <string>
#include <vector>

#include "common.h"
#include "memory.h"

struct Error {
  std::string msg;
  int line;
  const char* source;
};

class ErrorsContainer {
public:

  void Clear();
  void Err(int line, const char* format, ...);
  void Panic(int line, const char* format, ...);
  void ClearPanic() { panic_mode_ = false; };

  bool panic_mode() const { return panic_mode_; }
  bool has_errors() const { return has_errors_; }

private:
  std::vector<Error> errors_;
  bool panic_mode_;
  bool has_errors_;
};

#endif  // ERRORS__Hs