#ifndef COMPILER__H
#define COMPILER__H

#include <memory>

#include "scanner.h"

class Compiler {
public:
  Compiler();

private:
  std::unique_ptr<Scanner> scanner_;
};

#endif  // COMPILER__H