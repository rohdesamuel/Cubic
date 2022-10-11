#ifndef SCANNER__H
#define SCANNER__H

#include "common.h"
#include "tokens.h"

typedef struct Scanner_ {
  const char* start;
  const char* current;
  int line;
} Scanner_, * Scanner;

void scanner_init(Scanner_* scanner, const char* source);
Token_ scanner_scan(Scanner scanner);

#endif  // SCANNER__H