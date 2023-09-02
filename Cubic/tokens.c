#include "tokens.h"

#include <string.h>

bool token_eq(Token_ a, Token_ b) {
  return a.length == b.length && memcmp(a.start, b.start, a.length) == 0;
}

Token_ token_string(const char* str) {
  int len = (int)strlen(str);
  len = len == 0 ? 0 : len - 1;
  return (Token_) { .start = str, .length = len };
}