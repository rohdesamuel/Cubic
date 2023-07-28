#include "tokens.h"

#include <string.h>

bool token_eq(Token_ a, Token_ b) {
  return a.length == b.length && memcmp(a.start, b.start, a.length) == 0;
}