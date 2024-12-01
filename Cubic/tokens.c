#include "tokens.h"
#include "hash.h"

#include <string.h>

bool token_eq(Token_ a, Token_ b) {
  return a.length == b.length && memcmp(a.start, b.start, a.length) == 0;
}

Token_ token_string(const char* str) {
  int len = (int)strlen(str);
  return (Token_) { .start = str, .length = len };
}

Token_ token_concat(Token_ l, Token_ r, MemoryAllocator_* allocator) {
  Token_ ret = {.length = l.length + r.length};
  ret.start = alloc(allocator, ret.length);
  memcpy((void*)ret.start, l.start, l.length);
  memcpy((void*)(ret.start + l.length), r.start, r.length);

  return ret;
}

uint64_t token_hash(Token_ t) {
  return hash_bytes(t.start, t.length);
}