#include "tokens.h"

#include <cstdint>
#include <string_view>


Token Token::from_str(const char* str) {
  return Token{
    .type = TK_STRING,
    .tk = str,
    .line = -1
  };
}
