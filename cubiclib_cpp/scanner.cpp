#include "scanner.h"

#include "common.h"
#include "tokens.h"

#include <string>


Scanner::Scanner(const char* source):
  start_(source),
  current_(source),
  line_(1) { }

Token Scanner::Scan() {
  Token err_tk = SkipWhitespace();
  if (err_tk.type == TK_ERR) {
    return err_tk;
  }

  start_ = current_;

  if (*current_ == '\0') {
    return MakeToken(TK_EOF);
  }

  char c = Advance();
  if (isalpha(c) || c == '_') return MatchIdentifier();
  if (isdigit(c)) {
    while (isdigit(Peek())) Advance();
    if (Match('.')) {
      if (Match('.')) {
        Reverse();
        Reverse();
        return MakeToken(TK_INTEGER);
      }
      Reverse();
    }
    return MatchNumber();
  }

  switch (c) {
    // Single-character tokens.
    case '(': return MakeToken(TK_LPAREN);
    case ')': return MakeToken(TK_RPAREN);
    case '[': return MakeToken(TK_LBRACKET);
    case ']': return MakeToken(TK_RBRACKET);
    case '{': return MakeToken(TK_LBRACE);
    case '}': return MakeToken(TK_RBRACE);
    case ';': return MakeToken(TK_SEMICOLON);
    case ':': return MakeToken(TK_COLON);
    case ',': return MakeToken(TK_COMMA);

      // Multi-character tokens.
      // !, !=
    case '!':
      return MakeToken(
        Match('=') ? TK_BANG_EQUAL : TK_BANG);

      // ?, ??, ??=
    case '?':
    {
      char c1 = Peek();
      if (c1 != '?') {
        return MakeToken(TK_QUESTION);
      }
      Advance();

      char c2 = Peek();
      Advance();
      if (c2 == '=') {
        return MakeToken(TK_QQE);
      } else {
        return MakeToken(TK_QQ);
      }
    }

    // ==, =>
    case '=':
    {
      char c = Peek();
      switch (c) {
        case '=': Advance(); return MakeToken(TK_EQUAL_EQUAL);
        case '>': Advance(); return MakeToken(TK_FAT_ARROW);
      }
      return MakeToken(TK_EQUAL);
    }

    // <, <=, <<
    case '<':
    {
      char c = Peek();
      switch (c) {
        case '=': Advance(); return MakeToken(TK_LTE);
        case '<': Advance(); return MakeToken(TK_LSHIFT);
      }
      return MakeToken(TK_LT);
    }

    // >, >=, >>
    case '>':
    {
      char c = Peek();
      switch (c) {
        case '=': Advance(); return MakeToken(TK_GTE);
        case '>': Advance(); return MakeToken(TK_RSHIFT);
      }
      return MakeToken(TK_GT);
    }

    case '+':
    {
      if (Peek() == '=') {
        Advance();
        return MakeToken(TK_PLUS_EQUAL);
      }
      return MakeToken(TK_PLUS);
    }

    case '&':
    {
      if (Peek() == '=') {
        Advance();
        return MakeToken(TK_AMPERSAND_EQUAL);
      }
      return MakeToken(TK_AMPERSAND);
    }

    case '|':
    {
      if (Peek() == '=') {
        Advance();
        return MakeToken(TK_PIPE_EQUAL);
      }
      return MakeToken(TK_PIPE);
    }

    case '^':
    {
      if (Peek() == '=') {
        Advance();
        return MakeToken(TK_HAT_EQUAL);
      }
      return MakeToken(TK_HAT);
    }

    case '~':
    {
      if (Peek() == '=') {
        Advance();
        return MakeToken(TK_TILDE_EQUAL);
      }
      return MakeToken(TK_TILDE);
    }

    case '*':
    {
      if (Peek() == '=') {
        Advance();
        return MakeToken(TK_STAR_EQUAL);
      }
      return MakeToken(TK_STAR);
    }

    case '%':
    {
      if (Peek() == '=') {
        Advance();
        return MakeToken(TK_PERCENT_EQUAL);
      }
      return MakeToken(TK_PERCENT);
    }

    // -, ->
    case '-':
    {
      char c = Peek();
      switch (c) {
        case '>': Advance(); return MakeToken(TK_ARROW);
        case '=': Advance(); return MakeToken(TK_MINUS_EQUAL);
      }
      return MakeToken(TK_MINUS);
    }

    // /, //
    case '/':
    {
      char c = Peek();
      switch (c) {
        case '/':
        {
          Advance();
          if (Peek() == '=') {
            Advance();
            return MakeToken(TK_SLASH_SLASH_EQUAL);
          } else {
            return MakeToken(TK_DOUBLE_SLASH);
          }
        }
        case '=': Advance(); return MakeToken(TK_SLASH_EQUAL);
      }
      return MakeToken(TK_SLASH);
    }

    // ., .., ...
    case '.':
    {
      // Match numbers that start_ with a '.' and without a number before it.
      if (isdigit(Peek())) {
        return MatchFractional();
      } else {
        if (Match('.')) {
          if (Match('.')) {
            return MakeToken(TK_TRIPLE_DOT);
          } else {
            return MakeToken(TK_DOUBLE_DOT);
          }
        } else {
          return MakeToken(TK_DOT);
        }
      }
    }

    // '
    case '\'': return MatchString('\'');

      // "
    case '"': return MatchString('"');
  }

  return ErrorToken("Unexpected character.");
}

char Scanner::Advance() {
  current_++;
  return current_[-1];
}

void Scanner::Reverse() {
  current_--;
}

Token Scanner::MakeToken(TokenType info) const {
  return Token{
    .type = info,
    .tk = std::string_view(start_, (int)(current_ - start_)),
    .line = line_
  };
}

Token Scanner::ErrorToken(const char* message) const {
  return Token{
    .type = TK_ERR,
    .tk = std::string_view(message, (int)strlen(message)),
    .line = line_
  };
}

bool Scanner::Match(char expected) {
  if (IsAtEnd()) return false;
  if (*current_ != expected) return false;
  current_++;
  return true;
}

Token Scanner::SkipWhitespace() {
  for (;;) {
    switch (Peek()) {
      case ' ':
      case '\r':
      case '\t':
        Advance();
        break;
      case '\n':
        ++line_;
        Advance();
        break;
#if 1
      case '/':
        if (*(current_ + 1) == '#') {
          Advance();  // Consume '/'
          for (;;) {
            Advance();
            if (IsAtEnd()) {
              return ErrorToken("Unmatched multi-line comment.");
            }

            if (Peek() == '#' && *(current_ + 1) == '/') {
              Advance();
              break;
            }
          }
          Advance();
          break;
        }
        return { .type = TK_NIL };
#endif
      case '#':
        while (Peek() != '\n' && !IsAtEnd()) {
          Advance();
        }
        break;
      default:
        return { .type = TK_NIL };
    }
  }

  return { .type = TK_NIL };
}

bool Scanner::IsAtEnd() const {
  return *current_ == '\0';
}

char Scanner::Peek() const {
  return *current_;
}

char Scanner::PeekNext() const {
  if (IsAtEnd()) return '\0';
  return current_[1];
}

Token Scanner::MatchString(char string_token) {
  while (Peek() != string_token && !IsAtEnd()) {
    if (Peek() == '\n') line_++;
    Advance();
  }

  if (IsAtEnd()) return ErrorToken("Unterminated string.");

  // The closing quote.
  Advance();
  return MakeToken(TK_STRING);
}

Token Scanner::MatchNumber() {
  while (isdigit(Peek())) Advance();

  TokenType info = TK_INTEGER;
  // Look for a fractional part.
  if (Peek() == '.') {
    info = TK_NUMBER;

    // Consume the ".".
    Advance();

    while (isdigit(Peek())) Advance();

    // Allow for floats with the syntax like "1.f"
    if (Peek() == 'f') Advance();
  }

  return MakeToken(info);
}

Token Scanner::MatchInteger() {
  while (isdigit(Peek())) Advance();

  return MakeToken(TK_INTEGER);
}

Token Scanner::MatchFractional() {
  while (isdigit(Peek())) Advance();

  return MakeToken(TK_NUMBER);
}

Token Scanner::MatchIdentifier() {
  char c = Peek();
  while (isalpha(c) || isdigit(c) || c == '_') {
    Advance();
    c = Peek();
  }

  return MakeToken(IdentifierType());
}

TokenType Scanner::IdentifierType() {
  int length = (int)(current_ - start_);

  switch (start_[0]) {
    // and, async, await
    case 'a':
      if (length > 1) {
        switch (start_[1]) {
          case 'n': return CheckKeyword(2, 1, "d", TK_AND);
          case 's':
            if (current_ - start_ > 2) {
              switch (start_[2]) {
                case 's': return CheckKeyword(3, 3, "ert", TK_ASSERT);
                case 'y': return CheckKeyword(3, 2, "nc", TK_ASYNC);
              }
            }
          case 'w': return CheckKeyword(2, 3, "ait", TK_AWAIT);
        }
      }
      break;

      // bool
    case 'b': return CheckKeyword(1, 3, "ool", TK_BOOL);

      // case, class
    case 'c':
    {
      if (length > 1) {
        switch (start_[1]) {
          case 'a': return CheckKeyword(2, 2, "se", TK_CASE);
          case 'l': return CheckKeyword(2, 3, "ass", TK_CLASS);
        }
      }
      break;
    }


    // del, do, double
    case 'd':
    {
      if (length > 1) {
        switch (start_[1]) {
          case 'e': return CheckKeyword(2, 1, "l", TK_DEL);
          case 'o':
            if (length == 2) {
              return TK_DO;
            } else {
              return CheckKeyword(2, 4, "uble", TK_DOUBLE);
            }
        }
      }
      break;
    }

    // elif, else, end
    case 'e':
      if (length > 1) {
        switch (start_[1]) {
          case 'l':
            if (current_ - start_ > 2) {
              switch (start_[2]) {
                case 'i': return CheckKeyword(3, 1, "f", TK_ELIF);
                case 's': return CheckKeyword(3, 1, "e", TK_ELSE);
              }
            }
          case 'n': return CheckKeyword(2, 1, "d", TK_END);
        }
      }
      break;

      // false, float, for, function
    case 'f':
      if (length > 1) {
        switch (start_[1]) {
          case 'a': return CheckKeyword(2, 3, "lse", TK_FALSE);
          case 'l': return CheckKeyword(2, 3, "oat", TK_FLOAT);
          case 'o': return CheckKeyword(2, 1, "r", TK_FOR);
          case 'u': return CheckKeyword(2, 6, "nction", TK_FUNCTION);
        }
      }
      break;

      // if, in, int
    case 'i':
    {
      if (length > 1) {
        switch (start_[1]) {
          case 'f': return CheckKeyword(1, 1, "f", TK_IF);
          case 'n':
          {
            if (length == 2) {
              return TK_IN;
            } else {
              // TODO: match integer width
              return CheckKeyword(2, 1, "t", TK_INT);
            }
            break;
          }
          case 's': return TK_IS;
        }
      }
      break;
    }

    // match
    case 'm': return CheckKeyword(1, 4, "atch", TK_MATCH);

      // new, nil, not
    case 'n':
      if (length > 1) {
        switch (start_[1]) {
          case 'e': return CheckKeyword(2, 1, "w", TK_NEW);
          case 'i': return CheckKeyword(2, 1, "l", TK_NIL);
          case 'o': return CheckKeyword(2, 1, "t", TK_NOT);
        }
      }
      break;

      // or
    case 'o':
      if (length > 1) {
        switch (start_[1]) {
          case 'r': return CheckKeyword(1, 1, "r", TK_OR);
          case 'u': return CheckKeyword(1, 2, "ut", TK_OUT);
        }
      }

      // pass
    case 'p':
      if (length > 1) {
        switch (start_[1]) {
          case 'a': return CheckKeyword(2, 2, "ss", TK_PASS);
          case 'r': return CheckKeyword(2, 3, "int", TK_PRINT);
        }
      }
      break;

      // ref, repeat, return
    case 'r':
      if (length > 2) {
        if (start_[1] == 'e') {
          if (length == 3 && start_[2] == 'f') {
            return TK_REF;
          }

          switch (start_[2]) {
            case 'p': return CheckKeyword(3, 3, "eat", TK_REPEAT);
            case 't': return CheckKeyword(3, 3, "urn", TK_RETURN);
          }
        }
      }
      break;

      // step, string
    case 's':
      if (length > 2) {
        if (start_[1] == 't') {
          switch (start_[2]) {
            case 'e': return CheckKeyword(3, 1, "p", TK_STEP);
            case 'r':
              if (current_ - start_ > 3) {
                switch (start_[3]) {
                  case 'i': return CheckKeyword(4, 2, "ng", TK_STRING_TYPE);
                }
              }
          }
        }
      }
      break;

      // then, true
    case 't':
      if (length > 1) {
        switch (start_[1]) {
          case 'h': return CheckKeyword(2, 2, "en", TK_THEN);
          case 'r': return CheckKeyword(2, 2, "ue", TK_TRUE);
          case 'y': return CheckKeyword(2, 2, "pe", TK_TYPE);
        }
      }
      break;

      // uint, until
    case 'u':
      if (length > 1) {
        switch (start_[1]) {
          // TODO: match integer width
          case 'i': return CheckKeyword(2, 2, "nt", TK_UINT);
          case 'n': return CheckKeyword(2, 3, "til", TK_UNTIL);
        }
      }
      break;

      // pval, var
    case 'v':
      if (length == 3 && start_[1] == 'a') {
        if (start_[2] == 'l') {
          return TK_VAL;
        }

        if (start_[2] == 'r') {
          return TK_VAR;
        }
      }
      break;

      // while
    case 'w': return CheckKeyword(1, 4, "hile", TK_WHILE);

      // xor
    case 'x': return CheckKeyword(1, 2, "or", TK_XOR);

      // yield
    case 'y': return CheckKeyword(1, 4, "ield", TK_YIELD);

      // List
    case 'L': return CheckKeyword(1, 3, "ist", TK_LIST);

      // Map
    case 'M': return CheckKeyword(1, 2, "ap", TK_MAP);

      // Set
    case 'S': return CheckKeyword(1, 2, "et", TK_SET);
  }

  return TK_ID;
}

TokenType Scanner::CheckKeyword(int start, int length,
  const char* rest, TokenType info) const {
  if (current_ - start_ == start + length &&
    memcmp(start_ + start, rest, length) == 0) {
    return info;
  }

  return TK_ID;
}
