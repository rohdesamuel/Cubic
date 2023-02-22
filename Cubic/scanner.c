#include "scanner.h"
#include "lexer.h"

#include "common.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>


static char advance(Scanner scanner);
static bool isatend(Scanner scanner);
static Token_ make_token(Scanner scanner, TokenType info);
static Token_ error_token(Scanner scanner, const char* message);
static bool match(Scanner scanner, char expected);
static Token_ skip_whitespace(Scanner scanner);

static char peek(Scanner scanner);
static char peek_next(Scanner scanner);

static Token_ match_string(Scanner scanner, char string_token);
static Token_ match_number(Scanner scanner);
static Token_ match_fractional(Scanner scanner);
static Token_ match_identifier(Scanner scanner);
static TokenType identifier_type(Scanner scanner);
static TokenType check_keyword(Scanner scanner, int start, int length,
  const char* rest, TokenType info);

void scanner_init(Scanner_* scanner, const char* source) {
  scanner->start = source;
  scanner->current = source;
  scanner->line = 1;
}

Token_ scanner_scan(Scanner scanner) {
  Token_ err_tk = skip_whitespace(scanner);
  if (err_tk.type == TK_ERR) {
    return err_tk;
  }

  scanner->start = scanner->current;

  if (*scanner->current == '\0') {
    return make_token(scanner, TK_EOF);
  }

  char c = advance(scanner);
  if (isalpha(c) || c == '_') return match_identifier(scanner);
  if (isdigit(c)) return match_number(scanner);

  switch (c) {
    // Single-character tokens.
    case '(': return make_token(scanner, TK_LPAREN);
    case ')': return make_token(scanner, TK_RPAREN);
    case '[': return make_token(scanner, TK_LBRACKET);
    case ']': return make_token(scanner, TK_RBRACKET);
    case '{': return make_token(scanner, TK_LBRACE);
    case '}': return make_token(scanner, TK_RBRACE);
    case ';': return make_token(scanner, TK_SEMICOLON);
    case ':': return make_token(scanner, TK_COLON);
    case ',': return make_token(scanner, TK_COMMA);
    case '+': return make_token(scanner, TK_PLUS);
    case '&': return make_token(scanner, TK_AMPERSAND);
    case '|': return make_token(scanner, TK_PIPE);
    case '^': return make_token(scanner, TK_HAT);
    case '~': return make_token(scanner, TK_TILDE);
    case '*': return make_token(scanner, TK_STAR);
    case '%': return make_token(scanner, TK_PERCENT);

    // Multi-character tokens.
    case '!':
      return make_token(scanner,
        match(scanner, '=') ? TK_BANG_EQUAL : TK_BANG);
    case '=':
    {
      char c = peek(scanner);
      switch (c) {
        case '=': advance(scanner); return make_token(scanner, TK_EQUAL_EQUAL);
        case '>': advance(scanner); return make_token(scanner, TK_FAT_ARROW);
      }
      return make_token(scanner, TK_EQUAL);
    }
    case '<':
    {
      char c = peek(scanner);
      switch (c) {
        case '=': advance(scanner); return make_token(scanner, TK_LTE);
        case '<': advance(scanner); return make_token(scanner, TK_LSHIFT);
      }
      return make_token(scanner, TK_LT);
    }
    case '>':
    {
      char c = peek(scanner);
      switch (c) {
        case '=': advance(scanner); return make_token(scanner, TK_GTE);
        case '>': advance(scanner); return make_token(scanner, TK_RSHIFT);
      }
      return make_token(scanner, TK_GT);
    }
    case '-':
      return make_token(scanner,
        match(scanner, '>') ? TK_ARROW : TK_MINUS);
    case '/':
      return make_token(scanner,
        match(scanner, '/') ? TK_DOUBLE_SLASH : TK_SLASH);
    case '.':
    {
      // Match numbers that start with a '.' and without a number before it.
      if (isdigit(peek(scanner))) {
        return match_fractional(scanner);
      } else {
        if (match(scanner, '.')) {
          if (match(scanner, '.')) {
            return make_token(scanner, TK_TRIPLE_DOT);
          } else {
            return make_token(scanner, TK_DOUBLE_DOT);
          }
        } else {
          return make_token(scanner, TK_DOT);
        }
      }
    }
    case '\'': return match_string(scanner, '\'');
    case '"': return match_string(scanner, '"');
  }  

  return error_token(scanner, "Unexpected character.");
}

static char advance(Scanner scanner) {
  scanner->current++;
  return scanner->current[-1];
}

static Token_ make_token(Scanner scanner, TokenType info) {
  Token_ ret;
  ret.type = info;
  ret.start = scanner->start;
  ret.length = (int)(scanner->current - scanner->start);
  ret.line = scanner->line;
  
  return ret;
}

static Token_ error_token(Scanner scanner, const char* message) {
  Token_ token = {
    .type = TK_ERR,
    .start = message,
    .length = (int)strlen(message),
    .line = scanner->line
  };

  return token;
}

static bool match(Scanner scanner, char expected) {
  if (isatend(scanner)) return false;
  if (*scanner->current != expected) return false;
  scanner->current++;
  return true;
}

static Token_ skip_whitespace(Scanner scanner) {
  for (;;) {
    switch (peek(scanner)) {
      case ' ':
      case '\r':
      case '\t':
        advance(scanner);
        break;
      case '\n':
        ++scanner->line;
        advance(scanner);
        break;
#if 1
      case '/':
        if (*(scanner->current + 1) == '#') {
          advance(scanner);  // Consume '/'
          for (;;) {
            advance(scanner);
            if (isatend(scanner)) {
              return error_token(scanner, "Unmatched multi-line comment.");
            }

            if (peek(scanner) == '#' && *(scanner->current + 1) == '/') {
              advance(scanner);
              break;
            }            
          }
          advance(scanner);
          break;
        }
        return (Token_) { .type = TK_NIL };
#endif
      case '#':
        while (peek(scanner) != '\n' && !isatend(scanner)) {
          advance(scanner);
        }
        break;
      default:
        return (Token_) { .type = TK_NIL };
    }
  }

  return (Token_) {.type = TK_NIL};
}

static bool isatend(Scanner scanner) {
  return *scanner->current == '\0';
}

static char peek(Scanner scanner) {
  return *scanner->current;
}

static char peek_next(Scanner scanner) {
  if (isatend(scanner)) return '\0';
  return scanner->current[1];
}

static Token_ match_string(Scanner scanner, char string_token) {
  while (peek(scanner) != string_token && !isatend(scanner)) {
    if (peek(scanner) == '\n') scanner->line++;
    advance(scanner);
  }

  if (isatend(scanner)) return error_token(scanner, "Unterminated string.");

  // The closing quote.
  advance(scanner);
  return make_token(scanner, TK_STRING);
}

static Token_ match_number(Scanner scanner) {
  while (isdigit(peek(scanner))) advance(scanner);

  TokenType info = TK_INTEGER;
  // Look for a fractional part.
  if (peek(scanner) == '.') {
    info = TK_NUMBER;

    // Consume the ".".
    advance(scanner);

    while (isdigit(peek(scanner))) advance(scanner);
  }

  return make_token(scanner, info);
}

static Token_ match_fractional(Scanner scanner) {
  while (isdigit(peek(scanner))) advance(scanner);

  return make_token(scanner, TK_NUMBER);
}

static Token_ match_identifier(Scanner scanner) {
  char c = peek(scanner);
  while (isalpha(c) || isdigit(c) || c == '_') {
    advance(scanner);
    c = peek(scanner);
  }

  return make_token(scanner, identifier_type(scanner));
}

static TokenType identifier_type(Scanner scanner) {
  int length = (int)(scanner->current - scanner->start);

  switch (scanner->start[0]) {
    // and, async, await
    case 'a':
      if (length > 1) {
        switch (scanner->start[1]) {
          case 'n': return check_keyword(scanner, 2, 1, "d", TK_AND);
          case 's':
            if (scanner->current - scanner->start > 2) {
              switch (scanner->start[2]) {
                case 's': return check_keyword(scanner, 3, 3, "ert", TK_ASSERT);
                case 'y': return check_keyword(scanner, 3, 2, "nc", TK_ASYNC);
              }
            }
          case 'w': return check_keyword(scanner, 2, 3, "ait", TK_AWAIT);
        }
      }
      break;

    // bool
    case 'b': return check_keyword(scanner, 1, 3, "ool", TK_BOOL);

    // case, class
    case 'c': 
    {
      if (length > 1) {
        switch (scanner->start[1]) {
        case 'a': return check_keyword(scanner, 2, 2, "se", TK_CASE);
        case 'l': return check_keyword(scanner, 2, 3, "ass", TK_CLASS);
        }
      }
      break;
    }


    // delete, do, double
    case 'd':
    {
      if (length > 1) {
        switch (scanner->start[1]) {
          case 'e': return check_keyword(scanner, 2, 4, "lete", TK_DELETE);
          case 'o':
            if (length == 2) {
              return TK_DO;
            } else {
              return check_keyword(scanner, 2, 4, "uble", TK_DOUBLE);
            }
        }            
      }
      break;
    }

    // elif, else, end
    case 'e':
      if (length > 1) {
        switch (scanner->start[1]) {
          case 'l':
            if (scanner->current - scanner->start > 2) {
              switch (scanner->start[2]) {
                case 'i': return check_keyword(scanner, 3, 1, "f", TK_ELIF);
                case 's': return check_keyword(scanner, 3, 1, "e", TK_ELSE);
              }
            }
          case 'n': return check_keyword(scanner, 2, 1, "d", TK_END);
        }
      }
      break;

    // false, float, for, function
    case 'f':
      if (length > 1) {
        switch (scanner->start[1]) {
          case 'a': return check_keyword(scanner, 2, 3, "lse", TK_FALSE);
          case 'l': return check_keyword(scanner, 2, 3, "oat", TK_FLOAT);
          case 'o': return check_keyword(scanner, 2, 1, "r", TK_FOR);
          case 'u': return check_keyword(scanner, 2, 6, "nction", TK_FUNCTION);
        }
      }
      break;

    // if, in, int
    case 'i':
    {
      if (length > 1) {
        switch (scanner->start[1]) {
          case 'f': return TK_IF;
          case 'n':
          {
            if (length == 2) {
              return TK_IN;
            } else {
              // TODO: match integer width
              return check_keyword(scanner, 2, 1, "t", TK_INT);
            }
            break;
          }
        }
      }
      break;
    }

    // let
    case 'l': return check_keyword(scanner, 1, 2, "et", TK_LET);

    // match
    case 'm': return check_keyword(scanner, 1, 4, "atch", TK_MATCH);

    // new, nil, not
    case 'n':
      if (length > 1) {
        switch (scanner->start[1]) {
          case 'e': return check_keyword(scanner, 2, 1, "w", TK_NEW);
          case 'i': return check_keyword(scanner, 2, 1, "l", TK_NIL);
          case 'o': return check_keyword(scanner, 2, 1, "t", TK_NOT);
        }
      }
      break;

    // or
    case 'o': return check_keyword(scanner, 1, 1, "r", TK_OR);

    // pass
    case 'p':
      if (length > 1) {
        switch (scanner->start[1]) {
          case 'a': return check_keyword(scanner, 2, 2, "ss", TK_PASS);
          case 'r': return check_keyword(scanner, 2, 3, "int", TK_PRINT);
        }
      }
      break;

    // repeat, return
    case 'r':
      if (length > 2) {
        if (scanner->start[1] == 'e') {
          switch (scanner->start[2]) {
              case 'p': return check_keyword(scanner, 3, 3, "eat", TK_REPEAT);
              case 't': return check_keyword(scanner, 3, 3, "urn", TK_RETURN);
          }
        }
      }
      break;

    // step, string
    case 's':
      if (length > 2) {
        if (scanner->start[1] == 't') {
          switch (scanner->start[2]) {
            case 'e': return check_keyword(scanner, 3, 1, "p", TK_STEP);
            case 'r':
              if (scanner->current - scanner->start > 3) {
                switch (scanner->start[3]) {                  
                  case 'i': return check_keyword(scanner, 4, 2, "ng", TK_STRING_TYPE);
                }
              }
          }
        }
      }
      break;

    // then, true
    case 't':
      if (length > 1) {
        switch (scanner->start[1]) {
          case 'h': return check_keyword(scanner, 2, 2, "en", TK_THEN);
          case 'r': return check_keyword(scanner, 2, 2, "ue", TK_TRUE);
        }
      }
      break;

    // uint, until
    case 'u':
      if (length > 1) {
        switch (scanner->start[1]) {
          // TODO: match integer width
          case 'i': return check_keyword(scanner, 2, 2, "nt", TK_UINT);
          case 'n': return check_keyword(scanner, 2, 3, "til", TK_UNTIL);
        }
      }
      break;

    // while
    case 'w': return check_keyword(scanner, 1, 4, "hile", TK_WHILE);

    // xor
    case 'x': return check_keyword(scanner, 1, 2, "or", TK_XOR);

    // yield
    case 'y': return check_keyword(scanner, 1, 4, "ield", TK_YIELD);

    // List
    case 'L': return check_keyword(scanner, 1, 3, "ist", TK_LIST);

    // Map
    case 'M': return check_keyword(scanner, 1, 2, "ap", TK_MAP);

    // Set
    case 'S': return check_keyword(scanner, 1, 2, "et", TK_SET);
  }

  return TK_ID;
}

static TokenType check_keyword(Scanner scanner, int start, int length,
  const char* rest, TokenType info) {
  if (scanner->current - scanner->start == start + length &&
    memcmp(scanner->start + start, rest, length) == 0) {
    return info;
  }

  return TK_ID;
}
