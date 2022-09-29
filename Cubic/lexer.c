#include "lexer.h"

#include <inttypes.h>


#if 0
std::string TokenToString(const Token& token) {
  using Type = Token::Type;

  switch (token.type) {
    case Type::TK_EOF:
      return "EOF";
    case Type::TK_UNKNOWN:
      return "UNKNOWN( " + token.str + " )";
    case Type::TK_FUNCTION:
      return "FN( " + token.str + " )";
    case Type::TK_ID:
      return "ID( " + token.str + " )";
    case Type::TK_INTEGER:
      return "INTEGER( " + token.str + " )";
    case Type::TK_NUMBER:
      return "NUMBER( " + token.str + " )";
    case Type::TK_COLON:
      return "COLON";
    case Type::TK_DOT:
      return "DOT";
    case Type::TK_SINGLEQUOTE:
      return "SINGLEQUOTE";
    case Type::TK_DOUBLEQUOTE:
      return "DOUBLEQUOTE";
    case Type::TK_COMMA:
      return "COMMA";
    case Type::TK_LPAREN:
      return "LPAREN";
    case Type::TK_RPAREN:
      return "RPAREN";
    case Type::TK_LBRACKET:
      return "LPAREN";
    case Type::TK_RBRACKET:
      return "RPAREN";
    case Type::TK_EQUALS:
      return "EQUALS";
    case Type::TK_MINUS:
      return "MINUS";
    case Type::TK_PLUS:
      return "PLUS";
    case Type::TK_TIMES:
      return "TIMES";
    case Type::TK_DIVIDE:
      return "DIVIDE";
    case Type::TK_GT:
      return "GT";
    case Type::TK_LT:
      return "LT";
  }

  return "UNKNOWN_TOKEN";
}
#endif


#if 0
class Lexer {
public:
  Lexer(const std::string& contents);
  Token Get();

private:
  // Gets the next character in the string.
  int Next();
  void Swallow();

  const std::string contents_;
  size_t pos_ = 0;
  int last_char_ = ' ';
};

Lexer::Lexer(const std::string& contents) :
  contents_(contents) {}

Token Lexer::Get() {
  while (std::isspace(last_char_)) {
    last_char_ = Next();
  }

  if (std::isalpha(last_char_)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    std::string id;

    do {
      id += last_char_;
      last_char_ = Next();
    } while (isalnum(last_char_) || last_char_ == '_');

    if (id == "fn") {
      return Token{
        .type = Token::Type::TK_FUNCTION,
        .str = std::move(id)
      };
    } else if (id == "struct") {
      return Token{
        .type = Token::Type::TK_STRUCT,
        .str = std::move(id)
      };
    }

    return Token{
      .type = Token::Type::TK_ID,
      .str = std::move(id)
    };
  } else if (isdigit(last_char_)) { // Number: [0-9.]+
    std::string num_str;
    if (isdigit(last_char_)) {
      do {
        num_str += last_char_;
        last_char_ = Next();
      } while (isdigit(last_char_));
    }    

    if (last_char_ == '.') {
      do {
        num_str += last_char_;
        last_char_ = Next();
      } while (isdigit(last_char_));
      return {
        .type = Token::Type::TK_NUMBER,
        .str = num_str
      };
    } else {
      return {
        .type = Token::Type::TK_INTEGER,
        .str = num_str
      };
    }
  } else {
    switch (last_char_) {
      case '.':
      {
        std::string num_str;

        do {
          num_str += last_char_;
          last_char_ = Next();
        } while (isdigit(last_char_));

        if (num_str == ".") {
          return {.type = Token::Type::TK_DOT};
        } else {
          return {
            .type = Token::Type::TK_NUMBER,
            .str = num_str
          };
        }
      }
      case ',':
        Swallow();
        return {.type = Token::TK_COMMA};
      case '\'':
        Swallow();
        return {.type = Token::TK_SINGLEQUOTE};
      case '"':
        Swallow();
        return {.type = Token::TK_DOUBLEQUOTE};

      case '(':
        Swallow();
        return {.type = Token::TK_LPAREN};
      case ')':
        Swallow();
        return {.type = Token::TK_RPAREN};

      case '[':
        Swallow();
        return {.type = Token::TK_LBRACKET};
      case ']':
        Swallow();
        return {.type = Token::TK_RBRACKET};

      case ':':
        Swallow();
        return {.type = Token::TK_COLON};

      case '=':
        Swallow();
        return {.type = Token::TK_EQUALS};
      case '-':
        Swallow();
        return {.type = Token::TK_MINUS};
      case '+':
        Swallow();
        return {.type = Token::TK_PLUS};
      case  '*':
        Swallow();
        return {.type = Token::TK_TIMES};
      case '/':
        Swallow();
        return {.type = Token::TK_DIVIDE};

      case '>':
        Swallow();
        return {.type = Token::TK_GT};
      case '<':
        Swallow();
        return {.type = Token::TK_LT};

      case '#':
      {
        do {
          last_char_ = Next();
        } while (last_char_ != EOF && last_char_ != '\n' && last_char_ != '\r');

        if (last_char_ != EOF) {
          return Get();
        }
      }

      case EOF:
        return {};

      default:
      {
        int last_char = last_char_;
        Swallow();
        return {
          .type = Token::Type::TK_UNKNOWN,
          .str = std::string{(char)last_char}
        };
      }
    }
  }
}

int Lexer::Next() {
  if (pos_ >= contents_.size()) {
    return EOF;
  }

  return contents_[pos_++];
}

void Lexer::Swallow() {
  last_char_ = Next();
}
#endif