#ifndef SCANNER__H
#define SCANNER__H

#include "common.h"
#include "tokens.h"

class Scanner {
public:
  Scanner(const char* source);

  Token Scan();

private:
  bool Match(char expected);
  char Advance();
  char Peek() const;
  char PeekNext() const;
  bool IsAtEnd() const;
  void Reverse();

  Token SkipWhitespace();
  Token MakeToken(TokenType info) const;
  Token ErrorToken(const char* message) const;

  Token MatchIdentifier();
  Token MatchString(char string_token);
  Token MatchNumber();
  Token MatchInteger();
  Token MatchFractional();

  TokenType IdentifierType();
  TokenType CheckKeyword(int start, int length,
    const char* rest, TokenType info) const;

  const char* start_;
  const char* current_;
  int line_;
};

#endif  // SCANNER__H