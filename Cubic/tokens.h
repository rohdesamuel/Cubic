#ifndef TOKENS__H
#define TOKENS__H

#include "common.h"

typedef enum {
  TK_EOF,
  TK_ERR,
  TK_ID,

  // Single-character tokens.
  TK_LPAREN=10, TK_RPAREN,
  TK_LBRACKET, TK_RBRACKET,
  TK_LBRACE, TK_RBRACE,
  TK_SEMICOLON, TK_COLON,
  TK_DOT, TK_COMMA,
  TK_PLUS, TK_MINUS,
  TK_AMPERSAND, TK_PIPE, TK_HAT, TK_TILDE,
  TK_STAR, TK_PERCENT, TK_QUESTION,

  // Multi-character tokens.
  TK_BANG, TK_BANG_EQUAL,
  TK_EQUAL, TK_EQUAL_EQUAL,
  TK_GT, TK_GTE, TK_RSHIFT,
  TK_LT, TK_LTE, TK_LSHIFT,
  TK_ARROW, TK_FAT_ARROW,
  TK_SLASH, TK_DOUBLE_SLASH,
  TK_DOUBLE_DOT, TK_TRIPLE_DOT,
  TK_QQ, TK_QQE,

  // Literals.
  TK_STRING, TK_INTEGER, TK_NUMBER,

  //Keywords.
  TK_DO, TK_END,
  TK_IF, TK_THEN, TK_ELIF, TK_ELSE,
  TK_FOR, TK_IN, TK_STEP, TK_WHILE,
  TK_REPEAT, TK_UNTIL,
  TK_CLASS,
  TK_MATCH, TK_CASE,
  TK_FUNCTION, TK_RETURN,
  TK_PRINT,  // TODO: Remove the print keyword and replace with a function call.
  TK_AND, TK_OR, TK_XOR, TK_NOT,
  TK_TRUE, TK_FALSE, TK_PASS,
  TK_VAL, TK_VAR, TK_REF, TK_NIL,
  TK_NEW, TK_DEL,
  TK_ASSERT,

  // Types
  TK_BOOL,
  TK_INT, TK_UINT,
  TK_FLOAT, TK_DOUBLE,
  TK_STRING_TYPE,
  TK_LIST, TK_MAP, TK_SET,

  // Async Tokens
  TK_ASYNC, TK_AWAIT, TK_YIELD,

  __TK_COUNT__
} TokenType;

typedef struct Token_ {
  TokenType type;
  const char* start;
  int length;
  int line;
} Token_, * Token;

bool token_eq(Token_ a, Token_ b);

#endif  // TOKENS__H