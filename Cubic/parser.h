#ifndef PARSER__H
#define PARSER__H

#include "common.h"
#include "ast.h"
#include "scanner.h"
#include "tokens.h"

typedef struct Parser_ {
  Token_ current;
  Token_ previous;

  bool had_error;
  bool panic_mode;

  PageAllocator_ allocator;
} Parser_;

void parser_init(Parser_* parser);
void parser_clear(Parser_* parser);
AstNode_* parse(Parser_* parser, Scanner_* scanner, struct Scope_* scope, const char* source);

#endif  // PARSER__H