#ifndef PARSER__H
#define PARSER__H

#include "common.h"
#include "scanner.h"
#include "tokens.h"
#include "memory.h"
#include "map.h"

typedef struct Parser_ {
  Token_ current;
  Token_ previous;

  bool had_error;
  bool panic_mode;

  MemoryAllocator_* allocator;
} Parser_;

typedef struct AstParser_ {
  bool had_error;
  List_ work_queue;
  Hashmap* /*<Symbol_*, AstNode_*>*/ generic_nodes;
  MemoryAllocator_* allocator;
  struct ErrorsContainer_* errors;
} AstParser_;

void parser_init(Parser_* parser, MemoryAllocator_* allocator);
void parser_clear(Parser_* parser);

void ast_parser_init(AstParser_* parser, MemoryAllocator_* allocator);
void ast_parser_clear(AstParser_* parser);

struct CstNode_* parse_cst(Parser_* parser, Scanner_* scanner, struct Scope_* scope, const char* source);
struct AstNode_* parse_ast(AstParser_* parser, const struct CstNode_* node, struct Scope_* scope);

#endif  // PARSER__H