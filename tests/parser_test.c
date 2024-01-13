#include "parser.h"

#include "symbol_table.h"
#include "utest.h"

typedef struct ParserTest {
  PageAllocator_ allocator;
  Scanner_ scanner;
  Parser_ parser;
} ParserTest;

UTEST_F_SETUP(ParserTest) {
  pageallocator_init(&utest_fixture->allocator, 1ull << 14);  
  parser_init(&utest_fixture->parser, (MemoryAllocator_*)&utest_fixture->allocator);
}

UTEST_F_TEARDOWN(ParserTest) {}

UTEST_F(ParserTest, EmptySourceCode) {
  Scanner_ scanner;
  scanner_init(&scanner, "");
  
  Frame_* frame = frame_root((MemoryAllocator_*)&utest_fixture->allocator);

  AstNode_* ast = parse(&utest_fixture->parser, &scanner, frame->scope, "");
}

UTEST_F(ParserTest, DoubleDots) {
  Scanner_ scanner;
  scanner_init(&scanner, "");

  Frame_* frame = frame_root((MemoryAllocator_*)&utest_fixture->allocator);

  AstNode_* ast = parse(&utest_fixture->parser, &scanner, frame->scope, "1.. val a := 0");
}