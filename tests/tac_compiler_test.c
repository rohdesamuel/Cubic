#include "tac_compiler.h"

#include "ast.h"
#include "utest.h"

typedef struct TacCompilerTest {
  PageAllocator_ allocator;
  TacCompiler_ compiler;
} TacCompilerTest;

UTEST_F_SETUP(TacCompilerTest) {
  pageallocator_init(&utest_fixture->allocator, 1ull << 14);
  tac_compiler_init(&utest_fixture->compiler, (MemoryAllocator_*)&utest_fixture->allocator);
}

UTEST_F_TEARDOWN(TacCompilerTest) {}

UTEST_F(TacCompilerTest, EmptySourceCode) {
  //AstBlock_ block = { 0 };
  //AstProgram_ root = {
  //  .block = &block
  //};
  //
  //tac_compiler_compile(&utest_fixture->compiler, (AstNode_*)&root);
}