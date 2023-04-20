#ifndef TAC_COMPILER__H
#define TAC_COMPILER__H

#include "common.h"
#include "memory.h"

#include "tac.h"

typedef struct TacChunk_ {  
  int count;
  int capacity;

  Tac_* code;
  int* lines;

  int slot_index;
} TacChunk_;

typedef struct TacCompiler_ {
  struct TacChunk_ chunk;

  PageAllocator_ allocator;
} TacCompiler_;

void tac_compiler_init(TacCompiler_* compiler);
void tac_compiler_compile(TacCompiler_* compiler, struct AstNode_* root);
void tac_compiler_clear(TacCompiler_* compiler);

#endif  // TAC_COMPILER__H