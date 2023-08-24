#ifndef TAC_COMPILER__H
#define TAC_COMPILER__H

#include "common.h"
#include "memory.h"

#include "tac.h"
#include "map.h"

typedef struct TacChunk_ {
  int count;
  int capacity;

  Tac_* code;
  Location_ ret_loc;
  int* lines;

  int label_index;
  int slot_index;
  int slot_offset;

  MemoryAllocator_* allocator;

  Hashmap* fn_code;
} TacChunk_;

typedef struct TacCompiler_ {
  struct TacChunk_ chunk;

  MemoryAllocator_* allocator;
} TacCompiler_;

void tac_compiler_init(TacCompiler_* compiler, MemoryAllocator_* allocator);
void tac_compiler_compile(TacCompiler_* compiler, struct AstNode_* root);
void tac_compiler_clear(TacCompiler_* compiler);

#endif  // TAC_COMPILER__H