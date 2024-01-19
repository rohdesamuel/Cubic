#ifndef ANALYZER__H
#define ANALYZER__H

#include "common.h"
#include "memory.h"
#include "errors.h"

typedef struct Analyzer_ {
  struct Ast_* ast;

  MemoryAllocator_* allocator;
  struct Scope_* scope;
  struct Frame_* frame;

  bool had_error;
  bool panic_mode;
} Analyzer_;

extern thread_local ErrorsContainer_ analyzer_errors;

void analyzer_init(Analyzer_* analyzer, MemoryAllocator_* allocator);
void analyze(Analyzer_* analyzer, struct AstProgram_* ast);
void analyzer_clear(Analyzer_* analyzer);

#endif  // ANALYZER__H