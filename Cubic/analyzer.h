#ifndef ANALYZER__H
#define ANALYZER__H

#include "common.h"
#include "memory.h"

typedef struct AnalyzerError_ {
  const char* error_str;
} AnalyzerError_;

typedef struct AnalyzerErrors_ {
  ListOf_(AnalyzerError_) errors;
  bool panic_mode;
  bool has_errors;

  MemoryAllocator_* allocator;
} AnalyzerErrors_;

typedef struct Analyzer_ {
  struct Ast_* ast;

  MemoryAllocator_* allocator;
  struct Scope_* scope;
  struct Frame_* frame;

  bool had_error;
  bool panic_mode;
} Analyzer_;

extern thread_local AnalyzerErrors_ error_manager;

void analyzer_init(Analyzer_* analyzer, MemoryAllocator_* allocator);
void analyze(Analyzer_* analyzer, struct AstProgram_* ast);
void analyzer_clear(Analyzer_* analyzer);

void analyzererrors_init(AnalyzerErrors_* errors, MemoryAllocator_* allocator);
void analyzererrors_clear(AnalyzerErrors_* errors);

void error_add_(AnalyzerErrors_* errors, int line, const char* format, ...);
void error_panic_(AnalyzerErrors_* errors, int line, const char* format, ...);

#define COMPILE_ERROR(line, format, ...) error_add_(error_manager, line, format, __VA_ARGS__)
#define COMPILE_PANIC(line, format, ...) error_panic_(error_manager, line, format, __VA_ARGS__)

#endif  // ANALYZER__H