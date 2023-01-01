#ifndef COMPILER__H
#define COMPILER__H

#include "common.h"

#include "chunk.h"
#include "tokens.h"
#include "value.h"

typedef struct {
  Token_ name;
  RuntimeType_ info;
  int depth;
} Local;

typedef struct Compiler_ {
  Local locals[UINT8_MAX + 1];
  int locals_count;
  int locals_capacity;
  int scope_depth;
} Compiler_;

bool compile(const char* source, Chunk_* chunk);

#endif  //COMPILER__H