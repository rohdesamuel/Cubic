#ifndef COMPILER__H
#define COMPILER__H

#include "common.h"

#include "chunk.h"

bool compile(const char* source, Chunk_* chunk);

#endif  //COMPILER__H