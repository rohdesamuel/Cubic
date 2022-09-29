#ifndef DEBUG__H
#define DEBUG__H

#include "chunk.h"

void chunk_disassemble(Chunk chunk, const char* name);
int disassemble_instruction(Chunk chunk, int offset);

#endif  // DEBUG__H