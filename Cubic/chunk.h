#ifndef CHUNK__H
#define CHUNK__H

#include "common.h"
#include "value.h"

typedef enum {
  OP_CONSTANT,
  OP_CONSTANT_LONG,  // Unimplemented.
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_FADD,
  OP_FSUB,
  OP_FMUL,
  OP_FDIV,

  OP_MOD,  // Unimplemented.
  OP_BITWISE_AND,  // Unimplemented.
  OP_BITWISE_OR,  // Unimplemented.
  OP_BITWISE_XOR,  // Unimplemented.
  OP_LSHIFT,  // Unimplemented.
  OP_RSHIFT,  // Unimplemented.

  OP_AND,  // Unimplemented.
  OP_OR,  // Unimplemented.
  OP_XOR,  // Unimplemented.

  OP_NOT,  // Unimplemented.

  OP_NEGATE,
  OP_RETURN,
} OpCode;

typedef struct Chunk_ {
  int count;
  int capacity;
  uint8_t* code;

  ValueArray_ constants;
  int* lines;

} Chunk_, *Chunk;

void chunk_init(Chunk chunk);
void chunk_free(Chunk chunk);
void chunk_write(Chunk chunk, uint8_t byte, int line);
void chunk_writeconstant(Chunk chunk, Value_ value, int line);
int chunk_addconstant(Chunk chunk, Value_ value);

#endif  // CHUNK__H