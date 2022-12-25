#ifndef CHUNK__H
#define CHUNK__H

#include "common.h"
#include "value.h"

typedef enum {
  OP_NOP,
  OP_NIL,
  OP_TRUE,
  OP_FALSE,  
  
  OP_CONSTANT,
  OP_CONSTANT_LONG,  // Unimplemented.  

  OP_EQ,
  OP_NEQ,

  OP_GT,
  OP_GTE,
  OP_LT,
  OP_LTE,

  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_IMUL,
  OP_IDIV,

  OP_FADD, // Unimplemented.
  OP_FSUB, // Unimplemented.
  OP_FMUL, // Unimplemented.
  OP_FDIV, // Unimplemented.

  OP_CAST,

  OP_MOD, 
  OP_BITWISE_AND,
  OP_BITWISE_OR,
  OP_BITWISE_XOR,
  OP_BITWISE_NOT,
  OP_LSHIFT,
  OP_RSHIFT,

  OP_AND,
  OP_OR,
  OP_XOR,
  OP_NOT,

  OP_NEGATE,
  OP_RETURN,
  
  OP_POP,
  OP_JMP,
  OP_JMP_IF_FALSE,
  OP_LOOP,

  OP_PROLOGUE,
  OP_EPILOGUE,
  OP_BEGIN_SCOPE,
  OP_END_SCOPE,

  OP_GET_VAR,
  OP_SET_VAR,
  OP_COPY_VAR,

  OP_DESTROY_VAR,  // TODO: turn this into a function call. But maybe a VM operation is faster?

  OP_PRINT,
  OP_ASSERT,

  __OP_CODE_COUNT__
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