#include <stdlib.h>

#include "chunk.h"
#include "memory.h"

static inline void chunk_reserve(Chunk chunk, int num_bytes) {
  if (chunk->capacity < chunk->count + num_bytes) {
    int old_capacity = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(old_capacity);
    chunk->code = GROW_ARRAY(uint8_t, chunk->code,
      old_capacity, chunk->capacity);
    chunk->lines = GROW_ARRAY(int, chunk->lines,
      old_capacity, chunk->capacity);
  }
}

void chunk_init(Chunk chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  chunk->lines = NULL;

  valuearray_init(&chunk->constants);
}

void chunk_free(Chunk chunk) {
  FREE_ARRAY(int, chunk->lines, chunk->capacity);
  FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
  valuearray_free(&chunk->constants);
  chunk_init(chunk);
}

void chunk_write(Chunk chunk, uint8_t byte, int line) {
  chunk_reserve(chunk, 1);

  chunk->code[chunk->count] = byte;
  chunk->lines[chunk->count] = line;
  chunk->count++;
}

void chunk_writeconstant(Chunk chunk, Value_ value, int line) {
  int constant = chunk_addconstant(chunk, value);

  if (constant <= 0xFF) {
    chunk_reserve(chunk, 2);
    chunk_write(chunk, OP_CONSTANT, line);
    chunk_write(chunk, (uint8_t)constant, line);
    return;
  } else {
    chunk_reserve(chunk, 4);
    chunk_write(chunk, OP_CONSTANT_LONG, line);
    chunk_write(chunk, constant & 0xFF, line);
    chunk_write(chunk, (constant >> 8) & 0xFF, line);
    chunk_write(chunk, (constant >> 16) & 0xFF, line);
  }
}

int chunk_addconstant(Chunk chunk, Value_ value) {
  valuearray_write(&chunk->constants, value);
  return chunk->constants.count - 1;
}