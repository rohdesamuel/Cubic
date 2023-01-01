#include "debug.h"
#include "object.h"

#include <stdio.h>

static int constant_instruction(const char* name, Chunk chunk, int offset);
static int constant_long_instruction(const char* name, Chunk chunk, int offset);
static int simple_instruction(const char* name, Chunk chunk, int offset);
static int byte_instruction(const char* name, Chunk chunk, int offset);
static int short_instruction(const char* name, Chunk chunk, int offset);
static int long_instruction(const char* name, Chunk chunk, int offset);
static int jump_instruction(const char* name, Chunk chunk, int offset);
static int loop_instruction(const char* name, Chunk chunk, int offset);
static int cast_instruction(const char* name, Chunk chunk, int offset);

void chunk_disassemble(Chunk chunk, const char* name) {
  //printf("== %s ==\n", name);

  for (int offset = 0; offset < chunk->count;) {
    offset = disassemble_instruction(chunk, offset);
  }
}

int disassemble_instruction(Chunk chunk, int offset) {
  printf("%04d ", offset);
  if (offset > 0 &&
    chunk->lines[offset] == chunk->lines[offset - 1]) {
    printf("   | ");
  } else {
    printf("%4d ", chunk->lines[offset]);
  }

#define DEBUG_INSTRUCTION(OP, DEBUG_INSTR) case OP: return DEBUG_INSTR(#OP, chunk, offset)

  uint8_t instruction = chunk->code[offset];
  switch (instruction) {
    DEBUG_INSTRUCTION(OP_CONSTANT, constant_instruction);
    DEBUG_INSTRUCTION(OP_CONSTANT_LONG, constant_long_instruction);
    DEBUG_INSTRUCTION(OP_NOP, simple_instruction);
    DEBUG_INSTRUCTION(OP_NIL, simple_instruction);
    DEBUG_INSTRUCTION(OP_TRUE, simple_instruction);
    DEBUG_INSTRUCTION(OP_FALSE, simple_instruction);
    DEBUG_INSTRUCTION(OP_EQ, simple_instruction);
    DEBUG_INSTRUCTION(OP_LT, simple_instruction);
    DEBUG_INSTRUCTION(OP_LTE, simple_instruction);
    DEBUG_INSTRUCTION(OP_ADD, simple_instruction);
    DEBUG_INSTRUCTION(OP_SUB, simple_instruction);
    DEBUG_INSTRUCTION(OP_MUL, simple_instruction);
    DEBUG_INSTRUCTION(OP_DIV, simple_instruction);
    DEBUG_INSTRUCTION(OP_MOD, simple_instruction);
    DEBUG_INSTRUCTION(OP_BITWISE_AND, simple_instruction);
    DEBUG_INSTRUCTION(OP_BITWISE_OR, simple_instruction);
    DEBUG_INSTRUCTION(OP_BITWISE_XOR, simple_instruction);
    DEBUG_INSTRUCTION(OP_BITWISE_NOT, simple_instruction);
    DEBUG_INSTRUCTION(OP_LSHIFT, simple_instruction);
    DEBUG_INSTRUCTION(OP_RSHIFT, simple_instruction);
    DEBUG_INSTRUCTION(OP_AND, simple_instruction);
    DEBUG_INSTRUCTION(OP_OR, simple_instruction);
    DEBUG_INSTRUCTION(OP_XOR, simple_instruction);
    DEBUG_INSTRUCTION(OP_NOT, simple_instruction);
    DEBUG_INSTRUCTION(OP_NEG, simple_instruction);
    DEBUG_INSTRUCTION(OP_RETURN, simple_instruction);
    DEBUG_INSTRUCTION(OP_POP, simple_instruction);
    DEBUG_INSTRUCTION(OP_JMP, jump_instruction);
    DEBUG_INSTRUCTION(OP_JMP_IF_FALSE, jump_instruction);
    DEBUG_INSTRUCTION(OP_LOOP, loop_instruction);
    DEBUG_INSTRUCTION(OP_GET_VAR, byte_instruction);
    DEBUG_INSTRUCTION(OP_SET_VAR, byte_instruction);
    DEBUG_INSTRUCTION(OP_DESTROY_VAR, byte_instruction);
    DEBUG_INSTRUCTION(OP_PRINT, simple_instruction);
    DEBUG_INSTRUCTION(OP_ASSERT, simple_instruction);
    DEBUG_INSTRUCTION(OP_CALL, byte_instruction);

    DEBUG_INSTRUCTION(OP_CAST, cast_instruction);

    DEBUG_INSTRUCTION(OP_PROLOGUE, short_instruction);
    DEBUG_INSTRUCTION(OP_EPILOGUE, simple_instruction);

    default:
      printf("Unknown opcode %d\n", instruction);
      return offset + 1;
  }
}

static int constant_instruction(const char* name, Chunk chunk, int offset) {
  uint8_t constant = chunk->code[offset + 1];
  printf("%-16s %4d '", name, constant);
  value_print(chunk->constants.values[constant], INT_TY);
  printf("'\n");
  return offset + 2;
}

static int constant_long_instruction(const char* name, Chunk chunk, int offset) {
  int constant = chunk->code[offset + 1]
    | (int)(chunk->code[offset + 2]) << 8
    | (int)(chunk->code[offset + 3]) << 16;

  printf("%-16s %4d '", name, constant);
  value_print(chunk->constants.values[constant], INT_TY);
  printf("'\n");
  return offset + 4;
}

static int simple_instruction(const char* name, Chunk chunk, int offset) {
  printf("%s\n", name);
  return offset + 1;
}

static int byte_instruction(const char* name, Chunk chunk, int offset) {
  uint8_t slot = chunk->code[offset + 1];
  printf("%-16s %4d\n", name, slot);
  return offset + 2; // [debug]
}

static int short_instruction(const char* name, Chunk chunk, int offset) {
  uint16_t arg = chunk->code[offset + 1] << 8
    | (uint16_t)(chunk->code[offset + 2]);

  printf("%-16s %4d\n", name, arg);
  return offset + 3; // [debug]
}

static int long_instruction(const char* name, Chunk chunk, int offset) {
  uint32_t arg = chunk->code[offset + 1]  << 24
    | (uint32_t)(chunk->code[offset + 2]) << 16
    | (uint32_t)(chunk->code[offset + 3]) << 8
    | (uint32_t)(chunk->code[offset + 4]);

  printf("%-16s %4d\n", name, arg);
  return offset + 5; // [debug]
}

static int jump_instruction(const char* name, Chunk chunk, int offset) {
  uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
  jump |= chunk->code[offset + 2];
  printf("%-16s %4d -> %d\n", name, offset,
    offset + 3 + jump);
  return offset + 3;
}

static int loop_instruction(const char* name, Chunk chunk, int offset) {
  uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
  jump |= chunk->code[offset + 2];
  printf("%-16s %4d -> %d\n", name, offset,
    offset + 3 - jump);
  return offset + 3;
}

static int cast_instruction(const char* name, Chunk chunk, int offset) {
  uint32_t arg = chunk->code[offset + 1] << 24
    | (uint32_t)(chunk->code[offset + 2]) << 16
    | (uint32_t)(chunk->code[offset + 3]) << 8
    | (uint32_t)(chunk->code[offset + 4]);

  RuntimeType_ t = type_fromint(arg);

  printf("%-16s %4d %d %d\n", name, t.ty, t.kind, t.obj);

  return offset + 5;
}