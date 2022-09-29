#include "debug.h"

#include <stdio.h>

static int constant_instruction(const char* name, Chunk chunk, int offset);
static int constant_long_instruction(const char* name, Chunk chunk, int offset);
static int simple_instruction(const char* name, int offset);

void chunk_disassemble(Chunk chunk, const char* name) {
  printf("== %s ==\n", name);

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

  uint8_t instruction = chunk->code[offset];
  switch (instruction) {
    case OP_CONSTANT:
      return constant_instruction("OP_CONSTANT", chunk, offset);
    case OP_CONSTANT_LONG:
      return constant_long_instruction("OP_CONSTANT_LONG", chunk, offset);
    case OP_NEGATE:
      return simple_instruction("OP_NEGATE", offset);
    case OP_ADD:
      return simple_instruction("OP_ADD", offset);
    case OP_SUB:
      return simple_instruction("OP_SUBTRACT", offset);
    case OP_MUL:
      return simple_instruction("OP_MULTIPLY", offset);
    case OP_DIV:
      return simple_instruction("OP_DIVIDE", offset);
    case OP_RETURN:
      return simple_instruction("OP_RETURN", offset);
    default:
      printf("Unknown opcode %d\n", instruction);
      return offset + 1;
  }
}

static int constant_instruction(const char* name, Chunk chunk, int offset) {
  uint8_t constant = chunk->code[offset + 1];
  printf("%-16s %4d '", name, constant);
  value_print(chunk->constants.values[constant]);
  printf("'\n");
  return offset + 2;
}

static int constant_long_instruction(const char* name, Chunk chunk, int offset) {
  int constant =chunk->code[offset + 1]
    | (int)(chunk->code[offset + 2]) << 8
    | (int)(chunk->code[offset + 3]) << 16;

  printf("%-16s %4d '", name, constant);
  value_print(chunk->constants.values[constant]);
  printf("'\n");
  return offset + 4;
}

static int simple_instruction(const char* name, int offset) {
  printf("%s\n", name);
  return offset + 1;
}