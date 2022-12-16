#ifndef VM__H
#define VM__H

#include "common.h"

#include "chunk.h"
#include "value.h"

#define FRAMES_MAX 256
#define STACK_MAX 256

typedef struct CallFrame_ {
  Chunk chunk;
  uint8_t* ip;
  Value_* slots;

  size_t size;
} CallFrame_;

typedef struct VM_ {
  Chunk chunk;
  uint8_t* ip;

  CallFrame_ frames[FRAMES_MAX];
  int frame_count;

  // TODO: change this to dynamically grow.
  Value_ stack[STACK_MAX];
  Value_* stack_top;
} VM_, *VM;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR,
  INTERPRET_ASSERTION_FAILED
} InterpretResult;

void vm_init(VM_* vm);
void vm_free(VM_* vm);
InterpretResult vm_interpret(VM vm, const char* source);
void vm_push(VM_* vm, Value_ value);
Value_ vm_pop(VM_* vm);
void vm_popx(VM_* vm, int distance);
Value_ vm_peek(VM_* vm, int distance);

#endif  // VM__H