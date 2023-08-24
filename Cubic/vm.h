#ifndef VM__H
#define VM__H

#include "common.h"

#include "chunk.h"
#include "native_fns.h"
#include "value.h"

#define FRAMES_MAX 256
#define STACK_MAX 256
#define NATIVE_FNS_MAX 1024

typedef struct CallFrame_ {
  Chunk chunk;
  uint8_t* ip;
  Value_* slots;
  Value_* ret_slot;

  size_t size;
} CallFrame_;

typedef struct VM_ {
  Chunk chunk;
  uint8_t* ip;

  CallFrame_* frames;
  int frame_count;

  // TODO: change this to dynamically grow.
  Value_* stack;
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

InterpretResult vm_runtime_error(VM_* vm, const char* format, ...);

#endif  // VM__H