#include "vm.h"

#include "common.h"
#include "debug.h"
#include "compiler.h"

#include <string.h>
#include <stdarg.h>
#include <stdio.h>

static InterpretResult run(VM vm);
static void reset_stack(VM vm);
static void runtime_error(VM_* vm, const char* format, ...);

void vm_init(VM_* vm) {
  memset(vm, 0, sizeof(VM_));
  reset_stack(vm);
}

void vm_free(VM_* vm) {

}

InterpretResult vm_interpret(VM vm, const char* source) {
  Chunk_ chunk;
  chunk_init(&chunk);

  if (!compile(source, &chunk)) {
    chunk_free(&chunk);
    return INTERPRET_COMPILE_ERROR;
  }

  vm->chunk = &chunk;
  vm->ip = vm->chunk->code;

  InterpretResult result = run(vm);

  chunk_free(&chunk);
  return result;
}

void vm_push(VM_* vm, Value_ value) {
  *vm->stack_top = value;
  ++vm->stack_top;
}

Value_ vm_pop(VM_* vm) {
  --vm->stack_top;
  return *vm->stack_top;
}

void vm_popx(VM_* vm, int distance) {
  vm->stack_top -= distance;
}

Value_ vm_peek(VM_* vm, int distance) {
  return vm->stack_top[-1 - distance];
}

inline InterpretResult addi(VM_* vm) {
  Value_ a = vm_peek(vm, 0);
  Value_ b = vm_peek(vm, 1);

  if (a.type != b.type || !ISA_INT(a)) {
    runtime_error(vm, "Operands must be integers");
    return INTERPRET_RUNTIME_ERROR;
  }

  vm_popx(vm, 2);
  vm_push(vm, INT_VAL(a.as.i + b.as.i));

  return INTERPRET_OK;
}

inline InterpretResult addu(VM_* vm) {
  Value_ a = vm_peek(vm, 0);
  Value_ b = vm_peek(vm, 1);

  if (a.type != b.type || !ISA_UINT(a)) {
    runtime_error(vm, "Operands must be integers");
    return INTERPRET_RUNTIME_ERROR;
  }

  vm_popx(vm, 2);
  vm_push(vm, UINT_VAL(a.as.u + b.as.u));

  return INTERPRET_OK;
}

#define INTEGER_OP(TYPE, OP) do {\
Value_ a = vm_peek(vm, 0);\
Value_ b = vm_peek(vm, 1);\
if (a.type != b.type || !ISA_##TYPE(a)) {\
  runtime_error(vm, "Operands must be integers");\
  return INTERPRET_RUNTIME_ERROR;\
}\
vm_popx(vm, 2);\
vm_push(vm, TYPE##_VAL(a.as.u OP b.as.u));\
return INTERPRET_OK;\
} while(0)


inline InterpretResult addf(VM_* vm) {
  Value_ a = vm_peek(vm, 0);
  Value_ b = vm_peek(vm, 1);

  INTEGER_OP(INT, +);

  if (a.type != b.type || !ISA_NUMBER(a)) {
    runtime_error(vm, "Operands must be numbers");
    return INTERPRET_RUNTIME_ERROR;
  }

  vm_popx(vm, 2);
  switch (a.type) {
    case VAL_DOUBLE: vm_push(vm, DOUBLE_VAL(a.as.d + b.as.d)); break;
    case VAL_FLOAT:  vm_push(vm, FLOAT_VAL(a.as.f + b.as.f)); break;
  }

  return INTERPRET_OK;
}

static InterpretResult run(VM vm) {
#define READ_BYTE() (*vm->ip++)
#define READ_CONSTANT() (vm->chunk->constants.values[READ_BYTE()])
#define BINARY_OP(op) \
    do { \
      double b = vm_pop(vm); \
      double a = vm_pop(vm); \
      vm_push(vm, a op b); \
    } while (false)
  
  for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
    printf("\n==== Execution Trace ====");
    printf("          ");
    for (Value slot = vm->stack; slot < vm->stack_top; slot++) {
      printf("[ ");
      value_print(*slot);
      printf(" ]");
    }
    printf("\n");
    disassemble_instruction(vm->chunk, (int)(vm->ip - vm->chunk->code));
#endif  // DEBUG_TRACE_EXECUTION

    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
      case OP_CONSTANT:
      {
        Value_ constant = READ_CONSTANT();
        vm_push(vm, constant);
        break;
      }
      case OP_ADD:
      {
        Value_ a = vm_pop(vm);
        Value_ b = vm_pop(vm);
        assertf(a.type == b.type,
          "Operands to add do not have the same type left[%d] and right[%d]\n", a.type, b.type);

        switch (a.type) {
          case VAL_INT: vm_push(vm, INT_VAL(a.as.i + b.as.i)); break;
          case VAL_UINT: vm_push(vm, UINT_VAL(a.as.u + b.as.u)); break;
          default: assertf(false, "Unimplemented binary addition for type: %d\n", a.type);
        }
        break;
      }

      case OP_FADD:
      {
        Value_ a = vm_pop(vm);
        Value_ b = vm_pop(vm);
        assertf(a.type == b.type,
          "Operands to add do not have the same type left[%d] and right[%d]\n", a.type, b.type);

        switch (a.type) {
          case VAL_FLOAT: vm_push(vm, FLOAT_VAL(a.as.f + b.as.f)); break;
          case VAL_DOUBLE: vm_push(vm, DOUBLE_VAL(a.as.d + b.as.d)); break;
          default: assertf(false, "Unimplemented binary addition for type: %d\n", a.type);
        }
        break;
      }

      // case OP_SUB: BINARY_OP(-); break;
      case OP_MUL:
      {
        Value_ a = vm_pop(vm);
        Value_ b = vm_pop(vm);
        assertf(a.type == b.type,
          "Operands to add do not have the same type left[%d] and right[%d]\n", a.type, b.type);

        switch (a.type) {
          case VAL_INT: vm_push(vm, INT_VAL(a.as.i * b.as.i)); break;
          case VAL_DOUBLE: vm_push(vm, DOUBLE_VAL(a.as.d * b.as.d)); break;
          default: assertf(false, "Unimplemented binary addition for type: %d\n", a.type);
        }
        break;
      }
      // case OP_DIV: BINARY_OP(/); break;

      case OP_NEGATE:
      {
        Value_ val = vm_pop(vm);
        switch (val.type) {
          case VAL_INT: vm_push(vm, INT_VAL(-val.as.i)); break;
          case VAL_DOUBLE: vm_push(vm, DOUBLE_VAL(-val.as.d)); break;
          default: assertf(false, "Unimplemented unary negation for type: %d", val.type);
        }
        break;
      }
      case OP_RETURN:
      {
        value_print(vm_pop(vm));
        printf("\n");
        return INTERPRET_OK;
      }
    }
  }

#undef BINARY_OP
#undef READ_CONSTANT
#undef READ_BYTE
}

static void reset_stack(VM vm) {
  vm->stack_top = vm->stack;
}

static void runtime_error(VM_* vm, const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  size_t instruction = vm->ip - vm->chunk->code - 1;
  int line = vm->chunk->lines[instruction];
  fprintf(stderr, "[line %d] in script\n", line);
  reset_stack(vm);
}