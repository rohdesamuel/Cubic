#include "vm.h"

#include "common.h"
#include "debug.h"
#include "compiler.h"

#include <string.h>
#include <stdarg.h>
#include <stdio.h>

static InterpretResult run(VM vm);
static void reset_stack(VM vm);
static InterpretResult runtime_error(VM_* vm, const char* format, ...);

void vm_init(VM_* vm) {
  memset(vm, 0, sizeof(VM_));
  reset_stack(vm);
}

void vm_free(VM_* vm) {

}

InterpretResult vm_interpret(VM vm, const char* source) {
  Chunk_ chunk;
  chunk_init(&chunk);
  vm_init(vm);
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

inline InterpretResult add(VM_* vm) {
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

static bool is_falsey(Value_ value) {
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
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

#define FBINARY_OP(op) \
    do { \
      double b = vm_pop(vm); \
      double a = vm_pop(vm); \
      vm_push(vm, a op b); \
    } while (false)

#define READ_SHORT() \
    (vm->ip += 2, (uint16_t)((vm->ip[-2] << 8) | vm->ip[-1]))

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
        continue;
      }

      case OP_NIL:
        vm_push(vm, NIL_VAL);
        continue;

      case OP_TRUE:
        vm_push(vm, TRUE_VAL);
        continue;

      case OP_FALSE:
        vm_push(vm, FALSE_VAL);
        continue;

      case OP_GET_VAR:
      {
        uint8_t slot = READ_BYTE();
        vm_push(vm, vm->stack[slot]);
        continue;
      }

      case OP_SET_VAR:
      {
        uint8_t slot = READ_BYTE();
        vm->stack[slot] = vm_peek(vm, 0);
        continue;
      }

      // TODO: allow for different integer widths for operands.
      case OP_BITWISE_AND:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '&' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);
        assertf(ISA_INTEGER(l), "Operands are not integers.");

        switch (l.type) {
          case VAL_INT: vm_push(vm, INT_VAL(l.as.i & r.as.i)); break;
          case VAL_UINT: vm_push(vm, UINT_VAL(l.as.u & r.as.u)); break;
          default: assertf(false, "Unimplemented '&' for type: %d\n", l.type);
        }
        continue;
      }

      // TODO: allow for different integer widths for operands.
      case OP_BITWISE_OR:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '|' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);
        assertf(ISA_INTEGER(l), "Operands are not integers.");

        switch (l.type) {
          case VAL_INT: vm_push(vm, INT_VAL(l.as.i | r.as.i)); break;
          case VAL_UINT: vm_push(vm, UINT_VAL(l.as.u | r.as.u)); break;
          default: assertf(false, "Unimplemented '|' for type: %d\n", l.type);
        }
        continue;
      }

      // TODO: allow for different integer widths for operands.
      case OP_BITWISE_XOR:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '^' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);
        assertf(ISA_INTEGER(l), "Operands are not integers.");

        switch (l.type) {
          case VAL_INT: vm_push(vm, INT_VAL(l.as.i ^ r.as.i)); break;
          case VAL_UINT: vm_push(vm, UINT_VAL(l.as.u ^ r.as.u)); break;
          default: assertf(false, "Unimplemented '^ for type: %d\n", l.type);
        }
        continue;
      }

      case OP_BITWISE_NOT:
      {
        Value_ x = vm_pop(vm);
        assertf(ISA_INTEGER(x), "Operand is not an integer.");
        x.as.u = ~x.as.u;
        vm_push(vm, x);
        continue;
      }

      case OP_NOT:
      {
        Value_ x = vm_pop(vm);
        assertf(IS_BOOL(x), "Operand is not a boolean.");
        x.as.b = !x.as.b;
        vm_push(vm, x);
        continue;
      }

      case OP_LSHIFT:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '<<' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);
        assertf(ISA_INTEGER(l), "Operands are not integers.");

        switch (l.type) {
          case VAL_INT: vm_push(vm, INT_VAL(l.as.i << r.as.i)); break;
          case VAL_UINT: vm_push(vm, UINT_VAL(l.as.u << r.as.u)); break;
          default: assertf(false, "Unimplemented << for type: %d\n", l.type);
        }
        continue;
      }

      case OP_RSHIFT:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '>>' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);
        assertf(ISA_INTEGER(l), "Operands are not integers.");

        switch (l.type) {
          case VAL_INT: vm_push(vm, INT_VAL(l.as.i >> r.as.i)); break;
          case VAL_UINT: vm_push(vm, UINT_VAL(l.as.u >> r.as.u)); break;
          default: assertf(false, "Unimplemented >> for type: %d\n", l.type);
        }
        continue;
      }

      case OP_GT: 
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '>' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);

        switch (l.type) {
          case VAL_DOUBLE: vm_push(vm, BOOL_VAL(l.as.d > r.as.d)); break;
          case VAL_INT: vm_push(vm, BOOL_VAL(l.as.i > r.as.i)); break;
          case VAL_UINT: vm_push(vm, BOOL_VAL(l.as.u > r.as.u)); break;
          default: assertf(false, "Unimplemented > for type: %d\n", l.type);
        }
        continue;
      }
      
      case OP_GTE: 
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '>=' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);

        switch (l.type) {
          case VAL_DOUBLE: vm_push(vm, BOOL_VAL(l.as.d >= r.as.d)); break;
          case VAL_INT: vm_push(vm, BOOL_VAL(l.as.i >= r.as.i)); break;
          case VAL_UINT: vm_push(vm, BOOL_VAL(l.as.u >= r.as.u)); break;
          default: assertf(false, "Unimplemented >= for type: %d\n", l.type);
        }
        continue;
      }
      
      case OP_LT: 
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '<' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);

        switch (l.type) {
          case VAL_DOUBLE: vm_push(vm, BOOL_VAL(l.as.d < r.as.d)); break;
          case VAL_INT: vm_push(vm, BOOL_VAL(l.as.i < r.as.i)); break;
          case VAL_UINT: vm_push(vm, BOOL_VAL(l.as.u < r.as.u)); break;
          default: assertf(false, "Unimplemented < for type: %d\n", l.type);
        }
        continue;
      }
      
      case OP_LTE:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '<=' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);

        switch (l.type) {
          case VAL_DOUBLE: vm_push(vm, BOOL_VAL(l.as.d <= r.as.d)); break;
          case VAL_INT: vm_push(vm, BOOL_VAL(l.as.i <= r.as.i)); break;
          case VAL_UINT: vm_push(vm, BOOL_VAL(l.as.u <= r.as.u)); break;
          default: assertf(false, "Unimplemented <= for type: %d\n", l.type);
        }
        continue;
      }

      case OP_EQ:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '==' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);
        
        vm_push(vm, BOOL_VAL(l.as.u == r.as.u));
        continue;
      }

      case OP_NEQ:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '!=' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);

        vm_push(vm, BOOL_VAL(l.as.u != r.as.u));
        continue;
      }

      case OP_AND:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to 'and' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);
        assertf(IS_BOOL(l), "Operands are not boolean types.");
        vm_push(vm, BOOL_VAL(l.as.b && r.as.b));
        continue;
      }

      case OP_OR:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to 'or' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);
        assertf(IS_BOOL(l), "Operands are not boolean types.");
        vm_push(vm, BOOL_VAL(l.as.b || r.as.b));
        continue;
      }

      case OP_XOR:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to 'xor' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);
        assertf(IS_BOOL(l), "Operands are not boolean types.");
        vm_push(vm, BOOL_VAL(l.as.b != r.as.b));
        continue;
      }

      case OP_MOD:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '%%' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);
        assertf(ISA_INTEGER(r), "Operands are not integer types.");
        if (r.as.i == 0) {
          return runtime_error(vm, "Divide-by-zero error");
        }

        switch (l.type) {
          case VAL_INT: vm_push(vm, INT_VAL(l.as.i % r.as.i)); break;
          case VAL_UINT: vm_push(vm, UINT_VAL(l.as.u % r.as.u)); break;
          default: return runtime_error(vm, "Unimplemented '%%' for type: %d\n", l.type);
        }
        continue;
      }

      case OP_ADD:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '+' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);

        switch (l.type) {
          case VAL_INT: vm_push(vm, INT_VAL(l.as.i + r.as.i)); break;
          case VAL_UINT: vm_push(vm, UINT_VAL(l.as.u + r.as.u)); break;
          case VAL_DOUBLE: vm_push(vm, DOUBLE_VAL(l.as.d + r.as.d)); break;
          default: return runtime_error(vm, "Unimplemented '+' for type: %d\n", l.type);
        }
        continue;
      }

      case OP_SUB:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '-' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);

        switch (l.type) {
          case VAL_INT: vm_push(vm, INT_VAL(l.as.i - r.as.i)); break;
          case VAL_UINT: vm_push(vm, UINT_VAL(l.as.u - r.as.u)); break;
          case VAL_DOUBLE: vm_push(vm, DOUBLE_VAL(l.as.d - r.as.d)); break;
          default: return runtime_error(vm, "Unimplemented binary addition for type: %d\n", l.type);
        }
        continue;
      }

      // case OP_SUB: BINARY_OP(-); break;
      case OP_MUL:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '*' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);

        switch (l.type) {
          case VAL_INT: vm_push(vm, INT_VAL(l.as.i * r.as.i)); break;
          case VAL_UINT: vm_push(vm, UINT_VAL(l.as.u * r.as.u)); break;
          case VAL_DOUBLE: vm_push(vm, DOUBLE_VAL(l.as.d * r.as.d)); break;
          default: return runtime_error(vm, "Unimplemented '*' for type: %d\n", l.type);
        }
        continue;
      }
      // case OP_DIV: BINARY_OP(/); break;
      
      // TODO: allow for integer division conversion to floats.
      case OP_DIV:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        assertf(l.type == r.type,
          "Operands to '/' do not have the same type: left[%d] and right[%d]\n", l.type, r.type);

        if (r.as.i == 0) {
          return runtime_error(vm, "Divide-by-zero error");
        }

        switch (l.type) {
          case VAL_INT: vm_push(vm, INT_VAL(l.as.i / r.as.i)); break;
          case VAL_UINT: vm_push(vm, UINT_VAL(l.as.u / r.as.u)); break;
          case VAL_DOUBLE: vm_push(vm, DOUBLE_VAL(l.as.d / r.as.d)); break;
          default: return runtime_error(vm, "Unimplemented '/' for type: %d\n", l.type);
        }
        continue;
      }

      case OP_NEGATE:
      {
        Value_ val = vm_pop(vm);
        switch (val.type) {
          case VAL_INT: vm_push(vm, INT_VAL(-val.as.i)); break;
          case VAL_DOUBLE: vm_push(vm, DOUBLE_VAL(-val.as.d)); break;
          default: return runtime_error(vm, "Unimplemented unary negation for type: %d", val.type);
        }
        continue;
      }

      case OP_RETURN:
      {
        vm_pop(vm);
        return INTERPRET_OK;
      }

      case OP_PRINT:
      {
        value_print(vm_pop(vm));
        printf("\n");
        continue;
      }

      case OP_ASSERT:
      {
        Value_ assert_val = vm_pop(vm);
        if (is_falsey(assert_val)) {
          runtime_error(vm, "Assertion failed");
          return INTERPRET_ASSERTION_FAILED;
        }
        continue;
      }

      case OP_JMP:
      {
        uint16_t offset = READ_SHORT();
        vm->ip += offset;
        continue;
      }

      case OP_JMP_IF_FALSE:
      {
        uint16_t offset = READ_SHORT();
        if (is_falsey(vm_peek(vm, 0))) {
          vm->ip += offset;
        }
        continue;
      }

      case OP_LOOP:
      {
        uint16_t offset = READ_SHORT();
        vm->ip -= offset;
        break;
      }

      case OP_POP: vm_pop(vm); continue;
    }
  }

#undef READ_SHORT
#undef BINARY_OP
#undef READ_CONSTANT
#undef READ_BYTE
}

static void reset_stack(VM vm) {
  vm->stack_top = vm->stack;
}

static InterpretResult runtime_error(VM_* vm, const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  size_t instruction = vm->ip - vm->chunk->code - 1;
  int line = vm->chunk->lines[instruction];
  fprintf(stderr, "[line %d] in script\n", line);
  reset_stack(vm);

  return INTERPRET_RUNTIME_ERROR;
}