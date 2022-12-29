#include "vm.h"

#include "common.h"
#include "debug.h"
#include "compiler.h"
#include "symbol.h"
#include "object.h"

#include <string.h>
#include <stdarg.h>
#include <stdio.h>

static InterpretResult run(VM vm);
static void reset_stack(VM vm);
static InterpretResult runtime_error(VM_* vm, const char* format, ...);

void vm_init(VM_* vm) {
  memset(vm, 0, sizeof(VM_));
  vm->frames = calloc(FRAMES_MAX, sizeof(CallFrame_));
  vm->stack = calloc(STACK_MAX, sizeof(Value_));

  reset_stack(vm);
}

void vm_free(VM_* vm) {
  free(vm->frames);
  free(vm->stack);
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
  vm->frame_count = 1;

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

static bool is_falsey(Value_ value) {
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static ObjString_* concatenate(ObjString_* l, ObjString_* r) {
  int length = l->length + r->length;
  char* chars = malloc(length + 1);
  memcpy(chars, l->chars, l->length);
  memcpy(chars + l->length, r->chars, r->length);
  chars[length] = '\0';

  return objstring_create(chars, length);
}

static InterpretResult run(VM vm) {
#define FRAME_MAX 256
  CallFrame_* frame = &vm->frames[0];
  frame->ip = vm->ip;
  frame->chunk = vm->chunk;
  frame->slots = vm->stack;

#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() \
    (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_LONG() \
    (frame->ip += 4, (uint32_t)((frame->ip[-4] << 24) | (frame->ip[-3] << 16) | (frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() (frame->chunk->constants.values[READ_BYTE()])
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

  for (;;) {

//#define DEBUG_TRACE_EXECUTION
#ifdef DEBUG_TRACE_EXECUTION
    printf("\n==== Execution Trace ====");
    printf("          ");
    for (Value slot = vm->stack; slot < vm->stack_top; slot++) {
      printf("[ ");
      value_print(*slot);
      printf(" ]");
    }
    printf("\n");
    disassemble_instruction(frame->chunk, (int)(frame->ip - frame->chunk->code));
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

      case OP_PROLOGUE:
      {
        uint16_t size = READ_SHORT();
        frame->size = size;
        vm->stack_top += frame->size;
        continue;
      }

      case OP_EPILOGUE:
      {
        /*for (int i = 0; i < frame->size; ++i) {
          Value_* v = &frame->slots[i];
          if (IS_OBJ(*v) && v->type.kind == KIND_VAL) {
            obj_destroy(v->as.obj);
          }
        }*/
        vm->stack_top -= frame->size;
        continue;
      }

      case OP_DESTROY_VAR:
      {
        int var_index = READ_BYTE();
        Value_* v = &frame->slots[var_index];
        obj_destroy(v->as.obj);
        continue;
      }

      case OP_CAST:
      {
        Value_ expr_val = vm_pop(vm);
        Type_ from_type = type_fromint(READ_LONG());
        Type_ to_type = type_fromint(READ_LONG());

        switch (to_type.ty) {
          case VAL_FLOAT:
          {
            switch (from_type.ty) {
              case VAL_INT: expr_val.as.f = (float)expr_val.as.i32; break;
              case VAL_UINT: expr_val.as.f = (float)expr_val.as.u32; break;
            }
            break;
          }

          case VAL_DOUBLE:
          {
            switch (from_type.ty) {
              case VAL_INT: expr_val.as.d = (double)expr_val.as.i; break;
              case VAL_UINT: expr_val.as.d = (double)expr_val.as.u; break;
            }
            break;
          }
        }

        expr_val.type = to_type;
        vm_push(vm, expr_val);
        continue;
      }

      case OP_GET_VAR:
      {
        uint8_t slot = READ_BYTE();
        vm_push(vm, frame->slots[slot]);
        continue;
      }

      case OP_SET_VAR:
      {
        uint8_t slot = READ_BYTE();
        Value_ val = vm_peek(vm, 0);
        frame->slots[slot] = val;
        continue;
      }

      // TODO: allow for different integer widths for operands.
      case OP_BITWISE_AND:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        l.as.u = l.as.u & r.as.u;

        vm_push(vm, l);
        continue;
      }

      // TODO: allow for different integer widths for operands.
      case OP_BITWISE_OR:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        l.as.u = l.as.u | r.as.u;

        vm_push(vm, l);
        continue;
      }

      // TODO: allow for different integer widths for operands.
      case OP_BITWISE_XOR:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        l.as.u = l.as.u ^ r.as.u;

        vm_push(vm, l);
        continue;
      }

      case OP_BITWISE_NOT:
      {
        Value_ x = vm_pop(vm);
        x.as.u = ~x.as.u;
        vm_push(vm, x);
        continue;
      }

      case OP_NOT:
      {
        Value_ x = vm_pop(vm);
        x.as.b = !x.as.b;
        vm_push(vm, x);
        continue;
      }

      case OP_LSHIFT:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        l.as.u = l.as.u << r.as.u;

        vm_push(vm, l);
        continue;
      }

      case OP_RSHIFT:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        l.as.u = l.as.u >> r.as.u;

        vm_push(vm, l);
        continue;
      }
     
      case OP_LT: 
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        switch (l.type.ty) {
          case VAL_DOUBLE: vm_push(vm, BOOL_VAL(l.as.d < r.as.d)); break;
          case VAL_INT: vm_push(vm, BOOL_VAL(l.as.i < r.as.i)); break;
          case VAL_UINT: vm_push(vm, BOOL_VAL(l.as.u < r.as.u)); break;
          default: assertf(false, "Unimplemented < for type: %d\n", l.type.ty);
        }
        continue;
      }
      
      case OP_LTE:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        switch (l.type.ty) {
          case VAL_DOUBLE: vm_push(vm, BOOL_VAL(l.as.d <= r.as.d)); break;
          case VAL_INT: vm_push(vm, BOOL_VAL(l.as.i <= r.as.i)); break;
          case VAL_UINT: vm_push(vm, BOOL_VAL(l.as.u <= r.as.u)); break;
          default: assertf(false, "Unimplemented <= for type: %d\n", l.type.ty);
        }
        continue;
      }

      case OP_EQ:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        vm_push(vm, BOOL_VAL(value_equal(&l, &r)));
        continue;
      }

      case OP_AND:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        vm_push(vm, BOOL_VAL(l.as.b && r.as.b));
        continue;
      }

      case OP_OR:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        vm_push(vm, BOOL_VAL(l.as.b || r.as.b));
        continue;
      }

      case OP_XOR:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        vm_push(vm, BOOL_VAL(l.as.b != r.as.b));
        continue;
      }

      case OP_MOD:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        if (r.as.i == 0) {
          return runtime_error(vm, "Divide-by-zero error");
        }

        vm_push(vm, UINT_VAL(l.as.u % r.as.u));
        continue;
      }

      case OP_IMOD:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        if (r.as.i == 0) {
          return runtime_error(vm, "Divide-by-zero error");
        }

        vm_push(vm, INT_VAL(l.as.i % r.as.i));
        continue;
      }

      case OP_ADD:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        vm_push(vm, UINT_VAL(l.as.u + r.as.u));
        continue;
      }

      case OP_CONCAT:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        vm_push(vm, OBJ_VAL(concatenate(AS_STRING(l), AS_STRING(r))));
        continue;
      }

      case OP_FADD:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        vm_push(vm, DOUBLE_VAL(l.as.d + r.as.d));
        continue;
      }

      case OP_SUB:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        vm_push(vm, UINT_VAL(l.as.u - r.as.u));
        continue;
      }

      case OP_FSUB:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        vm_push(vm, DOUBLE_VAL(l.as.d - r.as.d));
        continue;
      }

      case OP_MUL:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        vm_push(vm, UINT_VAL(l.as.u * r.as.u));
        continue;
      }

      case OP_IMUL:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        vm_push(vm, INT_VAL(l.as.i * r.as.i));
        continue;
      }

      case OP_FMUL:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        vm_push(vm, DOUBLE_VAL(l.as.d * r.as.d));
        continue;
      }
      
      // TODO: allow for integer division conversion to floats.
      case OP_DIV:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        if (r.as.i == 0) {
          return runtime_error(vm, "Divide-by-zero error");
        }

        vm_push(vm, UINT_VAL(l.as.u / r.as.u));
        continue;
      }

      case OP_IDIV:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        if (r.as.i == 0) {
          return runtime_error(vm, "Divide-by-zero error");
        }

        vm_push(vm, INT_VAL(l.as.i / r.as.i));
        continue;
      }

      case OP_FDIV:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        if (r.as.i == 0) {
          return runtime_error(vm, "Divide-by-zero error");
        }

        vm_push(vm, DOUBLE_VAL(l.as.d / r.as.d));
        continue;
      }

      case OP_NEG:
      {
        Value_ val = vm_pop(vm);
        vm_push(vm, INT_VAL(-val.as.i));
        continue;
      }

      case OP_FNEG:
      {
        Value_ val = vm_pop(vm);
        vm_push(vm, DOUBLE_VAL(-val.as.d));
        continue;
      }

      case OP_CALL:
      {
        uint8_t num_params = READ_BYTE();
        Value_ obj_val = vm_peek(vm, num_params);

        ObjFunction_* obj_fn = AS_FUNCTION(obj_val);

        frame = &vm->frames[vm->frame_count++];
        frame->chunk = obj_fn->chunk;
        frame->ip = obj_fn->chunk->code;
        frame->slots = vm->stack_top - num_params - 1;
        continue;
      }

      case OP_RETURN:
      {
        Value_ result = vm_pop(vm);        
        if (--vm->frame_count == 0) {
          vm_pop(vm);
          return INTERPRET_OK;
        }

        vm->stack_top = frame->slots;
        vm_push(vm, result);
        frame = &vm->frames[vm->frame_count - 1];
        continue;
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
        frame->ip += offset;
        continue;
      }

      case OP_JMP_IF_FALSE:
      {
        uint16_t offset = READ_SHORT();
        if (is_falsey(vm_peek(vm, 0))) {
          frame->ip += offset;
        }
        continue;
      }

      case OP_LOOP:
      {
        uint16_t offset = READ_SHORT();
        frame->ip -= offset;
        continue;
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

  size_t instruction = vm->frames[vm->frame_count-1].ip - vm->chunk->code - 1;
  int line = vm->chunk->lines[instruction];
  fprintf(stderr, "[line %d] in script\n", line);
  reset_stack(vm);

  return INTERPRET_RUNTIME_ERROR;
}