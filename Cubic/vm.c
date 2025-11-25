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
  return value.as.u == 0;
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
#define READ_LONGLONG() \
    (frame->ip += 8, (uint64_t)(((uint64_t)frame->ip[-8] << 56) | ((uint64_t)frame->ip[-7] << 48) | ((uint64_t)frame->ip[-6] << 40) | ((uint64_t)frame->ip[-5] << 32) | \
                                ((uint64_t)frame->ip[-4] << 24) | ((uint64_t)frame->ip[-3] << 16) | ((uint64_t)frame->ip[-2] <<  8) | (uint64_t)frame->ip[-1]))

#define READ_CONSTANT() (frame->chunk->constants.values[READ_BYTE()])

#define UNARY_OP(OP) \
    do { \
      uint8_t dst = READ_BYTE(); \
      uint8_t src = READ_BYTE(); \
      frame->slots[dst].as.u = OP frame->slots[src].as.u; \
    } while (0)

#define UNARY_TYPED_OP(OP, TY) \
    do { \
      uint8_t dst = READ_BYTE(); \
      uint8_t src = READ_BYTE(); \
      frame->slots[dst].as.TY = OP frame->slots[src].as.TY; \
    } while (0)

#define BINARY_OP(OP) \
    do { \
      uint8_t dst = READ_BYTE(); \
      uint8_t l = READ_BYTE(); \
      uint8_t r = READ_BYTE(); \
      frame->slots[dst].as.u = frame->slots[l].as.u OP frame->slots[r].as.u; \
    } while (0)

#define BINARY_TYPED_OP(OP, TY) \
    do { \
      uint8_t dst = READ_BYTE(); \
      uint8_t l = READ_BYTE(); \
      uint8_t r = READ_BYTE(); \
      frame->slots[dst].as.TY = frame->slots[l].as.TY OP frame->slots[r].as.TY; \
    } while (0)

  for (;;) {
//#define DEBUG_TRACE_EXECUTION
#ifdef DEBUG_TRACE_EXECUTION
    //printf("\n==== Execution Trace ====");
    //printf("          ");
    /*for (Value slot = vm->stack; slot < vm->stack_top; slot++) {
      printf("[ ");
      value_print(*slot);
      printf(" ]");
    }*/
    //printf("\n");
    //disassemble_instruction(frame->chunk, (int)(frame->ip - frame->chunk->code));
    printf("%d\t%s\n", (int)(frame->ip - frame->chunk->code), OPCODE_STRING[*frame->ip]);
#endif  // DEBUG_TRACE_EXECUTION
    //printf("%llX\n", (uint64_t)vm->stack_top);
    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
      case OP_NIL:
      {
        uint8_t dst = READ_BYTE();
        frame->slots[dst] = NIL_VAL;
        continue;
      }

      case OP_CALL:
      {
        uint32_t jump_dst = READ_LONG();
        uint8_t ret_dst = READ_BYTE();
        uint32_t param_size = READ_LONG();

        CallFrame_* new_frame = &vm->frames[vm->frame_count++];
        new_frame->ip = frame->chunk->code + jump_dst;
        new_frame->chunk = frame->chunk;
        new_frame->slots = &frame->slots[ret_dst];
        new_frame->ret_slot = &frame->slots[ret_dst];
        frame = new_frame;

        continue;
      }

      case OP_RETURN:
      {
        //Value_ result = vm_pop(vm);
        uint8_t ret_src = READ_BYTE();
        vm->stack_top -= frame->size;
        if (--vm->frame_count == 0) {
          //vm_pop(vm);
          return INTERPRET_OK;
        }

        //vm_push(vm, result);
        //*frame->ret_slot = frame->slots[ret_src];
        frame = &vm->frames[vm->frame_count - 1];
        continue;
      }

      case OP_PROLOGUE:
      {
        uint32_t frame_size = READ_LONG();
        uint32_t param_size = READ_LONG();
        frame->size = frame_size;// +param_size;
        //frame->slots -= param_size;
        vm->stack_top += frame->size;
        continue;
      }

      case OP_JMP:
      {
        uint32_t jump_dst = READ_LONG();
        frame->ip = frame->chunk->code + jump_dst;
        continue;
      }

      case OP_JMP_IF_FALSE:
      {
        uint32_t jump_dst = READ_LONG();
        uint8_t condition = READ_BYTE();
        if (!frame->slots[condition].as.u) {
          frame->ip = frame->chunk->code + jump_dst;
        }
        continue;
      }

      case OP_FALSE:
      {
        uint8_t dst = READ_BYTE();
        frame->slots[dst] = FALSE_VAL;
        continue;
      }

      case OP_TRUE:
      {
        uint8_t dst = READ_BYTE();
        frame->slots[dst] = TRUE_VAL;
        continue;
      }

      case OP_CONSTANT:
      {
        uint8_t dst = READ_BYTE();
        Value_ constant = READ_CONSTANT();
        frame->slots[dst] = constant;
        continue;
      }

      case OP_MOVE:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        frame->slots[dst] = frame->slots[src];
        continue;
      }

      case OP_LEA:
      {
        uint8_t dst = READ_BYTE();
        uint8_t offset = READ_BYTE();
        uint64_t size = READ_LONGLONG();
        frame->slots[dst].as.ptr = frame->slots[dst].as.ptr + frame->slots[offset].as.u * size;
        continue;
      }

      case OP_MEMCPY:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        uint8_t size = READ_BYTE();
        memcpy((void*)frame->slots[dst].as.ptr, (void*)frame->slots[src].as.ptr, sizeof(Value_) * size);
        continue;
      }

      case OP_LOAD:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        uint8_t offset = READ_BYTE();
        frame->slots[dst] = *((Value_*)(frame->slots[src].as.ptr) + offset);
        continue;
      }

      case OP_LOADA:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        uint8_t offset = READ_BYTE();
        frame->slots[dst].as.ptr = (uintptr_t)(&frame->slots[src] + offset);
        continue;
      }

      case OP_STORE:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        uint8_t offset = READ_BYTE();
        *((Value_*)(frame->slots[dst].as.ptr) + offset) = frame->slots[src];
        continue;
      }

      case OP_RLOAD:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        uint8_t offset = READ_BYTE();
        frame->slots[dst] = *(Value_*)(frame->slots[src].as.ref.pval + offset);
        continue;
      }

      case OP_RLOADA:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        uint8_t offset = READ_BYTE();
        frame->slots[dst].as.ptr = (uintptr_t)(frame->slots[src].as.ref.pval + offset);
        continue;
      }

      case OP_RSTORE:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        uint8_t offset = READ_BYTE();
        *(frame->slots[dst].as.ref.pval + offset) = frame->slots[src];
        continue;
      }

      case OP_REF_MAKE:
      {
        uint8_t dst = READ_BYTE();
        size_t size = READ_LONGLONG();
        Value_ ret = {
          .as.ref = {
            .pval = calloc(size, sizeof(Value_)),
            .count = calloc(1, sizeof(int))
          },
        };
        *ret.as.ref.count = 1;
        frame->slots[dst] = ret;
        continue;
      }

      case OP_REF_DEL:
      {
        uint8_t slot = READ_BYTE();
        Value_* var = &frame->slots[slot];
        *var->as.ref.count -= 1;

        if (*var->as.ref.count <= 0) {
          free(var->as.ref.pval);
          free(var->as.ref.count);
        }
        continue;
      }

      case OP_EQ: BINARY_OP(==); continue;
      case OP_GT:
      {
        uint8_t dst = READ_BYTE();
        int64_t val = frame->slots[READ_BYTE()].as.i;
        frame->slots[dst].as.i = val == 1;
        continue;
      }

      case OP_GTE:
      {
        uint8_t dst = READ_BYTE();
        int64_t val = frame->slots[READ_BYTE()].as.i;
        frame->slots[dst].as.i = val >= 0;
        continue;
      }

      case OP_LT:
      {
        uint8_t dst = READ_BYTE();
        int64_t val = frame->slots[READ_BYTE()].as.i;
        frame->slots[dst].as.i = val == -1;
        continue;
      }

      case OP_LTE:
      {
        uint8_t dst = READ_BYTE();
        int64_t val = frame->slots[READ_BYTE()].as.i;
        frame->slots[dst].as.i = val <= 0;
        continue;
      }

      case OP_CMP:
      {
        uint8_t dst = READ_BYTE();
        uint64_t l = frame->slots[READ_BYTE()].as.u;
        uint64_t r = frame->slots[READ_BYTE()].as.u;
        frame->slots[dst].as.i = l == r ? 0 : (l < r ? -1 : 1);
        continue;
      }

      case OP_ICMP:
      {
        uint8_t dst = READ_BYTE();
        int64_t l = frame->slots[READ_BYTE()].as.i;
        int64_t r = frame->slots[READ_BYTE()].as.i;
        frame->slots[dst].as.i = l == r ? 0 : (l < r ? -1 : 1);
        continue;
      }

      case OP_FCMP:
      {
        uint8_t dst = READ_BYTE();
        float l = frame->slots[READ_BYTE()].as.f;
        float r = frame->slots[READ_BYTE()].as.f;
        frame->slots[dst].as.i = l == r ? 0 : (l < r ? -1 : 1);
        continue;
      }

      case OP_DCMP:
      {
        uint8_t dst = READ_BYTE();
        double l = frame->slots[READ_BYTE()].as.d;
        double r = frame->slots[READ_BYTE()].as.d;
        frame->slots[dst].as.i = l == r ? 0 : (l < r ? -1 : 1);
        continue;
      }

      case OP_ADDIMM:
      {
        uint8_t dst = READ_BYTE();
        uint8_t l = READ_BYTE();
        uint32_t r = READ_LONG();
        frame->slots[dst].as.u = frame->slots[l].as.u + r;
        continue;
      }

      case OP_ADD: BINARY_TYPED_OP(+, u); continue;
      case OP_SUB: BINARY_TYPED_OP(-, u); continue;
      case OP_MUL: BINARY_TYPED_OP(*, u); continue;
      case OP_DIV: BINARY_TYPED_OP(/, u); continue;
      case OP_IMUL: BINARY_TYPED_OP(*, i); continue;
      case OP_IDIV: BINARY_TYPED_OP(/, i); continue;
      case OP_FADD: BINARY_TYPED_OP(+, f); continue;
      case OP_FSUB: BINARY_TYPED_OP(-, f); continue;
      case OP_FMUL: BINARY_TYPED_OP(*, f); continue;
      case OP_FDIV: BINARY_TYPED_OP(/, f); continue;
      case OP_DADD: BINARY_TYPED_OP(+, d); continue;
      case OP_DSUB: BINARY_TYPED_OP(-, d); continue;
      case OP_DMUL: BINARY_TYPED_OP(*, d); continue;
      case OP_DDIV: BINARY_TYPED_OP(/, d); continue;
      case OP_MOD: BINARY_TYPED_OP(%, u); continue;
      case OP_IMOD: BINARY_TYPED_OP(%, i); continue;
      case OP_BITWISE_AND: BINARY_OP(&); continue;
      case OP_BITWISE_OR: BINARY_OP(|); continue;
      case OP_BITWISE_XOR: BINARY_OP(^); continue;
      case OP_BITWISE_NOT: UNARY_OP(~); continue;
      case OP_LSHIFT: BINARY_OP(<<); continue;
      case OP_RSHIFT: BINARY_OP(>>); continue;
      case OP_AND: BINARY_OP(&&); continue;
      case OP_OR: BINARY_OP(||); continue;
      case OP_XOR: BINARY_OP(^); continue;
      case OP_NOT: UNARY_OP(!); continue;
      case OP_NEG: UNARY_TYPED_OP(-, i); continue;
      case OP_FNEG: UNARY_TYPED_OP(-, f); continue;
      case OP_DNEG: UNARY_TYPED_OP(-, d); continue;

      case OP_PRINT:
      {
        uint8_t slot = READ_BYTE();
        RuntimeType_ t = type_fromint(READ_LONG());

        value_print(frame->slots[slot], t);
        printf("\n");
        continue;
      }

      case OP_ASSERT:
      {
        uint8_t slot = READ_BYTE();
        Value_ assert_val = frame->slots[slot];
        if (is_falsey(assert_val)) {
          vm_runtime_error(vm, "Assertion failed");
          return INTERPRET_ASSERTION_FAILED;
        }
        continue;
      }

      case OP_CAST_f2i:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        frame->slots[dst].as.i = (int64_t)frame->slots[src].as.f;
        continue;
      }

      case OP_CAST_f2d:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        frame->slots[dst].as.d = (double)frame->slots[src].as.f;
        continue;
      }

      case OP_CAST_d2i:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        frame->slots[dst].as.i = (int64_t)frame->slots[src].as.d;
        continue;
      }

      case OP_CAST_d2f:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        frame->slots[dst].as.f = (float)frame->slots[src].as.d;
        continue;
      }

      case OP_CAST_i2f:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        frame->slots[dst].as.f = (float)frame->slots[src].as.i;
        continue;
      }

      case OP_CAST_i2d:
      {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        frame->slots[dst].as.d = (double)frame->slots[src].as.i;
        continue;
      }
    }
  }

#undef READ_SHORT
#undef UNARY_OP
#undef BINARY_TYPED_OP
#undef BINARY_OP
#undef READ_CONSTANT
#undef READ_BYTE
}

static void reset_stack(VM vm) {
  vm->stack_top = vm->stack;
}

InterpretResult vm_runtime_error(VM_* vm, const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  for (int i = vm->frame_count - 1; i >= 0; i--) {
    CallFrame_* frame = &vm->frames[i];
    size_t instruction = frame->ip - frame->chunk->code - 1;
    fprintf(stderr, "[line %d] in ",
      frame->chunk->lines[instruction]);
    fprintf(stderr, "script\n");
  }
  return INTERPRET_RUNTIME_ERROR;
}