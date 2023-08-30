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
      frame->slots[dst].as.##TY = OP frame->slots[src].as.##TY; \
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
      frame->slots[dst].as.##TY = frame->slots[l].as.##TY OP frame->slots[r].as.##TY; \
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
        frame->slots[dst] = *(Value_*)(frame->slots[src].as.ptr + offset);
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
        *(Value_*)(frame->slots[dst].as.ptr + offset) = frame->slots[src];
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
        uint8_t size = READ_BYTE();
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

#if 0
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

      case OP_NEW_VAR:
      {
        uint32_t size = READ_LONG();
        Value_* new_val = calloc(size, sizeof(Value_));
        for (int64_t i = (int64_t)size - 1; i >= 0; --i) {
          new_val[i] = vm_pop(vm);
        }
        vm_push(vm, PTR_VAL(new_val));
        continue;
      }

      case OP_DESTROY_VAR:
      {
        int var_index = READ_BYTE();
        Value_* v = &frame->slots[var_index];
        obj_destroy(v->as.obj);
        continue;
      }

      case OP_ALLOC_PTR:
      {
        uint32_t size = READ_LONG();
        vm_push(vm, PTR_VAL(calloc(size, sizeof(Value_))));
        continue;
      }

      case OP_CAST:
      {
        Value_ expr_val = vm_pop(vm);
        RuntimeType_ from_type = type_fromint(READ_LONG());
        RuntimeType_ to_type = type_fromint(READ_LONG());

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

      case OP_ADDROF_REF:
      {
        uint8_t slot = READ_BYTE();
        vm_push(vm, PTR_VAL(frame->slots[slot].as.ref.pval));
        continue;
      }

      case OP_ADDROF_VAR:
      {
        uint8_t slot = READ_BYTE();
        vm_push(vm, PTR_VAL(&frame->slots[slot]));
        continue;
      }

      case OP_REF_MAKE:
      {
        Value_ loc = vm_pop(vm);
        Value_ ret = {
          .as.ref = {
            .pval = (Value_*)loc.as.ptr,
            .count = calloc(1, sizeof(int))
          },
        };
        vm_push(vm, ret);
        continue;
      }

      case OP_DEREF_PTR:
      {
        uint8_t slot = READ_BYTE();
        vm_push(vm, *(Value_*)vm_pop(vm).as.ptr);
        continue;
      }

      case OP_STACK_TOP:
      {
        vm_push(vm, PTR_VAL(vm->stack_top - 1));
        continue;
      }
      
      case OP_REF_PTR:
      {
        vm_push(vm, PTR_VAL(vm_pop(vm).as.ref.pval));
        continue;
      }

      case OP_ADD_OFFSET:
      {
        uint8_t slot = READ_BYTE();
        Value_* val = (Value_*)vm_pop(vm).as.ptr;
        vm_push(vm, PTR_VAL(val + slot));
        continue;
      }

      case OP_GET_OFFSET:
      {
        uint8_t slot = READ_BYTE();
        Value_* val = (Value_*)vm_pop(vm).as.ptr;
        vm_push(vm, *(val + slot));
        continue;
      }

      case OP_SET_OFFSET:
      {
        uint8_t slot = READ_BYTE();
        Value_* base = (Value_*)READ_LONGLONG();
        Value_ val = vm_pop(vm);
        *(base + slot) = val;
        continue;
      }

      case OP_GET_REF:
      {
        uint8_t slot = READ_BYTE();
        vm_push(vm, *frame->slots[slot].as.ref.pval);
        continue;
      }

      case OP_SET_REF:
      {
        uint8_t slot = READ_BYTE();
        Value_ val = vm_peek(vm, 0);
        *frame->slots[slot].as.ref.pval = val;
        continue;
      }

      case OP_REF_INC:
      {
        continue;
      }

      case OP_REF_DEC:
      {
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
        Value_ v = vm_pop(vm);
        vm_push(vm, BOOL_VAL(v.as.i < 0 ? true : false));
        continue;
      }
      
      case OP_LTE:
      {
        Value_ v = vm_pop(vm);
        vm_push(vm, BOOL_VAL(v.as.i <= 0 ? true : false));
        continue;
      }

      case OP_EQ:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        vm_push(vm, BOOL_VAL(l.as.u == r.as.u));
        continue;
      }

      case OP_OBJ_EQ:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        vm_push(vm, BOOL_VAL(obj_equal(l.as.obj, r.as.obj)));
        continue;
      }

      case OP_CMP:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        vm_push(vm, INT_VAL(l.as.u == r.as.u ? 0 : (l.as.u < r.as.u ? -1 : 1)));
        break;
      }

      case OP_ICMP:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        vm_push(vm, INT_VAL(l.as.i == r.as.i ? 0 : (l.as.i < r.as.i ? -1 : 1)));
        break;
      }

      case OP_FCMP:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        vm_push(vm, INT_VAL(l.as.d == r.as.d ? 0 : (l.as.d < r.as.d ? -1 : 1)));
        break;
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
          return vm_runtime_error(vm, "Divide-by-zero error");
        }

        vm_push(vm, UINT_VAL(l.as.u % r.as.u));
        continue;
      }

      case OP_IMOD:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);
        if (r.as.i == 0) {
          return vm_runtime_error(vm, "Divide-by-zero error");
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
          return vm_runtime_error(vm, "Divide-by-zero error");
        }

        vm_push(vm, UINT_VAL(l.as.u / r.as.u));
        continue;
      }

      case OP_IDIV:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        if (r.as.i == 0) {
          return vm_runtime_error(vm, "Divide-by-zero error");
        }

        vm_push(vm, INT_VAL(l.as.i / r.as.i));
        continue;
      }

      case OP_FDIV:
      {
        Value_ r = vm_pop(vm);
        Value_ l = vm_pop(vm);

        if (r.as.i == 0) {
          return vm_runtime_error(vm, "Divide-by-zero error");
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

      case OP_ASSERT:
      {
        Value_ assert_val = vm_pop(vm);
        if (is_falsey(assert_val)) {
          vm_runtime_error(vm, "Assertion failed");
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
#endif
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