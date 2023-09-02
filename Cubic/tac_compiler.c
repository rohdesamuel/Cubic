#include "tac_compiler.h"
#include "common.h"
#include "ast.h"
#include "symbol_table.h"

typedef Location_(*CodeGenFn)(TacChunk_*, AstNode_*);

typedef struct CodeGenRule_ {
  CodeGenFn code_gen;
} CodeGenRule_;

static void emit_return(TacChunk_* chunk, const Location_* location, int line);

#define OP_LOC(LOCATION) ((Operand_){.arg_type = OPERAND_TYPE_LOC, .loc = (LOCATION)})
#define OP_LOC_INDEX(INDEX) ((Operand_){.arg_type = OPERAND_TYPE_LOC, .loc = (Location_){.index = (INDEX)})
#define OP_SIZE(SIZE) ((Operand_){.arg_type = OPERAND_TYPE_SIZE, .size = (SIZE)})
#define OP_CONSTANT(TYPE, VAL) ((Operand_){.arg_type = OPERAND_TYPE_VAL, .opt_type = (TYPE), .size = 1, .val = (VAL)})
#define OP_SIZED_CONSTANT(TYPE, VAL, SIZE) ((Operand_){.arg_type = OPERAND_TYPE_VAL, .opt_type = (TYPE), .size = (SIZE), .val = (VAL)})
#define OP_OFFSET(OFFSET) ((Operand_){.arg_type = OPERAND_TYPE_OFFSET, .offset = (OFFSET)})

#define LOC_VAL(INDEX, SIZE) ((Location_){.type = LOCATION_TYPE_VAL, .index = INDEX, .size = SIZE})
#define LOC_VAR(INDEX, SIZE) ((Location_){.type = LOCATION_TYPE_VAR, .index = INDEX, .size = SIZE})
#define LOC_PTR(INDEX, SIZE) ((Location_){.type = LOCATION_TYPE_PTR, .index = INDEX, .size = SIZE})

static Location_ EMPTY_LOC = { 0 };
static Operand_ EMPTY_OPERAND = { 0 };

inline bool loc_is_empty(Location_ loc) {
  return loc.type == LOCATION_TYPE_EMPTY;
}

static CodeGenRule_* get_rule(int info);
static void tac_chunk_init(TacChunk_* chunk, struct MemoryAllocator_* allocator);
static void tac_chunk_free(TacChunk_* chunk);

static Location_ code_gen(TacChunk_* chunk, AstNode_* node) {
  return get_rule(node->cls)->code_gen(chunk, node);
}

void tac_compiler_init(TacCompiler_* compiler, MemoryAllocator_* allocator) {
  *compiler = (TacCompiler_){ 0 };
  compiler->allocator = allocator;
  compiler->chunk.allocator = allocator;
  tac_chunk_init(&compiler->chunk, allocator);
}

void location_print(const Location_* loc) {
  if (loc->token.start) {
    printf("%.*s", loc->token.length, loc->token.start);
  } else {
    printf("_t%d", loc->index);
  }
}

void opcode_print(OpCode op) {
  printf("%s ", OPCODE_STR(op));
}

static void tac_chunk_print(TacChunk_* chunk);

static void tac_compiler_print(TacCompiler_* compiler) {
  tac_chunk_print(&compiler->chunk);
}

static void tac_chunk_print_fn(const void* key, size_t ksize, uintptr_t value, void* usr) {
  TacChunk_* tac_chunk = (TacChunk_*)value;
  hashmap_iterate(tac_chunk->fn_code, tac_chunk_print_fn, usr);  
  tac_chunk_print(tac_chunk);
}

static void tac_chunk_print_fns(TacChunk_* chunk) {
  hashmap_iterate(chunk->fn_code, tac_chunk_print_fn, NULL);
}

static void tac_chunk_print(TacChunk_* chunk) {
#define PRINT_BINARY_OP(LOC_L, LOC_R, OP_CHAR) \
    location_print(LOC_L); \
    printf(" " ## OP_CHAR ## " "); \
    location_print(LOC_R);

  int level = 0;
  for (int i = 0; i < chunk->count; ++i) {
    Tac_* tac = chunk->code + i;
    Location_* dst = &tac->dst;
    Operand_* arg_l = &tac->arg_l;
    Operand_* arg_r = &tac->arg_r;
    Location_* loc_l = &arg_l->loc;
    Location_* loc_r = &arg_r->loc;

    if (tac->op == OP_NOP && dst->type == LOCATION_TYPE_LABEL && dst->token.start) {
      printf("%.*s:\n", dst->token.length, dst->token.start);
      continue;
    }

    if (tac->op == OP_END_SCOPE) {
      level--;
    }

    for (int j = 0; j < level; ++j) {
      printf("  ");
    }

    if (is_loc_empty(tac->dst) && tac->arg_l.arg_type == OPERAND_TYPE_EMPTY &&
      tac->arg_r.arg_type == OPERAND_TYPE_EMPTY) {
      if (tac->op == OP_BEGIN_SCOPE) {
        printf("do");
        level++;
      } else if (tac->op == OP_END_SCOPE) {
        printf("end");
      } else {
        opcode_print(tac->op);
      }
      printf("\n");
      continue;
    }

    if (!is_loc_empty(tac->dst)) {
      if (tac->op == OP_CALL) {
        printf("*");
        location_print(&tac->dst);
        printf(" <- CALL ");
        location_print(&tac->arg_l.loc);
        printf("(");
        for (int i = 0; i < tac->arg_r.size; ++i) {
          Location_ loc = tac->dst;
          loc.index += i + 1;  // +1 because index 0 is the return value address.
          location_print(&loc);
          if (i < tac->arg_r.size - 1) {
            printf(", ");
          }
        }
        printf(")");
        printf("\n");
        continue;
      }

      if (tac->op == OP_JMP) {
        printf("JMP ");
        location_print(&tac->dst);
        printf("\n");
        continue;
      }

      if (tac->op == OP_NIL) {
        location_print(&tac->dst);
        printf(" <- NIL\n");
        continue;
      }

      if (tac->op == OP_JMP_IF_FALSE) {
        printf("IF NOT ");
        location_print(&tac->arg_l.loc);
        printf(" JMP ");
        location_print(&tac->dst);
        printf("\n");
        continue;
      }

      if (tac->op == OP_STORE || tac->op == OP_RSTORE) {
        printf("*(");
      }

      location_print(&tac->dst);
      if (tac->op == OP_RSTORE) {
        printf(".ptr");
      }

      if (tac->op == OP_STORE || tac->op == OP_RSTORE) {
        if (tac->arg_r.offset > 0) {
          printf(" + %d", tac->arg_r.offset);
        }
        printf(")");
      } else if (tac->op == OP_MEMCPY) {
        printf("[0..%d]", tac->arg_r.offset - 1);
      }

      printf(" <- ");
    }

    switch (tac->op) {
      case OP_CONSTANT:
        value_print(tac->arg_l.val, tac->arg_l.opt_type);
        break;

      case OP_PROLOGUE:
        printf("PROLOGUE %lld", tac->arg_l.size);
        break;

      case OP_PUSH_PARAM:
        printf("PUSH_PARAM ");
        location_print(loc_l);
        break;

      case OP_PRINT:
        opcode_print(tac->op);
        location_print(loc_l);
        break;

      case OP_RETURN:
        printf("RETURN ");
        if (loc_l->type != LOCATION_TYPE_EMPTY) {
          location_print(loc_l);
        }
        break;

      case OP_ASSERT:
        printf("ASSERT ");
        location_print(loc_l);
        break;

      case OP_REF_MAKE:
        printf("new ref[%lld]", tac->arg_l.size);
        break;

      case OP_REF_INC:
        location_print(loc_l);
        printf(".count ++");
        break;

      case OP_MOVE:
        location_print(loc_l);
        break;

      case OP_MEMCPY:
        location_print(loc_l);
        printf("[0..%lld]", tac->arg_r.size - 1);
        break;

      case OP_LOAD:
        printf("*(");
        location_print(loc_l);
        if (tac->arg_r.offset > 0) {
          printf(" + %d", tac->arg_r.offset);
        }
        printf(")");
        break;

      case OP_LOADA:
        printf("&");
        location_print(loc_l);
        if (tac->arg_r.offset > 0) {
          printf(" + %d", tac->arg_r.offset);
        }
        break;

      case OP_STORE:
        location_print(loc_l);
        break;

      case OP_RLOAD:
        printf("*(");
        location_print(loc_l);
        printf(".ptr");
        if (tac->arg_r.offset > 0) {
          printf(" + %d", tac->arg_r.offset);
        }
        printf(")");
        break;

      case OP_RLOADA:
        location_print(loc_l);
        printf(".ptr");
        if (tac->arg_r.offset > 0) {
          printf(" + %d", tac->arg_r.offset);
        }
        break;

      case OP_RSTORE:
        location_print(loc_l);
        break;

      case OP_ADDIMM:
        location_print(loc_l);
        printf(" + %d", arg_r->offset);
        break;

      case OP_EQ:
        PRINT_BINARY_OP(loc_l, loc_r, "==");
        break;

      case OP_GT:
        PRINT_BINARY_OP(loc_l, loc_r, ">");
        break;

      case OP_GTE:
        PRINT_BINARY_OP(loc_l, loc_r, ">=");
        break;

      case OP_LT:
        PRINT_BINARY_OP(loc_l, loc_r, "<");
        break;

      case OP_LTE:
        PRINT_BINARY_OP(loc_l, loc_r, "<=");
        break;

      case OP_CMP:
        PRINT_BINARY_OP(loc_l, loc_r, "<=>");
        break;

      case OP_ICMP:
        PRINT_BINARY_OP(loc_l, loc_r, "<=>");
        break;

      case OP_FCMP:
        PRINT_BINARY_OP(loc_l, loc_r, "<=>");
        break;

      case OP_DCMP:
        PRINT_BINARY_OP(loc_l, loc_r, "<=>");
        break;

      case OP_ADD:
        PRINT_BINARY_OP(loc_l, loc_r, "+");
        break;

      case OP_SUB:
        PRINT_BINARY_OP(loc_l, loc_r, "-");
        break;

      case OP_MUL:
        PRINT_BINARY_OP(loc_l, loc_r, "*");
        break;

      case OP_DIV:
        PRINT_BINARY_OP(loc_l, loc_r, "/");
        break;

      case OP_IMUL:
        PRINT_BINARY_OP(loc_l, loc_r, "*");
        break;

      case OP_IDIV:
        PRINT_BINARY_OP(loc_l, loc_r, "//");
        break;

      case OP_FADD:
        PRINT_BINARY_OP(loc_l, loc_r, "+");
        break;

      case OP_FSUB:
        PRINT_BINARY_OP(loc_l, loc_r, "-");
        break;

      case OP_FMUL:
        PRINT_BINARY_OP(loc_l, loc_r, "*");
        break;

      case OP_FDIV:
        PRINT_BINARY_OP(loc_l, loc_r, "/");
        break;

      case OP_DADD:
        PRINT_BINARY_OP(loc_l, loc_r, "+");
        break;

      case OP_DSUB:
        PRINT_BINARY_OP(loc_l, loc_r, "-");
        break;

      case OP_DMUL:
        PRINT_BINARY_OP(loc_l, loc_r, "*");
        break;

      case OP_DDIV:
        PRINT_BINARY_OP(loc_l, loc_r, "/");
        break;

      case OP_CONCAT:
        PRINT_BINARY_OP(loc_l, loc_r, "+");
        break;

      case OP_MOD:
        PRINT_BINARY_OP(loc_l, loc_r, "%%");
        break;

      case OP_IMOD:
        PRINT_BINARY_OP(loc_l, loc_r, "%%");
        break;

      case OP_BITWISE_AND:
        PRINT_BINARY_OP(loc_l, loc_r, "&");
        break;

      case OP_BITWISE_OR:
        PRINT_BINARY_OP(loc_l, loc_r, "|");
        break;

      case OP_BITWISE_XOR:
        PRINT_BINARY_OP(loc_l, loc_r, "^");
        break;

      case OP_BITWISE_NOT:
        PRINT_BINARY_OP(loc_l, loc_r, "~");
        break;

      case OP_LSHIFT:
        PRINT_BINARY_OP(loc_l, loc_r, "<<");
        break;

      case OP_RSHIFT:
        PRINT_BINARY_OP(loc_l, loc_r, ">>");
        break;

      case OP_AND:
        PRINT_BINARY_OP(loc_l, loc_r, "&&");
        break;

      case OP_OR:
        PRINT_BINARY_OP(loc_l, loc_r, "||");
        break;

      case OP_XOR:
        PRINT_BINARY_OP(loc_l, loc_r, "^^");
        break;

      case OP_NOT:
        printf("!");
        location_print(loc_l);
        break;

      case OP_NEG:
        printf("-");
        location_print(loc_l);
        break;

      case OP_FNEG:
        printf("-");
        location_print(loc_l);
        break;

      case OP_DNEG:
        printf("-");
        location_print(loc_l);
        break;

      case OP_CAST_f2i:
        printf("(int)(");
        location_print(loc_l);
        printf(")");
        break;

      case OP_CAST_f2d:
        printf("(double)(");
        location_print(loc_l);
        printf(")");
        break;

      case OP_CAST_d2i:
        printf("(int)(");
        location_print(loc_l);
        printf(")");
        break;

      case OP_CAST_d2f:
        printf("(float)(");
        location_print(loc_l);
        printf(")");
        break;

      case OP_CAST_i2f:
        printf("(float)(");
        location_print(loc_l);
        printf(")");
        break;

      case OP_CAST_i2d:
        printf("(double)(");
        location_print(loc_l);
        printf(")");
        break;

      default:
        location_print(loc_l);
        printf(" ");
        opcode_print(tac->op);
        location_print(loc_r);
    }
    printf("\n");
  }
  printf("\n");
  tac_chunk_print_fns(chunk);
#undef PRINT_BINARY_OP
}

void tac_compiler_compile(TacCompiler_* compiler, struct AstNode_* root) {
  code_gen(&compiler->chunk, root);

  Location_ empty_ret = EMPTY_LOC;
  emit_return(&compiler->chunk, &empty_ret, 0);

  //tac_compiler_print(compiler);
}

void tac_compiler_clear(TacCompiler_* compiler) {
  tac_chunk_free(&compiler->chunk);
  *compiler = (TacCompiler_){ 0 };
}

static inline void tac_chunk_reserve(TacChunk_* chunk, int count) {
  if (chunk->capacity < chunk->count + count) {
    int old_capacity = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(old_capacity);
    chunk->code = GROW_ARRAY(Tac_, chunk->code,
      old_capacity, chunk->capacity);
    chunk->lines = GROW_ARRAY(int, chunk->lines,
      old_capacity, chunk->capacity);
  }
}

static void tac_chunk_init(TacChunk_* chunk, struct MemoryAllocator_* allocator) {
  *chunk = (TacChunk_){ 0 };
  chunk->fn_code = hashmap_create();
  chunk->allocator = allocator;
}

static void tac_chunk_free(TacChunk_* chunk) {
  FREE_ARRAY(int, chunk->lines, chunk->capacity);
  FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
  hashmap_free(chunk->fn_code);
  *chunk = (TacChunk_){ 0 };
}

static Location_ tac_alloc_ret(TacChunk_* chunk) {
  Location_ ret = EMPTY_LOC;
  ret.type = LOCATION_TYPE_PTR;
  ret.index = chunk->slot_index++;
  ret.frame_offset = chunk->slot_offset;
  ret.size = 1;
  return ret;
}

static Location_ tac_alloc_val(TacChunk_* chunk, size_t size) {
  assertf(size != 0, "Trying to allocate zero-sized memory.");

  Location_ ret = EMPTY_LOC;
  ret.type = LOCATION_TYPE_VAL;
  ret.index = ++chunk->slot_index;
  ret.frame_offset = chunk->slot_offset;
  chunk->slot_offset += (int32_t)size;
  ret.size = size;
  return ret;
}

static void tac_alloc_pop(TacChunk_* chunk, Location_* loc) {
  chunk->slot_offset -= (int)loc->size;
  --chunk->slot_index;
}

static Location_ tac_alloc_ptr(TacChunk_* chunk) {
  Location_ ret = EMPTY_LOC;
  ret.type = LOCATION_TYPE_PTR;
  ret.index = ++chunk->slot_index;
  ret.frame_offset = chunk->slot_offset;
  chunk->slot_offset += 1;
  ret.size = 1;
  return ret;
}

static Location_ tac_alloc_var(TacChunk_* chunk) {
  Location_ ret = EMPTY_LOC;
  ret.type = LOCATION_TYPE_VAR;
  ret.index = ++chunk->slot_index;
  ret.frame_offset = chunk->slot_offset;
  chunk->slot_offset += 1;
  ret.size = 1;
  return ret;
}

static Location_ tac_alloc(TacChunk_* chunk, const Type_* type) {
  if (type_is(type, NilType_)) {
    return EMPTY_LOC;
  }

  switch (type->cls) {
    case TYPE_CLS(VarType_): return tac_alloc_var(chunk);
    case TYPE_CLS(RefType_): return tac_alloc_ptr(chunk);
    case TYPE_CLS(InType_): return tac_alloc_ptr(chunk);
    case TYPE_CLS(OutType_): return tac_alloc_ptr(chunk);
    default: return tac_alloc_val(chunk, type->size);
  }
  assertf(false, "Encountered unknown variable cls: %d", type->cls);
  return EMPTY_LOC;
}

static void tac_free(TacChunk_* chunk, Location_* loc) {
  --chunk->slot_index;
  chunk->slot_offset -= (int)loc->size;
}

static Location_ tac_alloc_label(TacChunk_* chunk, const char* prefix) {
  Location_ ret = EMPTY_LOC;
  ret.type = LOCATION_TYPE_LABEL;
  ret.index = chunk->label_index++;

  char buf[64] = { 0 };
  int len = sprintf_s(buf, sizeof(buf), "_l%d_%s", ret.index, prefix);
  assertf(len > 0, "Could not create label");

  char* str = alloc(chunk->allocator, len);
  memcpy(str, buf, len);

  ret.token.start = str;
  ret.token.length = len;

  return ret;
}

static Location_ tac_alloc_fn_label(TacChunk_* chunk, Token_* fn_name) {
  Location_ ret = EMPTY_LOC;
  ret.type = LOCATION_TYPE_LABEL;
  ret.index = chunk->label_index++;
  ret.size = 1;

  char buf[256] = { 0 };
  int len = 0;
  if (fn_name->length) {
    len = sprintf_s(buf, sizeof(buf), "_fn%d_%.*s", ret.index, fn_name->length, fn_name->start);
  } else {
    len = sprintf_s(buf, sizeof(buf), "_fn%d", ret.index);
  }
  assertf(len > 0, "Could not create label");

  char* str = alloc(chunk->allocator, len);
  memcpy(str, buf, len);

  ret.token.start = str;
  ret.token.length = len;

  return ret;
}

static Location_ tac_alloca(TacChunk_* chunk, int64_t size) {
  assertf(size > 0, "Trying to allocate a non-positive number: %lld", size);

  Location_ ret = EMPTY_LOC;
  ret.type = LOCATION_TYPE_VAL;
  ret.index = chunk->slot_index + 1;
  chunk->slot_index += (int)size;
  chunk->slot_offset += (int32_t)size;
  return ret;
}

static Location_ tac_current(TacChunk_* chunk) {
  Location_ ret = EMPTY_LOC;
  ret.type = LOCATION_TYPE_VAL;
  ret.index = chunk->slot_index;
  ret.frame_offset = chunk->slot_offset;
  return ret;
}

typedef int TacHandle_;
static TacHandle_ tac_chunk_write(TacChunk_* chunk, Tac_* code, int line) {
  tac_chunk_reserve(chunk, 1);

  chunk->code[chunk->count] = *code;
  chunk->lines[chunk->count] = line;
  chunk->count++;

  return chunk->count - 1;
}

static Location_ tac_chunk_writeconstant(TacChunk_* chunk, TypedValue_ value, size_t size, int line) {  
  Tac_ tac = {
    .op = OP_CONSTANT,
    .dst = tac_alloc_val(chunk, size),
    .arg_l.opt_type = value.ty,
    .arg_l.val = value.val,
  };
  tac_chunk_write(chunk, &tac, line);
  return tac.dst;
}

static TacHandle_ emit_tac(TacChunk_* chunk, OpCode op, Location_ dst, Operand_ arg_l, Operand_ arg_r, int line) {
  Tac_ tac = { 0 };
  tac.op = op;
  tac.dst = dst;
  tac.arg_l = arg_l;
  tac.arg_r = arg_r;
  tac.line = line;

  return tac_chunk_write(chunk, &tac, line);
}

static Location_ emit_nil(TacChunk_* chunk, int line) {
  Location_ ret = tac_alloc_val(chunk, 1);
  emit_tac(chunk, OP_NIL, ret, EMPTY_OPERAND, EMPTY_OPERAND, line);
  return ret;
}

static Location_ emit_constant(TacChunk_* chunk, TypedValue_ value, size_t size, int line) {
  return tac_chunk_writeconstant(chunk, value, size, line);
}

static void emit_make_ref(TacChunk_* chunk, const Location_* dst, size_t size, int line) {
  assertf(dst->type == LOCATION_TYPE_VAR, "");
  emit_tac(chunk, OP_REF_MAKE, *dst, OP_SIZE(size), EMPTY_OPERAND, line);
}

static void emit_make_ref_from(TacChunk_* chunk, const Location_* dst, const Location_* src, size_t size, int line) {
  assertf(dst->type == LOCATION_TYPE_VAR, "");
  assertf(src->type == LOCATION_TYPE_VAR, "");
  emit_tac(chunk, OP_REF_INC, EMPTY_LOC, OP_LOC(*src), EMPTY_OPERAND, line);
  emit_tac(chunk, OP_MOVE, *dst, OP_LOC(*src), EMPTY_OPERAND, line);
}

static Location_ emit_cast(TacChunk_* chunk, Location_ val, const Type_* from, const Type_* to, size_t size, int line) {
  Location_ dst = tac_alloc_val(chunk, size);
  OpCode cast_op = OP_NOP;
  switch (from->cls) {
    case TYPE_CLS(DoubleType_):
      switch (to->cls) {
        case TYPE_CLS(FloatType_):
          cast_op = OP_CAST_d2f;
          break;
        default:
          cast_op = OP_CAST_d2i;
          break;
      }
      break;
    case TYPE_CLS(FloatType_):
      switch (to->cls) {
        case TYPE_CLS(DoubleType_):
          cast_op = OP_CAST_f2d;
          break;
        default:
          cast_op = OP_CAST_f2i;
          break;
      }
      break;
    default:
      switch (to->cls) {
        case TYPE_CLS(FloatType_):
          cast_op = OP_CAST_i2f;
          break;
        case TYPE_CLS(DoubleType_):
          cast_op = OP_CAST_i2d;
          break;
      }
      break;
  }
  assertf(cast_op != OP_NOP, "Trying an unknown cast. From: %d. To: %d", from->cls, to->cls);

  emit_tac(chunk, cast_op, dst, OP_LOC(val), EMPTY_OPERAND, line);
  return dst;
}

static void emit_op(TacChunk_* chunk, OpCode op, int line) {
  emit_tac(chunk, op, EMPTY_LOC, EMPTY_OPERAND, EMPTY_OPERAND, line);
}

static void emit_return(TacChunk_* chunk, const Location_* ret, int line) {
  emit_tac(chunk, OP_RETURN, EMPTY_LOC, OP_LOC(*ret), EMPTY_OPERAND, line);
}

static void emit_call(TacChunk_* chunk, const Location_* dst, const Location_* fn, int64_t arg_size, int line) {
  assertf(fn->type == LOCATION_TYPE_LABEL, "Expected location to call be a label.");

  emit_tac(chunk, OP_CALL, *dst, OP_LOC(*fn), OP_SIZE(arg_size), line);
}

static TacHandle_ emit_label(TacChunk_* chunk, Location_ label, int line) {
  Tac_ tac = { 0 };
  tac.dst = label;
  tac.line = line;
  tac.chunk_loc = chunk->count;

  return tac_chunk_write(chunk, &tac, line);
}

static void emit_set_val(TacChunk_* chunk, const Location_* dst, const Location_* src, size_t val_size, int line) {
  // dst       src       behavior              Op Code
  // KIND_VAL  KIND_VAL  dst = src             OP_MOVE
  // KIND_VAL  KIND_VAR  dst = *src.ptr        OP_RLOAD
  // KIND_VAL  KIND_REF  dst = *src            OP_LOAD
  assert(dst->type == LOCATION_TYPE_VAL);
  assertf(val_size > 0, "Trying to set a variable with no size.");

  if (val_size == 1) {
    switch (src->type) {
      case LOCATION_TYPE_VAL:
        emit_tac(chunk, OP_MOVE, *dst, OP_LOC(*src), EMPTY_OPERAND, line);
        break;
      case LOCATION_TYPE_VAR:
        emit_tac(chunk, OP_RLOAD, *dst, OP_LOC(*src), EMPTY_OPERAND, line);
        break;
      case LOCATION_TYPE_PTR:
        emit_tac(chunk, OP_LOAD, *dst, OP_LOC(*src), EMPTY_OPERAND, line);
        break;
    }
  } else {
    Location_ src_ptr = tac_alloc_ptr(chunk);
    Location_ dst_ptr = tac_alloc_ptr(chunk);

    emit_tac(chunk, OP_LOADA, dst_ptr, OP_LOC(*dst), EMPTY_OPERAND, line);
    switch (src->type) {
      case KIND_VAL:
        emit_tac(chunk, OP_LOADA, src_ptr, OP_LOC(*src), EMPTY_OPERAND, line);
        break;
      case KIND_VAR:
        emit_tac(chunk, OP_RLOADA, src_ptr, OP_LOC(*src), EMPTY_OPERAND, line);
        break;
      case KIND_REF:
        src_ptr = *src;
        break;
    }

    emit_tac(chunk, OP_MEMCPY, dst_ptr, OP_LOC(src_ptr), OP_SIZE(val_size), line);
  }
}

static void emit_set_var(TacChunk_* chunk, const Location_* dst, const Location_* src, size_t val_size, int line) {
  // dst       src       behavior              Op Code
  // KIND_VAR  KIND_VAL  *dst.ptr = src        OP_RSTORE
  // KIND_VAR  KIND_VAR  *dst.ptr = *src.ptr   OP_RLOAD, OP_RSTORE
  // KIND_VAR  KIND_REF  *dst.ptr = *src       OP_LOAD, OP_RSTORE
  assert(dst->type == LOCATION_TYPE_VAR);
  assertf(val_size > 0, "Trying to set a variable with no size.");

  if (val_size == 1) {
    Location_ tmp = src->type == LOCATION_TYPE_VAL ? *src : tac_alloc_val(chunk, val_size);

    switch (src->type) {
      case LOCATION_TYPE_VAL:
        break;

      case LOCATION_TYPE_VAR:
        emit_tac(chunk, OP_RLOAD, tmp, OP_LOC(*src), EMPTY_OPERAND, line);
        break;

      case LOCATION_TYPE_PTR:
        emit_tac(chunk, OP_LOAD, tmp, OP_LOC(*src), EMPTY_OPERAND, line);
        break;
    }
    emit_tac(chunk, OP_RSTORE, *dst, OP_LOC(tmp), EMPTY_OPERAND, line);
  } else {
    Location_ src_ptr = src->type == LOCATION_TYPE_PTR ? *src : tac_alloc_ptr(chunk);
    Location_ dst_ptr = tac_alloc_ptr(chunk);

    emit_tac(chunk, OP_RLOADA, dst_ptr, OP_LOC(*dst), EMPTY_OPERAND, line);
    switch (src->type) {
      case LOCATION_TYPE_VAL:
        emit_tac(chunk, OP_LOADA, src_ptr, OP_LOC(*src), EMPTY_OPERAND, line);
        break;
      case LOCATION_TYPE_VAR:
        emit_tac(chunk, OP_RLOADA, src_ptr, OP_LOC(*src), EMPTY_OPERAND, line);
        break;
      case LOCATION_TYPE_PTR:
        break;
    }

    emit_tac(chunk, OP_MEMCPY, dst_ptr, OP_LOC(src_ptr), OP_SIZE(val_size), line);
  }
}

static void emit_set_ref(TacChunk_* chunk, const Location_* dst, const Location_* src, size_t val_size, int line) {
  // dst       src       behavior              Op Code
  // KIND_REF  KIND_VAL  *dst = src            OP_STORE
  // KIND_REF  KIND_VAR  *dst = *src.ptr       OP_RLOAD, OP_STORE
  // KIND_REF  KIND_REF  *dst = *src           OP_LOAD, OP_STORE
  assert(dst->type == LOCATION_TYPE_PTR);
  assertf(val_size > 0, "Trying to set a variable with no size.");

  if (val_size == 1) {
    Location_ tmp = src->type == LOCATION_TYPE_VAL ? *src : tac_alloc_val(chunk, val_size);
    switch (src->type) {
      case LOCATION_TYPE_VAL:
        break;

      case LOCATION_TYPE_VAR:
        emit_tac(chunk, OP_RLOAD, tmp, OP_LOC(*src), EMPTY_OPERAND, line);
        break;

      case LOCATION_TYPE_PTR:
        emit_tac(chunk, OP_LOAD, tmp, OP_LOC(*src), EMPTY_OPERAND, line);
        break;
    }
    emit_tac(chunk, OP_STORE, *dst, OP_LOC(tmp), EMPTY_OPERAND, line);
  } else {
    Location_ src_ptr = src->type == KIND_REF ? *src : tac_alloc_ptr(chunk);
    Location_ dst_ptr = *dst;
    switch (src->type) {
      case LOCATION_TYPE_VAL:
        emit_tac(chunk, OP_LOADA, src_ptr, OP_LOC(*src), EMPTY_OPERAND, line);
        break;
      case LOCATION_TYPE_VAR:
        emit_tac(chunk, OP_RLOADA, src_ptr, OP_LOC(*src), EMPTY_OPERAND, line);
        break;
      case LOCATION_TYPE_PTR:
        break;
    }

    emit_tac(chunk, OP_MEMCPY, dst_ptr, OP_LOC(src_ptr), OP_SIZE(val_size), line);
  }
}

static void emit_set_variable(TacChunk_* chunk, const Location_* dst, const Location_* src, size_t val_size, int line) {
  switch (dst->type) {
    case LOCATION_TYPE_VAL:
      emit_set_val(chunk, dst, src, val_size, line);
      break;
    case LOCATION_TYPE_VAR:
      emit_set_var(chunk, dst, src, val_size, line);
      break;
    case LOCATION_TYPE_PTR:
      emit_set_ref(chunk, dst, src, val_size, line);
      break;
  }
}

//
static void emit_set_param(TacChunk_* chunk, const Location_* dst, const Location_* src, size_t val_size, int line) {
  switch (dst->type) {
    case LOCATION_TYPE_VAL:
      emit_set_val(chunk, dst, src, val_size, line);
      break;
    case LOCATION_TYPE_VAR:
      emit_set_var(chunk, dst, src, val_size, line);
      break;
    case LOCATION_TYPE_PTR:
      switch (src->type) {
        case LOCATION_TYPE_VAL:
          emit_tac(chunk, OP_LOADA, *dst, OP_LOC(*src), EMPTY_OPERAND, line);
          break;
        case LOCATION_TYPE_VAR:
          emit_tac(chunk, OP_RLOADA, *dst, OP_LOC(*src), EMPTY_OPERAND, line);
          break;
        case LOCATION_TYPE_PTR:
          emit_tac(chunk, OP_MOVE, *dst, OP_LOC(*src), EMPTY_OPERAND, line);
          break;
      }
      break;
  }
}

static Location_ emit_get_var(TacChunk_* chunk, const Location_* src, size_t size, int line) {
  assertf(src->type == LOCATION_TYPE_VAR, "");
  Location_ dst = tac_alloc_val(chunk, size);
  emit_set_variable(chunk, &dst, src, size, line);
  return dst;
}

static Location_ emit_get_ptr(TacChunk_* chunk, const Location_* src, size_t size, int line) {
  assertf(src->type == LOCATION_TYPE_PTR, "");
  Location_ dst = tac_alloc_val(chunk, size);
  emit_set_variable(chunk, &dst, src, size, line);
  return dst;
}

static Location_ emit_get_variable(TacChunk_* chunk, const Location_* src, size_t size, int line) {
  switch (src->type) {
    case LOCATION_TYPE_VAL:
      return *src;
    case LOCATION_TYPE_VAR:
      return emit_get_var(chunk, src, size, line);
    case LOCATION_TYPE_PTR:
      return emit_get_ptr(chunk, src, size, line);
  }
  assertf(false, "Received unknown location type %d", src->type);
  return EMPTY_LOC;
}

static Location_ emit_add(TacChunk_* chunk, Location_ l, Location_ r, const Type_* ltype, const Type_* rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  if (ltype->cls != rtype->cls) {
    r = emit_cast(chunk, r, rtype, ltype, type_size, line);
  }

  OpCode op = OP_NIL;
  if (type_isainteger(ltype)) {
    op = OP_ADD;
  } else if (type_is(ltype, FloatType_)) {
    op = OP_FADD;
  } else if (type_is(ltype, DoubleType_)) {
    op = OP_DADD;
  } else if (IS_TY_OBJ(ltype, OBJ_TYPE_STRING)) {
    op = OP_CONCAT;
  }
  emit_tac(chunk, op, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_sub(TacChunk_* chunk, Location_ l, Location_ r, const Type_* ltype, const Type_* rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  if (ltype->cls != rtype->cls) {
    r = emit_cast(chunk, r, rtype, ltype, type_size, line);
  }

  OpCode op = OP_NIL;
  if (type_isainteger(ltype)) {
    op = OP_SUB;
  } else if (type_is(ltype, FloatType_)) {
    op = OP_FSUB;
  } else if (type_is(ltype, DoubleType_)) {
    op = OP_DSUB;
  }
  emit_tac(chunk, op, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_mul(TacChunk_* chunk, Location_ l, Location_ r, const Type_* ltype, const Type_* rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  if (ltype->cls != rtype->cls) {
    r = emit_cast(chunk, r, rtype, ltype, type_size, line);
  }

  OpCode op = OP_NIL;
  if (ISA_TY_UINT(ltype)) {
    op = OP_MUL;
  } else if (ISA_TY_INT(ltype)) {
    op = OP_IMUL;
  } else if (ltype.ty == VAL_FLOAT) {
    op = OP_FMUL;
  } else if (ltype.ty == VAL_DOUBLE) {
    op = OP_DMUL;
  }
  emit_tac(chunk, op, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_div(TacChunk_* chunk, Location_ l, Location_ r, const Type_* ltype, const Type_* rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  if (ltype->cls != rtype->cls) {
    r = emit_cast(chunk, r, rtype, ltype, type_size, line);
  }

  OpCode op = OP_NIL;
  if (ISA_TY_UINT(ltype)) {
    op = OP_DIV;
  } else if (ISA_TY_INT(ltype)) {
    op = OP_IDIV;
  } else if (ltype.ty == VAL_FLOAT) {
    op = OP_FDIV;
  } else if (ltype.ty == VAL_DOUBLE) {
    op = OP_DDIV;
  }
  emit_tac(chunk, op, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_idiv(TacChunk_* chunk, Location_ l, Location_ r, const Type_* ltype, const Type_* rtype, size_t type_size, int line) {
  Location_ tmp = emit_div(chunk, l, r, ltype, rtype, type_size, line);

  if (ISA_TY_REAL(ltype)) {
    RuntimeType_ int_ty = {
      .ty = ltype.ty == VAL_FLOAT ? VAL_INT32 : VAL_INT64,
      .kind = KIND_VAL,
    };
    return emit_cast(chunk, tmp, ltype, int_ty, type_size, line);
  }

  return tmp;
}

static Location_ emit_mod(TacChunk_* chunk, Location_ l, Location_ r, const Type_* ltype, const Type_* rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  if (ltype.ty != rtype.ty) {
    r = emit_cast(chunk, r, rtype, ltype, type_size, line);
  }

  OpCode op = OP_NIL;
  if (ISA_TY_UINT(ltype)) {
    op = OP_MOD;
  } else if (ISA_TY_INT(ltype)) {
    op = OP_IMOD;
  }
  emit_tac(chunk, op, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_neg(TacChunk_* chunk, Location_ val, const Type_* ty, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  OpCode op = OP_NIL;
  if (t.ty == VAL_FLOAT) {
    op = OP_FNEG;
  } else if (t.ty == VAL_DOUBLE) {
    op = OP_DNEG;
  } else {
    op = OP_NEG;
  }
  emit_tac(chunk, op, dest, OP_LOC(val), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_lshift(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_LSHIFT, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_rshift(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_RSHIFT, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_bit_and(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_BITWISE_AND, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_bit_or(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_BITWISE_OR, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_bit_xor(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_BITWISE_XOR, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_bit_not(TacChunk_* chunk, Location_ val, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_BITWISE_NOT, dest, OP_LOC(val), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_and(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_AND, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_or(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_OR, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_xor(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_XOR, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_not(TacChunk_* chunk, Location_ val, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_NOT, dest, OP_LOC(val), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_eq(TacChunk_* chunk, Location_ l, Location_ r, Location_ l_val, Location_ r_val, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);

  if (l.type == r.type) {
    emit_tac(chunk, OP_EQ, dest, OP_LOC(l), OP_LOC(r), line);
  } else {
    emit_tac(chunk, OP_EQ, dest, OP_LOC(l_val), OP_LOC(r_val), line);
  }

  return dest;
}

static Location_ emit_neq(TacChunk_* chunk, Location_ l, Location_ r, Location_ l_val, Location_ r_val, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);

  if (l.type == r.type) {
    emit_tac(chunk, OP_EQ, dest, OP_LOC(l), OP_LOC(r), line);
  } else {
    emit_tac(chunk, OP_EQ, dest, OP_LOC(l_val), OP_LOC(r_val), line);
  }

  return emit_not(chunk, dest, type_size, line);
}

static Location_ emit_cmp(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  if (ltype.ty != rtype.ty) {
    r = emit_cast(chunk, r, rtype, ltype, type_size, line);
  }

  OpCode op = OP_NIL;
  if (ISA_TY_UINT(ltype)) {
    op = OP_CMP;
  } else if (ISA_TY_INT(ltype)) {
    op = OP_ICMP;
  } else if (ltype.ty == VAL_FLOAT) {
    op = OP_FCMP;
  } else if (ltype.ty == VAL_DOUBLE) {
    op = OP_DCMP;
  }
  emit_tac(chunk, op, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_gt(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ tmp = emit_cmp(chunk, l, r, ltype, rtype, type_size, line);
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_GT, dest, OP_LOC(tmp), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_gte(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ tmp = emit_cmp(chunk, l, r, ltype, rtype, type_size, line);
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_GTE, dest, OP_LOC(tmp), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_lt(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ tmp = emit_cmp(chunk, l, r, ltype, rtype, type_size, line);
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_LT, dest, OP_LOC(tmp), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_lte(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ tmp = emit_cmp(chunk, l, r, ltype, rtype, type_size, line);
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_LTE, dest, OP_LOC(tmp), EMPTY_OPERAND, line);
  return dest;
}

static Location_ binary_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstBinaryExp_* exp = (AstBinaryExp_*)node;
  AstExpr_* l = AS_EXPR(exp->left);
  AstExpr_* r = AS_EXPR(exp->right);

  RuntimeType_ ltype = semantictype_toruntime(l->sem_type);
  RuntimeType_ rtype = semantictype_toruntime(r->sem_type);

  Location_ l_loc = code_gen(chunk, (AstNode_*)l);
  Location_ r_loc = code_gen(chunk, (AstNode_*)r);
  Location_ l_orig = l_loc;
  Location_ r_orig = r_loc;

  if (l_loc.type != LOCATION_TYPE_VAL) {
    l_loc = emit_get_variable(chunk, &l_loc, l->sem_type.size, node->line);
  }

  if (r_loc.type != LOCATION_TYPE_VAL) {
    r_loc = emit_get_variable(chunk, &r_loc, r->sem_type.size, node->line);
  }

  size_t type_size = exp->base.sem_type.size;
  switch (exp->op) {
    case TK_AND:           return emit_and(chunk, l_loc, r_loc, type_size, node->line);
    case TK_OR:            return emit_or(chunk, l_loc, r_loc, type_size, node->line);
    case TK_XOR:           return emit_xor(chunk, l_loc, r_loc, type_size, node->line);
    case TK_PLUS:          return emit_add(chunk, l_loc, r_loc, ltype, rtype, type_size, node->line);
    case TK_MINUS:         return emit_sub(chunk, l_loc, r_loc, ltype, rtype, type_size, node->line);
    case TK_STAR:          return emit_mul(chunk, l_loc, r_loc, ltype, rtype, type_size, node->line);
    case TK_SLASH:         return emit_div(chunk, l_loc, r_loc, ltype, rtype, type_size, node->line);
    case TK_DOUBLE_SLASH:  return emit_idiv(chunk, l_loc, r_loc, ltype, rtype, type_size, node->line);
    case TK_PERCENT:       return emit_mod(chunk, l_loc, r_loc, ltype, rtype, type_size, node->line);
    case TK_EQUAL_EQUAL:   return emit_eq(chunk, l_orig, r_orig, l_loc, r_loc, type_size, node->line);
    case TK_BANG_EQUAL:    return emit_neq(chunk, l_orig, r_orig, l_loc, r_loc, type_size, node->line);
    case TK_GT:            return emit_gt(chunk, l_loc, r_loc, ltype, rtype, type_size, node->line);
    case TK_GTE:           return emit_gte(chunk, l_loc, r_loc, ltype, rtype, type_size, node->line);
    case TK_LT:            return emit_lt(chunk, l_loc, r_loc, ltype, rtype, type_size, node->line);
    case TK_LTE:           return emit_lte(chunk, l_loc, r_loc, ltype, rtype, type_size, node->line);
    case TK_LSHIFT:        return emit_lshift(chunk, l_loc, r_loc, type_size, node->line);
    case TK_RSHIFT:        return emit_rshift(chunk, l_loc, r_loc, type_size, node->line);
    case TK_AMPERSAND:     return emit_bit_and(chunk, l_loc, r_loc, type_size, node->line);
    case TK_PIPE:          return emit_bit_or(chunk, l_loc, r_loc, type_size, node->line);
    case TK_HAT:           return emit_bit_xor(chunk, l_loc, r_loc, type_size, node->line);
    default: printf("[Line %d] BinaryOp '%d' unimplemented\n", exp->base.base.line, exp->op);  break; // Unreachable.
  }

  return EMPTY_LOC;
}

static Location_ unary_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstUnaryExp_* exp = (AstUnaryExp_*)node;
  AstExpr_* e = AS_EXPR(exp->expr);

  RuntimeType_ type = semantictype_toruntime(e->sem_type);
  Location_ loc = code_gen(chunk, (AstNode_*)e);

  if (loc.type != LOCATION_TYPE_VAL) {
    loc = emit_get_variable(chunk, &loc, e->sem_type.size, node->line);
  }

  size_t type_size = exp->base.sem_type.size;
  switch (exp->op) {
    case TK_BANG:  return emit_not(chunk, loc, loc.size, node->line);
    case TK_MINUS: return emit_neg(chunk, loc, type, loc.size, node->line);
    case TK_NOT:  return emit_not(chunk, loc, loc.size, node->line);
    case TK_TILDE: return emit_bit_not(chunk, loc, loc.size, node->line);

    default: printf("[Line %d] UnaryOp '%d' unimplemented\n", exp->base.base.line, exp->op);  break; // Unreachable.
  }

  return loc;
}

static Location_ primary_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstPrimaryExp_* exp = (AstPrimaryExp_*)node;
  const Type_* ty = exp->base.sem_type.ty;
  
  TypedValue_ v = { 0 };

  if (type_is(ty, NilType_)) {
    v = (TypedValue_){ NIL_VAL, NIL_TY };
  } else if (type_is(ty, BoolType_)) {
    v = (TypedValue_){ exp->value.as.b ? TRUE_VAL : FALSE_VAL, BOOL_TY };
  } else {
    v = (TypedValue_){ exp->value, semantictype_toruntime(exp->base.sem_type)};
  }
  return emit_constant(chunk, v, exp->base.sem_type.ty->size, node->line);
}

static Location_ print_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstPrintStmt_* stmt = (AstPrintStmt_*)node;
  Location_ loc = code_gen(chunk, (AstNode_*)stmt->expr);
  assertf(!is_loc_empty(loc), "Encountered empty destination address");

  Operand_ op_l = OP_LOC(loc);
  op_l.opt_type = semantictype_toruntime(stmt->expr->sem_type);

  emit_tac(chunk, OP_PRINT, EMPTY_LOC, op_l, EMPTY_OPERAND, node->line);

  return EMPTY_LOC;
}

static Location_ program_code_gen(TacChunk_* chunk, AstNode_* node) {
  Frame_* frame = node->scope->frame;

  AstProgram_* program = (AstProgram_*)node;
  TacHandle_ prologue = emit_tac(chunk, OP_PROLOGUE, EMPTY_LOC, EMPTY_OPERAND, EMPTY_OPERAND, node->line);
  code_gen(chunk, (AstNode_*)program->block);
  
  chunk->code[prologue].arg_l = OP_SIZE(chunk->slot_offset);
  chunk->ret_loc = EMPTY_LOC;

  //emit_return(chunk, node->line);
  return EMPTY_LOC;
}

static Location_ stmt_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstStmt_* stmt = (AstStmt_*)node;
  code_gen(chunk, stmt->stmt);
  code_gen(chunk, stmt->cleanup);

  return EMPTY_LOC;
}

static Location_ expr_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstExpr_* expr = (AstExpr_*)node;
  if (!expr->expr) {
    return EMPTY_LOC;
  }
  return code_gen(chunk, (AstNode_*)expr->expr);
}

static void begin_scope(TacChunk_* chunk, AstBlock_* block) {
  emit_op(chunk, OP_BEGIN_SCOPE, block->base.line);
}

static void end_scope(TacChunk_* chunk, AstBlock_* block) {
  emit_op(chunk, OP_END_SCOPE, block->base.line);
}

static Location_ block_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstBlock_* block = (AstBlock_*)node;
  begin_scope(chunk, block);
  for (AstListNode_* n = block->statements.head; n != NULL; n = n->next) {
    code_gen(chunk, n->node);
  }
  end_scope(chunk, block);
  return EMPTY_LOC;
}

static Location_ return_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstReturnStmt_* stmt = (AstReturnStmt_*)node;
  Location_ dst = EMPTY_LOC;
  if (stmt->expr) {
    dst = code_gen(chunk, (AstNode_*)stmt->expr);
  }
  emit_set_variable(chunk, &chunk->ret_loc, &dst, stmt->expr->sem_type.ty->size, node->line);
  emit_return(chunk, &dst, node->line);
  return EMPTY_LOC;
}

static Location_ if_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstIfStmt_* stmt = (AstIfStmt_*)node;

  Location_ end_if = tac_alloc_label(chunk, "end_if");
  Location_ else_if = stmt->elif_exprs.count ? tac_alloc_label(chunk, "else_if") : EMPTY_LOC;
  Location_ final_else = stmt->else_stmt ? tac_alloc_label(chunk, "else") : EMPTY_LOC;
  Location_ next_else = loc_is_empty(else_if) ? (loc_is_empty(final_else) ?  end_if : final_else) : else_if;

  // if `condition_expr`
  Location_ condition_expr = code_gen(chunk, (AstNode_*)stmt->condition_expr);
  emit_tac(chunk, OP_JMP_IF_FALSE, next_else, OP_LOC(condition_expr), EMPTY_OPERAND, node->line);
  code_gen(chunk, stmt->if_stmt);

  if (stmt->else_stmt || stmt->elif_exprs.count) {
    emit_tac(chunk, OP_JMP, end_if, EMPTY_OPERAND, EMPTY_OPERAND, node->line);
  }

  // elif true then ...
  AstListNode_* elif_e = stmt->elif_exprs.head;
  AstListNode_* elif_s = stmt->elif_stmts.head;

  while (elif_e) {
    AstNode_* elif_expr = elif_e->node;
    AstNode_* elif_stmt = elif_s->node;
    
    emit_label(chunk, next_else, node->line);

    if (elif_e->next) {
      next_else = tac_alloc_label(chunk, "else_if");
    } else if (stmt->else_stmt) {
      next_else = final_else;
    } else {
      next_else = end_if;
    }

    Location_ elif_expr_val = code_gen(chunk, elif_expr);
    emit_tac(chunk, OP_JMP_IF_FALSE, next_else, OP_LOC(elif_expr_val), EMPTY_OPERAND, node->line);
    code_gen(chunk, elif_stmt);
    emit_tac(chunk, OP_JMP, end_if, EMPTY_OPERAND, EMPTY_OPERAND, node->line);

    elif_e = elif_e->next;
    elif_s = elif_s->next;
  }

  // else  
  if (stmt->else_stmt) {
    emit_label(chunk, final_else, node->line);
    code_gen(chunk, stmt->else_stmt);
  }
  emit_label(chunk, end_if, node->line);
  return EMPTY_LOC;
}

static Location_ assert_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstAssertStmt_* stmt = (AstAssertStmt_*)node;
  Location_ tmp = code_gen(chunk, (AstNode_*)stmt->expr);
  emit_tac(chunk, OP_ASSERT, EMPTY_LOC, OP_LOC(tmp), EMPTY_OPERAND, node->line);
  return EMPTY_LOC;
}

static int resolve_local(Scope_* scope, Token_* name) {
  VarSymbol_* var = scope_var(scope, name);
  assertf(var, "Could not find var %.*s", name->length, name->start);
  if (!var) return -1;

  return var->frame_index;
}

static int resolve_var(Scope_* scope, AstNode_* expr) {
  switch (((AstNode_*)expr)->cls) {
    case AST_CLS(AstVarExpr_):
    {
      AstVarExpr_* var_expr = AST_CAST(AstVarExpr_, expr);
      return resolve_var(scope, (AstNode_*)var_expr->expr);
    }

    case AST_CLS(AstIdExpr_):
    {
      AstIdExpr_* id_expr = AST_CAST(AstIdExpr_, expr);
      return resolve_local(scope, &id_expr->name);
    }

    case AST_CLS(AstDotExpr_):
    {
      AstDotExpr_* dot_expr = AST_CAST(AstDotExpr_, expr);
      ClassSymbol_* cls_sym = &dot_expr->prefix->sem_type.sym->cls;
      int offset = (int)symbol_findmember_offset(dot_expr->prefix->sem_type.sym, dot_expr->id);
      return resolve_var(scope, (AstNode_*)dot_expr->prefix) + offset;
    }

    default:
      assertf(false, "resolve_var unimplemented for AST_CLS(%d)", AS_EXPR(expr)->base.cls);
  }
}

static Location_ emit_decl_var(TacChunk_* chunk, Token_ var_name, Symbol_* var_sym, AstExpr_* opt_expr, int line) {
  SemanticType_ var_type = var_sym->var.sem_type;
  Location_ dst = tac_alloc(chunk, &var_type);
  dst.token = var_name;
  var_sym->var.location = dst;
  
  // TODO: allow for multiple expressions.
  if (opt_expr) {
    SemanticType_ rhs_type = opt_expr->sem_type;
    if (type_is(var_type.ty, VarType_)) {
      if (type_is(rhs_type.ty, VarType_)) {
        Location_ tmp = code_gen(chunk, (AstNode_*)opt_expr);
        emit_make_ref_from(chunk, &dst, &tmp, var_type.ty->size, line);
        return dst;
      } else {
        emit_make_ref(chunk, &dst, var_type.ty->size, line);
      }
    }

    Location_ tmp = code_gen(chunk, (AstNode_*)opt_expr);
    emit_set_variable(chunk, &dst, &tmp, var_type.ty->size, line);

    return dst;
  } else {
    if (type_is(var_type.ty, VarType_)) {
      emit_make_ref(chunk, &dst, var_type.ty->size, line);
    }
    return dst;
  }
}

static Location_ var_decl_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)node;
  Symbol_* sym = scope_find(node->scope, &stmt->name);

  return emit_decl_var(chunk, stmt->name, sym, stmt->expr, node->line);

  SemanticType_ var_type = sym->var.sem_type;
  Location_ dst = tac_alloc(chunk, &var_type);

  dst.token = stmt->name;
  sym->var.location = dst;
  
  // TODO: allow for multiple expressions.
  if (stmt->expr) {
    SemanticType_ rhs_type = stmt->expr->sem_type;
    if (type_is(var_type.ty, VarType_)) {
      if (type_is(rhs_type.ty, VarType_)) {
        Location_ tmp = code_gen(chunk, (AstNode_*)stmt->expr);
        emit_make_ref_from(chunk, &dst, &tmp, var_type.ty->size, node->line);
        return dst;
      } else {
        emit_make_ref(chunk, &dst, var_type.ty->size, node->line);
      }
    }

    Location_ tmp = code_gen(chunk, (AstNode_*)stmt->expr);
    emit_set_variable(chunk, &dst, &tmp, stmt->sem_type.ty->size, node->line);

    return dst;
  } else {
    if (type_is(var_type.ty, VarType_)) {
      emit_make_ref(chunk, &dst, var_type.ty->size, node->line);
    }
    return dst;
  }
}

static Location_ var_expr_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstVarExpr_* expr = (AstVarExpr_*)node;
  Location_ dst = code_gen(chunk, (AstNode_*)expr->expr);
  return dst;
}

// Get the specified variable.
static Location_ id_expr_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstIdExpr_* expr = (AstIdExpr_*)node;

  Symbol_* sym = scope_find(node->scope, &expr->name);

  switch (sym->type) {
    case SYMBOL_TYPE_VAR:
      return sym->var.location;
    case SYMBOL_TYPE_FN:
      return sym->fn.loc;
#if 0
    case SYMBOL_TYPE_CLOSURE:
      emit_bytes(chunk, OP_GET_VAR, (uint8_t)(sym->closure.frame_index), node->line);
      break;
#endif
    default:
      assertf(false, "Unknown symbol type %d for IdExpr", (int)sym->type);
  }

  return EMPTY_LOC;
}

// Set the specified variable.
static Location_ assignment_expr_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstAssignmentExpr_* expr = (AstAssignmentExpr_*)node;
  Location_ dst = code_gen(chunk, (AstNode_*)expr->left);
  Location_ src = code_gen(chunk, (AstNode_*)expr->right);

  emit_set_variable(chunk, &dst, &src, expr->left->sem_type.ty->size, node->line);
  return dst;
}

static Location_ while_stmt_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstWhileStmt_* stmt = (AstWhileStmt_*)node;

  // if `condition_expr`
  Location_ while_loop_begin = tac_alloc_label(chunk, "while_loop_begin");
  Location_ while_loop_end = tac_alloc_label(chunk, "while_loop_end");

  emit_label(chunk, while_loop_begin, stmt->condition_expr->base.line);
  Location_ condition = code_gen(chunk, (AstNode_*)stmt->condition_expr);

  emit_tac(chunk, OP_JMP_IF_FALSE, while_loop_end, OP_LOC(condition), EMPTY_OPERAND, stmt->condition_expr->base.line);

  // if true then ...
  code_gen(chunk, stmt->block_stmt);

  emit_tac(chunk, OP_JMP, while_loop_begin, EMPTY_OPERAND, EMPTY_OPERAND, stmt->block_stmt->line);
  emit_label(chunk, while_loop_end, stmt->block_stmt->line);
  return EMPTY_LOC;
}

static Location_ expression_statement_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstExpressionStmt_* stmt = (AstExpressionStmt_*)node;
  code_gen(chunk, (AstNode_*)stmt->expr);
  return EMPTY_LOC;
}

static Location_ function_def_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstFunctionDef_* def = (AstFunctionDef_*)node;
  FunctionSymbol_* fn = &def->fn_symbol->fn;

  Location_ fn_loc = tac_alloc_fn_label(chunk, &def->fn_symbol->name);
  fn->loc = fn_loc;

  //if (def->base.top_sem_type.kind != KIND_UNKNOWN) {
  //  fn_loc = tac_alloc_val(chunk, def->base.sem_type.ty->size);
  //}
  
  TacChunk_* fn_chunk = alloc_ty(chunk->allocator, TacChunk_);
  tac_chunk_init(fn_chunk, chunk->allocator);
  fn_chunk->ret_loc = tac_alloc_ret(fn_chunk);
  fn_chunk->slot_offset++;

  hashmap_set(chunk->fn_code, fn_loc.token.start, fn_loc.token.length, (uintptr_t)fn_chunk);
  code_gen(fn_chunk, (AstNode_*)def->body);  

  return fn_loc;
}

static Location_ function_param_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstFunctionParam_* param = AST_CAST(AstFunctionParam_, node);
  Symbol_* sym = scope_find(node->scope, &param->name);
  return emit_decl_var(chunk, param->name, sym, NULL, node->line);
}

static Location_ function_body_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstFunctionBody_* body = (AstFunctionBody_*)node;
  FunctionSymbol_* fn = &body->fn_symbol->fn;
  emit_label(chunk, fn->loc, node->line);
  TacHandle_ prologue = emit_tac(chunk, OP_PROLOGUE, EMPTY_LOC, EMPTY_OPERAND, EMPTY_OPERAND, node->line);
  for (AstListNode_* n = body->function_params.head; n != NULL; n = n->next) {
    code_gen(chunk, n->node);
  }

  code_gen(chunk, body->stmt);

  if (type_is(body->return_type.ty, NilType_)) {
    emit_tac(chunk, OP_RETURN, EMPTY_LOC, EMPTY_OPERAND, EMPTY_OPERAND, node->line);
  }
  
  chunk->code[prologue].arg_l = OP_SIZE(chunk->slot_offset);
  chunk->code[prologue].arg_r = OP_SIZE(fn->arg_size);
  //emit_tac(chunk, OP_EPILOGUE, EMPTY_LOC, EMPTY_OPERAND, EMPTY_OPERAND, node->line);
  return EMPTY_LOC;
}

static Location_ function_call_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstFunctionCall_* call = (AstFunctionCall_*)node;
  AstExpr_* prefix = call->prefix;
  Symbol_* sym = prefix->sem_type.sym;

  Symbol_* fn_sym = symbol_ascallable(sym);
  FunctionType_* fn_type = type_as(FunctionType_, fn_sym->ty);
  assertf(fn_sym, "Unknown symbol type.");


  // Push arguments.
  AstFunctionCallArgs_* args = call->args;
  Location_ args_locs[128];
  Location_ param_locs[128];
  int args_count = args->args.count;

  int i = 0;
  for (AstListNode_* n = args->args.head; n != NULL; n = n->next) {
    AstFunctionCallArg_* arg = AST_CAST(AstFunctionCallArg_, n->node);
    Location_ tmp = code_gen(chunk, n->node);
    //tmp = emit_cast_kind(chunk, &tmp, &arg->base.top_sem_type, &arg->base.sem_type, tmp.size, n->node->line);
    args_locs[i] = tmp;
    ++i;
  }

  Location_ ret_val = tac_current(chunk);
  Location_ ret_loc = tac_current(chunk);
  if (!type_is(fn_type->ret_ty, NilType_)) {
    ret_val = tac_alloc(chunk, fn_type->ret_ty);
    ret_loc = tac_alloc_ptr(chunk);
    emit_tac(chunk, OP_LOADA, ret_loc, OP_LOC(ret_val), EMPTY_OPERAND, node->line);
  } else {
    ret_loc = emit_nil(chunk, node->line);
  }

  // Get function to call.
  Location_ fn = code_gen(chunk, (AstNode_*)prefix);
  
  int64_t arg_size = 0;
  i = 0;
  for (AstListNode_* n = args->args.head; n != NULL; n = n->next) {
    AstFunctionCallArg_* arg = AST_CAST(AstFunctionCallArg_, n->node);
    param_locs[i] = tac_alloc(chunk, &arg->base.top_sem_type);
    arg_size += param_locs[i].size;
    ++i;
  }

  i = 0;
  for (AstListNode_* n = args->args.head; n != NULL; n = n->next) {
    AstFunctionCallArg_* arg = AST_CAST(AstFunctionCallArg_, n->node);
    emit_set_param(chunk, &param_locs[i], &args_locs[i], arg->expr->sem_type.ty->size, node->line);
    ++i;
  }

  emit_call(chunk, &ret_loc, &fn, arg_size, node->line);
  return ret_val;
}

static Location_ function_args_code_gen(TacChunk_* chunk, AstNode_* node) {
  return EMPTY_LOC;
}

static Location_ function_arg_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstFunctionCallArg_* arg = (AstFunctionCallArg_*)node;
  Location_ tmp = code_gen(chunk, (AstNode_*)arg->expr);
  Location_ dst = tmp;

//  SemanticType_* dst_type = &arg->base.top_sem_type;
//  SemanticType_* src_type = &arg->base.sem_type;
//  if (dst_type->kind != src_type->kind) {
//    switch (dst_type->kind) {
//      case KIND_VAL:
//        emit_set_variable(chu)
//        break;
//      case KIND_REF:
//        break;
//    }
//  }
  return dst;
}

static Location_ noop_code_gen(TacChunk_* chunk, AstNode_* node) { return EMPTY_LOC; }

static Location_ clean_up_temps_code_gen(TacChunk_* chunk, AstNode_* node) {
  return EMPTY_LOC;
}

static Location_ tmp_decl_code_gen(TacChunk_* chunk, AstNode_* n) {
  AstTmpDecl_* decl = (AstTmpDecl_*)n;
  int tmp_index = chunk->slot_index;
  Location_ ret = code_gen(chunk, (AstNode_*)decl->expr);
  chunk->slot_index = tmp_index;
  return ret;
}

// Returns the address of the class member.
static Location_ class_find_member_as_ptr(TacChunk_* chunk, const Location_* cls_loc, const Symbol_* cls_sym, const Token_* id, int line) {
  Symbol_* sym = symbol_findmember(cls_sym, *id);
  Location_ dst = tac_alloc_ptr(chunk);
  int offset = (int)sym->field.offset;

  switch (cls_loc->type) {
    case LOCATION_TYPE_VAL:
      emit_tac(chunk, OP_LOADA, dst, OP_LOC(*cls_loc), OP_OFFSET(offset), line);
      break;

    case LOCATION_TYPE_VAR:
      emit_tac(chunk, OP_RLOADA, dst, OP_LOC(*cls_loc), OP_OFFSET(offset), line);
      break;

    case LOCATION_TYPE_PTR:
      emit_tac(chunk, OP_MOVE, dst, OP_LOC(*cls_loc), EMPTY_OPERAND, line);
      emit_tac(chunk, OP_ADDIMM, dst, OP_LOC(dst), OP_OFFSET(offset * sizeof(Value_)), line);
      break;
  }

  return dst;
}

static Location_ class_find_member_as_val(TacChunk_* chunk, const Location_* cls_loc, const Symbol_* cls_sym, const Token_* id, int line) {
  Symbol_* sym = symbol_findmember(cls_sym, *id);
  Location_ tmp = class_find_member_as_ptr(chunk, cls_loc, cls_sym, id, line);

  Location_ dst = tac_alloc_val(chunk, sym->ty->size);
  if (dst.size == 1) {
    emit_tac(chunk, OP_LOAD, dst, OP_LOC(tmp), OP_OFFSET(0), line);
    return dst;
  }

  Location_ dst_ptr = tac_alloc_ptr(chunk);
  emit_tac(chunk, OP_LOADA, dst_ptr, OP_LOC(dst), EMPTY_OPERAND, line);
  emit_tac(chunk, OP_MEMCPY, dst_ptr, OP_LOC(tmp), OP_SIZE(dst.size), line);

  return dst;
}

static Location_ class_find_member_as_var(TacChunk_* chunk, const Location_* cls_loc, const Symbol_* cls_sym, const Token_* id, int line) {
  Symbol_* sym = symbol_findmember(cls_sym, *id);
  Location_ tmp = class_find_member_as_ptr(chunk, cls_loc, cls_sym, id, line);
  Location_ dst = tac_alloc_var(chunk);
  emit_tac(chunk, OP_MOVE, dst, OP_LOC(tmp), EMPTY_OPERAND, line);
  return dst;
}

static Location_ class_set_member(TacChunk_* chunk, const Location_* val, const Location_* cls_loc, const Symbol_* cls_sym, const Token_* id, int line) {
  Symbol_* sym = symbol_findmember(cls_sym, *id);
  Location_ dst = class_find_member_as_ptr(chunk, cls_loc, cls_sym, id, line);

  if (val->size == 1) {
    emit_tac(chunk, OP_STORE, dst, OP_LOC(*val), OP_OFFSET(0), line);
    return dst;
  }

  Location_ tmp = tac_alloc_ptr(chunk);
  emit_tac(chunk, OP_LOADA, tmp, OP_LOC(*val), EMPTY_OPERAND, line);
  emit_tac(chunk, OP_MEMCPY, dst, OP_LOC(tmp), OP_SIZE(sym->ty->size), line);

  return dst;
}

static Location_ dot_expr_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstDotExpr_* expr = AST_CAST(AstDotExpr_, node);
  Location_ cls_loc = code_gen(chunk, (AstNode_*)expr->prefix);

  if (type_isaref(expr->base.top_sem_type.ty)) {
    return class_find_member_as_ptr(chunk, &cls_loc, expr->cls_sym, &expr->id, node->line);
  } else if (type_is(expr->base.top_sem_type.ty, VarType_)) {
    return class_find_member_as_var(chunk, &cls_loc, expr->cls_sym, &expr->id, node->line);
  } else {
    return class_find_member_as_val(chunk, &cls_loc, expr->cls_sym, &expr->id, node->line);
  }
}

static void emit_construct_default_class(TacChunk_* chunk, const Symbol_* cls_sym, const Location_* dst, int line);

static void class_init_member(TacChunk_* chunk, const Location_* cls_loc, const Location_* field_ptr, const Symbol_* cls_sym, const Symbol_* field_sym, int line) {
  const FieldSymbol_* field = &field_sym->field;
  if (type_is(field_sym->ty, VarType_)) {
    Location_ tmp = tac_alloc_var(chunk);
    emit_make_ref(chunk, &tmp, field_sym->ty->size, line);
    class_set_member(chunk, &tmp, cls_loc, cls_sym, &field->name, line);
  }

  if (type_is(field_sym->ty, ClassType_)) {
    emit_construct_default_class(chunk, field_sym->ty, field_ptr, line);
  } else {
    assertf(field_sym->ty->size == 1, "");
    Location_ tmp = emit_nil(chunk, line);
    emit_set_variable(chunk, field_ptr, &tmp, field->sem_type.size, line);
  }
}

static void class_init_member_to(TacChunk_* chunk, const Location_* cls_loc, const Location_* field_ptr, const Location_* val, const Symbol_* cls_sym, const Symbol_* field_sym, int line) {
  const FieldSymbol_* field = &field_sym->field;
  if (field->sem_type.kind == KIND_VAR) {
    Location_ tmp = tac_alloc_var(chunk);
    emit_make_ref(chunk, &tmp, field->sem_type.size, line);
    class_set_member(chunk, &tmp, cls_loc, cls_sym, &field->name, line);
  }

  emit_set_variable(chunk, field_ptr, val, field->sem_type.size, line);
}

static void emit_construct_default_class(TacChunk_* chunk, const Type_* type, const Location_* dst, int line) {
  ClassType_* cls_type = type_as(ClassType_, type);

  Location_ dst_ptr;
  if (dst->type == LOCATION_TYPE_PTR) {
    dst_ptr = *dst;
  } else if (dst->type == LOCATION_TYPE_VAL) {
    dst_ptr = tac_alloc_ptr(chunk);
    emit_tac(chunk, OP_LOADA, dst_ptr, OP_LOC(*dst), EMPTY_OPERAND, line);
  } else if (dst->type == LOCATION_TYPE_VAR) {
    dst_ptr = tac_alloc_var(chunk);
    emit_tac(chunk, OP_RLOADA, dst_ptr, OP_LOC(*dst), EMPTY_OPERAND, line);
  }
  
  for (ListNode_* n = cls_type->members.head; n != NULL; n = n->next) {
    Symbol_* field = list_val(n, Symbol_*);
    FieldSymbol_* field_sym = &field->field;
    Location_ field_ptr = class_find_member_as_ptr(chunk, &dst_ptr, cls_sym, &field->name, line);
    class_init_member(chunk, dst, &field_ptr, cls_sym, field, line);
  }
}

static Location_ class_constructor_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstClassConstructor_* constructor = (AstClassConstructor_*)node;
  Type_* type = &constructor->base.sem_type.ty;

  Location_ dst = tac_alloc_val(chunk, type->size);
  if (type->opt_name.start) {
    Symbol_* cls_sym = type->sym;

    AstListNode_* current_param_node;
    ListNode_* current_member_node = cls_sym->cls.members.head;

    // The syntax is that all named constructor paramters must be at the end.
    // So first, generate code for all the unnamed parameters.
    for (current_param_node = constructor->params.head; current_param_node != NULL; current_param_node = current_param_node->next) {
      AstClassConstructorParam_* constructor_field = AST_CAST(AstClassConstructorParam_, current_param_node->node);
      if (constructor_field->name.start) {
        break;
      }

      Symbol_* member = list_val(current_member_node, Symbol_*);
      Location_ field_ptr = class_find_member_as_ptr(chunk, &dst, cls_sym, &member->name, node->line);
      Location_ tmp = code_gen(chunk, current_param_node->node);
      class_init_member_to(chunk, &dst, &field_ptr, &tmp, cls_sym, member, node->line);

      current_member_node = current_member_node->next;
    }

    // If the unnamed parameters iterator is at the end, that means there are
    // no named parameters.
    if (!current_member_node) {
      return dst;
    }

    // There are named parameters, so loop through the rest of the class
    // members we haven't seen yet to try and find matches.
    for (;
         current_member_node != NULL;
         current_member_node = current_member_node->next) {
      Symbol_* member = list_val(current_member_node, Symbol_*);
      FieldSymbol_* field = &member->field;

      bool found = false;
      for (AstListNode_* param_node = current_param_node; param_node != NULL; param_node = param_node->next) {
        AstClassConstructorParam_* constructor_param = AST_CAST(AstClassConstructorParam_, param_node->node);
        if (token_eq(constructor_param->name, member->name)) {
          Symbol_* member = list_val(current_member_node, Symbol_*);
          Location_ field_ptr = class_find_member_as_ptr(chunk, &dst, cls_sym, &member->name, node->line);
          Location_ tmp = code_gen(chunk, param_node->node);
          class_set_member(chunk, &tmp, &dst, cls_sym, &member->field.name, node->line);
          class_init_member_to(chunk, &dst, &field_ptr, &tmp, cls_sym, member, node->line);
          found = true;
          break;
        }
      }

      // It could be that the user did not write an initializer for the
      // parameter. In this case, create the default value.
      if (!found) {
        FieldSymbol_* field = &list_val(current_member_node, Symbol_*)->field;
        Location_ field_ptr = class_find_member_as_ptr(chunk, &dst, cls_sym, &field->name, node->line);
        
        if (field->opt_expr) {
          Location_ val = code_gen(chunk, (AstNode_*)field->opt_expr);
          class_init_member_to(chunk, &dst, &field_ptr, &val, cls_sym, member, node->line);
        } else {
          class_init_member(chunk, &dst, &field_ptr, cls_sym, member, node->line);
        }
      }
    }
  }

  return dst;
}

static Location_ class_constructor_field_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstClassConstructorParam_* field = (AstClassConstructorParam_*)node;
  return code_gen(chunk, (AstNode_*)field->expr);
}

static Location_ array_value_code_gen(TacChunk_* chunk, AstNode_* node) {
  return EMPTY_LOC;
}

static CodeGenRule_ code_gen_rules[] = {
  [AST_CLS(AstProgram_)]               = {program_code_gen},
  [AST_CLS(AstBlock_)]                 = {block_code_gen},
  [AST_CLS(AstStmt_)]                  = {stmt_code_gen},
  [AST_CLS(AstExpr_)]                  = {expr_code_gen},
  [AST_CLS(AstPrintStmt_)]             = {print_code_gen},
  [AST_CLS(AstUnaryExp_)]              = {unary_code_gen},
  [AST_CLS(AstBinaryExp_)]             = {binary_code_gen},
  [AST_CLS(AstPrimaryExp_)]            = {primary_code_gen},
  [AST_CLS(AstReturnStmt_)]            = {return_code_gen},
  [AST_CLS(AstIfStmt_)]                = {if_code_gen},
  [AST_CLS(AstAssertStmt_)]            = {assert_code_gen},
  [AST_CLS(AstVarDeclStmt_)]           = {var_decl_code_gen},
  [AST_CLS(AstVarExpr_)]               = {var_expr_code_gen},
  [AST_CLS(AstIdExpr_)]                = {id_expr_code_gen},
  [AST_CLS(AstAssignmentExpr_)]        = {assignment_expr_code_gen},
  [AST_CLS(AstWhileStmt_)]             = {while_stmt_code_gen},
  [AST_CLS(AstFunctionDef_)]           = {function_def_code_gen},
  [AST_CLS(AstFunctionBody_)]          = {function_body_code_gen},
  [AST_CLS(AstFunctionParam_)]         = {function_param_code_gen},
  [AST_CLS(AstFunctionCall_)]          = {function_call_code_gen},
  [AST_CLS(AstFunctionCallArgs_)]      = {function_args_code_gen},
  [AST_CLS(AstFunctionCallArg_)]       = {function_arg_code_gen},
  [AST_CLS(AstExpressionStmt_)]        = {expression_statement_code_gen},
  [AST_CLS(AstNoopExpr_)]              = {noop_code_gen},
  [AST_CLS(AstNoopStmt_)]              = {noop_code_gen},
  [AST_CLS(AstCleanUpTemps_)]          = {clean_up_temps_code_gen},
  [AST_CLS(AstTmpDecl_)]               = {tmp_decl_code_gen},
  [AST_CLS(AstClassDef_)]              = {noop_code_gen},
  [AST_CLS(AstClassMemberDecl_)]       = {noop_code_gen},
  [AST_CLS(AstClassConstructor_)]      = {class_constructor_code_gen},
  [AST_CLS(AstClassConstructorParam_)] = {class_constructor_field_code_gen},
  [AST_CLS(AstDotExpr_)]               = {dot_expr_code_gen},
  [AST_CLS(AstTypeExpr_)]              = {noop_code_gen},
  [AST_CLS(AstArrayValueExpr_)]        = {array_value_code_gen},
};

// Static assert to make sure that all node types are accounted for.
STATIC_ASSERT(
  sizeof(code_gen_rules) / sizeof(CodeGenRule_) == __AST_NODE_COUNT__,
  CHECK_CODE_GEN_COUNT);

static CodeGenRule_* get_rule(int info) {
  CodeGenRule_* ret = &code_gen_rules[info];
  assertf(ret->code_gen, "Could not find code gen rule for AST class: %d", info);
  return ret;
}