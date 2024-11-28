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
    printf("_t%d", loc->frame_offset);
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
    printf(" " OP_CHAR " "); \
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

  tac_compiler_print(compiler);
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

static Location_ emit_true(TacChunk_* chunk, int line) {
  Location_ ret = tac_alloc_val(chunk, 1);
  emit_tac(chunk, OP_TRUE, ret, EMPTY_OPERAND, EMPTY_OPERAND, line);
  return ret;
}

static Location_ emit_nil(TacChunk_* chunk, int line) {
  Location_ ret = tac_alloc_val(chunk, 1);
  emit_tac(chunk, OP_NIL, ret, EMPTY_OPERAND, EMPTY_OPERAND, line);
  return ret;
}

static Location_ emit_constant(TacChunk_* chunk, TypedValue_ value, size_t size, int line) {
  return tac_chunk_writeconstant(chunk, value, size, line);
}

static void emit_make_ref(TacChunk_* chunk, const Location_* dst, Type_* ty, int line) {
  assertf(dst->type == LOCATION_TYPE_VAR, "");
  assertf(type_is(ty, VarType_), "");
  emit_tac(chunk, OP_REF_MAKE, *dst, OP_SIZE(type_valtype(ty)->size), EMPTY_OPERAND, line);
}

static void emit_make_ref_from(TacChunk_* chunk, const Location_* dst, const Location_* src, size_t size, int line) {
  assertf(dst->type == LOCATION_TYPE_VAR, "");
  assertf(src->type == LOCATION_TYPE_VAR, "");
  emit_tac(chunk, OP_REF_INC, EMPTY_LOC, OP_LOC(*src), EMPTY_OPERAND, line);
  emit_tac(chunk, OP_MOVE, *dst, OP_LOC(*src), EMPTY_OPERAND, line);
}

static Location_ try_emit_cast(TacChunk_* chunk, Location_ val, const Type_* from, const Type_* to, size_t size, int line) {
  OpCode cast_op = OP_NOP;
  if (from->cls == to->cls) {
    return EMPTY_LOC;
  }

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
  if (cast_op == OP_NOP) {
    return EMPTY_LOC;
  }

  Location_ dst = tac_alloc_val(chunk, size);
  emit_tac(chunk, cast_op, dst, OP_LOC(val), EMPTY_OPERAND, line);
  return dst;
}

static Location_ emit_cast(TacChunk_* chunk, Location_ val, const Type_* from, const Type_* to, size_t size, int line) {
  Location_ dst = try_emit_cast(chunk, val, from, to, size, line);
  assertf(!loc_is_empty(dst), "Trying an unknown cast. From: %d. To: %d", from->cls, to->cls);
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

static void emit_set_variable(TacChunk_* chunk, const Location_* dst, const Location_* src, Type_* ty, int line) {
  size_t val_size = type_valtype(ty)->size;
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

static Location_ emit_get_var(TacChunk_* chunk, const Location_* src, Type_* ty, int line) {
  assertf(src->type == LOCATION_TYPE_VAR, "");
  Location_ dst = tac_alloc_val(chunk, type_valtype(ty)->size);
  emit_set_variable(chunk, &dst, src, ty, line);
  return dst;
}

static Location_ emit_get_ptr(TacChunk_* chunk, const Location_* src, Type_* ty, int line) {
  assertf(src->type == LOCATION_TYPE_PTR, "");
  Location_ dst = tac_alloc_val(chunk, type_valtype(ty)->size);
  emit_set_variable(chunk, &dst, src, ty, line);
  return dst;
}

static Location_ emit_get_variable(TacChunk_* chunk, const Location_* src, Type_* ty, int line) {
  switch (src->type) {
    case LOCATION_TYPE_VAL:
      return *src;
    case LOCATION_TYPE_VAR:
      return emit_get_var(chunk, src, ty, line);
    case LOCATION_TYPE_PTR:
      return emit_get_ptr(chunk, src, ty, line);
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
  } else if (type_is(ltype, StringType_)) {
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
  if (type_isunsigned(ltype)) {
    op = OP_MUL;
  } else if (type_issigned(ltype)) {
    op = OP_IMUL;
  } else if (type_is(ltype, FloatType_)) {
    op = OP_FMUL;
  } else if (type_is(ltype, DoubleType_)) {
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
  if (type_isunsigned(ltype)) {
    op = OP_DIV;
  } else if (type_issigned(ltype)) {
    op = OP_IDIV;
  } else if (type_is(ltype, FloatType_)) {
    op = OP_FDIV;
  } else if (type_is(ltype, DoubleType_)) {
    op = OP_DDIV;
  }
  emit_tac(chunk, op, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_idiv(TacChunk_* chunk, Location_ l, Location_ r, const Type_* ltype, const Type_* rtype, size_t type_size, int line) {
  Location_ tmp = emit_div(chunk, l, r, ltype, rtype, type_size, line);

  if (type_isareal(ltype)) {
    // Convert the float or double to 4 or 8 byte int respectively.
    const Type_* int_ty = type_is(ltype, FloatType_) ? Int32_Ty : Int64_Ty;
    return emit_cast(chunk, tmp, ltype, int_ty, type_size, line);
  }

  return tmp;
}

static Location_ emit_mod(TacChunk_* chunk, Location_ l, Location_ r, const Type_* ltype, const Type_* rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  if (ltype->cls != rtype->cls) {
    r = emit_cast(chunk, r, rtype, ltype, type_size, line);
  }

  OpCode op = OP_NIL;
  if (type_isunsigned(ltype)) {
    op = OP_MOD;
  } else if (type_issigned(ltype)) {
    op = OP_IMOD;
  }
  emit_tac(chunk, op, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_neg(TacChunk_* chunk, Location_ val, const Type_* ty, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  OpCode op = OP_NIL;
  if (type_is(ty, FloatType_)) {
    op = OP_FNEG;
  } else if (type_is(ty, DoubleType_)) {
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

static void emit_del(TacChunk_* chunk, Location_ val, int line) {
  emit_tac(chunk, OP_REF_DEL, val, EMPTY_OPERAND, EMPTY_OPERAND, line);
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

static Location_ emit_cmp(TacChunk_* chunk, Location_ l, Location_ r, const Type_* ltype, const Type_* rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc_val(chunk, type_size);
  if (ltype->cls != rtype->cls) {
    r = emit_cast(chunk, r, rtype, ltype, type_size, line);
  }

  OpCode op = OP_NIL;
  if (type_isunsigned(ltype)) {
    op = OP_CMP;
  } else if (type_issigned(ltype)) {
    op = OP_ICMP;
  } else if (type_is(ltype, FloatType_)) {
    op = OP_FCMP;
  } else if (type_is(ltype, DoubleType_)) {
    op = OP_DCMP;
  }
  emit_tac(chunk, op, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_gt(TacChunk_* chunk, Location_ l, Location_ r, const Type_* ltype, const Type_* rtype, size_t type_size, int line) {
  Location_ tmp = emit_cmp(chunk, l, r, ltype, rtype, type_size, line);
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_GT, dest, OP_LOC(tmp), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_gte(TacChunk_* chunk, Location_ l, Location_ r, const Type_* ltype, const Type_* rtype, size_t type_size, int line) {
  Location_ tmp = emit_cmp(chunk, l, r, ltype, rtype, type_size, line);
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_GTE, dest, OP_LOC(tmp), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_lt(TacChunk_* chunk, Location_ l, Location_ r, const Type_* ltype, const Type_* rtype, size_t type_size, int line) {
  Location_ tmp = emit_cmp(chunk, l, r, ltype, rtype, type_size, line);
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_LT, dest, OP_LOC(tmp), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_lte(TacChunk_* chunk, Location_ l, Location_ r, const Type_* ltype, const Type_* rtype, size_t type_size, int line) {
  Location_ tmp = emit_cmp(chunk, l, r, ltype, rtype, type_size, line);
  Location_ dest = tac_alloc_val(chunk, type_size);
  emit_tac(chunk, OP_LTE, dest, OP_LOC(tmp), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_binary_op(TacChunk_* chunk, TokenType_ op, Location_ l_loc, Location_ r_loc, Type_* ltype, Type_* rtype, Type_* dst_type, int line) {
  Location_ l_orig = l_loc;
  Location_ r_orig = r_loc;

  if (l_loc.type != LOCATION_TYPE_VAL) {
    l_loc = emit_get_variable(chunk, &l_loc, ltype, line);
  }

  if (r_loc.type != LOCATION_TYPE_VAL) {
    r_loc = emit_get_variable(chunk, &r_loc, rtype, line);
  }

  size_t type_size = dst_type->size;
  switch (op) {
    case TK_AND:           return emit_and(chunk, l_loc, r_loc, type_size, line);
    case TK_OR:            return emit_or(chunk, l_loc, r_loc, type_size, line);
    case TK_XOR:           return emit_xor(chunk, l_loc, r_loc, type_size, line);
    case TK_PLUS:          return emit_add(chunk, l_loc, r_loc, ltype, rtype, type_size, line);
    case TK_MINUS:         return emit_sub(chunk, l_loc, r_loc, ltype, rtype, type_size, line);
    case TK_STAR:          return emit_mul(chunk, l_loc, r_loc, ltype, rtype, type_size, line);
    case TK_SLASH:         return emit_div(chunk, l_loc, r_loc, ltype, rtype, type_size, line);
    case TK_DOUBLE_SLASH:  return emit_idiv(chunk, l_loc, r_loc, ltype, rtype, type_size, line);
    case TK_PERCENT:       return emit_mod(chunk, l_loc, r_loc, ltype, rtype, type_size, line);
    case TK_EQUAL_EQUAL:   return emit_eq(chunk, l_orig, r_orig, l_loc, r_loc, type_size, line);
    case TK_BANG_EQUAL:    return emit_neq(chunk, l_orig, r_orig, l_loc, r_loc, type_size, line);
    case TK_GT:            return emit_gt(chunk, l_loc, r_loc, ltype, rtype, type_size, line);
    case TK_GTE:           return emit_gte(chunk, l_loc, r_loc, ltype, rtype, type_size, line);
    case TK_LT:            return emit_lt(chunk, l_loc, r_loc, ltype, rtype, type_size, line);
    case TK_LTE:           return emit_lte(chunk, l_loc, r_loc, ltype, rtype, type_size, line);
    case TK_LSHIFT:        return emit_lshift(chunk, l_loc, r_loc, type_size, line);
    case TK_RSHIFT:        return emit_rshift(chunk, l_loc, r_loc, type_size, line);
    case TK_AMPERSAND:     return emit_bit_and(chunk, l_loc, r_loc, type_size, line);
    case TK_PIPE:          return emit_bit_or(chunk, l_loc, r_loc, type_size, line);
    case TK_HAT:           return emit_bit_xor(chunk, l_loc, r_loc, type_size, line);
    default: printf("[Line %d] BinaryOp '%d' unimplemented\n", line, op);  break; // Unreachable.
  }
  return EMPTY_LOC;
}

static Location_ binary_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstBinaryExp_* exp = (AstBinaryExp_*)node;
  AstExpr_* l = AS_EXPR(exp->left);
  AstExpr_* r = AS_EXPR(exp->right);

  Type_* ltype = type_valtype(l->type);
  Type_* rtype = type_valtype(r->type);

  Location_ l_loc = code_gen(chunk, (AstNode_*)l);
  Location_ r_loc = code_gen(chunk, (AstNode_*)r);
  Location_ l_orig = l_loc;
  Location_ r_orig = r_loc;

  if (l_loc.type != LOCATION_TYPE_VAL) {
    l_loc = emit_get_variable(chunk, &l_loc, ltype, node->line);
  }

  if (r_loc.type != LOCATION_TYPE_VAL) {
    r_loc = emit_get_variable(chunk, &r_loc, rtype, node->line);
  }

  size_t type_size = exp->base.type->size;
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

  Type_* type = e->type;
  Location_ loc = code_gen(chunk, (AstNode_*)e);
  Location_ orig_loc = loc;

  if (loc.type != LOCATION_TYPE_VAL && exp->op != TK_DEL) {
    loc = emit_get_variable(chunk, &loc, type, node->line);
  }

  size_t type_size = type->size;
  switch (exp->op) {
    case TK_BANG:  return emit_not(chunk, loc, loc.size, node->line);
    case TK_MINUS: return emit_neg(chunk, loc, type, loc.size, node->line);
    case TK_NOT:  return emit_not(chunk, loc, loc.size, node->line);
    case TK_TILDE: return emit_bit_not(chunk, loc, loc.size, node->line);
    case TK_DEL: emit_del(chunk, orig_loc, node->line); break;

    default: printf("[Line %d] UnaryOp '%d' unimplemented\n", exp->base.base.line, exp->op);  break; // Unreachable.
  }

  return loc;
}

static Location_ primary_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstPrimaryExp_* exp = (AstPrimaryExp_*)node;
  Type_* ty = exp->base.type;
  
  TypedValue_ v = { 0 };

  if (type_is(ty, NilType_)) {
    v = (TypedValue_){ NIL_VAL, type_toruntime(Nil_Ty)};
  } else if (type_is(ty, BoolType_)) {
    v = (TypedValue_){ exp->value.as.b ? TRUE_VAL : FALSE_VAL, type_toruntime(Bool_Ty) };
  } else {
    v = (TypedValue_){ exp->value, type_toruntime(exp->base.type)};
  }
  return emit_constant(chunk, v, exp->base.type->size, node->line);
}

static Location_ print_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstPrintStmt_* stmt = (AstPrintStmt_*)node;
  Location_ loc = code_gen(chunk, (AstNode_*)stmt->expr);
  assertf(!is_loc_empty(loc), "Encountered empty destination address");

  Operand_ op_l = OP_LOC(loc);
  op_l.opt_type = type_toruntime(stmt->expr->type);

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
    emit_set_variable(chunk, &chunk->ret_loc, &dst, stmt->expr->type, node->line);
  }
  
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

static void emit_init_var(TacChunk_* chunk, const Location_* var, Type_* var_type, AstExpr_* opt_expr, int line) {
  // TODO: allow for multiple expressions.
  if (opt_expr) {
    Type_* rhs_type = opt_expr->type;
    if (type_is(var_type, VarType_)) {
      if (type_is(rhs_type, VarType_)) {
        Location_ tmp = code_gen(chunk, (AstNode_*)opt_expr);
        emit_make_ref_from(chunk, var, &tmp, var_type->size, line);
        return;
      } else {
        emit_make_ref(chunk, var, var_type, line);
      }
    }

    Location_ tmp = code_gen(chunk, (AstNode_*)opt_expr);
    emit_set_variable(chunk, var, &tmp, var_type, line);

  } else {
    if (type_is(var_type, VarType_)) {
      emit_make_ref(chunk, var, var_type, line);
    }
  }
}

static Location_ emit_decl_var(TacChunk_* chunk, Token_ var_name, Symbol_* var_sym, AstExpr_* opt_expr, int line) {
  Type_* var_type = var_sym->ty;
  Location_ dst = tac_alloc(chunk, var_type);
  dst.token = var_name;
  emit_init_var(chunk, &dst, var_type, opt_expr, line);
  var_sym->var.location = dst;
  
  return dst;
}

static Location_ var_decl_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)node;
  Symbol_* sym = scope_find(node->scope, &stmt->name);

  return emit_decl_var(chunk, stmt->name, sym, stmt->expr, node->line);
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
    case SYMBOL_CLS_VAR:
      return sym->var.location;
    case SYMBOL_CLS_FN:
      return sym->fn.loc;
#if 0
    case SYMBOL_CLS_CLOSURE:
      emit_bytes(chunk, OP_GET_VAR, (uint8_t)(sym->closure.frame_index), node->line);
      break;
#endif
    default:
      assertf(false, "Unknown symbol type %d for IdExpr", (int)sym->type);
  }

  return EMPTY_LOC;
}

Location_ array_get_as_ptr(TacChunk_* chunk, Location_* array, Location_* index, ArrayType_* array_ty, int line) {
  Location_ prefix_ptr = tac_alloc_ptr(chunk);
  switch (array->type) {
    case LOCATION_TYPE_VAL:
      emit_tac(chunk, OP_LOADA, prefix_ptr, OP_LOC(*array), EMPTY_OPERAND, line);
      break;

    case LOCATION_TYPE_PTR:
      emit_tac(chunk, OP_MOVE, prefix_ptr, OP_LOC(*array), EMPTY_OPERAND, line);
      break;

    case LOCATION_TYPE_VAR:
      emit_tac(chunk, OP_RLOADA, prefix_ptr, OP_LOC(*array), EMPTY_OPERAND, line);
      break;
  }

  Type_* el_type = array_ty->el_type;
  size_t el_size = el_type->size * sizeof(Value_);

  emit_tac(chunk, OP_LEA, prefix_ptr, OP_LOC(*index), OP_SIZE(el_size), line);
  return prefix_ptr;
}

static Location_ array_get_as_ref(TacChunk_* chunk, Location_* array, Location_* index, ArrayType_* array_ty, int line) {
  Location_ ptr = array_get_as_ptr(chunk, array, index, array_ty, line);
  Type_* el_type = array_ty->el_type;
  if (type_is(el_type, VarType_)) {
    Location_ dst = tac_alloc_ptr(chunk);
    Location_ tmp = tac_alloc_ptr(chunk);
    emit_tac(chunk, OP_LOAD, tmp, OP_LOC(ptr), OP_OFFSET(0), line);
    emit_tac(chunk, OP_RLOADA, dst, OP_LOC(tmp), OP_OFFSET(0), line);
    return dst;
  } else {
    return ptr;
  }
}

static Location_ array_get_as_val(TacChunk_* chunk, Location_* array, Location_* index, ArrayType_* array_ty,  int line) {
  Location_ ptr = array_get_as_ptr(chunk, array, index, array_ty, line);
  Type_* el_type = array_ty->el_type;
  Location_ dst = tac_alloc(chunk, el_type);
  if (dst.size == 1) {
    emit_tac(chunk, OP_LOAD, dst, OP_LOC(ptr), OP_OFFSET(0), line);
    return dst;
  }

  Location_ dst_ptr = tac_alloc_ptr(chunk);
  emit_tac(chunk, OP_LOADA, dst_ptr, OP_LOC(dst), EMPTY_OPERAND, line);
  emit_tac(chunk, OP_MEMCPY, dst_ptr, OP_LOC(ptr), OP_SIZE(dst.size), line);

  return dst;
}

static Location_ array_get_as_var(TacChunk_* chunk, Location_* array, Location_* index, ArrayType_* array_ty, int line) {
  Location_ ptr = array_get_as_ptr(chunk, array, index, array_ty, line);
  Location_ dst = tac_alloc_var(chunk);
  emit_tac(chunk, OP_LOAD, dst, OP_LOC(ptr), EMPTY_OPERAND, line);
  return dst;
}

static Location_ index_expr_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstIndexExpr_* expr = (AstIndexExpr_*)node;  
  Location_ prefix = code_gen(chunk, (AstNode_*)expr->prefix);
  Location_ tmp = code_gen(chunk, (AstNode_*)expr->index);
  Location_ index;
  if (tmp.type == LOCATION_TYPE_VAL) {
    index = tmp;
  } else {
    index = tac_alloc_val(chunk, Int_Ty->size);
    emit_set_variable(chunk, &index, &tmp, expr->index->type, node->line);
  }

  ArrayType_* array_ty = type_as(ArrayType_, type_valtype(expr->prefix->type));
  if (type_isaref(expr->base.top_type)) {
    return array_get_as_ref(chunk, &prefix, &index, array_ty, node->line);
  } else if (type_is(expr->base.top_type, VarType_)) {
    return array_get_as_var(chunk, &prefix, &index, array_ty, node->line);
  } else {
    return array_get_as_val(chunk, &prefix, &index, array_ty, node->line);
  }
}

// Set the specified variable.
static Location_ assignment_expr_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstAssignmentExpr_* expr = (AstAssignmentExpr_*)node;
  Location_ dst = code_gen(chunk, (AstNode_*)expr->left);
  Location_ src = code_gen(chunk, (AstNode_*)expr->right);

  Type_* ltype = type_valtype(expr->left->type);
  Type_* rtype = type_valtype(expr->right->type);

  Location_ maybe_tmp = try_emit_cast(chunk, src, rtype, ltype, ltype->size, node->line);
  if (!loc_is_empty(maybe_tmp)) {
    src = maybe_tmp;
  }

  emit_set_variable(chunk, &dst, &src, expr->left->type, node->line);
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

static Location_ for_stmt_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstForStmt_* stmt = (AstForStmt_*)node;

  // if `condition_expr`
  Location_ for_loop_begin = tac_alloc_label(chunk, "for_loop_begin");
  Location_ for_loop_end = tac_alloc_label(chunk, "for_loop_end");

  if (stmt->opt_var_decl) {
    code_gen(chunk, stmt->opt_var_decl);
  }

  emit_label(chunk, for_loop_begin, node->line);
  Location_ condition = EMPTY_LOC;
  int condition_line = node->line;
  if (stmt->opt_condition_expr) {
    condition = code_gen(chunk, (AstNode_*)stmt->opt_condition_expr);
    condition_line = stmt->opt_condition_expr->base.line;
  } else {
    condition = emit_true(chunk, node->line);
  }

  emit_tac(chunk, OP_JMP_IF_FALSE, for_loop_end, OP_LOC(condition), EMPTY_OPERAND, condition_line);

  // if true then ...
  code_gen(chunk, stmt->block_stmt);

  if (stmt->opt_step_expr) {
    code_gen(chunk, (AstNode_*)stmt->opt_step_expr);
  }

  emit_tac(chunk, OP_JMP, for_loop_begin, EMPTY_OPERAND, EMPTY_OPERAND, stmt->block_stmt->line);
  emit_label(chunk, for_loop_end, stmt->block_stmt->line);
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
  
  emit_label(chunk, fn->loc, node->line);
  TacHandle_ prologue = emit_tac(chunk, OP_PROLOGUE, EMPTY_LOC, EMPTY_OPERAND, EMPTY_OPERAND, node->line);
  for (AstListNode_* n = def->function_params.head; n != NULL; n = n->next) {
    code_gen(chunk, n->node);
  }

  code_gen(chunk, def->body);

  if (type_is(def->return_type, NilType_)) {
    emit_tac(chunk, OP_RETURN, EMPTY_LOC, EMPTY_OPERAND, EMPTY_OPERAND, node->line);
  }

  chunk->code[prologue].arg_l = OP_SIZE(chunk->slot_offset);
  chunk->code[prologue].arg_r = OP_SIZE(fn->arg_size);
  //emit_tac(chunk, OP_EPILOGUE, EMPTY_LOC, EMPTY_OPERAND, EMPTY_OPERAND, node->line);

  return fn_loc;
}

static Location_ function_param_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstFunctionParam_* param = AST_CAST(AstFunctionParam_, node);
  Symbol_* sym = scope_find(node->scope, &param->name);
  return emit_decl_var(chunk, param->name, sym, NULL, node->line);
}

static Location_ function_body_code_gen(TacChunk_* chunk, AstNode_* node) {

}

static Location_ function_call_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstFunctionCall_* call = (AstFunctionCall_*)node;
  AstExpr_* prefix = call->prefix;
  FunctionType_* fn_type = type_as(FunctionType_, prefix->type);

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
    param_locs[i] = tac_alloc(chunk, arg->base.top_type);
    arg_size += param_locs[i].size;
    ++i;
  }

  i = 0;
  for (AstListNode_* n = args->args.head; n != NULL; n = n->next) {
    AstFunctionCallArg_* arg = AST_CAST(AstFunctionCallArg_, n->node);
    emit_set_param(chunk, &param_locs[i], &args_locs[i], arg->expr->type->size, node->line);
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
static Location_ class_find_member_as_ptr(TacChunk_* chunk, const Location_* cls_loc, const Type_* cls_ty, const Token_* id, int line) {
  size_t offset = 0;
  assertf(type_class_findmember(cls_ty, id, &offset), "Could not find class member %.*s", id->length, id->start);
  Location_ dst = tac_alloc_ptr(chunk);

  switch (cls_loc->type) {
    case LOCATION_TYPE_VAL:
      emit_tac(chunk, OP_LOADA, dst, OP_LOC(*cls_loc), OP_OFFSET((int)offset), line);
      break;

    case LOCATION_TYPE_VAR:
      emit_tac(chunk, OP_RLOADA, dst, OP_LOC(*cls_loc), OP_OFFSET((int)offset), line);
      break;

    case LOCATION_TYPE_PTR:
      emit_tac(chunk, OP_MOVE, dst, OP_LOC(*cls_loc), EMPTY_OPERAND, line);
      emit_tac(chunk, OP_ADDIMM, dst, OP_LOC(dst), OP_OFFSET((int)offset * sizeof(Value_)), line);
      break;
  }

  return dst;
}

static Location_ class_find_member_as_ref(TacChunk_* chunk, const Location_* cls_loc, const Type_* cls_ty, const Token_* id, int line) {
  Type_* field_ty = type_class_findmember(cls_ty, id, NULL);
  Location_ ptr = class_find_member_as_ptr(chunk, cls_loc, cls_ty, id, line);

  if (type_is(field_ty, VarType_)) {
    Location_ dst = tac_alloc_ptr(chunk);
    Location_ tmp = tac_alloc_ptr(chunk);
    emit_tac(chunk, OP_LOAD, tmp, OP_LOC(ptr), OP_OFFSET(0), line);
    emit_tac(chunk, OP_RLOADA, dst, OP_LOC(tmp), OP_OFFSET(0), line);
    return dst;
  } else {
    return ptr;
  }
}

static Location_ class_find_member_as_val(TacChunk_* chunk, const Location_* cls_loc, const Type_* cls_ty, const Token_* id, int line) {
  Type_* field_ty = type_class_findmember(cls_ty, id, NULL);
  Location_ tmp = class_find_member_as_ptr(chunk, cls_loc, cls_ty, id, line);
  Location_ dst = tac_alloc(chunk, field_ty);
  if (dst.size == 1) {
    emit_tac(chunk, OP_LOAD, dst, OP_LOC(tmp), OP_OFFSET(0), line);
    return dst;
  }

  Location_ dst_ptr = tac_alloc_ptr(chunk);
  emit_tac(chunk, OP_LOADA, dst_ptr, OP_LOC(dst), EMPTY_OPERAND, line);
  emit_tac(chunk, OP_MEMCPY, dst_ptr, OP_LOC(tmp), OP_SIZE(dst.size), line);

  return dst;
}

static Location_ class_find_member_as_var(TacChunk_* chunk, const Location_* cls_loc, const Type_* cls_ty, const Token_* id, int line) {
  Location_ tmp = class_find_member_as_ptr(chunk, cls_loc, cls_ty, id, line);
  Location_ dst = tac_alloc_var(chunk);
  emit_tac(chunk, OP_LOAD, dst, OP_LOC(tmp), EMPTY_OPERAND, line);
  return dst;
}

static Location_ class_set_member(TacChunk_* chunk, const Location_* val, const Location_* cls_loc, const Type_* cls_ty, const Token_* id, int line) {
  Type_* field_ty = type_class_findmember(cls_ty, id, NULL);
  Location_ dst = class_find_member_as_ptr(chunk, cls_loc, cls_ty, id, line);

  if (val->size == 1) {
    emit_tac(chunk, OP_STORE, dst, OP_LOC(*val), OP_OFFSET(0), line);
    return dst;
  }

  Location_ tmp = tac_alloc_ptr(chunk);
  emit_tac(chunk, OP_LOADA, tmp, OP_LOC(*val), EMPTY_OPERAND, line);
  emit_tac(chunk, OP_MEMCPY, dst, OP_LOC(tmp), OP_SIZE(field_ty->size), line);

  return dst;
}

static Location_ dot_expr_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstDotExpr_* expr = AST_CAST(AstDotExpr_, node);
  Location_ cls_loc = code_gen(chunk, (AstNode_*)expr->prefix);

  if (type_isaref(expr->base.top_type)) {
    return class_find_member_as_ref(chunk, &cls_loc, expr->cls_ty, &expr->id, node->line);
  } else if (type_is(expr->base.top_type, VarType_)) {
    return class_find_member_as_var(chunk, &cls_loc, expr->cls_ty, &expr->id, node->line);
  } else {
    return class_find_member_as_val(chunk, &cls_loc, expr->cls_ty, &expr->id, node->line);
  }
}

static void emit_construct_default_class(TacChunk_* chunk, Type_* cls_ty, const Location_* dst, int line);

static void class_init_member_to(TacChunk_* chunk, const Location_* cls_loc, const Location_* val, const Type_* cls_ty, const Token_* field_name, int line) {
  Type_* field_ty = type_class_findmember(cls_ty, field_name, NULL);
  if (type_is(field_ty, VarType_)) {
    Location_ tmp = tac_alloc_var(chunk);
    emit_make_ref(chunk, &tmp, field_ty, line);
    emit_set_variable(chunk, &tmp, val, field_ty, line);
    class_set_member(chunk, &tmp, cls_loc, cls_ty, field_name, line);
  } else {
    Location_ field_ptr = class_find_member_as_ptr(chunk, cls_loc, cls_ty, field_name, line);
    emit_set_variable(chunk, &field_ptr, val, field_ty, line);
  }
}

static void class_init_member(TacChunk_* chunk, const Location_* cls_loc, Type_* cls_ty, Type_* field_ty, const Token_* field_name, int line) {
  Type_* value_ty = type_valtype(field_ty);
  Location_ val = EMPTY_LOC;
  if (type_is(value_ty, ClassType_)) {
    val = tac_alloc(chunk, value_ty);
    emit_construct_default_class(chunk, value_ty, &val, line);
  } else {
    assertf(field_ty->size == 1, "");
    val = emit_nil(chunk, line);
  }

  class_init_member_to(chunk, cls_loc, &val, cls_ty, field_name, line);
}

static void emit_construct_default_class(TacChunk_* chunk, Type_* cls_ty, const Location_* dst, int line) {
  ClassType_* cls_type = type_as(ClassType_, cls_ty);

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
    ClassTypeField_* field = list_ptr(n, ClassTypeField_);
    class_init_member(chunk, dst, cls_ty, field->type, &field->name, line);
  }
}

static Location_ class_constructor_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstClassConstructor_* constructor = (AstClassConstructor_*)node;
  Type_* type = type_valtype(constructor->base.type);
  ClassType_* cls_type = type_as(ClassType_, type);

  Location_ dst = tac_alloc_val(chunk, type->size);
  if (type->opt_name.start) {

    AstListNode_* current_param_node;
    ListNode_* current_member_node = cls_type->members.head;

    // The syntax is that all named constructor paramters must be at the end.
    // So first, generate code for all the unnamed parameters.
    for (current_param_node = constructor->params.head; current_param_node != NULL; current_param_node = current_param_node->next) {
      AstClassConstructorParam_* constructor_field = AST_CAST(AstClassConstructorParam_, current_param_node->node);
      if (constructor_field->name.start) {
        break;
      }

      ClassTypeField_* member = list_ptr(current_member_node, ClassTypeField_);
      Location_ val = code_gen(chunk, current_param_node->node);
      class_init_member_to(chunk, &dst, &val, (Type_*)cls_type, &member->name, node->line);

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
      ClassTypeField_* cur_member = list_ptr(current_member_node, ClassTypeField_);

      bool found = false;
      for (AstListNode_* param_node = current_param_node; param_node != NULL; param_node = param_node->next) {
        AstClassConstructorParam_* constructor_param = AST_CAST(AstClassConstructorParam_, param_node->node);
        if (token_eq(constructor_param->name, cur_member->name)) {
          Location_ val = code_gen(chunk, param_node->node);
          class_init_member_to(chunk, &dst, &val, (Type_*)cls_type, &cur_member->name, node->line);
          found = true;
          break;
        }
      }

      // It could be that the user did not write an initializer for the
      // parameter. In this case, create the default value.
      if (!found) {
        if (cur_member->opt_expr) {
          Location_ val = code_gen(chunk, (AstNode_*)cur_member->opt_expr);
          class_init_member_to(chunk, &dst, &val, (Type_*)cls_type, &cur_member->name, node->line);
        } else {
          class_init_member(chunk, &dst, (Type_*)cls_type, cur_member->type, &cur_member->name, node->line);
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
  AstArrayValueExpr_* array_expr = (AstArrayValueExpr_*)node;
  ArrayType_* array_type = type_as(ArrayType_, array_expr->base.type);
  Location_ ret = tac_current(chunk);
  Location_ cur = ret;
  if (type_is(array_type->el_type, VarType_)) {
    cur.type = LOCATION_TYPE_VAR;
  } else {
    cur.type = LOCATION_TYPE_VAL;
  }
  tac_alloc_val(chunk, array_type->self.size);

  for (AstListNode_* n = array_expr->values.head; n != NULL; n = n->next) {
    emit_init_var(chunk, &cur, array_type->el_type, AS_EXPR(n->node), node->line);
    cur.frame_offset += (int32_t)array_type->el_type->size;
  }

  return ret;
}

static Location_ in_place_binary_stmt_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstInPlaceBinaryStmt_* expr = (AstInPlaceBinaryStmt_*)node;
  Location_ dst = code_gen(chunk, (AstNode_*)expr->left);
  Location_ tmp;
    if (dst.type == LOCATION_TYPE_VAL) {
    tmp = dst;
  } else {
    tmp = tac_alloc_val(chunk, Int_Ty->size);
    emit_set_variable(chunk, &tmp, &dst, expr->left->type, node->line);
  }
  Location_ src = code_gen(chunk, (AstNode_*)expr->right);
  Location_ res = emit_binary_op(
    chunk, expr->bin_op, tmp, src,
    type_valtype(expr->left->type), type_valtype(expr->right->type),
    expr->left->type, node->line);

  emit_set_variable(chunk, &dst, &res, expr->left->type, node->line);

  return dst;
}

static Location_ range_expr_code_gen(TacChunk_* chunk, AstNode_* node) {
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
  [AST_CLS(AstIndexExpr_)]             = {index_expr_code_gen},
  [AST_CLS(AstAssignmentExpr_)]        = {assignment_expr_code_gen},
  [AST_CLS(AstInPlaceBinaryStmt_)]     = {in_place_binary_stmt_code_gen},
  [AST_CLS(AstWhileStmt_)]             = {while_stmt_code_gen},
  [AST_CLS(AstForStmt_)]               = {for_stmt_code_gen},
  [AST_CLS(AstFunctionDef_)]           = {function_def_code_gen},
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
  [AST_CLS(AstRangeExpr_)]             = {range_expr_code_gen},
  [AST_CLS(AstTypeDef_)]               = {noop_code_gen},
  [AST_CLS(TypeMemberDecl_)]           = {noop_code_gen},
  [AST_CLS(AstGenericParam_)]          = {noop_code_gen},
  [AST_CLS(AstGenericParams_)]         = {noop_code_gen},
  [AST_CLS(AstIndexOrTypeExpr_)]       = {noop_code_gen},
  [AST_CLS(AstIndexOrGenericArgs_)]    = {noop_code_gen},
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