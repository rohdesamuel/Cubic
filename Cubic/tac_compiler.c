#include "tac_compiler.h"
#include "common.h"
#include "ast.h"
#include "symbol_table.h"

typedef Location_(*CodeGenFn)(TacChunk_*, AstNode_*);

typedef struct CodeGenRule_ {
  CodeGenFn code_gen;
} CodeGenRule_;

static void emit_return(TacChunk_* chunk, int line);

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

static CodeGenRule_* get_rule(int info);
static void tac_chunk_free(TacChunk_* chunk);

static Location_ code_gen(TacChunk_* chunk, AstNode_* node) {
  return get_rule(node->cls)->code_gen(chunk, node);
}

void tac_compiler_init(TacCompiler_* compiler, MemoryAllocator_* allocator) {
  *compiler = (TacCompiler_){ 0 };
  compiler->allocator = allocator;
  compiler->chunk.allocator = allocator;
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

void tac_compiler_print(TacCompiler_* compiler) {
#define PRINT_BINARY_OP(LOC_L, LOC_R, OP_CHAR) \
    location_print(LOC_L); \
    printf(" " ## OP_CHAR ## " "); \
    location_print(LOC_R);

  TacChunk_* chunk = &compiler->chunk;
  int level = 0;
  for (int i = 0; i < chunk->count; ++i) {
    Tac_* tac = chunk->code + i;
    Location_* dst = &tac->dst;
    Operand_* arg_l = &tac->arg_l;
    Operand_* arg_r = &tac->arg_r;
    Location_* loc_l = &arg_l->loc;
    Location_* loc_r = &arg_r->loc;

    if (tac->label) {
      printf("%s:\n", tac->label);
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

      case OP_PRINT:
        opcode_print(tac->op);
        location_print(loc_l);
        break;

      case OP_RETURN:
        opcode_print(tac->op);
        break;

      case OP_ASSERT:
        printf("ASSERT ");
        location_print(loc_l);
        break;

      case OP_MAKE_REF:
        printf("new ref[%lld]", tac->arg_l.size);
        break;

      case OP_INC_REF:
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

#undef PRINT_BINARY_OP
}

void tac_compiler_compile(TacCompiler_* compiler, struct AstNode_* root) {
  code_gen(&compiler->chunk, root);
  emit_return(&compiler->chunk, 0);

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

static void tac_chunk_init(TacChunk_* chunk) {
  *chunk = (TacChunk_){ 0 };
}

static void tac_chunk_free(TacChunk_* chunk) {
  FREE_ARRAY(int, chunk->lines, chunk->capacity);
  FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
  tac_chunk_init(chunk);
}

static Location_ tac_alloc(TacChunk_* chunk, size_t size) {
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
  return ret;
}

static Tac_* tac_chunk_write(TacChunk_* chunk, Tac_* code, int line) {
  tac_chunk_reserve(chunk, 1);

  chunk->code[chunk->count] = *code;
  chunk->lines[chunk->count] = line;
  chunk->count++;

  return chunk->code + chunk->count - 1;
}

static Location_ tac_chunk_writeconstant(TacChunk_* chunk, TypedValue_ value, size_t size, int line) {  
  Tac_ tac = {
    .op = OP_CONSTANT,
    .dst = tac_alloc(chunk, size),
    .arg_l.opt_type = value.ty,
    .arg_l.val = value.val,
  };
  tac_chunk_write(chunk, &tac, line);
  return tac.dst;
}

static Tac_* emit_tac(TacChunk_* chunk, OpCode op, Location_ dst, Operand_ arg_l, Operand_ arg_r, int line) {
  Tac_ tac = { 0 };
  tac.op = op;
  tac.dst = dst;
  tac.arg_l = arg_l;
  tac.arg_r = arg_r;
  tac.line = line;

  return tac_chunk_write(chunk, &tac, line);
}

static Location_ emit_constant(TacChunk_* chunk, TypedValue_ value, size_t size, int line) {
  return tac_chunk_writeconstant(chunk, value, size, line);
}

static Location_ unary_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstUnaryExp_* exp = (AstUnaryExp_*)node;
  code_gen(chunk, (AstNode_*)exp->expr);
  return EMPTY_LOC;
}

static Location_ emit_cast(TacChunk_* chunk, Location_ val, RuntimeType_ from, RuntimeType_ to, size_t size, int line) {
  Location_ dst = tac_alloc(chunk, size);
  OpCode cast_op = OP_NOP;
  switch (from.ty) {
    case VAL_DOUBLE:
      switch (to.ty) {
        case VAL_FLOAT:
          cast_op = OP_CAST_d2f;
          break;
        default:
          cast_op = OP_CAST_d2i;
          break;
      }
      break;
    case VAL_FLOAT:
      switch (to.ty) {
        case VAL_DOUBLE:
          cast_op = OP_CAST_f2d;
          break;
        default:
          cast_op = OP_CAST_f2i;
          break;
      }
      break;
    default:
      switch (to.ty) {
        case VAL_FLOAT:
          cast_op = OP_CAST_i2f;
          break;
        case VAL_DOUBLE:
          cast_op = OP_CAST_i2d;
          break;
      }
      break;
  }
  assertf(cast_op != OP_NOP, "Trying an unknown cast. From: %d. To: %d", from.ty, to.ty);

  emit_tac(chunk, cast_op, dst, OP_LOC(val), EMPTY_OPERAND, line);
  return dst;
}

static void emit_op(TacChunk_* chunk, OpCode op, int line) {
  emit_tac(chunk, op, EMPTY_LOC, EMPTY_OPERAND, EMPTY_OPERAND, line);
}

static void emit_return(TacChunk_* chunk, int line) {
  emit_tac(chunk, OP_RETURN, EMPTY_LOC, EMPTY_OPERAND, EMPTY_OPERAND, line);
}

static Tac_* emit_label(TacChunk_* chunk, const char* label, int line) {
  Tac_ tac = { 0 };
  tac.label = label;
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

  if (val_size == 1) {
    Location_ tmp = src->type == LOCATION_TYPE_VAL ? *src : tac_alloc(chunk, val_size);

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

  if (val_size == 1) {
    Location_ tmp = src->type == LOCATION_TYPE_VAL ? *src : tac_alloc(chunk, val_size);
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

static Location_ emit_get_var(TacChunk_* chunk, const Location_* src, size_t size, int line) {
  assertf(src->type == LOCATION_TYPE_VAR, "");
  Location_ dst = tac_alloc(chunk, size);
  emit_set_variable(chunk, &dst, src, size, line);
  return dst;
}

static Location_ emit_get_ptr(TacChunk_* chunk, const Location_* src, size_t size, int line) {
  assertf(src->type == LOCATION_TYPE_PTR, "");
  Location_ dst = tac_alloc(chunk, size);
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

static Tac_* emit_jmp(TacChunk_* chunk, const char* label, int line) {
  Tac_ tac = { 0 };
  tac.op = OP_JMP;
  tac.label = label;

  return tac_chunk_write(chunk, &tac, line);
}

static Location_ emit_add(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
  if (ltype.ty != rtype.ty) {
    r = emit_cast(chunk, r, rtype, ltype, type_size, line);
  }

  OpCode op = OP_NIL;
  if (ISA_TY_UINT(ltype) || ISA_TY_INT(ltype)) {
    op = OP_ADD;
  } else if (ltype.ty == VAL_FLOAT) {
    op = OP_FADD;
  } else if (ltype.ty == VAL_DOUBLE) {
    op = OP_DADD;
  } else if (IS_TY_OBJ(ltype, OBJ_TYPE_STRING)) {
    op = OP_CONCAT;
  }
  emit_tac(chunk, op, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_sub(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
  if (ltype.ty != rtype.ty) {
    r = emit_cast(chunk, r, rtype, ltype, type_size, line);
  }

  OpCode op = OP_NIL;
  if (ISA_TY_UINT(ltype) || ISA_TY_INT(ltype)) {
    op = OP_SUB;
  } else if (ltype.ty == VAL_FLOAT) {
    op = OP_FSUB;
  } else if (ltype.ty == VAL_DOUBLE) {
    op = OP_DSUB;
  }
  emit_tac(chunk, op, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_mul(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
  if (ltype.ty != rtype.ty) {
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

static Location_ emit_div(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
  if (ltype.ty != rtype.ty) {
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

static Location_ emit_idiv(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
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

static Location_ emit_mod(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
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

static Location_ emit_neg(TacChunk_* chunk, Location_ val, RuntimeType_ t, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
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
  Location_ dest = tac_alloc(chunk, type_size);
  emit_tac(chunk, OP_LSHIFT, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_rshift(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
  emit_tac(chunk, OP_RSHIFT, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_bit_and(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
  emit_tac(chunk, OP_BITWISE_AND, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_bit_or(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
  emit_tac(chunk, OP_BITWISE_OR, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_bit_xor(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
  emit_tac(chunk, OP_BITWISE_XOR, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_and(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
  emit_tac(chunk, OP_AND, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_or(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
  emit_tac(chunk, OP_OR, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_xor(TacChunk_* chunk, Location_ l, Location_ r, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
  emit_tac(chunk, OP_XOR, dest, OP_LOC(l), OP_LOC(r), line);
  return dest;
}

static Location_ emit_not(TacChunk_* chunk, Location_ val, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
  emit_tac(chunk, OP_NOT, dest, OP_LOC(val), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_eq(TacChunk_* chunk, Location_ l, Location_ r, Location_ l_val, Location_ r_val, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);

  if (l.type == r.type) {
    emit_tac(chunk, OP_EQ, dest, OP_LOC(l), OP_LOC(r), line);
  } else {
    emit_tac(chunk, OP_EQ, dest, OP_LOC(l_val), OP_LOC(r_val), line);
  }

  return dest;
}

static Location_ emit_neq(TacChunk_* chunk, Location_ l, Location_ r, Location_ l_val, Location_ r_val, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);

  if (l.type == r.type) {
    emit_tac(chunk, OP_EQ, dest, OP_LOC(l), OP_LOC(r), line);
  } else {
    emit_tac(chunk, OP_EQ, dest, OP_LOC(l_val), OP_LOC(r_val), line);
  }

  return emit_not(chunk, dest, type_size, line);
}

static Location_ emit_cmp(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ dest = tac_alloc(chunk, type_size);
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
  Location_ dest = tac_alloc(chunk, type_size);
  emit_tac(chunk, OP_GT, dest, OP_LOC(tmp), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_gte(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ tmp = emit_cmp(chunk, l, r, ltype, rtype, type_size, line);
  Location_ dest = tac_alloc(chunk, type_size);
  emit_tac(chunk, OP_GTE, dest, OP_LOC(tmp), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_lt(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ tmp = emit_cmp(chunk, l, r, ltype, rtype, type_size, line);
  Location_ dest = tac_alloc(chunk, type_size);
  emit_tac(chunk, OP_LT, dest, OP_LOC(tmp), EMPTY_OPERAND, line);
  return dest;
}

static Location_ emit_lte(TacChunk_* chunk, Location_ l, Location_ r, RuntimeType_ ltype, RuntimeType_ rtype, size_t type_size, int line) {
  Location_ tmp = emit_cmp(chunk, l, r, ltype, rtype, type_size, line);
  Location_ dest = tac_alloc(chunk, type_size);
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

static void emit_make_ref(TacChunk_* chunk, const Location_* dst, size_t size, int line) {
  assertf(dst->type == LOCATION_TYPE_VAR, "");
  emit_tac(chunk, OP_MAKE_REF, *dst, OP_SIZE(size), EMPTY_OPERAND, line);
}

static void emit_make_ref_from(TacChunk_* chunk, const Location_* dst, const Location_* src, size_t size, int line) {
  assertf(dst->type == LOCATION_TYPE_VAR, "");
  assertf(src->type == LOCATION_TYPE_VAR, "");
  emit_tac(chunk, OP_INC_REF, EMPTY_LOC, OP_LOC(*src), EMPTY_OPERAND, line);
  emit_tac(chunk, OP_MOVE, *dst, OP_LOC(*src), EMPTY_OPERAND, line);  
}

#if 0

static void emit_sub(TacChunk_* chunk, RuntimeType_ l, RuntimeType_ r, int line) {
  if (l.ty != r.ty) {
    emit_cast(chunk, r, l, line);
  }

  if (ISA_TY_UINT(l) || ISA_TY_INT(l)) {
    emit_byte(chunk, OP_SUB, line);
  } else if (ISA_TY_REAL(l)) {
    emit_byte(chunk, OP_FSUB, line);
  }
}

static void emit_mul(TacChunk_* chunk, RuntimeType_ l, RuntimeType_ r, int line) {
  if (l.ty != r.ty) {
    emit_cast(chunk, r, l, line);
  }

  if (ISA_TY_UINT(l)) {
    emit_byte(chunk, OP_MUL, line);
  } else if (ISA_TY_INT(l)) {
    emit_byte(chunk, OP_IMUL, line);
  } else if (ISA_TY_REAL(l)) {
    emit_byte(chunk, OP_FMUL, line);
  }
}

static void emit_div(TacChunk_* chunk, RuntimeType_ l, RuntimeType_ r, int line) {
  if (l.ty != r.ty) {
    emit_cast(chunk, r, l, line);
  }

  if (ISA_TY_UINT(l)) {
    emit_byte(chunk, OP_DIV, line);
  } else if (ISA_TY_INT(l)) {
    emit_byte(chunk, OP_IDIV, line);
  } else if (ISA_TY_REAL(l)) {
    emit_byte(chunk, OP_FDIV, line);
  }
}

static void emit_mod(TacChunk_* chunk, RuntimeType_ l, RuntimeType_ r, int line) {
  if (l.ty != r.ty) {
    emit_cast(chunk, r, l, line);
  }

  if (ISA_TY_UINT(l)) {
    emit_byte(chunk, OP_MOD, line);
  } else if (ISA_TY_INT(l)) {
    emit_byte(chunk, OP_IMOD, line);
  }
}

static void emit_neg(Chunk_* chunk, RuntimeType_ t, int line) {
  if (ISA_TY_REAL(t)) {
    emit_byte(chunk, OP_FNEG, line);
  } else {
    emit_byte(chunk, OP_NEG, line);
  }
}



static Location_ binary_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstBinaryExp_* exp = (AstBinaryExp_*)node;
  AstExpr_* l = AS_EXPR(exp->left);
  AstExpr_* r = AS_EXPR(exp->right);

  RuntimeType_ ltype = semantictype_toruntime(l->sem_type);
  RuntimeType_ rtype = semantictype_toruntime(r->sem_type);

  Location_ l_loc = code_gen(chunk, (AstNode_*)l);
  Location_ r_loc = code_gen(chunk, (AstNode_*)r);

  if (l_loc.type != LOCATION_TYPE_VAL) {
    l_loc = emit_get_variable(chunk, &l_loc, l->sem_type.size, node->line);
  }

  if (r_loc.type != LOCATION_TYPE_VAL) {
    r_loc = emit_get_variable(chunk, &r_loc, r->sem_type.size, node->line);
  }

  switch (exp->op) {
    case TK_PLUS:          return emit_add(chunk, l_loc, r_loc, ltype, rtype, exp->base.sem_type.size, node->line); break;
  }

  return EMPTY_LOC;
}
#endif

static Location_ primary_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstPrimaryExp_* exp = (AstPrimaryExp_*)node;
  RuntimeType_ info = semantictype_toruntime(exp->base.sem_type);
  
  TypedValue_ v = { 0 };

  if (IS_TY_NIL(info)) {
    v = (TypedValue_){ NIL_VAL, NIL_TY };
  } else if (IS_TY_BOOL(info)) {
    v = (TypedValue_){ exp->value.as.b ? TRUE_VAL : FALSE_VAL, BOOL_TY };
  } else {
    v = (TypedValue_){ exp->value, semantictype_toruntime(exp->base.sem_type)};
  }
  return emit_constant(chunk, v, exp->base.sem_type.size, node->line);
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
  code_gen(chunk, (AstNode_*)program->block);

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
  if (stmt->expr) {
    code_gen(chunk, (AstNode_*)stmt->expr);
  }
  emit_return(chunk, node->line);
  return EMPTY_LOC;
}

static Location_ if_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstIfStmt_* stmt = (AstIfStmt_*)node;

  // if `condition_expr`
  code_gen(chunk, (AstNode_*)stmt->condition_expr);
  code_gen(chunk, stmt->if_stmt);

  // elif true then ...
  AstListNode_* elif_e = stmt->elif_exprs.head;
  AstListNode_* elif_s = stmt->elif_stmts.head;
  while (elif_e != NULL && elif_s != NULL) {
    AstNode_* elif_expr = elif_e->node;
    AstNode_* elif_stmt = elif_s->node;
        
    emit_label(chunk, "else", node->line);

    code_gen(chunk, elif_expr);
    code_gen(chunk, elif_stmt);
  }

  // else
  emit_label(chunk, "end_if", node->line);
  if (stmt->else_stmt) {
    code_gen(chunk, stmt->else_stmt);
  }
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

static Location_ var_decl_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)node;
  Symbol_* sym = scope_find(node->scope, &stmt->name);
  SemanticType_ var_type = sym->var.sem_type;
  
  Location_ dst = EMPTY_LOC;
  switch (var_type.kind) {
    case KIND_VAL: dst = tac_alloc(chunk, var_type.size); break;
    case KIND_VAR: dst = tac_alloc_var(chunk); break;
    case KIND_REF: dst = tac_alloc_ptr(chunk); break;
    case KIND_PTR: dst = tac_alloc_ptr(chunk); break;
  }  

  dst.token = stmt->name;
  sym->var.location = dst;
  
  // TODO: allow for multiple expressions.
  if (stmt->expr) {
    SemanticType_ rhs_type = stmt->expr->sem_type;
    if (var_type.kind == KIND_VAR) {
      if (rhs_type.kind == KIND_VAR) {
        Location_ tmp = code_gen(chunk, (AstNode_*)stmt->expr);
        emit_make_ref_from(chunk, &dst, &tmp, var_type.size, node->line);
        return dst;
      } else {
        emit_make_ref(chunk, &dst, var_type.size, node->line);
      }
    }

    Location_ tmp = code_gen(chunk, (AstNode_*)stmt->expr);
    emit_set_variable(chunk, &dst, &tmp, stmt->sem_type.size, node->line);

    return dst;
  } else {
    if (var_type.kind == KIND_VAR) {
      emit_make_ref(chunk, &dst, var_type.size, node->line);
    }
    return dst;
  }
}

static Location_ var_expr_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstVarExpr_* expr = (AstVarExpr_*)node;
  Location_ dst = code_gen(chunk, (AstNode_*)expr->expr);
  return dst;

  SemanticType_ top_type = expr->base.top_sem_type;
  SemanticType_ expr_type = expr->base.sem_type;

  if (top_type.kind == expr_type.kind) {
    return dst;
  }

  if (top_type.kind == KIND_VAL) {
    Location_ tmp = tac_alloc(chunk, dst.size);
    emit_tac(chunk, OP_MOVE, tmp, OP_LOC(dst), EMPTY_OPERAND, node->line);
    return tmp;
  }

  return dst;
}

// Get the specified variable.
static Location_ id_expr_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstIdExpr_* expr = (AstIdExpr_*)node;

  Symbol_* sym = scope_find(node->scope, &expr->name);

  switch (sym->type) {
    case SYMBOL_TYPE_VAR:
      return sym->var.location;
#if 0
    case SYMBOL_TYPE_FN:
      return sym->fn.obj_fn->constant_index;
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

  emit_set_variable(chunk, &dst, &src, expr->left->sem_type.size, node->line);
  return dst;
}

static Location_ while_stmt_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstWhileStmt_* stmt = (AstWhileStmt_*)node;

  // if `condition_expr`
  code_gen(chunk, (AstNode_*)stmt->condition_expr);

  // if true then ...
  code_gen(chunk, stmt->block_stmt);
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

  ObjFunction_* obj_fn = fn->obj_fn;

  //emit_label(chunk, "fn", node->line);
  //obj_fn->constant_index = make_constant(chunk, OBJ_VAL(obj_fn));

  TacChunk_* fn_chunk = malloc(sizeof(TacChunk_));
  //obj_fn->chunk = fn_chunk;

  tac_chunk_init(fn_chunk);

  code_gen(fn_chunk, (AstNode_*)def->body);
  
  return EMPTY_LOC;
}

static Location_ function_body_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstFunctionBody_* body = (AstFunctionBody_*)node;
  code_gen(chunk, body->stmt);

  for (AstListNode_* n = body->function_params.head; n != NULL; n = n->next) {
    code_gen(chunk, n->node);
  }
  return EMPTY_LOC;
}

static Location_ function_call_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstFunctionCall_* call = (AstFunctionCall_*)node;
  AstExpr_* prefix = call->prefix;
  Symbol_* sym = prefix->sem_type.sym;

  FunctionSymbol_* fn_sym = symbol_ascallable(sym);
  assertf(fn_sym, "Unknown symbol type.");

  // Push function object to call.
  code_gen(chunk, (AstNode_*)prefix);

  // Push arguments.
  code_gen(chunk, (AstNode_*)call->args);
  return EMPTY_LOC;
}

static Location_ function_args_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstFunctionCallArgs_* args = (AstFunctionCallArgs_*)node;
  for (AstListNode_* n = args->args.head; n != NULL; n = n->next) {
    code_gen(chunk, n->node);
  }
  return EMPTY_LOC;
}

static Location_ function_arg_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstFunctionCallArg_* arg = (AstFunctionCallArg_*)node;
  code_gen(chunk, (AstNode_*)arg->expr);
  return EMPTY_LOC;
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

  Location_ dst = tac_alloc(chunk, sym->field.sem_type.size);
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
  emit_tac(chunk, OP_MEMCPY, dst, OP_LOC(tmp), OP_SIZE(sym->field.sem_type.size), line);

  return dst;
}

static Location_ dot_expr_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstDotExpr_* expr = AST_CAST(AstDotExpr_, node);
  Location_ cls_loc = code_gen(chunk, (AstNode_*)expr->prefix);

  if (expr->base.top_sem_type.kind == KIND_REF) {
    return class_find_member_as_ptr(chunk, &cls_loc, expr->cls_sym, &expr->id, node->line);
  } else if (expr->base.top_sem_type.kind == KIND_VAR) {
    return class_find_member_as_var(chunk, &cls_loc, expr->cls_sym, &expr->id, node->line);
  } else {
    return class_find_member_as_val(chunk, &cls_loc, expr->cls_sym, &expr->id, node->line);
  }
}

static void emit_construct_default_class(TacChunk_* chunk, const Symbol_* cls_sym, const Location_* dst, int line);

static void class_init_member(TacChunk_* chunk, const Location_* cls_loc, const Location_* field_ptr, const Symbol_* cls_sym, const Symbol_* field_sym, int line) {
  const FieldSymbol_* field = &field_sym->field;
  if (field->sem_type.kind == KIND_VAR) {
    Location_ tmp = tac_alloc_var(chunk);
    emit_make_ref(chunk, &tmp, field->sem_type.size, line);
    class_set_member(chunk, &tmp, cls_loc, cls_sym, &field->name, line);
  }

  if (field->sem_type.val == VAL_CLASS) {
    emit_construct_default_class(chunk, field->sem_type.sym, field_ptr, line);
  } else {
    assertf(field->sem_type.size == 1, "");
    Location_ tmp = tac_alloc(chunk, field->sem_type.size);
    emit_tac(chunk, OP_NIL, tmp, EMPTY_OPERAND, EMPTY_OPERAND, line);
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

static void emit_construct_default_class(TacChunk_* chunk, const Symbol_* cls_sym, const Location_* dst, int line) {
  const SemanticType_* type = &cls_sym->cls.self_type;
  ListNode_* current_member_node = cls_sym->cls.members.head;

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
  
  for (current_member_node = cls_sym->cls.members.head; current_member_node != NULL; current_member_node = current_member_node->next) {
    Symbol_* field = list_val(current_member_node, Symbol_*);
    FieldSymbol_* field_sym = &field->field;
    Location_ field_ptr = class_find_member_as_ptr(chunk, &dst_ptr, cls_sym, &field->name, line);
    class_init_member(chunk, dst, &field_ptr, cls_sym, field, line);
  }
}

static Location_ class_constructor_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstClassConstructor_* constructor = (AstClassConstructor_*)node;
  SemanticType_* type = &constructor->base.sem_type;

  Location_ dst = tac_alloc(chunk, type->size);
  if (type->name.start) {
    Symbol_* cls_sym = scope_find(node->scope, &type->name);

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

static CodeGenRule_ code_gen_rules[] = {
  [AST_CLS(AstProgram_)] = {program_code_gen},
  [AST_CLS(AstBlock_)] = {block_code_gen},
  [AST_CLS(AstStmt_)] = {stmt_code_gen},
  [AST_CLS(AstExpr_)] = {expr_code_gen},
  [AST_CLS(AstPrintStmt_)] = {print_code_gen},
  [AST_CLS(AstUnaryExp_)] = {unary_code_gen},
  [AST_CLS(AstBinaryExp_)] = {binary_code_gen},
  [AST_CLS(AstPrimaryExp_)] = {primary_code_gen},
  [AST_CLS(AstReturnStmt_)] = {return_code_gen},
  [AST_CLS(AstIfStmt_)] = {if_code_gen},
  [AST_CLS(AstAssertStmt_)] = {assert_code_gen},
  [AST_CLS(AstVarDeclStmt_)] = {var_decl_code_gen},
  [AST_CLS(AstVarExpr_)] = {var_expr_code_gen},
  [AST_CLS(AstIdExpr_)] = {id_expr_code_gen},
  [AST_CLS(AstAssignmentExpr_)] = {assignment_expr_code_gen},
  [AST_CLS(AstWhileStmt_)] = {while_stmt_code_gen},
  [AST_CLS(AstFunctionDef_)] = {function_def_code_gen},
  [AST_CLS(AstFunctionBody_)] = {function_body_code_gen},
  [AST_CLS(AstFunctionParam_)] = {noop_code_gen},
  [AST_CLS(AstFunctionCall_)] = {function_call_code_gen},
  [AST_CLS(AstFunctionCallArgs_)] = {function_args_code_gen},
  [AST_CLS(AstFunctionCallArg_)] = {function_arg_code_gen},
  [AST_CLS(AstExpressionStmt_)] = {expression_statement_code_gen},
  [AST_CLS(AstNoopExpr_)] = {noop_code_gen},
  [AST_CLS(AstNoopStmt_)] = {noop_code_gen},
  [AST_CLS(AstCleanUpTemps_)] = {clean_up_temps_code_gen},
  [AST_CLS(AstTmpDecl_)] = {tmp_decl_code_gen},
  [AST_CLS(AstClassDef_)] = {noop_code_gen},
  [AST_CLS(AstClassMemberDecl_)] = {noop_code_gen},
  [AST_CLS(AstClassConstructor_)] = {class_constructor_code_gen},
  [AST_CLS(AstClassConstructorParam_)] = {class_constructor_field_code_gen},
  [AST_CLS(AstDotExpr_)] = {dot_expr_code_gen},
  [AST_CLS(AstTypeExpr_)] = {noop_code_gen},
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

#if 0

static void unary_code_gen(TacChunk_* chunk, AstNode_* node) {
  AstUnaryExp_* exp = (AstUnaryExp_*)node;

  // If getting the address of a symbol...
  if (exp->op == TK_AMPERSAND) {
    int index = resolve_var(node->scope, (AstNode_*)exp->expr);

    if (exp->expr->sem_type.kind == KIND_VAL) {
      // This assumes the symbol lives on the stack or referencing a static, create a weak reference to it.
      emit_bytes(chunk, OP_ADDROF_VAR, (uint8_t)index, node->line);
      emit_byte(chunk, OP_MAKE_REF, node->line);
    } else if (exp->expr->sem_type.kind == KIND_VAR) {
      emit_bytes(chunk, OP_ADDROF_REF, (uint8_t)index, node->line);
    }
  } else if (exp->op == TK_NEW) {
    code_gen(chunk, (AstNode_*)exp->expr);
    emit_byte(chunk, OP_NEW_VAR, node->line);
    int64_t size = exp->expr->sem_type.size;
    size = size == 0 ? 1 : size;
    emit_long(chunk, (uint32_t)size, node->line);
    emit_byte(chunk, OP_MAKE_REF, node->line);
  } else {
    code_gen(chunk, (AstNode_*)exp->expr);
    // Emit the operator instruction.
    switch (exp->op) {
      case TK_MINUS: emit_byte(chunk, OP_NEG, node->line); break;
      case TK_NOT: emit_byte(chunk, OP_NOT, node->line); break;
      default: assertf(false, "UnaryOp '%d' unimplemented", exp->op);  return; // Unreachable.
    }
  }
}

static Tac_* emit_tac(TacChunk_* chunk, OpCode op, int dst, Operand_ src_a, Operand_ src_b, const Token_* token) {
  Tac_* tac = alloc_ty(chunk->allocator, Tac_);
  *tac = (Tac_){ 0 };
  tac->op = op;
  tac->dst = dst;
  tac->src_a = src_a;
  tac->src_b = src_b;
  tac->token = *token;

  return tac;
}

static Tac_* emit_label(TacChunk_* chunk, const char* label, const Token_* token) {
  Tac_* tac = alloc_ty(chunk->allocator, Tac_);
  *tac = (Tac_){ 0 };
  tac->label = label;
  tac->token = *token;

  return tac;
}

static Tac_* emit_jmp(TacChunk_* chunk, const char* label, const Token_* token) {
  Tac_* tac = alloc_ty(chunk->allocator, Tac_);
  *tac = (Tac_){ 0 };
  tac->op = OP_JMP;
  tac->label = label;
  tac->token = *token;

  return tac;
}

static void emit_add(TacChunk_* chunk, Operand_ l, Operand_ r, RuntimeType_ ltype, RuntimeType_ rtype, int line) {
  if (ltype.ty != rtype.ty) {
    emit_cast(chunk, rtype, ltype, line);
  }

  if (ISA_TY_UINT(ltype) || ISA_TY_INT(ltype)) {
    emit_tac(chunk, OP_ADD, line);
  } else if (ISA_TY_REAL(ltype)) {
    emit_byte(chunk, OP_FADD, line);
  } else if (IS_TY_OBJ(ltype, OBJ_TYPE_STRING)) {
    emit_byte(chunk, OP_CONCAT, line);
  }
}

static void emit_sub(Chunk_* chunk, RuntimeType_ l, RuntimeType_ r, int line) {
  if (l.ty != r.ty) {
    emit_cast(chunk, r, l, line);
  }

  if (ISA_TY_UINT(l) || ISA_TY_INT(l)) {
    emit_byte(chunk, OP_SUB, line);
  } else if (ISA_TY_REAL(l)) {
    emit_byte(chunk, OP_FSUB, line);
  }
}

static void emit_mul(Chunk_* chunk, RuntimeType_ l, RuntimeType_ r, int line) {
  if (l.ty != r.ty) {
    emit_cast(chunk, r, l, line);
  }

  if (ISA_TY_UINT(l)) {
    emit_byte(chunk, OP_MUL, line);
  } else if (ISA_TY_INT(l)) {
    emit_byte(chunk, OP_IMUL, line);
  } else if (ISA_TY_REAL(l)) {
    emit_byte(chunk, OP_FMUL, line);
  }
}

static void emit_div(Chunk_* chunk, RuntimeType_ l, RuntimeType_ r, int line) {
  if (l.ty != r.ty) {
    emit_cast(chunk, r, l, line);
  }

  if (ISA_TY_UINT(l)) {
    emit_byte(chunk, OP_DIV, line);
  } else if (ISA_TY_INT(l)) {
    emit_byte(chunk, OP_IDIV, line);
  } else if (ISA_TY_REAL(l)) {
    emit_byte(chunk, OP_FDIV, line);
  }
}

static void emit_mod(Chunk_* chunk, RuntimeType_ l, RuntimeType_ r, int line) {
  if (l.ty != r.ty) {
    emit_cast(chunk, r, l, line);
  }

  if (ISA_TY_UINT(l)) {
    emit_byte(chunk, OP_MOD, line);
  } else if (ISA_TY_INT(l)) {
    emit_byte(chunk, OP_IMOD, line);
  }
}

static void emit_neg(Chunk_* chunk, RuntimeType_ t, int line) {
  if (ISA_TY_REAL(t)) {
    emit_byte(chunk, OP_FNEG, line);
  } else {
    emit_byte(chunk, OP_NEG, line);
  }
}

static void binary_code_gen(Chunk_* chunk, AstNode_* node) {
  AstBinaryExp_* exp = (AstBinaryExp_*)node;
  AstExpr_* l = AS_EXPR(exp->left);
  AstExpr_* r = AS_EXPR(exp->right);

  RuntimeType_ ltype = semantictype_toruntime(l->sem_type);
  RuntimeType_ rtype = semantictype_toruntime(r->sem_type);

  code_gen(chunk, (AstNode_*)l);
  code_gen(chunk, (AstNode_*)r);

  switch (exp->op) {
    case TK_AND:           emit_byte(chunk, OP_AND, node->line); break;
    case TK_OR:            emit_byte(chunk, OP_OR, node->line); break;
    case TK_XOR:           emit_byte(chunk, OP_XOR, node->line); break;
    case TK_PLUS:          emit_add(chunk, ltype, rtype, node->line); break;
    case TK_MINUS:         emit_sub(chunk, ltype, rtype, node->line); break;
    case TK_STAR:          emit_mul(chunk, ltype, rtype, node->line); break;
    case TK_SLASH:         emit_div(chunk, ltype, rtype, node->line); break;
      // case TK_DOUBLE_SLASH:  emit_byte(chunk, OP_DIV, node->line); break; // TODO: convert integers to floats
    case TK_PERCENT:       emit_mod(chunk, ltype, rtype, node->line); break;

    case TK_EQUAL_EQUAL:
      if (ltype.ty == VAL_OBJ) {
        emit_byte(chunk, OP_OBJ_EQ, node->line);
      } else {
        emit_byte(chunk, OP_EQ, node->line);
      }
      break;

    case TK_BANG_EQUAL:
      if (ltype.ty == VAL_OBJ) {
        emit_byte(chunk, OP_OBJ_EQ, node->line);
      } else {
        emit_byte(chunk, OP_EQ, node->line);
      }
      emit_byte(chunk, OP_NOT, node->line);
      break;

    case TK_GT:
      if (ISA_TY_UINT(ltype)) {
        emit_byte(chunk, OP_CMP, node->line);
      } else if (ISA_TY_INT(ltype)) {
        emit_byte(chunk, OP_ICMP, node->line);
      } else if (ISA_TY_REAL(ltype)) {
        emit_byte(chunk, OP_FCMP, node->line);
      }
      emit_byte(chunk, OP_LTE, node->line);
      emit_byte(chunk, OP_NOT, node->line);
      break;

    case TK_GTE:
      if (ISA_TY_UINT(ltype)) {
        emit_byte(chunk, OP_CMP, node->line);
      } else if (ISA_TY_INT(ltype)) {
        emit_byte(chunk, OP_ICMP, node->line);
      } else if (ISA_TY_REAL(ltype)) {
        emit_byte(chunk, OP_FCMP, node->line);
      }
      emit_byte(chunk, OP_LT, node->line);
      emit_byte(chunk, OP_NOT, node->line);
      break;

    case TK_LT:
      if (ISA_TY_UINT(ltype)) {
        emit_byte(chunk, OP_CMP, node->line);
      } else if (ISA_TY_INT(ltype)) {
        emit_byte(chunk, OP_ICMP, node->line);
      } else if (ISA_TY_REAL(ltype)) {
        emit_byte(chunk, OP_FCMP, node->line);
      }
      emit_byte(chunk, OP_LT, node->line);
      break;

    case TK_LTE:
      if (ISA_TY_UINT(ltype)) {
        emit_byte(chunk, OP_CMP, node->line);
      } else if (ISA_TY_INT(ltype)) {
        emit_byte(chunk, OP_ICMP, node->line);
      } else if (ISA_TY_REAL(ltype)) {
        emit_byte(chunk, OP_FCMP, node->line);
      }
      emit_byte(chunk, OP_LTE, node->line);
      break;

    case TK_LSHIFT:        emit_byte(chunk, OP_LSHIFT, node->line); break;
    case TK_RSHIFT:        emit_byte(chunk, OP_RSHIFT, node->line); break;
    case TK_AMPERSAND:     emit_byte(chunk, OP_BITWISE_AND, node->line); break;
    case TK_PIPE:          emit_byte(chunk, OP_BITWISE_OR, node->line); break;
    case TK_HAT:           emit_byte(chunk, OP_BITWISE_XOR, node->line); break;
    default: printf("[Line %d] BinaryOp '%d' unimplemented\n", exp->base.base.line, exp->op);  break; // Unreachable.
  }
}

static void primary_code_gen(Chunk_* chunk, AstNode_* node) {
  AstPrimaryExp_* exp = (AstPrimaryExp_*)node;
  RuntimeType_ info = semantictype_toruntime(exp->base.sem_type);

  if (IS_TY_NIL(info)) {
    emit_byte(chunk, OP_NIL, node->line);
  } else if (IS_TY_BOOL(info)) {
    if (exp->value.as.b) {
      emit_byte(chunk, OP_TRUE, node->line);
    } else {
      emit_byte(chunk, OP_FALSE, node->line);
    }
  } else {
    emit_constant(chunk, exp->value, node->line);
  }
}

static void print_code_gen(Chunk_* chunk, AstNode_* node) {
  AstPrintStmt_* stmt = (AstPrintStmt_*)node;
  code_gen(chunk, (AstNode_*)stmt->expr);
  emit_byte(chunk, OP_PRINT, node->line);
  emit_long(chunk, type_toint(semantictype_toruntime(AS_EXPR(stmt->expr)->sem_type)), node->line);
}

static void program_code_gen(Chunk_* chunk, AstNode_* node) {
  Frame_* frame = node->scope->frame;

  AstProgram_* program = (AstProgram_*)node;
  emit_byte(chunk, OP_PROLOGUE, node->line);
  emit_short(chunk, (uint16_t)program->base.scope->frame->max_stack_size, node->line);
  code_gen(chunk, (AstNode_*)program->block);
  emit_byte(chunk, OP_EPILOGUE, node->line);
}

static void stmt_code_gen(Chunk_* chunk, AstNode_* node) {
  AstStmt_* stmt = (AstStmt_*)node;
  code_gen(chunk, stmt->stmt);
  code_gen(chunk, stmt->cleanup);
}

static void expr_code_gen(Chunk_* chunk, AstNode_* node) {
  AstExpr_* expr = (AstExpr_*)node;
  code_gen(chunk, (AstNode_*)expr->expr);
}

static void begin_scope(Chunk_* chunk, AstBlock_* block) {
#if 0
  List_* vars = &block->base.symbol_table->vars;

  emit_short((uint16_t)block->base.scope->frame->var_count, block->base.line);
  emit_byte(OP_BEGIN_SCOPE, block->base.line);
  for (ListNode_* n = vars->head; n != NULL; n = n->next) {
    Symbol_* s = list_val(n, Symbol_*);
    emit_bytes(OP_DECLARE_VAR, (uint8_t)s->type, block->base.line);
  }
#endif
}

static void end_scope(Chunk_* chunk, AstBlock_* block) {
  Scope_* scope = block->base.scope;
  Frame_* frame = scope->frame;
  List_* vars = &block->base.scope->table->vars;

  for (ListNode_* n = vars->head; n != NULL; n = n->next) {
    Symbol_* var = list_val(n, Symbol_*);
    switch (var->type) {
      case SYMBOL_TYPE_VAR:
        if (var->var.sem_type.val == VAL_OBJ && var->var.sem_type.kind == KIND_VAL) {
          emit_bytes(chunk, OP_DESTROY_VAR, var->var.frame_index, block->base.line);
        }
        break;
      case SYMBOL_TYPE_CLASS:
      case SYMBOL_TYPE_FN:  // TODO: how to handle this?
        break;
      default:
        assertf(false, "Unknown variable type to destroy %d", var->type);
        break;
    }
  }
}

static void block_code_gen(Chunk_* chunk, AstNode_* node) {
  AstBlock_* block = (AstBlock_*)node;
  begin_scope(chunk, block);
  for (AstListNode_* n = block->statements.head; n != NULL; n = n->next) {
    code_gen(chunk, n->node);
  }
  end_scope(chunk, block);
}

static void return_code_gen(Chunk_* chunk, AstNode_* node) {
  AstReturnStmt_* stmt = (AstReturnStmt_*)node;
  if (stmt->expr) {
    code_gen(chunk, (AstNode_*)stmt->expr);
  } else {
    emit_constant(chunk, NIL_VAL, node->line);
  }
  emit_return(chunk, node->line);
}

static void patch_jmp(Chunk_* chunk, int offset) {
  // -2 to adjust for the bytecode for the jump frame_offset itself.
  int jump = chunk->count - offset - 2;
  //assertf(jump > UINT16_MAX, "Too much code to jump over.");

  chunk->code[offset] = (jump >> 8) & 0xff;
  chunk->code[offset + 1] = jump & 0xff;
}

static void patch_jmplist(Chunk_* chunk, int start_offset, int num_jumps) {
  uint8_t* start = chunk->code;
  uint16_t count = chunk->count;

  uint16_t offset = start_offset;
  for (int i = 0; i < num_jumps; ++i) {
    uint8_t* jump = start + offset;
    int next_offset = offset + (uint16_t)((*jump << 8) | *(jump + 1));
    patch_jmp(chunk, offset);
    offset = next_offset;
  }
}

static void if_code_gen(Chunk_* chunk, AstNode_* node) {
  AstIfStmt_* stmt = (AstIfStmt_*)node;

  // if `condition_expr`
  code_gen(chunk, (AstNode_*)stmt->condition_expr);

  // if false then ...
  int then_jmp = emit_jmp(chunk, OP_JMP_IF_FALSE, node->line); // jmp :skip_then

  // if true then ...
  emit_byte(chunk, OP_POP, node->line);
  code_gen(chunk, stmt->if_stmt);
  int else_jmp = emit_jmp(chunk, OP_JMP, node->line); // jmp :end

  // :skip_then
  patch_jmp(chunk, then_jmp);
  emit_byte(chunk, OP_POP, node->line);

  // elif false then ...
  // The jmp_list variables is an optimization to not create a dynamic array
  // and patch the jumps to the end of the if block. All of the jumps are meant to
  // jump to the end if the if-case is true. All jumps point to the next jump
  // instruction. At the end, the code iterates through starting from the first
  // jump and patching to the end of the if block.
  int start_jmp_list = -1;
  int end_jmp_list = -1;
  int num_jumps = 0;
  int elif_jmp = -1;

  // elif true then ...
  AstListNode_* elif_e = stmt->elif_exprs.head;
  AstListNode_* elif_s = stmt->elif_stmts.head;
  while (elif_e != NULL && elif_s != NULL) {
    AstNode_* elif_expr = elif_e->node;
    AstNode_* elif_stmt = elif_s->node;

    code_gen(chunk, elif_expr);
    elif_jmp = emit_jmp(chunk, OP_JMP_IF_FALSE, node->line);

    emit_byte(chunk, OP_POP, elif_expr->line);
    code_gen(chunk, elif_stmt);
    int end_jmp = emit_jmp(chunk, OP_JMP, node->line);

    // Append the jump to the end of the list.
    if (start_jmp_list == -1) {
      start_jmp_list = end_jmp;
      end_jmp_list = end_jmp;
    } else {
      patch_jmp(chunk, end_jmp_list);
      end_jmp_list = end_jmp;
      num_jumps += 1;
    }

    patch_jmp(chunk, elif_jmp);
    emit_byte(chunk, OP_POP, elif_expr->line);

    elif_e = elif_e->next;
    elif_s = elif_s->next;

    assertf(elif_e == NULL && elif_s == NULL ||
      elif_e != NULL && elif_s != NULL,
      "Malformed list: every elif should have an expression and statement");
  }

  // :else
  if (stmt->else_stmt) {
    code_gen(chunk, stmt->else_stmt);
  }

  // :end
  // Patch all the jumps in the list to go here.
  if (start_jmp_list != -1) {
    patch_jmp(chunk, end_jmp_list);
    patch_jmplist(chunk, start_jmp_list, num_jumps);
  }
  patch_jmp(chunk, else_jmp);

  // end
}

static void assert_code_gen(Chunk_* chunk, AstNode_* node) {
  AstAssertStmt_* stmt = (AstAssertStmt_*)node;
  code_gen(chunk, (AstNode_*)stmt->expr);
  emit_byte(chunk, OP_ASSERT, node->line);
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

static void emit_set_var(Chunk_* chunk, int slot, SemanticType_* dest_sem_type, SemanticType_* source_sem_type, int line) {
  if (source_sem_type->val != dest_sem_type->val &&
    semantictype_iscoercible(*source_sem_type, *dest_sem_type)) {
    emit_cast(chunk, semantictype_toruntime(*source_sem_type), semantictype_toruntime(*dest_sem_type), line);
  }

  switch (source_sem_type->kind) {
    case KIND_VAL:
      emit_byte(chunk, OP_STACK_TOP, line);
      break;

    case KIND_VAR:
    case KIND_REF:
      emit_byte(chunk, OP_REF_PTR, line);
      break;
  }

  switch (dest_sem_type->kind) {
    case KIND_VAL:
      emit_bytes(chunk, OP_ADDROF_VAR, slot, line);
      break;

    case KIND_VAR:
    case KIND_REF:
      emit_bytes(chunk, OP_ADDROF_REF, slot, line);
      break;
  }

  OpCode set_op = dest_sem_type->kind == KIND_VAR ? OP_SET_REF : OP_SET_VAR;
  switch (dest_sem_type->kind) {
    case KIND_VAL:
      switch (source_sem_type->kind) {
        case KIND_VAL:
          for (int64_t i = source_sem_type->size - 1; i >= 0; --i) {
            emit_bytes(chunk, OP_SET_VAR, (uint8_t)slot + (uint8_t)i, line);
          }
          break;

        case KIND_VAR:
          for (int64_t i = source_sem_type->size - 1; i >= 0; --i) {
            emit_class_get_field(chunk, slot, (int)i, KIND_REF, KIND_VAL, line);
            emit_bytes(chunk, OP_SET_VAR, (uint8_t)slot + (uint8_t)i, line);
          }
          break;

        case KIND_REF:
          for (int64_t i = source_sem_type->size - 1; i >= 0; --i) {
            emit_class_get_field(chunk, slot, (int)i, KIND_REF, KIND_VAL, line);
            emit_bytes(chunk, OP_SET_VAR, (uint8_t)slot + (uint8_t)i, line);
          }
          break;
      } break;

    case KIND_VAR:
      break;

    case KIND_REF:
      break;
  }

  if (dest_sem_type->val == VAL_CLASS) {
    if (dest_sem_type->kind == KIND_VAL) {
      for (int64_t i = dest_sem_type->size - 1; i >= 0; --i) {
        emit_bytes(chunk, set_op, (uint8_t)slot + (uint8_t)i, line);
      }
    } else {
      emit_bytes(chunk, set_op, (uint8_t)slot, line);
    }
  } else if (dest_sem_type->kind == KIND_REF) {
    emit_bytes(chunk, OP_SET_REF, (uint8_t)slot, line);
  } else if (dest_sem_type->kind == KIND_VAR) {
    emit_bytes(chunk, OP_SET_REF, (uint8_t)slot, line);
  } else {
    emit_bytes(chunk, OP_SET_VAR, (uint8_t)slot, line);
  }
}

static void emit_init_var(Chunk_* chunk, int slot, SemanticType_* dest_sem_type, SemanticType_* source_sem_type, int line) {
  // References are initialized with pointers, simple set is ok.
  if (dest_sem_type->kind == KIND_REF) {
    emit_bytes(chunk, OP_SET_VAR, (uint8_t)slot, line);
  } else {
    emit_set_var(chunk, slot, dest_sem_type, source_sem_type, line);
  }
}

static void var_decl_code_gen(Chunk_* chunk, AstNode_* node) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)node;
  int slot = resolve_local(stmt->base.scope, &stmt->name);

  SemanticType_ sem_type = stmt->sem_type;

  if (sem_type.kind == KIND_VAR) {
    emit_byte(chunk, OP_ALLOC_PTR, node->line);
    int64_t size = sem_type.size;
    size = size == 0 ? 1 : size;
    emit_long(chunk, (uint32_t)size, node->line);
    emit_byte(chunk, OP_MAKE_REF, node->line);
    emit_bytes(chunk, OP_SET_VAR, (uint8_t)slot, node->line);
  }

  // TODO: allow for multiple expressions.
  if (stmt->expr) {
    code_gen(chunk, (AstNode_*)stmt->expr);
    emit_init_var(chunk, slot, &stmt->sem_type, &stmt->expr->sem_type, node->line);
  }
}

static void var_expr_code_gen(Chunk_* chunk, AstNode_* node) {
  AstVarExpr_* expr = (AstVarExpr_*)node;
  code_gen(chunk, (AstNode_*)expr->expr);
}

// Get the specified variable.
static void id_expr_code_gen(Chunk_* chunk, AstNode_* node) {
  AstIdExpr_* expr = (AstIdExpr_*)node;
  Symbol_* sym = scope_find(node->scope, &expr->name);

  switch (sym->type) {
    case SYMBOL_TYPE_VAR:
    {
      VarSymbol_* var = &sym->var;
      if (expr->base.top_sem_type.kind == KIND_VAL && (var->sem_type.kind == KIND_REF || var->sem_type.kind == KIND_VAR)) {
        emit_bytes(chunk, OP_GET_REF, (uint8_t)symbolvar_index(sym), node->line);
      } else if (expr->base.top_sem_type.kind == KIND_REF && var->sem_type.kind == KIND_VAL) {
        if (expr->base.sem_type.ref_kind == REF_KIND_WEAK) {
          emit_bytes(chunk, OP_ADDROF_VAR, (uint8_t)symbolvar_index(sym), node->line);
          emit_byte(chunk, OP_MAKE_REF, node->line);
        } else {
          assertf(false, "Unsupported reference operation");
        }
      } else {
        emit_bytes(chunk, OP_GET_VAR, (uint8_t)symbolvar_index(sym), node->line);
      }

      break;
    }
    case SYMBOL_TYPE_FN:
      emit_bytes(chunk, OP_CONSTANT, sym->fn.obj_fn->constant_index, node->line);
      break;
    case SYMBOL_TYPE_CLOSURE:
      emit_bytes(chunk, OP_GET_VAR, (uint8_t)(sym->closure.frame_index), node->line);
      break;
  }
}

// Set the specified variable.
static void assignment_expr_code_gen(Chunk_* chunk, AstNode_* node) {
  AstAssignmentExpr_* expr = (AstAssignmentExpr_*)node;

  // TODO: allow for assigning to more than simple variables
  // TODO: allow for assigning to multiple variables.
  AstVarExpr_* var = (AstVarExpr_*)expr->left;

  int slot = resolve_var(node->scope, (AstNode_*)var);
  SemanticType_ sem_type = var->base.sem_type;

  code_gen(chunk, (AstNode_*)expr->right);

  if (sem_type.val == VAL_OBJ && sem_type.kind == KIND_VAL) {
    emit_bytes(chunk, OP_DESTROY_VAR, (uint8_t)slot, node->line);
  }

  emit_set_var(chunk, slot, &sem_type, &expr->right->sem_type, node->line);
}

static void emit_loop(Chunk_* chunk, int loop_start, int line) {
  emit_byte(chunk, OP_LOOP, line);

  int offset = chunk->count - loop_start + 2;
  assertf(offset <= UINT16_MAX, "Loop body too large.");

  emit_byte(chunk, (offset >> 8) & 0xff, line);
  emit_byte(chunk, offset & 0xff, line);
}

static void while_stmt_code_gen(Chunk_* chunk, AstNode_* node) {
  AstWhileStmt_* stmt = (AstWhileStmt_*)node;

  // if `condition_expr`
  int loop_start = chunk->count;
  code_gen(chunk, (AstNode_*)stmt->condition_expr);

  // while false then break...
  int exit_jump = emit_jmp(chunk, OP_JMP_IF_FALSE, node->line); // jmp :exit_jmp

  // if true then ...
  emit_byte(chunk, OP_POP, node->line);
  code_gen(chunk, stmt->block_stmt);

  emit_loop(chunk, loop_start, node->line);

  // :exit_jmp
  patch_jmp(chunk, exit_jump);
  emit_byte(chunk, OP_POP, node->line);
}

static void expression_statement_code_gen(Chunk_* chunk, AstNode_* node) {
  AstExpressionStmt_* stmt = (AstExpressionStmt_*)node;
  code_gen(chunk, (AstNode_*)stmt->expr);

  for (int64_t i = 0; i < stmt->expr->sem_type.size; ++i) {
    emit_byte(chunk, OP_POP, node->line);
  }
}

static void function_def_code_gen(Chunk_* chunk, AstNode_* node) {
  AstFunctionDef_* def = (AstFunctionDef_*)node;
  FunctionSymbol_* fn = &def->fn_symbol->fn;
  ObjFunction_* obj_fn = fn->obj_fn;
  obj_fn->constant_index = make_constant(chunk, OBJ_VAL(obj_fn));

  Chunk_* fn_chunk = malloc(sizeof(Chunk_));
  obj_fn->chunk = fn_chunk;

  chunk_init(fn_chunk);

  code_gen(fn_chunk, (AstNode_*)def->body);

  emit_bytes(chunk, OP_CONSTANT, obj_fn->constant_index, node->line);
}

static void function_body_code_gen(Chunk_* chunk, AstNode_* node) {
  AstFunctionBody_* body = (AstFunctionBody_*)node;
  code_gen(chunk, body->stmt);

  for (AstListNode_* n = body->function_params.head; n != NULL; n = n->next) {
    code_gen(chunk, n->node);
  }

  if (semantictype_isnil(body->return_type)) {
    emit_constant(chunk, NIL_VAL, node->line);
    emit_byte(chunk, OP_RETURN, node->line);
  }
}

static void function_call_code_gen(Chunk_* chunk, AstNode_* node) {
  AstFunctionCall_* call = (AstFunctionCall_*)node;
  AstExpr_* prefix = call->prefix;
  Symbol_* sym = prefix->sem_type.sym;

  FunctionSymbol_* fn_sym = symbol_ascallable(sym);
  assertf(fn_sym, "Unknown symbol type.");

  // Push function object to call.
  code_gen(chunk, (AstNode_*)prefix);

  // Push arguments.
  code_gen(chunk, (AstNode_*)call->args);

  // Do the call.
  emit_bytes(chunk, OP_CALL, (uint8_t)fn_sym->params.count, node->line);
}

static void function_args_code_gen(Chunk_* chunk, AstNode_* node) {
  AstFunctionCallArgs_* args = (AstFunctionCallArgs_*)node;
  for (AstListNode_* n = args->args.head; n != NULL; n = n->next) {
    code_gen(chunk, n->node);
  }
}

static void function_arg_code_gen(Chunk_* chunk, AstNode_* node) {
  AstFunctionCallArg_* arg = (AstFunctionCallArg_*)node;
  code_gen(chunk, (AstNode_*)arg->expr);
}

static void noop_code_gen(Chunk_* chunk, AstNode_* node) {}

static void clean_up_temps_code_gen(Chunk_* chunk, AstNode_* node) {
  return;
  AstCleanUpTemps_* temps = (AstCleanUpTemps_*)node;

  for (ListNode_* n = temps->tmps.head; n != NULL; n = n->next) {
    Symbol_* tmp = list_val(n, Symbol_*);
    emit_bytes(chunk, OP_DESTROY_VAR, symboltmp_index(tmp), node->line);
  }
}

static void tmp_decl_code_gen(Chunk_* chunk, AstNode_* n) {
  AstTmpDecl_* decl = (AstTmpDecl_*)n;
  code_gen(chunk, (AstNode_*)decl->expr);
  if (semantictype_isaobj(decl->base.sem_type)) {
    // OP_SET_VAR doesn't pop the value from the stack, so there is no need to
    // have an OP_GET_VAR after.
    emit_bytes(chunk, OP_SET_VAR, symboltmp_index(decl->tmp), n->line);
  }
}

static void dot_expr_code_gen(Chunk_* chunk, AstNode_* node) {
  AstDotExpr_* expr = AST_CAST(AstDotExpr_, node);
  ClassSymbol_* cls_sym = NULL;
  const SemanticType_* type = &expr->prefix->sem_type;
  if (type->sym->type == SYMBOL_TYPE_VAR) {
    cls_sym = &type->sym->var.sem_type.sym->cls;
  } else if (type->sym->type == SYMBOL_TYPE_CLASS) {
    cls_sym = &type->sym->cls;
  } else if (type->sym->type == SYMBOL_TYPE_FIELD) {
    cls_sym = &type->sym->cls;
  } else {
    assertf(false, "Unexpected symbol type %d", type->sym->type);
  }

  int index = resolve_var(node->scope, (AstNode_*)expr->prefix);
  int field_index = symbol_findmember_index(cls_sym->self_type.sym, expr->id);

  emit_class_get_field(chunk, index, field_index, expr->prefix->sem_type.kind, expr->base.top_sem_type.kind, node->line);
}

static void class_default_constructor_code_gen(Chunk_* chunk, Symbol_* cls_sym, int line) {
  for (ListNode_* member_node = cls_sym->cls.members.head; member_node != NULL; member_node = member_node->next) {
    Symbol_* member = list_val(member_node, Symbol_*);
    FieldSymbol_* field = &member->field;

    // TODO: eventually remove this if-statement. This should be better automated. 
    if (field->sem_type.val == VAL_CLASS) {
      class_default_constructor_code_gen(chunk, field->sem_type.sym, line);
    } else {
      emit_constant(chunk, field->val, line);
    }
  }
}

static void class_constructor_code_gen(Chunk_* chunk, AstNode_* node) {
  AstClassConstructor_* constructor = (AstClassConstructor_*)node;

  SemanticType_* type = &constructor->base.sem_type;
  if (type->name.start) {
    Symbol_* cls_sym = scope_find(node->scope, &type->name);
    AstListNode_* current_field_node = constructor->params.head;
    for (ListNode_* member_node = cls_sym->cls.members.head; member_node != NULL; member_node = member_node->next) {
      Symbol_* member = list_val(member_node, Symbol_*);
      FieldSymbol_* field = &member->field;
      bool found_value = false;

      for (AstListNode_* field_node = current_field_node; field_node != NULL; field_node = field_node->next) {
        AstClassConstructorParam_* constructor_field = AST_CAST(AstClassConstructorParam_, field_node->node);
        if (!constructor_field->name.start || token_eq(constructor_field->name, member->name)) {
          code_gen(chunk, field_node->node);
          found_value = true;
          current_field_node = field_node->next;
          break;
        }
      }

      if (!found_value) {

        // TODO: eventually remove this if-statement. This should be better automated. 
        if (field->sem_type.val == VAL_CLASS) {
          class_default_constructor_code_gen(chunk, field->sem_type.sym, node->line);
        } else {
          emit_constant(chunk, field->val, node->line);
        }
      }
    }
  } else {
    for (AstListNode_* field_node = constructor->params.head; field_node != NULL; field_node = field_node->next) {
      AstClassConstructorParam_* constructor_field = AST_CAST(AstClassConstructorParam_, field_node->node);
      code_gen(chunk, field_node->node);
    }
  }
}

static void class_constructor_field_code_gen(Chunk_* chunk, AstNode_* node) {
  AstClassConstructorParam_* field = (AstClassConstructorParam_*)node;
  code_gen(chunk, (AstNode_*)field->expr);
}

static CodeGenRule_ code_gen_rules[] = {
  [AST_CLS(AstProgram_)]                = {program_code_gen},
  [AST_CLS(AstBlock_)]                  = {block_code_gen},
  [AST_CLS(AstStmt_)]                   = {stmt_code_gen},
  [AST_CLS(AstExpr_)]                   = {expr_code_gen},
  [AST_CLS(AstPrintStmt_)]              = {print_code_gen},
  [AST_CLS(AstUnaryExp_)]               = {unary_code_gen},
  [AST_CLS(AstBinaryExp_)]              = {binary_code_gen},
  [AST_CLS(AstPrimaryExp_)]             = {primary_code_gen},
  [AST_CLS(AstReturnStmt_)]             = {return_code_gen},
  [AST_CLS(AstIfStmt_)]                 = {if_code_gen},
  [AST_CLS(AstAssertStmt_)]             = {assert_code_gen},
  [AST_CLS(AstVarDeclStmt_)]            = {var_decl_code_gen},
  [AST_CLS(AstVarExpr_)]                = {var_expr_code_gen},
  [AST_CLS(AstIdExpr_)]                 = {id_expr_code_gen},
  [AST_CLS(AstAssignmentExpr_)]         = {assignment_expr_code_gen},
  [AST_CLS(AstWhileStmt_)]              = {while_stmt_code_gen},
  [AST_CLS(AstFunctionDef_)]            = {function_def_code_gen},
  [AST_CLS(AstFunctionBody_)]           = {function_body_code_gen},
  [AST_CLS(AstFunctionParam_)]          = {noop_code_gen},
  [AST_CLS(AstFunctionCall_)]           = {function_call_code_gen},
  [AST_CLS(AstFunctionCallArgs_)]       = {function_args_code_gen},
  [AST_CLS(AstFunctionCallArg_)]        = {function_arg_code_gen},
  [AST_CLS(AstExpressionStmt_)]         = {expression_statement_code_gen},
  [AST_CLS(AstNoopExpr_)]               = {noop_code_gen},
  [AST_CLS(AstNoopStmt_)]               = {noop_code_gen},
  [AST_CLS(AstCleanUpTemps_)]           = {clean_up_temps_code_gen},
  [AST_CLS(AstTmpDecl_)]                = {tmp_decl_code_gen},
  [AST_CLS(AstClassDef_)]               = {noop_code_gen},
  [AST_CLS(AstClassMemberDecl_)]        = {noop_code_gen},
  [AST_CLS(AstClassConstructor_)]            = {class_constructor_code_gen},
  [AST_CLS(AstClassConstructorParam_)]       = {class_constructor_field_code_gen},
  [AST_CLS(AstDotExpr_)]                = {dot_expr_code_gen},
  [AST_CLS(AstTypeExpr_)]               = {noop_code_gen},
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
#endif