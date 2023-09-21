#include "compiler.h"

#include "common.h"
#include "object.h"
#include "debug.h"
#include "scanner.h"
#include "parser.h"
#include "analyzer.h"
#include "ast.h"
#include "symbol_table.h"
#include "tac_compiler.h"
#include "map.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>

Compiler_* current_compiler;

static void end_compiler(Parser_* parser, Chunk_* chunk);
static void codegen(Compiler_* compiler, Chunk_* chunk, const TacChunk_* root, struct MemoryAllocator_* allocator);
static void codegen_tac(Compiler_* compiler, Chunk_* chunk, const TacChunk_* tac_chunk, struct MemoryAllocator_* allocator);

static void compiler_init(Compiler_* compiler, MemoryAllocator_* allocator) {
  compiler->locals_count = 0;
  compiler->locals_capacity = UINT8_MAX + 1;
  compiler->scope_depth = 0;
  compiler->allocator = allocator;
  compiler->label_locations = hashmap_create();
  compiler->patch_lists = hashmap_create();
  current_compiler = compiler;
}

static void compiler_clear(Compiler_* compiler) {
  hashmap_free(compiler->patch_lists);
  hashmap_free(compiler->label_locations);
}

bool compile(const char* source, Chunk_* chunk) {
  PageAllocator_ allocator;
  Parser_ parser;
  Scanner_ scanner;
  TacCompiler_ tac_compiler;
  Compiler_ compiler;
  Analyzer_ analyzer;
  pageallocator_init(&allocator, 1ull << 14);

  scanner_init(&scanner, source);
  parser_init(&parser, (MemoryAllocator_*)&allocator);
  analyzer_init(&analyzer, (MemoryAllocator_*)&allocator);
  tac_compiler_init(&tac_compiler, (MemoryAllocator_*)&allocator);
  compiler_init(&compiler, (MemoryAllocator_*)&allocator);

  AstNode_* root = parse(&parser, &scanner, analyzer.scope, source);
  bool error = parser.had_error;
  if (!error) {
    analyze(&analyzer, (AstProgram_*)root);
  }

  error = error || analyzer.had_error;
  if (!error) {
    tac_compiler_compile(&tac_compiler, root);
    codegen(&compiler, chunk, &tac_compiler.chunk, (MemoryAllocator_*)&allocator);
  }
  
  end_compiler(&parser, chunk);

  compiler_clear(&compiler);
  tac_compiler_clear(&tac_compiler);
  analyzer_clear(&analyzer);
  parser_clear(&parser);

  pageallocator_deinit(&allocator);
  return !error;
}

static void end_compiler(Parser_* parser, Chunk_* chunk) {
  //emit_return(chunk, parser->current.line);
  current_compiler = NULL;

#ifdef DEBUG_PRINT_CODE
  if (!parser->had_error) {
    chunk_disassemble(current_chunk(), "code");
  }
#endif  
}

///////////////////////////////////////////////////////////////////////////////

static uint8_t make_constant(Chunk_* chunk, Value_ value) {
  int constant = chunk_addconstant(chunk, value);

  // TODO: add in OP_CONSTANT_LONG support
  if (constant > UINT8_MAX) {
    // error("Too many constants in one chunk.");
    return 0;
  }

  return (uint8_t)constant;
}

static void emit_byte(Chunk_* chunk, uint8_t byte, int line) {
  chunk_write(chunk, byte, line);
}

static void emit_bytes(Chunk_* chunk, uint8_t byte1, uint8_t byte2, int line) {
  emit_byte(chunk, byte1, line);
  emit_byte(chunk, byte2, line);
}

static void emit_short(Chunk_* chunk, uint16_t s, int line) {
  emit_byte(chunk, (s & 0xFF00) >> 8, line);
  emit_byte(chunk, s & 0x00FF, line);
}

static void emit_long(Chunk_* chunk, uint32_t s, int line) {
  emit_byte(chunk, (s & 0xFF000000) >> 24, line);
  emit_byte(chunk, (s & 0x00FF0000) >> 16, line);
  emit_byte(chunk, (s & 0x0000FF00) >> 8, line);
  emit_byte(chunk,  s & 0x000000FF, line);
}

static void emit_longlong(Chunk_* chunk, uint64_t s, int line) {
  emit_byte(chunk, (uint8_t)((s & 0xFF00000000000000ll) >> 56), line);
  emit_byte(chunk, (uint8_t)((s & 0x00FF000000000000ll) >> 48), line);
  emit_byte(chunk, (uint8_t)((s & 0x0000FF0000000000ll) >> 40), line);
  emit_byte(chunk, (uint8_t)((s & 0x000000FF00000000ll) >> 32), line);
  emit_byte(chunk, (uint8_t)((s & 0x00000000FF000000ll) >> 24), line);
  emit_byte(chunk, (uint8_t)((s & 0x0000000000FF0000ll) >> 16), line);
  emit_byte(chunk, (uint8_t)((s & 0x000000000000FF00ll) >>  8), line);
  emit_byte(chunk, (uint8_t)( s & 0x00000000000000FFll), line);
}

static void emit_constant(Chunk_* chunk, Location_ loc, Value_ value, int line) {
  emit_byte(chunk, OP_CONSTANT, line);
  emit_byte(chunk, loc.frame_offset, line);
  emit_byte(chunk, make_constant(chunk, value), line);
}

static void print_gen(Chunk_* chunk, Tac_* tac) {
  emit_byte(chunk, OP_PRINT, tac->line);
  emit_byte(chunk, tac->arg_l.loc.frame_offset, tac->line);
  emit_long(chunk, type_toint(tac->arg_l.opt_type), tac->line);
}

static void label_memoize(Token_* label, Hashmap* labels, Chunk_* chunk, uintptr_t ip) {
  hashmap_get_set(labels, label->start, label->length, &ip);
}

static bool label_find_or_defer(Token_* label, Hashmap* labels, Hashmap* patch_lists, uintptr_t* ip, struct MemoryAllocator_* allocator) {
  uintptr_t jmp_ip = *ip;
  if (hashmap_get(labels, label->start, label->length, ip)) {
    return true;
  }

  List_* patch_list = NULL;
  if (!hashmap_get(patch_lists, label->start, label->length, (uintptr_t*)&patch_list)) {
    patch_list = alloc_ty(allocator, List_);
    list_of(patch_list, uintptr_t, allocator);
    hashmap_set(patch_lists, label->start, label->length, (uintptr_t)patch_list);
  }

  list_push(patch_list, &jmp_ip);

  return false;
}

typedef struct LabelResolveState_ {
  Hashmap* labels;
  Chunk_* chunk;
} LabelResolveState_;

static void label_patch(uintptr_t jump_dst, uintptr_t patch_offset, Chunk_* chunk) {
  OpCode op = *(chunk->code + patch_offset);
  assertf(op == OP_JMP || op == OP_JMP_IF_FALSE || op == OP_CALL, "Patch location is not a jump. Got: %d", op);

  uint32_t offset = (uint32_t)jump_dst;
  uint8_t* patch_dst = chunk->code + patch_offset + 1;
  patch_dst[0] = (offset & 0xFF000000) >> 24;
  patch_dst[1] = (offset & 0x00FF0000) >> 16;
  patch_dst[2] = (offset & 0x0000FF00) >> 8;
  patch_dst[3] = (offset & 0x000000FF) >> 0;
}

static void label_resolve_jump_list(const void* key, size_t ksize, uintptr_t patch_list_ptr, void* usr) {
  LabelResolveState_* state = (LabelResolveState_*)usr;

  uintptr_t jump_dst = 0;
  assertf(hashmap_get(state->labels, key, ksize, &jump_dst), "Unresolved label: '%.*s'", (int)ksize, (const char*)key);

  List_* patch_list = (List_*)patch_list_ptr;
  for (ListNode_* n = patch_list->head; n != NULL; n = n->next) {
    uintptr_t patch_offset = list_val(n, uintptr_t);
    label_patch((uintptr_t)jump_dst, patch_offset, state->chunk);
  }
}

static void label_resolve_jumps(Hashmap* labels, Hashmap* patch_lists, Chunk_* chunk) {
  LabelResolveState_ state = {
    .labels = labels,
    .chunk = chunk
  };

  hashmap_iterate(patch_lists, label_resolve_jump_list, &state);
}

struct CodegenFnState {
  Compiler_* compiler;
  Chunk_* chunk;
  struct MemoryAllocator_* allocator;
};

static void codegen_fn(const void* key, size_t ksize, uintptr_t value, void* usr) {
  struct CodegenFnState* state = (struct CodegenFnState*)usr;
  TacChunk_* tac_chunk = (TacChunk_*)value;
  hashmap_iterate(tac_chunk->fn_code, codegen_fn, state);
  codegen_tac(state->compiler, state->chunk, tac_chunk, state->allocator);
}

static void codegen(Compiler_* compiler, Chunk_* chunk, const TacChunk_* root, struct MemoryAllocator_* allocator) {
  codegen_tac(compiler, chunk, root, allocator);
  struct CodegenFnState state = {
    .compiler = compiler,
    .chunk = chunk,
    .allocator = allocator
  };

  hashmap_iterate(root->fn_code, codegen_fn, &state);

  label_resolve_jumps(compiler->label_locations, compiler->patch_lists, chunk);
}

static void codegen_tac(Compiler_* compiler, Chunk_* chunk, const TacChunk_* tac_chunk, struct MemoryAllocator_* allocator) {
#define UNARY_OP(OP) case OP: \
    emit_byte(chunk, OP, line); \
    emit_byte(chunk, tac->dst.frame_offset, line); \
    emit_byte(chunk, tac->arg_l.loc.frame_offset, line); \
    break

#define BINARY_OP(OP) case OP: \
    emit_byte(chunk, OP, line); \
    emit_byte(chunk, tac->dst.frame_offset, line); \
    emit_byte(chunk, tac->arg_l.loc.frame_offset, line); \
    emit_byte(chunk, tac->arg_r.loc.frame_offset, line); \
    break

  for (int i = 0; i < tac_chunk->count; ++i) {
    Tac_* tac = tac_chunk->code + i;
    int line = tac->line;
    switch (tac->op) {
      case OP_NOP:
      {
        if (tac->dst.type == LOCATION_TYPE_LABEL) {
          uintptr_t ip = chunk->count;
          label_memoize(&tac->dst.token, compiler->label_locations, chunk, ip);
        }
        break;
      }

      case OP_RETURN:
        emit_byte(chunk, OP_RETURN, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        break;

      case OP_JMP:
      {
        Token_* label = &tac->dst.token;
        uintptr_t ip = chunk->count;
        label_find_or_defer(label, compiler->label_locations, compiler->patch_lists, &ip, allocator);

        emit_byte(chunk, OP_JMP, line);
        emit_long(chunk, (uint32_t)ip, line);
        break;
      }

      case OP_JMP_IF_FALSE:
      {
        Token_* label = &tac->dst.token;
        uintptr_t ip = chunk->count;
        label_find_or_defer(label, compiler->label_locations, compiler->patch_lists, &ip, allocator);

        emit_byte(chunk, OP_JMP_IF_FALSE, line);
        emit_long(chunk, (uint32_t)ip, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        break;
      }

      case OP_CALL:
      {
        Token_* label = &tac->arg_l.loc.token;
        uintptr_t ip = chunk->count;
        label_find_or_defer(label, compiler->label_locations, compiler->patch_lists, &ip, allocator);

        emit_byte(chunk, OP_CALL, line);
        emit_long(chunk, (uint32_t)ip, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_long(chunk, (uint32_t)tac->arg_r.size, line);
        break;
      }

      case OP_PROLOGUE:
      {
        emit_byte(chunk, OP_PROLOGUE, line);
        emit_long(chunk, (uint32_t)tac->arg_l.size, line);
        emit_long(chunk, (uint32_t)tac->arg_r.size, line);
        break;
      }

      case OP_NIL:
        emit_byte(chunk, OP_NIL, tac->line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        break;

      case OP_TRUE:
        emit_byte(chunk, OP_TRUE, tac->line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        break;

      case OP_FALSE:
        emit_byte(chunk, OP_FALSE, tac->line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        break;

      case OP_CONSTANT:
        emit_constant(chunk, tac->dst, tac->arg_l.val, tac->line);
        break;

      case OP_CAST_f2i:
        emit_byte(chunk, OP_CAST_f2i, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        break;

      case OP_CAST_f2d:
        emit_byte(chunk, OP_CAST_f2d, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        break;

      case OP_CAST_d2i:
        emit_byte(chunk, OP_CAST_d2i, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        break;

      case OP_CAST_d2f:
        emit_byte(chunk, OP_CAST_d2f, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        break;

      case OP_CAST_i2f:
        emit_byte(chunk, OP_CAST_i2f, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        break;

      case OP_CAST_i2d:
        emit_byte(chunk, OP_CAST_i2d, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        break;

      case OP_PRINT:
        print_gen(chunk, tac);
        break;

      case OP_MOVE:
        emit_byte(chunk, OP_MOVE, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        break;

      case OP_LEA:
        emit_byte(chunk, OP_LEA, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        emit_longlong(chunk, tac->arg_r.size, line);
        break;

      case OP_MEMCPY:
        emit_byte(chunk, OP_MEMCPY, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        emit_byte(chunk, (uint8_t)tac->arg_r.size, line);
        break;

      case OP_LOAD:
        emit_byte(chunk, OP_LOAD, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        emit_byte(chunk, tac->arg_r.offset, line);
        break;

      case OP_LOADA:
        emit_byte(chunk, OP_LOADA, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        emit_byte(chunk, tac->arg_r.offset, line);
        break;

      case OP_STORE:
        emit_byte(chunk, OP_STORE, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        emit_byte(chunk, tac->arg_r.offset, line);
        break;

      case OP_RLOAD:
        emit_byte(chunk, OP_RLOAD, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        emit_byte(chunk, tac->arg_r.offset, line);
        break;

      case OP_RLOADA:
        emit_byte(chunk, OP_RLOADA, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        emit_byte(chunk, tac->arg_r.offset, line);
        break;

      case OP_RSTORE:
        emit_byte(chunk, OP_RSTORE, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        emit_byte(chunk, tac->arg_r.offset, line);
        break;

      case OP_REF_MAKE:
        emit_byte(chunk, OP_REF_MAKE, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_longlong(chunk, tac->arg_l.size, line);
        break;

      case OP_REF_DEL:
        emit_byte(chunk, OP_REF_DEL, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        break;

      case OP_ASSERT:
        emit_byte(chunk, OP_ASSERT, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        break;

      case OP_ADDIMM:
        emit_byte(chunk, OP_ADDIMM, line);
        emit_byte(chunk, tac->dst.frame_offset, line);
        emit_byte(chunk, tac->arg_l.loc.frame_offset, line);
        emit_long(chunk, tac->arg_r.offset, line);
        break;

      BINARY_OP(OP_EQ);
      BINARY_OP(OP_GT);
      BINARY_OP(OP_GTE);
      BINARY_OP(OP_LT);
      BINARY_OP(OP_LTE);
      BINARY_OP(OP_CMP);
      BINARY_OP(OP_ICMP);
      BINARY_OP(OP_FCMP);
      BINARY_OP(OP_DCMP);
      BINARY_OP(OP_ADD);
      BINARY_OP(OP_SUB);
      BINARY_OP(OP_MUL);
      BINARY_OP(OP_DIV);
      BINARY_OP(OP_IMUL);
      BINARY_OP(OP_IDIV);
      BINARY_OP(OP_FADD);
      BINARY_OP(OP_FSUB);
      BINARY_OP(OP_FMUL);
      BINARY_OP(OP_FDIV);
      BINARY_OP(OP_DADD);
      BINARY_OP(OP_DSUB);
      BINARY_OP(OP_DMUL);
      BINARY_OP(OP_DDIV);
      BINARY_OP(OP_CONCAT);
      BINARY_OP(OP_MOD);
      BINARY_OP(OP_IMOD);
      BINARY_OP(OP_BITWISE_AND);
      BINARY_OP(OP_BITWISE_OR);
      BINARY_OP(OP_BITWISE_XOR);
      BINARY_OP(OP_BITWISE_NOT);
      BINARY_OP(OP_LSHIFT);
      BINARY_OP(OP_RSHIFT);
      BINARY_OP(OP_AND);
      BINARY_OP(OP_OR);
      BINARY_OP(OP_XOR);
      UNARY_OP(OP_NOT);
      UNARY_OP(OP_NEG);
      UNARY_OP(OP_FNEG);
      UNARY_OP(OP_DNEG);
    }
  }  

#undef UNARY_OP
#undef BINARY_OP
}
