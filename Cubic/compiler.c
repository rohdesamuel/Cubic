#include "compiler.h"

#include "common.h"
#include "debug.h"
#include "scanner.h"
#include "parser.h"
#include "ast.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>

typedef void (*CodeGenFn)(AstNode_*);

typedef struct CodeGenRule_ {
  CodeGenFn code_gen;
} CodeGenRule_;

// TODO: move this out of global scope
Chunk compiling_chunk_;

inline static Chunk current_chunk() {
  return compiling_chunk_;
}

static void end_compiler(Parser_* parser);
static CodeGenRule_* get_rule(int type);

static void emit_return(int line);

static void code_gen(AstNode_* node) {
  get_rule(node->cls)->code_gen(node);
}

bool compile(const char* source, Chunk_* chunk) {
  Parser_ parser;
  Scanner_ scanner;

  compiling_chunk_ = chunk;
  scanner_init(&scanner, source);
  parser_init(&parser);

  Ast_ ast;
  memset(&ast, 0, sizeof(Ast_));

  AstNode_* root = parse(&parser, &scanner, source);
  code_gen(root);

  end_compiler(&parser);
  compiling_chunk_ = NULL;
  return !parser.had_error;
}

static void end_compiler(Parser_* parser) {
  emit_return(parser->current.line);

#ifdef DEBUG_PRINT_CODE
  if (!parser->had_error) {
    chunk_disassemble(current_chunk(), "code");
  }
#endif
}


///////////////////////////////////////////////////////////////////////////////

static uint8_t make_constant(Value_ value) {
  int constant = chunk_addconstant(current_chunk(), value);

  // TODO: add in OP_CONSTANT_LONG support
  if (constant > UINT8_MAX) {
    // error("Too many constants in one chunk.");
    return 0;
  }

  return (uint8_t)constant;
}

static void emit_byte(uint8_t byte, int line) {
  chunk_write(current_chunk(), byte, line);
}

static void emit_bytes(uint8_t byte1, uint8_t byte2, int line) {
  emit_byte(byte1, line);
  emit_byte(byte2, line);
}

static void emit_return(int line) {
  emit_byte(OP_RETURN, line);
}

static void emit_constant(Value_ value, int line) {
  emit_bytes(OP_CONSTANT, make_constant(value), line);
}

static void unary_code_gen(AstNode_* node) {
  AstUnaryExp_* exp = (AstUnaryExp_*)node;
  code_gen(exp->expr);
  // Emit the operator instruction.
  switch (exp->op) {
    case TK_MINUS: emit_byte(OP_NEGATE, node->line); break;
    default: assertf(false, "UnaryOp '%d' unimplemented", exp->op);  return; // Unreachable.
  }
}

static void binary_code_gen(AstNode_* node) {
  AstBinaryExp_* exp = (AstBinaryExp_*)node;
  code_gen(exp->left);
  code_gen(exp->right);

  switch (exp->op) {
    case TK_PLUS:          emit_byte(OP_ADD, node->line); break;
    case TK_MINUS:         emit_byte(OP_SUB, node->line); break;
    case TK_STAR:          emit_byte(OP_MUL, node->line); break;
    case TK_SLASH:         emit_byte(OP_DIV, node->line); break;
    default: assertf(false, "BinaryOp '%d' unimplemented", exp->op);  return; // Unreachable.
  }
}

static void integer_constant_code_gen(AstNode_* node) {
  AstIntegerExp_* exp = (AstIntegerExp_*)node;
  emit_constant(INT_VAL(exp->value), node->line);
}

static void number_constant_code_gen(AstNode_* node) {
  AstNumberExp_* exp = (AstNumberExp_*)node;
  emit_constant(DOUBLE_VAL(exp->value), node->line);
}

CodeGenRule_ code_gen_rules[] = {
  [AST_PRINT_STMT] = {NULL},
  [AST_UNARY_OP_EXPR] = {unary_code_gen},
  [AST_BINARY_OP_EXPR] = {binary_code_gen},
  [AST_INTEGER_CONSTANT] = {integer_constant_code_gen},
  [AST_NUMBER_CONSTANT] = {number_constant_code_gen},
};

static CodeGenRule_* get_rule(int type) {
  CodeGenRule_* ret = &code_gen_rules[type];
  assertf(ret->code_gen, "Could not find rule for AST class: %d", type);
  return ret;
}