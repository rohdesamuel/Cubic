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
Compiler_* current_compiler;

inline static Chunk current_chunk() {
  return compiling_chunk_;
}

static void end_compiler(Parser_* parser);
static CodeGenRule_* get_rule(int type);

static void emit_return(int line);

static void beginScope() {
  ++current_compiler->scope_depth;
}

static void endScope() {
  --current_compiler->scope_depth;
}

static void code_gen(AstNode_* node) {
  get_rule(node->cls)->code_gen(node);
}

static void compiler_init(Compiler_* compiler) {
  compiler->locals_count = 0;
  compiler->locals_capacity = UINT8_MAX + 1;
  compiler->scope_depth = 0;
  current_compiler = compiler;
}

bool compile(const char* source, Chunk_* chunk) {
  Parser_ parser;
  Scanner_ scanner;
  Compiler_ compiler;

  compiling_chunk_ = chunk;
  
  scanner_init(&scanner, source);
  parser_init(&parser);
  compiler_init(&compiler);

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
  current_compiler = NULL;

#ifdef DEBUG_PRINT_CODE
  if (!parser->had_error) {
    chunk_disassemble(current_chunk(), "code");
  }
#endif

  parser_clear(parser);
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

static int emit_jmp(uint8_t byte, int line) {
  emit_byte(byte, line);
  emit_byte(OP_NOP, line);
  emit_byte(OP_NOP, line);
  return current_chunk()->count - 2;
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
    case TK_NOT: emit_byte(OP_NOT, node->line); break;
    default: assertf(false, "UnaryOp '%d' unimplemented", exp->op);  return; // Unreachable.
  }
}

static void binary_code_gen(AstNode_* node) {
  AstBinaryExp_* exp = (AstBinaryExp_*)node;
  code_gen(exp->left);
  code_gen(exp->right);

  switch (exp->op) {
    case TK_AND:           emit_byte(OP_AND, node->line); break;
    case TK_OR:            emit_byte(OP_OR, node->line); break;
    case TK_XOR:           emit_byte(OP_XOR, node->line); break;
    case TK_PLUS:          emit_byte(OP_ADD, node->line); break;
    case TK_MINUS:         emit_byte(OP_SUB, node->line); break;
    case TK_STAR:          emit_byte(OP_MUL, node->line); break;
    case TK_SLASH:         emit_byte(OP_DIV, node->line); break;
    // case TK_DOUBLE_SLASH:  emit_byte(OP_DIV, node->line); break; // TODO: convert integers to floats
    case TK_PERCENT:       emit_byte(OP_MOD, node->line); break;
    case TK_EQUAL_EQUAL:   emit_byte(OP_EQ, node->line); break;
    case TK_BANG_EQUAL:    emit_byte(OP_NEQ, node->line); break;
    case TK_GT:            emit_byte(OP_GT, node->line); break;
    case TK_GTE:           emit_byte(OP_GTE, node->line); break;
    case TK_LT:            emit_byte(OP_LT, node->line); break;
    case TK_LTE:           emit_byte(OP_LTE, node->line); break;
    case TK_LSHIFT:        emit_byte(OP_LSHIFT, node->line); break;
    case TK_RSHIFT:        emit_byte(OP_RSHIFT, node->line); break;
    case TK_AMPERSAND:     emit_byte(OP_BITWISE_AND, node->line); break;
    case TK_PIPE:          emit_byte(OP_BITWISE_OR, node->line); break;
    case TK_HAT:           emit_byte(OP_BITWISE_XOR, node->line); break;
    default: printf("[Line %d] BinaryOp '%d' unimplemented\n", exp->base.base.line, exp->op);  break; // Unreachable.
  }
}

static void primary_code_gen(AstNode_* node) {
  AstPrimaryExp_* exp = (AstPrimaryExp_*)node;
  if (IS_NIL(exp->value)) {
    emit_byte(OP_NIL, exp->base.base.line);
  } else if (IS_BOOL(exp->value)) {
    if (exp->value.as.b) {
      emit_byte(OP_TRUE, exp->base.base.line);
    } else {
      emit_byte(OP_FALSE, exp->base.base.line);
    }
  } else {
    emit_constant(exp->value, exp->base.base.line);
  }
}

static void print_code_gen(AstNode_* node) {
  AstPrintStmt_* stmt = (AstPrintStmt_*)node;
  code_gen(stmt->expr);
  emit_byte(OP_PRINT, node->line);
}

static void program_code_gen(AstNode_* node) {
  AstProgram_* program = (AstProgram_*)node;
  code_gen((AstNode_*)program->block);
}

static void begin_scope() {
  ++current_compiler->scope_depth;
}

static void end_scope() {
  --current_compiler->scope_depth;
  while (current_compiler->locals_count > 0 &&
    current_compiler->locals[current_compiler->locals_count - 1].depth >
    current_compiler->scope_depth) {
    emit_byte(OP_POP, -1);
    current_compiler->locals_count--;
  }  
}

static void block_code_gen(AstNode_* node) {
  AstBlock_* block = (AstBlock_*)node;
  begin_scope();
  for (AstListNode_* n = block->statements.head; n != NULL; n = n->next) {
    code_gen(n->node);
  }
  end_scope();
}

static void return_code_gen(AstNode_* node) {
  AstReturnStmt_* stmt = (AstReturnStmt_*)node;
  if (stmt->expr) {
    code_gen(stmt->expr);
  } else {
    emit_constant(NIL_VAL, node->line);
  }
  emit_return(node->line);
}

static void patch_jmp(int offset) {
  // -2 to adjust for the bytecode for the jump offset itself.
  int jump = current_chunk()->count - offset - 2;
  //assertf(jump > UINT16_MAX, "Too much code to jump over.");

  current_chunk()->code[offset] = (jump >> 8) & 0xff;
  current_chunk()->code[offset + 1] = jump & 0xff;
}

static void patch_jmplist(int start_offset, int num_jumps) {
  uint8_t* start = current_chunk()->code;
  uint16_t count = current_chunk()->count;

  uint16_t offset = start_offset;
  for (int i = 0; i < num_jumps; ++i) {
    uint8_t* jump = start + offset;
    int next_offset = offset + (uint16_t)((*jump << 8) | *(jump + 1));
    patch_jmp(offset);
    offset = next_offset;
  }
}

static void if_code_gen(AstNode_* node) {
  AstIfStmt_* stmt = (AstIfStmt_*)node;

  // if `condition_expr`
  code_gen(stmt->condition_expr);

  // if false then ...
  int then_jmp = emit_jmp(OP_JMP_IF_FALSE, node->line); // jmp :skip_then

  // if true then ...
  emit_byte(OP_POP, node->line);
  code_gen(stmt->if_stmt);
  int else_jmp = emit_jmp(OP_JMP, node->line); // jmp :end

  // :skip_then
  patch_jmp(then_jmp);
  emit_byte(OP_POP, node->line);

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
  while(elif_e != NULL && elif_s != NULL) {
    AstNode_* elif_expr = elif_e->node;
    AstNode_* elif_stmt = elif_s->node;

    code_gen(elif_expr);
    elif_jmp = emit_jmp(OP_JMP_IF_FALSE, node->line);    

    emit_byte(OP_POP, elif_expr->line);
    code_gen(elif_stmt);
    int end_jmp = emit_jmp(OP_JMP, node->line);

    // Append the jump to the end of the list.
    if (start_jmp_list == -1) {
      start_jmp_list = end_jmp;
      end_jmp_list = end_jmp;
    } else {
      patch_jmp(end_jmp_list);
      end_jmp_list = end_jmp;
      num_jumps += 1;
    }

    patch_jmp(elif_jmp);
    emit_byte(OP_POP, elif_expr->line);

    elif_e = elif_e->next;
    elif_s = elif_s->next;

    assertf(elif_e == NULL && elif_s == NULL ||
            elif_e != NULL && elif_s != NULL,
            "Malformed list: every elif should have an expression and statement");
  }

  // :else
  if (stmt->else_stmt) {
    code_gen(stmt->else_stmt);
  }

  // :end
  // Patch all the jumps in the list to go here.
  if (start_jmp_list != -1) {
    patch_jmp(end_jmp_list);
    patch_jmplist(start_jmp_list, num_jumps);
  }
  patch_jmp(else_jmp);

  // end
}

static void assert_code_gen(AstNode_* node) {
  AstAssertStmt_* stmt = (AstAssertStmt_*)node;
  code_gen(stmt->expr);
  emit_byte(OP_ASSERT, node->line);
}

static int add_local(Token_* name) {
  assertf(current_compiler->locals_count < current_compiler->locals_capacity,
    "Exceeded number of maximum local variables %d >= %d",
    current_compiler->locals_count,
    current_compiler->locals_capacity);

  Local* local = &current_compiler->locals[current_compiler->locals_count++];
  local->name = *name;
  local->depth = current_compiler->scope_depth;

  return current_compiler->locals_count - 1;
}

static bool identifiers_equal(Token_* a, Token_* b) {
  if (a->length != b->length) return false;
  return memcmp(a->start, b->start, a->length) == 0;
}

static int declare_variable(AstVarDeclStmt_* stmt) {
  for (int i = current_compiler->locals_count - 1; i >= 0; i--) {
    Local* local = &current_compiler->locals[i];
    if (local->depth != -1 && local->depth < current_compiler->scope_depth) {
      break;
    }

    if (identifiers_equal(&stmt->name, &local->name)) {
      fprintf(stderr, "Already a variable with this name in this scope.");
    }
  }

  return add_local(&stmt->name);
}

static void var_decl_code_gen(AstNode_* node) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)node;
  int slot = declare_variable(stmt);

  // TODO: allow for multiple expressions.
  for (AstListNode_* n = stmt->exprs.head; n != NULL; n = n->next) {
    code_gen(n->node);
    emit_bytes(OP_SET_VAR, (uint8_t)slot, node->line);
  }
}

static void var_expr_code_gen(AstNode_* node) {
  AstVarExpr_* expr = (AstVarExpr_*)node;
  code_gen((AstNode_*)expr->expr);
}

static int resolve_local(Compiler_* compiler, Token_* name) {
  for (int i = compiler->locals_count - 1; i >= 0; i--) {
    Local* local = &compiler->locals[i];
    if (identifiers_equal(name, &local->name)) {
      return i;
    }
  }

  return -1;
}

static void id_expr_code_gen(AstNode_* node) {
  AstIdExpr_* expr = (AstIdExpr_*)node;
  int slot = resolve_local(current_compiler, &expr->name);
  assertf(slot != -1, "Could not find variable %.*s", expr->name.length, expr->name.start);

  emit_bytes(OP_GET_VAR, (uint8_t)slot, node->line);
}

static void assignment_stmt_code_gen(AstNode_* node) {
  AstAssignmentStmt_* stmt = (AstAssignmentStmt_*)node;

  // TODO: allow for assigning to more than simple variables
  // TODO: allow for assigning to multiple variables.
  AstVarExpr_* var = (AstVarExpr_*)stmt->vars.head->node;
  assertf(var->expr->base.cls == AST_CLS(AstIdExpr_),
    "Only assigning to variables is supported at this time.");
  
  AstIdExpr_* expr = (AstIdExpr_*)var->expr;
  int slot = resolve_local(current_compiler, &expr->name);

  code_gen(stmt->exprs.head->node);
  emit_bytes(OP_SET_VAR, (uint8_t)slot, node->line);
}

CodeGenRule_ code_gen_rules[] = {
  [AST_CLS(AstProgram_)]        = {program_code_gen},
  [AST_CLS(AstBlock_)]          = {block_code_gen},
  [AST_CLS(AstPrintStmt_)]      = {print_code_gen},
  [AST_CLS(AstUnaryExp_)]       = {unary_code_gen},
  [AST_CLS(AstBinaryExp_)]      = {binary_code_gen},
  [AST_CLS(AstPrimaryExp_)]     = {primary_code_gen},
  [AST_CLS(AstReturnStmt_)]     = {return_code_gen},
  [AST_CLS(AstIfStmt_)]         = {if_code_gen},
  [AST_CLS(AstAssertStmt_)]     = {assert_code_gen},
  [AST_CLS(AstVarDeclStmt_)]    = {var_decl_code_gen},
  [AST_CLS(AstVarExpr_)]        = {var_expr_code_gen},
  [AST_CLS(AstIdExpr_)]         = {id_expr_code_gen},
  [AST_CLS(AstAssignmentStmt_)] = {assignment_stmt_code_gen},
};

static CodeGenRule_* get_rule(int type) {
  CodeGenRule_* ret = &code_gen_rules[type];
  assertf(ret->code_gen, "Could not find rule for AST class: %d", type);
  return ret;
}