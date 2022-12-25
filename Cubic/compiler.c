#include "compiler.h"

#include "common.h"
#include "debug.h"
#include "scanner.h"
#include "parser.h"
#include "analyzer.h"
#include "ast.h"
#include "symbol_table.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>

typedef void (*CodeGenFn)(Chunk_*, AstNode_*);

typedef struct CodeGenRule_ {
  CodeGenFn code_gen;
} CodeGenRule_;

Compiler_* current_compiler;

static void end_compiler(Parser_* parser, Chunk_* chunk);
static CodeGenRule_* get_rule(int type);

static void emit_return(Chunk_* chunk, int line);

static void code_gen(Chunk_* chunk, AstNode_* node) {
  get_rule(node->cls)->code_gen(chunk, node);
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
  Analyzer_ analyzer;
  
  scanner_init(&scanner, source);
  parser_init(&parser);  
  analyzer_init(&analyzer);
  compiler_init(&compiler);

  AstNode_* root = parse(&parser, &scanner, analyzer.scope, source);

  if (!parser.had_error) {
    analyze(&analyzer, (AstProgram_*)root);
  }

  if (!parser.had_error && !analyzer.had_error) {
    code_gen(chunk, root);
  }  
  
  end_compiler(&parser, chunk);
  analyzer_clear(&analyzer);
  parser_clear(&parser);
  return !parser.had_error && !analyzer.had_error;
}

static void end_compiler(Parser_* parser, Chunk_* chunk) {
  emit_return(chunk, parser->current.line);
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
  emit_byte(chunk, s  & 0x000000FF, line);
}

static int emit_jmp(Chunk_* chunk, uint8_t byte, int line) {
  emit_byte(chunk, byte, line);
  emit_byte(chunk, OP_NOP, line);
  emit_byte(chunk, OP_NOP, line);
  return chunk->count - 2;
}

static void emit_return(Chunk_* chunk, int line) {
  emit_byte(chunk, OP_RETURN, line);
}

static void emit_constant(Chunk_* chunk, Value_ value, int line) {
  emit_bytes(chunk, OP_CONSTANT, make_constant(chunk, value), line);
}

static void unary_code_gen(Chunk_* chunk, AstNode_* node) {
  AstUnaryExp_* exp = (AstUnaryExp_*)node;
  code_gen(chunk, exp->expr);
  // Emit the operator instruction.
  switch (exp->op) {
    case TK_MINUS: emit_byte(chunk, OP_NEGATE, node->line); break;
    case TK_NOT: emit_byte(chunk, OP_NOT, node->line); break;
    default: assertf(false, "UnaryOp '%d' unimplemented", exp->op);  return; // Unreachable.
  }
}

static void binary_code_gen(Chunk_* chunk, AstNode_* node) {
  AstBinaryExp_* exp = (AstBinaryExp_*)node;
  AstExpr_* l = AS_EXPR(exp->left);
  AstExpr_* r = AS_EXPR(exp->right);
  
  code_gen(chunk, (AstNode_*)l);
  code_gen(chunk, (AstNode_*)r);

  switch (exp->op) {
    case TK_AND:           emit_byte(chunk, OP_AND, node->line); break;
    case TK_OR:            emit_byte(chunk, OP_OR, node->line); break;
    case TK_XOR:           emit_byte(chunk, OP_XOR, node->line); break;
    case TK_PLUS:          emit_byte(chunk, OP_ADD, node->line); break;
    case TK_MINUS:         emit_byte(chunk, OP_SUB, node->line); break;
    case TK_STAR:          emit_byte(chunk, OP_MUL, node->line); break;
    case TK_SLASH:         emit_byte(chunk, OP_DIV, node->line); break;
    // case TK_DOUBLE_SLASH:  emit_byte(chunk, OP_DIV, node->line); break; // TODO: convert integers to floats
    case TK_PERCENT:       emit_byte(chunk, OP_MOD, node->line); break;
    case TK_EQUAL_EQUAL:   emit_byte(chunk, OP_EQ, node->line); break;
    case TK_BANG_EQUAL:    emit_byte(chunk, OP_NEQ, node->line); break;
    case TK_GT:            emit_byte(chunk, OP_GT, node->line); break;
    case TK_GTE:           emit_byte(chunk, OP_GTE, node->line); break;
    case TK_LT:            emit_byte(chunk, OP_LT, node->line); break;
    case TK_LTE:           emit_byte(chunk, OP_LTE, node->line); break;
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
  if (IS_NIL(exp->value)) {
    emit_byte(chunk, OP_NIL, node->line);
  } else if (IS_BOOL(exp->value)) {
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
  code_gen(chunk, stmt->expr);
  emit_byte(chunk, OP_PRINT, node->line);
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
  code_gen(chunk, expr->expr);
}

static void begin_scope(Chunk_* chunk, AstBlock_* block) {
  emit_byte(chunk, OP_BEGIN_SCOPE, block->base.line);
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
    emit_bytes(chunk, OP_DESTROY_VAR, var->var.frame_index, block->base.line);
  }
  emit_byte(chunk, OP_END_SCOPE, block->base.line);
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
    code_gen(chunk, stmt->expr);
  } else {
    emit_constant(chunk, NIL_VAL, node->line);
  }
  emit_return(chunk, node->line);
}

static void patch_jmp(Chunk_* chunk, int offset) {
  // -2 to adjust for the bytecode for the jump offset itself.
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
  code_gen(chunk, stmt->condition_expr);

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
  while(elif_e != NULL && elif_s != NULL) {
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
  code_gen(chunk, stmt->expr);
  emit_byte(chunk, OP_ASSERT, node->line);
}

static int resolve_local(Scope_* scope, Token_* name) {
  VarSymbol_* var = scope_var(scope, name);
  assertf(var, "Could not find var %.*s", name->length, name->start);
  if (!var) return -1;

  return var->frame_index;
}

static void var_decl_code_gen(Chunk_* chunk, AstNode_* node) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)node;
  int slot = resolve_local(stmt->base.scope, &stmt->name);

  // TODO: allow for multiple expressions.
  code_gen(chunk, (AstNode_*)stmt->expr);
  if (type_iscoercible(stmt->expr->type, stmt->type)) {
    emit_byte(chunk, OP_CAST, node->line);
    emit_long(chunk, type_toint(stmt->type), node->line);
  }
  emit_bytes(chunk, OP_SET_VAR, (uint8_t)slot, node->line);
}

static void var_expr_code_gen(Chunk_* chunk, AstNode_* node) {
  AstVarExpr_* expr = (AstVarExpr_*)node;
  code_gen(chunk, (AstNode_*)expr->expr);
}

static void id_expr_code_gen(Chunk_* chunk, AstNode_* node) {
  AstIdExpr_* expr = (AstIdExpr_*)node;
  int slot = resolve_local(node->scope, &expr->name);

  emit_bytes(chunk, OP_GET_VAR, (uint8_t)slot, node->line);
}

static void assignment_expr_code_gen(Chunk_* chunk, AstNode_* node) {
  AstAssignmentExpr_* expr = (AstAssignmentExpr_*)node;

  // TODO: allow for assigning to more than simple variables
  // TODO: allow for assigning to multiple variables.
  AstVarExpr_* var = (AstVarExpr_*)expr->left;
  assertf(var->expr->base.cls == AST_CLS(AstIdExpr_),
    "Only assigning to variables is supported at this time.");
  
  AstIdExpr_* id_expr = (AstIdExpr_*)var->expr;
  int slot = resolve_local(node->scope, &id_expr->name);
  
  code_gen(chunk, expr->right);
  emit_bytes(chunk, OP_DESTROY_VAR, (uint8_t)slot, node->line);
  emit_bytes(chunk, OP_SET_VAR, (uint8_t)slot, node->line);
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
  code_gen(chunk, stmt->condition_expr);

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

void expression_statement_code_gen(Chunk_* chunk, AstNode_* node) {
  AstExpressionStmt_* stmt = (AstExpressionStmt_*)node;
  code_gen(chunk, stmt->expr);
  emit_byte(chunk, OP_POP, node->line);
}

void function_def_code_gen(Chunk_* chunk, AstNode_* node) {
  AstFunctionDef_* def = (AstFunctionDef_*)node;
  code_gen(chunk, (AstNode_*)def->body);
}

void function_body_code_gen(Chunk_* chunk, AstNode_* node) {
  AstFunctionBody_* body = (AstFunctionBody_*)node;
  code_gen(chunk, body->stmt);

  for (AstListNode_* n = body->function_params.head; n != NULL; n = n->next) {
    code_gen(chunk, n->node);
  }
}

void noop_code_gen(Chunk_* chunk, AstNode_* node) {}

void clean_up_temps_code_gen(Chunk_* chunk, AstNode_* node) {
  return;
  AstCleanUpTemps_* temps = (AstCleanUpTemps_*)node;
  
  for (ListNode_* n = temps->tmps.head; n != NULL; n = n->next) {
    Symbol_* tmp = list_val(n, Symbol_*);
    emit_bytes(chunk, OP_DESTROY_VAR, symboltmp_index(tmp), node->line);
  }
}

void ast_tmp_decl_code_gen(Chunk_* chunk, AstNode_* n) {
  AstTmpDecl_* decl = (AstTmpDecl_*)n;
  code_gen(chunk, (AstNode_*)decl->expr);
  if (decl->base.type.ty == VAL_OBJ) {
    // OP_SET_VAR doesn't pop the value from the stack, so there is no need to
    // have an OP_GET_VAR after.
    emit_bytes(chunk, OP_SET_VAR, symboltmp_index(decl->tmp), n->line);
  }
}

CodeGenRule_ code_gen_rules[] = {
  [AST_CLS(AstProgram_)]        = {program_code_gen},
  [AST_CLS(AstBlock_)]          = {block_code_gen},
  [AST_CLS(AstStmt_)]           = {stmt_code_gen},
  [AST_CLS(AstExpr_)]           = {expr_code_gen},
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
  [AST_CLS(AstAssignmentExpr_)] = {assignment_expr_code_gen},
  [AST_CLS(AstWhileStmt_)]      = {while_stmt_code_gen},
  [AST_CLS(AstFunctionDef_)]    = {function_def_code_gen},
  [AST_CLS(AstFunctionBody_)]   = {function_body_code_gen},
  [AST_CLS(AstFunctionParam_)]  = {NULL},
  [AST_CLS(AstFunctionCall_)]   = {NULL},
  [AST_CLS(AstFunctionArgs_)]   = {NULL},
  [AST_CLS(AstExpressionStmt_)] = {expression_statement_code_gen},
  [AST_CLS(AstNoopStmt_)]       = {noop_code_gen},
  [AST_CLS(AstCleanUpTemps_)]   = {clean_up_temps_code_gen},
  [AST_CLS(AstTmpDecl_)]        = {ast_tmp_decl_code_gen},
};
// Static assert to make sure that all node types are accounted for.
STATIC_ASSERT(
  sizeof(code_gen_rules) / sizeof(CodeGenRule_) == __AST_NODE_COUNT__,
  CHECK_CODE_GEN_COUNT);

static CodeGenRule_* get_rule(int type) {
  CodeGenRule_* ret = &code_gen_rules[type];
  assertf(ret->code_gen, "Could not find code gen rule for AST class: %d", type);
  return ret;
}