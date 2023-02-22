#include "compiler.h"

#include "common.h"
#include "object.h"
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
static CodeGenRule_* get_rule(int info);

static void emit_return(Chunk_* chunk, int line);
static void emit_cast(Chunk_* chunk, RuntimeType_ from, RuntimeType_ to, int line);
static int resolve_var(Scope_* scope, AstNode_* expr);

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

static void emit_cast(Chunk_* chunk, RuntimeType_ from, RuntimeType_ to, int line) {
  emit_byte(chunk, OP_CAST, line);
  emit_long(chunk, type_toint(from), line);
  emit_long(chunk, type_toint(to), line);
}

static void unary_code_gen(Chunk_* chunk, AstNode_* node) {
  AstUnaryExp_* exp = (AstUnaryExp_*)node;

  // If getting the address of a symbol...
  if (exp->op == TK_AMPERSAND) {
    Symbol_* sym = exp->base.sem_type.sym;
    if (!sym) {
      sym = exp->base.sem_type.info.sym;
    }

    ValueKind kind = KIND_UNKNOWN;
    switch (sym->type) {
      case SYMBOL_TYPE_FIELD:
        kind = sym->field.sem_type.info.kind;
        break;
      case SYMBOL_TYPE_VAR:
        kind = sym->var.sem_type.info.kind;
        break;

      default:
        assertf(false, "Unimplemented address of for symbol type '%d'", sym->type);
        break;
    }

    int index = resolve_var(node->scope, (AstNode_*)exp->expr);
    switch (kind) {
      // If the symbol lives on the stack or referencing a static, create a weak reference to it.
      case KIND_VAL:
        emit_bytes(chunk, OP_ADDROF_VAR, (uint8_t)index, node->line);
        emit_byte(chunk, OP_MAKE_REF, node->line);
        break;

        // If the symbol is a strong reference, increment the count and get it.
      case KIND_REF:
      {
        emit_bytes(chunk, OP_GET_VAR, (uint8_t)index, node->line);
        if (sym->var.sem_type.info.ref_kind == REF_KIND_STRONG) {
          // TODO: increment reference count.
        }
        break;
      }

      default:
        assertf(false, "Unimplemented address of for symbol kind '%d'", kind);
        break;
    }
  } else if (exp->op == TK_NEW) {
    code_gen(chunk, (AstNode_*)exp->expr);
    emit_byte(chunk, OP_NEW_VAR, node->line);
    int64_t size = exp->expr->sem_type.info.size;
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

static void emit_add(Chunk_* chunk, RuntimeType_ l, RuntimeType_ r, int line) {
  if (l.ty != r.ty) {
    emit_cast(chunk, r, l, line);
  }

  if (ISA_TY_UINT(l) || ISA_TY_INT(l)) {
    emit_byte(chunk, OP_ADD, line);
  } else if (ISA_TY_REAL(l)) {
    emit_byte(chunk, OP_FADD, line);
  } else if (IS_TY_OBJ(l, OBJ_TYPE_STRING)) {
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
        if (var->var.sem_type.info.val == VAL_OBJ && var->var.sem_type.info.kind == KIND_VAL) {
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
      ClassSymbol_* cls_sym = &dot_expr->prefix->sem_type.info.sym->cls;
      int index = symbol_findmember_index(dot_expr->prefix->sem_type.info.sym, dot_expr->id);
      return resolve_var(scope, (AstNode_*)dot_expr->prefix) + index;
    }

    default:
      assertf(false, "resolve_var unimplemented for AST_CLS(%d)", AS_EXPR(expr)->base.cls);
  }
}

static int resolve_var_code_gen(Scope_* scope, AstNode_* expr) {
  switch (((AstNode_*)expr)->cls) {
    case AST_CLS(AstVarExpr_):
    {
      AstVarExpr_* var_expr = AST_CAST(AstVarExpr_, expr);
      return resolve_var_code_gen(scope, (AstNode_*)var_expr->expr);
    }

    case AST_CLS(AstIdExpr_):
    {
      AstIdExpr_* id_expr = AST_CAST(AstIdExpr_, expr);
      return resolve_local(scope, &id_expr->name);
    }

    case AST_CLS(AstDotExpr_):
    {
      AstDotExpr_* dot_expr = AST_CAST(AstDotExpr_, expr);
      ClassSymbol_* cls_sym = &dot_expr->prefix->sem_type.info.sym->cls;
      int index = symbol_findmember_index(dot_expr->prefix->sem_type.info.sym, dot_expr->id);
      return resolve_var_code_gen(scope, (AstNode_*)dot_expr->prefix) + index;
    }

    default:
      assertf(false, "resolve_var unimplemented for AST_CLS(%d)", AS_EXPR(expr)->base.cls);
  }
}

static void var_decl_code_gen(Chunk_* chunk, AstNode_* node) {
  AstVarDeclStmt_* stmt = (AstVarDeclStmt_*)node;
  int slot = resolve_local(stmt->base.scope, &stmt->name);

  SemanticType_ sem_type = stmt->sem_type;

  // TODO: allow for multiple expressions.
  if (stmt->expr) {
    code_gen(chunk, (AstNode_*)stmt->expr);
    if (stmt->expr->sem_type.info.val != stmt->sem_type.info.val &&
        semantictype_iscoercible(stmt->expr->sem_type, stmt->sem_type)) {
      emit_cast(chunk, semantictype_toruntime(stmt->expr->sem_type), semantictype_toruntime(stmt->sem_type), node->line);
    }

    if (sem_type.info.kind == KIND_REF) {
      emit_byte(chunk, OP_INC_REF, node->line);
    }

    //emit_bytes(chunk, OP_SET_VAR, (uint8_t)slot, node->line);

    if (sem_type.info.val == VAL_CLASS) {
      if (sem_type.info.kind == KIND_VAL) {
        for (int64_t i = sem_type.info.size - 1; i >= 0; --i) {
          emit_bytes(chunk, OP_SET_VAR, (uint8_t)slot + (uint8_t)i, node->line);
          emit_byte(chunk, OP_POP, node->line);
        }
      } else {
        emit_bytes(chunk, OP_SET_VAR, (uint8_t)slot, node->line);
      }
    } else {
      emit_bytes(chunk, OP_SET_VAR, (uint8_t)slot, node->line);
    }
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
      if (expr->base.top_sem_type.info.kind == KIND_VAL && var->sem_type.info.kind == KIND_REF) {
        emit_bytes(chunk, OP_GET_REF, (uint8_t)symbolvar_index(sym), node->line);
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

  if (sem_type.info.val == VAL_OBJ && sem_type.info.kind == KIND_VAL) {
    emit_bytes(chunk, OP_DESTROY_VAR, (uint8_t)slot, node->line);
  }

  if (sem_type.info.kind == KIND_REF) {
    emit_bytes(chunk, OP_SET_REF, (uint8_t)slot, node->line);
  } else {
    emit_bytes(chunk, OP_SET_VAR, (uint8_t)slot, node->line);
  }
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

void expression_statement_code_gen(Chunk_* chunk, AstNode_* node) {
  AstExpressionStmt_* stmt = (AstExpressionStmt_*)node;
  code_gen(chunk, (AstNode_*)stmt->expr);
  emit_byte(chunk, OP_POP, node->line);
}

void function_def_code_gen(Chunk_* chunk, AstNode_* node) {
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

void function_body_code_gen(Chunk_* chunk, AstNode_* node) {
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

void function_call_code_gen(Chunk_* chunk, AstNode_* node) {
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

void function_args_code_gen(Chunk_* chunk, AstNode_* node) {
  AstFunctionCallArgs_* args = (AstFunctionCallArgs_*)node;
  for (AstListNode_* n = args->args.head; n != NULL; n = n->next) {
    code_gen(chunk, n->node);
  }
}

void function_arg_code_gen(Chunk_* chunk, AstNode_* node) {
  AstFunctionCallArg_* arg = (AstFunctionCallArg_*)node;
  code_gen(chunk, (AstNode_*)arg->expr);
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

void tmp_decl_code_gen(Chunk_* chunk, AstNode_* n) {
  AstTmpDecl_* decl = (AstTmpDecl_*)n;
  code_gen(chunk, (AstNode_*)decl->expr);
  if (semantictype_isaobj(decl->base.sem_type)) {
    // OP_SET_VAR doesn't pop the value from the stack, so there is no need to
    // have an OP_GET_VAR after.
    emit_bytes(chunk, OP_SET_VAR, symboltmp_index(decl->tmp), n->line);
  }
}

void dot_expr_code_gen(Chunk_* chunk, AstNode_* node) {
  AstDotExpr_* expr = AST_CAST(AstDotExpr_, node);
  ClassSymbol_* cls_sym = NULL;
  const SemanticType_* type = &expr->prefix->sem_type;
  if (type->info.sym->type == SYMBOL_TYPE_VAR) {
    cls_sym = &type->info.sym->var.sem_type.info.sym->cls;
  } else if (type->info.sym->type == SYMBOL_TYPE_CLASS) {
    cls_sym = &type->info.sym->cls;
  } else if (type->info.sym->type == SYMBOL_TYPE_FIELD) {
    cls_sym = &type->sym->cls;
  } else {
    assertf(false, "Unexpected symbol type %d", type->info.sym->type);
  }

  int index = resolve_var(node->scope, (AstNode_*)expr->prefix);
  int field_index = symbol_findmember_index(cls_sym->self_type.sym, expr->id);

  assertf(index + field_index < UINT8_MAX, "Trying to index struct out of bounds.");

  if (expr->prefix->sem_type.info.kind == KIND_REF) {
    emit_bytes(chunk, OP_ADDROF_REF, (uint8_t)index, node->line);
  } else {
    emit_bytes(chunk, OP_ADDROF_VAR, (uint8_t)index, node->line);
  }

  if (expr->base.top_sem_type.info.kind == KIND_VAL) {
    emit_bytes(chunk, OP_GET_OFFSET, (uint8_t)field_index, node->line);
  } else {
    emit_bytes(chunk, OP_ADD_OFFSET, (uint8_t)field_index, node->line);
  }
}

void ast_class_constructor_code_gen(Chunk_* chunk, AstNode_* node) {
  AstConstructor_* constructor = (AstConstructor_*)node;

  SemanticType_* type = &constructor->base.sem_type;
  if (type->name.start) {
    Symbol_* cls_sym = scope_find(node->scope, &type->name);
    AstListNode_* current_field_node = constructor->fields.head;
    for (ListNode_* member_node = cls_sym->cls.members.head; member_node != NULL; member_node = member_node->next) {
      Symbol_* member = list_val(member_node, Symbol_*);
      FieldSymbol_* field = &member->field;
      bool found_value = false;

      for (AstListNode_* field_node = current_field_node; field_node != NULL; field_node = field_node->next) {
        AstConstructorField_* constructor_field = AST_CAST(AstConstructorField_, field_node->node);
        if (!constructor_field->name.start || token_eq(constructor_field->name, member->name)) {
          code_gen(chunk, field_node->node);
          found_value = true;
          current_field_node = field_node->next;
          break;
        }
      }

      if (!found_value) {
        emit_constant(chunk, field->val, node->line);
      }
    }
  } else {
    for (AstListNode_* field_node = constructor->fields.head; field_node != NULL; field_node = field_node->next) {
      AstConstructorField_* constructor_field = AST_CAST(AstConstructorField_, field_node->node);
      code_gen(chunk, field_node->node);
    }
  }
}

void ast_class_constructor_field_code_gen(Chunk_* chunk, AstNode_* node) {
  AstConstructorField_* field = (AstConstructorField_*)node;
  code_gen(chunk, (AstNode_*)field->expr);
}

CodeGenRule_ code_gen_rules[] = {
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
  [AST_CLS(AstConstructor_)]            = {ast_class_constructor_code_gen},
  [AST_CLS(AstConstructorField_)]       = {ast_class_constructor_field_code_gen},
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