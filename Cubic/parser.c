#include "parser.h"
#include "memory.h"
#include "symbol_table.h"
#include "object.h"

#include <memory.h>
#include <string.h>

#pragma warning(3 : 4062)

typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_FUNCTION,    // function
  PREC_OR,          // or
  PREC_XOR,         // xor
  PREC_AND,         // and  
  PREC_EQUALITY,    // == !=
  PREC_BITWISE_OR,  // |
  PREC_BITWISE_XOR, // ^
  PREC_BITWISE_AND, // &
  PREC_SHIFT,       // << >>
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * / // %
  PREC_UNARY,       // * & not - ~
  PREC_CALL,        // . () []
  PREC_PRIMARY,     // 'nil', 'true', 'false', number, string, 
} Precedence;

typedef AstNode_* (*ParseFn)(Parser_*, Scanner_*, Scope_*);

typedef struct ParseRule_ {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule_;

static AstNode_* Number(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* UnaryOp(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* BinaryOp(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* grouping(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* Expr(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* parse_precedence(Parser_* parser, Scanner_* scanner, Precedence precedence, Scope_* scope);
static AstNode_* Statement(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* Block(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* Var(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* FunctionDef(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* ReturnStatement(Parser_* parser, Scanner_* scanner, Scope_* scope);

static void advance(Parser_* parser, Scanner_* scanner);
static void consume(Parser_* parser, Scanner_* scanner, TokenType type, const char* message);
static bool match(Parser_* parser, Scanner_* scanner, TokenType type);
static bool check(Parser_* parser, TokenType type);

static ParseRule_* get_rule(TokenType type);

static void error_at_current(Parser_* parser, const char* message);
static void error(Parser_* parser, const char* message);
static void error_at(Parser_* parser, Token_* token, const char* message);
static bool block_follow(Parser_* parser, Scanner_* scanner, bool withuntil);
static void statement_list(Parser_* parser, Scanner_* scanner, AstList_* statements, Scope_* scope);

void parser_init(Parser_* parser) {
  memset(parser, 0, sizeof(Parser_));
  pageallocator_init(&parser->allocator, 1LL << 14);
}

void parser_clear(Parser_* parser) {
  pageallocator_deinit(&parser->allocator);
}

static AstList_* astlist_create(struct MemoryAllocator_* allocator) {
  AstList_* ret = alloc(allocator, sizeof(AstList_));
  memset(ret, 0, sizeof(AstList_));
  ret->allocator = allocator;
  return ret;
}

static void astlist_init(AstList_* list, struct MemoryAllocator_* allocator) {
  memset(list, 0, sizeof(AstList_));
  list->allocator = allocator;
}

static void astlist_clear(AstList_* list) {
  struct MemoryAllocator_* allocator = list->allocator;

  AstListNode_* cur = list->head;
  while (cur) {
    AstListNode_* next = cur->next;
    dealloc(allocator, cur);
    cur = next;
  }

  list->count = 0;
}

static void astlist_destroy(AstList_** list) {
  struct MemoryAllocator_* allocator = (*list)->allocator;

  astlist_clear(*list);
  dealloc(allocator, *list);
  *list = NULL;
}

static void astlist_append(AstList_* list, AstNode_* node) {
  if (!node) return;

  struct MemoryAllocator_* allocator = list->allocator;
  AstListNode_* n = alloc(allocator, sizeof(AstListNode_));
  n->node = node;
  
  if (!list->head) {
    list->head = n;
  }

  if (list->tail) {
    list->tail->next = n;    
  }

  list->tail = n;
  ++list->count;
}

// Returns true if the current token is in the follow set of a block.
// 'until' closes syntactical blocks, but do not close scope,
// so it is handled in separate. This is used, for example, when determining if
// a function ends with a simple `return` or with a `return expr`.
static bool block_follow(Parser_* parser, Scanner_* scanner, bool withuntil) {
  switch (parser->current.type) {
    case TK_ELSE:
    case TK_ELIF:
    case TK_END:
    case TK_EOF:
    case TK_CASE:
      return true;

    case TK_UNTIL:
      return withuntil;

    default:
      return false;
  }
}

AstNode_* parse(Parser_* parser, Scanner_* scanner, struct Scope_* root_scope, const char* source) {
  advance(parser, scanner);  
  AstProgram_* root = MAKE_AST_NODE(&parser->allocator, AstProgram_, root_scope);

  root->block = (AstBlock_*)Block(parser, scanner, root->base.scope);

  consume(parser, scanner, TK_EOF, "Expected end of file.");
  return (AstNode_*)root;
}

Token_ parse_variable(Parser_* parser, Scanner_* scanner, Token token) {
  Token_ ret = *token;
  ret.start = alloc(&parser->allocator, ret.length);
  memcpy((char*)ret.start, token->start, token->length);

  return ret;
}

Type_ parse_type(Token_ token) {
  Type_ ret = {
    .ty   = VAL_UNKNOWN,
    .kind = KIND_VAL,
    .obj  = OBJ_TYPE_UNKNOWN
  };

  switch (token.type) {
    case TK_BOOL:        ret.ty = VAL_BOOL; break;
    case TK_INT:         ret.ty = VAL_INT; break;
    case TK_UINT:        ret.ty = VAL_UINT; break;
    case TK_FLOAT:       ret.ty = VAL_FLOAT; break;
    case TK_DOUBLE:      ret.ty = VAL_DOUBLE; break;
    case TK_STRING_TYPE:
      ret.ty = VAL_OBJ;
      ret.obj = OBJ_TYPE_STRING;
    default: break;
  }

  return ret;
}

// Escape edge of the parser state machine from the error state to the initial state.
void synchronize(Parser_* parser, Scanner_* scanner) {
  parser->panic_mode = false;

  // These cases are all the tokens that can start a statement.
  while (parser->current.type != TK_EOF) {
    switch (parser->current.type) {
      case TK_DO:
      case TK_LET:
      case TK_END:
      case TK_WHILE:
      case TK_ASSERT:
      case TK_RETURN:
      case TK_IF:
      case TK_PRINT:
      case TK_PASS:
      case TK_SEMICOLON:
      case TK_EOF:
      case TK_FUNCTION:
        return;
      default:
        ;
    }

    advance(parser, scanner);
  }
}

static void statement_list(Parser_* parser, Scanner_* scanner, AstList_* statements, Scope_* scope) {
  // TODO: how to handle the 'pass' keyword? Should it stop parsing statement lists?
  while (!block_follow(parser, scanner, true)) {
    
    AstNode_* statement = Statement(parser, scanner, scope);
    astlist_append(statements, statement);

    if (parser->panic_mode) {
      synchronize(parser, scanner);
    }

    if (parser->previous.type == TK_RETURN) {
      return;
    }
  }
}

static void enter_scope(Scope_* scope) {
  Frame_* frame = scope->frame;
  frame_enterscope(frame, scope);
}

static void leave_scope(Scope_* scope) {
  Frame_* frame = scope->frame;
  frame_leavescope(frame, scope);
}

static AstNode_* Block(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstBlock_* block = MAKE_AST_NODE(&parser->allocator, AstBlock_, scope_createfrom(scope));

  enter_scope(block->base.scope);
  astlist_init(&block->statements, (MemoryAllocator_*)&parser->allocator);
  statement_list(parser, scanner, &block->statements, block->base.scope);
  leave_scope(block->base.scope);

  return (AstNode_*)block;
}

static AstNode_* BlockStatement(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstNode_* block = Block(parser, scanner, scope);
  consume(parser, scanner, TK_END, "Expected 'end' at the end of a block.");
  return block;
}

static bool in_assign_first(TokenType type) {
  return
    type == TK_ID ||
    type == TK_LPAREN;
}

static void VarList(Parser_* parser, Scanner_* scanner, AstList_* vars, Scope_* scope) {
  TokenType tk = parser->current.type;  
  do {
    astlist_append(vars, Var(parser, scanner, scope));
  } while (match(parser, scanner, TK_COMMA));
}

static void ExprList(Parser_* parser, Scanner_* scanner, AstList_* exprs, Scope_* scope) {
  TokenType tk = parser->current.type;
  do {
    astlist_append(exprs, Expr(parser, scanner, scope));
  } while (match(parser, scanner, TK_COMMA));
}

static AstNode_* FunctionParam(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstFunctionParam_* param = MAKE_AST_NODE(&parser->allocator, AstFunctionParam_, scope);

  consume(parser, scanner, TK_ID, "Expecting parameter name");
  param->name = parse_variable(parser, scanner, &parser->previous);

  param->type = UNKNOWN_TY;
  if (match(parser, scanner, TK_COLON)) {
    param->type = parse_type(parser->current);
    advance(parser, scanner);
  }

  if (match(parser, scanner, TK_EQUAL)) {
    param->opt_expr = Expr(parser, scanner, scope);
  }

  frame_addparam(scope->frame, &param->name);

  return (AstNode_*)param;
}

static AstNode_* FunctionBody(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstFunctionBody_* body = MAKE_AST_NODE(&parser->allocator, AstFunctionBody_, scope);
  astlist_init(&body->function_params, (MemoryAllocator_*)&parser->allocator);  

  consume(parser, scanner, TK_LPAREN, "Expected a '(' for the start of a function's parameter list.");
  if (!match(parser, scanner, TK_RPAREN)) {
    do {
      if (match(parser, scanner, TK_TRIPLE_DOT)) {
        // TODO: implement variadic args
        error(parser, "Variadic args are unimplemented.");
        break;
      }

      astlist_append(&body->function_params, FunctionParam(parser, scanner, scope));
    } while (match(parser, scanner, TK_COMMA));
    consume(parser, scanner, TK_RPAREN, "Expected ')' after arguments.");
  }  

  if (match(parser, scanner, TK_ARROW)) {
    body->return_type = parse_type(parser->current);
    advance(parser, scanner);
  } else {
    body->return_type = NIL_TY;
  }

  if (match(parser, scanner, TK_END)) {
    body->stmt = MAKE_AST_NOOP(&parser->allocator);
  } else {
    body->stmt = BlockStatement(parser, scanner, scope);
  }
  return (AstNode_*)body;
}

static AstNode_* FunctionDef(Parser_* parser, Scanner_* scanner, Scope_* scope) {  
  MemoryAllocator_* allocator = (MemoryAllocator_*)&parser->allocator;
  AstFunctionDef_* def = MAKE_AST_NODE(allocator, AstFunctionDef_, scope);

  // TODO: implement lambdas
  Token_ name;
  if (match(parser, scanner, TK_ID)) {    
    name = parse_variable(parser, scanner, &parser->previous);
  } else {
    name = (Token_) { 0 };
  }

  Symbol_* fn_symbol = scope_addfn(scope, &name);
  Frame_* fn_frame = frame_create(fn_symbol, allocator);
  Scope_* fn_scope = fn_frame->scope;

  def->fn_symbol = fn_symbol;
  def->body = (AstFunctionBody_*)FunctionBody(parser, scanner, fn_scope);

  return (AstNode_*)def;
}

static AstNode_* ExpressionStmt(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstExpressionStmt_* stmt = MAKE_AST_STMT(&parser->allocator, AstExpressionStmt_, scope);
  stmt->expr = Expr(parser, scanner, scope);
  return (AstNode_*)stmt;
}

static AstNode_* clean_up_temps(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  Frame_* frame = scope->frame;
  AstCleanUpTemps_* ret = MAKE_AST_NODE(&parser->allocator, AstCleanUpTemps_, scope);
  list_of(&ret->tmps, Symbol_*, (MemoryAllocator_*)&parser->allocator);

  frame_movetemps(scope->frame, &ret->tmps);

  return (AstNode_*)ret;
}

static AstNode_* ReturnStatement(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstReturnStmt_* stmt = MAKE_AST_STMT(&parser->allocator, AstReturnStmt_, scope);
  if (block_follow(parser, scanner, true) || parser->current.type == ';') {
    stmt->expr = MAKE_AST_NOOP(&parser->allocator);
  } else {
    stmt->expr = Expr(parser, scanner, scope);
  }

  return (AstNode_*)stmt;
}

static AstNode_* Statement(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  TokenType tk = parser->current.type;
  MemoryAllocator_* allocator = (MemoryAllocator_*)&parser->allocator;
  AstStmt_* ret = MAKE_AST_NODE(allocator, AstStmt_, scope);
  ret->cleanup = MAKE_AST_NOOP(allocator);

  switch (tk) {
    case TK_DO:
    {
      advance(parser, scanner);
      ret->stmt = BlockStatement(parser, scanner, scope);
      break;
    }

    case TK_LET:
    {
      advance(parser, scanner);
      AstVarDeclStmt_* stmt = MAKE_AST_STMT(&parser->allocator, AstVarDeclStmt_, scope);
      consume(parser, scanner, TK_ID, "Expected variable name.");
      stmt->name = parse_variable(parser, scanner, &parser->previous);
      
      consume(parser, scanner, TK_COLON, "Expected a ':' for a variable declaration.");

      if (check(parser, TK_EQUAL)) {
        stmt->type = UNKNOWN_TY;
      } else {
        stmt->type = parse_type(parser->current);

        // TODO: Add object and custom types.
        assertf(stmt->type.ty != VAL_UNKNOWN, "Unknown type");
        
        advance(parser, scanner);
      }

      if (match(parser, scanner, TK_EQUAL)) {
        stmt->expr = (AstExpr_*)Expr(parser, scanner, scope);
      } else if (IS_TY_UNKNOWN(stmt->type)) {
        error_at_current(parser, "Expected an expression for a deduced type variable.");
      }

      frame_addvar(scope->frame, &stmt->name, scope);

      ret->stmt = (AstNode_*)stmt;
      break;
    }

    case TK_END:
    {
      error_at_current(parser, "Unmatched 'do'");
      ret->stmt = MAKE_AST_NOOP(&parser->allocator);
      break;
    }

    //case TK_FUNCTION:
    //{
    //  advance(parser, scanner);
    //  ret->stmt = FunctionDef(parser, scanner, scope);
    //  break;
    //}

    case TK_WHILE:
    {
      advance(parser, scanner);
      AstWhileStmt_* stmt = MAKE_AST_STMT(&parser->allocator, AstWhileStmt_, scope);
      stmt->condition_expr = Expr(parser, scanner, scope);      

      consume(parser, scanner, TK_DO, "Expected 'do' at end of while expression.");

      stmt->block_stmt = Block(parser, scanner, scope);

      consume(parser, scanner, TK_END, "Expected 'end' at end of while expression.");

      ret->stmt = (AstNode_*)stmt;
      break;
    }

    case TK_ASSERT:
    {
      advance(parser, scanner);
      AstAssertStmt_* stmt = MAKE_AST_STMT(&parser->allocator, AstAssertStmt_, scope);
      stmt->base.line = parser->current.line;
      stmt->expr = Expr(parser, scanner, scope);
      ret->stmt = (AstNode_*)stmt;
      break;
    }

    // TODO: add parsing multiple return values
    case TK_RETURN:
    {
      advance(parser, scanner);
      ret->stmt = ReturnStatement(parser, scanner, scope);
      break;
    }

    case TK_IF:
    {
      advance(parser, scanner);
      AstIfStmt_* stmt = MAKE_AST_STMT(&parser->allocator, AstIfStmt_, scope);

      stmt->condition_expr = Expr(parser, scanner, scope);
      consume(parser, scanner, TK_THEN, "Expected 'then' after condition.");

      stmt->if_stmt = (AstNode_*)Block(parser, scanner, scope);
      astlist_init(&stmt->elif_stmts, (MemoryAllocator_*)&parser->allocator);
      astlist_init(&stmt->elif_exprs, (MemoryAllocator_*)&parser->allocator);

      for(;;) {
        if (!match(parser, scanner, TK_ELIF)) {
          break;
        }
        astlist_append(&stmt->elif_exprs, Expr(parser, scanner, scope));
        consume(parser, scanner, TK_THEN, "Expected 'then' after condition.");
        astlist_append(&stmt->elif_stmts, Block(parser, scanner, scope));
      }

      if (match(parser, scanner, TK_ELSE)) {
        stmt->else_stmt = (AstNode_*)Block(parser, scanner, scope);
      } else {
        stmt->else_stmt = MAKE_AST_NOOP((MemoryAllocator_*)&parser->allocator);
      }

      consume(parser, scanner, TK_END, "Expected 'end' after if-statement.");

      ret->stmt = (AstNode_*)stmt;
      break;
    }

    case TK_PRINT:
    {
      advance(parser, scanner);
      AstPrintStmt_* stmt = MAKE_AST_STMT(&parser->allocator, AstPrintStmt_, scope);
      stmt->expr = Expr(parser, scanner, scope);
      ret->stmt = (AstNode_*)stmt;
      break;
    }

    case TK_PASS:
    case TK_SEMICOLON:
    case TK_EOF:
      advance(parser, scanner);
      ret->stmt = MAKE_AST_NOOP(allocator);
      break;

    default:
      ret->stmt = ExpressionStmt(parser, scanner, scope);
      break;
  }
  
  ret->cleanup = clean_up_temps(parser, scanner, scope);
  return (AstNode_*)ret;
}

static void advance(Parser_* parser, Scanner_* scanner) {
  parser->previous = parser->current;

  for (;;) {
    parser->current = scanner_scan(scanner);
    if (parser->current.type != TK_ERR) break;

    error_at_current(parser, parser->current.start);
  }
}

static void consume(Parser_* parser, Scanner_* scanner, TokenType type, const char* message) {
  if (parser->current.type == type) {
    advance(parser, scanner);
    return;
  }

  error_at_current(parser, message);
}

static bool match(Parser_* parser, Scanner_* scanner, TokenType type) {
  if (!check(parser, type)) return false;
  advance(parser, scanner);
  return true;
}

static bool check(Parser_* parser, TokenType type) {
  return parser->current.type == type;
}

///////////////////////////////////////////////////////////////////////////////
static AstNode_* Expr(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstExpr_* ret = MAKE_AST_NODE(&parser->allocator, AstExpr_, scope);
  ret->expr = parse_precedence(parser, scanner, PREC_ASSIGNMENT, scope);
  return (AstNode_*)ret;
}

static AstPrintStmt_* Print(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstPrintStmt_* stmt = MAKE_AST_STMT(&parser->allocator, AstPrintStmt_, scope);
  stmt->expr = Expr(parser, scanner, scope);

  return stmt;
}

static AstNode_* Number(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  double value = strtod(parser->previous.start, NULL);
  AstPrimaryExp_* expr = MAKE_AST_EXPR(&parser->allocator, AstPrimaryExp_, scope);
  expr->base.type = DOUBLE_TY;
  expr->value = DOUBLE_VAL(value);  
  return (AstNode_*)expr;
}

static AstNode_* Integer(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  int64_t value = strtoll(parser->previous.start, NULL, 10);  
  AstPrimaryExp_* expr = MAKE_AST_EXPR(&parser->allocator, AstPrimaryExp_, scope);
  expr->base.type = INT_TY;
  expr->value = INT_VAL(value);
  return (AstNode_*)expr;
}

static AstNode_* Nil(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstPrimaryExp_* expr = MAKE_AST_EXPR(&parser->allocator, AstPrimaryExp_, scope);
  expr->base.type = NIL_TY;
  expr->value = NIL_VAL;
  return (AstNode_*)expr;
}

static AstNode_* True(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstPrimaryExp_* expr = MAKE_AST_EXPR(&parser->allocator, AstPrimaryExp_, scope);
  expr->base.type = BOOL_TY;
  expr->value = TRUE_VAL;
  return (AstNode_*)expr;
}

static AstNode_* False(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstPrimaryExp_* expr = MAKE_AST_EXPR(&parser->allocator, AstPrimaryExp_, scope);
  expr->base.type = BOOL_TY;
  expr->value = FALSE_VAL;
  return (AstNode_*)expr;
}

static AstNode_* String(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstPrimaryExp_* expr = MAKE_AST_EXPR(&parser->allocator, AstPrimaryExp_, scope);
  expr->base.type = STRING_TY;
  expr->base.type.kind = KIND_STATIC;

  expr->value = OBJ_VAL(objstring_from(parser->previous.start + 1, parser->previous.length - 2));
  expr->value.type.kind = KIND_STATIC;
  return (AstNode_*)expr;
}

static AstNode_* grouping(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstNode_* expr = Expr(parser, scanner, scope);
  consume(parser, scanner, TK_RPAREN, "Expect ')' after expression.");

  return expr;
}

static AstNode_* UnaryOp(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  TokenType operator_type = parser->previous.type;

  AstUnaryExp_* unary = MAKE_AST_EXPR(&parser->allocator, AstUnaryExp_, scope);
  unary->op = operator_type;
  unary->expr = parse_precedence(parser, scanner, PREC_UNARY, scope);
  unary->base.type = UNKNOWN_TY;

  return (AstNode_*)unary;
}

static AstNode_* BinaryOp(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  TokenType operator_type = parser->previous.type;
  ParseRule_* rule = get_rule(operator_type);  
  AstNode_* expr = parse_precedence(parser, scanner, (Precedence)(rule->precedence + 1), scope);
  return expr;
}

static AstNode_* Var(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstVarExpr_* var_expr = MAKE_AST_EXPR(&parser->allocator, AstVarExpr_, scope);
  AstIdExpr_* expr = MAKE_AST_EXPR(&parser->allocator, AstIdExpr_, scope);
  var_expr->expr = (AstExpr_*)expr;
  expr->name = parse_variable(parser, scanner, &parser->previous);
  return (AstNode_*)var_expr;
}

static AstNode_* FunctionArgs(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstFunctionArgs_* args = MAKE_AST_NODE(&parser->allocator, AstFunctionArgs_, scope);
  astlist_init(&args->args, (MemoryAllocator_*)&parser->allocator);

  if (match(parser, scanner, TK_RPAREN)) {
    return (AstNode_*)args;
  }

  do {
    astlist_append(&args->args, Expr(parser, scanner, scope));
  } while (match(parser, scanner, TK_COMMA));

  consume(parser, scanner, TK_RPAREN, "Expected a ')' after a function call.");
  return (AstNode_*)args;
}

static AstNode_* parse_precedence(Parser_* parser, Scanner_* scanner, Precedence precedence, Scope_* scope) {
  advance(parser, scanner);
  ParseFn prefix_rule = get_rule(parser->previous.type)->prefix;
  if (!prefix_rule) {
    error(parser, "Expected expression");
    return NULL;
  }

  AstNode_* exp = prefix_rule(parser, scanner, scope);
  while (precedence <= get_rule(parser->current.type)->precedence) {
    ParseFn infix_rule = get_rule(parser->current.type)->infix;
    if (!infix_rule) {
      break;
    }
    advance(parser, scanner);

    if (parser->previous.type == TK_LPAREN) {
      AstFunctionCall_* new_exp = MAKE_AST_EXPR(&parser->allocator, AstFunctionCall_, scope);
      new_exp->prefix = (AstExpr_*)exp;
      new_exp->args = (AstFunctionArgs_*)infix_rule(parser, scanner, scope);
      exp = (AstNode_*)new_exp;
    } else if (parser->previous.type == TK_EQUAL) {
      AstAssignmentExpr_* new_exp = MAKE_AST_EXPR(&parser->allocator, AstAssignmentExpr_, scope);
      new_exp->left = exp;
      new_exp->right = infix_rule(parser, scanner, scope);
      exp = (AstNode_*)new_exp;
    } else {     
      AstBinaryExp_* bin_exp = MAKE_AST_EXPR(&parser->allocator, AstBinaryExp_, scope);
      bin_exp->base.type = UNKNOWN_TY;
      bin_exp->base.type.kind = KIND_TMP;
      bin_exp->op = parser->previous.type;
      bin_exp->left = exp;
      bin_exp->right = infix_rule(parser, scanner, scope);

      AstTmpDecl_* tmp_decl = MAKE_AST_EXPR(&parser->allocator, AstTmpDecl_, scope);
      tmp_decl->expr = (AstExpr_*)bin_exp;
      tmp_decl->tmp = frame_addtmp(scope->frame, scope);

      exp = (AstNode_*)tmp_decl;
    }
  }

  return exp;
}

ParseRule_ rules[] = {
  [TK_EOF]          = {NULL,        NULL,         PREC_NONE},
  [TK_ERR]          = {NULL,        NULL,         PREC_NONE},
  [TK_ID]           = {Var,         NULL,         PREC_NONE},
  [TK_LPAREN]       = {grouping,    FunctionArgs, PREC_CALL},
  [TK_RPAREN]       = {NULL,        NULL,         PREC_NONE},
  [TK_LBRACKET]     = {NULL,        NULL,         PREC_NONE},
  [TK_RBRACKET]     = {NULL,        NULL,         PREC_NONE},
  [TK_LBRACE]       = {NULL,        NULL,         PREC_NONE},
  [TK_RBRACE]       = {NULL,        NULL,         PREC_NONE},
  [TK_SEMICOLON]    = {NULL,        NULL,         PREC_NONE},
  [TK_COLON]        = {NULL,        NULL,         PREC_NONE},
  [TK_DOT]          = {NULL,        NULL,         PREC_NONE},
  [TK_COMMA]        = {NULL,        NULL,         PREC_NONE},
  [TK_PLUS]         = {NULL,        BinaryOp,     PREC_TERM},
  [TK_MINUS]        = {UnaryOp,     BinaryOp,     PREC_TERM},
  [TK_AMPERSAND]    = {NULL,        BinaryOp,     PREC_BITWISE_AND},
  [TK_PIPE]         = {NULL,        BinaryOp,     PREC_BITWISE_OR},
  [TK_HAT]          = {NULL,        BinaryOp,     PREC_BITWISE_XOR},
  [TK_TILDE]        = {UnaryOp,     NULL,         PREC_UNARY},
  [TK_STAR]         = {NULL,        BinaryOp,     PREC_FACTOR},
  [TK_PERCENT]      = {NULL,        BinaryOp,     PREC_FACTOR},
  [TK_BANG]         = {NULL,        NULL,         PREC_NONE},
  [TK_BANG_EQUAL]   = {NULL,        BinaryOp,     PREC_EQUALITY},
  [TK_EQUAL]        = {NULL,        BinaryOp,     PREC_ASSIGNMENT},
  [TK_EQUAL_EQUAL]  = {NULL,        BinaryOp,     PREC_EQUALITY},
  [TK_GT]           = {NULL,        BinaryOp,     PREC_COMPARISON},
  [TK_GTE]          = {NULL,        BinaryOp,     PREC_COMPARISON},
  [TK_RSHIFT]       = {NULL,        BinaryOp,     PREC_SHIFT},
  [TK_LT]           = {NULL,        BinaryOp,     PREC_COMPARISON},
  [TK_LTE]          = {NULL,        BinaryOp,     PREC_COMPARISON},
  [TK_LSHIFT]       = {NULL,        BinaryOp,     PREC_SHIFT},
  [TK_ARROW]        = {NULL,        NULL,         PREC_NONE},
  [TK_FAT_ARROW]    = {NULL,        NULL,         PREC_NONE},
  [TK_SLASH]        = {NULL,        BinaryOp,     PREC_FACTOR},
  [TK_DOUBLE_SLASH] = {NULL,        BinaryOp,     PREC_FACTOR},
  [TK_DOUBLE_DOT]   = {NULL,        NULL,         PREC_NONE},
  [TK_TRIPLE_DOT]   = {NULL,        NULL,         PREC_NONE},
  [TK_STRING]       = {String,      NULL,         PREC_NONE},
  [TK_INTEGER]      = {Integer,     NULL,         PREC_NONE},
  [TK_NUMBER]       = {Number,      NULL,         PREC_NONE},
  [TK_DO]           = {NULL,        NULL,         PREC_NONE},
  [TK_END]          = {NULL,        NULL,         PREC_NONE},
  [TK_IF]           = {NULL,        NULL,         PREC_NONE},
  [TK_THEN]         = {NULL,        NULL,         PREC_NONE},
  [TK_ELIF]         = {NULL,        NULL,         PREC_NONE},
  [TK_ELSE]         = {NULL,        NULL,         PREC_NONE},
  [TK_FOR]          = {NULL,        NULL,         PREC_NONE},
  [TK_IN]           = {NULL,        NULL,         PREC_NONE},
  [TK_STEP]         = {NULL,        NULL,         PREC_NONE},
  [TK_WHILE]        = {NULL,        NULL,         PREC_NONE},
  [TK_REPEAT]       = {NULL,        NULL,         PREC_NONE},
  [TK_UNTIL]        = {NULL,        NULL,         PREC_NONE},
  [TK_STRUCT]       = {NULL,        NULL,         PREC_NONE},
  [TK_MATCH]        = {NULL,        NULL,         PREC_NONE},
  [TK_FUNCTION]     = {FunctionDef, NULL,         PREC_FUNCTION},
  [TK_RETURN]       = {NULL,        NULL,         PREC_NONE},
  [TK_AND]          = {NULL,        BinaryOp,     PREC_AND},
  [TK_OR]           = {NULL,        BinaryOp,     PREC_OR},
  [TK_XOR]          = {NULL,        BinaryOp,     PREC_XOR},
  [TK_NOT]          = {UnaryOp,     NULL,         PREC_UNARY},
  [TK_TRUE]         = {True,        NULL,         PREC_NONE},
  [TK_FALSE]        = {False,       NULL,         PREC_NONE},
  [TK_PASS]         = {NULL,        NULL,         PREC_NONE},
  [TK_LET]          = {NULL,        NULL,         PREC_NONE},
  [TK_NIL]          = {Nil,         NULL,         PREC_NONE},
  [TK_BOOL]         = {NULL,        NULL,         PREC_NONE},
  [TK_INT]          = {NULL,        NULL,         PREC_NONE},
  [TK_UINT]         = {NULL,        NULL,         PREC_NONE},
  [TK_FLOAT]        = {NULL,        NULL,         PREC_NONE},
  [TK_DOUBLE]       = {NULL,        NULL,         PREC_NONE},
  [TK_STRING_TYPE]  = {NULL,        NULL,         PREC_NONE},
  [TK_LIST]         = {NULL,        NULL,         PREC_NONE},
  [TK_MAP]          = {NULL,        NULL,         PREC_NONE},
  [TK_SET]          = {NULL,        NULL,         PREC_NONE},
  [TK_ASYNC]        = {NULL,        NULL,         PREC_NONE},
  [TK_AWAIT]        = {NULL,        NULL,         PREC_NONE},
  [TK_YIELD]        = {NULL,        NULL,         PREC_NONE},
};
// Static assert to make sure that all token types are accounted for.
STATIC_ASSERT(
  sizeof(rules) / sizeof(ParseRule_) == __TK_COUNT__,
  CHECK_TOKEN_COUNT);

static ParseRule_* get_rule(TokenType type) {
  return &rules[type];
}

///////////////////////////////////////////////////////////////////////////////

static void error_at_current(Parser_* parser, const char* message) {
  error_at(parser, &parser->current, message);
}

static void error(Parser_* parser, const char* message) {
  error_at(parser, &parser->previous, message);
}

static void error_at(Parser_* parser, Token_* token, const char* message) {
  if (parser->panic_mode) return;
  parser->panic_mode = true;
  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TK_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TK_ERR) {
    // Nothing.
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser->had_error = true;
}