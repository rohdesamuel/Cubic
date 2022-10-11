#include "parser.h"
#include "memory.h"

#include <memory.h>
#include <string.h>

#pragma warning(3 : 4062)

typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
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
  PREC_PRIMARY      // 'nil', 'true', 'false', number, string, 
} Precedence;

typedef AstNode_* (*ParseFn)(Parser_*, Scanner_*, int);

typedef struct ParseRule_ {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule_;

static AstNode_* Number(Parser_* parser, Scanner_* scanner, int line);
static AstNode_* UnaryOp(Parser_* parser, Scanner_* scanner, int line);
static AstNode_* BinaryOp(Parser_* parser, Scanner_* scanner, int line);
static AstNode_* grouping(Parser_* parser, Scanner_* scanner, int line);
static AstNode_* Expr(Parser_* parser, Scanner_* scanner, int line);
static AstNode_* parse_precedence(Parser_* parser, Scanner_* scanner, Precedence precedence);
static AstNode_* Statement(Parser_* parser, Scanner_* scanner, int line);
static AstNode_* Block(Parser_* parser, Scanner_* scanner, int line);

static void advance(Parser_* parser, Scanner_* scanner);
static void consume(Parser_* parser, Scanner_* scanner, TokenType type, const char* message);
static bool match(Parser_* parser, Scanner_* scanner, TokenType type);
static bool check(Parser_* parser, TokenType type);

static ParseRule_* get_rule(TokenType type);

static void error_at_current(Parser_* parser, const char* message);
static void error(Parser_* parser, const char* message);
static void error_at(Parser_* parser, Token_* token, const char* message);
static bool block_follow(Parser_* parser, Scanner_* scanner, bool withuntil);
static void statement_list(Parser_* parser, Scanner_* scanner, AstList_* statements);

void parser_init(Parser_* parser) {
  memset(parser, 0, sizeof(Parser_));
  pageallocator_init(&parser->allocator, 1LL << 14);
}

void parser_clear(Parser_* parser) {
  pageallocator_deinit(&parser->allocator);
}

// Returns true if the current token is in the follow set of a block.
// 'until' closes syntactical blocks, but do not close scope,
// so it is handled in separate.
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

AstNode_* parse(Parser_* parser, Scanner_* scanner, const char* source) {
  advance(parser, scanner);
  AstProgram_* root = MAKE_AST_NODE(&parser->allocator, AstProgram_);

  root->block = (AstBlock_*)Block(parser, scanner, parser->current.line);

  consume(parser, scanner, TK_EOF, "Expected end of file.");
  return (AstNode_*)root;
}

char* parse_variable(Parser_* parser, Scanner_* scanner, Token token) {
  char* ret;
  ret = alloc(&parser->allocator, token->length + 1);
  memcpy(ret, token->start, token->length);
  ret[token->length] = '\0';

  return ret;
}

static void statement_list(Parser_* parser, Scanner_* scanner, AstList_* statements) {
  // TODO: how to handle the 'pass' keyword? Should it stop parsing statement lists?
  while (!block_follow(parser, scanner, true)) {
    if (statements->count >= statements->capacity) {
      error(parser, "Too many statements in block");
      continue;
    }
    AstNode_* statement = Statement(parser, scanner, parser->current.line);
    if (statement) {
      statements->list[statements->count] = statement;
      statements->count += 1;
    }

    if (parser->previous.type == TK_RETURN) {
      return;
    }
  }
}

static AstNode_* Block(Parser_* parser, Scanner_* scanner, int line) {
  AstBlock_* block = MAKE_AST_NODE(&parser->allocator, AstBlock_);
  block->statements.capacity = 100;  // TODO: remove the 100 restriction.
  block->statements.list = alloc(&parser->allocator, block->statements.capacity * sizeof(AstNode_*));

  statement_list(parser, scanner, &block->statements);

  return (AstNode_*)block;
}

static AstNode_* BlockStatement(Parser_* parser, Scanner_* scanner, int line) {
  AstNode_* block = Block(parser, scanner, line);
  consume(parser, scanner, TK_END, "Expected 'end' at the end of a block.");
  return block;
}

static AstNode_* Statement(Parser_* parser, Scanner_* scanner, int line) {
  TokenType tk = parser->current.type;
  switch (tk) {
    case TK_DO:
    {
      advance(parser, scanner);
      return BlockStatement(parser, scanner, line);
    }

    case TK_LET:
    {
      advance(parser, scanner);
      AstVarDeclStmt_* stmt = MAKE_AST_NODE(&parser->allocator, AstVarDeclStmt_);
      consume(parser, scanner, TK_ID, "Expected variable name.");
      stmt->id = parse_variable(parser, scanner, &parser->previous);
      
      consume(parser, scanner, TK_COLON, "Expected a ':' for a variable declaration.");

      if (match(parser, scanner, TK_EQUAL)) {
        stmt->type = VAL_UNKNOWN;
        error(parser, "Type inference is unimplemented");
      } else {
        switch (parser->current.type) {
          case TK_BOOL:   stmt->type = VAL_BOOL; break;
          case TK_INT:    stmt->type = VAL_INT; break;
          case TK_UINT:   stmt->type = VAL_UINT; break;
          case TK_FLOAT:  stmt->type = VAL_FLOAT; break;
          case TK_DOUBLE: stmt->type = VAL_DOUBLE; break;
          
          // TODO: Add object and custom types.
          default: assertf(false, "Unknown type");
        }
        advance(parser, scanner);        
      }

      if (match(parser, scanner, TK_EQUAL)) {
        stmt->exprs.capacity = 10;
        stmt->exprs.count = 0;
        stmt->exprs.list = alloc(&parser->allocator, stmt->exprs.capacity * sizeof(AstNode_*));

        for (int i = 0; i < stmt->exprs.capacity; ++i) {
          stmt->exprs.list[stmt->exprs.count] = Expr(parser, scanner, line);
          ++stmt->exprs.count;

          if (!match(parser, scanner, TK_COMMA)) {
            break;
          }
        }
      }

      return (AstNode_*)stmt;
    }

    case TK_END:
    {
      error_at_current(parser, "Unmatched 'do'");
      return NULL;
    }

    case TK_ASSERT:
    {
      advance(parser, scanner);
      AstAssertStmt_* stmt = MAKE_AST_NODE(&parser->allocator, AstAssertStmt_);
      stmt->base.line = parser->current.line;
      stmt->expr = Expr(parser, scanner, parser->current.line);
      return (AstNode_*)stmt;
    }

    // TODO: add parsing multiple return values
    case TK_RETURN:
    {
      advance(parser, scanner);
      AstReturnStmt_* stmt = MAKE_AST_NODE(&parser->allocator, AstReturnStmt_);
      if (block_follow(parser, scanner, true) || parser->current.type == ';') {
        stmt->expr = NULL;
      } else {
        stmt->expr = Expr(parser, scanner, parser->current.line);
      }
      return (AstNode_*)stmt;
    }

    case TK_IF:
    {
      advance(parser, scanner);
      AstIfStmt_* stmt = MAKE_AST_NODE(&parser->allocator, AstIfStmt_);

      stmt->condition_expr = Expr(parser, scanner, line);
      consume(parser, scanner, TK_THEN, "Expected 'then' after condition.");

      stmt->if_stmt = (AstNode_*)Block(parser, scanner, line);
      stmt->elif_stmts.capacity = 100;  // TODO: remove the 100 restriction.
      stmt->elif_stmts.list = alloc(&parser->allocator, stmt->elif_stmts.capacity * sizeof(AstNode_*));

      stmt->elif_exprs.capacity = stmt->elif_stmts.capacity;
      stmt->elif_exprs.list = alloc(&parser->allocator, stmt->elif_exprs.capacity * sizeof(AstNode_*));

      for (int i = 0; i < stmt->elif_stmts.capacity; ++i) {
        if (!match(parser, scanner, TK_ELIF)) {
          break;
        }

        stmt->elif_exprs.list[stmt->elif_exprs.count] = Expr(parser, scanner, line);
        stmt->elif_exprs.count++;

        consume(parser, scanner, TK_THEN, "Expected 'then' after condition.");

        stmt->elif_stmts.list[stmt->elif_stmts.count] = (AstNode_*)Block(parser, scanner, line);
        stmt->elif_stmts.count ++;
      }

      if (match(parser, scanner, TK_ELSE)) {
        stmt->else_stmt = (AstNode_*)Block(parser, scanner, line);
      }

      consume(parser, scanner, TK_END, "Expected 'end' after if-statement.");

      return (AstNode_*)stmt;
    }

    case TK_PRINT:
    {
      advance(parser, scanner);
      AstPrintStmt_* stmt = MAKE_AST_NODE(&parser->allocator, AstPrintStmt_);
      stmt->expr = Expr(parser, scanner, parser->current.line);
      return (AstNode_*)stmt;
    }

    case TK_PASS:
    case TK_SEMICOLON:
    case TK_EOF:
      advance(parser, scanner);
      return NULL;
    default:
      advance(parser, scanner);
      break;
  }
  error(parser, "Syntax error: exepcted a statement");
  return NULL;
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
static AstNode_* Expr(Parser_* parser, Scanner_* scanner, int line) {
  return parse_precedence(parser, scanner, PREC_ASSIGNMENT);
}

static AstPrintStmt_* Print(Parser_* parser, Scanner_* scanner, int line) {
  AstPrintStmt_* stmt = MAKE_AST_NODE(&parser->allocator, AstPrintStmt_);
  stmt->expr = Expr(parser, scanner, line);

  return stmt;
}

static AstNode_* Number(Parser_* parser, Scanner_* scanner, int line) {
  double value = strtod(parser->previous.start, NULL);
  AstPrimaryExp_* expr = MAKE_AST_NODE(&parser->allocator, AstPrimaryExp_);
  expr->base.type = VAL_DOUBLE;
  expr->value = DOUBLE_VAL(value);  
  return (AstNode_*)expr;
}

static AstNode_* Integer(Parser_* parser, Scanner_* scanner, int line) {
  int64_t value = strtoll(parser->previous.start, NULL, 10);  
  AstPrimaryExp_* expr = MAKE_AST_NODE(&parser->allocator, AstPrimaryExp_);
  expr->base.type = VAL_INT;
  expr->value = INT_VAL(value);
  return (AstNode_*)expr;
}

static AstNode_* Nil(Parser_* parser, Scanner_* scanner, int line) {
  AstPrimaryExp_* expr = MAKE_AST_NODE(&parser->allocator, AstPrimaryExp_);
  expr->base.type = VAL_NIL;
  expr->value = NIL_VAL;
  return (AstNode_*)expr;
}

static AstNode_* True(Parser_* parser, Scanner_* scanner, int line) {
  AstPrimaryExp_* expr = MAKE_AST_NODE(&parser->allocator, AstPrimaryExp_);
  expr->base.type = VAL_BOOL;
  expr->value = TRUE_VAL;
  return (AstNode_*)expr;
}

static AstNode_* False(Parser_* parser, Scanner_* scanner, int line) {
  AstPrimaryExp_* expr = MAKE_AST_NODE(&parser->allocator, AstPrimaryExp_);
  expr->base.type = VAL_BOOL;
  expr->value = FALSE_VAL;
  return (AstNode_*)expr;
}

static AstNode_* grouping(Parser_* parser, Scanner_* scanner, int line) {
  AstNode_* expr = Expr(parser, scanner, line);
  consume(parser, scanner, TK_RPAREN, "Expect ')' after expression.");

  return expr;
}

static AstNode_* UnaryOp(Parser_* parser, Scanner_* scanner, int line) {
  TokenType operator_type = parser->previous.type;

  AstUnaryExp_* unary = MAKE_AST_NODE(&parser->allocator, AstUnaryExp_);
  unary->op = operator_type;
  unary->expr = parse_precedence(parser, scanner, PREC_UNARY);
  unary->base.type = VAL_UNKNOWN;

  return (AstNode_*)unary;
}

static AstNode_* BinaryOp(Parser_* parser, Scanner_* scanner, int line) {
  TokenType operator_type = parser->previous.type;
  ParseRule_* rule = get_rule(operator_type);  
  AstNode_* expr = parse_precedence(parser, scanner, (Precedence)(rule->precedence + 1));
  return expr;
}

static AstNode_* Var(Parser_* parser, Scanner_* scanner, int line) {
  AstVarExpr_* var_expr = MAKE_AST_NODE(&parser->allocator, AstVarExpr_);
  AstIdExpr_* expr = MAKE_AST_NODE(&parser->allocator, AstIdExpr_);
  var_expr->expr = (AstExpr_*)expr;
  expr->id = parse_variable(parser, scanner, &parser->previous);
  return (AstNode_*)var_expr;
}

static AstNode_* parse_precedence(Parser_* parser, Scanner_* scanner, Precedence precedence) {
  advance(parser, scanner);
  ParseFn prefix_rule = get_rule(parser->previous.type)->prefix;
  if (!prefix_rule) {
    error(parser, "Expected expression");
    return NULL;
  }

  AstNode_* exp = prefix_rule(parser, scanner, parser->previous.line);
  while (precedence <= get_rule(parser->current.type)->precedence) {
    advance(parser, scanner);
    ParseFn infix_rule = get_rule(parser->previous.type)->infix;
    AstBinaryExp_* new_exp = MAKE_AST_NODE(&parser->allocator, AstBinaryExp_);
    new_exp->base.type = VAL_UNKNOWN;
    new_exp->op = parser->previous.type;    
    new_exp->left = exp;
    new_exp->right = infix_rule(parser, scanner, parser->previous.line);
    exp = (AstNode_*)new_exp;
  }

  return exp;
}

ParseRule_ rules[] = {
  [TK_EOF]          = {NULL,     NULL,     PREC_NONE},
  [TK_ERR]          = {NULL,     NULL,     PREC_NONE},
  [TK_ID]           = {Var,      NULL,     PREC_NONE},
  [TK_LPAREN]       = {grouping, NULL,     PREC_NONE},
  [TK_RPAREN]       = {NULL,     NULL,     PREC_NONE},
  [TK_LBRACKET]     = {NULL,     NULL,     PREC_NONE},
  [TK_RBRACKET]     = {NULL,     NULL,     PREC_NONE},
  [TK_LBRACE]       = {NULL,     NULL,     PREC_NONE},
  [TK_RBRACE]       = {NULL,     NULL,     PREC_NONE},
  [TK_SEMICOLON]    = {NULL,     NULL,     PREC_NONE},
  [TK_COLON]        = {NULL,     NULL,     PREC_NONE},
  [TK_DOT]          = {NULL,     NULL,     PREC_NONE},
  [TK_COMMA]        = {NULL,     NULL,     PREC_NONE},
  [TK_PLUS]         = {NULL,     BinaryOp, PREC_TERM},
  [TK_MINUS]        = {UnaryOp,  BinaryOp, PREC_TERM},
  [TK_AMPERSAND]    = {NULL,     BinaryOp, PREC_BITWISE_AND},
  [TK_PIPE]         = {NULL,     BinaryOp, PREC_BITWISE_OR},
  [TK_HAT]          = {NULL,     BinaryOp, PREC_BITWISE_XOR},
  [TK_TILDE]        = {UnaryOp,  NULL,     PREC_UNARY},
  [TK_STAR]         = {NULL,     BinaryOp, PREC_FACTOR},
  [TK_PERCENT]      = {NULL,     BinaryOp, PREC_FACTOR},
  [TK_BANG]         = {NULL,     NULL,     PREC_NONE},
  [TK_BANG_EQUAL]   = {NULL,     BinaryOp, PREC_EQUALITY},
  [TK_EQUAL]        = {NULL,     NULL,     PREC_NONE},
  [TK_EQUAL_EQUAL]  = {NULL,     BinaryOp, PREC_EQUALITY},
  [TK_GT]           = {NULL,     BinaryOp, PREC_COMPARISON},
  [TK_GTE]          = {NULL,     BinaryOp, PREC_COMPARISON},
  [TK_RSHIFT]       = {NULL,     BinaryOp, PREC_SHIFT},
  [TK_LT]           = {NULL,     BinaryOp, PREC_COMPARISON},
  [TK_LTE]          = {NULL,     BinaryOp, PREC_COMPARISON},
  [TK_LSHIFT]       = {NULL,     BinaryOp, PREC_SHIFT},
  [TK_ARROW]        = {NULL,     NULL,     PREC_NONE},
  [TK_FAT_ARROW]    = {NULL,     NULL,     PREC_NONE},
  [TK_SLASH]        = {NULL,     BinaryOp, PREC_FACTOR},
  [TK_DOUBLE_SLASH] = {NULL,     BinaryOp, PREC_FACTOR},
  [TK_DOUBLE_DOT]   = {NULL,     NULL,     PREC_NONE},
  [TK_TRIPLE_DOT]   = {NULL,     NULL,     PREC_NONE},
  [TK_STRING]       = {NULL,     NULL,     PREC_NONE},
  [TK_INTEGER]      = {Integer,  NULL,     PREC_NONE},
  [TK_NUMBER]       = {Number,   NULL,     PREC_NONE},
  [TK_DO]           = {NULL,     NULL,     PREC_NONE},
  [TK_END]          = {NULL,     NULL,     PREC_NONE},
  [TK_IF]           = {NULL,     NULL,     PREC_NONE},
  [TK_THEN]         = {NULL,     NULL,     PREC_NONE},
  [TK_ELIF]         = {NULL,     NULL,     PREC_NONE},
  [TK_ELSE]         = {NULL,     NULL,     PREC_NONE},
  [TK_FOR]          = {NULL,     NULL,     PREC_NONE},
  [TK_IN]           = {NULL,     NULL,     PREC_NONE},
  [TK_STEP]         = {NULL,     NULL,     PREC_NONE},
  [TK_WHILE]        = {NULL,     NULL,     PREC_NONE},
  [TK_REPEAT]       = {NULL,     NULL,     PREC_NONE},
  [TK_UNTIL]        = {NULL,     NULL,     PREC_NONE},
  [TK_STRUCT]       = {NULL,     NULL,     PREC_NONE},
  [TK_MATCH]        = {NULL,     NULL,     PREC_NONE},
  [TK_FUNCTION]     = {NULL,     NULL,     PREC_NONE},
  [TK_RETURN]       = {NULL,     NULL,     PREC_NONE},
  [TK_AND]          = {NULL,     BinaryOp, PREC_AND},
  [TK_OR]           = {NULL,     BinaryOp, PREC_OR},
  [TK_XOR]          = {NULL,     BinaryOp, PREC_XOR},
  [TK_NOT]          = {UnaryOp,  NULL,     PREC_UNARY},
  [TK_TRUE]         = {True,     NULL,     PREC_NONE},
  [TK_FALSE]        = {False,    NULL,     PREC_NONE},
  [TK_PASS]         = {NULL,     NULL,     PREC_NONE},
  [TK_LET]          = {NULL,     NULL,     PREC_NONE},
  [TK_NIL]          = {Nil,      NULL,     PREC_NONE},
  [TK_BOOL]         = {NULL,     NULL,     PREC_NONE},
  [TK_INT]          = {NULL,     NULL,     PREC_NONE},
  [TK_UINT]         = {NULL,     NULL,     PREC_NONE},
  [TK_FLOAT]        = {NULL,     NULL,     PREC_NONE},
  [TK_DOUBLE]       = {NULL,     NULL,     PREC_NONE},
  [TK_STRING_TYPE]  = {NULL,     NULL,     PREC_NONE},
  [TK_LIST]         = {NULL,     NULL,     PREC_NONE},
  [TK_MAP]          = {NULL,     NULL,     PREC_NONE},
  [TK_SET]          = {NULL,     NULL,     PREC_NONE},
  [TK_ASYNC]        = {NULL,     NULL,     PREC_NONE},
  [TK_AWAIT]        = {NULL,     NULL,     PREC_NONE},
  [TK_YIELD]        = {NULL,     NULL,     PREC_NONE},
};

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