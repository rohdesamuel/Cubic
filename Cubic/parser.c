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

static void advance(Parser_* parser, Scanner_* scanner);
static void consume(Parser_* parser, Scanner_* scanner, TokenType type, const char* message);
static bool match(Parser_* parser, Scanner_* scanner, TokenType type);
static bool check(Parser_* parser, TokenType type);

static ParseRule_* get_rule(TokenType type);

static void error_at_current(Parser_* parser, const char* message);
static void error(Parser_* parser, const char* message);
static void error_at(Parser_* parser, Token_* token, const char* message);

void parser_init(Parser_* parser) {
  memset(parser, 0, sizeof(Parser_));
}

AstNode_* parse(Parser_* parser, Scanner_* scanner, const char* source) {
  advance(parser, scanner);
  AstProgram_* root = calloc(1, sizeof(AstProgram_));
  AstBlock_* block = root->block = calloc(1, sizeof(AstBlock_));

  while (!match(parser, scanner, TK_EOF)) {
    TokenType tk = parser->current.type;
    switch (tk) {
      case TK_PRINT:
        advance(parser, scanner);
        AstPrintStmt_* stmt = calloc(1, sizeof(AstPrintStmt_));
        stmt->base.cls = AST_PRINT_STMT;
        stmt->expr = Expr(parser, scanner, parser->current.line);
        block->statements.list = (AstNode_*)stmt;
        break;

      default:
        advance(parser, scanner);
        break;
    }    
  }

  consume(parser, scanner, TK_EOF, "Expected end of file.");
  return (AstNode_*)root;
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
  AstPrintStmt_* stmt = calloc(1, sizeof(AstPrintStmt_));
  stmt->expr = Expr(parser, scanner, line);

  return stmt;
}

static AstNode_* Number(Parser_* parser, Scanner_* scanner, int line) {
  double value = strtod(parser->previous.start, NULL);
  
  AstNumberExp_* expr = calloc(1, sizeof(AstNumberExp_));
  expr->base.cls = AST_NUMBER_CONSTANT;
  expr->value = value;
  return (AstNode_*)expr;
}

static AstNode_* Integer(Parser_* parser, Scanner_* scanner, int line) {
  int64_t value = strtoll(parser->previous.start, NULL, 10);  

  AstIntegerExp_* expr = calloc(1, sizeof(AstIntegerExp_));
  expr->base.cls = AST_INTEGER_CONSTANT;
  expr->value = value;
  return (AstNode_*)expr;
}

static AstNode_* grouping(Parser_* parser, Scanner_* scanner, int line) {
  AstNode_* expr = Expr(parser, scanner, line);
  consume(parser, scanner, TK_RPAREN, "Expect ')' after expression.");

  return expr;
}

static AstNode_* UnaryOp(Parser_* parser, Scanner_* scanner, int line) {
  TokenType operator_type = parser->previous.type;

  AstUnaryExp_* unary = calloc(1, sizeof(AstUnaryExp_));
  unary->base.cls = AST_UNARY_OP_EXPR;
  unary->op = operator_type;
  unary->expr = parse_precedence(parser, scanner, PREC_UNARY);

  return (AstNode_*)unary;
}

static AstNode_* BinaryOp(Parser_* parser, Scanner_* scanner, int line) {
  TokenType operator_type = parser->previous.type;
  ParseRule_* rule = get_rule(operator_type);  

  AstNode_* expr = parse_precedence(parser, scanner, (Precedence)(rule->precedence + 1));
  return expr;
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
    AstBinaryExp_* new_exp = calloc(1, sizeof(AstBinaryExp_));
    new_exp->base.cls = AST_BINARY_OP_EXPR;
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
  [TK_ID]           = {NULL,     NULL,     PREC_NONE},
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
  [TK_AMPERSAND]    = {NULL,     NULL,     PREC_NONE},
  [TK_PIPE]         = {NULL,     NULL,     PREC_NONE},
  [TK_HAT]          = {NULL,     NULL,     PREC_NONE},
  [TK_TILDE]        = {NULL,     NULL,     PREC_NONE},
  [TK_STAR]         = {NULL,     BinaryOp, PREC_FACTOR},
  [TK_PERCENT]      = {NULL,     NULL,     PREC_NONE},
  [TK_BANG]         = {NULL,     NULL,     PREC_NONE},
  [TK_BANG_EQUAL]   = {NULL,     NULL,     PREC_NONE},
  [TK_EQUAL]        = {NULL,     NULL,     PREC_NONE},
  [TK_EQUAL_EQUAL]  = {NULL,     NULL,     PREC_NONE},
  [TK_GT]           = {NULL,     NULL,     PREC_NONE},
  [TK_GTE]          = {NULL,     NULL,     PREC_NONE},
  [TK_RSHIFT]       = {NULL,     NULL,     PREC_NONE},
  [TK_LT]           = {NULL,     NULL,     PREC_NONE},
  [TK_LTE]          = {NULL,     NULL,     PREC_NONE},
  [TK_LSHIFT]       = {NULL,     NULL,     PREC_NONE},
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
  [TK_AND]          = {NULL,     NULL,     PREC_NONE},
  [TK_OR]           = {NULL,     NULL,     PREC_NONE},
  [TK_XOR]          = {NULL,     NULL,     PREC_NONE},
  [TK_NOT]          = {NULL,     NULL,     PREC_NONE},
  [TK_TRUE]         = {NULL,     NULL,     PREC_NONE},
  [TK_FALSE]        = {NULL,     NULL,     PREC_NONE},
  [TK_PASS]         = {NULL,     NULL,     PREC_NONE},
  [TK_LET]          = {NULL,     NULL,     PREC_NONE},
  [TK_NIL]          = {NULL,     NULL,     PREC_NONE},
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