#ifndef AST__H
#define AST__H

#include "tokens.h"
#include "value.h"

#define AST_CLS(name) AST_##name

typedef struct AstNode_ {
  enum {
    AST_PROGRAM,
    AST_BLOCK,
    AST_PRINT_STMT,
    AST_UNARY_OP_EXPR,
    AST_BINARY_OP_EXPR,
    AST_INTEGER_CONSTANT,
    AST_NUMBER_CONSTANT
  } cls;
  int line;
} AstNode_;

typedef struct AstList_ {
  int capacity;
  int count;
  struct AstNode_* list;
} AstList_;

typedef struct AstProgram_ {
  AstNode_ base;
  struct AstBlock_* block;
} AstProgram_;

typedef struct AstBlock_ {
  AstNode_ base;

  AstList_ statements;
} AstBlock_;

typedef struct AstPrintStmt_ {
  AstNode_ base;

  struct AstNode_* expr;
} AstPrintStmt_;

typedef struct AstUnaryExp_ {
  struct AstNode_ base;
  TokenType op;

  AstNode_* expr;
} AstUnaryExp_;


typedef struct AstBinaryExp_ {
  struct AstNode_ base;
  TokenType op;

  AstNode_* left;
  AstNode_* right;
} AstBinaryExp_;

typedef struct AstIntegerExp_ {
  struct AstNode_ base;
  int64_t value;
} AstIntegerExp_;

typedef struct AstNumberExp_ {
  struct AstNode_ base;
  double value;
} AstNumberExp_;

typedef struct Ast_ {
  struct AstProgram_* program;
} Ast_;

#endif  // AST__H