#ifndef AST__H
#define AST__H

#include "memory.h"
#include "tokens.h"
#include "value.h"

#define AST_CLS(name) AST_##name
#define MAKE_AST_NODE(allocator, type) ((type*) make_ast_node((MemoryAllocator_*)(allocator), AST_CLS(type), sizeof(type)))

typedef struct AstNode_ {
  enum {
    AST_CLS(AstProgram_),
    AST_CLS(AstBlock_),
    AST_CLS(AstPrintStmt_),
    AST_CLS(AstUnaryExp_),
    AST_CLS(AstBinaryExp_),
    AST_CLS(AstPrimaryExp_),
    AST_CLS(AstReturnStmt_),
    AST_CLS(AstIfStmt_),
    AST_CLS(AstAssertStmt_),
    AST_CLS(AstVarDeclStmt_),
    AST_CLS(AstVarExpr_),
    AST_CLS(AstIdExpr_),
  } cls;
  int line;
} AstNode_;

typedef struct AstExpr_ {
  AstNode_ base;

  ValueType type;
} AstExpr_;

typedef struct AstList_ {
  int capacity;
  int count;
  struct AstNode_** list;
} AstList_;

// Program ::= Block
typedef struct AstProgram_ {
  AstNode_ base;
  struct AstBlock_* block;
} AstProgram_;

// Block ::= {Statement} [ReturnStmt]
typedef struct AstBlock_ {
  AstNode_ base;

  AstList_ statements;
} AstBlock_;

// TODO: Remove this in favor of a function call
typedef struct AstPrintStmt_ {
  AstNode_ base;

  struct AstNode_* expr;
} AstPrintStmt_;

// VarDecl ::= 'let' IdList ':' (UnionType ['=' ExprList] | '=' ExprList)
typedef struct AstVarDeclStmt_ {
  AstNode_ base;
  char* id;
  ValueType type;

  AstList_ exprs;
} AstVarDeclStmt_;

// Expr ::= UnaryOp Expr
typedef struct AstUnaryExp_ {
  struct AstExpr_ base;
  TokenType op;

  AstNode_* expr;
} AstUnaryExp_;

// Expr ::= Expr BinaryOp Expr
typedef struct AstBinaryExp_ {
  struct AstExpr_ base;
  TokenType op;

  AstNode_* left;
  AstNode_* right;
} AstBinaryExp_;

// Primary ::= 'nil'
//     | 'false'
//     | 'true'
//     | Number
//     | String
typedef struct AstPrimaryExp_ {
  struct AstExpr_ base;

  Value_ value;
} AstPrimaryExp_;

typedef struct AstVarExpr_ {
  struct AstExpr_ base;

  AstExpr_* expr;
} AstVarExpr_;

typedef struct AstIdExpr_ {
  struct AstExpr_ base;

  char* id;
  int id_length;
} AstIdExpr_;

// ReturnStmt ::= 'return' [Expr {',' Expr}]
typedef struct AstReturnStmt_ {
  struct AstNode_ base;
  AstNode_* expr;
} AstReturnStmt_;

// IfStmt ::= 'if' Expr 'then' Block
//   {'elif' Expr 'then' Block}
//   ['else' Block]
// 'end'
typedef struct AstIfStmt_ {
  struct AstNode_ base;
  AstNode_* condition_expr;
  AstNode_* if_stmt;
  AstList_ elif_exprs;
  AstList_ elif_stmts;
  AstNode_* else_stmt;
} AstIfStmt_;

// AssertStmt ::= 'assert' Expr
typedef struct AstAssertStmt_ {
  struct AstNode_ base;
  AstNode_* expr;
} AstAssertStmt_;

typedef struct Ast_ {
  struct AstProgram_* program;
} Ast_;

AstNode_* make_ast_node(MemoryAllocator_* allocator, int cls, size_t size);

#endif  // AST__H