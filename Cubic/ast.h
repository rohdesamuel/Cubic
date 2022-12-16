#ifndef AST__H
#define AST__H

#include "memory.h"
#include "tokens.h"
#include "value.h"

#define AST_CLS(name) AST_##name
#define MAKE_AST_NODE(allocator, type, scope) ((type*) make_ast_node((MemoryAllocator_*)(allocator), AST_CLS(type), sizeof(type), (scope)))
#define MAKE_AST_NOOP(allocator) (make_ast_node((MemoryAllocator_*)(allocator), AST_CLS(AstNoopStmt_), sizeof(AstNoopStmt_), (NULL)))

// TODO: implement types as UnionTypes.
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
    AST_CLS(AstAssignmentExpr_),
    AST_CLS(AstWhileStmt_),
    AST_CLS(AstFunctionDef_),
    AST_CLS(AstFunctionBody_),
    AST_CLS(AstFunctionParam_),
    AST_CLS(AstFunctionCall_),
    AST_CLS(AstFunctionArgs_),
    AST_CLS(AstExpressionStmt_),
    AST_CLS(AstNoopStmt_),

    __AST_NODE_COUNT__,
  } cls;
  int line;
  struct Scope_* scope;
} AstNode_;

#define AS_NODE(PTR) ((struct AstNode_*)(PTR))
#define AS_EXPR(PTR) ((struct AstExpr_*)(PTR))

typedef struct AstNoopStmt_ {
  AstNode_ base;
} AstNoopStmt_;

typedef struct AstExpr_ {
  AstNode_ base;

  struct Type_ type;
} AstExpr_;

typedef struct AstListNode_ {
  AstNode_* node;
  struct AstListNode_* next;
} AstListNode_;

typedef struct AstList_ {
  struct MemoryAllocator_* allocator;
  AstListNode_* head;
  AstListNode_* tail;
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

  // Owned by Parser allocator.
  Token_ name;
  struct Type_ type;

  AstExpr_* expr;
} AstVarDeclStmt_;

// ExpressionStmt ::= AssignmentExpr | PrefixExpr
typedef struct AstExpressionStmt_ {
  AstNode_ base;
  AstNode_* expr;
} AstExpressionStmt_;

// AssignmentExpr ::= VarList '=' ExprList
typedef struct AstAssignmentExpr_ {
  AstExpr_ base;

  AstNode_* left;
  AstNode_* right;
} AstAssignmentExpr_;

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

// Var ::= Id | PrefixExpr '[' Expr ']' | PrefixExpr '.' Id
typedef struct AstVarExpr_ {
  struct AstExpr_ base;

  AstExpr_* expr;
} AstVarExpr_;

// Var :: = Id
typedef struct AstIdExpr_ {
  struct AstExpr_ base;

  Token_ name;
} AstIdExpr_;

// Var ::= PrefixExpr '[' Expr ']'
typedef struct AstIndexExpr_ {
  struct AstExpr_ base;

  Token_ name;
} AstIndexExpr_;

// Var ::= PrefixExpr '.' Id
typedef struct AstDotExpr_ {
  struct AstExpr_ base;
} AstDotExpr_;

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

// WhileStmt ::= 'while' Expr 'do' Block 'end'
typedef struct AstWhileStmt_ {
  struct AstNode_ base;
  AstNode_* condition_expr;
  AstNode_* block_stmt;
} AstWhileStmt_;

// AssertStmt ::= 'assert' Expr
typedef struct AstAssertStmt_ {
  struct AstNode_ base;
  AstNode_* expr;
} AstAssertStmt_;

// FunctionDef ::= 'function' [Id] FunctionBody 'end'
typedef struct AstFunctionDef_ {
  struct AstNode_ base;
  Token_ name;
  struct AstFunctionBody_* body;
} AstFunctionDef_;

// FunctionBody ::= '(' [FunctionParamList] [',' '...'] ')' ['->' UnionType] Statement
typedef struct AstFunctionBody_ {
  struct AstNode_ base;
  AstList_ function_params;
  struct Type_ return_type;
  AstNode_* stmt;
} AstFunctionBody_;

// FunctionParam ::= (Id [':' UnionType]) ['=' Expr]
typedef struct AstFunctionParam_ {
  struct AstNode_ base;
  Token_ name;
  struct Type_ type;
  AstNode_* opt_expr;

} AstFunctionParam_;

// FunctionCall ::= PrefixExpr FunctionArgs
// FunctionArgs :: = '('[ExprList] ')'
typedef struct AstFunctionCall_ {
  struct AstExpr_ base;
  AstNode_* prefix;
  AstNode_* args;
} AstFunctionCall_;

// FunctionArgs ::= '(' [ExprList] ')'
typedef struct AstFunctionArgs_ {
  struct AstNode_* base;
  AstList_ args;
} AstFunctionArgs_;

typedef struct Ast_ {
  struct AstProgram_* program;
} Ast_;

AstNode_* make_ast_node(MemoryAllocator_* allocator, int cls, size_t size, struct Scope_* symbol_table);

#endif  // AST__H