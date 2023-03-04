#ifndef AST__H
#define AST__H

#include "memory.h"
#include "tokens.h"
#include "symbol.h"
#include "value.h"

#define AST_CLS(name) AST_##name
#define MAKE_AST_NODE(ALLOCATOR, CLS, SCOPE, LINE) ((CLS*) make_ast_node((MemoryAllocator_*)(ALLOCATOR), AST_CLS(CLS), sizeof(CLS), (SCOPE), LINE))
#define MAKE_AST_NOOP(ALLOCATOR) (make_ast_node((MemoryAllocator_*)(ALLOCATOR), AST_CLS(AstNoopStmt_), sizeof(AstNoopStmt_), (NULL), 0))
#define MAKE_AST_STMT(ALLOCATOR, CLS, SCOPE, LINE) MAKE_AST_NODE(ALLOCATOR, CLS, SCOPE, LINE)
#define MAKE_AST_EXPR(ALLOCATOR, CLS, SCOPE, LINE) MAKE_AST_NODE(ALLOCATOR, CLS, SCOPE, LINE)
#define AST_CAST(TYPE, EXPR) ((TYPE*)(assert_astnode_is(EXPR, AST_CLS(TYPE))))

// TODO: implement types as UnionTypes.
typedef struct AstNode_ {
  enum {
    AST_CLS(AstProgram_),
    AST_CLS(AstBlock_),
    AST_CLS(AstStmt_),
    AST_CLS(AstExpr_),
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
    AST_CLS(AstFunctionCallArgs_),
    AST_CLS(AstFunctionCallArg_),
    AST_CLS(AstExpressionStmt_),
    AST_CLS(AstNoopExpr_),
    AST_CLS(AstNoopStmt_),
    AST_CLS(AstCleanUpTemps_),
    AST_CLS(AstTmpDecl_),
    AST_CLS(AstClassDef_),
    AST_CLS(AstClassMemberDecl_),
    AST_CLS(AstConstructor_),
    AST_CLS(AstConstructorField_),
    AST_CLS(AstDotExpr_),
    AST_CLS(AstTypeExpr_),

    __AST_NODE_COUNT__,
  } cls;
  int line;
  struct Scope_* scope;
} AstNode_;

#define AS_NODE(PTR) ((struct AstNode_*)(PTR))
#define AS_EXPR(PTR) ((struct AstExpr_*)(PTR))

typedef struct AstStmt_ {
  AstNode_ base;
  AstNode_* cleanup;
  AstNode_* stmt;
} AstStmt_;

typedef struct AstExpr_ {
  AstNode_ base;
  struct AstExpr_* expr;

  // This type is the result of executing this expression.
  struct SemanticType_ sem_type;

  // This type is what the result of this expression should be interpreted as.
  // For instance, when automatically dereferencing a reference this type is a
  // value type.
  struct SemanticType_ top_sem_type;
} AstExpr_;

typedef struct AstCleanUpTemps_ {
  AstNode_ base;

  ListOf_(Symbol_*) tmps;
} AstCleanUpTemps_;

typedef struct AstTmpDecl_ {
  struct AstExpr_ base;
  struct Symbol_* tmp;

  struct AstExpr_* expr;
} AstTmpDecl_;

typedef struct AstNoopStmt_ {
  AstNode_ base;
} AstNoopStmt_;

typedef struct AstNoopExpr_ {
  AstExpr_ base;
} AstNoopExpr_;

typedef struct AstListNode_ {
  AstNode_* node;
  struct AstListNode_* next;
} AstListNode_;

typedef struct AstList_ {
  struct MemoryAllocator_* allocator;
  AstListNode_* head;
  AstListNode_* tail;
  int count;
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

  struct AstExpr_* expr;
} AstPrintStmt_;

// VarDecl ::= 'val' IdList ':' (UnionType ['=' ExprList] | '=' ExprList)
typedef struct AstVarDeclStmt_ {
  AstNode_ base;

  // Owned by Parser allocator.
  Token_ name;
  struct SemanticType_ sem_type;

  AstExpr_* expr;
} AstVarDeclStmt_;

// ExpressionStmt ::= AssignmentExpr | PrefixExpr
typedef struct AstExpressionStmt_ {
  AstNode_ base;
  AstExpr_* expr;
} AstExpressionStmt_;

// AssignmentExpr ::= VarList '=' ExprList
typedef struct AstAssignmentExpr_ {
  AstExpr_ base;

  AstNode_* left;
  AstExpr_* right;
} AstAssignmentExpr_;

// Expr ::= UnaryOp Expr
typedef struct AstUnaryExp_ {
  struct AstExpr_ base;
  TokenType op;

  AstExpr_* expr;
} AstUnaryExp_;

// Expr ::= Expr BinaryOp Expr
typedef struct AstBinaryExp_ {
  struct AstExpr_ base;
  TokenType op;

  AstExpr_* left;
  AstExpr_* right;
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

// Var ::= Id
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

  struct AstExpr_* prefix;
  Token_ id;
} AstDotExpr_;

// ReturnStmt ::= 'return' [Expr {',' Expr}]
typedef struct AstReturnStmt_ {
  struct AstNode_ base;
  AstExpr_* expr;
} AstReturnStmt_;

// IfStmt ::= 'if' Expr 'then' Block
//   {'elif' Expr 'then' Block}
//   ['else' Block]
// 'end'
typedef struct AstIfStmt_ {
  struct AstNode_ base;
  AstExpr_* condition_expr;
  AstNode_* if_stmt;
  AstList_ elif_exprs;
  AstList_ elif_stmts;
  AstNode_* else_stmt;
} AstIfStmt_;

// WhileStmt ::= 'while' Expr 'do' Block 'end'
typedef struct AstWhileStmt_ {
  struct AstNode_ base;
  AstExpr_* condition_expr;
  AstNode_* block_stmt;
} AstWhileStmt_;

// AssertStmt ::= 'assert' Expr
typedef struct AstAssertStmt_ {
  struct AstNode_ base;
  AstExpr_* expr;
} AstAssertStmt_;

// FunctionDef ::= 'function' [Id] FunctionBody 'end'
typedef struct AstFunctionDef_ {
  struct AstExpr_ base;
  struct Symbol_* fn_symbol;

  struct AstFunctionBody_* body;
} AstFunctionDef_;

// FunctionBody ::= '(' [FunctionParamList] [',' '...'] ')' ['->' UnionType] Statement
typedef struct AstFunctionBody_ {
  struct AstNode_ base;
  AstList_ function_params;
  struct SemanticType_ return_type;
  AstNode_* stmt;
} AstFunctionBody_;

// FunctionParam ::= (Id [':' UnionType])
typedef struct AstFunctionParam_ {
  struct AstNode_ base;
  Token_ name;
  struct SemanticType_ type;

} AstFunctionParam_;

// FunctionCall ::= PrefixExpr FunctionCallArgs
// FunctionCallArgs ::= '(' [ExprList] ')'
typedef struct AstFunctionCall_ {
  struct AstExpr_ base;
  AstExpr_* prefix;
  struct AstFunctionCallArgs_* args;
  struct Symbol_* fn_sym;
} AstFunctionCall_;

// FunctionCallArgs ::= '(' { FunctionCallArg } ')'
typedef struct AstFunctionCallArgs_ {
  struct AstNode_ base;
  AstList_ args;
  struct FunctionSymbol_* fn_sym;
} AstFunctionCallArgs_;

// FunctionCallArg ::= Expr
typedef struct AstFunctionCallArg_ {
  struct AstExpr_ base;
  AstExpr_* expr;
  struct Symbol_* field_sym;
} AstFunctionCallArg_;

// ClassDef ::= 'struct' {ClassMemberDecl} 'end'
typedef struct AstClassDef_ {
  struct AstNode_ base;
  Token_ name;
  AstList_ members;

  SemanticType_ class_type;
} AstClassDef_;

// ClassMemberDecl_ ::= IdList ':' UnionType ['=' ExprList]
typedef struct AstClassMemberDecl_ {
  struct AstNode_ base;

  Token_ name;
  struct SemanticType_ sem_type;
  AstExpr_* opt_expr;
} AstClassMemberDecl_;

// ClassConstructor ::= Id '{' [ClassConstructorFieldList] '}'
typedef struct AstConstructor_ {
  struct AstExpr_ base;

  struct AstList_ fields;
} AstConstructor_;

// ClassConstructorField ::= Id '=' Expr
typedef struct AstConstructorField_ {
  struct AstExpr_ base;
  Token_ name;

  struct AstExpr_* expr;
} AstConstructorField_;

typedef struct AstTypeExpr_ {
  struct AstExpr_ base;
} AstTypeExpr_;

typedef struct Ast_ {
  struct AstProgram_* program;
} Ast_;

AstNode_* make_ast_node(MemoryAllocator_* allocator, int cls, size_t size, struct Scope_* symbol_table, int line);
#define assert_astnode_is(NODE, CLS) assert_astnode_is_((AstNode_*)(NODE), CLS)
AstNode_* assert_astnode_is_(AstNode_* node, int cls);
#endif  // AST__H