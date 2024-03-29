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
    AST_CLS(AstIndexExpr_),
    AST_CLS(AstIdExpr_),
    AST_CLS(AstAssignmentExpr_),
    AST_CLS(AstInPlaceBinaryStmt_),
    AST_CLS(AstWhileStmt_),
    AST_CLS(AstForStmt_),
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
    AST_CLS(AstClassConstructor_),
    AST_CLS(AstClassConstructorParam_),
    AST_CLS(AstDotExpr_),
    AST_CLS(AstTypeExpr_),
    AST_CLS(AstArrayValueExpr_),
    AST_CLS(AstRangeExpr_),
    AST_CLS(AstTypeDef_),
    AST_CLS(TypeMemberDecl_),
    AST_CLS(AstGenericParam_),
    AST_CLS(AstGenericParams_),
    AST_CLS(AstIndexOrTypeExpr_),
    AST_CLS(AstIndexOrGenericArgs_),
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
  struct Type_* type;

  // This type is what the result of this expression should be interpreted as.
  // For instance, when automatically dereferencing a reference this type is a
  // value type.
  struct Type_* top_type;
} AstExpr_;

typedef struct AstCleanUpTemps_ {
  AstNode_ base;

  ListOf_(Symbol_*) tmps;
} AstCleanUpTemps_;

typedef struct AstTmpDecl_ {
  struct AstExpr_ base;
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
  struct Type_* decl_type;

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

  AstExpr_* left;
  AstExpr_* right;
} AstAssignmentExpr_;

// AssignmentExpr ::= VarList OP ExprList
typedef struct AstInPlaceBinaryStmt_ {
  AstExpr_ base;

  TokenType_ op;
  TokenType_ bin_op;
  AstExpr_* left;
  AstExpr_* right;
} AstInPlaceBinaryStmt_;

// Expr ::= UnaryOp Expr
typedef struct AstUnaryExp_ {
  struct AstExpr_ base;
  TokenType_ op;

  AstExpr_* expr;
} AstUnaryExp_;

// Expr ::= Expr BinaryOp Expr
typedef struct AstBinaryExp_ {
  struct AstExpr_ base;
  TokenType_ op;

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

// ArrayValue ::= '[' Expr {',' Expr} ']'
typedef struct AstArrayValueExpr_ {
  struct AstExpr_ base;
  AstList_ values;
} AstArrayValueExpr_;

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

  struct AstExpr_* prefix;
  struct AstExpr_* index;
} AstIndexExpr_;

typedef struct AstTypeExpr_ {
  struct AstExpr_ base;
} AstTypeExpr_;

typedef struct AstIndexOrTypeExpr_ {
  union {
    struct AstExpr_ base;
    AstVarExpr_ var_index_expr;
    AstTypeExpr_ type_expr;
  };
  struct AstExpr_* prefix;
  struct AstIndexOrGenericArgs_* index_args;
} AstIndexOrTypeExpr_;

typedef struct AstIndexOrGenericArgs_ {
  struct AstExpr_ base;
  AstList_ args;
} AstIndexOrGenericArgs_;

// Var ::= PrefixExpr '.' Id
typedef struct AstDotExpr_ {
  struct AstExpr_ base;

  Type_* cls_ty;
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

// ForStmt ::= 'for' [VarDecl | ExpressionStmt] ';' [Expr] ';' [Expr] 'do' Block 'end'
typedef struct AstForStmt_ {
  struct AstNode_ base;
  AstNode_* opt_var_decl;
  AstExpr_* opt_condition_expr;
  AstExpr_* opt_step_expr;
  AstNode_* block_stmt;
} AstForStmt_;


// AssertStmt ::= 'assert' Expr
typedef struct AstAssertStmt_ {
  struct AstNode_ base;
  AstExpr_* expr;
} AstAssertStmt_;

// GenericParam ::= Id [':' UnionType {'&' UnionType} ]
typedef struct AstGenericParam_ {
  struct AstNode_ base;
  Token_ name;

  Type_* type;
} AstGenericParam_;

// GenericParams ::= '[' GenericParam {',' GenericParam} ']'
typedef struct AstGenericParams_ {
  struct AstNode_ base;
  Token_ generic_str;
  AstList_ params;
  ListOf_(ConstraintType_*) type_params;
} AstGenericParams_;

// TypeMemberDecl ::= [Id ':'] Type {'|' Type}
typedef struct TypeMemberDecl_ {
  struct AstNode_ base;
  Token_ opt_name;

  Type_* type;
} TypeMemberDecl_;

// TypeDef ::= 'type' Id [GenericParams] {TypeMemberDecl ','} [TypeMemberDecl] 'end'
typedef struct AstTypeDef_ {
  struct AstNode_ base;
  Token_ name;
  AstGenericParams_* opt_generics;
  Type_* type;
  AstList_ members;
} AstTypeDef_;

// FunctionDef ::= 'function' [Id] [GenericParams] FunctionBody 'end'
typedef struct AstFunctionDef_ {
  struct AstExpr_ base;
  struct Symbol_* fn_symbol;

  struct AstFunctionBody_* body;
} AstFunctionDef_;

// FunctionBody ::= '(' [FunctionParamList] [',' '...'] ')' ['->' UnionType] Statement
typedef struct AstFunctionBody_ {
  struct AstNode_ base;
  struct Symbol_* fn_symbol;
  AstList_ function_params;
  struct Type_* return_type;
  AstNode_* stmt;
} AstFunctionBody_;

// FunctionParam ::= (Id [':' UnionType])
typedef struct AstFunctionParam_ {
  struct AstNode_ base;
  Token_ name;
  struct Type_* type;

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
  struct Type_* fn_type;
  AstList_ args;
  struct Symbol_* fn_sym;  
} AstFunctionCallArgs_;

// FunctionCallArg ::= Expr
typedef struct AstFunctionCallArg_ {
  struct AstExpr_ base;
  AstExpr_* expr;
} AstFunctionCallArg_;

// ClassDef ::= 'struct' Id [GenericParams] {ClassMemberDecl} 'end'
typedef struct AstClassDef_ {
  struct AstNode_ base;
  Token_ name;
  AstGenericParams_* opt_generics;
  AstList_ members;

  Type_* class_type;
} AstClassDef_;


// ClassMemberDecl_ ::= IdList ':' UnionType ['=' ExprList]
typedef struct AstClassMemberDecl_ {
  struct AstNode_ base;

  Token_ name;
  struct Type_* field_type;
  Symbol_* field_sym;
  AstExpr_* opt_expr;
} AstClassMemberDecl_;

// ClassConstructor ::= [Id] '{' [ClassConstructorParamList] [',' ClassConstructorNamedParamList] '}'
typedef struct AstClassConstructor_ {
  struct AstExpr_ base;
  Token_ name;
  struct AstExpr_* prefix;
  struct AstList_ params;
} AstClassConstructor_;

// ClassConstructorParam ::= [Id '='] Expr
typedef struct AstClassConstructorParam_ {
  struct AstExpr_ base;
  Token_ name;
  struct AstExpr_* expr;
} AstClassConstructorParam_;

typedef struct AstRangeExpr_ {
  struct AstExpr_ base;
  struct AstExpr_* left;
  struct AstExpr_* right;
} AstRangeExpr_;

typedef struct Ast_ {
  struct AstProgram_* program;
} Ast_;

AstNode_* make_ast_node(MemoryAllocator_* allocator, int cls, size_t size, struct Scope_* symbol_table, int line);
#define assert_astnode_is(NODE, CLS) assert_astnode_is_((AstNode_*)(NODE), CLS)
AstNode_* assert_astnode_is_(AstNode_* node, int cls);
#endif  // AST__H