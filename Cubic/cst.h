#ifndef CST__H
#define CST__H

#include "memory.h"
#include "tokens.h"
#include "symbol.h"
#include "value.h"

#define CST_CLS(name) CST_##name
#define MAKE_CST_NODE(ALLOCATOR, CLS, LINE) ((CLS*) make_cst_node((MemoryAllocator_*)(ALLOCATOR), CST_CLS(CLS), sizeof(CLS), LINE))
#define MAKE_CST_NOOP(ALLOCATOR) (make_cst_node((MemoryAllocator_*)(ALLOCATOR), CST_CLS(CstNoopStmt_), sizeof(CstNoopStmt_), 0))
#define CST_CAST(TYPE, EXPR) ((TYPE*)(assert_cstnode_is(EXPR, CST_CLS(TYPE))))

// TODO: implement types as UnionTypes.
#define CSTNODE_LIST(CST_NODE) \
  CST_NODE(CstNode_), \
  CST_NODE(CstProgram_), \
  CST_NODE(CstBlock_), \
  CST_NODE(CstPrintStmt_), \
  CST_NODE(CstUnaryExp_), \
  CST_NODE(CstBinaryExp_), \
  CST_NODE(CstPrimaryExp_), \
  CST_NODE(CstReturnStmt_), \
  CST_NODE(CstIfStmt_), \
  CST_NODE(CstAssertStmt_), \
  CST_NODE(CstVarDeclStmt_), \
  CST_NODE(CstVarExpr_), \
  CST_NODE(CstIndexExpr_), \
  CST_NODE(CstIdExpr_), \
  CST_NODE(CstAssignmentExpr_), \
  CST_NODE(CstInPlaceBinaryStmt_), \
  CST_NODE(CstWhileStmt_), \
  CST_NODE(CstForStmt_), \
  CST_NODE(CstFunctionDef_), \
  CST_NODE(CstFunctionParam_), \
  CST_NODE(CstFunctionCall_), \
  CST_NODE(CstFunctionCallArgs_), \
  CST_NODE(CstExpressionStmt_), \
  CST_NODE(CstNoopExpr_), \
  CST_NODE(CstNoopStmt_), \
  CST_NODE(CstClassDef_), \
  CST_NODE(CstClassMemberDecl_), \
  CST_NODE(CstClassConstructor_), \
  CST_NODE(CstClassConstructorParam_), \
  CST_NODE(CstDotExpr_), \
  CST_NODE(CstUnionType_), \
  CST_NODE(CstTupleType_), \
  CST_NODE(CstPrimitiveType_), \
  CST_NODE(CstIdType_), \
  CST_NODE(CstReferenceType_), \
  CST_NODE(CstType_), \
  CST_NODE(CstArrayValueExpr_), \
  CST_NODE(CstRangeExpr_), \
  CST_NODE(CstTypeDef_), \
  CST_NODE(CstTypeMemberDecl_), \
  CST_NODE(CstGenericParam_), \
  CST_NODE(CstVarOrTypeExpr_), \
  CST_NODE(CstIndexOrGenericArgs_), \
  CST_NODE(CstGenericOrArrayType_), \
  CST_NODE(CstGenericOrArrayExpr_), \
  CST_NODE(CstGenericFunctionDef_), \
  CST_NODE(CstGenericClassDef_)

#define GENERATE_CSTNODE(NODE) CST_CLS(NODE)

typedef struct CstNode_ {
  enum {
    CSTNODE_LIST(GENERATE_CSTNODE),
    __CST_NODE_COUNT__,
  } cls;
  int line;
} CstNode_;

extern size_t cst_node_sizes[__CST_NODE_COUNT__];

#define AS_CNODE(PTR) ((CstNode_*)(PTR))
#define AS_CEXPR(PTR) ((CstNode_*)(PTR))

struct CstList_* cstlist_create(struct MemoryAllocator_* allocator);

// Performs a deep copy of node in the given list.
struct CstList_* cstlist_copy(const struct CstList_* from, struct MemoryAllocator_* allocator);
void cstlist_copyto(struct CstList_* to, const struct CstList_* from, struct MemoryAllocator_* allocator);

void cstlist_init(struct CstList_* list, struct MemoryAllocator_* allocator);
void cstlist_clear(struct CstList_* list);
void cstlist_destroy(struct CstList_** list);
void cstlist_append(struct CstList_* list, CstNode_* node);

typedef struct CstNoopStmt_ {
  CstNode_ base;
} CstNoopStmt_;

typedef struct CstNoopExpr_ {
  CstNode_ base;
} CstNoopExpr_;

typedef struct CstListNode_ {
  CstNode_* node;
  struct CstListNode_* next;
} CstListNode_;

typedef struct CstList_ {
  struct MemoryAllocator_* allocator;
  CstListNode_* head;
  CstListNode_* tail;
  int count;
} CstList_;

// Program ::= Block
typedef struct CstProgram_ {
  CstNode_ base;
  struct CstBlock_* block;
} CstProgram_;

// Block ::= {Statement} [ReturnStmt]
typedef struct CstBlock_ {
  CstNode_ base;

  CstList_ statements;
} CstBlock_;

// TODO: Remove this in favor of a function call
typedef struct CstPrintStmt_ {
  CstNode_ base;

  CstNode_* expr;
} CstPrintStmt_;

// VarDecl ::= 'val' IdList ':' (UnionType ['=' ExprList] | '=' ExprList)
typedef struct CstVarDeclStmt_ {
  CstNode_ base;

  // Either val or var.
  TokenType_ decl_token;
  Token_ name;
  CstNode_* opt_type;
  CstNode_* expr;
} CstVarDeclStmt_;

// ExpressionStmt ::= AssignmentExpr | PrefixExpr
typedef struct CstExpressionStmt_ {
  CstNode_ base;
  CstNode_* expr;
} CstExpressionStmt_;

// AssignmentExpr ::= VarList '=' ExprList
typedef struct CstAssignmentExpr_ {
  CstNode_ base;

  CstNode_* left;
  CstNode_* right;
} CstAssignmentExpr_;

// AssignmentExpr ::= VarList OP ExprList
typedef struct CstInPlaceBinaryStmt_ {
  CstNode_ base;

  TokenType_ op;
  TokenType_ bin_op;
  CstNode_* left;
  CstNode_* right;
} CstInPlaceBinaryStmt_;

// Expr ::= UnaryOp Expr
typedef struct CstUnaryExp_ {
  CstNode_ base;
  TokenType_ op;

  CstNode_* expr;
} CstUnaryExp_;

// Expr ::= Expr BinaryOp Expr
typedef struct CstBinaryExp_ {
  CstNode_ base;
  TokenType_ op;

  CstNode_* left;
  CstNode_* right;
} CstBinaryExp_;

// Primary ::= 'nil'
//     | 'false'
//     | 'true'
//     | Number
//     | String
typedef struct CstPrimaryExp_ {
  CstNode_ base;
  TokenType_ type;
  Value_ value;
} CstPrimaryExp_;

// ArrayValue ::= '[' Expr {',' Expr} ']'
typedef struct CstArrayValueExpr_ {
  CstNode_ base;
  CstList_ values;
} CstArrayValueExpr_;

// Var ::= Id | PrefixExpr '[' Expr ']' | PrefixExpr '.' Id
typedef struct CstVarExpr_ {
  CstNode_ base;

  CstNode_* expr;
} CstVarExpr_;

// Var ::= Id
typedef struct CstIdExpr_ {
  CstNode_ base;

  Token_ name;
} CstIdExpr_;

// Var ::= PrefixExpr '[' Expr ']'
typedef struct CstIndexExpr_ {
  CstNode_ base;

  CstNode_* prefix;
  CstNode_* index;
} CstIndexExpr_;

typedef struct CstUnionType_ {
  CstNode_ base;
  CstList_ types;
} CstUnionType_;

typedef struct CstTupleType_ {
  CstNode_ base;
  CstList_ types;
} CstTupleType_;

typedef struct CstPrimitiveType_ {
  CstNode_ base;
  TokenType_ type;
} CstPrimitiveType_;

typedef struct CstIdType_ {
  CstNode_ base;
  Token_ id;
} CstIdType_;

typedef struct CstReferenceType_ {
  CstNode_ base;
  CstNode_* type;
} CstReferenceType_;

typedef struct CstType_ {
  CstNode_ base;
  CstNode_* impl;
} CstType_;

typedef struct CstVarOrTypeExpr_ {
  union {
    CstNode_ base;
    CstVarExpr_ var_expr;
    CstType_ type_expr;
  };
  CstNode_* prefix;
  struct CstIndexOrGenericArgs_* index_args;
} CstVarOrTypeExpr_;

typedef struct CstIndexOrGenericArgs_ {
  CstNode_ base;
  CstList_ args;
} CstIndexOrGenericArgs_;

typedef struct CstGenericOrArrayType_ {
  CstNode_ base;
  CstNode_* prefix;
  CstList_ args;
} CstGenericOrArrayType_;

typedef struct CstGenericOrArrayExpr_ {
  CstNode_ base;
  CstNode_* prefix;
  CstList_ args;
} CstGenericOrArrayExpr_;

// Var ::= PrefixExpr '.' Id
typedef struct CstDotExpr_ {
  CstNode_ base;
  CstNode_* prefix;
  Token_ id;
} CstDotExpr_;

// ReturnStmt ::= 'return' [Expr {',' Expr}]
typedef struct CstReturnStmt_ {
  CstNode_ base;
  CstNode_* expr;
} CstReturnStmt_;

// IfStmt ::= 'if' Expr 'then' Block
//   {'elif' Expr 'then' Block}
//   ['else' Block]
// 'end'
typedef struct CstIfStmt_ {
  CstNode_ base;
  CstNode_* condition_expr;
  CstNode_* if_stmt;
  CstList_ elif_exprs;
  CstList_ elif_stmts;
  CstNode_* else_stmt;
} CstIfStmt_;

// WhileStmt ::= 'while' Expr 'do' Block 'end'
typedef struct CstWhileStmt_ {
  CstNode_ base;
  CstNode_* condition_expr;
  CstNode_* block_stmt;
} CstWhileStmt_;

// ForStmt ::= 'for' [VarDecl | ExpressionStmt] ';' [Expr] ';' [Expr] 'do' Block 'end'
typedef struct CstForStmt_ {
  CstNode_ base;
  CstNode_* opt_var_decl;
  CstNode_* opt_condition_expr;
  CstNode_* opt_step_expr;
  CstNode_* block_stmt;
} CstForStmt_;

// AssertStmt ::= 'assert' Expr
typedef struct CstAssertStmt_ {
  CstNode_ base;
  CstNode_* expr;
} CstAssertStmt_;

// GenericParam ::= Id [':' UnionType {'&' UnionType} ]
typedef struct CstGenericParam_ {
  CstNode_ base;
  Token_ name;
  CstList_ constraints;
} CstGenericParam_;

// TypeMemberDecl ::= [Id ':'] Type {'|' Type}
typedef struct CstTypeMemberDecl_ {
  CstNode_ base;
  Token_ opt_name;
  CstNode_* type;
} CstTypeMemberDecl_;

// TypeDef ::= 'type' Id [GenericParams] {TypeMemberDecl ','} [TypeMemberDecl] 'end'
typedef struct CstTypeDef_ {
  CstNode_ base;
  Token_ name;
  CstList_ generic_params;
  CstNode_* type;
} CstTypeDef_;

// FunctionDef ::= 'function' [Id] [GenericParams] '(' [FunctionParamList] [',' '...'] ')' ['->' UnionType] Statement 'end'
typedef struct CstFunctionDef_ {
  CstNode_ base;
  Token_ name;
  CstList_ function_params;
  CstNode_* return_type;
  CstNode_* body;
} CstFunctionDef_;

// FunctionDef ::= 'function' [Id] [GenericParams] '(' [FunctionParamList] [',' '...'] ')' ['->' UnionType] Statement 'end'
typedef struct CstGenericFunctionDef_ {
  CstNode_ base;
  Token_ name;
  CstList_ generic_params;
  CstFunctionDef_* function_def;
} CstGenericFunctionDef_;

// FunctionParam ::= (['in' | 'out'] Id [':' UnionType])
typedef struct CstFunctionParam_ {
  CstNode_ base;
  TokenType_ kind;
  Token_ name;
  CstNode_* type;
} CstFunctionParam_;

// FunctionCall ::= PrefixExpr '(' [ExprList] ')'
typedef struct CstFunctionCall_ {
  CstNode_ base;
  CstNode_* prefix;
  CstNode_* args;
} CstFunctionCall_;

// FunctionCallArgs ::= '(' { FunctionCallArg } ')'
typedef struct CstFunctionCallArgs_ {
  CstNode_ base;
  CstList_ args;
} CstFunctionCallArgs_;

// ClassDef ::= 'struct' Id [GenericParams] {ClassMemberDecl} 'end'
typedef struct CstClassDef_ {
  CstNode_ base;
  Token_ name;
  CstList_ generic_params;
  CstList_ members;
} CstClassDef_;

// ClassMemberDecl_ ::= IdList ':' UnionType ['=' ExprList]
typedef struct CstClassMemberDecl_ {
  CstNode_ base;

  Token_ name;
  CstNode_* field_type;
  CstNode_* opt_expr;
} CstClassMemberDecl_;

// ClassConstructor ::= [Id] '{' [ClassConstructorParamList] [',' ClassConstructorNamedParamList] '}'
typedef struct CstClassConstructor_ {
  CstNode_ base;
  Token_ name;
  CstNode_* prefix;
  struct CstList_ params;
} CstClassConstructor_;

// ClassConstructorParam ::= [Id '='] Expr
typedef struct CstClassConstructorParam_ {
  CstNode_ base;
  Token_ name;
  CstNode_* expr;
} CstClassConstructorParam_;

typedef struct CstRangeExpr_ {
  CstNode_ base;
  CstNode_* left;
  CstNode_* right;
} CstRangeExpr_;

typedef struct Cst_ {
  struct CstProgram_* program;
} Cst_;

CstNode_* make_cst_node(MemoryAllocator_* allocator, int cls, size_t size, int line);
#define assert_cstnode_is(NODE, CLS) assert_cstnode_is_((CstNode_*)(NODE), CLS)
CstNode_* assert_cstnode_is_(CstNode_* node, int cls);
#endif  // CST__H