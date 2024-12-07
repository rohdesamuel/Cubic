#ifndef AST__H
#define AST__H

#include "memory.h"
#include "tokens.h"
#include "symbol.h"
#include "value.h"

#define AST_CLS(name) AST_##name
#define MAKE_AST_NODE(ALLOCATOR, CLS, SCOPE, NODE) ((CLS*) make_ast_node((MemoryAllocator_*)(ALLOCATOR), AST_CLS(CLS), sizeof(CLS), (SCOPE), (struct CstNode_*)(NODE)))
#define MAKE_AST_NOOP(ALLOCATOR) (make_ast_node((MemoryAllocator_*)(ALLOCATOR), AST_CLS(AstNoopStmt_), sizeof(AstNoopStmt_), NULL, NULL))
#define AST_CAST(TYPE, EXPR) ((TYPE*)(assert_astnode_is(EXPR, AST_CLS(TYPE))))

// TODO: implement types as UnionTypes.
#define ASTNODE_LIST(AST_NODE) \
  AST_NODE(AstProgram_), \
  AST_NODE(AstBlock_), \
  AST_NODE(AstStmt_), \
  AST_NODE(AstExpr_), \
  AST_NODE(AstPrintStmt_), \
  AST_NODE(AstUnaryExp_), \
  AST_NODE(AstBinaryExp_), \
  AST_NODE(AstPrimaryExp_), \
  AST_NODE(AstReturnStmt_), \
  AST_NODE(AstIfStmt_), \
  AST_NODE(AstAssertStmt_), \
  AST_NODE(AstVarDeclStmt_), \
  AST_NODE(AstVarExpr_), \
  AST_NODE(AstIndexExpr_), \
  AST_NODE(AstIdExpr_), \
  AST_NODE(AstAssignmentExpr_), \
  AST_NODE(AstInPlaceBinaryStmt_), \
  AST_NODE(AstWhileStmt_), \
  AST_NODE(AstForStmt_), \
  AST_NODE(AstFunctionDef_), \
  AST_NODE(AstGenericFunctionDef_), \
  AST_NODE(AstFunctionParam_), \
  AST_NODE(AstFunctionCall_), \
  AST_NODE(AstFunctionCallArgs_), \
  AST_NODE(AstExpressionStmt_), \
  AST_NODE(AstNoopExpr_), \
  AST_NODE(AstNoopStmt_), \
  AST_NODE(AstCleanUpTemps_), \
  AST_NODE(AstTmpDecl_), \
  AST_NODE(AstClassDef_), \
  AST_NODE(AstClassMemberDecl_), \
  AST_NODE(AstClassConstructor_), \
  AST_NODE(AstClassConstructorParam_), \
  AST_NODE(AstDotExpr_), \
  AST_NODE(AstTypeExpr_), \
  AST_NODE(AstArrayValueExpr_), \
  AST_NODE(AstRangeExpr_), \
  AST_NODE(AstTypeDef_), \
  AST_NODE(TypeMemberDecl_), \
  AST_NODE(AstGenericParam_), \
  AST_NODE(AstGenericParams_), \
  AST_NODE(AstVarOrTypeExpr_), \
  AST_NODE(AstIndexOrGenericArgs_)

#define GENERATE_ASTNODE(NODE) AST_CLS(NODE)

typedef struct AstListNode_ {
  struct AstNode_* node;
  struct AstListNode_* next;
} AstListNode_;

typedef struct AstList_ {
  struct MemoryAllocator_* allocator;
  AstListNode_* head;
  AstListNode_* tail;
  int count;
} AstList_;

typedef struct AstNode_ {
  enum {
    ASTNODE_LIST(GENERATE_ASTNODE),
    __AST_NODE_COUNT__,
  } cls;

  const struct CstNode_* parent;
  struct Scope_* scope;
  AstList_ specializations;
  int line;
} AstNode_;

extern size_t ast_node_sizes[__AST_NODE_COUNT__];

#define AS_NODE(PTR) ((struct AstNode_*)(PTR))
#define AS_EXPR(PTR) ((struct AstExpr_*)(PTR))

struct AstList_* astlist_create(struct MemoryAllocator_* allocator);

// Performs a deep copy of node in the given list.
struct AstList_* astlist_copy(const struct AstList_* from, struct MemoryAllocator_* allocator, struct Scope_* scope);
void astlist_copyto(struct AstList_* to, const struct AstList_* from, struct MemoryAllocator_* allocator, struct Scope_* scope);

void astlist_init(struct AstList_* list, struct MemoryAllocator_* allocator);
void astlist_clear(struct AstList_* list);
void astlist_destroy(struct AstList_** list);
void astlist_append(struct AstList_* list, struct AstNode_* node);

typedef struct AstStmt_ {
  AstNode_ base;
  AstNode_* cleanup;
  AstNode_* stmt;
} AstStmt_;

typedef struct AstExpr_ {
  AstNode_ base;
  struct AstExpr_* expr;

  // This type is the result of executing this expression.
  Type_* type;

  // This type is what the result of this expression should be interpreted as.
  // For instance, when automatically dereferencing a reference this type is a
  // value type.
  Type_* top_type;
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

  Token_ name;
  Type_* decl_type;

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

typedef struct AstVarOrTypeExpr_ {
  union {
    struct AstExpr_ base;
    AstVarExpr_ var_expr;
    AstTypeExpr_ type_expr;
  };
  struct AstExpr_* prefix;
  struct AstIndexOrGenericArgs_* index_args;
} AstVarOrTypeExpr_;

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
  Type_* type;
  AstList_ members;
} AstTypeDef_;

// FunctionDef ::= 'function' [Id] '(' [FunctionParamList] [',' '...'] ')' ['->' UnionType] Statement 'end'
typedef struct AstFunctionDef_ {
  struct AstExpr_ base;
  struct Symbol_* fn_symbol;
  Type_* fn_type;

  struct AstNode_* body;
  AstList_ function_params;
  Type_* return_type;
} AstFunctionDef_;

// GenericFunctionDef ::= 'function' [Id] [GenericParams] FunctionBody 'end'
typedef struct AstGenericFunctionDef_ {
  AstFunctionDef_ base;
  struct AstGenericParams_* generic_params;
} AstGenericFunctionDef_;

// FunctionParam ::= (Id [':' UnionType])
typedef struct AstFunctionParam_ {
  struct AstNode_ base;
  Token_ name;
  Type_* type;
} AstFunctionParam_;

// FunctionCall ::= PrefixExpr FunctionCallArgs
// FunctionCallArgs ::= '(' [ExprList] ')'
typedef struct AstFunctionCall_ {
  struct AstExpr_ base;
  AstExpr_* prefix;
  struct AstFunctionCallArgs_* args;
} AstFunctionCall_;

// FunctionCallArgs ::= '(' { FunctionCallArg } ')'
typedef struct AstFunctionCallArgs_ {
  struct AstNode_ base;
  Type_* fn_type;
  AstList_ args;
} AstFunctionCallArgs_;

// ClassDef ::= 'struct' Id [GenericParams] {ClassMemberDecl} 'end'
typedef struct AstClassDef_ {
  struct AstNode_ base;
  Token_ name;
  AstList_ members;

  Type_* class_type;
} AstClassDef_;


// ClassMemberDecl_ ::= IdList ':' UnionType ['=' ExprList]
typedef struct AstClassMemberDecl_ {
  struct AstNode_ base;

  Token_ name;
  Type_* field_type;
  AstExpr_* opt_expr;
} AstClassMemberDecl_;

// ClassConstructor ::= [Id] '{' [ClassConstructorParamList] [',' ClassConstructorNamedParamList] '}'
typedef struct AstClassConstructor_ {
  struct AstExpr_ base;
  Token_ name;
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

AstNode_* make_ast_node(MemoryAllocator_* allocator, int cls, size_t size, struct Scope_* symbol_table, const struct CstNode_* node);
AstNode_* copy_ast_node(MemoryAllocator_* allocator, AstNode_* node, struct Scope_* scope);
#define assert_astnode_is(NODE, CLS) assert_astnode_is_((AstNode_*)(NODE), CLS)
AstNode_* assert_astnode_is_(AstNode_* node, int cls);
#endif  // AST__H