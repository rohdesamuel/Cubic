#ifndef AST_NODE__H
#define AST_NODE__H

#include <vector>

#include "tokens.h"
#include "value.h"

struct AstNode {
  int line;
};

struct AstStmt : public AstNode {};

struct AstExpr : public AstNode {};

struct AstNoopStmt : public AstStmt {};

struct AstNoopExpr : public AstExpr {};

// Program ::= Block
struct AstProgram : public AstStmt {
  struct AstBlock* block;
};

// Block ::= {Statement} [ReturnStmt]
struct AstBlock : public AstStmt {
  std::vector<AstStmt*> statements;
};

struct AstDoBlock : public AstStmt {};

// TODO: Remove this in favor of a function call
struct AstPrintStmt : public AstStmt {
  AstNode* expr;
};

// VarDecl ::= 'val' IdList ':' (UnionType ['=' ExprList] | '=' ExprList)
struct AstVarDeclStmt : public AstStmt {
  // Either val or var.
  TokenType decltoken;
  Token name;
  AstNode* opttype;
  AstNode* expr;
};

// ExpressionStmt ::= AssignmentExpr | PrefixExpr
struct AstExpressionStmt : public AstStmt {
  AstExpr* expr;
};

// AssignmentExpr ::= VarList '=' ExprList
struct AstAssignmentExpr : public AstExpr {
  AstNode* left;
  AstNode* right;
};

// AssignmentExpr ::= VarList OP ExprList
struct AstInPlaceBinaryExpr : public AstExpr {
  TokenType op;
  TokenType binop;
  AstNode* left;
  AstNode* right;
};

// Expr ::= UnaryOp Expr
struct AstUnaryExp : public AstExpr {
  TokenType op;

  AstNode* expr;
};

// Expr ::= Expr BinaryOp Expr
struct AstBinaryExp : public AstExpr {
  TokenType op;

  AstNode* left;
  AstNode* right;
};

// Primary ::= 'nil'
//     | 'false'
//     | 'true'
//     | Number
//     | String
struct AstPrimaryExp : public AstExpr {
  TokenType type;
  Value value;
};

// ArrayValue ::= '[' Expr {',' Expr} ']'
struct AstArrayValueExpr : public AstExpr {
  std::vector<AstExpr*> values;
};

// Var ::= Id | PrefixExpr '[' Expr ']' | PrefixExpr '.' Id
struct AstVarExpr : public AstExpr {
  AstExpr* expr;
};

// Var ::= Id
struct AstIdExpr : public AstExpr {
  Token name;
};

// Var ::= PrefixExpr '[' Expr ']'
struct AstIndexExpr : public AstExpr {
  AstNode* prefix;
  AstNode* index;
};

// Var ::= PrefixExpr '.' Id
struct AstDotExpr : public AstExpr {
  AstNode* prefix;
  Token id;
};

// ReturnStmt ::= 'return' [Expr {',' Expr}]
struct AstReturnStmt : public AstStmt {
  AstNode* expr;
};

// IfStmt ::= 'if' Expr 'then' Block
//   {'elif' Expr 'then' Block}
//   ['else' Block]
// 'end'
struct AstIfStmt : public AstStmt {
  AstExpr* conditionexpr;
  AstStmt* ifstmt;
  std::vector<AstExpr*> elifexprs;
  std::vector<AstStmt*> elifstmts;
  AstStmt* elsestmt;
};

// WhileStmt ::= 'while' Expr 'do' Block 'end'
struct AstWhileStmt : public AstStmt {
  AstNode* conditionexpr;
  AstNode* blockstmt;
};

// ForStmt ::= 'for' [VarDecl | ExpressionStmt] ';' [Expr] ';' [Expr] 'do' Block 'end'
struct AstForStmt : public AstStmt {
  AstNode* optvardecl;
  AstNode* optconditionexpr;
  AstNode* optstepexpr;
  AstNode* blockstmt;
};

// AssertStmt ::= 'assert' Expr
struct AstAssertStmt : public AstStmt {
  AstNode* expr;
};

// TypeMemberDecl ::= [Id ':'] Type {'|' Type}
struct AstTypeMemberDecl : public AstStmt {
  Token optname;
  AstNode* type;
};

// ::= 'type' Id [GenericParams] {TypeMemberDecl ','} [TypeMemberDecl] 'end'
struct AstTypeDef : public AstStmt {
  Token name;
  std::vector<AstStmt*> genericparams;
  AstNode* type;
};

// FunctionParam ::= (['in' | 'out'] Id [':' UnionType])
struct AstFunctionParam : public AstStmt {
  TokenType kind;
  Token name;
  AstNode* type;
};

// FunctionDef ::= 'function' [Id] [GenericParams] '(' [FunctionParamList] [',' '...'] ')' ['->' UnionType] Statement 'end'
struct AstFunctionDef : public AstStmt {
  Token name;
  std::vector<AstFunctionParam*> functionparams;
  AstNode* returntype;
  AstNode* body;
};

// FunctionCallArgs ::= '(' { FunctionCallArg } ')'
struct AstFunctionCallArgs : public AstStmt {
  std::vector<AstNode*> args;
};

// FunctionCall ::= PrefixExpr '(' [ExprList] ')'
struct AstFunctionCall : public AstExpr {
  AstExpr* prefix;
  AstNode* args;
};

// ClassMemberDecl ::= IdList ':' UnionType ['=' ExprList]
struct AstClassMemberDecl : public AstStmt {
  Token name;
  AstNode* fieldtype;
  AstNode* optexpr;
};

// ClassDef ::= 'struct' Id [GenericParams] {ClassMemberDecl} 'end'
struct AstClassDef : public AstStmt {
  Token name;
  std::vector<AstNode*> genericparams;
  std::vector<AstClassMemberDecl*> members;
};

// ClassConstructorParam ::= [Id '='] Expr
struct AstClassConstructorParam : public AstExpr {
  Token name;
  AstExpr* expr;
};

// ClassConstructor ::= [Id] '{' [ClassConstructorParamList] [',' ClassConstructorNamedParamList] '}'
struct AstClassConstructor : public AstExpr {
  Token name;
  AstNode* prefix;
  std::vector<AstClassConstructorParam*> members;
};

struct AstRangeExpr : public AstExpr {
  AstNode* left;
  AstNode* right;
};

struct AstIndexOrGenericArgs : public AstExpr {};

struct AstVarOrTypeExpr : public AstExpr {
  AstExpr* prefix;
  AstIndexOrGenericArgs* index_args;
};

struct AstType : public AstExpr {};

struct Ast {
  struct AstProgram* program;
};

#endif  // AST_NODE__H