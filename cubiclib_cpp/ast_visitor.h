#ifndef AST_VISITOR__H
#define AST_VISITOR__H

#include "ast_node.h"

class AstVisitor {
public:
  virtual void Visit(Ast* ast);
  virtual void Visit(AstNoopStmt* n);
  virtual void Visit(AstNoopExpr* n);

  virtual void Visit(AstProgram* n) = 0;
  virtual void Visit(AstBlock* n) = 0;
  virtual void Visit(AstPrintStmt* n) = 0;
  virtual void Visit(AstVarDeclStmt* n) = 0;
  virtual void Visit(AstExpressionStmt* n) = 0;
  virtual void Visit(AstAssignmentExpr* n) = 0;
  virtual void Visit(AstInPlaceBinaryExpr* n) = 0;
  virtual void Visit(AstUnaryExp* n) = 0;
  virtual void Visit(AstBinaryExp* n) = 0;
  virtual void Visit(AstPrimaryExp* n) = 0;
  virtual void Visit(AstArrayValueExpr* n) = 0;
  virtual void Visit(AstVarExpr* n) = 0;
  virtual void Visit(AstIdExpr* n) = 0;
  virtual void Visit(AstIndexExpr* n) = 0;
  virtual void Visit(AstDotExpr* n) = 0;
  virtual void Visit(AstReturnStmt* n) = 0;
  virtual void Visit(AstIfStmt* n) = 0;
  virtual void Visit(AstWhileStmt* n) = 0;
  virtual void Visit(AstForStmt* n) = 0;
  virtual void Visit(AstAssertStmt* n) = 0;
  virtual void Visit(AstTypeMemberDecl* n) = 0;
  virtual void Visit(AstTypeDef* n) = 0;
  virtual void Visit(AstFunctionParam* n) = 0;
  virtual void Visit(AstFunctionDef* n) = 0;
  virtual void Visit(AstFunctionCallArgs* n) = 0;
  virtual void Visit(AstFunctionCall* n) = 0;
  virtual void Visit(AstClassMemberDecl* n) = 0;
  virtual void Visit(AstClassDef* n) = 0;
  virtual void Visit(AstClassConstructorParam* n) = 0;
  virtual void Visit(AstClassConstructor* n) = 0;
  virtual void Visit(AstRangeExpr* n) = 0;
};

#endif  // AST_VISITOR__H
