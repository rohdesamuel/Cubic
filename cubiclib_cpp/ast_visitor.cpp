#include "ast_visitor.h"

void AstVisitor::Visit(Ast* ast) {
  Visit(ast->program);
}

void AstVisitor::Visit(AstNoopStmt* n) {}

void AstVisitor::Visit(AstNoopExpr* n) {}