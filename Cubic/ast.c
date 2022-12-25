#include "ast.h"

#include <string.h>

AstNode_* make_ast_node(MemoryAllocator_* allocator, int cls, size_t size, struct Scope_* scope) {
  AstNode_* ret = alloc(allocator, size);
  assertf(ret != NULL, "Cannot make AstNode: Allocator OOM");

  memset(ret, 0, size);
  ret->cls = cls;
  ret->scope = scope;
  return ret;
}

AstStmt_* make_ast_stmt(MemoryAllocator_* allocator, int cls, size_t size, struct Scope_* scope) {
  AstStmt_* stmt = alloc(allocator, sizeof(AstStmt_));
  assertf(stmt != NULL, "Cannot make AstNode: Allocator OOM");

  memset(stmt, 0, size);
  stmt->base.cls = AST_CLS(AstStmt_);
  stmt->base.scope = scope;
  stmt->stmt = make_ast_node(allocator, cls, size, scope);
  stmt->cleanup = (AstNode_*)MAKE_AST_NODE(allocator, AstCleanUpTemps_, scope);
  return stmt;
}

AstNode_* make_ast_expr(MemoryAllocator_* allocator, int cls, size_t size, struct Scope_* scope) {
  AstNode_* ret = alloc(allocator, size);
  assertf(ret != NULL, "Cannot make AstNode: Allocator OOM");

  memset(ret, 0, size);
  ret->cls = cls;
  ret->scope = scope;
  return ret;
}