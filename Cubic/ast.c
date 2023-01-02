#include "ast.h"

#include <string.h>

AstNode_* make_ast_node(MemoryAllocator_* allocator, int cls, size_t size, struct Scope_* scope, int line) {
  AstNode_* ret = alloc(allocator, size);
  assertf(ret != NULL, "Cannot make AstNode: Allocator OOM");

  memset(ret, 0, size);
  ret->cls = cls;
  ret->scope = scope;
  ret->line = line;
  return ret;
}

AstNode_* assert_astnode_is_(AstNode_* node, int cls) {
  assertf(node->cls == cls, "AstNode class does not match %d vs. %d", node->cls, cls);
  return node;
}