#include "ast.h"

#include <string.h>

AstNode_* make_ast_node(MemoryAllocator_* allocator, int cls, size_t size, struct SymbolTable_* symbol_table) {
  AstNode_* ret = alloc(allocator, size);
  assertf(ret != NULL, "Cannot make AstNode: Allocator OOM");

  memset(ret, 0, size);
  ret->cls = cls;
  ret->symbol_table = symbol_table;
  return ret;
}