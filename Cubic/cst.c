#include "cst.h"

#include "symbol_table.h"
#include <string.h>

typedef CstNode_* (*CopyFn)(MemoryAllocator_*, CstNode_*, Scope_*);
typedef struct CopyRule_ {
  CopyFn fn;
} CopyRule_;

static CopyRule_* get_rule(int info);

CstNode_* make_cst_node(MemoryAllocator_* allocator, int cls, size_t size, int line) {
  CstNode_* ret = alloc(allocator, size);
  assertf(ret != NULL, "Cannot make CstNode: Allocator OOM");

  memset(ret, 0, size);
  ret->cls = cls;
  ret->line = line;
  return ret;
}

CstNode_* assert_cstnode_is_(CstNode_* node, int cls) {
  assertf(node->cls == cls, "CstNode class does not match %d vs. %d", node->cls, cls);
  return node;
}
