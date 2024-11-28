#ifndef TYPE_PARSER__H
#define TYPE_PARSER__H

const struct TypeExpr_* parse_type(const struct CstNode_* node, struct MemoryAllocator_* allocator);

#endif  // TYPE_PARSER__H