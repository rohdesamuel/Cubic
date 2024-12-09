#ifndef AST_INTERNAL__H
#define AST_INTERNAL__H

struct Type_* defer_type_resolution(struct AstParser_* parser, const struct TypeExpr_* type_expr, struct Scope_* scope, struct MemoryAllocator_* allocator);

#endif  // AST_INTERNAL__H