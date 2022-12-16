#ifndef SYMBOL__H
#define SYMBOL__H

#include "common.h"
#include "type.h"
#include "memory.h"
#include "tokens.h"

typedef struct Type_ Type_;

typedef enum {
  SYMBOL_TYPE_STRUCT,
  SYMBOL_TYPE_VAR,
  SYMBOL_TYPE_FN,
  SYMBOL_TYPE_CLOSURE,
} SymbolType_;

typedef struct StructSymbol_ {
  int a;
} StructSymbol_;

typedef struct VarSymbol_ {
  Type_ type;

  // Stack offset into parent frame's stack.
  int offset;

  // Incrementing index from 0 in local scope.
  int index;
} VarSymbol_;

typedef struct FunctionSymbol_ {
  ListOf_(Symbol_*) params;
  Type_ return_type;
} FunctionSymbol_;

typedef struct ClosureSymbol_ {
  struct Symbol_* fn;
  ListOf_(Symbol_*) closures;
} ClosureSymbol_;

typedef struct Symbol_ {
  SymbolType_ type;
  union {
    StructSymbol_ strct;
    VarSymbol_ var;
    FunctionSymbol_ fn;
    ClosureSymbol_ closure;
  };

  Token_ name;
  struct Scope_* parent;
} Symbol_;

#endif  // SYMBOL__H