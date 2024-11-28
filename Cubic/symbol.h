#ifndef SYMBOL__H
#define SYMBOL__H

#include "common.h"
#include "type.h"
#include "type.h"
#include "memory.h"
#include "tokens.h"
#include "value.h"
#include "object.h"
#include "tac.h"

typedef struct Value_ Value_;

typedef enum {
  // A symbol that is a value or has a reference on the stack.
  SYMBOL_CLS_VAR,

  // A symbol holding return and parameter typing.
  SYMBOL_CLS_FN,

  // A symbol holding member typing.
  SYMBOL_CLS_CLASS,

  // A field within a struct.
  SYMBOL_CLS_FIELD,

  SYMBOL_CLS_CLOSURE,

  // A symbol that lives on the stack temporarily during an expression
  // evaluation. 
  SYMBOL_CLS_TMP,

  SYMBOL_CLS_TYPE,
} SymbolCls_;

typedef struct ClassSymbol_ {
  ListOf_(Symbol_*) members;
  struct Symbol_* constructor;
} ClassSymbol_;

// Any symbol living on the stack uses this as the base variable.
typedef struct VarSymbol_ {
  Location_ location;
} VarSymbol_;

typedef struct FunctionSymbol_ {
  ListOf_(Symbol_*) params;
  int64_t arg_size;

  Location_ loc;
} FunctionSymbol_;

typedef struct ClosureSymbol_ {
  struct Symbol_* fn;
  ListOf_(Symbol_*) closures;  

  int frame_index;
} ClosureSymbol_;

typedef struct ArraySymbol_ {
  int a;
} ArraySymbol_;

typedef struct TypeDefSymbol_ {
  int unused;
} TypeDefSymbol_;

typedef struct Symbol_ {
  SymbolCls_ type;

  Token_ name;
  struct Scope_* parent;
  const struct TypeExpr_* type_expr;

  Type_* ty;

  union {
    ClassSymbol_ cls;
    VarSymbol_ var;
    FunctionSymbol_ fn;
    ClosureSymbol_ closure;
    TypeDefSymbol_ type_def;
  };
} Symbol_;

#endif  // SYMBOL__H