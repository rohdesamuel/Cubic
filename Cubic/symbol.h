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
  SYMBOL_TYPE_TMP,
} SymbolType_;

typedef struct SemanticInfo_ {
  Type_ type;
  struct Symbol_* sym;
} SemanticInfo_;

#define MAKE_SEMANTIC_INFO(TYPE) ((SemanticInfo_){.type = (TYPE), .sym = NULL})

typedef struct StructSymbol_ {
  int a;
} StructSymbol_;

typedef struct VarSymbol_ {
  SemanticInfo_ meta;

  // Incrementing index from 0 in the frame stack.
  int frame_index;

  // Incrementing index from 0 in local scope.  
  int scope_index;
} VarSymbol_;

typedef struct FunctionSymbol_ {
  ListOf_(Symbol_*) params;
  Type_ return_type;
  
  struct ObjFunction_* obj_fn;
} FunctionSymbol_;

typedef struct ClosureSymbol_ {
  struct Symbol_* fn;
  ListOf_(Symbol_*) closures;  

  int frame_index;
} ClosureSymbol_;

typedef struct TmpSymbol_ {
  // Incrementing index from 0 in local scope.  
  int tmp_index;
} TmpSymbol_;

typedef struct Symbol_ {
  SymbolType_ type;
  SemanticInfo_ info;

  union {
    StructSymbol_ strct;
    VarSymbol_ var;
    FunctionSymbol_ fn;
    ClosureSymbol_ closure;
    TmpSymbol_ tmp;
  };

  Token_ name;
  struct Scope_* parent;
} Symbol_;

FunctionSymbol_* symbol_ascallable(Symbol_* sym);

#endif  // SYMBOL__H