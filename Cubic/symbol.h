#ifndef SYMBOL__H
#define SYMBOL__H

#include "common.h"
#include "type.h"
#include "memory.h"
#include "tokens.h"

typedef struct Type_ Type_;

typedef enum {
  // A symbol that is a value or has a reference on the stack.
  SYMBOL_TYPE_VAR,

  // A symbol holding return and parameter typing.
  SYMBOL_TYPE_FN,

  // A symbol holding member typing.
  SYMBOL_TYPE_STRUCT,

  SYMBOL_TYPE_CLOSURE,

  // A symbol that lives on the stack temporarily during an expression
  // evaluation. 
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

// Any symbol living on the stack uses this as the base variable.
// If the symbol references another symbol, the SemanticInfo_::sym will be set.
typedef struct VarSymbol_ {
  SemanticInfo_ meta;

  // Incrementing index from 0 in the frame stack.
  int frame_index;

  // Incrementing index from 0 in local scope.  
  int scope_index;
} VarSymbol_;

typedef struct RefSymbol_ {
  struct Symbol_* var;
} RefSymbol_;

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

  union {
    StructSymbol_ strct;
    VarSymbol_ var;
    FunctionSymbol_ fn;
    ClosureSymbol_ closure;
    TmpSymbol_ tmp;
    // RefSymbol_ ref;
  };

  Token_ name;
  struct Scope_* parent;
} Symbol_;

FunctionSymbol_* symbol_ascallable(Symbol_* sym);
Symbol_* symbol_resolveref(Symbol_* sym);

#endif  // SYMBOL__H