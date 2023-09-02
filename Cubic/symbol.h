#ifndef SYMBOL__H
#define SYMBOL__H

#include "common.h"
#include "type.h"
#include "memory.h"
#include "tokens.h"
#include "value.h"
#include "object.h"
#include "tac.h"

typedef struct Value_ Value_;

typedef enum {
  // A symbol that is a value or has a reference on the stack.
  SYMBOL_TYPE_VAR,

  // A symbol holding return and parameter typing.
  SYMBOL_TYPE_FN,

  // A symbol holding member typing.
  SYMBOL_TYPE_CLASS,

  // A field within a struct.
  SYMBOL_TYPE_FIELD,

  SYMBOL_TYPE_CLOSURE,

  // A symbol that lives on the stack temporarily during an expression
  // evaluation. 
  SYMBOL_TYPE_TMP,
} SymbolType_;

typedef struct SemanticType_ {
  // The pointer to the type.
  struct Type_* ty;

  // When to destroy the associated value.
  enum ValueLifetime lifetime;

  // If the type is user-defined like classes or functions, this will point
  // to the symbol holding more metadata.
  struct Symbol_* sym;
} SemanticType_;

#define MAKE_SEMANTIC_INFO(TYPE) ((SemanticType_){.info = (TYPE), .sym = NULL})

typedef struct ClassSymbol_ {
  ListOf_(Symbol_*) members;
  struct Symbol_* constructor;
} ClassSymbol_;

typedef struct FieldSymbol_ {
  // Name of the field.
  Token_ name;

  // Field index in the containing class.
  int index;

  // Byte frame_offset into the outer-most class.
  size_t offset;

  struct Value_ val;
  bool has_default_val;

  struct AstExpr_* opt_expr;

  struct Symbol_* cls_sym;
} FieldSymbol_;

// Any symbol living on the stack uses this as the base variable.
// If the symbol references another symbol, the SemanticType_::sym will be set.
typedef struct VarSymbol_ {
  SemanticType_ sem_type;

  // Incrementing index from 0 in the frame stack.
  int frame_index;

  // Incrementing index from 0 in local scope.  
  int scope_index;

  Location_ location;
} VarSymbol_;

typedef struct RefSymbol_ {
  struct Symbol_* var;
} RefSymbol_;

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

typedef struct TmpSymbol_ {
  // Incrementing index from 0 in local scope.  
  int tmp_index;
} TmpSymbol_;

typedef struct ArraySymbol_ {
  int a;
} ArraySymbol_;

typedef struct Symbol_ {
  SymbolType_ type;
  Token_ name;
  struct Scope_* parent;
  uint64_t uid;
  Type_* ty;

  union {
    ClassSymbol_ cls;
    VarSymbol_ var;
    FunctionSymbol_ fn;
    ClosureSymbol_ closure;
    TmpSymbol_ tmp;
    RefSymbol_ ref;
    FieldSymbol_ field;
  };  
} Symbol_;

Symbol_* symbol_ascallable(Symbol_* sym);
Symbol_* symbol_resolveref(Symbol_* sym);

bool semantictype_iscoercible(SemanticType_ from, SemanticType_ to);

RuntimeType_ semantictype_toruntime(SemanticType_ semantic_type);

Symbol_* symbol_findmember(const Symbol_* cls, Token_ name);
int symbol_findmember_index(const Symbol_* cls, Token_ name);
size_t symbol_findmember_offset(const Symbol_* cls, Token_ name);

extern SemanticType_ SemanticType_Unknown;
extern SemanticType_ SemanticType_Nil;

size_t semantictype_size(SemanticType_* type);
bool semantictype_hascycle(const SemanticType_* type);

inline static SemanticType_ semantictype_as(Type_* ty) {
  return (SemanticType_) {
    .ty = ty,
    .sym = NULL
  };
}

inline static SemanticType_ semantictype_frominfo(Type_* ty) {
  return (SemanticType_) {
    .ty = ty,
    .sym = NULL
  };
}

inline static SemanticType_ semantictype_tmp(Type_* ty) {
  return (SemanticType_) {
    .ty = ty,
    .lifetime = LIFETIME_TMP,
    .sym = NULL,
  };
}

inline static SemanticType_ semantictype_static(Type_* ty) {
  return (SemanticType_) {
    .ty = ty,
    .lifetime = LIFETIME_STATIC,
    .sym = NULL
  };
}

#endif  // SYMBOL__H