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
  // The type that is the final result of resolving the symbol.
  // E.g. return value of a function, the type that a pointer is addressing
  // to, the field of a given struct.
  enum ValueType val;

  // How to interpret the type, is it an address, a reference?
  enum ValueKind kind;

  enum ValueRefKind ref_kind;

  // When to destroy the associated value.
  enum ValueLifetime lifetime;

  // If the value is allocated on the heap, this type will be filled.
  enum ObjType obj;

  // The size of this type in memory.
  int64_t size;

  // If the type is user-defined, this is the name of the type.
  Token_ name;

  // If the type is user-defined like classes or functions, this will point
  // to the symbol holding more metadata.
  struct Symbol_* sym;
} SemanticType_;

#define MAKE_SEMANTIC_INFO(TYPE) ((SemanticType_){.info = (TYPE), .sym = NULL})

typedef struct ClassSymbol_ {
  ListOf_(Symbol_*) members;
  struct Symbol_* constructor;

  SemanticType_ self_type;
} ClassSymbol_;

typedef struct FieldSymbol_ {
  SemanticType_ sem_type;

  // Name of the field.
  Token_ name;

  // Field index in the containing class.
  int index;

  // Byte frame_offset into the outer-most class.
  size_t offset;

  struct Value_ val;
  bool has_default_val;

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
  SemanticType_ return_type;
  
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
  Token_ name;
  struct Scope_* parent;

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

FunctionSymbol_* symbol_ascallable(Symbol_* sym);
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

inline static bool semantictype_infoequal(SemanticType_ a, SemanticType_ b) {
  return a.val == b.val && a.kind == b.kind && a.obj == b.obj;
}

inline static SemanticType_ semantictype_empty(ValueType val) {
  return (SemanticType_) {
    .val = val,
      .kind = KIND_UNKNOWN,
      .obj = OBJ_TYPE_UNKNOWN,
      .size = 0,
      .name = { 0 },
      .sym = NULL
  };
}

inline static SemanticType_ semantictype_as(ValueType val) {
  return (SemanticType_) {
    .val = val,
    .kind = KIND_UNKNOWN,
    .obj = OBJ_TYPE_UNKNOWN,
    .name = { 0 },
    .sym = NULL
  };
}

inline static SemanticType_ semantictype_frominfo(ValueType val, ValueKind kind, ObjType obj) {
  return (SemanticType_) {
    .val = val,
    .kind = kind,
    .obj = obj,
    .name = { 0 },
    .sym = NULL
  };
}

inline static SemanticType_ semantictype_tmp(ValueType val) {
  return (SemanticType_) {
    .val = val,
    .kind = KIND_VAL,
    .lifetime = LIFETIME_TMP,
    .obj = OBJ_TYPE_UNKNOWN,
    .name = { 0 },
    .sym = NULL,
  };
}

inline static SemanticType_ semantictype_static(ValueType val) {
  return (SemanticType_) {
    .val = val,
    .kind = KIND_VAL,
    .lifetime = LIFETIME_STATIC,
    .obj = OBJ_TYPE_UNKNOWN,
    .name = { 0 },
    .sym = NULL
  };
}

// Returns true if value and object types are the same.
inline static bool semantictype_equiv(SemanticType_ from, SemanticType_ to) {
  return from.val == to.val && from.obj == to.obj;
}

inline static bool semantictype_isabool(SemanticType_ type) {
  return type.val == VAL_BOOL;
}

inline static bool semantictype_isaobj(SemanticType_ type) {
  return type.val == VAL_OBJ;
}

inline static bool semantictype_isobj(SemanticType_ type, enum ObjType obj_type) {
  return type.val == VAL_OBJ && type.obj == obj_type;
}

inline static bool semantictype_isastring(SemanticType_ type) {
  return semantictype_isobj(type, OBJ_TYPE_STRING);
}

inline static bool semantictype_isanumber(SemanticType_ type) {
  return type.val >= VAL_INT && type.val <= VAL_DOUBLE;
}

inline static bool semantictype_isainteger(SemanticType_ type) {
  return type.val >= VAL_INT && type.val <= VAL_UINT64;
}

inline static bool semantictype_isaint(SemanticType_ type) {
  return type.val >= VAL_INT && type.val <= VAL_INT64;
}

inline static bool semantictype_isauint(SemanticType_ type) {
  return type.val >= VAL_UINT && type.val <= VAL_UINT64;
}

inline static bool semantictype_isareal(SemanticType_ type) {
  return type.val == VAL_FLOAT || type.val == VAL_DOUBLE;
}

inline static bool semantictype_isunknown(SemanticType_ type) {
  return type.val == VAL_UNKNOWN;
}

inline static bool semantictype_isnil(SemanticType_ type) {
  return type.val == VAL_NIL;
}

#endif  // SYMBOL__H