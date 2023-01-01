#include "symbol.h"

#include "object.h"

extern SemanticType_ SemanticType_Unknown = {
  .info = {
    .val = VAL_UNKNOWN,
    .kind = KIND_UNKNOWN,
    .obj = OBJ_TYPE_UNKNOWN,
  },
  .name = {0},
  .sym = NULL,
};

extern SemanticType_ SemanticType_Nil = {
  .info = {
    .val = VAL_NIL,
    .kind = KIND_VAL,
    .obj = OBJ_TYPE_UNKNOWN,
  },
  .name = {0},
  .sym = NULL,
};

FunctionSymbol_* symbol_ascallable(Symbol_* sym) {
  if (!sym) {
    return NULL;
  }

  switch (sym->info) {
    case SYMBOL_TYPE_FN:
      return &sym->fn;
    case SYMBOL_TYPE_CLOSURE:
      return &sym->closure.fn->fn;
    case SYMBOL_TYPE_VAR:
      return symbol_ascallable(sym->var.sem_type.sym);
  }

  return NULL;
}

Symbol_* symbol_resolveref(Symbol_* sym) {
  if (!sym || sym->info != SYMBOL_TYPE_VAR) {
    return NULL;
  }

  return sym->var.sem_type.sym;
}

RuntimeType_ semantictype_toruntime(SemanticType_ semantic_type) {
  return (RuntimeType_){
    .ty = semantic_type.info.val,
    .kind = semantic_type.info.kind,
    .obj = semantic_type.info.obj,
  };
}

bool semantictype_iscoercible(SemanticType_ from, SemanticType_ to) {
  return semantictype_equiv(from, to) ||
    // 64-bit conversion
    ((to.info.val == VAL_UINT || to.info.val == VAL_UINT64 || to.info.val == VAL_INT || to.info.val == VAL_INT64) &&
      (from.info.val >= VAL_INT && from.info.val <= VAL_UINT64)) ||

    // 8-bit conversion
    ((to.info.val == VAL_UINT8 || to.info.val == VAL_INT8) && (from.info.val == VAL_UINT8 || from.info.val == VAL_INT8)) ||

    // 16-bit conversion
    ((to.info.val == VAL_UINT16 || to.info.val == VAL_INT16) && (from.info.val == VAL_UINT8 || from.info.val == VAL_INT8 ||
      from.info.val == VAL_UINT16 || from.info.val == VAL_INT16)) ||

    // 32-bit conversion
    ((to.info.val == VAL_UINT32 || to.info.val == VAL_INT32) && (from.info.val == VAL_UINT8 || from.info.val == VAL_INT8 ||
      from.info.val == VAL_UINT16 || from.info.val == VAL_INT16 ||
      from.info.val == VAL_UINT32 || from.info.val == VAL_INT32)) ||

    // Double conversion
    (to.info.val == VAL_DOUBLE && (from.info.val >= VAL_INT && from.info.val <= VAL_DOUBLE)) ||

    // Float conversion
    (to.info.val == VAL_FLOAT && (from.info.val == VAL_FLOAT ||
      from.info.val >= VAL_INT8 && from.info.val <= VAL_INT32 ||
      from.info.val >= VAL_UINT8 && from.info.val <= VAL_UINT32));
}