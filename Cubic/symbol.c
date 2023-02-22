#include "symbol.h"

#include "object.h"
#include "map.h"

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

  switch (sym->type) {
    case SYMBOL_TYPE_FN:
      return &sym->fn;
    case SYMBOL_TYPE_CLOSURE:
      return &sym->closure.fn->fn;
    case SYMBOL_TYPE_VAR:
      return symbol_ascallable(sym->var.sem_type.sym);
    case SYMBOL_TYPE_CLASS:
      return &sym->cls.constructor->fn;
    default:
      assertf(false, "Unhandled symbol type %d in `symbol_ascallable`", sym->type);
  }

  return NULL;
}

Symbol_* symbol_resolveref(Symbol_* sym) {
  if (!sym || sym->type != SYMBOL_TYPE_VAR) {
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

Symbol_* symbol_findmember(Symbol_* cls, Token_ name) {
  for (ListNode_* n = cls->cls.members.head; n != NULL; n = n->next) {
    Symbol_* field = list_val(n, Symbol_*);
    Token_ field_name = field->field.sem_type.name;
    if (field_name.length == name.length && memcmp(field_name.start, name.start, field_name.length) == 0) {
      return field;
    }
  }

  return NULL;
}

int symbol_findmember_index(Symbol_* cls, Token_ name) {
  int index = 0;
  for (ListNode_* n = cls->cls.members.head; n != NULL; n = n->next) {
    Symbol_* field = list_val(n, Symbol_*);
    if (token_eq(field->name, name)) {
      return index;
    }
    ++index;
  }

  return index;
}

static bool semantictype_hascycle_recur(const Symbol_* symbol, Hashmap* seen) {
  if (!symbol || symbol->type != SYMBOL_TYPE_CLASS) {
    return false;
  }

  if (hashmap_get_set(seen, symbol->name.start, symbol->name.length, (uintptr_t*)&symbol)) {
    return true;
  }

  const ClassSymbol_* cls_sym = &symbol->cls;
  for (ListNode_* n = cls_sym->members.head; n != NULL; n = n->next) {
    Symbol_* field = list_val(n, Symbol_*);
    if (field->field.sem_type.info.kind != KIND_VAL) {
      continue;
    }

    if (semantictype_hascycle_recur(field->field.sem_type.sym, seen)) {
      return true;
    }
  }

  return false;
}

bool semantictype_hascycle(const SemanticType_* type) {
  if (type->info.val != VAL_CLASS) {
    return false;
  }

  Hashmap* seen = hashmap_create();
  bool has_cycle = semantictype_hascycle_recur(type->info.sym, seen);
  hashmap_free(seen);

  return has_cycle;
}

static size_t semantictype_size_recur(SemanticType_* type) {
  if (type->info.size > 0) {
    return type->info.size;
  }

  ClassSymbol_* sym = &type->info.sym->cls;

  size_t ret = 0;
  for (ListNode_* n = sym->members.head; n != NULL; n = n->next) {
    FieldSymbol_* field = &list_val(n, Symbol_*)->field;
    ret += semantictype_size(&field->sem_type);
  }

  return ret;
}

size_t semantictype_size(SemanticType_* type) {
  if (type->info.val != VAL_CLASS) {
    return 1;
  }

  if (type->info.size <= 0) {
    type->info.size = semantictype_size_recur(type);
  }
  return type->info.size;
}