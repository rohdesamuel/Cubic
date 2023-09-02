#include "symbol.h"

#include "object.h"
#include "map.h"

Symbol_* symbol_ascallable(Symbol_* sym) {
  if (!sym) {
    return NULL;
  }

  switch (sym->type) {
    case SYMBOL_TYPE_FN:
      return sym;
    case SYMBOL_TYPE_CLOSURE:
      return sym->closure.fn;
    case SYMBOL_TYPE_VAR:
      return symbol_ascallable(sym->var.sem_type.sym);
    case SYMBOL_TYPE_CLASS:
      return sym->cls.constructor;
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

Symbol_* symbol_findmember(const Symbol_* cls, Token_ name) {
  for (ListNode_* n = cls->cls.members.head; n != NULL; n = n->next) {
    Symbol_* field = list_val(n, Symbol_*);
    Token_ field_name = field->field.name;
    if (field_name.length == name.length && memcmp(field_name.start, name.start, field_name.length) == 0) {
      return field;
    }
  }

  return NULL;
}

int symbol_findmember_index(const Symbol_* cls, Token_ name) {
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

size_t symbol_findmember_offset(const Symbol_* cls, Token_ name) {
  for (ListNode_* n = cls->cls.members.head; n != NULL; n = n->next) {
    Symbol_* field = list_val(n, Symbol_*);
    if (token_eq(field->name, name)) {
      return field->field.offset;
    }
  }

  return 0;
}