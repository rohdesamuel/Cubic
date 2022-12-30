#include "symbol.h"

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
      return symbol_ascallable(sym->var.meta.sym);
  }

  return NULL;
}