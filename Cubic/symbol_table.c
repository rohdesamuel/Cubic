#include "symbol_table.h"
#include "memory.h"

#include <string.h>

SymbolTable_* symboltable_create(struct MemoryAllocator_* allocator) {
  SymbolTable_* ret = alloc(allocator, sizeof(SymbolTable_));
  memset(ret, 0, sizeof(SymbolTable_));

  ret->symbols = hashmap_create();
  ret->allocator = allocator;
  list_init(&ret->children, sizeof(SymbolTable_*), allocator);
  list_init(&ret->symbol_list, sizeof(Symbol_), allocator);

  return ret;
}

void symboltable_destroy(SymbolTable_** symbol_table) {
  SymbolTable_* table = *symbol_table;
  
  for (ListNode_* n = table->children.head; n != NULL; n = n->next) {
    SymbolTable_* child = list_val(n, SymbolTable_*);
    symboltable_destroy(&child);
  }
  list_clear(&table->children);
  list_clear(&table->symbol_list);
  hashmap_free(table->symbols);
  dealloc(table->allocator, table);
  *symbol_table = NULL;
}

void symboltable_link(SymbolTable_* table, SymbolTable_* parent) {
  assertf(table != parent, "Cannot have child scope and parent scope be the same.");
  table->parent = parent;
  list_push(&parent->children, &table);
}

Symbol_* symboltable_addvar(SymbolTable_* table, Token_* name, ValueType val_type, ValueKind kind) {
  Symbol_* ret = NULL;
  Symbol_* existing;
  if (hashmap_get(table->symbols, name->start, name->length, (uintptr_t*)&existing)) {
    return NULL;
  }

  Symbol_ s = (Symbol_){
    .type = SYMBOL_TYPE_VAR,
    .var = (VarSymbol_) {
      .type = val_type,
      .kind = kind,
      .offset = 0,
    }
  };

  ListNode_* n = list_push(&table->symbol_list, &s);
  hashmap_set(table->symbols, name->start, name->length, (uintptr_t)&n->data);
  return ret;
}

Symbol_* symboltable_get(SymbolTable_* table, Token_* name) {
  Symbol_* ret = NULL;

  if (!hashmap_get(table->symbols, name->start, name->length, (uintptr_t*)&ret)) {
    if (!table->parent) {
      return NULL;
    }
    return symboltable_get(table->parent, name);
  }

  return ret;
}

VarSymbol_* symboltable_var(SymbolTable_* table, Token_* name) {
  Symbol_* ret = symboltable_get(table, name);
  if (!ret || ret->type != SYMBOL_TYPE_VAR) {
    return NULL;
  }

  return &ret->var;
}