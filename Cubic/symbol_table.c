#include "symbol_table.h"
#include "memory.h"
#include "object.h"

#include <string.h>

Frame_* frame_root(struct MemoryAllocator_* allocator) {
  Frame_* ret = alloc(allocator, sizeof(Frame_));
  memset(ret, 0, sizeof(Frame_));

  ret->allocator = allocator;
  ret->scope = scope_create(ret, NULL, allocator);
  ret->fn_symbol = NULL;
  return ret;
}

Frame_* frame_create(Symbol_* fn_symbol, struct MemoryAllocator_* allocator) {
  Frame_* ret = alloc(allocator, sizeof(Frame_));
  memset(ret, 0, sizeof(Frame_));

  ret->allocator = allocator;
  ret->scope = scope_create(ret, NULL, allocator);
  ret->fn_symbol = fn_symbol;
  return ret;
}

void frame_destroy(Frame_** frame) {
  dealloc((*frame)->allocator, *frame);
  *frame = NULL;
}

Symbol_* frame_addparam(Frame_* frame, Token_* name) {
  Scope_* scope = frame->scope;
  SymbolTable_* table = scope->table;  
  Symbol_* s = scope_addvar(scope, name, UNKNOWN_TY);
  list_push(&frame->fn_symbol->fn.params, &s);

  return s;
}

Symbol_* frame_addvar(Frame_* frame, Token_* name, Scope_* scope) {
  Symbol_* s = scope_addvar(scope, name, UNKNOWN_TY);
  frame->var_count += 1;
  frame->max_stack_offset = max(frame->max_stack_offset, scope->offset + scope->table->vars.count);
  return s;
}

Scope_* scope_create(Frame_* frame, SymbolTable_* table, struct MemoryAllocator_* allocator) {
  Scope_* ret = alloc(allocator, sizeof(Scope_));
  memset(ret, 0, sizeof(Scope_));

  ret->allocator = allocator;
  ret->frame = frame;
  if (table) {
    ret->table = symboltable_createfrom(table);
  } else {
    ret->table = symboltable_create(allocator);
  }

  list_of(&ret->children, Scope_*, allocator);

  return ret;
}

Scope_* scope_createfrom(Scope_* scope) {
  Scope_* ret = scope_create(scope->frame, scope->table, scope->allocator);
  ret->parent = scope;
  ret->offset = scope->offset + scope->table->vars.count;

  list_push(&scope->children, &ret);

  return ret;
}

void scope_destroy(Scope_** scope) {
  Scope_* to_destroy = *scope;
  struct MemoryAllocator_* allocator = to_destroy->allocator;

  for (ListNode_* n = to_destroy->children.head; n != NULL; n = n->next) {
    Scope_* child = list_val(n, Scope_*);
    scope_destroy(&child);
  }
  list_clear(&to_destroy->children);

  symboltable_destroy(&to_destroy->table);
  dealloc(allocator, to_destroy);
  *scope = NULL;
}

SymbolTable_* symboltable_create(struct MemoryAllocator_* allocator) {
  SymbolTable_* ret = alloc(allocator, sizeof(SymbolTable_));
  memset(ret, 0, sizeof(SymbolTable_));

  ret->symbols_ = hashmap_create();
  ret->allocator_ = allocator;
  list_init(&ret->vars, sizeof(Symbol_*), allocator);
  list_init(&ret->symbol_list_, sizeof(Symbol_), allocator);

  return ret;
}

SymbolTable_* symboltable_createfrom(SymbolTable_* parent) {
  SymbolTable_* ret = symboltable_create(parent->allocator_);
  ret->parent = parent;
  return ret;
}

void symboltable_destroy(SymbolTable_** symbol_table) {
  SymbolTable_* table = *symbol_table;
  
  list_clear(&table->vars);
  list_clear(&table->symbol_list_);
  hashmap_free(table->symbols_);
  dealloc(table->allocator_, table);
  *symbol_table = NULL;
}

void scope_add(Scope_* scope, Symbol_* symbol) {
  Symbol_* existing;
  SymbolTable_* table = scope->table;
  Token_* name = &symbol->name;
  if (hashmap_get(table->symbols_, name->start, name->length, (uintptr_t*)&existing)) {
    return;
  }

  list_push(&table->vars, &symbol);
  list_push(&table->symbol_list_, &symbol);
  hashmap_set(table->symbols_, name->start, name->length, (uintptr_t)symbol);
}

Symbol_* scope_addvar(Scope_* scope, Token_* name, Type_ type) {
  Symbol_* existing;
  SymbolTable_* table = scope->table;

  if (hashmap_get(table->symbols_, name->start, name->length, (uintptr_t*)&existing)) {
    return NULL;
  }

  Symbol_ s = (Symbol_){
    .type = SYMBOL_TYPE_VAR,
    .var = (VarSymbol_) {
      .type = type,
      .offset = scope->offset + table->vars.count,
      .index = table->vars.count,
    },
    .name = *name,
    .parent = scope,
  };

  ListNode_* n = list_push(&table->symbol_list_, &s);
  void* symbol_addr = &n->data;

  list_push(&table->vars, &symbol_addr);
  list_push(&table->symbol_list_, &symbol_addr);
  hashmap_set(table->symbols_, name->start, name->length, (uintptr_t)symbol_addr);
  return (Symbol_*)symbol_addr;
}

Symbol_* scope_addfn(Scope_* scope, Token_* name) {
  Symbol_* existing;
  SymbolTable_* table = scope->table;

  if (hashmap_get(table->symbols_, name->start, name->length, (uintptr_t*)&existing)) {
    return NULL;
  }

  Symbol_ s = (Symbol_){
    .type = SYMBOL_TYPE_FN,
    .fn = (FunctionSymbol_) {
      .return_type = VAL_UNKNOWN,
      .params = {0}
    },
    .name = *name,
    .parent = scope,
  };

  list_of(&s.fn.params, Symbol_*, scope->allocator);

  ListNode_* n = list_push(&table->symbol_list_, &s);
  void* symbol_addr = &n->data;

  list_push(&table->vars, &symbol_addr);
  list_push(&table->symbol_list_, &symbol_addr);
  hashmap_set(table->symbols_, name->start, name->length, (uintptr_t)symbol_addr);
  return (Symbol_*)symbol_addr;
}

Symbol_* scope_find(Scope_* scope, Token_* name) {
  Symbol_* ret = NULL;
  SymbolTable_* table = scope->table;
  if (!hashmap_get(table->symbols_, name->start, name->length, (uintptr_t*)&ret)) {
    if (!table->parent) {
      return NULL;
    }
    return scope_find(scope->parent, name);
  }

  return ret;
}

VarSymbol_* scope_var(Scope_* scope, Token_* name) {
  Symbol_* ret = scope_find(scope, name);
  if (!ret || ret->type != SYMBOL_TYPE_VAR) {
    return NULL;
  }

  return &ret->var;
}

FunctionSymbol_* scope_fn(Scope_* scope, Token_* name) {
  Symbol_* ret = scope_find(scope, name);
  if (!ret || ret->type != SYMBOL_TYPE_FN) {
    return NULL;
  }

  return &ret->fn;
}

Symbol_* symbol_closure(Symbol_* fn_symbol, Symbol_* upvalue, struct MemoryAllocator_* allocator) {
  Symbol_* ret = (Symbol_*)alloc(allocator, sizeof(Symbol_));
  memset(ret, 0, sizeof(Symbol_));

  *ret = (Symbol_){
    .type = SYMBOL_TYPE_CLOSURE,
    .closure = {
      .fn = fn_symbol,
      .closures = {0}
    }
  };

  list_of(&ret->closure.closures, Symbol_*, allocator);
  list_push(&ret->closure.closures, upvalue);

  return ret;
}

void closure_addto(ClosureSymbol_* closure, Symbol_* upvalue) {
  list_push(&closure->closures, upvalue);
}