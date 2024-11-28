#include "symbol_table.h"
#include "memory.h"
#include "object.h"
#include "native_fns.h"
#include "random.h"

#include <string.h>

static Frame_* frame_create(Type_* fn_type, struct MemoryAllocator_* allocator);
static Symbol_* frame_addclosure(Frame_* frame, Token_* name, Symbol_* fn);
static Symbol_* scope_addclosure(Scope_* table, Token_* name, Symbol_* fn);
static Symbol_* scope_addvar(Scope_* scope, Token_* name, Type_* type);
static Symbol_* scope_addfn(Scope_* scope, Token_* name, Type_* type);
static Symbol_* scope_addclass(Scope_* scope, Type_* cls_ty);
static Symbol_* scope_addtype(Scope_* scope, Token_* name, Type_* type);

Frame_* frame_root(struct MemoryAllocator_* allocator) {
  Token_ entry_name = {
    .type = TK_ID,
    .start = "main",
    .length = 4,
    .line = 0,
  };

  return frame_create(make_function_ty(entry_name, NULL, NULL, NULL, NULL, allocator), allocator);
}

Frame_* frame_create(Type_* fn_type, struct MemoryAllocator_* allocator) {
  Frame_* ret = alloc(allocator, sizeof(Frame_));
  memset(ret, 0, sizeof(Frame_));
  ret->parent = NULL;
  ret->allocator = allocator;
  ret->scope = scope_create(ret, NULL, allocator);
  ret->fn_symbol = frame_addfn(ret, &fn_type->opt_name, fn_type, ret->scope);
//  ret->fn_closure = scope_add(ret->scope, fn_symbol);// frame_addclosure(ret, &fn_symbol->name, fn_symbol);
  
  list_of(&ret->children, Frame_*, allocator);
  list_of(&ret->tmps, Symbol_*, allocator);
  return ret;
}

Frame_* frame_createfrom(Frame_* frame, Scope_* prev_scope, Type_* fn_type) {
  Frame_* ret = frame_create(fn_type, frame->allocator);
  ret->parent = frame;
  ret->scope->prev = prev_scope;
  list_push(&frame->children, &ret);

  return ret;
}

void frame_destroy(Frame_** frame) {
  Frame_* to_destroy = *frame;
  struct MemoryAllocator_* allocator = to_destroy->allocator;

  for (ListNode_* n = to_destroy->children.head; n != NULL; n = n->next) {
    Frame_* child = list_val(n, Frame_*);
    frame_destroy(&child);
  }

  dealloc((*frame)->allocator, *frame);
  *frame = NULL;
}

Symbol_* frame_addparam(Frame_* frame, Token_* name, Type_* type) {
  Scope_* scope = frame->scope;
  SymbolTable_* table = scope->table;  
  Symbol_* s = frame_addvar(frame, name, type, scope);
  list_push(&frame->fn_symbol->fn.params, &s);

  return s;
}

Symbol_* frame_addvar(Frame_* frame, Token_* name, Type_* type, Scope_* scope) {
  Symbol_* s = scope_addvar(scope, name, type);
  frame->var_count += 1;
  frame->stack_size += 1;// max(frame->stack_size, scope->frame_offset + scope->table->vars.count);  
  return s;
}

Symbol_* frame_addfn(Frame_* frame, Token_* name, Type_* fn_ty, Scope_* scope) {
  return scope_addfn(scope, name, fn_ty);
}

Symbol_* frame_addclass(Frame_* frame, Type_* cls_ty, Scope_* scope) {
  return scope_addclass(scope, cls_ty);
}

Symbol_* frame_addtype(Frame_* frame, Token_* name, Type_* type, Scope_* scope) {
  return scope_addtype(scope, name, type);
}

void frame_enterscope(Frame_* frame, Scope_* scope) {
}

void frame_leavescope(Frame_* frame, Scope_* scope) {
  frame->max_stack_size = max(frame->max_stack_size, frame->stack_size);
  frame->max_var_count = max(frame->max_var_count, frame->var_count);  

  frame->stack_size -= scope->stack_size;
  frame->var_count -= scope->table->vars.count;
}

static Symbol_* frame_addclosure(Frame_* frame, Token_* name, Symbol_* fn) {
  Symbol_* ret = scope_addclosure(frame->scope, name, fn);
  frame->var_count += 1;
  frame->stack_size += 1;

  return ret;
}

void frame_closevar(Frame_* frame, Symbol_* sym) {
  
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
  ret->prev = scope;
  
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

  if (to_destroy->parent) {
    Scope_* parent = to_destroy->parent;
    assertf(list_remove(&parent->children, (void*)&to_destroy), "");
  }

  dealloc(allocator, to_destroy);
  **scope = (Scope_){ 0 };
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

Symbol_* scope_addnew(Scope_* scope, Symbol_* symbol) {
  Symbol_* existing;
  SymbolTable_* table = scope->table;
  Token_* name = &symbol->name;
  if (hashmap_get(table->symbols_, name->start, name->length, (uintptr_t*)&existing)) {
    return NULL;
  }

  ListNode_* n = list_push(&table->symbol_list_, symbol);
  void* symbol_addr = &n->data;

  list_push(&table->vars, &symbol_addr);
  list_push(&table->symbol_list_, &symbol_addr);
  hashmap_set(table->symbols_, name->start, name->length, (uintptr_t)symbol_addr);

  return (Symbol_*)symbol_addr;
}

bool scope_addexisting(Scope_* scope, Symbol_* symbol) {
  Symbol_* existing;
  SymbolTable_* table = scope->table;
  Token_* name = &symbol->name;
  if (hashmap_get(table->symbols_, name->start, name->length, (uintptr_t*)&existing)) {
    return false;
  }

  hashmap_set(table->symbols_, name->start, name->length, (uintptr_t)symbol);
  return true;
}

Symbol_* scope_addvar(Scope_* scope, Token_* name, Type_* type) {
  int scope_index = scope->stack_size++;

  Symbol_ s = (Symbol_){
    .type = SYMBOL_CLS_VAR,
    .var = (VarSymbol_) {0},
    .name = *name,
    .parent = scope,
    .ty = type
  };
  return scope_addnew(scope, &s);
}

Symbol_* scope_addfn(Scope_* scope, Token_* name, Type_* ty) {
  Symbol_ s = (Symbol_){
    .type = SYMBOL_CLS_FN,
    .fn = (FunctionSymbol_) {
      .params = {0}
    },
    .name = *name,
    .parent = scope,
    .ty = ty
  };
  list_of(&s.fn.params, Symbol_*, scope->allocator);
  return scope_addnew(scope, &s);
}

static Symbol_* scope_addclosure(Scope_* scope, Token_* name, Symbol_* fn) {
  scope->stack_size++;

  Symbol_ s = {
    .type = SYMBOL_CLS_CLOSURE,
    .ty = fn->ty,
    .closure = {
      .fn = fn,
      .closures = {0},
      .frame_index = 0
    },
    .name = *name,
    .parent = scope,
  };

  list_of(&s.closure.closures, Symbol_*, scope->allocator);
  return scope_addnew(scope, &s);
}

Symbol_* scope_addclass(Scope_* scope, Type_* cls_ty) {
  MemoryAllocator_* allocator = scope->allocator;
  Symbol_ s = (Symbol_){
    .type = SYMBOL_CLS_CLASS,
    .cls = {0},
    .name = cls_ty->opt_name,
    .parent = scope,
    .ty = cls_ty
  };
  s.cls.constructor = alloc_ty(allocator, Symbol_);
  *s.cls.constructor = (Symbol_){
    .type = SYMBOL_CLS_FN,
  };
  
  list_of(&s.cls.constructor->fn.params, Symbol_*, allocator);
  list_of(&s.cls.members, Symbol_*, allocator);
  return scope_addnew(scope, &s);
}

Symbol_* scope_addtype(Scope_* scope, Token_* name, Type_* type) {
  int scope_index = scope->stack_size++;

  Symbol_ s = (Symbol_){
    .type = SYMBOL_CLS_TYPE,
    .type_def = (TypeDefSymbol_) {0},
    .name = *name,
    .parent = scope,
    .ty = type
  };

  return scope_addnew(scope, &s);
}

Symbol_* classsymbol_addmember(Symbol_* sym, Token_ name, Type_* type) {
  ClassSymbol_* cls_sym = &sym->cls;
  MemoryAllocator_* allocator = sym->parent->allocator;

  Symbol_* field = alloc(allocator, sizeof(Symbol_));
  *field = (Symbol_){
    .type = SYMBOL_CLS_FIELD,
    .name = name,
    .parent = sym->parent,
    .ty = type
  };

  list_push(&cls_sym->members, &field);

  
  cls_sym->constructor->fn.params;
  return field;
}

Symbol_* scope_find(Scope_* scope, const Token_* name) {
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

Symbol_* scope_search_to_root(Scope_* scope, const Token_* name) {
  if (!name || !name->start) {
    return NULL;
  }

  Symbol_* ret = NULL;
  SymbolTable_* table = scope->table;
  if (!hashmap_get(table->symbols_, name->start, name->length, (uintptr_t*)&ret)) {
    if (!scope->prev) {
      return NULL;
    }
    return scope_search_to_root(scope->prev, name);
  }

  return ret;
}

VarSymbol_* scope_var(Scope_* scope, Token_* name) {
  Symbol_* ret = scope_find(scope, name);
  if (!ret || ret->type != SYMBOL_CLS_VAR) {
    return NULL;
  }

  return &ret->var;
}

FunctionSymbol_* scope_fn(Scope_* scope, Token_* name) {
  Symbol_* ret = scope_find(scope, name);
  if (!ret || ret->type != SYMBOL_CLS_FN) {
    return NULL;
  }

  return &ret->fn;
}

void closure_addto(ClosureSymbol_* closure, Symbol_* upvalue) {
  list_push(&closure->closures, upvalue);
}