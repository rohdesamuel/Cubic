#include "symbol_table.h"
#include "memory.h"
#include "object.h"
#include "native_fns.h"

#include <string.h>

static Frame_* frame_create(Symbol_* fn_symbol, struct MemoryAllocator_* allocator);
static Symbol_* frame_addclosure(Frame_* frame, Token_* name, Symbol_* fn);
static Symbol_* scope_addclosure(Scope_* table, Token_* name, Symbol_* fn);
static Symbol_* scope_add(Scope_* scope, Symbol_* symbol);
static Symbol_* scope_addvar(Scope_* scope, Token_* name, SemanticType_ type);
static Symbol_* scope_addfn(Scope_* scope, Token_* name);
static Symbol_* scope_addstruct(Scope_* scope, Token_* name);

Frame_* frame_root(struct MemoryAllocator_* allocator) {
  Token_ entry_name = {
    .type = TK_ID,
    .start = "main",
    .length = 4,
    .line = 0,
  };

  Symbol_* entry_fn = alloc_ty(allocator, Symbol_);
  *entry_fn = (Symbol_){
    .type = SYMBOL_TYPE_FN,
    .fn = (FunctionSymbol_) {
      .return_type = SemanticType_Unknown,
      .params = {0}
    },
    .name = entry_name,
    .parent = NULL,
  };

  return frame_create(entry_fn, allocator);
}

Frame_* frame_create(Symbol_* fn_symbol, struct MemoryAllocator_* allocator) {
  Frame_* ret = alloc(allocator, sizeof(Frame_));
  memset(ret, 0, sizeof(Frame_));
  ret->parent = NULL;
  ret->allocator = allocator;
  ret->scope = scope_create(ret, NULL, allocator);
  ret->fn_symbol = fn_symbol;
  ret->fn_closure = frame_addclosure(ret, &fn_symbol->name, fn_symbol);

  list_of(&ret->children, Frame_*, allocator);
  list_of(&ret->tmps, Symbol_*, allocator);
  return ret;
}

Frame_* frame_createfrom(Frame_* frame, Symbol_* fn_symbol) {
  Frame_* ret = frame_create(fn_symbol, frame->allocator);
  ret->parent = frame;
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

Symbol_* frame_addparam(Frame_* frame, Token_* name) {
  Scope_* scope = frame->scope;
  SymbolTable_* table = scope->table;  
  Symbol_* s = frame_addvar(frame, name, scope);
  list_push(&frame->fn_symbol->fn.params, &s);

  return s;
}

Symbol_* frame_addvar(Frame_* frame, Token_* name, Scope_* scope) {
  Symbol_* s = scope_addvar(scope, name, SemanticType_Unknown);
  frame->var_count += 1;
  frame->stack_size += 1;// max(frame->stack_size, scope->offset + scope->table->vars.count);  
  return s;
}

Symbol_* frame_addfn(Frame_* frame, Token_* name, Scope_* scope) {
  return scope_addfn(scope, name);
}

Symbol_* frame_addstruct(Frame_* frame, Token_* name, Scope_* scope) {
  return scope_addstruct(scope, name);
}

Symbol_* frame_addtmp(Frame_* frame, Scope_* scope) {
  MemoryAllocator_* allocator = frame->allocator;

  int tmp_index = frame->tmp_count++;

  Symbol_* ret = alloc(allocator, sizeof(Symbol_));
  *ret = (Symbol_){
    .type = SYMBOL_TYPE_TMP,
    .tmp = {
      .tmp_index = tmp_index,
    },
    .name = {0},
    .parent = scope
  };

  list_push(&frame->tmps, &ret);
  
  return ret;
}

void frame_enterscope(Frame_* frame, Scope_* scope) {
}

void frame_leavescope(Frame_* frame, Scope_* scope) {
  frame->max_stack_size = max(frame->max_stack_size, frame->stack_size);
  frame->max_var_count = max(frame->max_var_count, frame->var_count);  

  frame->stack_size -= scope->stack_size;
  frame->var_count -= scope->table->vars.count;
}

static void frame_cleartemps(Frame_* frame) {
  frame->max_tmp_count = max(frame->max_tmp_count, frame->tmp_count);
  //frame->tmp_count = 0;
  list_clear(&frame->tmps);
}

void frame_movetemps(Frame_* frame, List_* list) {
  for (ListNode_* n = frame->tmps.head; n != NULL; n = n->next) {
    Symbol_* s = list_val(n, Symbol_*);
    list_push(list, &s);
  }

  frame_cleartemps(frame);
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

Symbol_* scope_add(Scope_* scope, Symbol_* symbol) {
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

Symbol_* scope_addvar(Scope_* scope, Token_* name, SemanticType_ type) {
  int scope_index = scope->stack_size++;

  Symbol_ s = (Symbol_){
    .type = SYMBOL_TYPE_VAR,
    .var = (VarSymbol_) {
      .sem_type = type,
      .scope_index = scope_index,
      .frame_index = 0,
    },
    .name = *name,
    .parent = scope,
  };

  return scope_add(scope, &s);
}

Symbol_* scope_addfn(Scope_* scope, Token_* name) {
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
  return scope_add(scope, &s);
}

static Symbol_* scope_addclosure(Scope_* scope, Token_* name, Symbol_* fn) {
  scope->stack_size++;

  Symbol_ s = {
    .type = SYMBOL_TYPE_CLOSURE,
    .closure = {
      .fn = fn,
      .closures = {0},
      .frame_index = 0
    },
    .name = *name,
    .parent = scope,
  };

  list_of(&s.closure.closures, Symbol_*, scope->allocator);

  return scope_add(scope, &s);
}

Symbol_* scope_addstruct(Scope_* scope, Token_* name) {
  Symbol_ s = (Symbol_){
    .type = SYMBOL_TYPE_STRUCT,
    .strct = {0},
    .name = *name,
    .parent = scope,
  };
  MemoryAllocator_* allocator = scope->allocator;
  s.strct.constructor = alloc_ty(allocator, Symbol_);
  
  list_of(&s.strct.constructor->fn.params, Symbol_*, allocator);
  list_of(&s.strct.members, Symbol_*, allocator);
  return scope_add(scope, &s);
}

Symbol_* structsymbol_addmember(Symbol_* sym, Token_ name, SemanticType_ type) {
  StructSymbol_* struct_sym = &sym->strct;
  MemoryAllocator_* allocator = sym->parent->allocator;

  Symbol_* field = alloc(allocator, sizeof(Symbol_));
  *field = (Symbol_){
    .type = SYMBOL_TYPE_FIELD,
    .field = {
      .sem_type = type,
      .index = struct_sym->members.count,
      .val = NIL_VAL,
    },
    .name = name,
    .parent = sym->parent,
  };

  list_push(&struct_sym->members, &field);

  
  struct_sym->constructor->fn.params;
  return field;
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

void closure_addto(ClosureSymbol_* closure, Symbol_* upvalue) {
  list_push(&closure->closures, upvalue);
}

int symbolvar_index(Symbol_* var) {
  return var->var.frame_index;
}

int symboltmp_index(Symbol_* tmp) {
  return tmp->tmp.tmp_index + tmp->parent->frame->max_var_count;
}