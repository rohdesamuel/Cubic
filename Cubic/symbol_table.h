#ifndef SYMBOL_TABLE__H
#define SYMBOL_TABLE__H

#include "common.h"
#include "map.h"
#include "memory.h"
#include "value.h"
#include "tokens.h"
#include "symbol.h"

// Frame -> Scope :: 1 -> *
// Scope -> Scope :: 1 -> *
// Scope -> Symbol Table :: 1 -> 1
typedef struct Scope_ {
  struct MemoryAllocator_* allocator;
  struct Scope_* parent;
  struct Frame_* frame;
  struct SymbolTable_* table;

  ListOf_(Scope_*) children;

  int stack_size;
} Scope_;

typedef struct Frame_ {
  struct MemoryAllocator_* allocator;
  Symbol_* fn_symbol;
  Symbol_* fn_closure;

  // The root scope of the function frame including parameters.
  // This scope doesn't have a parent to disallow implicit closures.
  struct Scope_* scope;

  ListOf_(Symbol_*) tmps;

  int var_count;
  int tmp_count;
  int stack_size;

  int max_var_count;
  int max_tmp_count;
  int max_stack_size;
} Frame_;

typedef struct SymbolTable_ {
  ListOf_(Symbol_*) vars;
  ListOf_(Symbol_*) fns;
  ListOf_(Symbol_*) structs;

  struct SymbolTable_* parent;
  ListOf_(Symbol_) symbol_list_;
  Hashmap* symbols_;
  struct MemoryAllocator_* allocator_;
} SymbolTable_;

Frame_* frame_root(struct MemoryAllocator_* allocator);
Frame_* frame_create(Symbol_* fn_symbol, struct MemoryAllocator_* allocator);
void frame_destroy(Frame_** frame);
Symbol_* frame_addparam(Frame_* frame, Token_* name);
Symbol_* frame_addvar(Frame_* frame, Token_* name, Scope_* scope);
Symbol_* frame_addtmp(Frame_* frame, Scope_* scope);
void frame_enterscope(Frame_* frame, Scope_* scope);
void frame_leavescope(Frame_* frame, Scope_* scope);
void frame_movetemps(Frame_* frame, List_* list);
void frame_closevar(Frame_* frame, Symbol_* sym);
Symbol_* frame_addclosure(Frame_* frame, Token_* name, Symbol_* fn);

Scope_* scope_create(Frame_* frame, SymbolTable_* table, struct MemoryAllocator_* allocator);
Scope_* scope_createfrom(Scope_* scope);
void scope_destroy(Scope_** scope);

SymbolTable_* symboltable_create(struct MemoryAllocator_* allocator);
SymbolTable_* symboltable_createfrom(SymbolTable_* parent);
void symboltable_destroy(SymbolTable_** symbol_table);

Symbol_* scope_add(Scope_* scope, Symbol_* symbol);
Symbol_* scope_addvar(Scope_* scope, Token_* name, Type_ type);
Symbol_* scope_addfn(Scope_* scope, Token_* name);

Symbol_* scope_find(Scope_* table, Token_* name);
VarSymbol_* scope_var(Scope_* table, Token_* name);
FunctionSymbol_* scope_fn(Scope_* table, Token_* name);
StructSymbol_* scope_struct(Scope_* table, Token_* name);


void closure_addto(ClosureSymbol_* closure, Symbol_* upvalue);

int symbolvar_index(Symbol_* var);
int symboltmp_index(Symbol_* tmp);

#endif  // SYMBOL_TABLE__H