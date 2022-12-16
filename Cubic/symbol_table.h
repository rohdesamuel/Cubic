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

  int offset;
} Scope_;

typedef struct Frame_ {
  struct MemoryAllocator_* allocator;
  Symbol_* fn_symbol;

  // The root scope of the function frame including parameters.
  // This scope doesn't have a parent to disallow implicit closures.
  struct Scope_* scope;

  int var_count;
  int max_stack_offset;
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

Scope_* scope_create(Frame_* frame, SymbolTable_* table, struct MemoryAllocator_* allocator);
Scope_* scope_createfrom(Scope_* scope);
void scope_destroy(Scope_** scope);

SymbolTable_* symboltable_create(struct MemoryAllocator_* allocator);
SymbolTable_* symboltable_createfrom(SymbolTable_* parent);
void symboltable_destroy(SymbolTable_** symbol_table);

void scope_add(Scope_* scope, Symbol_* symbol);
Symbol_* scope_addvar(Scope_* scope, Token_* name, Type_ type);
Symbol_* scope_addfn(Scope_* scope, Token_* name);
Symbol_* scope_addclosure(Scope_* table, Token_* name, Scope_* parent);
Symbol_* scope_find(Scope_* table, Token_* name);

VarSymbol_* scope_var(Scope_* table, Token_* name);
FunctionSymbol_* scope_fn(Scope_* table, Token_* name);
StructSymbol_* scope_struct(Scope_* table, Token_* name);

Symbol_* symbol_closure(Symbol_* fn_symbol, Symbol_* upvalue, struct MemoryAllocator_* allocator);
void closure_addto(ClosureSymbol_* closure, Symbol_* upvalue);

#endif  // SYMBOL_TABLE__H