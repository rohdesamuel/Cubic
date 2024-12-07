#ifndef SYMBOL_TABLE__H
#define SYMBOL_TABLE__H

#include "common.h"
#include "map.h"
#include "memory.h"
#include "value.h"
#include "tokens.h"
#include "symbol.h"

// Frame -> Frame :: 1 -> *
// Frame <- Scope :: 1 -> *
// Scope <- Scope :: 1 -> *
// Scope -> Symbol Table :: 1 -> 1
typedef struct Scope_ {
  struct MemoryAllocator_* allocator;
  // The parent scope that this scope belongs to. Roots are created at
  // function definitions and program node. If this is a root, the parent is
  // NULL.
  struct Scope_* parent;
  // The scope that this scope belongs to. This links between roots and will
  // only be NULL if the node is the program root.
  struct Scope_* prev;
  struct Frame_* frame;
  struct SymbolTable_* table;

  ListOf_(Scope_*) children;

  int stack_size;
} Scope_;

typedef struct Frame_ {
  struct MemoryAllocator_* allocator;
  struct Frame_* parent;
  ListOf_(Frame_*) children;

  Symbol_* fn_symbol;
  Symbol_* fn_closure;

  // The root scope of the function frame including parameters.
  // This scope doesn't have a parent to disallow implicit closures.
  struct Scope_* scope;

  ListOf_(Symbol_*) tmps;

  int var_count;
  int stack_size;
} Frame_;

typedef struct SymbolTable_ {
  ListOf_(Symbol_*) vars;
  ListOf_(Symbol_*) fns;
  ListOf_(Symbol_*) classes;

  struct SymbolTable_* parent;
  ListOf_(Symbol_) symbol_list_;
  Hashmap* symbols_;
  struct MemoryAllocator_* allocator_;
} SymbolTable_;

Frame_* frame_root(struct MemoryAllocator_* allocator);
Frame_* frame_createfrom(Frame_* frame, Scope_* prev_scope, Type_* fn_type);
void frame_destroy(Frame_** frame);
Symbol_* frame_addparam(Frame_* frame, Token_* name, Type_* type);
Symbol_* frame_addvar(Frame_* frame, Token_* name, Type_* type, Scope_* scope);
Symbol_* frame_addclass(Frame_* frame, Token_* name, Type_* cls_ty, Scope_* scope);
Symbol_* frame_addtype(Frame_* frame, Token_* name, Type_* type, Scope_* scope);

void frame_enterscope(Frame_* frame, Scope_* scope);
void frame_leavescope(Frame_* frame, Scope_* scope);
void frame_closevar(Frame_* frame, Symbol_* sym);
Symbol_* frame_addclosure(Frame_* frame, Token_* name, Symbol_* fn);

Scope_* scope_create(Frame_* frame, SymbolTable_* table, struct MemoryAllocator_* allocator);
Scope_* scope_createfrom(Scope_* scope);
void scope_destroy(Scope_** scope);

SymbolTable_* symboltable_create(struct MemoryAllocator_* allocator);
SymbolTable_* symboltable_createfrom(SymbolTable_* parent);
void symboltable_destroy(SymbolTable_** symbol_table);

Symbol_* scope_find(Scope_* scope, const Token_* name);
Symbol_* scope_search_to_root(Scope_* scope, const Token_* name);
VarSymbol_* scope_var(Scope_* scope, Token_* name);
FunctionSymbol_* scope_fn(Scope_* scope, Token_* name);
Symbol_* scope_addnew(Scope_* scope, Symbol_* symbol);
bool scope_addexisting(Scope_* scope, Symbol_* symbol);
bool scope_addplaceholder(Scope_* scope, const Token_* name);

void closure_addto(ClosureSymbol_* closure, Symbol_* upvalue);

Symbol_* classsymbol_addmember(Symbol_* sym, Token_ name, Type_* type);

#endif  // SYMBOL_TABLE__H