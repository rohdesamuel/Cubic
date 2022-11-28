#ifndef SYMBOL_TABLE__H
#define SYMBOL_TABLE__H

#include "common.h"
#include "map.h"
#include "memory.h"
#include "value.h"
#include "tokens.h"

typedef enum {
  SYMBOL_TYPE_STRUCT,
  SYMBOL_TYPE_VAR,
  SYMBOL_TYPE_FN
} SymbolType_;

typedef struct StructSymbol_ {
  int a;
} StructSymbol_;

typedef struct VarSymbol_ {
  ValueType type;
  ValueKind kind;

  int offset;
} VarSymbol_;

typedef struct FunctionSymbol_ {
  int a;
} FunctionSymbol_;

typedef struct Symbol_ {
  SymbolType_ type;
  union {
    StructSymbol_ strct;
    VarSymbol_ var;
    FunctionSymbol_ fn;
  };

  const char* name;
} Symbol_;

typedef struct SymbolTable_ {
  Hashmap* symbols;
  struct SymbolTable_* parent;
  struct MemoryAllocator_* allocator;

  List_ children;
  List_ symbol_list;
} SymbolTable_;

SymbolTable_* symboltable_create(struct MemoryAllocator_* allocator);
void symboltable_destroy(SymbolTable_** symbol_table);

void symboltable_link(SymbolTable_* table, SymbolTable_* parent);
Symbol_* symboltable_addvar(SymbolTable_* table, Token_* name, ValueType val_type, ValueKind kind);
Symbol_* symboltable_get(SymbolTable_* table, Token_* name);

VarSymbol_* symboltable_var(SymbolTable_* table, Token_* name);
FunctionSymbol_* symboltable_fn(SymbolTable_* table, Token_* name);
StructSymbol_* symboltable_struct(SymbolTable_* table, Token_* name);

#endif  // SYMBOL_TABLE__H