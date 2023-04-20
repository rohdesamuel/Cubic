#ifndef TAC__H
#define TAC__H

#include "common.h"
#include "chunk.h"

typedef struct Location_ {
  int32_t index;
  Token_ token;
} Location_;

typedef enum {
  TAC_ARG_NIL,
  TAC_ARG_LOC,
  TAC_ARG_VAL,
} OperandType_;

typedef struct Operand_ {
  OperandType_ arg_type;
  RuntimeType_ opt_type;
  union {
    Location_ loc;
    Value_ val;
  };
} Operand_;

typedef struct Tac_ {
  OpCode op;
  Location_ dst;

  Operand_ arg_l;
  Operand_ arg_r;

  int line;
  const char* label;
} Tac_;

inline bool is_loc_empty(Location_ addr) {
  return addr.index == -1;
}

#endif  // TAC__H
