#ifndef TAC__H
#define TAC__H

#include "common.h"
#include "chunk.h"

typedef enum {
  LOCATION_TYPE_EMPTY,
  LOCATION_TYPE_VAL,
  LOCATION_TYPE_VAR,
  LOCATION_TYPE_PTR,
  LOCATION_TYPE_LABEL,
} LocationType_;

typedef struct Location_ {
  LocationType_ type;
  int32_t index;
  int32_t frame_offset;

  Token_ token;
  size_t size;
} Location_;

typedef enum {
  OPERAND_TYPE_EMPTY,
  OPERAND_TYPE_LOC,
  OPERAND_TYPE_VAL,
  OPERAND_TYPE_OFFSET,
  OPERAND_TYPE_SIZE,
} OperandType_;

typedef struct Operand_ {
  OperandType_ arg_type;
  RuntimeType_ opt_type;
  union {
    Location_ loc;
    Value_ val;
    int32_t offset;
    int64_t size;
  };
} Operand_;

typedef struct Tac_ {
  OpCode op;
  Location_ dst;

  Operand_ arg_l;
  Operand_ arg_r;

  int line;
  int chunk_loc;
} Tac_;

inline bool is_loc_empty(Location_ addr) {
  return addr.type == LOCATION_TYPE_EMPTY;
}

#endif  // TAC__H
