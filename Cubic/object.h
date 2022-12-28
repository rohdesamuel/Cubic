#ifndef OBJECT__H
#define OBJECT__H

#include "common.h"
#include "value.h"

#include <string.h>

#define OBJ_TYPE(value)    (AS_OBJ(value)->type)
#define AS_STRING(value)   ((struct ObjString_*)AS_OBJ(value))
#define AS_CSTRING(value)  (((struct ObjString_*)AS_OBJ(value))->chars)
#define AS_FUNCTION(value) ((struct ObjFunction_*)AS_OBJ(value))
#define IS_STRING(value)   obj_istype(value, OBJ_STRING)
#define IS_FUNCTION(value) obj_istype(value, OBJ_FUNCTTION)

typedef enum ObjType {
  OBJ_TYPE_UNKNOWN,
  OBJ_TYPE_STRING,
  OBJ_TYPE_FUNCTION,
} ObjType;

typedef struct Obj_ {
  ObjType type;
  size_t size;
} Obj_;

typedef struct ObjString_ {
  Obj_ base;
  int length;
  char* chars;
} ObjString_;

typedef struct ObjFunction_ {
  Obj_ base;

  int constant_index;
  struct Chunk_* chunk;

} ObjFunction_;

Obj_* obj_copy(Obj_* obj);
void obj_destroy(Obj_* obj);

static inline bool obj_istype(Value_ value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

static inline Value_ obj_val(struct Obj_* obj) {
  return (Value_) {
    {
      .obj = obj
    },
      .type = {
          VAL_OBJ,
          KIND_VAL,
          ((struct Obj_*)obj)->type
      }
  };
}

ObjString_* objstring_create(char* chars, int length);
ObjString_* objstring_from(const char* chars, int length);
ObjFunction_* objfn_create(struct Symbol_* fn_sym);

#endif  // OBJECT__H