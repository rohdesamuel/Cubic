#ifndef OBJECT__H
#define OBJECT__H

#include "common.h"
#include "value.h"

#include <string.h>

#define OBJ_TYPE(value)         (AS_OBJ(value)->type)
#define AS_STRING(value)        ((struct ObjString_*)AS_OBJ(value))
#define AS_CSTRING(value)       (((struct ObjString_*)AS_OBJ(value))->chars)
#define IS_STRING(value)       obj_istype(value, OBJ_STRING)

typedef enum ObjType {
  OBJ_TYPE_UNKNOWN,
  OBJ_TYPE_STRING,
} ObjType;

typedef struct Obj_ {
  ObjType type;
} Obj_;

typedef struct ObjString_ {
  Obj_ base;
  int length;
  char* chars;
} ObjString_;

static inline bool obj_istype(Value_ value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

ObjString_* obj_fromstring(const char* chars, int length);

#endif  // OBJECT__H