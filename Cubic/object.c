#include "object.h"

static Obj_* obj_create(size_t size, ObjType type) {
  Obj_* object = (Obj_*)malloc(size);
  memset(object, 0, size);
  object->type = type;
  return object;
}

static ObjString_* objstring_create(char* chars, int length) {
  ObjString_* string = (ObjString_*)obj_create(sizeof(ObjString_), OBJ_TYPE_STRING);
  string->length = length;
  string->chars = chars;
  return string;
}

ObjString_* obj_fromstring(const char* chars, int length) {
  char* heap_chars = malloc(length + 1);
  assert(heap_chars);

  memcpy(heap_chars, chars, length);
  heap_chars[length] = '\0';
  return objstring_create(heap_chars, length);
}
