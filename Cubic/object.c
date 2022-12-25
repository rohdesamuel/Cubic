#include "object.h"

static void objstring_destroy(ObjString_* obj);

static Obj_* obj_create(size_t size, ObjType type) {
  Obj_* object = (Obj_*)malloc(size);
  memset(object, 0, size);
  object->type = type;
  object->size = size;
  return object;
}

Obj_* obj_copy(Obj_* obj) {
  Obj_* object = (Obj_*)malloc(obj->size);
  memcpy(object, obj, sizeof(Obj_));

  switch (obj->type) {
    case OBJ_TYPE_STRING: 
    {
      ObjString_* from = (ObjString_*)obj;
      ObjString_* to = (ObjString_*)object;

      to->length = from->length;
      to->chars = (char*)malloc(to->length + 1);
      memcpy(to->chars, from->chars, to->length);
      to->chars[to->length] = '\0';
      break;
    }
  }

  return object;
}

void obj_destroy(Obj_* obj) {
  switch (obj->type) {
    case OBJ_TYPE_STRING: objstring_destroy((ObjString_*)obj); break;
  }

  free(obj);
}

ObjString_* objstring_create(char* chars, int length) {
  ObjString_* string = (ObjString_*)obj_create(sizeof(ObjString_), OBJ_TYPE_STRING);
  string->length = length;
  string->chars = chars;
  return string;
}

static void objstring_destroy(ObjString_* obj) {
  free(obj->chars);
}

ObjString_* objstring_from(const char* chars, int length) {
  char* heap_chars = (char*)malloc(length + 1);
  assert(heap_chars);

  memcpy(heap_chars, chars, length);
  heap_chars[length] = '\0';
  return objstring_create(heap_chars, length);
}