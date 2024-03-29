#include "object.h"

static Obj_* obj_copy(Obj_* obj);
static void objstring_destroy(ObjString_* obj);

static Obj_* obj_create(size_t size, ObjType info) {
  Obj_* object = (Obj_*)malloc(size);
  memset(object, 0, size);
  object->type = info;
  object->count = 1;
  object->size = size;
  return object;
}

Obj_* obj_copy(Obj_* obj) {
  Obj_* object = (Obj_*)malloc(max(obj->size, sizeof(Obj_)));
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

ObjFunction_* objfn_create(struct Symbol_* fn_sym) {
  ObjFunction_* ret = malloc(sizeof(ObjFunction_));
  *ret = (ObjFunction_){0};
  ret->base.type = OBJ_TYPE_FUNCTION;
  return ret;
}

bool obj_equal(struct Obj_* l, struct Obj_* r) {
  switch (l->type) {
    case OBJ_TYPE_STRING:
    {
      ObjString_* l_string = (ObjString_*)l;
      ObjString_* r_string = (ObjString_*)r;
      return l_string == r_string ||
        (l_string->length == r_string->length && memcmp(l_string->chars, r_string->chars, l_string->length) == 0);
    }
  }

  return 0;
}