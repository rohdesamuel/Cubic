#include "type.h"

const Type_ Nil_Ty = {
  .val = VAL_NIL,
  .kind = KIND_VAL,
};

const Type_ Bool_Ty = {
  .val = VAL_BOOL,
  .kind = KIND_VAL,
};

const Type_ Int_Ty = {
  .val = VAL_INT,
  .kind = KIND_VAL,
};

const Type_ UintType = {
  .val = VAL_UINT,
  .kind = KIND_VAL,
};

const Type_ Float_Ty = {
  .val = VAL_FLOAT,
  .kind = KIND_VAL,
};

const Type_ Double_Ty = {
  .val = VAL_DOUBLE,
  .kind = KIND_VAL,
};

Type_* make_var_ty(const Type_* sub_type, MemoryAllocator_* allocator) {
  Type_* ret = alloc_ty(allocator, Type_);
  *ret = (Type_){0};

  ret->val = VAL_VAR;
  ret->kind = KIND_VAR;
  list_of(&ret->component_types, Type_*, allocator);

  return ret;
}

Type_* make_in_ty(const Type_* sub_type, MemoryAllocator_* allocator) {
  Type_* ret = alloc_ty(allocator, Type_);
  *ret = (Type_){ 0 };

  ret->val = VAL_IN;
  ret->kind = KIND_REF;
  list_of(&ret->component_types, Type_*, allocator);

  return ret;
}

Type_* make_out_ty(const Type_* sub_type, MemoryAllocator_* allocator) {
  Type_* ret = alloc_ty(allocator, Type_);
  *ret = (Type_){ 0 };

  ret->val = VAL_OUT;
  ret->kind = KIND_REF;
  list_of(&ret->component_types, Type_*, allocator);
  list_push(&ret->component_types, &sub_type);

  return ret;
}

Type_* make_array_ty(const Type_* el_type, MemoryAllocator_* allocator) {
  Type_* ret = alloc_ty(allocator, Type_);
  *ret = (Type_){ 0 };

  ret->val = VAL_ARRAY;
  ret->kind = KIND_VAL;
  list_of(&ret->component_types, Type_*, allocator);
  list_push(&ret->component_types, &el_type);

  return ret;
}