#include "type.h"
#include "symbol_table.h"

const NilType_ Nil_Ty = {
  .self = {
    .cls = TYPE_CLS(NilType_),
    .size = 1,
  }
};

const BoolType_ Bool_Ty = {
  .self = {
    .cls = TYPE_CLS(BoolType_),
    .size = 1,
  }
};

const IntType_ Int_Ty = {
  .self = {
    .cls = TYPE_CLS(IntType_),
    .size = 1,
  }
};

const UintType_ UintType = {
  .self = {
    .cls = TYPE_CLS(UintType_),
    .size = 1,
  }
};

const FloatType_ Float_Ty = {
  .self = {
    .cls = TYPE_CLS(FloatType_),
    .size = 1,
  }
};

const DoubleType_ Double_Ty = {
  .self = {
    .cls = TYPE_CLS(DoubleType_),
    .size = 1,
  }
};

const StringType_ String_Ty = {
  .self = {
    .cls = TYPE_CLS(StringType_),
    .size = 1,
  }
};

Type_* make_var_ty(Type_* sub_type, MemoryAllocator_* allocator) {
  VarType_* ret = type_alloc_ty(allocator, VarType_);
  ret->ty = sub_type;
  return (Type_*)ret;
}

Type_* make_in_ty(Type_* sub_type, MemoryAllocator_* allocator) {
  InType_* ret = type_alloc_ty(allocator, InType_);
  ret->ty = sub_type;
  return (Type_*)ret;
}

Type_* make_out_ty(Type_* sub_type, MemoryAllocator_* allocator) {
  OutType_* ret = type_alloc_ty(allocator, OutType_);
  ret->ty = sub_type;
  return (Type_*)ret;
}

Type_* make_array_ty(Type_* el_type, size_t count, MemoryAllocator_* allocator) {
  ArrayType_* ret = type_alloc_ty(allocator, ArrayType_);
  ret->el_type = el_type;
  ret->count = count;
  return (Type_*)ret;
}

Type_* make_class_ty(Token_ name, MemoryAllocator_* allocator) {
  ClassType_* ret = type_alloc_ty(allocator, ClassType_);
  ret->self.opt_name = name;
  list_of(&ret->members, Type_*, allocator);
  list_of(&ret->field_names, Type_*, allocator);
  return (Type_*)ret;
}

Type_* make_placeholder_ty(Token_ name, MemoryAllocator_* allocator) {
  PlaceholderType_* ret = type_alloc_ty(allocator, PlaceholderType_);
  ret->self.opt_name = name;
  return (Type_*)ret;
}

Type_* make_const_ty(Type_* sub_type, MemoryAllocator_* allocator) {
  ConstType_* ret = type_alloc_ty(allocator, ConstType_);
  ret->ty = sub_type;
  return (Type_*)ret;
}

Type_* type_alloc(MemoryAllocator_* allocator, size_t type_size) {
  Type_* ret = alloc(allocator, type_size);
  memset(ret, 0, type_size);
  return ret;
}

// Fills all placeholders in the given type with types found starting at the
// given scope.
bool type_fill(Type_* type, struct Scope_* scope) {
  if (type_isaprimitive(type)) {
    return true;
  }

  if (type_is(type, PlaceholderType_)) {
    PlaceholderType_* placeholder = type_as(PlaceholderType_, type);
    Symbol_* sym = scope_search_to_root(scope, &type->opt_name);
    if (!sym) {
      return false;
    }

    if (placeholder->ty == NULL) {
      placeholder->ty = sym->ty;
    }

    return type_fill(sym->ty, scope);
  }

  switch (type->cls) {
    case TYPE_CLS(ClassType_):
    {
      ClassType_* class_ty = type_as(ClassType_, type);
      bool ret = true;
      for (ListNode_* n = class_ty->members.head; n != NULL; n = n->next) {
        ret = ret && type_fill(list_val(n, Type_*), scope);
      }
      return ret;
    }

    case TYPE_CLS(ArrayType_):
    {
      ArrayType_* ty = type_as(ArrayType_, type);
      return type_fill(ty->el_type, scope);
    }

    case TYPE_CLS(VarType_):
    {
      VarType_* ty = type_as(VarType_, type);
      return type_fill(ty->ty, scope);
    }

    case TYPE_CLS(InType_):
    {
      InType_* ty = type_as(InType_, type);
      return type_fill(ty->ty, scope);
    }

    case TYPE_CLS(OutType_):
    {
      OutType_* ty = type_as(OutType_, type);
      return type_fill(ty->ty, scope);
    }

    case TYPE_CLS(ConstType_):
    {
      ConstType_* ty = type_as(ConstType_, type);
      return type_fill(ty->ty, scope);
    }
  }

  return true;
}

size_t type_calcsize(Type_* type) {
  size_t size = 0;
  assertf(!type_is(type, PlaceholderType_), "Received a type that is still a placeholder.");

  switch (type->cls) {
    // TODO: Change this to return byte sizes.
    case TYPE_CLS(IntType_):
    case TYPE_CLS(Int8Type_):
    case TYPE_CLS(Int16Type_):
    case TYPE_CLS(Int32Type_):
    case TYPE_CLS(Int64Type_):
    case TYPE_CLS(UintType_):
    case TYPE_CLS(Uint8Type_):
    case TYPE_CLS(Uint16Type_):
    case TYPE_CLS(Uint32Type_):
    case TYPE_CLS(Uint64Type_):
    case TYPE_CLS(FloatType_):
    case TYPE_CLS(DoubleType_):
    case TYPE_CLS(StringType_):
      size = 1;
      break;

    case TYPE_CLS(ClassType_):
    {
      ClassType_* ty = type_as(ClassType_, type);
      for (ListNode_* n = ty->members.head; n != NULL; n = n->next) {
        size += type_calcsize(list_val(n, Type_*));
      }
      break;
    }

    case TYPE_CLS(ArrayType_):
    {
      ArrayType_* ty = type_as(ArrayType_, type);
      size_t el_size = type_calcsize(ty->el_type);
      size = el_size * ty->count;
      assertf(el_size == 0 || size / el_size == ty->count, "Array size overflow.");
      break;
    }

    case TYPE_CLS(VarType_):
    {
      VarType_* ty = type_as(VarType_, type);
      size = 1;
      type_calcsize(ty->ty);
      break;
    }

    case TYPE_CLS(InType_):
    {
      InType_* ty = type_as(InType_, type);
      size = 1;
      type_calcsize(ty->ty);
      break;
    }

    case TYPE_CLS(OutType_):
    {
      OutType_* ty = type_as(OutType_, type);
      size = 1;
      type_calcsize(ty->ty);
      break;
    }

    case TYPE_CLS(ConstType_):
    {
      ConstType_* ty = type_as(ConstType_, type);
      size = type_calcsize(ty->ty);
      break;
    }
  }

  type->size = size;
  return size;
}

Type_* assert_type_is_(Type_* ty, int cls) {
  assertf(ty->cls == cls, "Type does not match %d vs. %d", ty->cls, cls);
  return ty;
}

static const Type_* type_subtype(const Type_* ty) {
  switch (ty->cls) {
    case TYPE_CLS(ConstType_): return type_as(ConstType_, ty)->ty;
    case TYPE_CLS(InType_): return type_as(InType_, ty)->ty;
    case TYPE_CLS(OutType_): return type_as(OutType_, ty)->ty;
    case TYPE_CLS(VarType_): return type_as(VarType_, ty)->ty;
    case TYPE_CLS(RefType_): return type_as(RefType_, ty)->ty;
    case TYPE_CLS(PlaceholderType_): return type_as(PlaceholderType_, ty)->ty;
  }

  return NULL;
}

bool type_equal(const Type_* a, const Type_* b) {
  if (a == b) {
    return true;
  }

  if (a->cls != b->cls) {
    return false;
  }

  // Recurse past any unary types.
  const Type_* a_sub = type_subtype(a);
  const Type_* b_sub = type_subtype(b);
  if (a_sub || b_sub) {
    a_sub = a_sub ? a_sub : a;
    b_sub = b_sub ? b_sub : b;
    return type_equal(a_sub, b_sub);
  }

  switch (a->cls) {
    // Degenerate case, written for completeness.
    case TYPE_CLS(ClassType_):
      return a == b;

    case TYPE_CLS(ArrayType_):
    {
      ArrayType_* a_arr = type_as(ArrayType_, a);
      ArrayType_* b_arr = type_as(ArrayType_, b);

      return a_arr->count == b_arr->count && type_equal(a_arr->el_type, b_arr->el_type);
    }

    case TYPE_CLS(FunctionType_):
    {
      FunctionType_* a_fn = type_as(FunctionType_, a);
      FunctionType_* b_fn = type_as(FunctionType_, b);

      if (a_fn->parameters.count != b_fn->parameters.count) {
        return false;
      }

      if (!type_equal(a_fn->ret_ty, b_fn->ret_ty)) {
        return false;
      }

      ListNode_* a_param = a_fn->parameters.head;
      ListNode_* b_param = b_fn->parameters.head;
      for (int i = 0; i < a_fn->parameters.count; ++i) {
        if (!type_equal(list_val(a_param, Type_*), list_val(b_param, Type_*))) {
          return false;
        }

        a_param = a_param->next;
        b_param = b_param->next;
      }

      break;
    }
  }

  assertf(false, "Unknown type when determining type equality.");
  return false;
}

static const Type_* type_subtype_or_type(const Type_* ty) {
  const Type_* sub = type_subtype(ty);
  return sub ? sub : ty;
}

bool type_assignable(const Type_* from, const Type_* to) {
  // Cannot assign to a const.
  if (type_is(to, InType_) || type_is(to, ConstType_)) {
    return false;
  }

  // Return true if the type is the same, e.g. both are the same classs, or a
  // primitive can be coerced into another.
  if (from == to || type_coercible(from, to)) {
    return true;
  }

  // Recurse past any unary types.
  const Type_* from_sub = type_subtype(from);
  const Type_* to_sub = type_subtype(to);
  if (from_sub || to_sub) {
    from_sub = from_sub ? from_sub : from;
    to_sub = to_sub ? to_sub : to;
    return type_assignable(from_sub, to_sub);
  }

  // If we recurse past any unary types but are still left with placeholders,
  // then the placeholders haven't been filled out yet. The assignability is
  // still unknown.
  if (type_is(from, PlaceholderType_) || type_is(to, PlaceholderType_)) {
    return false;
  }

  switch (from->cls) {
    case TYPE_CLS(ArrayType_):
      return type_is(to, ArrayType_) &&
        type_as(ArrayType_, from)->count == type_as(ArrayType_, to)->count &&
        type_assignable(type_as(ArrayType_, from)->el_type, type_as(ArrayType_, to)->el_type);

    case TYPE_CLS(FunctionType_):
      return type_equal(from, to);
  }

  assertf(false, "Unknown type when determining type assignability.");
  return false;
}

bool type_coercible(const Type_* from, const Type_* to) {
  return
    from == to ||
    // 64-bit conversion
    ((to->cls == TYPE_CLS(UintType_) || to->cls == TYPE_CLS(Uint64Type_) || to->cls == TYPE_CLS(IntType_) || to->cls == TYPE_CLS(Int64Type_)) &&
      (from->cls >= TYPE_CLS(IntType_) && from->cls <= TYPE_CLS(Uint64Type_))) ||

    // 8-bit conversion
    ((to->cls == TYPE_CLS(Uint8Type_) || to->cls == TYPE_CLS(Int8Type_)) && (from->cls == TYPE_CLS(Uint8Type_) || from->cls == TYPE_CLS(Int8Type_))) ||

    // 16-bit conversion
    ((to->cls == TYPE_CLS(Uint16Type_) || to->cls == TYPE_CLS(Int16Type_)) && (from->cls == TYPE_CLS(Uint8Type_) || from->cls == TYPE_CLS(Int8Type_) ||
      from->cls == TYPE_CLS(Uint16Type_) || from->cls == TYPE_CLS(Int16Type_))) ||

    // 32-bit conversion
    ((to->cls == TYPE_CLS(Uint32Type_) || to->cls == TYPE_CLS(Int32Type_)) && (from->cls == TYPE_CLS(Uint8Type_) || from->cls == TYPE_CLS(Int8Type_) ||
      from->cls == TYPE_CLS(Uint16Type_) || from->cls == TYPE_CLS(Int16Type_) ||
      from->cls == TYPE_CLS(Uint32Type_) || from->cls == TYPE_CLS(Int32Type_))) ||

    // Double conversion
    (to->cls == TYPE_CLS(DoubleType_) && (from->cls >= TYPE_CLS(IntType_) && from->cls <= TYPE_CLS(DoubleType_))) ||

    // Float conversion
    (to->cls == TYPE_CLS(FloatType_) && (from->cls == TYPE_CLS(FloatType_) ||
      from->cls >= TYPE_CLS(Int8Type_) && from->cls <= TYPE_CLS(Int32Type_) ||
      from->cls >= TYPE_CLS(Uint8Type_) && from->cls <= TYPE_CLS(Uint32Type_)));
}

bool type_isconst(const Type_* ty) {
  return type_is(ty, ConstType_) || type_is(ty, InType_);
}

bool type_isval(const Type_* ty) {
  return !type_is(ty, VarType_) && !type_is(ty, RefType_) && !type_is(ty, InType_) && !type_is(ty, OutType_);
}

bool type_isaref(const Type_* ty) {
  return type_is(ty, RefType_) || type_is(ty, InType_) || type_is(ty, OutType_);
}