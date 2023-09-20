#include "type.h"
#include "symbol_table.h"

const UnknownType_ Unknown_Ty = {
  .self = {
    .cls = TYPE_CLS(UnknownType_),
    .size = 0,
  }
};

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

const Int32Type_ Int32_Ty = {
  .self = {
    .cls = TYPE_CLS(Int32Type_),
    .size = 1,
  }
};

const Int64Type_ Int64_Ty = {
  .self = {
    .cls = TYPE_CLS(Int64Type_),
    .size = 1,
  }
};

const UintType_ Uint_Ty = {
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

Type_* type_deref(Type_* type) {
  if (type_is(type, Type_)) {
    return type_deref(type_cast(UnaryType_, type)->ty);
  }
  return type;
}

Type_* make_unknown_ty(MemoryAllocator_* allocator) {
  UnaryType_* ret = alloc_ty(allocator, UnaryType_);
  memset(ret, 0, sizeof(UnaryType_));
  ret->self.cls = TYPE_CLS(UnknownType_);

  return (Type_*)ret;
}

Type_* make_placeholder_ty(Token_ name, MemoryAllocator_* allocator) {
  Type_* ret = make_unknown_ty(allocator);
  ret->opt_name = name;

  return (Type_*)ret;
}

Type_* make_var_ty(Type_* sub_type, MemoryAllocator_* allocator) {
  VarType_* ret = type_alloc_ty(allocator, VarType_);
  ret->unary.ty = sub_type;
  return (Type_*)ret;
}

Type_* make_in_ty(Type_* sub_type, MemoryAllocator_* allocator) {
  InType_* ret = type_alloc_ty(allocator, InType_);
  ret->unary.ty = sub_type;
  return (Type_*)ret;
}

Type_* make_out_ty(Type_* sub_type, MemoryAllocator_* allocator) {
  OutType_* ret = type_alloc_ty(allocator, OutType_);
  ret->unary.ty = sub_type;
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
  list_of(&ret->members, ClassTypeField_, allocator);
  return (Type_*)ret;
}

Type_* make_const_ty(Type_* sub_type, MemoryAllocator_* allocator) {
  ConstType_* ret = type_alloc_ty(allocator, ConstType_);
  ret->unary.ty = sub_type;
  return (Type_*)ret;
}

Type_* make_function_ty(Token_ name, MemoryAllocator_* allocator) {
  FunctionType_* ret = type_alloc_ty(allocator, FunctionType_);
  ret->ret_ty = (Type_*)&Nil_Ty;
  list_of(&ret->params, Type_*, allocator);
  return (Type_*)ret;
}

Type_* type_alloc(MemoryAllocator_* allocator, int type_cls, size_t type_size) {
  Type_* ret =  alloc(allocator, type_size);
  memset(ret, 0, type_size);
  ret->cls = type_cls;
  return (Type_*)ret;
}

bool type_isunknown(const Type_* ty) {
  return type_is(type_deref((Type_*)ty), UnknownType_);
}

bool type_isaprimitive(const Type_* ty) {
  ty = type_deref((Type_*)ty);
  return ty->cls > __TYPE_PRIMITIVE_START__ && ty->cls < __TYPE_PRIMITIVE_END__;
}

bool type_isunary(const Type_* ty) {
  ty = type_deref((Type_*)ty);
  return ty->cls > __TYPE_UNARY_START__ && ty->cls < __TYPE_UNARY_END__;
}

bool type_isnil(const Type_* ty) {
  ty = type_deref((Type_*)ty);
  return type_is(ty, NilType_);
}

bool type_isabool(const Type_* ty) {
  ty = type_deref((Type_*)ty);
  return type_is(ty, BoolType_);
}

bool type_isanumber(const Type_* ty) {
  ty = type_deref((Type_*)ty);
  return ty->cls >= TYPE_CLS(IntType_) && ty->cls <= TYPE_CLS(DoubleType_);
}

bool type_isainteger(const Type_* ty) {
  ty = type_deref((Type_*)ty);
  return ty->cls >= TYPE_CLS(IntType_) && ty->cls <= TYPE_CLS(Uint64Type_);
}

bool type_issigned(const Type_* ty) {
  ty = type_deref((Type_*)ty);
  return ty->cls >= TYPE_CLS(IntType_) && ty->cls <= TYPE_CLS(Int64Type_);
}

bool type_isunsigned(const Type_* ty) {
  ty = type_deref((Type_*)ty);
  return ty->cls >= TYPE_CLS(UintType_) && ty->cls <= TYPE_CLS(Uint64Type_);
}

bool type_isareal(const Type_* ty) {
  ty = type_deref((Type_*)ty);
  return ty->cls == TYPE_CLS(FloatType_) || ty->cls == TYPE_CLS(DoubleType_);
}

bool type_isastring(const Type_* ty) {
  ty = type_deref((Type_*)ty);
  return type_is(ty, StringType_);
}

// Fills all placeholders in the given type with types found starting at the
// given scope.
bool type_fill(Type_* type, struct Scope_* scope) {
  if (type_isaprimitive(type)) {
    return true;
  }

  if (type_isunary(type)) {
    return type_fill(type_cast(UnaryType_, type)->ty, scope);
  }

  if (type_is(type, UnknownType_)) {
    UnaryType_* placeholder = type_cast(UnaryType_, type);
    assertf(placeholder->ty == NULL, "Encountered unknown type with filled type.");
    assertf(type->opt_name.start, "Encountered an unknown type without a type name.");

    Symbol_* sym = scope_search_to_root(scope, &type->opt_name);
    if (!sym) {
      return false;
    }

    type_set(type, sym->ty);
  }

  switch (type->cls) {
    case TYPE_CLS(Type_):
      return type_fill(type_cast(UnaryType_, type)->ty, scope);

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
  }

  return true;
}

void type_set(Type_* type, Type_* new_type) {
  assertf(type_is(type, UnknownType_), "Trying to set an already reified typed");
  assertf(type_cast(UnaryType_, type)->ty == NULL, "Trying to set an already reified typed");
  
  type->cls = TYPE_CLS(Type_);
  type_cast(UnaryType_, type)->ty = new_type;
}

void type_replace(Type_* type, Type_* new_type) {
  type->impl = new_type->impl;
}


size_t type_calcsize(Type_* type) {
  size_t size = 0;
  switch (type->cls) {
    case TYPE_CLS(NilType_):
      return 0;

    // TODO: Change this to return byte sizes.
    case TYPE_CLS(BoolType_):
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
      return 1;

    case TYPE_CLS(ClassType_):
    {
      ClassType_* ty = type_as(ClassType_, type);
      for (ListNode_* n = ty->members.head; n != NULL; n = n->next) {
        size += type_calcsize(list_val(n, ClassTypeField_).type);
      }
      break;
    }

    case TYPE_CLS(ArrayType_):
    {
      ArrayType_* ty = type_as(ArrayType_, type);
      size_t el_size = type_calcsize(ty->el_type);

      // Array is like struct { int count; void* data[]; }
      size = el_size * ty->count;
      assertf(el_size == 0 || size / el_size == ty->count, "Array size overflow.");
      break;
    }

    case TYPE_CLS(Type_):
    {
      UnaryType_* ty = type_cast(UnaryType_, type);
      size = type_calcsize(ty->ty);
      break;
    }

    case TYPE_CLS(VarType_):
    {
      VarType_* ty = type_as(VarType_, type);
      size = 1;
      type_calcsize(ty->unary.ty);
      break;
    }

    case TYPE_CLS(InType_):
    {
      InType_* ty = type_as(InType_, type);
      size = 1;
      type_calcsize(ty->unary.ty);
      break;
    }

    case TYPE_CLS(OutType_):
    {
      OutType_* ty = type_as(OutType_, type);
      size = 1;
      type_calcsize(ty->unary.ty);
      break;
    }

    case TYPE_CLS(ConstType_):
    {
      ConstType_* ty = type_as(ConstType_, type);
      size = type_calcsize(ty->unary.ty);
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
  if (type_is(ty, Type_) || type_isunary(ty)) {
    return type_cast(UnaryType_, ty)->ty;
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

      if (a_fn->params.count != b_fn->params.count) {
        return false;
      }

      if (!type_equal(a_fn->ret_ty, b_fn->ret_ty)) {
        return false;
      }

      ListNode_* a_param = a_fn->params.head;
      ListNode_* b_param = b_fn->params.head;
      for (int i = 0; i < a_fn->params.count; ++i) {
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
  if (type_is(from, Type_) || type_is(to, Type_)) {
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
  from = type_valtype((Type_*)from);
  to = type_valtype((Type_*)to);

  if (type_is(from, ArrayType_) && type_is(to, ArrayType_)) {
    ArrayType_* arr_from = type_as(ArrayType_, from);
    ArrayType_* arr_to = type_as(ArrayType_, to);

    return arr_from->count == arr_to->count && type_coercible(arr_from->el_type, arr_to->el_type);
  }

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
  if (type_is(ty, Type_)) {
    return type_isconst(type_cast(UnaryType_, ty)->ty);
  }

  return type_is(ty, ConstType_) || type_is(ty, InType_);
}

bool type_isval(const Type_* ty) {
  if (type_is(ty, Type_)) {
    return type_isval(type_cast(UnaryType_, ty)->ty);
  }

  return !type_is(ty, VarType_) && !type_is(ty, RefType_) && !type_is(ty, InType_) && !type_is(ty, OutType_);
}

bool type_isaref(const Type_* ty) {
  if (type_is(ty, Type_)) {
    return type_isaref(type_cast(UnaryType_, ty)->ty);
  }

  return type_is(ty, RefType_) || type_is(ty, InType_) || type_is(ty, OutType_);
}

const char* type_tostr(const Type_* ty) {
  switch (ty->cls) {
    case TYPE_CLS(UnknownType_):
      return "<unknown>";

    case TYPE_CLS(Type_):
      return type_tostr(type_cast(UnaryType_, ty)->ty);

    case TYPE_CLS(NilType_):
      return "nil";

    case TYPE_CLS(IntType_):
      return "int";

    case TYPE_CLS(Int8Type_):
      return "int8";

    case TYPE_CLS(Int16Type_):
      return "int16";

    case TYPE_CLS(Int32Type_):
      return "int32";

    case TYPE_CLS(Int64Type_):
      return "int64";

    case TYPE_CLS(UintType_):
      return "uint";

    case TYPE_CLS(Uint8Type_):
      return "uint8";

    case TYPE_CLS(Uint16Type_):
      return "uint16";

    case TYPE_CLS(Uint32Type_):
      return "uint32";

    case TYPE_CLS(Uint64Type_):
      return "uint64";

    case TYPE_CLS(FloatType_):
      return "float";

    case TYPE_CLS(DoubleType_):
      return "double";

    case TYPE_CLS(StringType_):
      return "string";

    case TYPE_CLS(ClassType_):
      return "class";

    case TYPE_CLS(ArrayType_):
      return "array";

    case TYPE_CLS(VarType_):
      return "var";

    case TYPE_CLS(InType_):
      return "in";

    case TYPE_CLS(OutType_):
      return "out";
  }
  return "unknown";
}

void print_type(const Type_* ty) {
  switch (ty->cls) {
    case TYPE_CLS(UnknownType_):
      printf("<unknown>");
      break;

    case TYPE_CLS(Type_):
      print_type(type_cast(UnaryType_, ty)->ty);
      break;

    case TYPE_CLS(NilType_):
      printf("nil");
      break;

    case TYPE_CLS(IntType_):
      printf("int");
      break;

    case TYPE_CLS(Int8Type_):
      printf("int8");
      break;

    case TYPE_CLS(Int16Type_):
      printf("int16");
      break;

    case TYPE_CLS(Int32Type_):
      printf("int32");
      break;

    case TYPE_CLS(Int64Type_):
      printf("int64");
      break;

    case TYPE_CLS(UintType_):
      printf("uint");
      break;

    case TYPE_CLS(Uint8Type_):
      printf("uint8");
      break;

    case TYPE_CLS(Uint16Type_):
      printf("uint16");
      break;

    case TYPE_CLS(Uint32Type_):
      printf("uint32");
      break;

    case TYPE_CLS(Uint64Type_):
      printf("uint64");
      break;

    case TYPE_CLS(FloatType_):
      printf("float");
      break;

    case TYPE_CLS(DoubleType_):
      printf("double");
      break;

    case TYPE_CLS(StringType_):
      printf("string");
      break;

    case TYPE_CLS(ClassType_):
      printf("%.*s", ty->opt_name.length, ty->opt_name.start);
      break;

    case TYPE_CLS(ArrayType_):
      print_type(type_as(ArrayType_, ty)->el_type);
      printf("%zu", type_as(ArrayType_, ty)->count);
      break;

    case TYPE_CLS(ConstType_):
      printf("const[");
      print_type(type_cast(UnaryType_, ty)->ty);
      printf("]");
      break;

    case TYPE_CLS(InType_):
      printf("in[");
      print_type(type_cast(UnaryType_, ty)->ty);
      printf("]");
      break;

    case TYPE_CLS(OutType_):
      printf("out[");
      print_type(type_cast(UnaryType_, ty)->ty);
      printf("]");
      break;

    case TYPE_CLS(VarType_):
      printf("var[");
      print_type(type_cast(UnaryType_, ty)->ty);
      printf("]");
      break;

    case TYPE_CLS(RefType_):
      printf("ref[");
      print_type(type_cast(UnaryType_, ty)->ty);
      printf("]");
      break;

  }

  printf("unknown");
}

RuntimeType_ type_toruntime(const Type_* ty) {
  ty = type_valtype(type_deref((Type_*)ty));
  return (RuntimeType_) {
    .ty = ty->cls
  };
}

uint32_t type_toint(RuntimeType_ info) {
  return info.ty;
}

RuntimeType_ type_fromint(uint32_t n) {
  return (RuntimeType_) {
    .ty = n,
  };
}

Type_* type_valtype(Type_* ty) {
  if (type_is(ty, Type_) || type_isunary(ty)) {
    if (!type_cast(UnaryType_, ty)->ty) {
      return ty;
    }
    return type_valtype(type_cast(UnaryType_, ty)->ty);
  }

  return ty;
}

bool type_isdefined(const Type_* ty) {
  if (!ty) {
    return false;
  }

  if (type_is(ty, UnknownType_)) {
    return false;
  }

  if (type_isaprimitive(ty)) {
    return true;
  }

  if (type_is(ty, Type_) || type_isunary(ty)) {
    return type_isdefined(type_cast(UnaryType_, ty)->ty);
  }

  switch (ty->cls) {
    case TYPE_CLS(ClassType_):
    {
      const ClassType_* cls_ty = type_as(ClassType_, ty);
      for (ListNode_* n = cls_ty->members.head; n != NULL; n = n->next) {
        
      }
    }
    
    case TYPE_CLS(ArrayType_):
      break;
    
    case TYPE_CLS(FunctionType_):
      break;
  }

  return false;
}

void type_class_calcoffsets(Type_* ty) {
  ClassType_* cls_ty = type_as(ClassType_, ty);
  size_t ret = 0;
  for (ListNode_* n = cls_ty->members.head; n != NULL; n = n->next) {
    ClassTypeField_* field = list_ptr(n, ClassTypeField_);
    field->offset = ret;
    ret += field->type->size;
  }
}

Type_* type_class_findmember(const Type_* cls_ty, const Token_* name, size_t* offset) {
  ClassType_* cls_type = type_as(ClassType_, cls_ty);

  for (ListNode_* n = cls_type->members.head; n != NULL; n = n->next) {
    ClassTypeField_* field = list_ptr(n, ClassTypeField_);
    if (field->name.length == name->length && memcmp(field->name.start, name->start, field->name.length) == 0) {
      if (offset) {
        *offset = field->offset;
      }
      return field->type;
    }
  }

  return NULL;
}