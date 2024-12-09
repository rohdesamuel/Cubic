#include "type.h"

#include <stdarg.h>

#include "hash.h"
#include "symbol_table.h"
#include "random.h"
#include "type_expr.h"

#define DEF_PRIMITIVE_TYPE(NAME) \
static Type_ NAME##_Ty_ = { \
  .cls = TYPE_CLS(NAME##Type_), \
  .size = 1, \
  .id = TYPE_CLS(NAME##Type_), \
}; \
const Type_* NAME##_Ty = &NAME##_Ty_;

DEF_PRIMITIVE_TYPE(Nil);
DEF_PRIMITIVE_TYPE(Bool);
DEF_PRIMITIVE_TYPE(Int);
DEF_PRIMITIVE_TYPE(Int8);
DEF_PRIMITIVE_TYPE(Int16);
DEF_PRIMITIVE_TYPE(Int32);
DEF_PRIMITIVE_TYPE(Int64);
DEF_PRIMITIVE_TYPE(Uint);
DEF_PRIMITIVE_TYPE(Uint8);
DEF_PRIMITIVE_TYPE(Uint16);
DEF_PRIMITIVE_TYPE(Uint32);
DEF_PRIMITIVE_TYPE(Uint64);
DEF_PRIMITIVE_TYPE(Float);
DEF_PRIMITIVE_TYPE(Double);
DEF_PRIMITIVE_TYPE(String);

#define TYPE_SIZE(TYPE) sizeof(TYPE)

static const size_t type_sizes[__TYPE_COUNT__] = {
  TYPE_LIST(TYPE_SIZE, CB_COMMA)
};

static void generictype_bindargs(Type_* generic_ty, List_* type_args, Frame_* frame, Scope_* scope);
static uint64_t type_id_calc(Type_* ty);

void type_init() {
  Nil_Ty_.tmpl = Nil_TypeExpr;
  Bool_Ty_.tmpl = Bool_TypeExpr;
  Int_Ty_.tmpl = Int_TypeExpr;
  Int8_Ty_.tmpl = Int8_TypeExpr;
  Int16_Ty_.tmpl = Int16_TypeExpr;
  Int32_Ty_.tmpl = Int32_TypeExpr;
  Int64_Ty_.tmpl = Int64_TypeExpr;
  Uint_Ty_.tmpl = Uint_TypeExpr;
  Uint8_Ty_.tmpl = Uint8_TypeExpr;
  Uint16_Ty_.tmpl = Uint16_TypeExpr;
  Uint32_Ty_.tmpl = Uint32_TypeExpr;
  Uint64_Ty_.tmpl = Uint64_TypeExpr;
  Float_Ty_.tmpl = Float_TypeExpr;
  Double_Ty_.tmpl = Double_TypeExpr;
  String_Ty_.tmpl = String_TypeExpr;
}

Type_* make_placeholder_ty(const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator) {
  PlaceholderType_* ret =  (PlaceholderType_*)type_alloc_ty(allocator, scope, tmpl, PlaceholderType_);
  ret->self.id = 0;
  return (Type_*)ret;
}

Type_* make_const_ty(Type_* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator) {
  ConstType_* ret = type_alloc_ty(allocator, scope, tmpl, ConstType_);
  ret->unary.ty = sub_type;
  ret->unary.self.id = type_id_calc((Type_*)ret);
  return (Type_*)ret;
}

Type_* make_var_ty(Type_* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator) {
  VarType_* ret = type_alloc_ty(allocator, scope, tmpl, VarType_);
  ret->unary.ty = sub_type;
  ret->unary.self.id = type_id_calc((Type_*)ret);
  return (Type_*)ret;
}

Type_* make_ref_ty(Type_* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator) {
  RefType_* ret = type_alloc_ty(allocator, scope, tmpl, RefType_);
  ret->unary.ty = sub_type;
  ret->unary.self.id = type_id_calc((Type_*)ret);
  return (Type_*)ret;
}

Type_* make_in_ty(Type_* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator) {
  InType_* ret = type_alloc_ty(allocator, scope, tmpl, InType_);
  ret->unary.ty = sub_type;
  ret->unary.self.id = type_id_calc((Type_*)ret);
  return (Type_*)ret;
}

Type_* make_out_ty(Type_* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator) {
  OutType_* ret = type_alloc_ty(allocator, scope, tmpl, OutType_);
  ret->unary.ty = sub_type;
  ret->unary.self.id = type_id_calc((Type_*)ret);
  return (Type_*)ret;
}

Type_* make_array_ty(Type_* el_type, size_t count, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator) {
  ArrayType_* ret = type_alloc_ty(allocator, scope, tmpl, ArrayType_);
  ret->el_type = el_type;
  ret->count = count;
  ret->self.id = type_id_calc((Type_*)ret);
  return (Type_*)ret;
}

Type_* make_class_ty(Token_ name, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator) {
  ClassType_* ret = type_alloc_ty(allocator, scope, tmpl, ClassType_);
  ret->self.opt_name = name;
  list_of(&ret->members, ClassTypeField_, allocator);
  
  ret->scope = scope_createfrom(scope);

  static const char constructor_str[] = "__constructor";
  int name_size = sizeof(constructor_str) + ret->self.opt_name.length;
  char* constructor_name = alloc(allocator, name_size);
  sprintf_s(constructor_name, name_size, "%.*s%.*s\0", ret->self.opt_name.length, ret->self.opt_name.start, name_size - 1, constructor_str);

  FunctionType_* constructor_ty = (FunctionType_*)make_function_ty(token_string("constructor"), NULL, (Type_*)ret, tmpl, scope, allocator);
  constructor_ty->ret_ty = (Type_*)ret;
  constructor_ty->self.opt_name = (Token_){ .start = constructor_name, .length = name_size };
  ret->constructor = constructor_ty;
  ret->self.id = type_id_calc((Type_*)ret);
  return (Type_*)ret;
}

Type_* make_function_ty(Token_ name, ListOf_(Type_*)* params, Type_* ret_ty, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator) {
  FunctionType_* ret = type_alloc_ty(allocator, scope, tmpl, FunctionType_);
  ret->self.opt_name = name;

  if (ret_ty) {
    ret->ret_ty = ret_ty;
  } else {
    ret->ret_ty = (Type_*)Nil_Ty;
  }

  if (params) {
    ret->params = *params;
  } else {
    list_of(&ret->params, Type_*, allocator);
  }
  ret->self.id = type_id_calc((Type_*)ret);

  return (Type_*)ret;
}

Type_* make_tuple_ty(const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator, int n, ...) {
  va_list args;
  va_start(args, n);

  TupleType_* ret = type_alloc_ty(allocator, scope, tmpl, TupleType_);
  list_of(&ret->self.types, Type_*, allocator);

  for (int i = 0; i < n; ++i) {
    Type_* ty = va_arg(args, Type_*);
    list_push(&ret->self.types, &ty);
  }

  va_end(args);
  return (Type_*)ret;
}

Type_* make_union_ty(const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator, int n, ...) {
  va_list args;
  va_start(args, n);

  UnionType_* ret = type_alloc_ty(allocator, scope, tmpl, UnionType_);
  list_of(&ret->self.types, Type_*, allocator);

  for (int i = 0; i < n; ++i) {
    Type_* ty = va_arg(args, Type_*);
    list_push(&ret->self.types, &ty);
  }

  va_end(args);
  return (Type_*)ret;
}

Type_* make_constraint_ty(const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator, Token_ name, int n, ...) {
  va_list args;
  va_start(args, n);

  ConstraintType_* ret = type_alloc_ty(allocator, scope, tmpl, ConstraintType_);
  ret->self.self.opt_name = name;
  list_of(&ret->self.types, Type_*, allocator);

  for (int i = 0; i < n; ++i) {
    Type_* ty = va_arg(args, Type_*);
    list_push(&ret->self.types, &ty);
  }

  va_end(args);
  return (Type_*)ret;
}

Type_* make_field_ty(Token_ field_name, Type_* sub_type, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator) {
  FieldType_* ret = type_alloc_ty(allocator, scope, tmpl, FieldType_);
  ret->name = field_name;
  ret->self.ty = sub_type;

  return (Type_*)ret;
}

Type_* make_genericparam_ty(Token_ name, Type_* constraint, const struct TypeExpr_* tmpl, struct Scope_* scope, MemoryAllocator_* allocator) {
  GenericParamType_* ret = type_alloc_ty(allocator, scope, tmpl, GenericParamType_);
  ret->name = name;
  ret->self.ty = constraint;
  return (Type_*)ret;
}

Type_* type_alloc(MemoryAllocator_* allocator, struct Scope_* scope, const struct TypeExpr_* tmpl, int type_cls, size_t type_size) {
  Type_* ret =  alloc(allocator, type_size);
  memset(ret, 0, type_size);
  ret->cls = type_cls;
  ret->scope = scope;
  ret->tmpl = tmpl;
  list_of(&ret->specializations, Type_*, allocator);

  return (Type_*)ret;
}

bool type_isaprimitive(const Type_* ty) {
  return ty->cls >= __TYPE_PRIMITIVE_START__ && ty->cls <= __TYPE_PRIMITIVE_END__;
}

bool type_isunary(const Type_* ty) {
  return ty->cls >= __TYPE_UNARY_START__ && ty->cls <= __TYPE_UNARY_END__;
}

bool type_ismultitype(const Type_* ty) {
  return ty->cls == TYPE_CLS(TupleType_) || ty->cls == TYPE_CLS(UnionType_);
}

bool type_isnil(const Type_* ty) {
  return type_is(ty, NilType_);
}

bool type_isabool(const Type_* ty) {
  return type_is(ty, BoolType_);
}

bool type_isanumber(const Type_* ty) {
  return ty->cls >= TYPE_CLS(IntType_) && ty->cls <= TYPE_CLS(DoubleType_);
}

bool type_isainteger(const Type_* ty) {
  return ty->cls >= TYPE_CLS(IntType_) && ty->cls <= TYPE_CLS(Uint64Type_);
}

bool type_issigned(const Type_* ty) {
  return ty->cls >= TYPE_CLS(IntType_) && ty->cls <= TYPE_CLS(Int64Type_);
}

bool type_isunsigned(const Type_* ty) {
  return ty->cls >= TYPE_CLS(UintType_) && ty->cls <= TYPE_CLS(Uint64Type_);
}

bool type_isareal(const Type_* ty) {
  return ty->cls == TYPE_CLS(FloatType_) || ty->cls == TYPE_CLS(DoubleType_);
}

bool type_isastring(const Type_* ty) {
  return type_is(ty, StringType_);
}

Type_* type_findspecialization(const Type_* base_type, Type_** resolved_args) {
  Type_* found_specialization = NULL;
  for (ListNode_* n = base_type->specializations.head; n != NULL; n = n->next) {
    Type_* s = list_val(n, Type_*);

    bool is_same = true;
    for (size_t i = 0; i < s->args_count; ++i) {
      const Type_* s_type = s->args[i];
      const Type_* arg_type = resolved_args[i];
      is_same &= s_type->id == arg_type->id;
      if (!is_same) break;
    }

    if (is_same) {
      return s;
    }
  }

  return NULL;
}

static bool constrainttype_satisfied(ConstraintType_* constraints_ty, Type_* ty) {
  for (ListNode_* constraint_n = constraints_ty->self.types.head;
       constraint_n != NULL;
       constraint_n = constraint_n->next) {

    Type_* constraint = list_val(constraint_n, Type_*);
    if (!type_isassignable_to(ty, constraint)) {
      return false;
    }
  }

  return true;
}

// Fills all placeholders in the given type with types found starting at the
// given scope.
const Type_* type_resolve(Type_* type, struct Scope_* scope, struct ErrorsContainer_* errors) {
  type = type_valtype(type);
  if (!type_is(type, PlaceholderType_)) {
    return type;
  }
  PlaceholderType_* placeholder = type_as(PlaceholderType_, type);
  Type_* resolved_type = (Type_*)resolve_typeexpr(type->tmpl, NULL, scope, errors, scope->allocator);

  assertf(resolved_type->cls < sizeof(type_sizes) / sizeof(type_sizes[0]), "Trying to copy unknown type.");
  memcpy(placeholder, resolved_type, type_sizes[resolved_type->cls]);

  dealloc(scope->allocator, resolved_type);
  return (Type_*)placeholder;
}

size_t type_calcsize(Type_* type) {
  size_t size = 0;

  if (type_isunary(type)) {
    size = type_calcsize(type_cast(UnaryType_, type)->ty);
    type->size = size;
    return size;
  }

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

    case TYPE_CLS(UnionType_):
    {
      UnionType_* ty = type_as(UnionType_, type);
      for (ListNode_* n = ty->self.types.head; n != NULL; n = n->next) {
        size_t type_size = type_calcsize(list_val(n, Type_*));
        size = max(size, type_size);
      }
      break;
    }

    case TYPE_CLS(TupleType_):
    {
      TupleType_* ty = type_as(TupleType_, type);
      for (ListNode_* n = ty->self.types.head; n != NULL; n = n->next) {
        size += type_calcsize(list_val(n, Type_*));
      }
      break;
    }

    case TYPE_CLS(ConstraintType_):
    {
      ConstraintType_* ty = type_as(ConstraintType_, type);
      break;
    }

    // No-op for generic params.
    case TYPE_CLS(GenericParamType_):
      break;

    default:
      assertf(false, "Unknown type");
  }

  type->size = size;
  return size;
}

Type_* assert_type_is_(Type_* ty, int cls) {
  assertf(ty->cls == cls, "Type does not match %d vs. %d", ty->cls, cls);
  return ty;
}

static const Type_* type_subtype(const Type_* ty) {
  if (type_isunary(ty)) {
    return type_cast(UnaryType_, ty)->ty;
  }

  return NULL;
}

bool type_isequal(const Type_* a, const Type_* b) {
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
    return type_isequal(a_sub, b_sub);
  }

  switch (a->cls) {
    // Degenerate case, written for completeness.
    case TYPE_CLS(ClassType_):
      return a == b;

    case TYPE_CLS(ArrayType_):
    {
      ArrayType_* a_arr = type_as(ArrayType_, a);
      ArrayType_* b_arr = type_as(ArrayType_, b);

      return a_arr->count == b_arr->count && type_isequal(a_arr->el_type, b_arr->el_type);
    }

    case TYPE_CLS(FunctionType_):
    {
      FunctionType_* a_fn = type_as(FunctionType_, a);
      FunctionType_* b_fn = type_as(FunctionType_, b);

      if (a_fn->params.count != b_fn->params.count) {
        return false;
      }

      if (!type_isequal(a_fn->ret_ty, b_fn->ret_ty)) {
        return false;
      }

      ListNode_* a_param = a_fn->params.head;
      ListNode_* b_param = b_fn->params.head;
      for (int i = 0; i < a_fn->params.count; ++i) {
        if (!type_isequal(list_val(a_param, Type_*), list_val(b_param, Type_*))) {
          return false;
        }

        a_param = a_param->next;
        b_param = b_param->next;
      }

      break;
    }

    case TYPE_CLS(UnionType_):
    {
      UnionType_* union_a = type_as(UnionType_, a);
      UnionType_* union_b = type_as(UnionType_, b);

      for (ListNode_* a_n = union_a->self.types.head; a_n != NULL; a_n = a_n->next) {
        Type_* a_type = list_val(a_n, Type_*);
        for (ListNode_* b_n = union_b->self.types.head; b_n != NULL; b_n = b_n->next) {
          Type_* b_type = list_val(b_n, Type_*);
          if (!type_isequal(a_type, b_type)) {
            return false;
          }
        }
      }
      return true;
    }

    case TYPE_CLS(TupleType_):
    {
      TupleType_* tuple_a = type_as(TupleType_, a);
      TupleType_* tuple_b = type_as(TupleType_, b);
      for (ListNode_* a_n = tuple_a->self.types.head; a_n != NULL; a_n = a_n->next) {
        Type_* a_type = list_val(a_n, Type_*);
        for (ListNode_* b_n = tuple_b->self.types.head; b_n != NULL; b_n = b_n->next) {
          Type_* b_type = list_val(b_n, Type_*);
          if (!type_isequal(a_type, b_type)) {
            return false;
          }
        }
      }
      return true;
    }
  }

  assertf(false, "Unknown type when determining type equality.");
  return false;
}

static const Type_* type_subtype_or_type(const Type_* ty) {
  const Type_* sub = type_subtype(ty);
  return sub ? sub : ty;
}

bool uniontype_has(const Type_* union_ty, const Type_* ty) {
  UnionType_* union_type = type_as(UnionType_, union_ty);

  for (ListNode_* n = union_type->self.types.head; n != NULL; n = n->next) {
    Type_* el_type = list_val(n, Type_*);
    if (type_isequal(el_type, ty)) {
      return true;
    }
  }

  return false;
}

bool uniontype_isassignable(const Type_* from, const Type_* to) {
  if (type_is(from, UnionType_) && !type_is(to, UnionType_)) {
    return uniontype_has((Type_*)type_as(UnionType_, from), to);
  } else if (!type_is(from, UnionType_) && type_is(to, UnionType_)) {
    return uniontype_has((Type_*)type_as(UnionType_, to), from);
  }

  UnionType_* u_from = type_as(UnionType_, from);
  UnionType_* u_to = type_as(UnionType_, to);

  if (u_to->selected_type) {
    return u_from && type_isassignable_to(u_from->selected_type, u_to->selected_type);
  }

  if (u_from->selected_type) {
    return uniontype_has((Type_*)u_to, u_from->selected_type);
  }

  for (ListNode_* a_n = u_to->self.types.head; a_n != NULL; a_n = a_n->next) {
    Type_* a_type = list_val(a_n, Type_*);
    bool found = false;
    for (ListNode_* b_n = u_from->self.types.head; b_n != NULL; b_n = b_n->next) {
      Type_* b_type = list_val(b_n, Type_*);
      if (type_isequal(a_type, b_type)) {
        found = true;
        break;
      }
    }

    if (!found) {
      return false;
    }
  }

  return true;
}

bool type_isassignable(const Type_* ty) {
  if (type_isconst(ty) || type_isval(ty)) {
    return false;
  }

  return true;
}

bool type_isassignable_to(const Type_* from, const Type_* to) {
  // Cannot assign to a const.
  if (type_is(to, InType_) || type_is(to, ConstType_)) {
    return false;
  }

  // Return true if the type is the same, e.g. both are the same classs, or a
  // primitive can be coerced into another.
  if (from == to || type_iscoercible(from, to)) {
    return true;
  }

  // Recurse past any unary types.
  const Type_* from_sub = type_subtype(from);
  const Type_* to_sub = type_subtype(to);
  if (from_sub || to_sub) {
    from_sub = from_sub ? from_sub : from;
    to_sub = to_sub ? to_sub : to;
    return type_isassignable_to(from_sub, to_sub);
  }

  if (type_is(to, UnionType_) || type_is(from, UnionType_)) {
    return uniontype_isassignable(from, to);
  }

  switch (from->cls) {
    case TYPE_CLS(ArrayType_):
      return type_is(to, ArrayType_) &&
        type_as(ArrayType_, from)->count == type_as(ArrayType_, to)->count &&
        type_isassignable_to(type_as(ArrayType_, from)->el_type, type_as(ArrayType_, to)->el_type);

    case TYPE_CLS(FunctionType_):
      return type_isequal(from, to);
  }

  return false;
}

bool type_iscoercible(const Type_* from, const Type_* to) {
  from = type_valtype((Type_*)from);
  to = type_valtype((Type_*)to);

  if (type_is(from, ArrayType_) && type_is(to, ArrayType_)) {
    ArrayType_* arr_from = type_as(ArrayType_, from);
    ArrayType_* arr_to = type_as(ArrayType_, to);

    return arr_from->count == arr_to->count && type_iscoercible(arr_from->el_type, arr_to->el_type);
  }

  if (type_is(to, UnionType_) || type_is(from, UnionType_)) {
    return uniontype_isassignable(from, to);
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
      from->cls >= TYPE_CLS(IntType_) && from->cls <= TYPE_CLS(Int64Type_) ||
      from->cls >= TYPE_CLS(UintType_) && from->cls <= TYPE_CLS(Uint64Type_)));
}

bool type_isconst(const Type_* ty) {
  if (!ty) {
    return false;
  }

  return type_is(ty, ConstType_) || type_is(ty, InType_);
}

bool type_isval(const Type_* ty) {
  if (!ty) {
    return false;
  }

  return !type_is(ty, VarType_) && !type_is(ty, RefType_) && !type_is(ty, InType_) && !type_is(ty, OutType_);
}

bool type_isaref(const Type_* ty) {
  if (!ty) {
    return false;
  }

  return type_is(ty, RefType_) || type_is(ty, InType_) || type_is(ty, OutType_);
}

bool type_isavar(const Type_* ty) {
  return type_is(ty, VarType_);
}

struct TypeUnionIteratorState {
  uint64_t id;
};

void type_union_id_calc(const void* key, size_t ksize, uintptr_t value, void* usr) {
  struct TypeUnionIteratorState* state = (struct TypeUnionIteratorState*)usr;
}

static uint64_t type_id(const Type_* ty) {
  if (ty->id) {
    return ty->id;
  }

  // Random number to increase entropy.
  uint64_t id = 0x82fa908abb15a711;
  id = hash_incr(id, (uint64_t)ty);
  id = hash_incr(id, (uint64_t)ty->scope);
  return hash_finish(id);
}

static uint64_t type_id_calc(Type_* ty) {
  if (ty->id == 0) {
    ty->id = type_id(ty);
  }

  return ty->id;
}

const char* type_tostr(const Type_* ty) {
  switch (ty->cls) {
    case TYPE_CLS(NilType_):
      return "nil";

    case TYPE_CLS(BoolType_):
      return "int";

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

    case TYPE_CLS(ConstType_):
      return "const";

    case TYPE_CLS(InType_):
      return "in";

    case TYPE_CLS(OutType_):
      return "out";

    case TYPE_CLS(VarType_):
      return "var";

    case TYPE_CLS(RefType_):
      return "ref";

    case TYPE_CLS(ClassType_):
      return "class";

    case TYPE_CLS(ArrayType_):
      return "array";    
  }
  return "unknown";
}

void print_type(const Type_* ty) {
  switch (ty->cls) {
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

    case TYPE_CLS(FieldType_):
    {
      FieldType_* field = type_as(FieldType_, ty);
      printf("%.*s: ", field->name.length, field->name.start);
      print_type(field->self.ty);
      break;
    }

    case TYPE_CLS(UnionType_):
      printf("(");
      for (ListNode_* n = type_cast(UnionType_, ty)->self.types.head; n != NULL; n = n->next) {
        print_type(list_val(n, Type_*));
        if (n->next) {
          printf(" | ");
        }
      }
      printf(")");
      break;

    case TYPE_CLS(TupleType_):
      printf("(");
      for (ListNode_* n = type_cast(TupleType_, ty)->self.types.head; n != NULL; n = n->next) {
        print_type(list_val(n, Type_*));
        if (n->next) {
          printf(", ");
        }
      }
      printf(")");
      break;

    case TYPE_CLS(ConstraintType_):
    {
      ConstraintType_* constraint_ty = type_as(ConstraintType_, ty);
      printf("%.*s", constraint_ty->self.self.opt_name.length, constraint_ty->self.self.opt_name.start);
      if (constraint_ty->self.types.head) {
        printf(": ");
      }

      for (ListNode_* constraint_n = constraint_ty->self.types.head;
        constraint_n != NULL;
        constraint_n = constraint_n->next) {

        Type_* constraint = list_val(constraint_n, Type_*);
        print_type(constraint);

        if (constraint_n->next) {
          printf(" & ");
        }
      }      
      break;
    }
  }
}

Type_* type_valtype(Type_* ty) {
  if (type_isunary(ty)) {
    if (!type_cast(UnaryType_, ty)->ty) {
      return ty;
    }
    return type_valtype(type_cast(UnaryType_, ty)->ty);
  }

  if (type_is(ty, UnionType_)) {
    return type_cast(UnionType_, ty)->selected_type ? type_valtype(type_cast(UnionType_, ty)->selected_type) : ty;
  }

  return ty;
}

bool type_isdefined(const Type_* ty) {
  if (!ty) {
    return false;
  }

  if (type_isaprimitive(ty)) {
    return true;
  }

  if (type_isunary(ty)) {
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

void type_class_addmember(Type_* cls_ty, Token_ name, Type_* type, struct AstExpr_* opt_expr) {
  ClassTypeField_ class_field = {
    .type = type,
    .name = name,
    .opt_expr = opt_expr,
  };
  list_push(&type_as(ClassType_, cls_ty)->members, &class_field);
}

void tupletype_add(Type_* ty, Type_* new) {
  TupleType_* type = type_as(TupleType_, ty);
  list_push(&type->self.types, &new);
}

void uniontype_add(Type_* ty, Type_* new) {
  UnionType_* type = type_as(UnionType_, ty);
  list_push(&type->self.types, &new);
}

Type_* uniontype_findassignable(const Type_* ty, const Type_* assign_ty) {
  UnionType_* union_ty = type_as(UnionType_, ty);
  for (ListNode_* n = union_ty->self.types.head; n != NULL; n = n->next) {
    Type_* n_ty = list_val(n, Type_*);
    if (type_isassignable_to(assign_ty, n_ty)) {
      return n_ty;
    }
  }

  return NULL;
}

Type_* uniontype_select(const Type_* ty, const Type_* assign_ty) {
  Type_* ret = NULL;
  UnionType_* union_ty = type_as(UnionType_, ty);
  if (assign_ty) {
    ret = uniontype_findassignable(ty, assign_ty);
  } else if (uniontype_has(ty, Nil_Ty)) {
    ret = (Type_*)Nil_Ty;
  }

  return ret;
}