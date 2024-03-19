#include "type.h"

#include <stdarg.h>

#include "hash.h"
#include "symbol_table.h"
#include "random.h"

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

const Int8Type_ Int8_Ty = {
  .self = {
    .cls = TYPE_CLS(Int8Type_),
    .size = 1,
  }
};

const Int16Type_ Int16_Ty = {
  .self = {
    .cls = TYPE_CLS(Int16Type_),
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

const Uint8Type_ Uint8_Ty = {
  .self = {
    .cls = TYPE_CLS(Uint8Type_),
    .size = 1,
  }
};

const Uint16Type_ Uint16_Ty = {
  .self = {
    .cls = TYPE_CLS(Uint16Type_),
    .size = 1,
  }
};

const Uint32Type_ Uint32_Ty = {
  .self = {
    .cls = TYPE_CLS(Uint32Type_),
    .size = 1,
  }
};

const Uint64Type_ Uint64_Ty = {
  .self = {
    .cls = TYPE_CLS(Uint64Type_),
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

static Type_* type_replace_decltype(Type_* ty, Type_* replace_with, MemoryAllocator_* allocator);
static void generictype_bindargs(Type_* generic_ty, List_* type_args, Frame_* frame, Scope_* scope);

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

Type_* make_primitive_ty(int primitive_type, MemoryAllocator_* allocator) {
  Type_* ret = alloc(allocator, sizeof(Type_));
  memset(ret, 0, sizeof(UnaryType_));
  ret->cls = primitive_type;
  ret->size = type_calcsize(ret);

  return (Type_*)ret;
}

Type_* make_const_ty(Type_* sub_type, MemoryAllocator_* allocator) {
  ConstType_* ret = type_alloc_ty(allocator, ConstType_);
  ret->unary.ty = sub_type;
  return (Type_*)ret;
}

Type_* make_var_ty(Type_* sub_type, MemoryAllocator_* allocator) {
  VarType_* ret = type_alloc_ty(allocator, VarType_);
  ret->unary.ty = sub_type;
  return (Type_*)ret;
}

Type_* make_ref_ty(Type_* sub_type, MemoryAllocator_* allocator) {
  RefType_* ret = type_alloc_ty(allocator, RefType_);
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

  static const char constructor_str[] = "__constructor";
  int name_size = sizeof(constructor_str) + ret->self.opt_name.length;
  char* constructor_name = alloc(allocator, name_size);
  sprintf_s(constructor_name, name_size, "%.*s%.*s\0", ret->self.opt_name.length, ret->self.opt_name.start, name_size - 1, constructor_str);

  FunctionType_* constructor_ty = (FunctionType_*)make_function_ty(token_string("constructor"), allocator);
  constructor_ty->ret_ty = (Type_*)ret;
  constructor_ty->self.opt_name = (Token_){ .start = constructor_name, .length = name_size };
  ret->constructor = constructor_ty;

  return (Type_*)ret;
}

Type_* make_function_ty(Token_ name, MemoryAllocator_* allocator) {
  FunctionType_* ret = type_alloc_ty(allocator, FunctionType_);
  ret->self.opt_name = name;
  ret->ret_ty = (Type_*)&Nil_Ty;
  list_of(&ret->params, Type_*, allocator);
  return (Type_*)ret;
}

Type_* make_tuple_ty(MemoryAllocator_* allocator, int n, ...) {
  va_list args;
  va_start(args, n);

  TupleType_* ret = type_alloc_ty(allocator, TupleType_);
  list_of(&ret->self.types, Type_*, allocator);

  for (int i = 0; i < n; ++i) {
    Type_* ty = va_arg(args, Type_*);
    list_push(&ret->self.types, &ty);
  }

  va_end(args);
  return (Type_*)ret;
}

Type_* make_union_ty(MemoryAllocator_* allocator, int n, ...) {
  va_list args;
  va_start(args, n);

  UnionType_* ret = type_alloc_ty(allocator, UnionType_);
  list_of(&ret->self.types, Type_*, allocator);

  for (int i = 0; i < n; ++i) {
    Type_* ty = va_arg(args, Type_*);
    list_push(&ret->self.types, &ty);
  }

  va_end(args);
  return (Type_*)ret;
}

Type_* make_constraint_ty(MemoryAllocator_* allocator, Token_ name, int n, ...) {
  va_list args;
  va_start(args, n);

  ConstraintType_* ret = type_alloc_ty(allocator, ConstraintType_);
  ret->self.self.opt_name = name;
  list_of(&ret->self.types, Type_*, allocator);

  for (int i = 0; i < n; ++i) {
    Type_* ty = va_arg(args, Type_*);
    list_push(&ret->self.types, &ty);
  }

  va_end(args);
  return (Type_*)ret;
}

Type_* make_field_ty(Token_ field_name, Type_* sub_type, MemoryAllocator_* allocator) {
  FieldType_* ret = type_alloc_ty(allocator, FieldType_);
  ret->name = field_name;
  ret->self.ty = sub_type;

  return (Type_*)ret;
}

Type_* make_array_or_generic_ty(Token_ name, MemoryAllocator_* allocator) {
  GenericOrArrayType_* ret = type_alloc_ty(allocator, GenericOrArrayType_);
  ret->unary.self.opt_name = name;
  list_of(&ret->args, TypeArgument_, allocator);

  return (Type_*)ret;
}

Type_* make_generic_ty(Type_* prototype, List_* type_params, struct Scope_* scope, MemoryAllocator_* allocator) {
  GenericType_* ret = type_alloc_ty(allocator, GenericType_);
  ret->unary.self.opt_name = prototype->opt_name;
  ret->prototype = prototype;
  ret->params = *type_params;
  ret->scope = scope;
  return (Type_*)ret;
}

Type_* make_genericimpl_ty(Type_* generic_ty, ListOf_(TypeArgument_)* type_args, struct Scope_* scope, MemoryAllocator_* allocator) {
  GenericImplType_* ret = type_alloc_ty(allocator, GenericImplType_);
  ret->unary.self.opt_name = generic_ty->opt_name;
  ret->generic_type = type_as(GenericType_, generic_ty);
  ret->args_count = ret->generic_type->params.count;
  ret->args = alloc(allocator, ret->args_count * sizeof(ret->args[0]));
  ret->scope = scope_createfrom(scope);

  size_t index = 0;
  for (ListNode_* arg = type_args->head; arg != NULL; arg = arg->next) {
    TypeArgument_ type_arg = list_val(arg, TypeArgument_);
    assertf(type_arg.is_type, "Generics only support type arguments.");

    ret->args[index] = type_arg.type;
    ++index;
  }

  generictype_bindargs(generic_ty, type_args, ret->scope->frame, ret->scope);

  return (Type_*)ret;
}

Type_* type_alloc(MemoryAllocator_* allocator, int type_cls, size_t type_size) {
  Type_* ret =  alloc(allocator, type_size);
  memset(ret, 0, type_size);
  ret->cls = type_cls;
  return (Type_*)ret;
}

bool type_isunknown(const Type_* ty) {
  return !ty->opt_name.start && type_is(type_deref((Type_*)ty), UnknownType_);
}

bool type_isplaceholder(const Type_* ty) {
  return ty->opt_name.start && type_is(type_deref((Type_*)ty), UnknownType_);
}

bool type_isaprimitive(const Type_* ty) {
  ty = type_deref((Type_*)ty);
  return ty->cls > __TYPE_PRIMITIVE_START__ && ty->cls < __TYPE_PRIMITIVE_END__;
}

bool type_isunary(const Type_* ty) {
  ty = type_deref((Type_*)ty);
  return ty->cls > __TYPE_UNARY_START__ && ty->cls < __TYPE_UNARY_END__;
}

bool type_ismultitype(const Type_* ty) {
  ty = type_deref((Type_*)ty);
  return ty->cls == TYPE_CLS(TupleType_) || ty->cls == TYPE_CLS(UnionType_);
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

static bool analyze_generic(const Type_* generic_ty, List_* type_args, Scope_* scope) {
  GenericType_* generic = type_as(GenericType_, generic_ty);
  List_* type_params = &generic->params;

  if (type_params->count != type_args->count) {
    printf("Type parameters count do not given argument count.");
    return false;
  }

  ListNode_* param_n = type_params->head;
  for (ListNode_* arg_n = type_args->head; arg_n != NULL; arg_n = arg_n->next) {
    ConstraintType_* constraints = list_val(param_n, ConstraintType_*);
    TypeArgument_* arg = list_ptr(arg_n, TypeArgument_);

    // TODO: implement expressions as generic parameter?
    if (!arg->is_type) {
      printf("Type argument is not type");
      return false;
    }

    for (ListNode_* constraint_n = constraints->self.types.head;
      constraint_n != NULL;
      constraint_n = constraint_n->next) {
      Type_* ty = list_val(constraint_n, Type_*);
      type_resolve(ty, scope);
    }

    Type_* arg_ty = arg->type;
    if (!constrainttype_satisfied(constraints, arg_ty)) {
      return false;
    }

    param_n = param_n->next;
  }

  return true;
}

static void generictype_bindargs(Type_* generic_ty, List_* type_args, Frame_* frame, Scope_* scope) {
  GenericType_* generic = type_as(GenericType_, generic_ty);
  List_* type_params = &generic->params;

  ListNode_* param_n = type_params->head;
  for (ListNode_* arg_n = type_args->head; arg_n != NULL; arg_n = arg_n->next) {
    TypeArgument_* arg = list_ptr(arg_n, TypeArgument_);
    ConstraintType_* constraints = list_val(param_n, ConstraintType_*);
    frame_addtype(frame, &constraints->self.self.opt_name, arg->type, scope);
    param_n = param_n->next;
  }
}

Type_* generictype_specialize(Type_* ty, ListOf_(TypeArgument_)* type_args, Token_ name_tk, MemoryAllocator_* allocator, Scope_* scope) {
  for (ListNode_* arg_n = type_args->head; arg_n != NULL; arg_n = arg_n->next) {
    TypeArgument_* arg = list_ptr(arg_n, TypeArgument_);
    type_resolve(arg->type, scope);
    if (type_is(type_valtype(arg->type), ConstraintType_)) {
      return ty;
    }
  }

  GenericImplType_* impl = NULL;
  if (type_is(ty, GenericImplType_)) {
    impl = type_cast(GenericImplType_, ty);
  } else {
    impl = type_cast(GenericImplType_, make_genericimpl_ty(ty, type_args, scope, allocator));
  }

  if (impl->unary.ty) {
    return impl->unary.ty;
  }

  bool can_specialize = true;
  for (size_t i = 0; i < impl->args_count; ++i) {
    can_specialize &= type_resolve(impl->args[i], scope);// && !type_is(type_valtype(impl->args[i]), ConstraintType_);
  }

  if (!can_specialize) {
    return (Type_*)impl;
  }

  Type_* ret = NULL;
  GenericType_* generic_ty = impl->generic_type;
  Type_* prototype = type_valtype(generic_ty->prototype);

  switch (prototype->cls) {
    case TYPE_CLS(ClassType_):
    {
      const ClassType_* proto = type_as(ClassType_, prototype);
      ClassType_* cls_ty = (ClassType_*)make_class_ty(name_tk, allocator);
      cls_ty->scope = scope_createfrom(proto->scope);

      if (type_args) {
        generictype_bindargs((Type_*)generic_ty, type_args, cls_ty->scope->frame, cls_ty->scope);
      }

      for (ListNode_* member = proto->members.head; member != NULL; member = member->next) {
        ClassTypeField_* field = list_ptr(member, ClassTypeField_);
        Type_* field_ty = make_placeholder_ty(type_valtype(field->type)->opt_name, allocator);
        type_resolve(field_ty, cls_ty->scope);
        Type_* val_ty = type_valtype(field_ty);
        Type_* new_field_ty = field_ty;


        switch (val_ty->cls) {
          case TYPE_CLS(GenericType_):
          {
            GenericType_* val_generic_ty = type_cast(GenericType_, val_ty);

            ListOf_(TypeArgument_) args;
            list_of(&args, TypeArgument_, &DefaultAllocator);

            for (ListNode_* p = val_generic_ty->params.head; p != NULL; p = p->next) {
              ConstraintType_* param = list_val(p, ConstraintType_*);
              Symbol_* sym = scope_find(cls_ty->scope, &param->self.self.opt_name);
              assertf(sym->type == SYMBOL_CLS_TYPE, "Symbol is not a type");

              TypeArgument_ arg = {
                .type = sym->ty,
                .is_type = true,
              };

              list_push(&args, &arg);
            }

            new_field_ty = generictype_specialize(val_ty, &args, field_ty->opt_name, allocator, cls_ty->scope);
            list_clear(&args);
            break;
          }

          case TYPE_CLS(ConstraintType_):
          {
            break;
          }
        }

        new_field_ty = type_replace_decltype(field->type, new_field_ty, allocator);
        type_class_addmember((Type_*)cls_ty, field->name, new_field_ty, NULL);
      }

      ret = (Type_*)cls_ty;
      type_calcsize(ret);
      type_class_calcoffsets(ret);

      break;
    }

    case TYPE_CLS(UnionType_):
    {
      const UnionType_* proto = type_as(UnionType_, prototype);
      UnionType_* union_ty = (UnionType_*)make_union_ty(allocator, 0);

      Type_* selected_type = NULL;
      for (ListNode_* n = proto->self.types.head; n != NULL; n = n->next) {
        Type_* union_subty = list_val(n, Type_*);
        Type_* val_ty = type_valtype(union_subty);

        if (type_is(val_ty, ConstraintType_)) {
          Symbol_* sym = scope_find(impl->scope, &union_subty->opt_name);
          assertf(sym, "Could not find type");
          union_subty = sym->ty;
        } else if (type_is(val_ty, GenericType_)) {
          GenericType_* val_generic_ty = type_cast(GenericType_, val_ty);

          ListOf_(TypeArgument_) args;
          list_of(&args, TypeArgument_, &DefaultAllocator);

          for (ListNode_* p = val_generic_ty->params.head; p != NULL; p = p->next) {
            ConstraintType_* param = list_val(p, ConstraintType_*);
            Symbol_* sym = scope_find(impl->scope, &param->self.self.opt_name);
            assertf(sym->type == SYMBOL_CLS_TYPE, "Symbol is not a type");

            TypeArgument_ arg = {
              .type = sym->ty,
              .is_type = true,
            };

            list_push(&args, &arg);
          }

          union_subty = generictype_specialize(val_ty, &args, union_subty->opt_name, allocator, impl->scope);
          list_clear(&args);
        } else if (type_is(val_ty, GenericImplType_)) {
          GenericImplType_* impl_ty = type_as(GenericImplType_, val_ty);
          if (impl_ty->unary.ty) {
            union_subty = impl_ty->unary.ty;
          } else {
            GenericImplType_* val_generic_ty = type_cast(GenericImplType_, val_ty);

            ListOf_(TypeArgument_) args;
            list_of(&args, TypeArgument_, &DefaultAllocator);

            for (ListNode_* p = val_generic_ty->generic_type->params.head; p != NULL; p = p->next) {
              ConstraintType_* param = list_val(p, ConstraintType_*);
              Symbol_* sym = scope_find(impl->scope, &param->self.self.opt_name);
              assertf(sym->type == SYMBOL_CLS_TYPE, "Symbol is not a type");

              TypeArgument_ arg = {
                .type = sym->ty,
                .is_type = true,
              };

              list_push(&args, &arg);
            }

            union_subty = generictype_specialize(val_ty, &args, union_subty->opt_name, allocator, impl->scope);
            val_generic_ty->unary.ty = (Type_*)union_subty;
            list_clear(&args);
          }
        }

        list_push(&union_ty->self.types, &union_subty);
      }

      ret = (Type_*)union_ty;

      break;
    }
    
    case TYPE_CLS(FunctionType_):
      break;

    case TYPE_CLS(ConstraintType_):
    {
      assertf(type_is(type_valtype(prototype), ConstraintType_), "Could not specialize.");
      Symbol_* sym = scope_find(impl->scope, &prototype->opt_name);
      assertf(sym, "Could not find type");
      ret = sym->ty;
      break;
    }

    case TYPE_CLS(GenericType_):
    {
      ret = generictype_specialize(prototype, type_args, prototype->opt_name, allocator, impl->scope);
      break;
    }

    default:
      assertf(false, "Trying to specialize an unsupported type for generics.");
      break;
  }

#if 0
  GenericType_* generic = type_as(GenericType_, ty);
  List_* type_params = &generic->params;

  //Token_ name = token_concat(name_tk, args_tk, allocator);
  Type_* prototype = generic->prototype;
  Type_* ret = NULL;

  switch (prototype->cls) {
    case TYPE_CLS(ClassType_):
    {
      ClassType_* proto = type_as(ClassType_, prototype);
      ClassType_* cls_ty = (ClassType_*)make_class_ty(name_tk, allocator);
      cls_ty->scope = scope_createfrom(proto->scope);
      
      if (type_args) {
        generictype_bindargs(generic_ty, type_args, cls_ty->scope->frame, cls_ty->scope);
      }

      for (ListNode_* member = proto->members.head; member != NULL; member = member->next) {
        ClassTypeField_* field = list_ptr(member, ClassTypeField_);
        Type_* field_ty = make_placeholder_ty(type_valtype(field->type)->opt_name, allocator);
        type_class_addmember((Type_*)cls_ty, field->name, field_ty, NULL);
        type_resolve(field_ty, scope);

        Type_* val_ty = type_valtype(field_ty);
        if (type_is(val_ty, GenericType_)) {
          generictype_specialize(val_ty, NULL, field_ty->opt_name, allocator, cls_ty->scope);
        }
      }

      //generictype_findimpl(generic_ty, cls_ty->scope);
      ret = (Type_*)cls_ty;
      break;
    }

    case TYPE_CLS(FunctionType_):
      break;
    case TYPE_CLS(Type_):
      break;
  }
#endif

  type_calcsize(ret);

  return ret;
}

// Fills all placeholders in the given type with types found starting at the
// given scope.
bool type_resolve(Type_* type, struct Scope_* scope) {
  if (type_isaprimitive(type)) {
    return true;
  }

  if (type_isunary(type) && type_cast(UnaryType_, type)->ty) {
    return type_resolve(type_cast(UnaryType_, type)->ty, scope);
  }

  if (type_ismultitype(type)) {
    bool ret = true;
    for (ListNode_* n = type_cast(MultiType_, type)->types.head; n != NULL; n = n->next) {
      ret &= type_resolve(list_val(n, Type_*), scope);
    }
    return ret;
  }

  if (type_is(type, UnknownType_)) {
    UnaryType_* placeholder = type_cast(UnaryType_, type);
    assertf(placeholder->ty == NULL, "Encountered unknown type with filled type.");
    assertf(type->opt_name.start, "Encountered an unknown type without a type name.");

    Symbol_* sym = scope_search_to_root(scope, &type->opt_name);
    if (!sym) {
      return false;
    }
    //if (!type_is(sym->ty, ConstraintType_)) {
      type_set(type, sym->ty);
    //}
  }

  switch (type->cls) {
    case TYPE_CLS(Type_):
      return type_resolve(type_cast(UnaryType_, type)->ty, scope);

    case TYPE_CLS(ClassType_):
    {
      ClassType_* class_ty = type_as(ClassType_, type);
      bool ret = true;
      for (ListNode_* n = class_ty->members.head; n != NULL; n = n->next) {
        ret = ret && type_resolve(list_val(n, Type_*), scope);
      }
      return ret;
    }

    case TYPE_CLS(ArrayType_):
    {
      ArrayType_* ty = type_as(ArrayType_, type);
      return type_resolve(ty->el_type, scope);
    }

    case TYPE_CLS(GenericOrArrayType_):
    {
      if (type_cast(UnaryType_, type)->ty) {
        return true;
      }

      Symbol_* sym = scope_search_to_root(scope, &type->opt_name);
      if (!sym) {
        return false;
      }

      GenericOrArrayType_* ty = type_as(GenericOrArrayType_, type);

      if (type_is(sym->ty, ArrayType_)) {
        type_cast(UnaryType_, type)->ty = sym->ty;
      } else {
        GenericType_* generic_ty = type_as(GenericType_, sym->ty);        
        if (!analyze_generic((Type_*)generic_ty, &ty->args, scope)) {
          return false;
        }
        type_cast(UnaryType_, type)->ty = generictype_specialize(
          (Type_*)generic_ty, &ty->args, type->opt_name, scope->allocator, scope);
      }
      return true;
    }

    case TYPE_CLS(ConstraintType_):
    {
      ConstraintType_* ty = type_as(ConstraintType_, type);
      return true;
    }
  }

  return true;
}

void type_set(Type_* type, Type_* new_type) {
  assertf(type_is(type, UnknownType_), "Trying to set an already resolved typed");
  assertf(type_cast(UnaryType_, type)->ty == NULL, "Trying to set an already resolved typed");
  
  type->cls = TYPE_CLS(Type_);
  type_cast(UnaryType_, type)->ty = new_type;
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

    case TYPE_CLS(GenericOrArrayType_):
    {
      GenericOrArrayType_* ty = type_as(GenericOrArrayType_, type);
      size = type_calcsize(ty->unary.ty);
      break;
    }

    case TYPE_CLS(GenericType_):
    {
      break;
    }

    case TYPE_CLS(GenericImplType_):
    {
      GenericImplType_* ty = type_as(GenericImplType_, type);
      size = type_calcsize(ty->unary.ty);
      break;
    }

    case TYPE_CLS(ConstraintType_):
    {
      ConstraintType_* ty = type_as(ConstraintType_, type);
      break;
    }

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
  if (type_is(ty, Type_) || type_isunary(ty)) {
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

  // If we recurse past any unary types but are still left with placeholders,
  // then the placeholders haven't been resolved out yet. The assignability is
  // still unknown.
  if (type_is(from, Type_) || type_is(to, Type_)) {
    return false;
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

struct TypeUnionIteratorState {
  uint64_t id;
};

void type_union_id_calc(const void* key, size_t ksize, uintptr_t value, void* usr) {
  struct TypeUnionIteratorState* state = (struct TypeUnionIteratorState*)usr;
}

static uint64_t type_id_recur(const Type_* ty, uint64_t id) {
  if (!ty || type_is(ty, Type_) || type_is(ty, UnknownType_)) {
    return 0;
  }

  id = hash_incr(id, ty->cls + 1);

  if (type_isaprimitive(ty)) {
    return id;
  } else if (type_isunary(ty)) {
    return type_id_recur(type_cast(UnaryType_, ty)->ty, id);
  }

  switch (ty->cls) {
    case TYPE_CLS(ClassType_):
      id = hash_incr(id, (uint64_t)ty);
      break;

    case TYPE_CLS(FunctionType_):
      id = hash_incr(id, (uint64_t)ty);
      break;

    case TYPE_CLS(ArrayType_):
      id = hash_incr(id, type_id_recur(type_as(ArrayType_, ty)->el_type, id));
      id = hash_incr(id, type_as(ArrayType_, ty)->count);
      break;

    case TYPE_CLS(UnionType_):
    {
      uint64_t tmp = id;
      for (ListNode_* n = type_cast(UnionType_, ty)->self.types.head; n != NULL; n = n->next) {
        uint64_t sub_id = type_id_recur(list_val(n, Type_*), id);
        if (sub_id == 0) {
          return 0;
        }
        tmp += sub_id;
      }
      id = hash_incr(id, tmp);
      break;
    }

    case TYPE_CLS(TupleType_):
      for (ListNode_* n = type_cast(TupleType_, ty)->self.types.head; n != NULL; n = n->next) {
        id = type_id_recur(list_val(n, Type_*), id);
      }
      break;
  }

  return id;
}

uint64_t type_id(const Type_* ty) {
  if (ty->id) {
    return ty->id;
  }

  uint64_t id = type_id_recur(ty, 0);
  return hash_finish(id);
}

uint64_t type_id_calc(Type_* ty) {
  if (ty->id == 0) {
    ty->id = type_id(ty);
  }

  return ty->id;
}

// uint64_t type_list_calc(List)

const char* type_name(const Type_* ty) {
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

    case TYPE_CLS(GenericType_):
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
  if (type_is(ty, Type_) || type_isunary(ty)) {
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

static Type_* type_replace_decltype(Type_* ty, Type_* replace_with, MemoryAllocator_* allocator) {
  Type_* ret = NULL;
  Type_* sub_ty = NULL;

  if (!ty) {
    return replace_with;
  }

  if (ty->cls > __TYPE_DECLS_START__ && ty->cls < __TYPE_DECLS_END__) {    
    sub_ty = type_replace_decltype(type_cast(UnaryType_, ty)->ty, replace_with, allocator);
  } else {
    return replace_with;
  }

  switch (ty->cls) {
    case TYPE_CLS(ConstType_):
      return make_const_ty(sub_ty, allocator);

    case TYPE_CLS(InType_):
      return make_in_ty(sub_ty, allocator);
    
    case TYPE_CLS(OutType_):
      return make_out_ty(sub_ty, allocator);
    
    case TYPE_CLS(VarType_):
      return make_var_ty(sub_ty, allocator);
    
    case TYPE_CLS(RefType_):
      return make_ref_ty(sub_ty, allocator);
  }

  assertf(false, "Could not replace decltype. Unknown type: %d", ty->cls);
  return NULL;
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
  } else if (uniontype_has(ty, (const Type_*)&Nil_Ty)) {
    ret = (Type_*)&Nil_Ty;
  }

  return ret;
}