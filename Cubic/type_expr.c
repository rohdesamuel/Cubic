#include "type_expr.h"

#include "common.h"
#include "errors.h"
#include "memory.h"
#include "type.h"
#include "symbol_table.h"
#include "map.h"
#include "hash.h"

#include <malloc.h>


#define DEF_PRIMITIVE_TYPE_EXPR(NAME, TK, STR) \
static TypeExprPrimitive_ NAME##_TypeExpr_ = \
  { .base = { .cls = TYPE_EXPR_CLS(TypeExprPrimitive_), .name = { .type = TK, .start = STR, .length = sizeof(STR) - 1 } } };

#define DEF_PRIMITIVE_TYPE_EXPR_PTR(NAME) const TypeExpr_* NAME##_TypeExpr = (const TypeExpr_*)&NAME##_TypeExpr_;

DEF_PRIMITIVE_TYPE_EXPR(Nil,    TK_NIL,         "nil");
DEF_PRIMITIVE_TYPE_EXPR(Bool,   TK_BOOL,        "bool");
DEF_PRIMITIVE_TYPE_EXPR(Int,    TK_INT,         "int");
DEF_PRIMITIVE_TYPE_EXPR(Int8,   TK_INT8,        "int8");
DEF_PRIMITIVE_TYPE_EXPR(Int16,  TK_INT16,       "int16");
DEF_PRIMITIVE_TYPE_EXPR(Int32,  TK_INT32,       "int32");
DEF_PRIMITIVE_TYPE_EXPR(Int64,  TK_INT64,       "int64");
DEF_PRIMITIVE_TYPE_EXPR(Uint,   TK_UINT,        "uint");
DEF_PRIMITIVE_TYPE_EXPR(Uint8,  TK_UINT8,       "uint8");
DEF_PRIMITIVE_TYPE_EXPR(Uint16, TK_UINT16,      "uint16");
DEF_PRIMITIVE_TYPE_EXPR(Uint32, TK_UINT32,      "uint32");
DEF_PRIMITIVE_TYPE_EXPR(Uint64, TK_UINT64,      "uint64");
DEF_PRIMITIVE_TYPE_EXPR(Float,  TK_FLOAT,       "float");
DEF_PRIMITIVE_TYPE_EXPR(Double, TK_DOUBLE,      "double");
DEF_PRIMITIVE_TYPE_EXPR(String, TK_STRING_TYPE, "string");

DEF_PRIMITIVE_TYPE_EXPR_PTR(Nil);
DEF_PRIMITIVE_TYPE_EXPR_PTR(Bool);
DEF_PRIMITIVE_TYPE_EXPR_PTR(Int);
DEF_PRIMITIVE_TYPE_EXPR_PTR(Int8);
DEF_PRIMITIVE_TYPE_EXPR_PTR(Int16);
DEF_PRIMITIVE_TYPE_EXPR_PTR(Int32);
DEF_PRIMITIVE_TYPE_EXPR_PTR(Int64);
DEF_PRIMITIVE_TYPE_EXPR_PTR(Uint);
DEF_PRIMITIVE_TYPE_EXPR_PTR(Uint8);
DEF_PRIMITIVE_TYPE_EXPR_PTR(Uint16);
DEF_PRIMITIVE_TYPE_EXPR_PTR(Uint32);
DEF_PRIMITIVE_TYPE_EXPR_PTR(Uint64);
DEF_PRIMITIVE_TYPE_EXPR_PTR(Float);
DEF_PRIMITIVE_TYPE_EXPR_PTR(Double);
DEF_PRIMITIVE_TYPE_EXPR_PTR(String);


#define alloc_typeexpr(ALLOCATOR, TYPE) \
((TYPE*)make_typeexpr(TYPE_EXPR_CLS(TYPE), sizeof(TYPE), ALLOCATOR))

static const Type_* resolve_typeexpr_recur(const TypeExpr_* type, Scope_* scope, Hashmap* type_env, ErrorsContainer_* errors, MemoryAllocator_* allocator);

void type_expr_init() {
  Nil_TypeExpr_.primitive_ty = Nil_Ty;
  Bool_TypeExpr_.primitive_ty = Bool_Ty;
  Int_TypeExpr_.primitive_ty = Int_Ty;
  Int8_TypeExpr_.primitive_ty = Int8_Ty;
  Int16_TypeExpr_.primitive_ty = Int16_Ty;
  Int32_TypeExpr_.primitive_ty = Int32_Ty;
  Int64_TypeExpr_.primitive_ty = Int64_Ty;
  Uint_TypeExpr_.primitive_ty = Uint_Ty;
  Uint8_TypeExpr_.primitive_ty = Uint8_Ty;
  Uint16_TypeExpr_.primitive_ty = Uint16_Ty;
  Uint32_TypeExpr_.primitive_ty = Uint32_Ty;
  Uint64_TypeExpr_.primitive_ty = Uint64_Ty;
  Float_TypeExpr_.primitive_ty = Float_Ty;
  Double_TypeExpr_.primitive_ty = Double_Ty;
  String_TypeExpr_.primitive_ty = String_Ty;
}

static TypeExpr_* make_typeexpr(int cls, size_t cls_size, MemoryAllocator_* allocator) {
  TypeExpr_* ret = alloc(allocator, cls_size);
  ret->cls = cls;

  return (TypeExpr_*)ret;
}

const TypeExpr_* make_symbol_typeexpr(Token_ tk, MemoryAllocator_* allocator) {
  TypeExprSymbol_* ret = alloc_typeexpr(allocator, TypeExprSymbol_);
  ret->base.name = token_string("symbol");
  ret->symbol = tk;

  return (TypeExpr_*)ret;
}

const TypeExpr_* make_union_typeexpr(ListOf_(const TypeExpr_*)* types, MemoryAllocator_* allocator) {
  TypeExprUnion_* ret = alloc_typeexpr(allocator, TypeExprUnion_);
  ret->base.name = token_string("union");
  ret->types = *types;

  return (TypeExpr_*)ret;
}

const TypeExpr_* make_tuple_typeexpr(ListOf_(const TypeExpr_*)* types, MemoryAllocator_* allocator) {
  TypeExprTuple_* ret = alloc_typeexpr(allocator, TypeExprTuple_);
  ret->base.name = token_string("tuple");
  ret->types = *types;

  return (TypeExpr_*)ret;
}

const TypeExpr_* make_in_typeexpr(const TypeExpr_* type, MemoryAllocator_* allocator) {
  TypeExprIn_* ret = alloc_typeexpr(allocator, TypeExprIn_);
  ret->base.name = token_string("in");
  ret->type = type;

  return (TypeExpr_*)ret;
}

const TypeExpr_* make_out_typeexpr(const TypeExpr_* type, MemoryAllocator_* allocator) {
  TypeExprOut_* ret = alloc_typeexpr(allocator, TypeExprOut_);
  ret->base.name = token_string("out");
  ret->type = type;

  return (TypeExpr_*)ret;
}

const TypeExpr_* make_var_typeexpr(const TypeExpr_* type, MemoryAllocator_* allocator) {
  TypeExprVar_* ret = alloc_typeexpr(allocator, TypeExprVar_);
  ret->base.name = token_string("var");
  ret->type = type;

  return (TypeExpr_*)ret;
}

const TypeExpr_* make_constraint_typeexpr(Token_ name, ListOf_(const TypeExpr_*)* types, MemoryAllocator_* allocator) {
  TypeExprConstraint_* ret = alloc_typeexpr(allocator, TypeExprConstraint_);
  ret->base.name = token_string("constraint");
  ret->name = name;
  ret->constraints = *types;

  return (TypeExpr_*)ret;
}

const TypeExpr_* make_function_typeexpr(Token_ name, const TypeExpr_* return_type,
  ListOf_(const TypeExpr_*)* params, ListOf_(const TypeExprGenericParam_*)* type_params,
  MemoryAllocator_* allocator) {

  TypeExprFunction_* ret = alloc_typeexpr(allocator, TypeExprFunction_);
  ret->base.name = token_string("function");

  if (type_params) {
    ret->base.params = *type_params;
  }

  ret->name = name;
  ret->ret_type = return_type;
  ret->params = *params;

  if (type_params) {
    for (ListNode_* n = type_params->head; n != NULL; n = n->next) {
      assert(list_val(n, TypeExprGenericParam_*)->base.cls == TYPE_EXPR_CLS(TypeExprGenericParam_));
    }
  }

  return (const TypeExpr_*)ret;
}

const TypeExpr_* make_class_typeexpr(Token_ name,
  ListOf_(const TypeExprClassMember_*)* members, ListOf_(const TypeExprGenericParam_*)* type_params,
  MemoryAllocator_* allocator) {

  TypeExprClass_* ret = alloc_typeexpr(allocator, TypeExprClass_);
  ret->base.name = token_string("class");
  ret->base.params = *type_params;

  ret->name = name;
  ret->members = *members;

  for (ListNode_* n = type_params->head; n != NULL; n = n->next) {
    assert(list_val(n, TypeExprGenericParam_*)->base.cls == TYPE_EXPR_CLS(TypeExprGenericParam_));
  }

  for (ListNode_* n = members->head; n != NULL; n = n->next) {
    assert(list_val(n, TypeExprClassMember_*)->base.cls == TYPE_EXPR_CLS(TypeExprClassMember_));
  }

  return (const TypeExpr_*)ret;
}

const TypeExpr_* make_class_member_typeexpr(Token_ field_name, const TypeExpr_* field_type,
  MemoryAllocator_* allocator) {

  TypeExprClassMember_* ret = alloc_typeexpr(allocator, TypeExprClassMember_);
  ret->base.name = token_string("class_member");
  ret->name = field_name;
  ret->type = field_type;

  return (const TypeExpr_*)ret;
}

const TypeExpr_* make_genericparam_typeexpr(Token_ name, const TypeExpr_* type, MemoryAllocator_* allocator) {
  TypeExprGenericParam_* ret = alloc_typeexpr(allocator, TypeExprGenericParam_);
  ret->base.name = token_string("generic_param");
  ret->name = name;
  ret->type = type;

  return (const TypeExpr_*)ret;
}

const TypeExpr_* make_generic_or_array_typeexpr(const TypeExpr_* prefix, ListOf_(TypeExpr_*)* args, MemoryAllocator_* allocator) {
  TypeExprGenericOrArrayType_* ret = alloc_typeexpr(allocator, TypeExprGenericOrArrayType_);
  ret->base.name = token_string("generic_or_array");
  ret->prefix = prefix;
  ret->args = *args;

  return (const TypeExpr_*)ret;
}

const TypeExpr_* make_primary_typeexpr(TokenType_ type, Value_ val, MemoryAllocator_* allocator) {
  TypeExprPrimary_* ret = alloc_typeexpr(allocator, TypeExprPrimary_);
  ret->base.name = token_string("primary");
  ret->type = type;
  ret->value = val;

  return (const TypeExpr_*)ret;
}

const TypeExpr_* make_function_call_typeexpr(const TypeExpr_* prefix, MemoryAllocator_* allocator) {
  TypeExprFunctionCall_* ret = alloc_typeexpr(allocator, TypeExprFunctionCall_);
  ret->base.name = token_string("function_call");
  ret->prefix = prefix;

  return (const TypeExpr_*)ret;
}

static void bind_type_params(const TypeExpr_* type, ListOf_(TypeExpr_*)* args, Hashmap* type_env) {
  if (args) {
    ListNode_* p = type->params.head;
    for (ListNode_* n = args->head; n != NULL; n = n->next) {
      const TypeExpr_* arg = list_val(n, TypeExpr_*);
      const TypeExprGenericParam_* type = list_val(n, const TypeExprGenericParam_*);

      hashmap_set(type_env, type->name.start, type->name.length, (uintptr_t)arg);

      p = p->next;
    }
  } else {
    for (ListNode_* p = type->params.head; p != NULL; p = p->next) {
      const TypeExprGenericParam_* param = list_val(p, const TypeExprGenericParam_*);
      const TypeExpr_* type_expr = list_val(p, const TypeExpr_*);

      assert(type_expr->cls == TYPE_EXPR_CLS(TypeExprGenericParam_));

      const TypeExpr_* maybe_exists = NULL;
      hashmap_get(type_env, param->name.start, param->name.length, (uintptr_t*)&maybe_exists);
      if (maybe_exists && maybe_exists->cls == TYPE_EXPR_CLS(TypeExprGenericParam_)) {
        hashmap_set(type_env, param->name.start, param->name.length, (uintptr_t)type_expr);
      }
    }
  }
}

static const Type_* apply_template(const TypeExpr_* tmpl, const ListOf_(const TypeExpr_*)* args,
  Scope_* scope, Hashmap* type_env, ErrorsContainer_* errors, MemoryAllocator_* allocator) {

  // Copy the env to scope the type arguments to this class.
  Hashmap* new_env = hashmap_copy(type_env);

  // Bind the given type arguments to the type environment.
  ListNode_* param_n = tmpl->params.head;
  for (ListNode_* arg_n = args->head; arg_n != NULL && param_n != NULL; arg_n = arg_n->next) {
    const TypeExpr_* arg = list_val(arg_n, const TypeExpr_*);
    const TypeExprGenericParam_* param = (TypeExprGenericParam_*)list_val(param_n, const TypeExpr_*);
    assert(param->base.cls == TYPE_EXPR_CLS(TypeExprGenericParam_));

    hashmap_set(new_env, param->name.start, param->name.length, (uintptr_t)arg);

    param_n = param_n->next;
  }

  // Using the prefix template, monomorphize and return the new type.
  const Type_* ret = resolve_typeexpr_recur(tmpl, scope, new_env, errors, allocator);
  hashmap_free(new_env);
  return ret;
}

thread_local struct TypeArena_ {
  Type_** types;
  int count;
  int capacity;

  bool in_use;
} type_arena = { 0 };

Type_** alloc_type_array(int count) {
  assertf(!type_arena.in_use, "Trying to alloc type array while already in use.");

  if (type_arena.capacity < count) {
    int oldCapacity = type_arena.capacity;
    type_arena.capacity = GROW_CAPACITY(oldCapacity);
    type_arena.types = GROW_ARRAY(Type_*, type_arena.types,
      oldCapacity, type_arena.capacity);
  }
  type_arena.count = count;
  type_arena.in_use = true;
  return type_arena.types;
}

void return_type_array() {
  type_arena.in_use = false;
}

static void resolve_type_args(const TypeExpr_* type, Type_** resolved_args, Scope_* scope, Hashmap* type_env, ErrorsContainer_* errors, MemoryAllocator_* allocator) {
  Hashmap* temp_env = hashmap_copy(type_env);
  Scope_* temp_scope = scope_createfrom(scope);
  int param_index = 0;
  for (ListNode_* n = type->params.head; n != NULL; n = n->next) {
    const TypeExprGenericParam_* param = list_val(n, const TypeExprGenericParam_*);

    // Do not overwrite already bound type arguments. The previous step
    // should have bound the resolved arguments into the type environment
    // already.
    const TypeExpr_* existing_arg = NULL;
    const Type_* arg_ty = NULL;
    hashmap_get(type_env, param->name.start, param->name.length, (uintptr_t*)&existing_arg);
    if (existing_arg) {
      arg_ty = resolve_typeexpr_recur(existing_arg, temp_scope, temp_env, errors, allocator);
    } else {
      // If here, the type parameter has not been resolved so class members
      // need access to the param at the scope level.
      arg_ty = resolve_typeexpr_recur((const TypeExpr_*)param, temp_scope, temp_env, errors, allocator);
    }
    frame_addtype(temp_scope->frame, (Token_*)&param->name, (Type_*)arg_ty, temp_scope);
    resolved_args[param_index] = (Type_*)arg_ty;
    ++param_index;
  }

  scope_destroy(&temp_scope);
  hashmap_free(temp_env);
}

static const Type_* find_specialization(const Token_* sym_name, Type_** resolved_args, Scope_* scope) {
  Symbol_* sym = scope_find(scope, sym_name);
  if (!sym) {
    return NULL;
  }

  return type_findspecialization(sym->ty, resolved_args);
}

static void add_specialization(Type_* t, Type_** args, int args_count, MemoryAllocator_* allocator) {
  t->args_count = args_count;
  
  int size = sizeof(Type_*) * t->args_count;
  t->args = alloc(allocator, size);
  memcpy(t->args, args, size);
}

static const Type_* resolve_typeexpr_recur(const TypeExpr_* type, Scope_* scope, Hashmap* type_env, ErrorsContainer_* errors, MemoryAllocator_* allocator) {
  if (errors->panic_mode) {
    return NULL;
  }

  const Type_* ret = NULL;

  switch (type->cls) {
    case TYPE_EXPR_CLS(TypeExprError_):
      assert(false && "unimplemented");

    case TYPE_EXPR_CLS(TypeExprPrimitive_):
    {
      const TypeExprPrimitive_* ret = (const TypeExprPrimitive_*)type;
      return ret->primitive_ty;
    }

    case TYPE_EXPR_CLS(TypeExprSymbol_):
    {
      const TypeExprSymbol_* type_expr_symbol = (const TypeExprSymbol_*)type;

      Symbol_* sym = scope_find(scope, &type_expr_symbol->symbol);
      if (sym) {
        return sym->ty;
      }

      const TypeExpr_* symbol_type = NULL;
      hashmap_get(type_env, type_expr_symbol->symbol.start, type_expr_symbol->symbol.length, (uintptr_t*)&symbol_type);
      if (symbol_type) {
        return resolve_typeexpr_recur(symbol_type, scope, type_env, errors, allocator);
      } else {
          error_panic(errors, -1, "Could not find type %.*s while generating type %.*s",
            type_expr_symbol->symbol.length, type_expr_symbol->symbol.start,
            type->name.length, type->name.start);
          return NULL;
      }
    }

    case TYPE_EXPR_CLS(TypeExprUnion_):
    {
      const TypeExprUnion_* type_expr_union = (const TypeExprUnion_*)type;
      UnionType_* ret = (UnionType_*)make_union_ty(type, scope, allocator, 0);
      for (ListNode_* n = type_expr_union->types.head; n != NULL; n = n->next) {
        uniontype_add((Type_*)ret, (Type_*)resolve_typeexpr_recur(list_val(n, const TypeExpr_*), scope, type_env, errors, allocator));
      }
      return (Type_*)ret;
    }

    case TYPE_EXPR_CLS(TypeExprTuple_):
      assert(false && "unimplemented");

    case TYPE_EXPR_CLS(TypeExprIn_):
    {
      const TypeExprIn_* ty = (const TypeExprIn_*)type;
      return make_in_ty((Type_*)resolve_typeexpr_recur(ty->type, scope, type_env, errors, allocator), type, scope, allocator);
    }

    case TYPE_EXPR_CLS(TypeExprOut_):
    {
      const TypeExprOut_* ty = (const TypeExprOut_*)type;
      return make_out_ty((Type_*)resolve_typeexpr_recur(ty->type, scope, type_env, errors, allocator), type, scope, allocator);
    }

    case TYPE_EXPR_CLS(TypeExprConstraint_):
      return NULL;

    case TYPE_EXPR_CLS(TypeExprGenericParam_):
    {
      const TypeExprGenericParam_* param = (const TypeExprGenericParam_*)type;
      const Type_* constraint = resolve_typeexpr_recur(param->type, scope, type_env, errors, allocator);
      return make_genericparam_ty(param->name, (Type_*)constraint, type, scope, allocator);
    }

    case TYPE_EXPR_CLS(TypeExprFunction_):
    {
      const TypeExprFunction_* ty = (const TypeExprFunction_*)type;
      FunctionType_* fn_type = NULL;
      Scope_* fn_scope = NULL;

      {
        Type_** type_args = alloc_type_array(type->params.count);
        resolve_type_args(type, type_args, scope, type_env, errors, allocator);

        Symbol_* base_cls_sym = scope_find(scope, &ty->name);
        const Type_* found_ty = find_specialization(&ty->name, type_args, scope);
        if (found_ty) {
          return_type_array();
          return found_ty;
        }


        fn_type = (FunctionType_*)make_function_ty(ty->name, NULL, NULL, (TypeExpr_*)ty, scope, allocator);
        fn_scope = fn_type->self.scope;
        if (base_cls_sym) {
          list_push(&base_cls_sym->ty->specializations, &fn_type);
          add_specialization((Type_*)fn_type, type_args, type->params.count, allocator);
        }

        ListNode_* param_n = type->params.head;
        for (int i = 0; i < type->params.count; ++i) {
          const TypeExprGenericParam_* param = list_val(param_n, const TypeExprGenericParam_*);
          frame_addtype(fn_scope->frame, (Token_*)&param->name, type_args[i], fn_scope);
          param_n = param_n->next;
        }

        return_type_array();
      }


      //const TypeExprFunction_* ty = (const TypeExprFunction_*)type;
      Hashmap* new_env = hashmap_copy(type_env);
      bind_type_params(type, NULL, new_env);
      fn_type->ret_ty = (Type_*)resolve_typeexpr_recur(ty->ret_type, scope, new_env, errors, allocator);
      
      for (ListNode_* n = ty->params.head; n != NULL; n = n->next) {
        const Type_* param_type = resolve_typeexpr_recur(list_val(n, const TypeExpr_*), scope, new_env, errors, allocator);
        list_push(&fn_type->params, &param_type);
      }
      
      hashmap_free(new_env);
      return (Type_*)fn_type;
    }

    case TYPE_EXPR_CLS(TypeExprClass_):
    {
      const TypeExprClass_* ty = (const TypeExprClass_*)type;
      ClassType_* cls_type = NULL;
      Scope_* cls_scope = NULL;

      {
        Type_** type_args = alloc_type_array(type->params.count);
        resolve_type_args(type, type_args, scope, type_env, errors, allocator);

        Symbol_* base_cls_sym = scope_find(scope, &ty->name);
        const Type_* cls_ty = find_specialization(&ty->name, type_args, scope);
        if (cls_ty) {
          return_type_array();
          return cls_ty;
        }

        cls_type = (ClassType_*)make_class_ty(ty->name, type, scope, allocator);
        cls_scope = cls_type->scope;
        if (base_cls_sym) {
          list_push(&base_cls_sym->ty->specializations, &cls_type);
          add_specialization((Type_*)cls_type, type_args, type->params.count, allocator);
        }

        ListNode_* param_n = type->params.head;
        for (int i = 0; i < type->params.count; ++i) {
          const TypeExprGenericParam_* param = list_val(param_n, const TypeExprGenericParam_*);
          frame_addtype(cls_scope->frame, (Token_*)&param->name, type_args[i], cls_scope);
          param_n = param_n->next;
        }

        return_type_array();
      }

      // Bind the type parameters to the class scope. This is so that class
      // members can access the generic parameters.
      //for (ListNode_* n = ty->base.params.head; n != NULL; n = n->next) {
      //  const TypeExprGenericParam_* param = list_val(n, const TypeExprGenericParam_*);
      //
      //  // Do not overwrite already bound type arguments. The previous step
      //  // should have bound the resolved arguments into the type environment
      //  // already.
      //  const TypeExpr_* existing_arg = NULL;
      //  hashmap_get(type_env, param->name.start, param->name.length, (uintptr_t*)&existing_arg);
      //  if (existing_arg) {
      //    continue;
      //  }
      //
      //  // If here, the type parameter has not been resolved so class members
      //  // need access to the param at the scope level.
      //  const Type_* arg_ty = resolve_typeexpr_recur((const TypeExpr_*)param, scope, type_env, errors, allocator);
      //  frame_addtype(scope->frame, (Token_*)&param->name, (Type_*)arg_ty, scope);
      //}


      // In case this is an inner class, rebind the type parameters to the new environment.
      Hashmap* new_env = hashmap_copy(type_env);
      bind_type_params(type, NULL, new_env);

      for (ListNode_* n = ty->members.head; n != NULL; n = n->next) {
        const TypeExprClassMember_* member_type_expr = list_val(n, const TypeExprClassMember_*);
        const Type_* member_type = resolve_typeexpr_recur(member_type_expr->type, cls_scope, new_env, errors, allocator);
        type_class_addmember((Type_*)cls_type, member_type_expr->name, (Type_*)member_type, NULL);
        frame_addtype(cls_type->scope->frame, (Token_*)&member_type_expr->name, (Type_*)member_type, cls_scope);
      }

      //maybe_add_specialization();

      hashmap_free(new_env);
      return (Type_*)cls_type;
    }

    case TYPE_EXPR_CLS(TypeExprGenericOrArrayType_):
    {
      const TypeExprGenericOrArrayType_* ty = (const TypeExprGenericOrArrayType_*)type;
      const Type_* prefix = resolve_typeexpr_recur(ty->prefix, scope, type_env, errors, allocator);
      const TypeExpr_* tmpl = prefix->tmpl;

      // If there are any type parameters, then assume that the prefix is a
      // generic, and consume the arguments as type arguments.
      if (tmpl->params.count) {
        return apply_template(tmpl, &ty->args, scope, type_env, errors, allocator);
      } else {
        assertf(false, "unimplemented");
      }

      break;
    }

    case TYPE_EXPR_CLS(TypeExprFunctionCall_):
    {
      const TypeExprFunctionCall_* ty = (const TypeExprFunctionCall_*)type;
      const Type_* prefix = resolve_typeexpr_recur(ty->prefix, scope, type_env, errors, allocator);
      assertf(type_is(prefix, FunctionType_), "Type of function call is not function.");
      return ((FunctionType_*)prefix)->ret_ty;
    }

    case TYPE_EXPR_CLS(TypeExprClassMember_):
      assert(false && "This should not be hit");

    default:
      assert(false && "Unimplemented type.");
  }

  return NULL;
}

const Type_* resolve_typeexpr(const TypeExpr_* type, ListOf_(const TypeExpr_*)* opt_args, Scope_* scope, ErrorsContainer_* error, MemoryAllocator_* allocator) {
  if (opt_args) {
    assertf(
      opt_args->count > type->params.count,
      "Type received more than expected number of type arguments. Wanted %d, got %d",
      type->params.count, opt_args->count);
  }

  Hashmap* type_env = hashmap_create();

  bind_type_params(type, opt_args, type_env);

  const Type_* ret = resolve_typeexpr_recur(type, scope, type_env, error, allocator);
  hashmap_free(type_env);

  return ret;
}


TypeExpr_* assert_typeexpr_is_(TypeExpr_* ty, int cls) {
  assertf(ty->cls == cls, "TypeExpr does not match %d vs. %d", ty->cls, cls);
  return ty;
}