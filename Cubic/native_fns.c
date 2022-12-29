#include "native_fns.h"
#include "value.h"
#include "object.h"
#include "vm.h"
#include "symbol.h"

static const char kNativeStringEq[] = "__string_eq__";

int nativefn_find(const char* name);
Value_ nativefn_call(VM_* vm, int id, int param_count, Value_ params[]);

static int register_nativefn(const char* name, NativeFunction_ fn);

NativeFunction_* native_fns;
NativeFunctionName_* native_fns_names;
Symbol_* native_fn_syms;
int native_fns_count;

static Value_ __string_eq__(struct VM_* vm, int param_count, Value_ params[]) {
  if (param_count != 2) {
    vm_runtime_error(vm,
      "Incorrect amount of parameters for `string_eq` expecting 2, got %d",
      param_count);
    return NIL_VAL;
  }

  ObjString_* l_string = AS_STRING(params[0]);
  ObjString_* r_string = AS_STRING(params[1]);

  return BOOL_VAL(
    l_string == r_string ||
    (l_string->length == r_string->length && memcmp(l_string->chars, r_string->chars, l_string->length) == 0)
  );
}

static Value_ __noop__(struct VM_* vm, int param_count, Value_ params[]) {
  return NIL_VAL;
}

void init_native_fns(struct VM_* vm) {
  native_fns = calloc(NATIVE_FNS_MAX, sizeof(NativeFunction_));
  native_fns_names = calloc(NATIVE_FNS_MAX, sizeof(char*));
  native_fn_syms = calloc(NATIVE_FNS_MAX, sizeof(Symbol_));

  register_nativefn("__noop__", __noop__);
  register_nativefn("__string_eq__", __string_eq__);
}

int register_nativefn(const char* name, NativeFunction_ fn) {
  if (native_fns_count > NATIVE_FNS_MAX) {
    return -1;
  }

  int id = native_fns_count++;

  native_fns[id] = fn;
  native_fns_names[id] = (NativeFunctionName_){
    .length = strlen(name),
    .name = name
  };

  return id;
}

int nativefn_find(const char* name) {
  for (int i = 0; i < native_fns_count; ++i) {
    NativeFunctionName_ native = native_fns_names[i];
    if (memcmp(native.name, name, native.length) == 0) {
      return i;
    }
  }

  return -1;
}

Value_ nativefn_call(struct VM_* vm, int id, int param_count, Value_ params[]) {
  if (id < 0 || id >= native_fns_count) {
    vm_runtime_error(vm, "Trying to call unknown native function.");
    return NIL_VAL;
  }

  return native_fns[id](vm, param_count, params);
}