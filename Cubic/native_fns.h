#ifndef NATIVE_FNS__H
#define NATIVE_FNS__H

#include "value.h"

typedef Value_(*NativeFunction_)(struct VM_* vm, int param_count, Value_ params[]);
typedef struct NativeFunctionName_ {
  size_t length;
  const char* name;
} NativeFunctionName_;

extern const char kNativeStringEq[];

void init_native_fns(struct VM_* vm);
int nativefn_find(const char* name);
Value_ nativefn_call(struct VM_* vm, int id, int param_count, Value_ params[]);

#endif  // NATIVE_FNS__H