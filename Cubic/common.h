#ifndef COMMON__H
#define COMMON__H

#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

#include "memory.h"

// #define DEBUG_PRINT_CODE
// #define DEBUG_TRACE_EXECUTION

#define expectf(cond, ...) \
do { if (!(cond)) fprintf(stderr, __VA_ARGS__); __debugbreak(); }  while(0)

#define assertf(cond, ...) \
do { if (!(cond)) { fprintf(stderr, __VA_ARGS__); __debugbreak(); exit(1); } }  while(0)

#define CTASTR2(pre,post) pre ## post
#define CTASTR(pre,post) CTASTR2(pre,post)
#define STATIC_ASSERT(cond,msg) \
    typedef struct { int CTASTR(static_assertion_failed_,msg) : !!(cond); } \
        CTASTR(static_assertion_failed_,__COUNTER__)

#endif  // COMMON__H