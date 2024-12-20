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

#if (defined __WIN32__ || defined __CYGWIN32__ || defined _WIN32 || defined _WIN64 || defined _MSC_VER)
#define __COMPILE_AS_WINDOWS__
#elif (defined __linux__ || defined __GNUC__)
#define __COMPILE_AS_LINUX__
#endif

#define expectf(cond, ...) \
do { if (!(cond)) fprintf(stderr, __VA_ARGS__); __debugbreak(); }  while(0)

#define assertf(cond, ...) \
do { if (!(cond)) { fprintf(stderr, __VA_ARGS__); __debugbreak(); exit(1); } }  while(0)

#define CTASTR2(pre,post) pre ## post
#define CTASTR(pre,post) CTASTR2(pre,post)
#define STATIC_ASSERT(cond,msg) \
    typedef struct { int CTASTR(static_assertion_failed_,msg) : !!(cond); } \
        CTASTR(static_assertion_failed_,__COUNTER__)


#ifndef thread_local
# if __STDC_VERSION__ >= 201112 && !defined __STDC_NO_THREADS__
#  define thread_local _Thread_local
# elif defined _WIN32 && ( \
       defined _MSC_VER || \
       defined __ICL || \
       defined __DMC__ || \
       defined __BORLANDC__ )
#  define thread_local __declspec(thread) 
/* note that ICC (linux) and Clang are covered by __GNUC__ */
# elif defined __GNUC__ || \
       defined __SUNPRO_C || \
       defined __xlC__
#  define thread_local __thread
# else
#  error "Cannot define thread_local"
# endif
#endif

#define Optional(TYPE) \
struct { \
  bool has_val; \
  TYPE val; \
}

#define CB_COMMA ,
#define CB_SEMICOLON ;

#endif  // COMMON__H