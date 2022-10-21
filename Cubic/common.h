#ifndef COMMON__H
#define COMMON__H

#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

// #define DEBUG_PRINT_CODE
// #define DEBUG_TRACE_EXECUTION

#define expectf(cond, ...) \
do { if (!(cond)) fprintf(stderr, __VA_ARGS__); }  while(0)

#define assertf(cond, ...) \
do { if (!(cond)) { fprintf(stderr, __VA_ARGS__); exit(1); } }  while(0)

#endif  // COMMON__H