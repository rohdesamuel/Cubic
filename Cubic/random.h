#ifndef RANDOM__H
#define RANDOM__H

#include "common.h"

typedef struct cbSplitmix_ {
  uint64_t s;
} cbSplitmix_, * cbSplitmix;

typedef struct cbXorshift_ {
  uint64_t s;
} cbXorshift_, * cbXorshift;

typedef struct cbXorshift128p_ {
  uint64_t s[2];
} cbXorshift128p_, * cbXorshift128p;

typedef struct cbXoshiro256ss_ {
  uint64_t s[4];
} cbXoshiro256ss_, * cbXoshiro256ss;

void cb_seed(uint64_t s);
uint64_t cb_rand();

uint64_t cb_splitmix(cbSplitmix state);
uint64_t cb_xorshift(cbXorshift state);
uint64_t cb_xorshift128p(cbXorshift128p state);
uint64_t cb_xoshiro256ss(cbXoshiro256ss state);

#endif  // RANDOM__H