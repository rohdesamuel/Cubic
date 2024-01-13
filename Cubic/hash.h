#ifndef HASH__H
#define HASH__H

#include "common.h"

uint64_t hash_bytes(const uint8_t* start, size_t length);
uint64_t hash_incr(uint64_t h, uint64_t val);
uint64_t hash_finish(uint64_t h);
uint64_t hash_combine(uint64_t a, uint64_t b);

#endif  // HASH__H