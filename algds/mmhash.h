#ifndef ALGDS_HASH_H_
#define ALGDS_HASH_H_

#include <stdint.h>

uint64_t mmhash(const void *key, int len, uint64_t seed);

#endif
