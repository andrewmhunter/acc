#ifndef UTIL_H
#define UTIL_H

#include <stdbool.h>
#include <stdlib.h>

#define MAX(A, B) ((A) >= (B) ? (A) : (B))
#define MIN(A, B) ((A) <= (B) ? (A) : (B))
#define LENGTH_OF(ARR) (sizeof(ARR) / sizeof((ARR)[0]))

bool intIsPowerOf2(unsigned int n);
size_t ceilLog2(size_t number);

#endif

