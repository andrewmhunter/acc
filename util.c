#include "util.h"
#include <stdlib.h>

bool intIsPowerOf2(unsigned int n) {
    int leadingZeroes = __builtin_clz(n);
    int traillingZeroes = __builtin_ctz(n);
    int intBitSize = sizeof(unsigned int) * 8;
    return leadingZeroes + traillingZeroes + 1 == intBitSize;
}

// Calculates the log base 2 of a integer, rounded up to next integer.
size_t ceilLog2(size_t number) {
    if (number <= 1) {
        return 0;
    }
    int leadingZeroes = __builtin_clzll(number - 1);
    size_t bitSize = sizeof(number) * 8;
    return bitSize - leadingZeroes;
}


