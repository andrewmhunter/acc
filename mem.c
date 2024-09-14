#include <stdlib.h>
#include <stdio.h>
#include "mem.h"

size_t growSize(size_t minSize) {
    if (minSize == 0) {
        return 0;
    }

    int leadingZeroes = __builtin_clzll(minSize);
    size_t bitSize = sizeof(minSize) * 8;
    size_t floorInvSquareMinSize = bitSize - leadingZeroes - 1;
    size_t adjustedSize = 1 << floorInvSquareMinSize;
    if (minSize > adjustedSize) {
        adjustedSize <<= 1;
    }
    return adjustedSize;
}

void* resizeArray(void* array, size_t elementSize, size_t* capacity, size_t newCapacity) {
    newCapacity = growSize(newCapacity);
    if (*capacity >= newCapacity) {
        return array;
    }
    *capacity = newCapacity;
    return realloc(array, newCapacity * elementSize);
}

