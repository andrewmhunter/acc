#ifndef MEM_H
#define MEM_H

#include <stdlib.h>

#define RESIZE_ARRAY(ARRAY, TYPE, CAPACITY, NEW_SIZE) \
    resizeArray(ARRAY, sizeof(TYPE), CAPACITY, NEW_SIZE)
    

#define EXTEND_ARRAY(ARRAY, TYPE, CAPACITY, OLD_SIZE, EXTEND_BY) \
    RESIZE_ARRAY(ARRAY, TYPE, CAPACITY, OLD_SIZE + EXTEND_BY)

void* resizeArray(void* array, size_t elementSize, size_t* capacity, size_t newCapacity);

#endif

