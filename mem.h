#ifndef MEM_H
#define MEM_H

#include <stdlib.h>
#include <stddef.h>
#include <stdalign.h>
#include <stdarg.h>

#define RESIZE_ARRAY(ARENA, ARRAY, TYPE, CAPACITY, NEW_SIZE) \
    ((TYPE*)resizeArray(ARENA, ARRAY, sizeof(TYPE), CAPACITY, NEW_SIZE, alignof(TYPE)))
    

#define EXTEND_ARRAY(ARENA, ARRAY, TYPE, CAPACITY, OLD_SIZE, EXTEND_BY) \
    ((TYPE*)RESIZE_ARRAY(ARENA, ARRAY, TYPE, CAPACITY, OLD_SIZE + EXTEND_BY))

#define APPEND_ARRAY(ARENA, ARRAY, TYPE, CAPACITY, SIZE, VALUE) \
    do { \
        ARRAY = EXTEND_ARRAY(ARENA, ARRAY, TYPE, &CAPACITY, SIZE, 1); \
        ARRAY[SIZE++] = VALUE; \
    } while (0)


typedef struct Region {
    size_t size;
    struct Region* next;
    size_t offset;
    alignas(alignof(max_align_t)) char data[];
} Region;

typedef struct {
    Region* current;
} Arena;

Arena arenaNew();
void arenaFree(Arena* arena);

void* arenaAllocAligned(Arena* arena, size_t count, size_t alignment);
void* arenaAlloc(Arena* arena, size_t count);

void* arenaReallocAligned(Arena* arena, void* data, size_t oldSize, size_t newSize, size_t alignment);
void* arenaRealloc(Arena* arena, void* data, size_t oldSize, size_t newSize);

void* arenaMemcpyAligned(Arena* arena, const void* data, size_t size, size_t alignment);
void* arenaMemcpy(Arena* arena, const void* data, size_t size);

char* arenaStrdup(Arena* arena, const char* string);
char* arenaVsprintf(Arena* arena, const char* format, va_list args);
char* arenaSprintf(Arena* arena, const char* format, ...);

#define ARENA_ALLOC_ARRAY(ARENA, TYPE, LENGTH) \
    ((TYPE*)arenaAllocAligned((ARENA), sizeof(TYPE) * (LENGTH), alignof(TYPE)))

#define ARENA_ALLOC(ARENA, TYPE) \
    ARENA_ALLOC_ARRAY(ARENA, TYPE, 1)

#define ARENA_REALLOC(ARENA, TYPE, DATA, OLD_LENGTH, NEW_LENGTH) \
    ((TYPE*)arenaReallocAligned((ARENA), (DATA), (OLD_LENGTH) * sizeof(TYPE), (NEW_LENGTH) * sizeof(TYPE), alignof(TYPE)))


void* resizeArray(Arena* arena, void* array, size_t elementSize, size_t* capacity, size_t newCapacity, size_t alignment);
size_t ceilLog2(size_t number);
size_t growSize(size_t minSize);

#endif

