#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include "mem.h"
#include "util.h"

size_t growSize(size_t minSize) {
    if (minSize == 0) {
        return 0;
    }

    if (minSize <= 8) {
        return 8;
    }

    size_t adjustedSize = 1 << ceilLog2(minSize);
    if (minSize > adjustedSize) {
        adjustedSize <<= 1;
    }
    return adjustedSize;
}

void* resizeArray(
    Arena* arena,
    void* array,
    size_t elementSize,
    size_t* capacity,
    size_t newCapacity,
    size_t alignment
) {
    size_t oldCapacity = *capacity;
    newCapacity = growSize(newCapacity);
    if (*capacity >= newCapacity) {
        return array;
    }
    *capacity = newCapacity;
    return arenaReallocAligned(
        arena,
        array,
        oldCapacity * elementSize,
        newCapacity * elementSize,
        alignment
    );
}

#define REGION_BASE_SIZE 0x1000
//#define REGION_BASE_SIZE 0x1

static Region* regionNew(size_t minSize) {
    size_t blockSize = MAX(minSize, REGION_BASE_SIZE);
    Region* region = malloc(sizeof(Region) + blockSize);
    if (region == NULL) {
        fprintf(stderr, "Failed to allocate region of size %lu\n", blockSize);
        exit(3);
    }
    region->size = blockSize;
    region->next = NULL;
    region->offset = 0;
    //printf("Allocating a region of size: %lu\n", blockSize);
    return region;
}

Arena arenaNew() {
    return (Arena) {.current = NULL};
}

#define ALIGN_TO(NUMBER, ALIGNMENT) \
    ((((NUMBER) - 1) / (ALIGNMENT) + 1 ) * (ALIGNMENT))

static void* regionAlloc(Region* region, size_t count, size_t alignment) {
    if (region == NULL) {
        return NULL;
    }

    size_t alignedOffset = ALIGN_TO(region->offset, alignment);
    if (alignedOffset + count > region->size) {
        return NULL;
    }

    region->offset = alignedOffset + count;
    return region->data + alignedOffset;
}

void* arenaAllocAligned(Arena* arena, size_t count, size_t alignment) {
    if (arena == NULL) {
        return malloc(count);
    }

    void* allocation = regionAlloc(arena->current, count, alignment);
    if (allocation == NULL) {
        Region* newRegion = regionNew(count);
        allocation = regionAlloc(newRegion, count, alignment);

        if (arena->current != NULL 
            && arena->current->size - arena->current->offset
            > newRegion->size - newRegion->offset
        ) {
            newRegion->next = arena->current->next;
            arena->current->next = newRegion;
        } else {
            newRegion->next = arena->current;
            arena->current = newRegion;
        }
    }

    //printf("Allocated %lu: %p offalign %lu\n", count, allocation, (uintptr_t)allocation % alignment);
    return allocation;
}

void* arenaAlloc(Arena* arena, size_t count) {
    return arenaAllocAligned(arena, count, alignof(max_align_t));
}

void arenaFree(Arena* arena) {
    Region* current = arena->current;
    while (current != NULL) {
        Region* next = current->next;
        free(current);
        current = next;
    }
}

void* arenaReallocAligned(
    Arena* arena,
    void* data,
    size_t oldSize,
    size_t newSize,
    size_t alignment
) {
    if (newSize <= oldSize) {
        return data;
    }

    if (arena == NULL) {
        return realloc(data, newSize);
    }

    void* newData = arenaAllocAligned(arena, newSize, alignment);
    memcpy(newData, data, oldSize);
    return newData;
}

void* arenaRealloc(Arena* arena, void* data, size_t oldSize, size_t newSize) {
    return arenaReallocAligned(arena, data, oldSize, newSize, alignof(max_align_t));
}

void* arenaMemcpyAligned(Arena* arena, const void* data, size_t size, size_t alignment) {
    void* newData = arenaAllocAligned(arena, size, alignment);
    memcpy(newData, data, size);
    return newData;
}

void* arenaMemcpy(Arena* arena, const void* data, size_t size) {
    return arenaMemcpyAligned(arena, data, size, alignof(max_align_t));
}

char* arenaStrdup(Arena* arena, const char* string) {
    return arenaMemcpyAligned(arena, string, strlen(string) + 1, alignof(char));
}

char* arenaVsprintf(Arena* arena, const char* format, va_list args) {
    va_list lengthCheckArgs;
    va_copy(lengthCheckArgs, args);
    int length = vsnprintf(NULL, 0, format, lengthCheckArgs);
    va_end(lengthCheckArgs);

    char* outputString = arenaAllocAligned(arena, length + 1, alignof(char));
    vsprintf(outputString, format, args);
    return outputString;
}

char* arenaSprintf(Arena* arena, const char* format, ...) {
    va_list args;
    va_start(args, format);
    char* outputString = arenaVsprintf(arena, format, args);
    va_end(args);
    return outputString;
}

