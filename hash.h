#ifndef HASH_H
#define HASH_H

#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#define HASH_OFFSET 0xcbf29ce484222325

typedef uint64_t Hash;

void hashBytes(Hash* hash, void* bytes, size_t count);
void hashBytesSized(Hash* hash, void* bytes, size_t count);
void hashString(Hash* hash, char* string);

typedef void (*HashFn)(Hash* hash, void* value);
typedef bool (*EqualsFn)(void* left, void* right);

typedef struct {
    Hash hash;
    void* data;
} Entry;

typedef struct {
    Entry* entries;
    size_t capacity;
    size_t count;
    HashFn hashFn;
    EqualsFn equalsFn;
} Set;

Set setNew(HashFn hashFn, EqualsFn equalsFn);
void setExtend(Set* set);
Entry* setGetEntry(Set* set, void* key);
bool setHas(Set* set, void* key);
void* setGet(Set* set, void* key);
void* setInsert(Set* set, void* key);
Entry* setIterate(Set* set, Entry* current);
void* setDelete(Set* set, void* key);

#endif

