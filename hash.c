#include <string.h>
#include "hash.h"
#include "mem.h"
#include "util.h"

#define MAX_LOAD_FACTOR 0.5f
#define HASH_PRIME 0x00000100000001b3

void hashBytes(Hash* hash, const void* bytes, size_t count) {
    const char* chars = bytes;
    for (size_t i = 0; i < count; ++i) {
        *hash ^= chars[i];
        *hash *= HASH_PRIME;
    }
}

void hashBytesSized(Hash* hash, const void* bytes, size_t count) {
    hashBytes(hash, &count, sizeof(count));
    hashBytes(hash, bytes, count);
}

void hashString(Hash* hash, const char* string) {
    hashBytesSized(hash, string, strlen(string));
}


Set setNew(HashFn hashFn, EqualsFn equalsFn) {
    Set set = (Set) {
        .entries = NULL,
        .capacity = 0,
        .count = 0,
        .hashFn = hashFn,
        .equalsFn = equalsFn,
    };
    setExtend(&set);
    return set;
}

static Entry* setGetEntryHashed(Set* set, const void* key, Hash hash) {
    size_t index = hash % set->capacity;
    while (set->entries[index].data != NULL) {
        if (
            set->entries[index].hash == hash
            && set->equalsFn(key, set->entries[index].data)
        ) {
            break;
        }
        index = (index + 1) % set->capacity;
    }

    return &set->entries[index];
}

Entry* setGetEntry(Set* set, const void* key) {
    Hash hash = HASH_OFFSET;
    set->hashFn(&hash, key);
    return setGetEntryHashed(set, key, hash);
}

bool setHas(Set* set, const void* key) {
    return setGetEntry(set, key)->data != NULL;
}

void* setGet(Set* set, const void* key) {
    return setGetEntry(set, key)->data;
}

void setExtend(Set* set) {
    Entry* oldEntries = set->entries;
    size_t oldCapacity = set->capacity;
    set->capacity = MAX(growSize(set->capacity + 1), 8);

    set->entries = malloc(set->capacity * sizeof(Entry));
    memset(set->entries, 0, set->capacity * sizeof(Entry));

    set->count = 0;
    for (size_t i = 0; i < oldCapacity; ++i) {
        Entry* oldEntry = &oldEntries[i];
        if (oldEntries[i].data == NULL) {
            continue;
        }
        Entry* entry = setGetEntryHashed(set, oldEntry->data, oldEntry->hash);
        *entry = *oldEntry;
        set->count++;
    }

    free(oldEntries);
}

void* setInsert(Set* set, void* key) {
    if (set->capacity == 0
        || ((float)(set->count + 1) / (float)set->capacity) >= MAX_LOAD_FACTOR
    ) {
        setExtend(set);
    }

    Hash hash = HASH_OFFSET;
    set->hashFn(&hash, key);
    Entry* entry = setGetEntryHashed(set, key, hash);

    Entry oldEntry = *entry;
    entry->hash = hash;
    entry->data = key;

    if (oldEntry.hash == 0 && oldEntry.data == NULL) {
        set->count++;
    }
    return oldEntry.data;
}

Entry* setIterate(Set* set, Entry* current) {
    if (current == NULL) {
        current = set->entries - 1;
    }

    do {
        current++;
        if (current >= set->entries + set->capacity) {
            return NULL;
        }
    } while (current->data == NULL);
    return current;
}

void* setDelete(Set* set, void* key) {
    Entry* entry = setGetEntry(set, key);
    void* oldData = NULL;
    if (entry->data != NULL) {
        oldData = entry->data;
        entry->data = NULL;
        entry->hash = 1;
    }
    return oldData;
}

void setFree(Set* set) {
    free(set->entries);
}

void setFreeAll(Set* set) {
    Entry* current = NULL;
    while ((current = setIterate(set, current))) {
        free(current->data);
    }

    setFree(set);
}

