#ifndef IDENTIFIER_H
#define IDENTIFIER_H

#include <stdbool.h>

typedef struct {
    const char* start;
    int length;
} Identifier;

bool identEquals(Identifier* i0, Identifier* i1);

#endif

