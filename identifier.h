#ifndef IDENTIFIER_H
#define IDENTIFIER_H

#include <stdbool.h>
#include <stdio.h>

typedef struct {
    const char* start;
    int length;
} Identifier;

bool identEquals(Identifier id0, Identifier id1);
void identPrint(FILE* file, Identifier id);

#endif

