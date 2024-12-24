#ifndef IDENTIFIER_H
#define IDENTIFIER_H

#include <stdbool.h>
#include <stdio.h>
#include "diag.h"

typedef struct {
    const char* start;
    int length;
} Identifier;

#define BLANK_IDENTIFIER ((Identifier) {.start = NULL, .length = 0})

bool identEquals(const Identifier* id0, const Identifier* id1);
void identPrint(FILE* file, const Identifier* id);
Location identLoc(const Identifier* id);

#endif

