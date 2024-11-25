#include <string.h>
#include <stdio.h>
#include "identifier.h"

bool identEquals(Identifier id0, Identifier id1) {
    return id0.length == id1.length && memcmp(id0.start, id1.start, id0.length) == 0;
}

void identPrint(FILE* file, Identifier id) {
    fprintf(file, "#%.*s", id.length, id.start);
}

