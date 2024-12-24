#include <string.h>
#include <stdio.h>
#include "identifier.h"

bool identEquals(const Identifier* id0, const Identifier* id1) {
    return id0->length == id1->length && memcmp(id0->start, id1->start, id0->length) == 0;
}

void identPrint(FILE* file, const Identifier* id) {
    fprintf(file, "#%.*s", id->length, id->start);
}

Location identLoc(const Identifier* id) {
    return locNew(id->start, id->start + id->length - 1);
}

