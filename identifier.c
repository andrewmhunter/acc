#include <string.h>
#include "identifier.h"

bool identEquals(Identifier* i0, Identifier* i1) {
    return i0->length == i1->length && memcmp(i0->start, i1->start, i0->length) == 0;
}

