#include <stdlib.h>
#include <stdio.h>
#include "type.h"

Type* typeNew(TypeTag tag) {
    Type* type = malloc(sizeof(Type));
    type->tag = tag;
    return type;
}

Type* typeInteger(Signedness sign, IntegerSize size) {
    Type* type = typeNew(TYPE_INTEGER);
    type->integer.sign = sign;
    type->integer.size = size;
    return type;
}

Type* typePointer(Type* inner) {
    Type* type = typeNew(TYPE_POINTER);
    type->pointer = inner;
    return type;
}

void typeFree(Type* type) {
    switch (type->tag) {
        case TYPE_POINTER:
            typeFree(type->pointer);
            break;
        default:
            break;
    }
    free(type);
}

void typePrint(Type* type) {
    switch (type->tag) {
        case TYPE_INTEGER:
            if (type->integer.sign == SIGN_UNSIGNED) {
                printf("unsigned ");
            }

            if (type->integer.size == SIZE_INT) {
                printf("int");
            } else {
                printf("char");
            }
            break;
        case TYPE_POINTER:
            typePrint(type->pointer);
            printf("*");
            break;
        case TYPE_VOID:
            printf("void");
            break;
    }
}

bool typeEquals(Type* t0, Type* t1) {
    if (t0->tag != t1->tag) {
        return false;
    }

    switch (t0->tag) {
        case TYPE_INTEGER:
            return t0->integer.size == t1->integer.size
                && t0->integer.sign == t1->integer.sign;
        case TYPE_POINTER:
            return typeEquals(t0->pointer, t1->pointer);
        default:
            return true;
    }
}

