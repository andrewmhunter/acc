#include <stdio.h>
#include <assert.h>
#include "type.h"
#include "mem.h"
#include "diag.h"

Type* typeNew(Arena* arena, TypeTag tag) {
    Type* type = ARENA_ALLOC(arena, Type);
    type->tag = tag;
    return type;
}

const Type* typeInteger(Arena* arena, Signedness sign, IntegerSize size) {
    Type* type = typeNew(arena, TYPE_INTEGER);
    type->integer.sign = sign;
    type->integer.size = size;
    return type;
}

const Type* typePointer(Arena* arena, const Type* inner) {
    Type* type = typeNew(arena, TYPE_POINTER);
    type->pointer = inner;
    return type;
}

const Type* typeVoid() {
    static Type inner = (Type) {.tag = TYPE_VOID};
    return &inner;
}

const Type* typeUChar() {
    static Type inner = (Type) {.tag = TYPE_INTEGER, .integer = {.size = SIZE_BYTE, .sign = SIGN_UNSIGNED}};
    return &inner;
}

const Type* typeAnyInteger() {
    static Type inner = (Type) {.tag = TYPE_INTEGER, .integer = {.size = SIZE_ANY, .sign = SIGN_EITHER}};
    return &inner;
}

int typeSize(const Type* type) {
    switch (type->tag) {
        case TYPE_INTEGER:
            switch (type->integer.size) {
                case SIZE_BYTE:
                    return 1;
                case SIZE_INT:
                    return WORD_SIZE;
                case SIZE_ANY:
                    return WORD_SIZE;
                    assert(false && "any sized interger must be converted to"
                            "a sized integer before finding the size");
            }
            break;
        case TYPE_POINTER:
            return WORD_SIZE;
        case TYPE_VOID:
            return 1;
    }
}

void typePrint(FILE* file, const Type* type) {
    switch (type->tag) {
        case TYPE_INTEGER:
            if (type->integer.sign == SIGN_UNSIGNED) {
                fprintf(file, "unsigned ");
            }

            if (type->integer.size == SIZE_INT) {
                fprintf(file, "int");
            } else {
                fprintf(file, "char");
            }
            break;
        case TYPE_POINTER:
            typePrint(file, type->pointer);
            fprintf(file, "*");
            break;
        case TYPE_VOID:
            fprintf(file, "void");
            break;
    }
}

bool typeEquals(const Type* t0, const Type* t1) {
    if (t0->tag != t1->tag) {
        return false;
    }

    switch (t0->tag) {
        case TYPE_INTEGER:
            return t0->integer.size == t1->integer.size
                && t0->integer.sign == t1->integer.sign;
        case TYPE_POINTER:
            return typeEquals(t0->pointer, t1->pointer);
        case TYPE_VOID:
            return true;
    }
}

bool typeCompatible(const Type* t0, const Type* t1) {
    if (t0->tag != t1->tag) {
        return false;
    }

    switch (t0->tag) {
        case TYPE_INTEGER:
            return (
                    t0->integer.size == t1->integer.size
                    || t0->integer.size == SIZE_ANY
                    || t1->integer.size == SIZE_ANY
                ) && (
                    t0->integer.sign == t1->integer.sign
                    || t0->integer.sign == SIGN_EITHER
                    || t1->integer.sign == SIGN_EITHER
                );
        case TYPE_POINTER:
            return typeEquals(t0->pointer, t1->pointer)
                || t0->pointer->tag == TYPE_VOID
                || t1->pointer->tag == TYPE_VOID;
        case TYPE_VOID:
            return true;
    }
}

bool isInteger(const Type* type) {
    return type->tag == TYPE_INTEGER;
}

bool isPointer(const Type* type) {
    return type->tag == TYPE_POINTER;
}

const Type* commonType(const Type* t0, const Type* t1) {
    ASSERT(isInteger(t0) && isInteger(t1), "can only find the common type of integers");

    // If the integers are the same size, sign precedence goes: either < signed < unsigned
    if (t0->integer.size == t1->integer.size) {
        if (t0->integer.sign >= t1->integer.sign) {
            return t0;
        }
        return t1;
    }

    // If sizes are unequal, return longest integer
    if (t0->integer.size > t1->integer.size) {
        return t0;
    }
    return t1;
}

