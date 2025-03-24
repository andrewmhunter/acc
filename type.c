#include <stdio.h>
#include <assert.h>
#include "type.h"
#include "mem.h"
#include "diag.h"

#define INTEGER(SIGN, SIZE) \
    ((Type) {.tag = TYPE_INTEGER, .integer = {.size = (SIZE), .sign = (SIGN)}})

const Type typeInt = INTEGER(SIGN_SIGNED, SIZE_INT);
const Type typeUInt = INTEGER(SIGN_UNSIGNED, SIZE_INT);
const Type typeAnyInt = INTEGER(SIGN_EITHER, SIZE_ANY);
const Type typeChar = INTEGER(SIGN_SIGNED, SIZE_BYTE);
const Type typeUChar = INTEGER(SIGN_UNSIGNED, SIZE_BYTE);
const Type typeVoid = (Type){.tag = TYPE_VOID};
const Type typeBool = typeUInt;

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

const Type* typeArray(Arena* arena, const Type* element, int length) {
    Type* type = typeNew(arena, TYPE_ARRAY);
    type->pointer = element;
    type->length = length;
    return type;
}

const Type* typeAnyInteger() {
    static Type inner = (Type) {.tag = TYPE_INTEGER, .integer = {.size = SIZE_ANY, .sign = SIGN_EITHER}};
    return &inner;
}

int typeSize(const Type* type) {
    if (type == NULL) {
        return 1;
    }

    switch (type->tag) {
        case TYPE_INTEGER:
            switch (type->integer.size) {
                case SIZE_BYTE:
                    return 1;
                case SIZE_INT:
                    return WORD_SIZE;
                case SIZE_24:
                    return 3;
                case SIZE_LONG:
                    return 4;
                case SIZE_ANY:
                    return WORD_SIZE;
                    assert(false && "any sized interger must be converted to"
                            "a sized integer before finding the size");
            }
            break;
        case TYPE_POINTER:
            return WORD_SIZE;
        case TYPE_ARRAY:
            return typeSize(type->pointer) * type->length;
        case TYPE_VOID:
            return 1;
    }
}

void typePrint(FILE* file, const Type* type) {
    if (type == NULL) {
        fprintf(file, "NULL_TYPE");
    }

    switch (type->tag) {
        case TYPE_INTEGER:
            if (type->integer.sign == SIGN_UNSIGNED) {
                fprintf(file, "unsigned ");
            }

            switch (type->integer.size) {
                case SIZE_BYTE:
                    fprintf(file, "char");
                    break;
                case SIZE_INT:
                    fprintf(file, "int");
                    break;
                case SIZE_24:
                    fprintf(file, "int24_t");
                    break;
                case SIZE_LONG:
                    fprintf(file, "long");
                    break;
                case SIZE_ANY:
                    fprintf(file, "int_any");
                    break;
            }
            break;
        case TYPE_POINTER:
            typePrint(file, type->pointer);
            fprintf(file, "*");
            break;
        case TYPE_ARRAY:
            typePrint(file, type->pointer);
            fprintf(file, "[%d]", type->length);
            break;
        case TYPE_VOID:
            fprintf(file, "void");
            break;
    }
}

bool typeEquals(const Type* t0, const Type* t1) {
    if (t0 == NULL || t1 == NULL) {
        return true;
    }

    if (t0->tag != t1->tag) {
        return false;
    }

    switch (t0->tag) {
        case TYPE_INTEGER:
            return t0->integer.size == t1->integer.size
                && t0->integer.sign == t1->integer.sign;
        case TYPE_POINTER:
            return typeEquals(t0->pointer, t1->pointer);
        case TYPE_ARRAY:
            return typeEquals(t0->pointer, t1->pointer)
                && t0->length == t1->length;
        case TYPE_VOID:
            return true;
    }
}

bool typeCompatible(const Type* t0, const Type* t1) {
    if (t0 == NULL || t1 == NULL) {
        return true;
    }

    if (isPointer(t0) && isPointer(t1)) {
        return typeEquals(t0->pointer, t1->pointer)
            || t0->pointer->tag == TYPE_VOID
            || t1->pointer->tag == TYPE_VOID;
    }

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
        default:
            break;
    }

    return false;
}

bool typeConvertable(const Type* from, const Type* into) {
    if (from == NULL || into == NULL) {
        return true;
    }

    if ((typeSize(from) == typeSize(into)
                || (from->tag == TYPE_INTEGER && from->integer.size == SIZE_ANY)
                || (into->tag == TYPE_INTEGER && into->integer.size == SIZE_ANY)
            )
            && (isPointer(from) || isInteger(from))
            && (isPointer(into) || isInteger(into))
    ) {
        return true;
    }

    if (into->tag == TYPE_VOID) {
        return true;
    }

    return false;
}

bool isVoid(const Type* type) {
    return type != NULL && type->tag == TYPE_VOID;
}

bool isInteger(const Type* type) {
    return type != NULL && type->tag == TYPE_INTEGER;
}

bool isPointer(const Type* type) {
    return type != NULL && (type->tag == TYPE_POINTER || type->tag == TYPE_ARRAY);
}

bool isArray(const Type* type) {
    return type != NULL && type->tag == TYPE_ARRAY;
}

const Type* integerPromotion(Diagnostics* diag, const Type* t0, const Type* t1, Location location) {
    if (t0 == NULL || t1 == NULL) {
        return NULL;
    }

    if (isPointer(t0) && isPointer(t1)) {
        if (isVoid(t0->pointer)) {
            return t1;
        }

        if (isVoid(t1->pointer) || typeEquals(t0->pointer, t1->pointer)) {
            return t0;
        }
    }

    if (!isInteger(t0) || !isInteger(t1)) {
        errorStart(diag, location);
        fprintf(stderr, "cannot find common type of '");
        typePrint(stderr, t0);
        fprintf(stderr, "' and '");
        typePrint(stderr, t1);
        fprintf(stderr, "'\n");
        return NULL;
    }

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

