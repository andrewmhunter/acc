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

Type* typeInteger(Arena* arena, Signedness sign, IntegerSize size) {
    Type* type = typeNew(arena, TYPE_INTEGER);
    type->integer.sign = sign;
    type->integer.size = size;
    return type;
}

Type* typePointer(Arena* arena, Type* inner) {
    Type* type = typeNew(arena, TYPE_POINTER);
    type->pointer = inner;
    return type;
}

Type* typeCondition(Arena* arena, Flag flag, bool negate) {
    Type* type = typeNew(arena, TYPE_CONDITION);
    type->condition.flag = flag;
    type->condition.negate = negate;
    return type;
}

Type* typeVoid() {
    static Type inner = (Type) {.tag = TYPE_VOID};
    return &inner;
}

int typeSize(Type* type) {
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
        case TYPE_CONDITION:
        case TYPE_VOID:
            return 1;
    }
}

void typePrint(FILE* file, Type* type) {
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
        case TYPE_CONDITION:
            fprintf(file, "<cond ");
            if (type->condition.negate) {
                fprintf(file, "not ");
            }
            switch (type->condition.flag) {
                case FLAG_C:
                    fprintf(file, "carry");
                    break;
                case FLAG_Z:
                    fprintf(file, "zero");
                    break;
                case FLAG_N:
                    fprintf(file, "negative");
                    break;
            }
            fprintf(file, ">");
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
        case TYPE_CONDITION:
            return t0->condition.flag == t1->condition.flag
                && t0->condition.negate == t1->condition.negate;
        case TYPE_VOID:
            return true;
    }
}

bool typeCompatible(Type* t0, Type* t1) {
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
        case TYPE_CONDITION:
            return t0->condition.flag == t1->condition.flag
                && t0->condition.negate == t1->condition.negate;
        case TYPE_VOID:
            return true;
    }
}

bool isInteger(Type* type) {
    return type->tag == TYPE_INTEGER;
}

bool isPointer(Type* type) {
    return type->tag == TYPE_POINTER;
}

Type* commonType(Type* t0, Type* t1) {
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

