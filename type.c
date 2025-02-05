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
    type->array.element = element;
    type->array.length = length;
    return type;
}

//const Type* typeVoid() {
//    static Type inner = (Type) {.tag = TYPE_VOID};
//    return &inner;
//}
//
//const Type* typeUChar() {
//    static Type inner = (Type) {.tag = TYPE_INTEGER, .integer = {.size = SIZE_BYTE, .sign = SIGN_UNSIGNED}};
//    return &inner;
//}

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
                case SIZE_ANY:
                    return WORD_SIZE;
                    assert(false && "any sized interger must be converted to"
                            "a sized integer before finding the size");
            }
            break;
        case TYPE_POINTER:
            return WORD_SIZE;
        case TYPE_ARRAY:
            return typeSize(type->array.element) * type->array.length;
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
        case TYPE_ARRAY:
            typePrint(file, type->array.element);
            fprintf(file, "[%d]", type->array.length);
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
            return typeEquals(t0->array.element, t1->array.element)
                && t0->array.length == t1->array.length;
        case TYPE_VOID:
            return true;
    }
}

bool typeCompatible(const Type* t0, const Type* t1) {
    if (t0 == NULL || t1 == NULL) {
        return true;
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
    }
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
    return type != NULL ? type->tag == TYPE_VOID : false;
}

bool isInteger(const Type* type) {
    return type != NULL ? type->tag == TYPE_INTEGER : false;
}

bool isPointer(const Type* type) {
    return type != NULL ? type->tag == TYPE_POINTER : false;
}

const Type* integerPromotion(const Type* t0, const Type* t1) {
    ASSERT(t0 && t1, "cannot promote null type");
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

