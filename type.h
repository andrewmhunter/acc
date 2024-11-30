#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>
#include <stdio.h>
#include "mem.h"

#define WORD_SIZE 2

typedef enum {
    TYPE_VOID,
    TYPE_INTEGER,
    TYPE_POINTER,
} TypeTag;

typedef enum {
    SIGN_EITHER,
    SIGN_SIGNED,
    SIGN_UNSIGNED,
} Signedness;

typedef enum {
    SIZE_ANY = 0,
    SIZE_BYTE = 1,
    SIZE_INT = 2,
} IntegerSize;

typedef struct Type {
    TypeTag tag;
    union {
        struct {
            IntegerSize size;
            Signedness sign;
        } integer;
        const struct Type* pointer;
    };
} Type;

Type* typeNew(Arena* arena, TypeTag tag);
const Type* typeInteger(Arena* arena, Signedness sign, IntegerSize size);
const Type* typePointer(Arena* arena, const Type* inner);
const Type* typeVoid();

const Type* typeUChar();
const Type* typeAnyInteger();

int typeSize(const Type* type);

void typePrint(FILE* file, const Type* type);
bool typeEquals(const Type* t0, const Type* t1);
bool typeCompatible(const Type* t0, const Type* t1);

bool isInteger(const Type* type);
bool isPointer(const Type* type);
const Type* commonType(const Type* t0, const Type* t1);

#endif

