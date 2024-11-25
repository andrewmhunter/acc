#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>
#include <stdio.h>
#include "mem.h"

#define WORD_SIZE 2

typedef enum {
    FLAG_C,
    FLAG_Z,
    FLAG_N,
} Flag;

typedef enum {
    TYPE_VOID,
    TYPE_INTEGER,
    TYPE_POINTER,
    TYPE_CONDITION,
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
        struct Type* pointer;
        struct {
            Flag flag;
            bool negate;
        } condition;
    };
} Type;

Type* typeNew(Arena* arena, TypeTag tag);
Type* typeInteger(Arena* arena, Signedness sign, IntegerSize size);
Type* typePointer(Arena* arena, Type* inner);
Type* typeCondition(Arena* arena, Flag flag, bool negate);
Type* typeVoid();

int typeSize(Type* type);

void typePrint(FILE* file, Type* type);
bool typeEquals(Type* t0, Type* t1);
bool typeCompatible(Type* t0, Type* t1);

bool isInteger(Type* type);
bool isPointer(Type* type);
Type* commonType(Type* t0, Type* t1);

#endif

