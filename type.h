#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>

typedef enum {
    TYPE_VOID,
    TYPE_INTEGER,
    TYPE_POINTER,
} TypeTag;

typedef enum {
    SIGN_SIGNED,
    SIGN_UNSIGNED,
} Signedness;

typedef enum {
    SIZE_BYTE,
    SIZE_INT,
} IntegerSize;

typedef struct Type {
    TypeTag tag;
    union {
        struct {
            IntegerSize size;
            Signedness sign;
        } integer;
        struct Type* pointer;
    };
} Type;

Type* typeNew(TypeTag tag);
Type* typeInteger(Signedness sign, IntegerSize size);
Type* typePointer(Type* inner);
void typeFree(Type* type);

void typePrint(Type* type);
bool typeEquals(Type* t0, Type* t1);

#endif

