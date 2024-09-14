#ifndef COMPILER_H
#define COMPILER_H

#include "expression.h"

typedef enum {
    VALUE_IMMEDIATE,
    VALUE_DIRECT,
} ValueType;

typedef struct {
    ValueType kind;
    union {
        Expression* expression;
    };
} Value;

#endif

