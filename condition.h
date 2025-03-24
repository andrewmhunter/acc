#ifndef CONDITION_H
#define CONDITION_H

#include <stdio.h>
#include <stdbool.h>

typedef bool Invert;

#define INVERT ((Invert)true)
#define MAINTAIN ((Invert)false)

typedef enum {
    FLAG_ALWAYS,
    FLAG_C,
    FLAG_Z,
    FLAG_N,
} Flag;

typedef struct {
    Flag flag;
    Invert negate;
} Condition;

#define CONDITION(FLAG, NEGATE) ((Condition){.flag = (FLAG), .negate = (NEGATE)})
#define CONDITION_INVERT(OLD) (CONDITION((OLD).flag, !(OLD).negate))

#define CONDITION_ALWAYS CONDITION(FLAG_ALWAYS, MAINTAIN)
#define CONDITION_NEVER CONDITION(FLAG_ALWAYS, INVERT)
#define CONDITION_C CONDITION(FLAG_C, MAINTAIN)
#define CONDITION_NC CONDITION(FLAG_C, INVERT)
#define CONDITION_Z CONDITION(FLAG_Z, MAINTAIN)
#define CONDITION_NZ CONDITION(FLAG_Z, INVERT)
#define CONDITION_N CONDITION(FLAG_N, MAINTAIN)
#define CONDITION_P CONDITION(FLAG_N, INVERT)

void conditionPrint(FILE* file, Condition condition);
bool conditionEqual(Condition condition0, Condition condition1);

#endif

