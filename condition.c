#include "condition.h"
#include <stdio.h>

void conditionPrint(FILE* file, Condition condition) {
    fprintf(file, "<cond ");
    if (condition.negate) {
        fprintf(file, "not ");
    }
    switch (condition.flag) {
        case FLAG_ALWAYS:
            fprintf(file, "always");
            break;
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
}

bool conditionEqual(Condition condition0, Condition condition1) {
    return condition0.flag == condition1.flag
        && condition0.negate == condition1.negate;
}

