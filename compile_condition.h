#ifndef COMPILE_CONDITION_H
#define COMPILE_CONDITION_H

#include "compiler.h"
#include "expression.h"

void compileCondition(
        Function* func,
        const Expression* expr,
        ConditionTarget target
);

#endif

