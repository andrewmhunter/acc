#ifndef COMPILE_EXPRESSION_H
#define COMPILE_EXPRESSION_H

#include "compiler.h"

Value moveValueToValue(Function* func, Value src, Value dest);
Value moveValueToTarget(Function* func, Value value, Target target);
Value compileExpression(Function* func, const Expression* expr, Target target);

#endif

