#include "compile_condition.h"
#include "compiler.h"
#include "diag.h"
#include "compile_expression.h"

static void applyCondition(Function* func, Condition cond, ConditionTarget target) {
    /*if (target.invert) {
        cond = CONDITION_INVERT(cond);
    }*/

    if (target.putInA) {
        UNIMPLEMENTED();
    }

    if (target.jumpToLabel) {
        conditionalJump(func, cond, target.label);
    }
}

static void valueToCondition(Function* func, Value value, ConditionTarget target) {
    if (value.kind == VALUE_IMMEDIATE && isLiteral(value.expression)) {
        bool noJump = (bool)value.expression->literal == target.invert;
        applyCondition(func, CONDITION(FLAG_ALWAYS, noJump), target);
        return;
    }

    Condition cond = CONDITION_NZ;
    if (target.invert) {
        cond = CONDITION_INVERT(cond);
    }

    int size = typeSize(value.type);

    load(func, REG_A, value, 0);

    if (size == 1) {
        emitRegValue(func, INS_SUB, REG_A, valueConstant(func->lifetime, &typeUChar, 0, NO_LOCATION), 0);
        applyCondition(func, cond, target);
        return;
    }

    for (int i = 1; i < size; ++i) {
        emitRegValue(func, INS_OR, REG_A, value, i);
    }

    applyCondition(func, cond, target);
}

static void standardCompileCondition(
        Function* func,
        const Expression* expr,
        ConditionTarget target
) {
    Value value = compileExpression(func, expr, ANY_TARGET);
    valueToCondition(func, value, target);
}

static void logicalOr(
        Function* func,
        const Expression* lhs,
        const Expression* rhs,
        ConditionTarget target
) {
    compileCondition(func, lhs, target);
    compileCondition(func, rhs, target);
}

static void logicalAnd(
        Function* func,
        const Expression* lhs,
        const Expression* rhs,
        ConditionTarget target
) {
    Value skipLabel = createLabel(func->compiler);
    compileCondition(func, lhs, conditionTarget(skipLabel, MAINTAIN));
    compileCondition(func, rhs, target);
    emitLabel(func, skipLabel);
}

static void logicalOperator(
        Function* func,
        bool isAnd,
        const Expression* lhs,
        const Expression* rhs,
        ConditionTarget target
) {
    if (isAnd == target.invert) {
        logicalOr(func, lhs, rhs, target);
        return;
    }

    logicalAnd(func, lhs, rhs, target);
}

static void applyNotEqual(
        Function* func,
        const Expression* lhs,
        const Expression* rhs,
        ConditionTarget target
) {
    Value left = compileExpression(func, lhs, ANY_TARGET);
    Value right = compileExpression(func, rhs, ANY_TARGET);

    const Type* common = integerPromotion(func->diag, left.type, right.type, locSpan(exprLoc(lhs), exprLoc(rhs)));
    left = moveValueToTarget(func, left, targetType(common));
    right = moveValueToTarget(func, right, targetType(common));

    int size = typeSize(common);
    load(func, REG_A, left, 0);
    emitRegValue(func, INS_SUB, REG_A, right, 0);
    applyCondition(func, CONDITION_NZ, target);

    for (int i = 1; i < size; ++i) {
        load(func, REG_A, left, i);
        emitRegValue(func, INS_SUBB, REG_A, right, i);
        applyCondition(func, CONDITION_NZ, target);
    }
}

static void applyEqual(
        Function* func,
        const Expression* lhs,
        const Expression* rhs,
        ConditionTarget target
) {
    Value left = compileExpression(func, lhs, ANY_TARGET);
    Value right = compileExpression(func, rhs, ANY_TARGET);

    const Type* common = integerPromotion(func->diag, left.type, right.type, locSpan(exprLoc(lhs), exprLoc(rhs)));
    left = moveValueToTarget(func, left, targetType(common));
    right = moveValueToTarget(func, right, targetType(common));

    Value skipLabel = createLabel(func->compiler);
    ConditionTarget skipTarget = conditionTarget(skipLabel, MAINTAIN);

    int size = typeSize(common);

    for (int i = 0; i < size; ++i) {
        Opcode ins = i == 0 ? INS_SUB : INS_SUBB;
        load(func, REG_A, left, i);
        emitRegValue(func, ins, REG_A, right, i);
        if (i < size - 1) {
            applyCondition(func, CONDITION_NZ, skipTarget);
        }
    }

    applyCondition(func, CONDITION_Z, target);

    if (size > 1) {
        emitLabel(func, skipLabel);
    }
}

static void equality(
        Function* func,
        bool notEqual,
        const Expression* lhs,
        const Expression* rhs,
        ConditionTarget target
) {
    if (notEqual == target.invert) {
        applyEqual(func, lhs, rhs, target);
        return;
    }

    applyNotEqual(func, lhs, rhs, target);
}

static void ordering(
        Function* func,
        bool greaterOrEqual,
        bool opposite,
        const Expression* lhs,
        const Expression* rhs,
        ConditionTarget target
) {
    greaterOrEqual = greaterOrEqual != target.invert;

    Value left = compileExpression(func, lhs, ANY_TARGET);
    Value right = compileExpression(func, rhs, ANY_TARGET);

    if (opposite) {
        Value swap = left;
        left = right;
        right = swap;
    }

    const Type* common = integerPromotion(func->diag, left.type, right.type, locSpan(exprLoc(lhs), exprLoc(rhs)));
    left = moveValueToTarget(func, left, targetType(common));
    right = moveValueToTarget(func, right, targetType(common));

    int size = typeSize(common);
    bool isSigned = common->integer.sign == SIGN_SIGNED;
    Condition cond = isSigned ? CONDITION_N : CONDITION_NC;
    if (greaterOrEqual) {
        cond = CONDITION_INVERT(cond);
    }

    for (int i = 0; i < size; ++i) {
        Opcode ins = i == 0 ? INS_SUB : INS_SUBB;

        load(func, REG_A, left, i);
        emitRegValue(func, ins, REG_A, right, i);
    }

    applyCondition(func, cond, target);
}

static void compileBinaryCondition(
        Function* func,
        const Expression* expr,
        ConditionTarget target
) {
    const Expression* lhs = expr->binary.left;
    const Expression* rhs = expr->binary.right;

    switch (expr->binary.operation) {
        case BINARY_LOGICAL_OR:
            logicalOperator(func, false, lhs, rhs, target);
            break;
        case BINARY_LOGICAL_AND:
            logicalOperator(func, true, lhs, rhs, target);
            break;
        case BINARY_EQUAL:
            equality(func, false, lhs, rhs, target);
            break;
        case BINARY_NOT_EQUAL:
            equality(func, true, lhs, rhs, target);
            break;
        case BINARY_LESS:
            ordering(func, false, false, lhs, rhs, target);
            break;
        case BINARY_GREATER_EQUAL:
            ordering(func, true, false, lhs, rhs, target);
            break;
        case BINARY_GREATER:
            ordering(func, false, true, lhs, rhs, target);
            break;
        case BINARY_LESS_EQUAL:
            ordering(func, true, true, lhs, rhs, target);
            break;
        default:
            standardCompileCondition(func, expr, target);
            break;
    }
}

static void compileUnaryCondition(
        Function* func,
        const Expression* expr,
        ConditionTarget target
) {
    switch (expr->unary.operation) {
        case UNARY_NOT:
            compileCondition(func, expr->unary.inner, invertConditionTarget(target));
            break;
        default:
            standardCompileCondition(func, expr, target);
            break;
    }
}

void compileCondition(
        Function* func,
        const Expression* expr,
        ConditionTarget target
) {
    switch (expr->type) {
        case EXPR_BINARY:
            compileBinaryCondition(func, expr, target);
            break;
        case EXPR_UNARY:
            compileUnaryCondition(func, expr, target);
            break;
        case EXPR_STRING:
        case EXPR_CAST:
        case EXPR_ASSIGN:
        case EXPR_LABEL:
        case EXPR_LITERAL:
        case EXPR_STACKOFFSET:
        case EXPR_VARIABLE:
        case EXPR_CALL:
        case EXPR_SIZEOF:
            standardCompileCondition(func, expr, target);
            break;
    }
}
