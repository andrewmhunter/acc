#include "compiler.h"
#include "mem.h"
#include "util.h"
#include "diag.h"
#include "condition.h"
#include <stdio.h>

/// ############################################################################
/// Initalizers
/// ############################################################################

Compiler compilerNew(Arena* staticLifetime) {
    return (Compiler) {
        .label = 0,
        .lifetime = staticLifetime,
    };
}

Function functionNew(Compiler* compiler) {
    return (Function) {
        .compiler = compiler,
        .lifetime = compiler->lifetime,
        .instructions = NULL,
        .instructionsLength = 0,
        .instructionsCapacity = 0,
        .stack = NULL,
        .stackLength = 0,
        .stackCapacity = 0,
        .currentStackSize = 0,
        .maxStackSize = 0,
        .scopeDepth = 0,
    };
}

/// ############################################################################
/// Instruction generation
/// ############################################################################

static void appendInstruction(Function* func, Instruction instruction) {
    printInstruction(stdout, &instruction);
    APPEND_ARRAY(
        func->lifetime,
        func->instructions,
        Instruction,
        func->instructionsCapacity,
        func->instructionsLength,
        instruction
    );
}

static void emitImplied(Function* func, Opcode opcode) {
    appendInstruction(func, instruction0(opcode));
}

static void emitReg(Function* func, Opcode opcode, Reg reg) {
    appendInstruction(func, instruction1(opcode, addressReg(reg)));
}

static void emitValue(Function* func, Opcode opcode, Value value, int index) {
    if (value.kind == VALUE_DISCARD) {
        UNREACHABLE();
    }
    appendInstruction(func, instruction1Value(
                func->lifetime,
                opcode,
                GET_ADDRESS_VALUE(),
                value,
                index
            ));
}

static void emitRegReg(Function* func, Opcode opcode, Reg dest, Reg src) {
    appendInstruction(func, instruction2(opcode, addressReg(dest), addressReg(src)));
}

static void emitRegValue(Function* func, Opcode opcode, Reg dest, Value src, int index) {
    if (src.kind == VALUE_DISCARD) {
        UNREACHABLE();
    }
    appendInstruction(func, instruction2Value(
                func->lifetime,
                opcode,
                addressReg(dest),
                GET_ADDRESS_VALUE(),
                src,
                index
            ));
}

static void emitValueReg(Function* func, Opcode opcode, Value dest, int index, Reg src) {
    if (dest.kind == VALUE_DISCARD) {
        UNREACHABLE();
    }
    appendInstruction(func, instruction2Value(
                func->lifetime,
                opcode,
                GET_ADDRESS_VALUE(),
                addressReg(src),
                dest,
                index
            ));
}

static void emitLabel(Function* func, Value label) {
    emitValue(func, INS_LABEL, label, 0);
}

static void load(Function* func, Reg reg, Value src, int index) {
    emitRegValue(func, INS_MOV, reg, src, index);
}

static void store(Function* func, Value dest, int index, Reg reg) {
    if (dest.kind == VALUE_DISCARD) {
        return;
    }
    emitValueReg(func, INS_MOV, dest, index, reg);
}

static void transfer(Function* func, Reg dest, Reg src) {
    emitRegReg(func, INS_MOV, dest, src);
}

static Value createLabel(Compiler* compiler) {
    Label label = compiler->label++;
    return valueImmediateExpr(typeAnyInteger(), exprLabel(compiler->lifetime, label));
}

static Value pushStack(Function* func, const Type* type, Identifier ident) {
    func->stack = EXTEND_ARRAY(
        func->lifetime,
        func->stack,
        StackElement,
        &func->stackCapacity,
        func->stackLength,
        1
    );
    size_t offset = func->currentStackSize;

    StackElement* element = &func->stack[func->stackLength++];
    element->type = type;
    element->ident = ident;
    element->depth = func->scopeDepth;
    element->offset = offset;

    func->currentStackSize += typeSize(type);
    func->maxStackSize = MAX(func->maxStackSize, func->currentStackSize);

    return valueStackOffset(func->lifetime, type, offset);
}

static Value pushStackAnon(Function* func, const Type* type) {
    Identifier id = {.start = NULL, .length = 0};
    return pushStack(func, type, id);
}

static void popStack(Function* func) {
    StackElement* element = &func->stack[func->stackLength - 1];
    func->currentStackSize -= typeSize(element->type);
    --func->stackLength;

    ASSERT(func->stackLength >= 0 && func->currentStackSize >= 0, "stack empty");
}

static void emitJump(Function* func, Value value) {
    emitValue(func, INS_JMP, value, 0);
}

static void conditionalJump(Function* func, Condition condition, Value value) {
    Opcode ins = INS_JMP;

    if (!condition.negate) {
        switch (condition.flag) {
            case FLAG_Z:
                ins = INS_JZ;
                break;
            case FLAG_C:
                ins = INS_JC;
                break;
            case FLAG_N:
                ins = INS_JN;
                break;
            case FLAG_ALWAYS:
                ins = INS_JMP;
                break;
        }
    } else {
        switch (condition.flag) {
            case FLAG_Z:
                ins = INS_JNZ;
                break;
            case FLAG_C:
                ins = INS_JNC;
                break;
            case FLAG_N:
                ins = INS_JP;
                break;
            case FLAG_ALWAYS:
                return;
        }
    }

    emitValue(func, ins, value, 0);
}

static void enterScope(Function* func) {
    func->scopeDepth++;
}

static void leaveScope(Function* func) {
    ASSERT(func->scopeDepth > 0, "already at outermost scope");
    
    func->scopeDepth--;
    for (int i = func->stackLength - 1; i >= 0; --i) {
        StackElement* element = &func->stack[i];
        if (element->depth <= func->scopeDepth) {
            break;
        }

        popStack(func);
    }
}

static Value lookupSymbol(Function* func, Identifier ident) {
    for (int i = func->stackLength - 1; i >= 0; --i) {
        if (identEquals(ident, func->stack[i].ident)) {
            return valueStackOffset(func->lifetime, func->stack[i].type, func->stack[i].offset);
        }
    }

    PANIC("failed to lookup symbol");
}


/// ############################################################################
/// Expressions
/// ############################################################################

static Value getValueTarget(Function* func, Target target, const Type* type) {
    switch (target.kind) {
        case TARGET_VALUE:
            return target.value;
        case TARGET_ANY:
            return pushStackAnon(func, type);
        case TARGET_DISCARD:
            return valueDiscard();
    }
}

static const Type* getTargetTypeOr(Target target, const Type* type) {
    if (target.kind == TARGET_VALUE) {
        return target.value.type;
    }
    return type;
}

static Value compileExpression(Function* func, const Expression* expr, Target target);



// TODO: Very poor generated code. Need to add multipart value types.
static Value widenUnsignedInteger(
        Function* func,
        Value value,
        const Type* toType,
        Target target
) {
    ASSERT(isInteger(value.type), "must be integer");
    ASSERT(value.type->integer.sign != SIGN_SIGNED, "must not be signed");

    Value tvalue = getValueTarget(func, target, toType);

    int fromSize = typeSize(value.type);
    int toSize = typeSize(toType);

    for (int i = 0; i < fromSize; ++i) {
        load(func, REG_A, value, i);
        store(func, tvalue, i, REG_A);
    }

    load(func, REG_A, valueConstant(func->lifetime, typeUChar(), 0), 0);
    for (int i = fromSize; i < toSize; ++i) {
        store(func, tvalue, i, REG_A);
    }
    return tvalue;
}

static Value implicitCast(Function* func, Value value, const Type* toType, Target target) {
    const Type* fromType = value.type;

    if (typeCompatible(fromType, toType) || toType->tag == TYPE_VOID) {
        value.type = toType;
        return value;
    }

    if (isInteger(fromType) && isInteger(toType)) {
        if (toType->integer.size <= fromType->integer.size) {
            value.type = toType;
            return value;
        }

        if (fromType->integer.sign == SIGN_SIGNED) {
            UNIMPLEMENTED();
        }
        return widenUnsignedInteger(func, value, toType, target);
    }

    UNREACHABLE();
}

static Value moveValueToValue(Function* func, Value src, Value dest) {
    if (dest.type->tag == TYPE_VOID) {
        return dest;
    }

    if (valueEquals(&src, &dest)) {
        return dest;
    }

    if (!typeCompatible(src.type, dest.type)) {
        typePrint(stderr, src.type);
        fprintf(stderr, " into ");
        typePrint(stderr, dest.type);
        fprintf(stderr, "\n");
        PANIC("cannot move into incompatible type");
    }

    for (int i = 0; i < typeSize(dest.type); ++i) {
        load(func, REG_A, src, i);
        store(func, dest, i, REG_A);
    }

    return dest;
}

static Value moveValueToTarget(Function* func, Value value, Target target) {
    const Type* targetType = getTargetTypeOr(target, value.type);
    value = implicitCast(func, value, targetType, target);

    switch (target.kind) {
        case TARGET_ANY:
            return value;
        case TARGET_VALUE:
            return moveValueToValue(func, value, target.value);
        case TARGET_DISCARD:
            break;
    }
    return valueDiscard();
}

static Value assignValue(Function* func, Value lhs, Value rhs, Target target) {
    rhs = implicitCast(func, rhs, lhs.type, targetValue(lhs));
    return moveValueToTarget(func, rhs, target);
}

static Value assignExpression(Function* func, const Expression* expr, Target target) {
    Value lhs = compileExpression(func, expr->binary.left, ANY_TARGET);
    compileExpression(func, expr->binary.right, targetValue(lhs));
    return moveValueToTarget(func, lhs, target);
}

static Value performSimpleBinary(
    Function* func,
    Value lhs,
    Value rhs,
    Opcode first,
    Opcode rest,
    BinaryOperation binop,
    Target target
) {
    ASSERT(isInteger(lhs.type) && isInteger(rhs.type), "must be integer");

    const Type* common = commonType(lhs.type, rhs.type);

    lhs = implicitCast(func, lhs, common, ANY_TARGET);
    rhs = implicitCast(func, rhs, common, ANY_TARGET);

    if (isImmediate(&lhs) && isImmediate(&rhs)) {
        const Expression* expr = exprBinary(
            func->lifetime, binop, lhs.expression, rhs.expression
        );
        Value value = valueImmediateExpr(common, expr);
        return moveValueToTarget(func, value, target);
    }

    Value tvalue = getValueTarget(func, target, lhs.type);

    load(func, REG_A, lhs, 0);
    emitRegValue(func, first, REG_A, rhs, 0);
    store(func, tvalue, 0, REG_A);

    int size = typeSize(lhs.type);
    for (int i = 1; i < size; ++i) {
        load(func, REG_A, lhs, i);
        emitRegValue(func, rest, REG_A, rhs, i);
        store(func, tvalue, i, REG_A);
    }

    return tvalue;
}

static Value pointerAllowedArithmetic(
    Function* func,
    Value lhs,
    Value rhs,
    Opcode first,
    Opcode rest,
    BinaryOperation binop,
    bool commutative,
    Target target
) {
    /*if (isPointer(rhs.type)) {
        Value swap = rhs;
        rhs = lhs;
        lhs = swap;
    }
    if (isPointer(lhs.type)) {
        ASSERT(isInteger(rhs.type), "must be integer");

    }*/

    return performSimpleBinary(func, lhs, rhs, first, rest, binop, target);
}

static Value addValues(Function* func, Value lhs, Value rhs, Target target) {
    return pointerAllowedArithmetic(
        func, lhs, rhs, INS_ADD, INS_ADDC, BINARY_ADD, true, target
    );
}

static Value subtractValues(Function* func, Value lhs, Value rhs, Target target) {
    return pointerAllowedArithmetic(
        func, lhs, rhs, INS_SUB, INS_SUBB, BINARY_SUBTRACT, false, target
    );
}

static Value leftShiftValue(Function* func, Value toShift, Value shiftBy, Target target, bool noOptimize);

static Value leftShiftByImmediate(Function* func, Value toShift, int shiftBy, Target target) {
    if (shiftBy == 0) {
        return moveValueToTarget(func, toShift, target);
    }

    Value tvalue = getValueTarget(func, target, toShift.type);

    if (shiftBy >= typeSize(tvalue.type) * 8) {
        return moveValueToValue(func, valueZero(tvalue.type), tvalue);
    }

    int size = typeSize(tvalue.type);
    for (int i = 0; i < shiftBy; ++i) {
        Opcode ins = INS_SHL;
        for (int j = 0; j < size; ++j) {
            load(func, REG_A, toShift, j);
            emitReg(func, ins, REG_A);
            store(func, tvalue, j, REG_A);

            ins = INS_ROL;
        }
    }

    return tvalue;
}

static Value leftShiftValue(Function* func, Value toShift, Value shiftBy, Target target, bool noOptimize) {
    ASSERT(isInteger(toShift.type) && isInteger(shiftBy.type), "must be integers");

    if (isImmediate(&toShift) && isImmediate(&shiftBy)) {
        const Expression* expr = exprBinary(
                func->lifetime,
                BINARY_SHIFT_LEFT,
                toShift.expression,
                shiftBy.expression
            );

        return valueImmediateExpr(toShift.type, expr);
    }

    int literal = 0;
    if (immediateResolved(&shiftBy, &literal)) {
        return leftShiftByImmediate(func, toShift, literal, target);
    }

    UNIMPLEMENTED();
}

static Value multiplyByImmediate(Function* func, Value lhs, int rhs, Target target) {
    if (rhs == 0) {
        return moveValueToTarget(func, valueZero(lhs.type), target);
    }

    if (rhs > 0 && intIsPowerOf2(rhs)) {
        return leftShiftByImmediate(func, lhs, ceilLog2(rhs), target);
    }

    UNIMPLEMENTED();
}

static Value multiplyValues(Function* func, Value lhs, Value rhs, Target target) {
    ASSERT(isInteger(lhs.type) && isInteger(rhs.type), "must be integer");

    const Type* common = commonType(lhs.type, rhs.type);

    lhs = implicitCast(func, lhs, common, ANY_TARGET);
    rhs = implicitCast(func, rhs, common, ANY_TARGET);

    if (isImmediate(&lhs) && isImmediate(&rhs)) {
        const Expression* expr = exprBinary(
                func->lifetime,
                BINARY_MULTIPLY,
                lhs.expression,
                rhs.expression
            );

        Value value = valueImmediateExpr(common, expr);
        return moveValueToTarget(func, value, target);
    }

    int literal = 0;
    if (immediateResolved(&rhs, &literal)) {
        return multiplyByImmediate(func, lhs, literal, target);
    }
    if (immediateResolved(&lhs, &literal)) {
        return multiplyByImmediate(func, rhs, literal, target);
    }

    UNIMPLEMENTED();
}


static Value binaryExpression(
    Function* func,
    BinaryOperation operation,
    Value lhs,
    Value rhs,
    Target target
) {
    switch (operation) {
        case BINARY_ADD:
            return addValues(func, lhs, rhs, target);
        case BINARY_SUBTRACT:
            return subtractValues(func, lhs, rhs, target);
        case BINARY_SHIFT_LEFT:
            return leftShiftValue(func, lhs, rhs, target, false);
        case BINARY_MULTIPLY: 
            return multiplyValues(func, lhs, rhs, target);
        case BINARY_SHIFT_RIGHT:
        case BINARY_DIVIDE:
        case BINARY_LOGICAL_OR:
        case BINARY_LOGICAL_AND:
        case BINARY_EQUAL:
        case BINARY_NOT_EQUAL:
        case BINARY_LESS:
        case BINARY_LESS_EQUAL:
        case BINARY_GREATER:
        case BINARY_GREATER_EQUAL:
            UNIMPLEMENTED();
            break;
        case BINARY_NONE:
            UNREACHABLE();
    }
    UNREACHABLE();
}

static Value compileBinaryExpression(
        Function* func,
        const Expression* expr,
        Target target
) {
    Value lhs = compileExpression(func, expr->binary.left, ANY_TARGET);
    Value rhs = compileExpression(func, expr->binary.right, ANY_TARGET);

    return binaryExpression(func, expr->binary.operation, lhs, rhs, target);
}

static Value negateValue(Function* func, Value inner, Target target) {
    if (inner.kind == VALUE_IMMEDIATE) {
        const Expression* expr = exprUnary(func->lifetime, UNARY_NEGATE, inner.expression);
        Value value = valueImmediateExpr(inner.type, expr);
        return moveValueToTarget(func, value, target);
    }

    Value zero = valueConstant(func->lifetime, inner.type, 0);
    return subtractValues(func, zero, inner, target);
}

static Value addressOf(Function* func, Value inner, Target target) {
    ASSERT(inner.kind == VALUE_DIRECT, "cannot get the address of value");

    inner.kind = VALUE_IMMEDIATE;
    inner.type = typePointer(func->lifetime, inner.type);
    return moveValueToTarget(func, inner, target);
}

static Value dereference(Function* func, Value inner, Target target) {
    ASSERT(isPointer(inner.type), "can only dereference pointer");

    if (inner.kind == VALUE_IMMEDIATE) {
        inner.kind = VALUE_DIRECT;
        inner.type = inner.type->pointer;
        return moveValueToTarget(func, inner, target);
    }

    UNIMPLEMENTED();
}

static Value compileUnaryExpression(Function* func, const Expression* expr, Target target) {
    Value inner = compileExpression(func, expr->unary.inner, ANY_TARGET);

    switch (expr->unary.operation) {
        case UNARY_NEGATE:
            return negateValue(func, inner, target);
        case UNARY_ADDRESSOF:
            return addressOf(func, inner, target);
        case UNARY_DEREFERENCE:
            return dereference(func, inner, target);
        case UNARY_NOT:
            UNIMPLEMENTED();
        
    }
}

static Value compileVariableExpression(Function* func, Identifier ident, Target target) {
    return moveValueToTarget(func, lookupSymbol(func, ident), target);
}

static Value compileLiteralExpression(Function* func, const Expression* expr, Target target) {
    Value value = valueImmediateExpr(typeAnyInteger(), expr);
    return moveValueToTarget(func, value, target);
}

static Value compileExpression(Function* func, const Expression* expr, Target target) {
    switch (expr->type) {
        case EXPR_LABEL:
        case EXPR_STACKOFFSET:
        case EXPR_LITERAL:
            return compileLiteralExpression(func, expr, target);
        case EXPR_VARIABLE:
            return compileVariableExpression(func, expr->variable, target);
        case EXPR_BINARY:
            return compileBinaryExpression(func, expr, target);
        case EXPR_UNARY:
            return compileUnaryExpression(func, expr, target);
        case EXPR_ASSIGN:
            return assignExpression(func, expr, target);
        case EXPR_CAST:
            UNIMPLEMENTED();
    }

    UNREACHABLE();
}


/// ############################################################################
/// Conditions
/// ############################################################################

static void compileCondition(Function* func, const Expression* expr, ConditionTarget target);

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
        emitRegValue(func, INS_SUB, REG_A, valueConstant(func->lifetime, typeUChar(), 0), 0);
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

    const Type* common = commonType(left.type, right.type);
    left = implicitCast(func, left, common, ANY_TARGET);
    right = implicitCast(func, right, common, ANY_TARGET);

    int size = typeSize(common);
    load(func, REG_A, left, 0);
    emitRegValue(func, INS_SUB, REG_A, right, 0);
    applyCondition(func, CONDITION_NZ, target);

    for (int i = 1; i < size; ++i) {
        load(func, REG_A, left, i);
        emitRegValue(func, INS_SUBB, REG_A, left, i);
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

    const Type* common = commonType(left.type, right.type);
    left = implicitCast(func, left, common, ANY_TARGET);
    right = implicitCast(func, right, common, ANY_TARGET);

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

    const Type* common = commonType(left.type, right.type);
    left = implicitCast(func, left, common, ANY_TARGET);
    right = implicitCast(func, right, common, ANY_TARGET);

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

static void compileCondition(
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
        case EXPR_CAST:
        case EXPR_ASSIGN:
        case EXPR_LABEL:
        case EXPR_LITERAL:
        case EXPR_STACKOFFSET:
        case EXPR_VARIABLE:
            standardCompileCondition(func, expr, target);
            break;
    }
}

/// ############################################################################
/// Statements
/// ############################################################################

static void compileStatement(Function* func, Statement* stmt);

static void compileBlock(Function* func, Statement* stmt) {
    enterScope(func);
    for (size_t i = 0; i < stmt->block.length; ++i) {
        compileStatement(func, stmt->block.data[i]);
    }
    leaveScope(func);
}

static void compileVariable(Function* func, Statement* stmt) {
    for (int i = func->stackLength - 1; i >= 0; --i) {
        if (func->stack[i].depth != func->scopeDepth) {
            break;
        }

        if (identEquals(stmt->variableDeclaration.id, func->stack[i].ident)) {
            PANIC("redefined variable");
        }
    }

    Value value = pushStack(func,
            stmt->variableDeclaration.type,
            stmt->variableDeclaration.id);

    if (stmt->variableDeclaration.expr) {
        compileExpression(func, stmt->variableDeclaration.expr, targetValue(value));
    }
}

static void compileIf(Function* func, Statement* stmt) {
    Value elseLabel = createLabel(func->compiler);
    compileCondition(func, stmt->conditional.condition, conditionTarget(elseLabel, INVERT));
    compileStatement(func, stmt->conditional.inner);

    Value endLabel;
    if (stmt->conditional.onElse != NULL) {
        endLabel = createLabel(func->compiler);
        emitJump(func, endLabel);
    }

    emitLabel(func, elseLabel);

    if (stmt->conditional.onElse != NULL) {
        compileStatement(func, stmt->conditional.onElse);
        emitLabel(func, endLabel);
    }
}

static void compileWhile(Function* func, Statement* stmt) {
    Value loopLabel = createLabel(func->compiler);
    Value endLabel = createLabel(func->compiler);

    emitLabel(func, loopLabel);
    compileCondition(func, stmt->whileLoop.condition, conditionTarget(endLabel, INVERT));
    compileStatement(func, stmt->whileLoop.inner);
    emitJump(func, loopLabel);
    emitLabel(func, endLabel);
}


static void compileStatement(Function* func, Statement* stmt) {
    fprintf(stdout, "; ");
    stmtPrint(stdout, stmt, 0, true);
    switch (stmt->type) {
        case STATEMENT_BLOCK:
            compileBlock(func, stmt);
            break;
        case STATEMENT_EXPRESSION:
            enterScope(func);
            compileExpression(func, stmt->expression, DISCARD_TARGET);
            leaveScope(func);
            break;
        case STATEMENT_VARIABLE:
            compileVariable(func, stmt);
            break;
        case STATEMENT_IF:
            compileIf(func, stmt);
            break;
        case STATEMENT_WHILE:
            compileWhile(func, stmt);
            break;
    }
}

void compileFunction(Compiler* compiler, Statement* stmt) {
    Function func = functionNew(compiler);

    compileStatement(&func, stmt);
    emitImplied(&func, INS_RET);

    //for (int i = 0; i < func.instructionsLength; ++i) {
    //    printInstruction(stdout, &func.instructions[i]);
    //}
    fprintf(stdout, "variable _Stack %lu\n", func.maxStackSize);
    fprintf(stderr, "\n; Instruction count: %lu\n", func.instructionsLength);
}

