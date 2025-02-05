#include "compile_expression.h"
#include "compiler.h"
#include "diag.h"
#include "util.h"


#define ASSERT_ERROR(CONDITION, FUNC, LOCATION, FORMAT, ...) do { \
        if (!(CONDITION)) { \
            printError((FUNC)->diag, LOCATION, (FORMAT) __VA_OPT__(,) __VA_ARGS__); \
            return valueError(); \
        } \
    } while (0)


static Value getValueTarget(Function* func, Target target, const Type* type) {
    switch (target.kind) {
        case TARGET_VALUE:
            return target.value;
        case TARGET_TYPE:
            return pushStackAnon(func, target.type, NO_LOCATION);
        case TARGET_ANY:
            return pushStackAnon(func, type, NO_LOCATION);
        case TARGET_DISCARD:
            return valueDiscard();
    }
}

// TODO: Very poor generated code. Need to add multipart value types.
static Value widenUnsignedInteger(
        Function* func,
        Value value,
        const Type* toType,
        Target target
) {
    ASSERT_ERROR(isInteger(value.type), func, valueLoc(&value),
            "must be integer");
    ASSERT_ERROR(value.type->integer.sign != SIGN_SIGNED, func, valueLoc(&value),
            "must not be signed");


    int fromSize = typeSize(value.type);
    // TODO: this might be an error. check it.
    int toSize = typeSize(toType);

    ValueList list = valueList(func->lifetime); 
    valueListPush(&list, value, fromSize, 0);

    Value zero = valueConstant(func->lifetime, &typeUChar, 0, NO_LOCATION);
    valueListPush(&list, zero, toSize - fromSize, 0);

    Value widenedValue = valueMultipart(func->lifetime, toType, &list);
    return moveValueToTarget(func, widenedValue, target);
}

static Value widenSignedInteger(
        Function* func,
        Value value,
        const Type* toType,
        Target target
) {
    ASSERT(isInteger(value.type), "must be integer");
    ASSERT(value.type->integer.sign == SIGN_SIGNED, "must be signed");

    Value tvalue = getValueTarget(func, target, toType);
    toType = tvalue.type;

    int fromSize = typeSize(value.type);
    // TODO: this might be an error. check it.
    int toSize = typeSize(toType);


    for (int i = 0; i < fromSize; ++i) {
        load(func, REG_A, value, i);
        store(func, tvalue, i, REG_A);
    }

    emitRegValue(func, INS_SUB, REG_A, valueZero(&typeUChar), 0);
    load(func, REG_A, valueZero(&typeUChar), 0);

    Value positiveLabel = createLabel(func->compiler);
    conditionalJump(func, CONDITION_P, positiveLabel);
    emitReg(func, INS_NOT, REG_A);
    emitLabel(func, positiveLabel);

    for (int i = fromSize; i < toSize; ++i) {
        store(func, tvalue, i, REG_A);
    }
    return tvalue;
}

static Value internalImplicitCast(
        Function* func,
        Value value,
        const Type* toType,
        Target target
) {
    if (isValueError(&value)) {
        return value;
    }

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
            return widenSignedInteger(func, value, toType, target);
        }
        return widenUnsignedInteger(func, value, toType, target);
    }

    printError(func->diag, valueLoc(&value), "cannot convert between types");
    return valueError();
}

Value moveValueToValue(Function* func, Value src, Value dest) {
    if (isValueError(&src) || isValueError(&dest)) {
        return valueError();
    }

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

Value moveValueToTarget(Function* func, Value value, Target target) {
    const Type* targetType = getTargetTypeOr(&target, value.type);
    value = internalImplicitCast(func, value, targetType, target);

    switch (target.kind) {
        case TARGET_TYPE:
        case TARGET_ANY:
            return value;
        case TARGET_VALUE:
            return moveValueToValue(func, value, target.value);
        case TARGET_DISCARD:
            break;
    }
    return valueDiscard();
}

static Value explicitCast(
        Function* func,
        const Expression* expr,
        const Type* into,
        Target target
) {
    Value value = compileExpression(func, expr, ANY_TARGET);

    const Type* from = value.type;

    if (typeConvertable(from, into)) {
        value.type = into;
        return moveValueToTarget(func, value, target);
    }

    value = moveValueToTarget(func, value, targetType(into));
    return moveValueToTarget(func, value, target);
}

static Value assignValue(Function* func, Value lhs, Value rhs, Target target) {
    rhs = moveValueToTarget(func, rhs, targetValue(lhs));
    return moveValueToTarget(func, rhs, target);
}

static Value assignDereference(
        Function* func,
        const Expression* pointerInto,
        const Expression* from,
        Target target
) {
    Value pvalue = compileExpression(func, pointerInto, ANY_TARGET);

    ASSERT_ERROR(isPointer(pvalue.type), func, exprLoc(from),
            "can only dereference pointer");

    const Type* derefType = pvalue.type->pointer;

    if (pvalue.kind == VALUE_IMMEDIATE) {
        pvalue.kind = VALUE_DIRECT;
        pvalue.type = derefType;
        return compileExpression(func, from, targetValue(pvalue));
    }

    Value fromValue = compileExpression(func, from, ANY_TARGET);
    ASSERT(typeCompatible(fromValue.type, derefType),
            "can only dereference into compatible type");

    int size = typeSize(derefType);

    load(func, REG_C, pvalue, 0);
    load(func, REG_D, pvalue, 1);

    for (int i = 0; i < size; ++i) {
        load(func, REG_A, fromValue, i);
        emitIndexedReg(func, INS_MOV, REG_A);
        if (i < size - 1) {
            emitReg(func, INS_INC, REG_C);
            emitReg(func, INS_INCC, REG_D);
        }
    }

    return moveValueToTarget(func, fromValue, target);
}

static Value assignExpression(
        Function* func,
        const Expression* into,
        const Expression* from,
        Target target
) {
    if (into->type == EXPR_UNARY && into->unary.operation == UNARY_DEREFERENCE) {
        return assignDereference(func, into->unary.inner, from, target);
    }
    Value lhs = compileExpression(func, into, ANY_TARGET);
    compileExpression(func, from, targetValue(lhs));
    return moveValueToTarget(func, lhs, target);
}


static Value performUncheckedBinary(
    Function* func,
    Value lhs,
    Value rhs,
    Opcode first,
    Opcode rest,
    BinaryOperation binop,
    Target target
) {
    if (isValueError(&lhs) || isValueError(&rhs)) {
        return valueError();
    }

    if (isImmediate(&lhs) && isImmediate(&rhs)) {
        const Expression* expr = exprBinary(
            func->lifetime, binop, lhs.expression, rhs.expression
        );
        Value value = valueImmediateExpr(lhs.type, expr);
        return moveValueToTarget(func, value, target);
    }

    Value tvalue = getValueTarget(func, target, lhs.type);

    ASSERT(typeSize(lhs.type) == typeSize(rhs.type) 
                && typeSize(tvalue.type) == typeSize(lhs.type),
            "all values must be the same size");


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

static Value performSimpleBinary(
    Function* func,
    Value lhs,
    Value rhs,
    Opcode first,
    Opcode rest,
    BinaryOperation binop,
    Target target
) {
    ASSERT_ERROR(isInteger(lhs.type), func, valueLoc(&lhs), "must be integer");
    ASSERT_ERROR(isInteger(rhs.type), func, valueLoc(&rhs), "must be integer");

    const Type* common = commonType(lhs.type, rhs.type, &target);

    lhs = moveValueToTarget(func, lhs, targetType(common));
    rhs = moveValueToTarget(func, rhs, targetType(common));

    return performUncheckedBinary(func, lhs, rhs, first, rest, binop, target);
}

static Value multiplyByImmediate(Function* func, Value lhs, int rhs, Target target);

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
    if (commutative && isPointer(rhs.type)) {
        Value swap = rhs;
        rhs = lhs;
        lhs = swap;
    }

    const Type* pointerType = NULL;
    if (isPointer(lhs.type)) {
        ASSERT_ERROR(isInteger(rhs.type), func, valueLoc(&rhs), "must be integer");

        rhs = multiplyByImmediate(func, rhs, typeSize(lhs.type->pointer), targetType(&typeUInt));
        pointerType = lhs.type;
        lhs.type = typeInteger(func->lifetime, SIGN_UNSIGNED, SIZE_INT);

        Value result = performUncheckedBinary(func, lhs, rhs, first, rest, binop, target);
        result.type = pointerType;
        return result;
    }

    Value result = performSimpleBinary(func, lhs, rhs, first, rest, binop, target);
    
    return result;
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

static Value leftShiftValue(
        Function* func,
        Value toShift,
        Value shiftBy,
        Target target,
        bool noOptimize
    );

static Value leftShiftByImmediate(
        Function* func,
        Value toShift,
        int shiftBy,
        Target target
) {
    if (shiftBy == 0) {
        return moveValueToTarget(func, toShift, target);
    }

    Value tvalue = getValueTarget(func, target, toShift.type);
    toShift = moveValueToTarget(func, toShift, targetType(tvalue.type));

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

    return moveValueToTarget(func, tvalue, target);
}

static Value leftShiftValue(
        Function* func,
        Value toShift,
        Value shiftBy,
        Target target,
        bool noOptimize
) {
    ASSERT_ERROR(isInteger(toShift.type), func, valueLoc(&toShift), "must be integer");
    ASSERT_ERROR(isInteger(shiftBy.type), func, valueLoc(&shiftBy), "must be integer");

    if (isImmediate(&toShift) && isImmediate(&shiftBy)) {
        const Expression* expr = exprBinary(
                func->lifetime,
                BINARY_SHIFT_LEFT,
                toShift.expression,
                shiftBy.expression
            );

        Value value = valueImmediateExpr(toShift.type, expr);
        return moveValueToTarget(func, value, target);
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
    ASSERT_ERROR(isInteger(lhs.type), func, valueLoc(&lhs), "must be integers");
    ASSERT_ERROR(isInteger(rhs.type), func, valueLoc(&rhs), "must be integers");

    const Type* common = commonType(lhs.type, rhs.type, &target);

    lhs = moveValueToTarget(func, lhs, targetType(common));
    rhs = moveValueToTarget(func, rhs, targetType(common));

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
            UNIMPLEMENTED();
            break;
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
}

static Value compileBinaryExpression(
        Function* func,
        BinaryOperation operation,
        const Expression* left,
        const Expression* right,
        Target target
) {
    Value lhs = compileExpression(func, left, ANY_TARGET);
    Value rhs = compileExpression(func, right, ANY_TARGET);

    return binaryExpression(func, operation, lhs, rhs, target);
}

static Value negateValue(Function* func, Value inner, Target target, Location location) {
    if (inner.kind == VALUE_IMMEDIATE) {
        const Expression* expr = exprUnary(func->lifetime, UNARY_NEGATE, inner.expression, location);
        Value value = valueImmediateExpr(inner.type, expr);
        return moveValueToTarget(func, value, target);
    }

    Value zero = valueConstant(func->lifetime, inner.type, 0, NO_LOCATION);
    return subtractValues(func, zero, inner, target);
}

static Value addressOf(Function* func, Value inner, Target target, Location location) {
    if (isValueError(&inner)) {
        return inner;
    }

    ASSERT_ERROR(inner.kind == VALUE_DIRECT, func, location,
            "cannot get the address of value");

    inner.kind = VALUE_IMMEDIATE;
    inner.type = typePointer(func->lifetime, inner.type);
    return moveValueToTarget(func, inner, target);
}

static Value dereference(Function* func, Value pvalue, Target target, Location location) {
    if (isValueError(&pvalue)) {
        return pvalue;
    }

    ASSERT_ERROR(isPointer(pvalue.type), func, location,
            "can only dereference pointer");

    const Type* derefType = pvalue.type->pointer;

    if (pvalue.kind == VALUE_IMMEDIATE) {
        pvalue.kind = VALUE_DIRECT;
        pvalue.type = derefType;
        return moveValueToTarget(func, pvalue, target);
    }

    // TODO: Should be able to deref into any implicitly castable type
    Value tvalue = getValueTarget(func, target, pvalue.type->pointer);
    ASSERT(typeCompatible(tvalue.type, derefType),
            "can only dereference into compatible type");

    int size = typeSize(tvalue.type);

    load(func, REG_C, pvalue, 0);
    load(func, REG_D, pvalue, 1);

    for (int i = 0; i < size; ++i) {
        emitRegIndexed(func, INS_MOV, REG_A);
        store(func, tvalue, i, REG_A);
        if (i < size - 1) {
            emitReg(func, INS_INC, REG_C);
            emitReg(func, INS_INCC, REG_D);
        }
    }

    return moveValueToTarget(func, tvalue, target);
}

static Value compileUnaryExpression(
        Function* func,
        UnaryOperation operation,
        const Expression* innerExpr,
        Target target,
        Location location
) {
    Value inner = compileExpression(func, innerExpr, ANY_TARGET);

    switch (operation) {
        case UNARY_NEGATE:
            return negateValue(func, inner, target, location);
        case UNARY_ADDRESSOF:
            return addressOf(func, inner, target, location);
        case UNARY_DEREFERENCE:
            return dereference(func, inner, target, location);
        case UNARY_NOT:
            UNIMPLEMENTED();
        
    }
}


static Value compileFunctionCall(
        Function* func,
        const Expression* nameExpr,
        size_t argumentCount,
        const Expression** arguments,
        Target target
) {
    ASSERT_ERROR(nameExpr->type == EXPR_VARIABLE, func, exprLoc(nameExpr),
            "function name must be an identifier");

    Identifier name = nameExpr->variable;

    const Declaration* decl = lookupGlobal(func->compiler, name);

    ASSERT_ERROR(decl != NULL, func, exprLoc(nameExpr), "function '%.*s' does not exist", name.length, name.start);
    ASSERT_ERROR(decl->kind == DECL_FUNCTION, func, exprLoc(nameExpr),
            "can only call a function");

    const FunctionDeclaration* funcDecl = &decl->function;

    ASSERT_ERROR(funcDecl->arity == argumentCount, func,
            exprLoc(nameExpr),
            "funcion declared with different arity");

    int offset =  0;
    if (!isVoid(funcDecl->returnType)) {
        offset = typeSize(funcDecl->returnType);
    }

    // TODO: Currently if a function call is used in as an argument to itself
    // it will be overwritten
    for (size_t i = 0; i < funcDecl->arity; ++i) {
        const Type* ty = funcDecl->parameters[i].type;
        Value argValue = valueStackOffset(func->lifetime, ty, &name, offset, NO_LOCATION);
        offset += typeSize(ty);
        compileExpression(func, arguments[i], targetValue(argValue));
    }

    emitValue(func, INS_CALL, valueImmediateExpr(typeAnyInteger(), nameExpr), 0);

    if (isVoid(funcDecl->returnType) || target.kind == TARGET_DISCARD) {
        return valueDiscard();
    }

    Value rvalue = getFuncReturnValue(func->lifetime, funcDecl, NO_LOCATION);
    Value tvalue = getValueTarget(func, target, funcDecl->returnType);

    return moveValueToTarget(func, rvalue, targetValue(tvalue));
}

static Value compileVariableExpression(Function* func, Identifier ident, Target target) {
    return moveValueToTarget(func, lookupSymbol(func, ident), target);
}

static Value compileLiteralExpression(Function* func, const Expression* expr, Target target) {
    Value value = valueImmediateExpr(typeAnyInteger(), expr);
    return moveValueToTarget(func, value, target);
}

Value compileExpression(Function* func, const Expression* expr, Target target) {
    switch (expr->type) {
        case EXPR_LABEL:
        case EXPR_STACKOFFSET:
        case EXPR_LITERAL:
            return compileLiteralExpression(func, expr, target);
        case EXPR_VARIABLE:
            return compileVariableExpression(func, expr->variable, target);
        case EXPR_BINARY:
            return compileBinaryExpression(func, expr->binary.operation,
                    expr->binary.left, expr->binary.right, target);
        case EXPR_UNARY:
            return compileUnaryExpression(func, expr->unary.operation,
                    expr->unary.inner, target, exprLoc(expr));
        case EXPR_ASSIGN:
            return assignExpression(func, expr->binary.left, expr->binary.right, target);
        case EXPR_CAST:
            return explicitCast(func, expr->cast.inner, expr->cast.type, target);
        case EXPR_CALL:
            return compileFunctionCall(func, expr->call.name,
                    expr->call.argumentCount, expr->call.arguments, target);
    }

    UNREACHABLE();
}
