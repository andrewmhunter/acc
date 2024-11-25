#include <stdio.h>
#include "compiler.h"
#include "mem.h"
#include "util.h"
#include "diag.h"


/// ############################################################################
/// Initalizers
/// ############################################################################

Compiler compilerNew(Arena* staticLifetime) {
    return (Compiler) {
        .label = 0,
        .staticLifetime = staticLifetime,
    };
}

Function functionNew(Compiler* compiler) {
    return (Function) {
        .compiler = compiler,
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

static void appendInstruction(Function* function, Instruction instruction) {
    printInstruction(stdout, &instruction);
    APPEND_ARRAY(
        function->compiler->staticLifetime,
        function->instructions,
        Instruction,
        function->instructionsCapacity,
        function->instructionsLength,
        instruction
    );
}

static void emitImplied(Function* function, Opcode opcode) {
    appendInstruction(function, instruction0(opcode));
}

static void emitReg(Function* function, Opcode opcode, Reg reg) {
    appendInstruction(function, instruction1(opcode, addressReg(reg)));
}

static void emitValue(Function* function, Opcode opcode, Value value, int index) {
    if (value.kind == VALUE_DISCARD) {
        UNREACHABLE();
    }
    appendInstruction(function, instruction1(opcode, addressValue(value, index)));
}

static void emitRegReg(Function* function, Opcode opcode, Reg dest, Reg src) {
    appendInstruction(function, instruction2(opcode, addressReg(dest), addressReg(src)));
}

static void emitRegValue(Function* function, Opcode opcode, Reg dest, Value src, int index) {
    if (src.kind == VALUE_DISCARD) {
        UNREACHABLE();
    }
    appendInstruction(function, instruction2(opcode, addressReg(dest), addressValue(src, index)));
}

static void emitValueReg(Function* function, Opcode opcode, Value dest, int index, Reg src) {
    if (dest.kind == VALUE_DISCARD) {
        UNREACHABLE();
    }
    appendInstruction(function, instruction2(opcode, addressValue(dest, index), addressReg(src)));
}

static void load(Function* function, Reg reg, Value src, int index) {
    emitRegValue(function, INS_MOV, reg, src, index);
}

static void store(Function* function, Value dest, int index, Reg reg) {
    if (dest.kind == VALUE_DISCARD) {
        return;
    }
    emitValueReg(function, INS_MOV, dest, index, reg);
}

static Label createLabel(Compiler* compiler) {
    return compiler->label++;
}

static Value pushStack(Function* function, Type* type, Identifier ident) {
    function->stack = EXTEND_ARRAY(
        function->compiler->staticLifetime,
        function->stack,
        StackElement,
        &function->stackCapacity,
        function->stackLength,
        1
    );
    size_t offset = function->currentStackSize;

    StackElement* element = &function->stack[function->stackLength++];
    element->type = type;
    element->ident = ident;
    element->depth = function->scopeDepth;
    element->offset = offset;

    function->currentStackSize += typeSize(type);
    function->maxStackSize = MAX(function->maxStackSize, function->currentStackSize);

    return valueDirectStack(type, offset);
}

static Value pushStackAnon(Function* function, Type* type) {
    Identifier id = {.start = NULL, .length = 0};
    return pushStack(function, type, id);
}

static void popStack(Function* function) {
    StackElement* element = &function->stack[function->stackLength - 1];
    function->currentStackSize -= typeSize(element->type);
    --function->stackLength;

    ASSERT(function->stackLength >= 0 && function->currentStackSize >= 0, "stack empty");
}

static void enterScope(Function* function) {
    function->scopeDepth++;
}

static void leaveScope(Function* function) {
    ASSERT(function->scopeDepth > 0, "already at base scope");
    
    function->scopeDepth--;
    for (int i = function->stackLength - 1; i >= 0; --i) {
        StackElement* element = &function->stack[i];
        if (element->depth <= function->scopeDepth) {
            break;
        }

        popStack(function);
    }
}

static Value lookupSymbol(Function* function, Identifier ident) {
    for (int i = function->stackLength - 1; i >= 0; --i) {
        if (identEquals(ident, function->stack[i].ident)) {
            return valueDirectStack(function->stack[i].type, function->stack[i].offset);
        }
    }

    PANIC("failed to lookup symbol");
}


/// ############################################################################
/// Expressions
/// ############################################################################

static Value getValueTarget(Function* function, Target target, Type* type) {
    switch (target.kind) {
        case TARGET_VALUE:
            return target.value;
        case TARGET_ANY:
            return pushStackAnon(function, type);
        case TARGET_DISCARD:
            return valueDiscard();
    }
}

static Type* getTargetTypeOr(Target target, Type* type) {
    if (target.kind == TARGET_VALUE) {
        return target.value.type;
    }
    return type;
}

static Value compileExpression(Function* function, Expression* expr, Target target);


static Value implicitCast(Function* function, Value value, Type* type, Target target) {
    if (typeCompatible(value.type, type)) {
        value.type = type;
        return value;
    }
    UNIMPLEMENTED();
}

static Value moveValueToValue(Function* function, Value src, Value dest) {
    if (dest.type->tag == TYPE_VOID) {
        return dest;
    }

    if (valueEquals(src, dest)) {
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
        load(function, REG_A, src, i);
        store(function, dest, i, REG_A);
    }

    return dest;
}

static Value moveValueToTarget(Function* function, Value value, Target target) {
    Type* targetType = getTargetTypeOr(target, value.type);
    value = implicitCast(function, value, targetType, target);

    switch (target.kind) {
        case TARGET_ANY:
            return value;
        case TARGET_VALUE:
            return moveValueToValue(function, value, target.value);
        case TARGET_DISCARD:
            break;
    }
    return valueDiscard();
}

static Value assignValue(Function* function, Value lhs, Value rhs, Target target) {
    rhs = implicitCast(function, rhs, lhs.type, target);
    return moveValueToValue(function, rhs, lhs);
}

static Value assignExpression(Function* function, Expression* expr, Target target) {
    Value lhs = compileExpression(function, expr->binary.left, target);
    return compileExpression(function, expr->binary.right, targetValue(lhs));
}

static Value performSimpleBinary(
    Function* function,
    Value lhs,
    Value rhs,
    Opcode first,
    Opcode rest,
    Target target
) {
    Value tvalue = getValueTarget(function, target, lhs.type);

    load(function, REG_A, lhs, 0);
    emitRegValue(function, first, REG_A, rhs, 0);
    store(function, tvalue, 0, REG_A);

    int size = typeSize(lhs.type);
    for (int i = 1; i < size; ++i) {
        load(function, REG_A, lhs, i);
        emitRegValue(function, rest, REG_A, rhs, i);
        store(function, tvalue, i, REG_A);
    }

    return tvalue;
}

static Value addValues(Function* function, Value lhs, Value rhs, Target target) {
    /*if (isPointer(rhs.type)) {
        Value swap = rhs;
        rhs = lhs;
        lhs = swap;
    }
    if (isPointer(lhs.type)) {
        ASSERT(isInteger(rhs.type), "must be integer");

    }*/

    ASSERT(isInteger(lhs.type) && isInteger(rhs.type), "must be integer");

    Type* common = commonType(lhs.type, rhs.type);
    lhs = implicitCast(function, lhs, common, ANY_TARGET);
    rhs = implicitCast(function, rhs, common, ANY_TARGET);

    return performSimpleBinary(function, lhs, rhs, INS_ADD, INS_ADDC, target);
}

static Value binaryExpression(
    Function* function,
    BinaryOperation operation,
    Value lhs,
    Value rhs,
    Target target
) {
    switch (operation) {
        case BINARY_ADD:
            return performSimpleBinary(function, lhs, rhs, INS_ADD, INS_ADDC, target);
        case BINARY_SUBTRACT:
            return performSimpleBinary(function, lhs, rhs, INS_SUB, INS_SUBB, target);
        case BINARY_MULTIPLY: 
        case BINARY_DIVIDE:
            UNIMPLEMENTED();
            break;
        case BINARY_NONE:
            UNREACHABLE();
    }
}

static Value compileBinaryExpression(
        Function* function,
        Expression* expr,
        Target target
) {
    Value lhs = compileExpression(function, expr->binary.left, ANY_TARGET);
    Value rhs = compileExpression(function, expr->binary.right, ANY_TARGET);

    return binaryExpression(function, expr->binary.operation, lhs, rhs, target);
}

static Value compileUnaryExpression(Function* function, Expression* expr, Target target) {
    //Value inner = compileExpression(function, expr->unary.inner, ANY_TARGET);
    UNIMPLEMENTED();
}

static Value valueVariable(Function* function, Identifier ident) {
    return lookupSymbol(function, ident);
}

static Value compileExpression(Function* function, Expression* expr, Target target) {
    Value value;

    switch (expr->type) {
        case EXPR_LITERAL:
            value = valueImmediateExpr(typeInteger(
                    function->compiler->staticLifetime,
                    SIGN_EITHER,
                    SIZE_ANY
                ), expr);
            break;
        case EXPR_VARIABLE:
            value = valueVariable(function, expr->variable);
            break;
        case EXPR_BINARY:
            value = compileBinaryExpression(function, expr, target);
            break;
        case EXPR_UNARY:
            value = compileUnaryExpression(function, expr, target);
            break;
        case EXPR_ASSIGN:
            value = assignExpression(function, expr, target);
            break;
    }

    return moveValueToTarget(function, value, target);
}

/// ############################################################################
/// Statements
/// ############################################################################

static void compileStatement(Function* function, Statement* stmt);

static void compileBlock(Function* function, Statement* stmt) {
    enterScope(function);
    for (size_t i = 0; i < stmt->block.length; ++i) {
        compileStatement(function, stmt->block.data[i]);
    }
    leaveScope(function);
}

static void compileVariable(Function* function, Statement* stmt) {
    for (int i = function->stackLength - 1; i >= 0; --i) {
        if (function->stack[i].depth != function->scopeDepth) {
            break;
        }

        if (identEquals(stmt->variableDeclaration.id, function->stack[i].ident)) {
            PANIC("redefined variable");
        }
    }

    Value value = pushStack(function,
            stmt->variableDeclaration.type,
            stmt->variableDeclaration.id);

    if (stmt->variableDeclaration.expr) {
        compileExpression(function, stmt->variableDeclaration.expr, targetValue(value));
    }
}

static void compileIf(Function* function, Statement* stmt) {

}


static void compileStatement(Function* function, Statement* stmt) {
    fprintf(stdout, "; ");
    stmtPrint(stdout, stmt, 0);
    switch (stmt->type) {
        case STATEMENT_BLOCK:
            compileBlock(function, stmt);
            break;
        case STATEMENT_EXPRESSION:
            enterScope(function);
            compileExpression(function, stmt->expression, ANY_TARGET);
            leaveScope(function);
            break;
        case STATEMENT_VARIABLE:
            compileVariable(function, stmt);
            break;
        case STATEMENT_IF:
            compileIf(function, stmt);
            break;
    }
}

void compileFunction(Compiler* compiler, Statement* stmt) {
    Function function = functionNew(compiler);

    compileStatement(&function, stmt);
    emitImplied(&function, INS_RET);

    //for (int i = 0; i < function.instructionsLength; ++i) {
    //    printInstruction(stdout, &function.instructions[i]);
    //}
    fprintf(stdout, "variable _Stack %lu\n", function.maxStackSize);
}

