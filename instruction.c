#include "instruction.h"
#include "util.h"
#include <assert.h>

// Initalization

Address addressReg(Reg reg) {
    return (Address) {.kind = ADDRESS_REGISTER, .reg = reg};
}

static Value indexValue(Arena* arena, Value value, int index) {
    if (index == 0) {
        return value;
    }
    const Expression* oldExpr = value.expression;
    switch (value.kind) {
        case VALUE_DIRECT:
            value.expression = exprBinary(arena, BINARY_ADD, oldExpr, exprLiteral(arena, index));
            break;
        case VALUE_IMMEDIATE:
            value.expression = exprBinary(arena, BINARY_SHIFT_RIGHT, oldExpr, exprLiteral(arena, index * 8));
            break;
        default:
            break;
    }
    return value;
}

Target targetValue(Value value) {
    return (Target) {.kind = TARGET_VALUE, .value = value};
}

ConditionTarget conditionTarget(Value label, Invert invert) {
    return (ConditionTarget) {
        .invert = invert,
        .putInA = false,
        .jumpToLabel = true,
        .label = label
    };
}

ConditionTarget invertConditionTarget(ConditionTarget target) {
    target.invert = !target.invert;
    return target;
}

Value valueImmediateExpr(const Type* type, const Expression* expr) {
    return (Value) {
        .kind = VALUE_IMMEDIATE,
        .type = type,
        .expression = expr
    };
}

Value valueConstant(Arena* arena, const Type* type, int literal) {
    return valueImmediateExpr(type, exprLiteral(arena, literal));
}

Value valueStackOffset(Arena* arena, const Type* type, int offset) {
    return valueDirectExpr(type, exprStackOffset(arena, offset));
}

Value valueDirectExpr(const Type* type, const Expression* expr) {
    return (Value) {
        .kind = VALUE_DIRECT,
        .type = type,
        .expression = expr
    };
}

Value valueDirectStack(const Type* type, size_t stackOffset) {
    return (Value) {
        .kind = VALUE_DIRECT,
        .type = type,
        .stackOffset = stackOffset
    };
}

Value valueDiscard() {
    return (Value) {
        .kind = VALUE_DISCARD,
        .type = typeVoid()
    };
}

Value valueZero(const Type* type) {
    static Expression zeroExpression = {.type = EXPR_LITERAL, .literal = 0};
    Value value = valueImmediateExpr(type, &zeroExpression);
    return value;
}

bool isImmediate(const Value* value) {
    return value->kind == VALUE_IMMEDIATE;
}

bool isDirect(const Value* value) {
    return value->kind == VALUE_DIRECT;
}

bool immediateResolved(const Value* value, int* output) {
    if (!isImmediate(value) || value->expression->type != EXPR_LITERAL) {
        return false;
    }

    if (output != NULL) {
        *output = value->expression->literal;
    }
    return true;
}

bool valueEquals(const Value* value0, const Value* value1) {
    if (value0->kind != value1->kind
        || !typeCompatible(value0->type, value1->type)
    ) {
        return false;
    }
    return exprEquals(value0->expression, value1->expression);
}

Instruction instruction2Value(
        Arena* arena,
        Opcode opcode,
        Address dest,
        Address src,
        Value value,
        int index
) {
    return (Instruction) {
        .opcode = opcode,
        .dest = dest,
        .src = src,
        .value = indexValue(arena, value, index),
    };
}

Instruction instruction1Value(
        Arena* arena,
        Opcode opcode,
        Address address,
        Value value,
        int index
) {
    return instruction2Value(arena, opcode, address, IMPLIED_ADDRESS, value, index);
}

Instruction instruction2(Opcode opcode, Address dest, Address src) {
    return (Instruction) {
        .opcode = opcode,
        .dest = dest,
        .src = src,
        .value = (Value) {
            .kind = VALUE_DISCARD,
            .type = NULL
        },
    };
}


Instruction instruction1(Opcode opcode, Address address) {
    return instruction2(opcode, address, IMPLIED_ADDRESS);
}

Instruction instruction0(Opcode opcode) {
    return instruction2(opcode, IMPLIED_ADDRESS, IMPLIED_ADDRESS);
}

// Printing

void printReg(FILE* file, Reg reg) {
    static const char* arr[] = {
        [REG_A] = "a",
        [REG_C] = "c",
        [REG_D] = "d",
        [REG_F] = "f",
    };

    fprintf(file, "%s", arr[reg]);
}

void printValue(FILE* file, const Value* value) {
    switch (value->kind) {
        case VALUE_IMMEDIATE:
            exprPrint(file, value->expression);
            /*if (index > 0) {
                fprintf(file, " >> %d", index * 8);
            }*/
            break;
        case VALUE_DIRECT:
            fprintf(file, "[");
            exprPrint(file, value->expression);
            /*if (index > 0) {
                fprintf(file, " + %d", index);
            }*/
            fprintf(file, "]");
            break;
        default:
            // TODO: Error
            break;
    }
}

void printAddress(FILE* file, const Address* address, const Value* value) {
    switch (address->kind) {
        case ADDRESS_REGISTER:
            printReg(file, address->reg);
            break;
        case ADDRESS_VALUE:
            printValue(file, value);
            break;
        case ADDRESS_INDEXED:
            fprintf(file, "[cd]");
            break;
        case ADDRESS_IMPLIED:
            assert(false && "can not print implied address");
            break;
    }
}

void printOpcode(FILE* file, Opcode opcode) {
    static const char* arr[] = {
        [INS_MOV]  = "mov",
        [INS_NOT]  = "not",
        [INS_ADD]  = "add",
        [INS_ADDC] = "addc",
        [INS_SUB]  = "sub",
        [INS_SUBB] = "subb",
        [INS_JMP]  = "jmp",
        [INS_JZ]   = "jz",
        [INS_JNZ]  = "jnz",
        [INS_JC]   = "jc",
        [INS_JNC]  = "jnc",
        [INS_JN]   = "jn",
        [INS_JP]   = "jp",
        [INS_OR]   = "or",
        [INS_AND]  = "and",
        [INS_INC]  = "inc",
        [INS_INCC] = "incc",
        [INS_DEC]  = "dec",
        [INS_DECB] = "decb",
        [INS_SHL]  = "shl",
        [INS_ROL]  = "rol",
        [INS_RET]  = "ret",
    };

    if ((unsigned)opcode > LENGTH_OF(arr) || arr[opcode] == NULL) {
        fprintf(file, "INVALID_INSTRUCTION");
    }

    fprintf(file, "%s", arr[opcode]);
}

void printInstruction(FILE* file, const Instruction* ins) {
    switch (ins->opcode) {
        case INS_LABEL:
            printAddress(file, &ins->dest, &ins->value);
            fprintf(file, ":\n");
            return;
        default:
            break;
    }

    fprintf(file, "    ");
    printOpcode(file, ins->opcode);
    if (ins->dest.kind != ADDRESS_IMPLIED) {
        fprintf(file, " ");
        printAddress(file, &ins->dest, &ins->value);
    }
    if (ins->src.kind != ADDRESS_IMPLIED) {
        fprintf(file, ", ");
        printAddress(file, &ins->src, &ins->value);
    }
    fprintf(file, "\n");
}

