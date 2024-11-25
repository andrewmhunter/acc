#include "instruction.h"
#include "util.h"
#include <assert.h>

// Initalization

Address addressReg(Reg reg) {
    return (Address) {.kind = ADDRESS_REGISTER, .reg = reg};
}

Address addressValue(Value value, int index) {
    return (Address) {.kind = ADDRESS_VALUE, .value = value, .index = index};
}

Target targetValue(Value value) {
    return (Target) {.kind = TARGET_VALUE, .value = value};
}

Value valueImmediateExpr(Type* type, Expression* expr) {
    return (Value) {
        .kind = VALUE_IMMEDIATE,
        .type = type,
        .contents = CONTENTS_EXPRESSION,
        .expression = expr
    };
}

Value valueDirectExpr(Type* type, Expression* expr) {
    return (Value) {
        .kind = VALUE_DIRECT,
        .type = type,
        .contents = CONTENTS_EXPRESSION,
        .expression = expr
    };
}

Value valueDirectStack(Type* type, size_t stackOffset) {
    return (Value) {
        .kind = VALUE_DIRECT,
        .type = type,
        .contents = CONTENTS_STACK,
        .stackOffset = stackOffset
    };
}

Value valueDiscard() {
    return (Value) {
        .kind = VALUE_DISCARD,
        .contents = CONTENTS_STACK,
        .type = typeVoid()
    };
}

bool valueEquals(Value value0, Value value1) {
    if (value0.kind != value1.kind
        || !typeCompatible(value0.type, value1.type)
        || value0.contents != value1.contents
    ) {
        return false;
    }

    switch (value0.contents) {
        case CONTENTS_STACK:
            return value0.stackOffset == value1.stackOffset;
        case CONTENTS_LABEL:
            return value0.label == value1.label;
        case CONTENTS_EXPRESSION:
            return exprEquals(value0.expression, value1.expression);
    }
}

Instruction instruction2(Opcode opcode, Address dest, Address src) {
    return (Instruction) {
        .opcode = opcode,
        .dest = dest,
        .src = src,
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

void printContents(FILE* file, Value value) {
    switch (value.contents) {
        case CONTENTS_EXPRESSION:
            exprPrint(file, value.expression);
            break;
        case CONTENTS_LABEL:
            fprintf(file, "_Label_%d", value.label);
            break;
        case CONTENTS_STACK:
            fprintf(file, "(_Stack + %lu)", value.stackOffset);
            break;
    }
}

void printValue(FILE* file, Value value, int index) {
    switch (value.kind) {
        case VALUE_IMMEDIATE:
            printContents(file, value);
            if (index > 0) {
                fprintf(file, " >> %d", index * 8);
            }
            break;
        case VALUE_DIRECT:
            fprintf(file, "[");
            printContents(file, value);
            if (index > 0) {
                fprintf(file, " + %d", index);
            }
            fprintf(file, "]");
            break;
        default:
            // TODO: Error
            break;
    }
}

void printAddress(FILE* file, Address* address) {
    switch (address->kind) {
        case ADDRESS_REGISTER:
            printReg(file, address->reg);
            break;
        case ADDRESS_VALUE:
            printValue(file, address->value, address->index);
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
        [INS_MOV] = "mov",
        [INS_ADD] = "add",
        [INS_ADDC] = "addc",
        [INS_SUB] = "sub",
        [INS_SUBB] = "subb",
        [INS_JMP] = "jmp",
        [INS_RET] = "ret",
    };

    if (opcode > LENGTH_OF(arr) || arr[opcode] == NULL) {
        fprintf(file, "INVALID_INSTRUCTION");
    }

    fprintf(file, "%s", arr[opcode]);
}

void printInstruction(FILE* file, Instruction* ins) {
    fprintf(file, "    ");
    printOpcode(file, ins->opcode);
    if (ins->dest.kind != ADDRESS_IMPLIED) {
        fprintf(file, " ");
        printAddress(file, &ins->dest);
    }
    if (ins->src.kind != ADDRESS_IMPLIED) {
        fprintf(file, ", ");
        printAddress(file, &ins->src);
    }
    fprintf(file, "\n");
}

