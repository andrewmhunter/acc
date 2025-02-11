#include "instruction.h"
#include "util.h"
#include "diag.h"
#include <assert.h>
//#include "match.h"

// Initalization

Address address(AddressType kind) {
    return (Address) {.kind = kind};
}

Address addressReg(Reg reg) {
    return (Address) {.kind = ADDRESS_REGISTER, .reg = reg};
}

static Value indexValue(Arena* arena, Value value, int index);

static Value indexMultipartValue(Arena* arena, Value value, int index) {
    // TODO: WARNING: This will not work quite right

    int offset = 0;
    for (int i = 0; i < value.partCount; ++i) {
        const ValuePart* part = &value.parts[i];
        if (index >= offset + part->width) {
            offset += part->width;
            continue;
        }

        return indexValue(arena, part->value, part->index + (index - offset));
    }

    PANIC("index into multipart out of range");
}

static Value indexValue(Arena* arena, Value value, int index) {
    if (index == 0 && value.kind != VALUE_MULTIPART) {
        return value;
    }
    const Expression* oldExpr = value.expression;
    switch (value.kind) {
        case VALUE_DIRECT:
            value.expression = exprBinary(arena, BINARY_ADD, oldExpr, exprLiteral(arena, index, NO_LOCATION));
            break;
        case VALUE_IMMEDIATE:
            value.expression = exprBinary(arena, BINARY_SHIFT_RIGHT, oldExpr, exprLiteral(arena, index * 8, NO_LOCATION));
            break;
        case VALUE_MULTIPART:
            value = indexMultipartValue(arena, value, index);
            break;
        case VALUE_ERROR:
        case VALUE_DISCARD:
            break;
    }
    return value;
}

Target targetValue(Value value) {
    return (Target) {.kind = TARGET_VALUE, .value = value};
}

Target targetType(const Type* type) {
    return (Target) {.kind = TARGET_TYPE, .type = type};
}

const Type* getTargetType(const Target* target) {
    if (target->kind == TARGET_VALUE) {
        return target->value.type;
    }
    if (target->kind == TARGET_TYPE) {
        return target->type;
    }
    return NULL;
}

const Type* getTargetTypeOr(const Target* target, const Type* type) {
    const Type* ttype = getTargetType(target);
    if (ttype != NULL) {
        return ttype;
    }
    return type;
}

const Type* commonType(const Type* t0, const Type* t1, const Target* target) {
    const Type* ttype = getTargetType(target);
    if (ttype != NULL) {
        return ttype;
    }
    return integerPromotion(t0, t1);
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

Opcode invertJump(Opcode opcode) {
    switch (opcode) {
        case INS_JC:
            return INS_JNC;
        case INS_JNC:
            return INS_JC;
        case INS_JZ:
            return INS_JNZ;
        case INS_JNZ:
            return INS_JZ;
        case INS_JN:
            return INS_JP;
        case INS_JP:
            return INS_JN;
        default:
            break;
    }
    PANIC("cannot invert instruction which is not a conditional jump");
}

Value valueImmediateExpr(const Type* type, const Expression* expr) {
    return (Value) {
        .kind = VALUE_IMMEDIATE,
        .type = type,
        .expression = expr
    };
}

Value valueConstant(Arena* arena, const Type* type, int literal, Location location) {
    return valueImmediateExpr(type, exprLiteral(arena, literal, location));
}

Value valueStackOffset(Arena* arena, const Type* type, const Identifier* functionName, int offset, Location location) {
    return valueDirectExpr(type, exprStackOffset(arena, functionName, offset, location));
}

Value valueDirectExpr(const Type* type, const Expression* expr) {
    return (Value) {
        .kind = VALUE_DIRECT,
        .type = type,
        .expression = expr
    };
}

Value valueDiscard() {
    return (Value) {
        .kind = VALUE_DISCARD,
        .type = &typeVoid
    };
}

Value valueError() {
    return (Value) {
        .kind = VALUE_ERROR,
        .type = &typeVoid
    };
}

Value valueZero(const Type* type) {
    static Expression zeroExpression = {.type = EXPR_LITERAL, .literal = 0};
    Value value = valueImmediateExpr(type, &zeroExpression);
    return value;
}

Value valueMultipart(Arena* arena, const Type* type, const ValueList* parts) {
    if (parts->length == 1) {
        return indexValue(arena, parts->values[0].value, parts->values[0].index);
    }
    return (Value) {
        .kind = VALUE_MULTIPART,
        .type = type,
        .partCount = parts->length,
        .parts = parts->values,
    };
}

bool isValueError(const Value* value) {
    return value->kind == VALUE_ERROR;
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

bool valueEqualsUntyped(const Value* value0, const Value* value1) {
    if (value0->kind != value1->kind) {
        return false;
    }
    return exprEquals(value0->expression, value1->expression);
}

bool valueEquals(const Value* value0, const Value* value1) {
    if (!typeCompatible(value0->type, value1->type)) {
        return false;
    }
    return valueEqualsUntyped(value0, value1);
}

Location valueLoc(const Value* value) {
    switch (value->kind) {
        case VALUE_IMMEDIATE:
        case VALUE_DIRECT:
            return exprLoc(value->expression);
        case VALUE_MULTIPART:
        {
            Location loc = valueLoc(&value->parts[0].value);
            for (int i = 1; i < value->partCount; ++i) {
                loc = locSpan(loc, valueLoc(&value->parts[i].value));
            }
            return loc;
        }
        case VALUE_ERROR:
        case VALUE_DISCARD:
            return NO_LOCATION;
    }
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
            break;
        case VALUE_DIRECT:
            fprintf(file, "[");
            exprPrint(file, value->expression);
            fprintf(file, "]");
            break;
        case VALUE_ERROR:
            fprintf(file, "ERROR");
            break;
        case VALUE_MULTIPART:
            fprintf(file, "<multipart");
            for (int i = 0; i < value->partCount; ++i) {
                fprintf(file, " ");
                printValue(file, &value->parts[i].value);
                fprintf(file, " %d-%d,",
                        value->parts[i].index,
                        value->parts[i].index + value->parts[i].width);
            }
            fprintf(file, ">");
            break;
        case VALUE_DISCARD:
            PANIC("cannot print discarded value");
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
        [INS_DELETED] = "; deleted :)",
        [INS_NOP]  = "nop",
        [INS_MOV]  = "mov",
        [INS_NOT]  = "not",
        [INS_ADD]  = "add",
        [INS_XOR]  = "xor",
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
        [INS_SHR]  = "shr",
        [INS_ROR]  = "ror",
        [INS_RET]  = "ret",
        [INS_CALL] = "call",
    };

    if ((unsigned)opcode > LENGTH_OF(arr) || arr[opcode] == NULL) {
        fprintf(file, "INVALID_INSTRUCTION");
    }

    fprintf(file, "%s", arr[opcode]);
}

static void printAllocation(FILE* file, const Identifier* ident, int offset, int size) {
    if (size > 1) {
        fprintf(file, ";  Alloc: %d -> %d: ", offset, offset + size - 1);
    } else {
        fprintf(file, ";  Alloc: %d: ", offset);
    }

    if (ident->start != NULL) {
        identPrint(file, ident);
    } else {
        fprintf(file, "?");
    }
    fprintf(file, "\n");
}

void printInstruction(FILE* file, const Instruction* ins, Diagnostics* diag) {
    switch (ins->opcode) {
        case INS_LABEL:
            printAddress(file, &ins->dest, &ins->value);
            fprintf(file, ":\n");
            return;
        case INS_COMMENT:
            fprintf(file, "; %s\n", ins->comment.string);
            return;
        case INS_COMMENT_STATEMENT:
            fprintf(file, "; ");
            stmtPrint(file, ins->comment.statement, 0, true);
            return;
        case INS_COMMENT_LOCATION:
            fprintf(file, "; ");
            if (diag != NULL) {
                printLocation(file, diag, ins->comment.location);
            } else {
                fprintf(file, "unknown location");
            }
            fprintf(file, "\n");
            return;
        case INS_COMMENT_ALLOCATION:
        {
            printAllocation(
                file,
                &ins->comment.allocation.ident,
                ins->comment.allocation.offset,
                ins->comment.allocation.size
            );
            return;
        }
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

    //fprintf(file, " %c%c", insReadsMemory(ins) ? 'r' : '-', insWritesMemory(ins) ? 'w' : '-');

    fprintf(file, "\n");
}

ValueList valueList(Arena* arena) {
    const size_t defaultCapacity = 2;

    return (ValueList) {
        .arena = arena,
        .length = 0,
        .capacity = defaultCapacity,
        .values = ARENA_ALLOC_ARRAY(arena, ValuePart, defaultCapacity),
    };
}

void valueListPush(ValueList* list, Value value, int width, int index) {
    list->values = EXTEND_ARRAY(list->arena, list->values, ValuePart, &list->capacity, list->length, 1);
    list->values[list->length++] = (ValuePart) {
        .width = width,
        .index = index,
        .value = value,
    };
}

