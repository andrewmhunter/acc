#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include "type.h"
#include "expression.h"

typedef int Label;

typedef enum {
    VALUE_DISCARD,
    VALUE_IMMEDIATE,
    VALUE_DIRECT,
} ValueType;

typedef enum {
    CONTENTS_EXPRESSION,
    CONTENTS_LABEL,
    CONTENTS_STACK,
} ValueContents;

typedef struct {
    ValueType kind;
    ValueContents contents;
    Type* type;
    union {
        Expression* expression;
        Label label;
        size_t stackOffset;
    };
} Value;

typedef enum {
    TARGET_DISCARD,
    TARGET_VALUE,
    TARGET_ANY,
//    TARGET_CONDITION,
} TargetType;

typedef struct {
    TargetType kind;
    Value value;
} Target;

#define DISCARD_TARGET ((Target) {.kind = TARGET_DISCARD})
#define ANY_TARGET ((Target) {.kind = TARGET_ANY})

typedef enum {
    REG_A,
    REG_C,
    REG_D,
    REG_F,
} Reg;

typedef enum {
    ADDRESS_IMPLIED,
    ADDRESS_REGISTER,
    ADDRESS_VALUE,
    ADDRESS_INDEXED,
} AddressType;

typedef struct {
    AddressType kind;
    union {
        Reg reg;
        struct {
            Value value;
            int index;
        };
    };
} Address;

#define IMPLIED_ADDRESS ((Address){.kind = ADDRESS_IMPLIED})

typedef enum {
    INS_MOV,
    INS_ADD,
    INS_ADDC,
    INS_SUB,
    INS_SUBB,
    INS_OR,
    INS_AND,
    INS_JMP,
    INS_RET,
} Opcode;

typedef struct {
    Opcode opcode;
    Address dest;
    Address src;
} Instruction;


// Initialization

Address addressReg(Reg reg);
Address addressValue(Value value, int index);
Target targetValue(Value value);
Value valueImmediateExpr(Type* type, Expression* expr);
Value valueDirectExpr(Type* type, Expression* expr);
Value valueDirectStack(Type* type, size_t stackOffset);
Value valueDiscard();
Instruction instruction2(Opcode opcode, Address dest, Address src);
Instruction instruction1(Opcode opcode, Address address);
Instruction instruction0(Opcode opcode);

bool valueEquals(Value value0, Value value1);

// Printing

void printReg(FILE* file, Reg reg);
void printContents(FILE* file, Value value);
void printValue(FILE* file, Value value, int index);
void printAddress(FILE* file, Address* address);
void printOpcode(FILE* file, Opcode opcode);
void printInstruction(FILE* file, Instruction* ins);


#endif

