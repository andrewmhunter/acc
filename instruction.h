#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include "type.h"
#include "expression.h"
#include "condition.h"

typedef enum {
    VALUE_ERROR,
    VALUE_DISCARD,
    VALUE_IMMEDIATE,
    VALUE_DIRECT,
} ValueType;

typedef struct Value {
    ValueType kind;
    const Type* type;
    const Expression* expression;
} Value;

typedef enum {
    TARGET_DISCARD,
    TARGET_VALUE,
    TARGET_ANY,
    TARGET_TYPE,
} TargetType;

typedef struct {
    TargetType kind;
    union {
        Value value;
        const Type* type;
    };
} Target;

#define DISCARD_TARGET ((Target) {.kind = TARGET_DISCARD})
#define ANY_TARGET ((Target) {.kind = TARGET_ANY})

typedef struct {
    Invert invert;
    bool putInA;
    bool jumpToLabel;
    Value label;
} ConditionTarget;

typedef enum: char {
    REG_A,
    REG_C,
    REG_D,
    REG_F,
} Reg;

typedef enum: char {
    ADDRESS_IMPLIED,
    ADDRESS_REGISTER,
    ADDRESS_VALUE,
    ADDRESS_INDEXED,
} AddressType;

typedef struct {
    AddressType kind;
    Reg reg;
} Address;

#define IMPLIED_ADDRESS ((Address){.kind = ADDRESS_IMPLIED})

typedef enum: char {
    INS_LABEL,
    INS_MOV,
    INS_NOT,
    INS_ADD,
    INS_ADDC,
    INS_SUB,
    INS_SUBB,
    INS_OR,
    INS_AND,
    INS_JMP,
    INS_JZ,
    INS_JNZ,
    INS_JC,
    INS_JNC,
    INS_JN,
    INS_JP,
    INS_INC,
    INS_INCC,
    INS_DEC,
    INS_DECB,
    INS_SHL,
    INS_ROL,
    INS_RET,
    INS_OUT,
    INS_CALL,
} Opcode;

typedef struct {
    Opcode opcode;
    Address dest;
    Address src;
    Value value;
} Instruction;

// Initialization

#define GET_ADDRESS_VALUE() ((Address) {.kind = ADDRESS_VALUE, .reg = REG_D})

Address addressReg(Reg reg);

Target targetValue(Value value);
Target targetType(const Type* type);
const Type* getTargetTypeOr(const Target* target, const Type* type);
const Type* commonType(const Type* t0, const Type* t1, const Target* target);

ConditionTarget conditionTarget(Value label, Invert invert);
ConditionTarget invertConditionTarget(ConditionTarget target);

Value valueImmediateExpr(const Type* type, const Expression* expr);
Value valueConstant(Arena* arena, const Type* type, int literal, Location location);
Value valueStackOffset(Arena* arena, const Type* type, const Identifier* functionName, int offset, Location location);
Value valueDirectExpr(const Type* type, const Expression* expr);
Value valueDiscard();
Value valueError();
Value valueZero(const Type* type);

bool isValueError(const Value* value);
bool isImmediate(const Value* value);
bool isDirect(const Value* value);
bool immediateResolved(const Value* value, int* output);

bool valueEquals(const Value* value0, const Value* value1);

Location valueLoc(const Value* value);

Instruction instruction2Value(
        Arena* arena,
        Opcode opcode,
        Address dest,
        Address src,
        Value value,
        int index
    );

Instruction instruction1Value(
        Arena* arena,
        Opcode opcode,
        Address address,
        Value value,
        int index
    );

Instruction instruction2(Opcode opcode, Address dest, Address src);
Instruction instruction1(Opcode opcode, Address address);
Instruction instruction0(Opcode opcode);

// Printing

void printReg(FILE* file, Reg reg);
void printValue(FILE* file, const Value* value);
void printAddress(FILE* file, const Address* address, const Value* value);
void printOpcode(FILE* file, Opcode opcode);
void printInstruction(FILE* file, const Instruction* ins);


#endif

