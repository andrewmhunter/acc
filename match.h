#ifndef MATCH_H
#define MATCH_H

#include "instruction.h"
#include <stdbool.h>

struct Optimizer;

typedef enum {
    MATCH_AND,
    MATCH_OR,
    MATCH_ANY,
    MATCH_ENTER,
    MATCH_EXIT,
    MATCH_PARTIAL_EXIT,
    MATCH_WRITES_REG,
    MATCH_READS_REG,
    MATCH_USES_REG,
    MATCH_READS_VALUE,
    MATCH_WRITES_VALUE,
    MATCH_USES_VALUE,
    MATCH_INSTRUCTION,
    MATCH_ANY_JUMP,
    MATCH_CONDITIONAL_JUMP,
    MATCH_NORMAL_JUMP,
    MATCH_LABEL,
} MatchKind;

typedef enum {
    MATCHFLAG_INVERT = 0b1,
    MATCHFLAG_ALLOW_EXIT = 0b10,
    MATCHFLAG_ALLOW_ENTER = 0b100,
} MatchFlags;

typedef struct {
    Opcode opcode;
    AddressType dest;
    AddressType src;
} InstructionPrototype;

#define PROTOTYPE_2(OPCODE, DEST, SRC) \
    ((InstructionPrototype) {.opcode = (OPCODE), .dest = (DEST), .src = (SRC)})

#define PROTOTYPE_1(OPCODE, DEST) \
    PROTOTYPE_2(OPCODE, DEST, ADDRESS_IMPLIED)

#define PROTOTYPE_0(OPCODE) \
    PROTOTYPE_1(OPCODE, ADDRESS_IMPLIED)

typedef struct Match {
    MatchKind kind;
    MatchFlags flags;
    union {
        struct {
            struct Match* left;
            struct Match* right;
        } binary;
        struct {
            const InstructionPrototype* prototype;
            union {
                Reg* reg;
                const Value** value;
            } dest;
            union {
                Reg* reg;
                const Value** value;
            } src;
        } instruction;
        const Value* value;
        const Value** valuePtr;
        Reg reg;
    };
} Match;

typedef struct {
    Instruction* start;
    Instruction* end;
} InsSpan;

Match matchNot(Match inner);
Match matchAllowExit(Match inner);
Match matchAllowEnter(Match inner);
Match matchAllowFlow(Match inner);

Match matchAny();
Match matchExit();
Match matchPartialExit();
Match matchEnter();

Match matchAnd(Match* inner0, Match* inner1);
Match matchOr(Match* inner0, Match* inner1);

Match matchReads(Reg reg);
Match matchWrites(Reg reg);
Match matchUses(Reg reg);

Match matchReadsValue(const Value* value);
Match matchWritesValue(const Value* value);
Match matchUsesValue(const Value* value);

Match matchInstruction(const InstructionPrototype* prototype);
Match matchInstructionReg(const InstructionPrototype* prototype, Reg* dest);
Match matchInstructionValue(const InstructionPrototype* prototype, const Value** dest);
Match matchInstructionRegReg(const InstructionPrototype* prototype, Reg* dest, Reg* src);
Match matchInstructionValueReg(const InstructionPrototype* prototype, const Value** dest, Reg* src);
Match matchInstructionRegValue(const InstructionPrototype* prototype, Reg* dest, const Value** src);
Match matchLoad(Reg* regOut, const Value** valueOut);
Match matchStore(const Value** valueOut, Reg* regOut);

Match matchAnyJump(const Value** label);
Match matchConditionalJump(const Value** label);
Match matchNormalJump(const Value** label);
Match matchLabel(const Value** label);

bool instructionMatches(
        struct Optimizer* optim,
        const Instruction* instruction,
        Match* matcher
    );


struct Optimizer;

bool matchPattern(struct Optimizer* optim, Instruction** instruction, Match matcher);
int matchAllPattern(struct Optimizer* optim, InsSpan* span, Match matcher);

bool insReadsMemory(const Instruction* ins);
bool insWritesMemory(const Instruction* ins);
bool opcodeSkip(Opcode opcode);

#endif

