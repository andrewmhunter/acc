#include "match.h"
#include "instruction.h"
#include "optimizer.h"
#include <stdbool.h>

static bool opcodeConditionalJump(Opcode opcode) {
    switch (opcode) {
        case INS_JC:
        case INS_JZ:
        case INS_JP:
        case INS_JNC:
        case INS_JNZ:
        case INS_JN:
            return true;
        default:
            break;
    }
    return false;
}

static bool opcodeAnyJump(Opcode opcode) {
    if (opcode == INS_JMP) {
        return true;
    }

    return opcodeConditionalJump(opcode);
}

static bool instructionMatchSingleValue(const Instruction* ins, const Value** value) {
    if (ins->dest.kind != ADDRESS_VALUE) {
        return false;
    }

    if (value == NULL) {
        return true;
    }

    if (*value == NULL) {
        *value = &ins->value;
        return true;
    }

    return valueEqualsUntyped(*value, &ins->value);
}

static bool insAnyJump(const Instruction* ins, const Value** label) {
    return opcodeAnyJump(ins->opcode)
        && instructionMatchSingleValue(ins, label);
}

static bool insLabel(const Instruction* ins, const Value** label) {
    return ins->opcode == INS_LABEL
        && instructionMatchSingleValue(ins, label);
}

static bool insConditionalJump(const Instruction* ins, const Value** label) {
    return opcodeConditionalJump(ins->opcode)
        && instructionMatchSingleValue(ins, label);
}

static bool insNormalJump(const Instruction* ins, const Value** label) {
    return ins->opcode == INS_JMP
        && instructionMatchSingleValue(ins, label);
}

bool opcodePartialExit(Opcode opcode) {
    switch (opcode) {
        case INS_JMP:
        case INS_CALL:
        case INS_RET:
            return true;
        default:
            break;
    }
    return opcodeConditionalJump(opcode);
}

bool opcodeExit(Opcode opcode) {
    switch (opcode) {
        case INS_JMP:
        case INS_RET:
            return true;
        default:
            break;
    }
    return false;
}

bool opcodeEnter(Opcode opcode) {
    switch (opcode) {
        case INS_LABEL:
        case INS_CALL:
            return true;
        default:
            break;
    }
    return false;
}

bool opcodeFlow(Opcode opcode) {
    return opcodeEnter(opcode) || opcodeExit(opcode);
}

static bool opcodeAluBinaryFlagsRead(Opcode opcode) {
    switch (opcode) {
        case INS_ADDC:
        case INS_SUBB:
            return true;
        default:
            break;
    }
    return false;
}

static bool opcodeAluBinary(Opcode opcode) {
    switch (opcode) {
        case INS_ADD:
        case INS_SUB:
        case INS_AND:
        case INS_OR:
        case INS_XOR:
            return true;
        default:
            break;
    }
    return opcodeAluBinaryFlagsRead(opcode);
}

static bool opcodeAluUnary(Opcode opcode) {
    switch (opcode) {
        case INS_INC:
        case INS_INCC:
        case INS_DEC:
        case INS_DECB:
        case INS_SHL:
        case INS_ROL:
        case INS_NOT:
            return true;
        default:
            break;
    }
    return false;
}

static bool opcodeWrites(Opcode opcode) {
    switch (opcode) {
        case INS_MOV:
            return true;
        default:
            break;
    }
    return opcodeAluBinary(opcode) || opcodeAluUnary(opcode);
}

bool insWrites(const Instruction* ins, Reg reg) {
    if (reg == REG_F
        && (opcodeAluBinary(ins->opcode)
        || opcodeAluUnary(ins->opcode))
    ) {
        return true;
    }

    return opcodeWrites(ins->opcode)
        && ins->dest.kind == ADDRESS_REGISTER
        && ins->dest.reg == reg;
}

static bool addressIsMemory(const Address* address, const Value* value) {
    return (address->kind == ADDRESS_VALUE && value->kind != VALUE_IMMEDIATE)
        || address->kind == ADDRESS_INDEXED;
}

bool insWritesMemory(const Instruction* ins) {
    return addressIsMemory(&ins->dest, &ins->value) && opcodeWrites(ins->opcode);
}

static bool opcodeReadsDest(Opcode opcode) {
    switch (opcode) {
        case INS_JMP:
        case INS_CALL:
            return true;
        default:
            break;
    }
    return opcodeAluBinary(opcode)
        || opcodeAluUnary(opcode)
        || opcodeConditionalJump(opcode);
}

static bool opcodeReadsSrc(Opcode opcode) {
    switch (opcode) {
        case INS_MOV:
            return true;
        default:
            break;
    }
    return opcodeAluBinary(opcode);
}

bool insReadsMemory(const Instruction* ins) {
    return (opcodeReadsDest(ins->opcode) && addressIsMemory(&ins->dest, &ins->value))
        || (opcodeReadsSrc(ins->opcode) && addressIsMemory(&ins->src, &ins->value));
}

bool insReads(const Instruction* ins, Reg reg) {
    if (reg == REG_F
        && (opcodeAluBinary(ins->opcode)
        || opcodeAluUnary(ins->opcode)
        || opcodeConditionalJump(ins->opcode))
    ) {
        return true;
    }

    if ((ins->dest.kind == ADDRESS_INDEXED
        || ins->src.kind == ADDRESS_INDEXED)
        && (reg == REG_C
        || reg == REG_D)
    ) {
        return true;
    }

    if (ins->dest.kind == ADDRESS_REGISTER
        && ins->dest.reg == reg
        && opcodeReadsDest(ins->opcode)
    ) {
        return true;
    }

    if (ins->src.kind == ADDRESS_REGISTER
        && ins->src.reg == reg
        && opcodeReadsSrc(ins->opcode)
    ) {
        return true;
    }

    return false;
}

bool insReadsValue(const Instruction* ins, const Value* value) {
    if (value->kind != VALUE_IMMEDIATE) {
        return insReadsMemory(ins);
    }

    return false;

    /*if (opcodeReadsDest(ins->opcode)
        && (ins->dest.kind == ADDRESS_VALUE
        && valueEqualsUntyped(&ins->value, value))
    ) {
        return true;
    }

    return opcodeReadsSrc(ins->opcode)
        && ins->src.kind == ADDRESS_VALUE
        && valueEqualsUntyped(&ins->value, value);*/
}

bool insWritesValue(const Instruction* ins, const Value* value) {
    // Since the same byte of memory could be accessed by any number of unique
    // Values not to mention indexing which could access any byte of memory
    // we have to assume that any memory write writes to every non IMMEDIATE Value.
    
    if (value->kind != VALUE_IMMEDIATE) {
        return insWritesMemory(ins);
    }

    return false;

    /*return opcodeWrites(ins->opcode)
        && ins->dest.kind == ADDRESS_VALUE
        && valueEqualsUntyped(&ins->value, value);*/
}

bool insMatchRegValue(
    const Instruction* ins,
    Opcode expectedOpcode,
    const Address* regAddress,
    const Address* valueAddress,
    Reg* regOut,
    const Value** valueOut
) {
    if (ins->opcode != expectedOpcode
        || regAddress->kind != ADDRESS_REGISTER
        || valueAddress->kind != ADDRESS_VALUE
    ) {
        return false;
    }

    if (regOut != NULL
        && *regOut != REG_INVALID
        && regAddress->reg != *regOut
    ) {
        return false;
    }

    if (valueOut != NULL
        && *valueOut != NULL
        && !valueEqualsUntyped(&ins->value, *valueOut)
    ) {
        return false;
    }

    if (regOut != NULL
        && *regOut == REG_INVALID
    ) {
        *regOut = regAddress->reg;
    }

    if (valueOut != NULL
        && *valueOut == NULL
    ) {
        *valueOut = &ins->value;
    }

    return true;
}

static bool addressMatchAddress(Address addr0, AddressType addr1, const Value* insValue, Reg* regOut, const Value** valueOut) {
    if (addr0.kind != addr1) {
        return false;
    }

    if (addr1 == ADDRESS_REGISTER) {
        if (regOut == NULL) {
            return true;
        }

        if (*regOut == REG_INVALID) {
            *regOut = addr0.reg;
            return true;
        }

        return addr0.reg == *regOut;
    }

    if (addr1 != ADDRESS_VALUE
        || valueOut == NULL
    ) {
        return true;
    }

    if (*valueOut == NULL) {
        *valueOut = insValue;
        return true;
    }

    bool equals = valueEqualsUntyped(insValue, *valueOut);
    return equals;
}

static bool insMatchInstruction(
    const Instruction* ins,
    Match* matcher
) {
    const InstructionPrototype* prototype = matcher->instruction.prototype;
    return ins->opcode == prototype->opcode
        && addressMatchAddress(ins->dest, prototype->dest, &ins->value,
                matcher->instruction.dest.reg, matcher->instruction.dest.value)
        && addressMatchAddress(ins->src, prototype->src, &ins->value,
                matcher->instruction.src.reg, matcher->instruction.src.value);
}


Match matchNot(Match inner) {
    inner.flags ^= MATCHFLAG_INVERT;
    return inner;
}

Match matchAllowExit(Match inner) {
    inner.flags |= MATCHFLAG_ALLOW_EXIT;
    return inner;
}

Match matchAllowEnter(Match inner) {
    inner.flags |= MATCHFLAG_ALLOW_ENTER;
    return inner;
}

Match matchAllowFlow(Match inner) {
    inner.flags |= MATCHFLAG_ALLOW_EXIT | MATCHFLAG_ALLOW_ENTER;
    return inner;
}

Match matchAny() {
    return (Match) {
        .kind = MATCH_ANY,
        .flags = 0,
    };
}

Match matchExit() {
    return (Match) {
        .kind = MATCH_EXIT,
        .flags = MATCHFLAG_ALLOW_EXIT,
    };
}

Match matchPartialExit() {
    return (Match) {
        .kind = MATCH_PARTIAL_EXIT,
        .flags = MATCHFLAG_ALLOW_EXIT,
    };
}

Match matchEnter() {
    return (Match) {
        .kind = MATCH_ENTER,
        .flags = MATCHFLAG_ALLOW_ENTER,
    };
}

Match matchAnd(Match* inner0, Match* inner1) {
    return (Match) {
        .kind = MATCH_AND,
        .flags = 0,
        .binary = {
            .left = inner0,
            .right = inner1
        },
    };
};

Match matchOr(Match* inner0, Match* inner1) {
    return (Match) {
        .kind = MATCH_OR,
        .flags = 0,
        .binary = {
            .left = inner0,
            .right = inner1
        },
    };
}

Match matchReads(Reg reg) {
    return (Match) {
        .kind = MATCH_READS_REG,
        .flags = 0,
        .reg = reg,
    };
}

Match matchWrites(Reg reg) {
    return (Match) {
        .kind = MATCH_WRITES_REG,
        .flags = 0,
        .reg = reg,
    };
}

Match matchUses(Reg reg) {
    return (Match) {
        .kind = MATCH_USES_REG,
        .flags = 0,
        .reg = reg,
    };
}


Match matchReadsValue(const Value* value) {
    return (Match) {
        .kind = MATCH_READS_VALUE,
        .flags = 0,
        .value = value,
    };
}

Match matchWritesValue(const Value* value) {
    return (Match) {
        .kind = MATCH_WRITES_VALUE,
        .flags = 0,
        .value = value,
    };
}

Match matchUsesValue(const Value* value) {
    return (Match) {
        .kind = MATCH_USES_VALUE,
        .flags = 0,
        .value = value,
    };
}

Match matchAnyJump(const Value** label) {
    return (Match) {
        .kind = MATCH_ANY_JUMP,
        .flags = MATCHFLAG_ALLOW_EXIT,
        .valuePtr = label,
    };
}

Match matchConditionalJump(const Value** label) {
    return (Match) {
        .kind = MATCH_CONDITIONAL_JUMP,
        .flags = MATCHFLAG_ALLOW_EXIT,
        .valuePtr = label,
    };
}

Match matchNormalJump(const Value** label) {
    return (Match) {
        .kind = MATCH_NORMAL_JUMP,
        .flags = MATCHFLAG_ALLOW_EXIT,
        .valuePtr = label,
    };
}

Match matchLabel(const Value** label) {
    return (Match) {
        .kind = MATCH_LABEL,
        .flags = MATCHFLAG_ALLOW_ENTER,
        .valuePtr = label,
    };
}

Match matchInstruction(const InstructionPrototype* prototype) {
    return (Match) {
        .kind = MATCH_INSTRUCTION,
        .flags = 0,
        .instruction = {
            .prototype = prototype,
            .dest = {
                .reg = NULL
            },
            .src = {
                .reg = NULL
            }
        }
    };
}



Match matchInstructionRegReg(const InstructionPrototype* prototype, Reg* dest, Reg* src) {
    Match match = matchInstruction(prototype);
    match.instruction.dest.reg = dest;
    match.instruction.src.reg = src;
    return match;
}

Match matchInstructionValueReg(const InstructionPrototype* prototype, const Value** dest, Reg* src) {
    Match match = matchInstruction(prototype);
    match.instruction.dest.value = dest;
    match.instruction.src.reg = src;
    return match;
}

Match matchInstructionRegValue(const InstructionPrototype* prototype, Reg* dest, const Value** src) {
    Match match = matchInstruction(prototype);
    match.instruction.dest.reg = dest;
    match.instruction.src.value = src;
    return match;
}

Match matchInstructionReg(const InstructionPrototype* prototype, Reg* dest) {
    return matchInstructionRegReg(prototype, dest, NULL);
}

Match matchInstructionValue(const InstructionPrototype* prototype, const Value** dest) {
    return matchInstructionValueReg(prototype, dest, NULL);
}

Match matchLoad(Reg* regOut, const Value** valueOut) {
    static const InstructionPrototype loadPrototype = {
        .opcode = INS_MOV,
        .dest = ADDRESS_REGISTER,
        .src = ADDRESS_VALUE,
    };

    return matchInstructionRegValue(&loadPrototype, regOut, valueOut);
}


Match matchStore(const Value** valueOut, Reg* regOut) {
    static const InstructionPrototype storePrototype = {
        .opcode = INS_MOV,
        .dest = ADDRESS_VALUE,
        .src = ADDRESS_REGISTER,
    };

    return matchInstructionValueReg(&storePrototype, valueOut, regOut);
}


bool instructionMatches(
    struct Optimizer* optim,
    const Instruction* instruction,
    Match* matcher
);

static bool instructionMatchesInner(
        struct Optimizer* optim,
        const Instruction* ins,
        Match* matcher
) {
    switch (matcher->kind) {
        case MATCH_OR:
            return instructionMatches(optim, ins, matcher->binary.left)
                || instructionMatches(optim, ins, matcher->binary.right);
        case MATCH_AND:
            return instructionMatches(optim, ins, matcher->binary.left)
                && instructionMatches(optim, ins, matcher->binary.right);
        case MATCH_ANY:
            return true;
        case MATCH_ENTER:
            return opcodeEnter(ins->opcode);
        case MATCH_EXIT:
            return opcodeExit(ins->opcode);
        case MATCH_PARTIAL_EXIT:
            return opcodePartialExit(ins->opcode);
        case MATCH_READS_REG:
            return insReads(ins, matcher->reg);
        case MATCH_WRITES_REG:
            return insWrites(ins, matcher->reg);
        case MATCH_USES_REG:
            return insReads(ins, matcher->reg) || insWrites(ins, matcher->reg);
        case MATCH_READS_VALUE:
            return insReadsValue(ins, matcher->value);
        case MATCH_WRITES_VALUE:
            return insWritesValue(ins, matcher->value);
        case MATCH_USES_VALUE:
            return insReadsValue(ins, matcher->value)
                || insWritesValue(ins, matcher->value);
        case MATCH_INSTRUCTION:
            return insMatchInstruction(ins, matcher);
        case MATCH_ANY_JUMP:
            return insAnyJump(ins, matcher->valuePtr);
        case MATCH_CONDITIONAL_JUMP:
            return insConditionalJump(ins, matcher->valuePtr);
        case MATCH_NORMAL_JUMP:
            return insNormalJump(ins, matcher->valuePtr);
        case MATCH_LABEL:
            return insLabel(ins, matcher->valuePtr);
    }
    UNIMPLEMENTED();
}

bool instructionMatches(
        struct Optimizer* optim,
        const Instruction* ins,
        Match* matcher
) {
    if (~matcher->flags & MATCHFLAG_ALLOW_EXIT && opcodePartialExit(ins->opcode)) {
        return false;
    }

    if (~matcher->flags & MATCHFLAG_ALLOW_ENTER && opcodeEnter(ins->opcode)) {
        return false;
    }

    bool result = instructionMatchesInner(optim, ins, matcher);
    if (matcher->flags & MATCHFLAG_INVERT) {
        return !result;
    }

    return result;
}

bool opcodeSkip(Opcode opcode) {
    switch (opcode) {
        case INS_DELETED:
        case INS_COMMENT:
        case INS_COMMENT_LOCATION:
        case INS_COMMENT_STATEMENT:
            return true;
        default:
            break;
    }
    return false;
}

static bool skipBlanks(Optimizer* optim) {
    while (opcodeSkip(optim->func->instructions[optim->patternEnd].opcode)) {
        optim->patternEnd += 1;
        if (optim->patternEnd >= optim->func->instructionsLength) {
            return false;
        }
    }

    return optim->patternEnd < optim->func->instructionsLength;
}

bool matchPattern(Optimizer* optim, Instruction** instruction, Match matcher) {
    if (!skipBlanks(optim)) {
        return false;
    }

    Instruction* thisInstruction = &optim->func->instructions[optim->patternEnd];
    if (!instructionMatches(optim, thisInstruction, &matcher)) {
        return false;
    }

    if (instruction != NULL) {
        *instruction = thisInstruction;
    }

    optim->patternEnd += 1;
    return true;
}

int matchAllPattern(Optimizer* optim, InsSpan* span, Match matcher) {
    int matches = 0;
    bool firstIns = true;

    for (;;) {
        if (!skipBlanks(optim)) {
            return matches;
        }

        Instruction* thisInstruction = &optim->func->instructions[optim->patternEnd];


        if (!instructionMatches(optim, thisInstruction, &matcher)) {
            return matches;
        }

        if (firstIns && span != NULL) {
            span->start = thisInstruction;
        }
        firstIns = false;

        if (span != NULL) {
            span->end = thisInstruction + 1;
        }

        optim->patternEnd += 1;
        matches += 1;
    }
}

