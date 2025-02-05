#include "rule.h"
#include "optimizer.h"
#include "match.h"
#include "util.h"

static bool ruleDeadCode(Optimizer* optim) {
    if (!matchPattern(optim, NULL, matchExit())) {
        return false;
    }

    InsSpan deadCode;
    if (!matchAllPattern(optim, &deadCode, matchAllowExit(matchAny()))) {
        return false;
    }

    optimDeleteAll(&deadCode);
    return true;
}

static bool binaryToUnary(Optimizer* optim, Opcode initialOp, Opcode swapOp, int onValue) {
    const InstructionPrototype prototype = {
        .opcode = initialOp,
        .dest = ADDRESS_REGISTER,
        .src = ADDRESS_VALUE,
    };

    Instruction* initial = NULL;
    Reg reg = REG_A;
    Value value = valueImmediateExpr(NULL, exprLiteral(optim->arena, onValue, NO_LOCATION));
    const Value* valuePtr = &value;
    if (!matchPattern(optim, &initial, matchInstructionRegValue(&prototype, &reg, &valuePtr))) {
        return false;
    }

    optimReplace(initial, instruction1(swapOp, (Address){.kind = ADDRESS_REGISTER, .reg = reg}));
    return true;
}

static bool ruleAddToInc(Optimizer* optim) {
    return binaryToUnary(optim, INS_ADD, INS_INC, 1);
}

static bool ruleAddcToIncc(Optimizer* optim) {
    return binaryToUnary(optim, INS_ADDC, INS_INCC, 0);
}

static bool ruleSubToDec(Optimizer* optim) {
    return binaryToUnary(optim, INS_SUB, INS_DEC, 1);
}

static bool ruleSubbToDecb(Optimizer* optim) {
    return binaryToUnary(optim, INS_SUBB, INS_DECB, 0);
}

static bool unaryInPlace(Optimizer* optim, Opcode op) {
    const InstructionPrototype prototype = {
        .opcode = op,
        .dest = ADDRESS_REGISTER,
        .src = ADDRESS_IMPLIED,
    };

    Reg reg = REG_INVALID;
    const Value* value = NULL;
    Instruction* load = NULL;
    if (!matchPattern(optim, &load, matchLoad(&reg, &value))) {
        return false;
    }

    Instruction* unary = NULL;
    if (!matchPattern(optim, &unary, matchInstructionReg(&prototype, &reg))) {
        return false;
    }

    Instruction* store = NULL;
    if (!matchPattern(optim, &store, matchStore(&value, &reg))) {
        return false;
    }

    optimReplace(load, instruction1Value(optim->arena, op, GET_ADDRESS_VALUE(), *value, 0));
    optimDelete(unary);
    optimDelete(store);
    return true;
}

static bool ruleIncInPlace(Optimizer* optim) {
    return unaryInPlace(optim, INS_INC);
}

static bool ruleInccInPlace(Optimizer* optim) {
    return unaryInPlace(optim, INS_INCC);
}

static bool ruleDecInPlace(Optimizer* optim) {
    return unaryInPlace(optim, INS_DEC);
}

static bool ruleDecbInPlace(Optimizer* optim) {
    return unaryInPlace(optim, INS_DECB);
}

static bool ruleDoubleLoad(Optimizer* optim) {
    Reg reg = REG_INVALID;
    const Value* value = NULL;
    if (!matchPattern(optim, NULL, matchLoad(&reg, &value))) {
        return false;
    }

    Match regWrites = matchWrites(reg);
    Match valueWrites = matchWritesValue(value);
    matchAllPattern(optim, NULL, matchNot(matchOr(&regWrites, &valueWrites)));

    Instruction* doubleLoad = NULL;
    if (!matchPattern(optim, &doubleLoad, matchLoad(&reg, &value))) {
        return false;
    }

    optimDelete(doubleLoad);
    return true;
}

static bool ruleLoadAfterStore(Optimizer* optim) {
    Reg reg = REG_INVALID;
    const Value* value = NULL;
    if (!matchPattern(optim, NULL, matchStore(&value, &reg))) {
        return false;
    }

    Match valueUses = matchUsesValue(value);
    Match regWrites = matchWrites(reg);
    matchAllPattern(optim, NULL, matchNot(matchOr(&valueUses, &regWrites)));

    Instruction* loadAfterStore = NULL;
    if (!matchPattern(optim, &loadAfterStore, matchLoad(&reg, &value))) {
        return false;
    }

    optimDelete(loadAfterStore);
    return true;
}

static bool ruleDoubleStore(Optimizer* optim) {
    const Value* value = NULL;
    Instruction* firstStore = NULL;

    Match storeMatch = matchStore(&value, NULL);

    if (!matchPattern(optim, &firstStore, storeMatch)) {
        return false;
    }

    Match uses = matchUsesValue(value);
    matchAllPattern(optim, NULL, matchNot(matchOr(&uses, &storeMatch)));

    if (!matchPattern(optim, NULL, storeMatch)) {
        return false;
    }

    optimDelete(firstStore);
    return true;
}

static bool ruleUselessJump(Optimizer* optim) {
    const Value* label = NULL;
    Instruction* jump = NULL;

    if (!matchPattern(optim, &jump, matchAnyJump(&label))) {
        return false;
    }

    if (!matchPattern(optim, NULL, matchLabel(&label))) {
        return false;
    }

    optimDelete(jump);
    return true;
}

static bool ruleConditionalJumpOverJump(Optimizer* optim) {
    const Value* label = NULL;
    Instruction* conditional = NULL;

    if (!matchPattern(optim, &conditional, matchConditionalJump(&label))) {
        return false;
    }

    Instruction* normal = NULL;
    if (!matchPattern(optim, &normal, matchNormalJump(NULL))) {
        return false;
    }

    if (!matchPattern(optim, NULL, matchLabel(&label))) {
        return false;
    }

    normal->opcode = invertJump(conditional->opcode);
    optimDelete(conditional);
    return true;
}


static const Optimization optimizations[] = {
    {"deadCode", ruleDeadCode, 1},

    {"addToInc", ruleAddToInc, 1},
    {"addcToIncc", ruleAddcToIncc, 1},
    {"subToDec", ruleSubToDec, 1},
    {"subbToDecb", ruleSubbToDecb, 1},

    {"conditionalJumpOverJump", ruleConditionalJumpOverJump, 1},
    {"uselessJump", ruleUselessJump, 1},

    {"doubleLoad", ruleDoubleLoad, 2},
    {"loadAfterStore", ruleLoadAfterStore, 2},
    {"doubleStore", ruleDoubleStore, 2},

    {"incInPlace", ruleIncInPlace, 1},
    {"inccInPlace", ruleInccInPlace, 1},
    {"decInPlace", ruleDecInPlace, 1},
    {"decbInPlace", ruleDecbInPlace, 1},
};

const Optimization* getOptimizations() {
    return optimizations;
}

int getOptimizationCount() {
    return LENGTH_OF(optimizations);
}

