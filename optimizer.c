#include "optimizer.h"
#include "match.h"
#include "rule.h"
#include "hash.h"
#include <stdbool.h>
#include <stdio.h>

void optimDelete(Instruction* ins) {
    ins->opcode = INS_DELETED;
    ins->dest.kind = ADDRESS_IMPLIED;
    ins->src.kind = ADDRESS_IMPLIED;
}

void optimDeleteAll(InsSpan* span) {
    for (Instruction* ins = span->start; ins < span->end; ++ins) {
        optimDelete(ins);
    }
}

void optimReplace(Instruction* ins, const Instruction replacement) {
    *ins = replacement;
}

void optimInsertBefore(Optimizer* optim, Instruction* ins, const Instruction toInsert) {
    insertInstruction(optim->func, toInsert, ins - optim->func->instructions);
}

void optimInsertAfter(Optimizer* optim, Instruction* ins, const Instruction toInsert) {
    insertInstruction(optim->func, toInsert, ins - optim->func->instructions + 1);
}

static Optimizer optimNew(Arena* arena, Function* func, int optimizationLevel) {
    return (Optimizer) {
        .arena = arena,
        .func = func,
        .patternStart = 0,
        .patternEnd = 0,
        .optimizationLevel = optimizationLevel,
    };
}

static bool optimizeSpan(Optimizer* optim) {
    const Optimization* optimizations = getOptimizations();
    int optimizationCount = getOptimizationCount();

    for (int i = 0; i < optimizationCount; ++i) {
        optim->patternEnd = optim->patternStart;

        if (optimizations[i].level > optim->optimizationLevel) {
            continue;
        }

        if (optimizations[i].fn(optim)) {
#ifdef LOG_OPTIMIZER
            fprintf(stderr, "OPT: applied %s %lu-%lu\n",
                optimizations[i].name, optim->patternStart, optim->patternEnd - 1);
#endif
            return true;
        }
    }
    return false;
}

bool optimizerPass(Arena* arena, Function* func, int optimizationLevel) {
    Optimizer optim = optimNew(arena, func, optimizationLevel);

    bool appliedOptimizations = false;
    while (optim.patternStart < func->instructionsLength) {
        if (optimizeSpan(&optim)) {
            appliedOptimizations = true;
            continue;
        }
        optim.patternStart++;
    }

    return appliedOptimizations;
}

static void findLabels(Set* set, const Function* func) {
    for (size_t i = 0; i < func->instructionsLength; ++i) {
        const Instruction* ins = &func->instructions[i];
        if (!opcodeSkip(ins->opcode)
            && ins->opcode != INS_LABEL
            && (ins->dest.kind == ADDRESS_VALUE || ins->src.kind == ADDRESS_VALUE)
            && (ins->value.kind == VALUE_DIRECT || ins->value.kind == VALUE_IMMEDIATE)
            && ins->value.expression->type == EXPR_LABEL
        ) {
            Label* label = malloc(sizeof(label));
            *label = ins->value.expression->label;
            setInsert(set, label);
        }
    }
}

static void deleteLabels(Set* set, const Function* func) {
    for (size_t i = 0; i < func->instructionsLength; ++i) {
        Instruction* ins = &func->instructions[i];
        if (ins->opcode == INS_LABEL
            && ins->dest.kind == ADDRESS_VALUE
            && ins->value.expression->type == EXPR_LABEL
            && !setHas(set, &ins->value.expression->label)
        ) {
#ifdef LOG_OPTIMIZER
            fprintf(stderr, "OPT: deleting label %d\n", ins->value.expression->label);
#endif
            optimDelete(ins);
        }
    }
}

static void labelHash(Hash* hash, const void* label) {
    hashBytes(hash, label, sizeof(Label));
}

static bool labelEquals(const void* left, const void* right) {
    return *(Label*)left == *(Label*)right;
}

static void removeUnusedLabels(const Function* func) {
    Set labels = setNew(labelHash, labelEquals);
    findLabels(&labels, func);
    deleteLabels(&labels, func);
    setFreeAll(&labels);
}


void optimizeFunction(Arena* arena, Function* func, int optimizationLevel) {
    if (optimizationLevel <= 0) {
        return;
    }
    
    do {
#ifdef LOG_OPTIMIZER
    fprintf(stderr, "OPT: Optimizing %.*s\n",
            func->decl->name.length, func->decl->name.start);
#endif
        removeUnusedLabels(func);
    } while (optimizerPass(arena, func, optimizationLevel));
}

void optimizeProgram(Arena* arena, Function* program) {
    Function* current = program;
    while (current != NULL) {
        optimizeFunction(arena, current, DEFAULT_OPTIMIZATION_LEVEL);
        current = current->nextFunction;
    }
}

