#ifndef OPTIMIZER_H
#define OPTIMIZER_H

#include "compiler.h"
#include "mem.h"
#include "match.h"
#include <stdbool.h>

#define LOG_OPTIMIZER
#define MAX_OPTIMIZATION_PASSES 1024
#define DEFAULT_OPTIMIZATION_LEVEL 0

typedef struct Optimizer {
    Arena* arena;
    Function* func;
    size_t patternStart;
    size_t patternEnd;
    int optimizationLevel;
} Optimizer;

void optimizeProgram(Arena* arena, Function* program);
bool optimizerPass(Arena* arena, Function* func, int optimizationLevel);
void optimizeFunction(Arena* arena, Function* program, int optimizationLevel);

void optimDelete(Instruction* ins);
void optimDeleteAll(InsSpan* span);
void optimReplace(Instruction* ins, const Instruction replacement);

#endif

