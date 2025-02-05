#ifndef RULE_H
#define RULE_H

#include <stdbool.h>

struct Optimizer;

typedef struct {
    const char* name;
    bool (*fn)(struct Optimizer* optim);
    int level;
} Optimization;

const Optimization* getOptimizations();
int getOptimizationCount();

#endif

