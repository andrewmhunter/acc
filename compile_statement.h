#ifndef COMPILE_STATEMENT_H
#define COMPILE_STATEMENT_H

#include "compiler.h"
#include "statement.h"

void compileFunction(Compiler* compiler, const FunctionDeclaration* decl);
void compileProgram(Compiler* compiler, const Program* program);

#endif

