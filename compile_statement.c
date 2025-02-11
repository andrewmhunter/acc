#include "compile_statement.h"
#include "statement.h"
#include "compiler.h"
#include "diag.h"
#include "compile_expression.h"
#include "compile_condition.h"
#include "optimizer.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

static void compileStatement(Function* func, const Statement* stmt);

static void compileBlock(Function* func, const Statement* stmt) {
    enterScope(func);
    for (size_t i = 0; i < stmt->block.length; ++i) {
        compileStatement(func, stmt->block.data[i]);
    }
    leaveScope(func);
}

static void compileVariable(Function* func, const Type* type, Identifier id, const Expression* expr) {
    for (int i = func->stackLength - 1; i >= 0; --i) {
        if (func->stack[i].depth != func->scopeDepth) {
            break;
        }

        if (identEquals(&id, &func->stack[i].ident)) {
            printError(func->diag, identLoc(&id), "redefined variable");
            return;
        }
    }

    Value value = pushStack(func, type, id);

    if (expr) {
        compileExpression(func, expr, targetValue(value));
    }
}

static void compileIf(Function* func, const Statement* stmt) {
    Value elseLabel = createLabel(func->compiler);
    compileCondition(func, stmt->conditional.condition, conditionTarget(elseLabel, INVERT));
    compileStatement(func, stmt->conditional.inner);

    Value endLabel;
    if (stmt->conditional.onElse != NULL) {
        endLabel = createLabel(func->compiler);
        emitJump(func, endLabel);
    }

    emitLabel(func, elseLabel);

    if (stmt->conditional.onElse != NULL) {
        compileStatement(func, stmt->conditional.onElse);
        emitLabel(func, endLabel);
    }
}

static void compileWhile(Function* func, const Statement* stmt) {
    Value loopLabel = createLabel(func->compiler);
    Value endLabel = createLabel(func->compiler);

    emitLabel(func, loopLabel);
    compileCondition(func, stmt->whileLoop.condition, conditionTarget(endLabel, INVERT));
    compileStatement(func, stmt->whileLoop.inner);
    emitJump(func, loopLabel);
    emitLabel(func, endLabel);
}

static void compileReturn(Function* func, const Expression* expr, Location loc) {
    bool returnTypeIsVoid = isVoid(func->decl->returnType);
    if (expr == NULL) {
        if (!returnTypeIsVoid) {
            printError(func->diag, loc, "void function should not return a value");
        }

        emitImplied(func, INS_RET);
        return;
    }

    if (returnTypeIsVoid) {
        printError(func->diag, loc, "non-void function must return a value");
    }

    Value rvalue = getFuncReturnValue(func->lifetime, func->decl, NO_LOCATION);
    compileExpression(func, expr, targetValue(rvalue));
    emitImplied(func, INS_RET);
}

static void compileStatement(Function* func, const Statement* stmt) {
#ifdef EMIT_COMMENTS_LOCATION
    emitCommentLocation(func, stmtLoc(stmt));
#endif
#ifdef EMIT_COMMENTS_STATEMENT
    emitCommentStatement(func, stmt);
#endif

    switch (stmt->type) {
        case STATEMENT_BLOCK:
            compileBlock(func, stmt);
#ifdef EMIT_COMMENTS_STATEMENT
            emitComment(func, "}");
#endif
            break;
        case STATEMENT_EXPRESSION:
            enterScope(func);
            compileExpression(func, stmt->expression, DISCARD_TARGET);
            leaveScope(func);
            break;
        case STATEMENT_VARIABLE:
            compileVariable(func,
                    stmt->variableDeclaration.type,
                    stmt->variableDeclaration.id,
                    stmt->variableDeclaration.expr);
            break;
        case STATEMENT_IF:
            compileIf(func, stmt);
            break;
        case STATEMENT_WHILE:
            compileWhile(func, stmt);
            break;
        case STATEMENT_RETURN:
            compileReturn(func, stmt->expression, stmtLoc(stmt));
            break;
    }
}

bool functionDeclMatchesExisting(Compiler* compiler, const FunctionDeclaration* decl) {
    const Declaration* existingDecl = lookupGlobal(compiler, decl->name);
    if (existingDecl == NULL) {
        return true;
    }
    const FunctionDeclaration* existing = &existingDecl->function;

    if (existingDecl->kind != DECL_FUNCTION) {
        printError(compiler->diag, decl->location,
                "'%.*s' declared earlier as non-function",
                decl->name.length, decl->name.start);
        return false;
    }

    if (!typeEquals(decl->returnType, existing->returnType)) {
        printError(compiler->diag, decl->location,
                "'%.*s' declared with different return value",
                decl->name.length, decl->name.start);
        return false;
    }

    if (decl->arity != existing->arity) {
        printError(compiler->diag, decl->location,
                "'%.*s' declared with different parameter count",
                decl->name.length, decl->name.start);
        return false;
    }

    for (size_t i = 0; i < decl->arity; ++i) {
        if (decl->arity != existing->arity) {
            printError(compiler->diag, decl->location,
                    "'%.*s' declared with different parameter types",
                    decl->name.length, decl->name.start);
            return false;
        }
    }

    return true;
}

Function* compileFunction(Compiler* compiler, const FunctionDeclaration* decl) {
    // Make sure this function declaration matches previous declarations of the function
    functionDeclMatchesExisting(compiler, decl);
    compiler->globalCount += 1;

    if (decl->body == NULL) {
        return NULL;
    }

    // Make sure that the function hasn't previously been defined
    for (int i = 0; i < compiler->globalCount - 1; ++i) {
        const Declaration* existing = compiler->globals[i];
        if (existing->kind != DECL_FUNCTION || !identEquals(&existing->name, &decl->name)) {
            continue;
        }

        if (existing->function.body != NULL) {
            printError(compiler->diag, decl->location,
                    "'%.*s' already defined",
                    decl->name.length, decl->name.start);
            break;
        }
    }

    Function* func = functionNew(compiler->lifetime, compiler, decl);

    // stmtPrint(stdout, decl->body, 0, false);
    
    if (!isVoid(decl->returnType)) {
        pushStackAnon(func, decl->returnType, NO_LOCATION);
    }

    for (size_t i = 0; i < decl->arity; ++i) {
        compileVariable(func, decl->parameters[i].type, decl->parameters[i].name, NULL);
    }

    compileStatement(func, decl->body);
    emitImplied(func, INS_RET);

    optimizeFunction(func->lifetime, func, func->compiler->optimizationLevel);

    printFunction(stdout, func);

    //free(func.instructions);

    return func;
}

void compileGlobalVariable(Compiler* compiler, const VariableDeclaration* decl) {
    if (lookupGlobal(compiler, decl->name) != NULL) {
        printError(compiler->diag, decl->location, "symbol already defined");
    }
    compiler->globalCount += 1;

    fprintf(stdout, "variable ");
    identPrint(stdout, &decl->name);
    fprintf(stdout, " %d\n", typeSize(decl->type));
}

void compileProgram(Compiler* compiler, const Program* program) {
    fprintf(stdout,
        "    call main\n"
        "    hlt\n"
        "out:\n"
        "    mov a, [_Stack_out + 0]\n"
        "    out a\n"
        "    ret\n"
        "variable _Stack_out 1\n"
        "\n"
        //"include \"stdlib/multiply.spdr\"\n"
        //"include \"stdlib/multiply_16.spdr\"\n"
        //"include \"stdlib/divide.spdr\"\n"
        //"include \"stdlib/divide_16.spdr\"\n"
    );

    Function* head = NULL;
    Function** nextFunction = &head;
    
    for (size_t i = 0; i < program->delarationsCount; ++i) {
        const Declaration* decl = program->declarations[i];

        switch (decl->kind) {
            case DECL_FUNCTION:
            {
                Function* func = compileFunction(compiler, &decl->function);
                if (func == NULL) {
                    break;
                }
                *nextFunction = func;
                nextFunction = &func->nextFunction;
                break;
            }
            case DECL_VARIABLE:
                compileGlobalVariable(compiler, &decl->variable);
                break;
        }
    }

    const char mainName[] = "main";
    Identifier mainId = {.start = mainName, .length = strlen(mainName), .position = -1};
    const Declaration* mainDecl = lookupGlobal(compiler, mainId);

    if (mainDecl == NULL || mainDecl->kind != DECL_FUNCTION) {
        printError(compiler->diag, NO_LOCATION, "main function not defined");
        return;
    }

    if (mainDecl->function.arity != 0) {
        printError(compiler->diag, identLoc(&mainDecl->function.parameters[0].name), "main function should take no arguments");
        return;
    }
}

