#ifndef DIAG_H
#define DIAG_H

#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>


#define BREAK_ON_PANIC


typedef struct {
    const char* fileName;
    int fileNameLength;
    int line;
    int column;
    int lineStart;
} QualifiedPosition;

typedef struct {
    QualifiedPosition start;
    QualifiedPosition end;
} QualifiedLocation;

//typedef const char* Position;
typedef int Position;

typedef struct {
    Position start;
    Position end;
} Location;

#define NO_POSITION ((Position)-1)
#define NO_LOCATION ((Location){.start = NO_POSITION, .end = NO_POSITION})


typedef struct {
    const char* fileText;
    int errorCount;
    int warningCount;
} Diagnostics;

Diagnostics newDiagnostics(const char* text);

Position positionMin(Position pos0, Position pos1);
Position positionMax(Position pos0, Position pos1);

Location locNew(Position start, Position end);
Location locSpan(Location first, Location last);
Location locSpanPosition(Location loc, Position position);

void printLocation(FILE* file, Diagnostics* diag, Location location);

void errorStart(Diagnostics* diag, Location location);
void warningStart(Diagnostics* diag, Location location);

void vprintDiagnostic(Diagnostics* diag, const char* level, Location location, const char* format, va_list args);
void vprintError(Diagnostics* diag, Location location, const char* format, va_list args); 
void printError(Diagnostics* diag, Location location, const char* format, ...);
void vprintWarning(Diagnostics* diag, Location location, const char* format, va_list args);
void printWarning(Diagnostics* diag, Location location, const char* format, ...);

[[noreturn]]
void panic(const char* fileName, int line, const char* function, const char* format, ...);
[[noreturn]]
void vpanic(const char* fileName, int line, const char* function, const char* format, va_list args);

#define PANIC(FORMAT, ...) \
    panic(__FILE__, __LINE__, __FUNCTION__, (FORMAT) __VA_OPT__(,) __VA_ARGS__)

#define UNREACHABLE() \
    PANIC("unreachable code")

#define UNIMPLEMENTED() \
    PANIC("not yet implemented")

#define ASSERT(CONDITION, FORMAT, ...) \
    do { \
        if (!(CONDITION)) { \
            PANIC((FORMAT) __VA_OPT__(,) __VA_ARGS__); \
        } \
    } while (0)

#endif

