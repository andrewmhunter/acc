#include "diag.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

Diagnostics newDiagnostics(const char* text) {
    return (Diagnostics) {
        .fileText = text,
        .errorCount = 0,
        .warningCount = 0,
    };
}

Position positionMin(Position pos0, Position pos1) {
    if (pos0 == NULL) {
        return pos1;
    }

    return pos0 < pos1 ? pos0 : pos1;
}

Position positionMax(Position pos0, Position pos1) {
    return pos0 > pos1 ? pos0 : pos1;
}

Location locNew(Position start, Position end) {
    return (Location) {
        .start = positionMin(start, end),
        .end = positionMax(start, end),
    };
}

static const char defaultFilename[] = "<source>";

static QualifiedPosition positionQualify(Diagnostics* diag, Position position) {
    QualifiedPosition qualified = {
        .fileName = defaultFilename,
        .fileNameLength = strlen(defaultFilename),
        .line = 1,
        .column = 1
    };

    if (diag->fileText == NULL || position == NULL) {
        qualified.line = 0;
        qualified.column = 0;
        return qualified;
    }

    bool blankLine = true;
    bool settingLocation = true;

    for (const char* ch = diag->fileText; ch < position; ++ch) {
        qualified.column++;
        switch (*ch) {
            case '\n':
                qualified.line++;
                blankLine = true;

                if (settingLocation) {
                    int newLine = 0;
                    int fnameStart = 0;
                    int fnameEnd = 0;
                    int parsed = sscanf(ch, "# %d %n%*s%n", &newLine, &fnameStart, &fnameEnd);
                    const char* newName = ch;
                    int newLength = fnameEnd - fnameStart;
                    if (parsed == 3 && newName[0] == '"' && newName[newLength - 1] == '"') {
                        qualified.line = newLine;
                        qualified.fileName = newName + 1;
                        qualified.fileNameLength = newLength - 2;
                    }
                }

                qualified.column = 1;
                settingLocation = false;
                break;
            case ' ':
            case '\t':
                break;
            case '#':
                if (blankLine) {
                    settingLocation = true;
                }

                blankLine = false;
                break;
            case '\0':
                PANIC("cannot qualify invalid position");
            default:
                blankLine = false;
                break;
        }
    }

    return qualified;
}

Location locSpan(Location first, Location last) {
    return locNew(
        positionMin(first.start, last.start),
        positionMax(first.end, last.end)
    );
}

Location locSpanPosition(Location loc, Position position) {
    return locSpan(loc, locNew(position, NO_POSITION));
}

void printLocation(FILE* file, Diagnostics* diag, Location location) {
    if (diag->fileText == NULL || location.start == NULL) {
        fprintf(file, "?:?.?");
        return;
    }

    QualifiedPosition start = positionQualify(diag, location.start);
    QualifiedPosition end = positionQualify(diag, location.end);

    bool filesSame = (start.fileNameLength == end.fileNameLength)
        && (strncmp(start.fileName, end.fileName, start.fileNameLength) == 0);

    bool linesSame = start.line == end.line;

    fprintf(file, "%.*s", start.fileNameLength, start.fileName);
    if (!filesSame) {
        fprintf(file, "-%.*s", end.fileNameLength, end.fileName);
    }

    fprintf(file, ":%d", start.line);
    if (!linesSame || !filesSame) {
        fprintf(file, "-%d", end.line);
    }

    fprintf(file, ".%d", start.column);
    if (start.column != end.column || !linesSame || !filesSame) {
        fprintf(file, "-%d", end.column);
    }
}


static void diagnosticStart(Diagnostics* diag, const char* level, Location location) {
    if (location.start != NULL) {
        printLocation(stderr, diag, location);
        fprintf(stderr, ": ");
    }
    fprintf(stderr, "%s:\x1b[0m ", level);
}

void errorStart(Diagnostics* diag, Location location) {
    diagnosticStart(diag, "\x1b[31merror", location);
    diag->errorCount += 1;
}

void warningStart(Diagnostics* diag, Location location) {
    diagnosticStart(diag, "\x1b[35mwarning", location);
    diag->warningCount += 1;
}

void vprintError(Diagnostics* diag, Location location, const char* format, va_list args) {
    errorStart(diag, location);
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
}

void printError(Diagnostics* diag, Location location, const char* format, ...) {
    va_list args;
    va_start(args, format);
    vprintError(diag, location, format, args);
    va_end(args);
}

void vprintWarning(Diagnostics* diag, Location location, const char* format, va_list args) {
    warningStart(diag, location);
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
}

void printWarning(Diagnostics* diag, Location location, const char* format, ...) {
    va_list args;
    va_start(args, format);
    vprintWarning(diag, location, format, args);
    va_end(args);
}


void panic(const char* file, int line, const char* function, const char* format, ...) {
    va_list args;
    va_start(args, format);
    vpanic(file, line, function, format, args);
    va_end(args);
}

void vpanic(const char* file, int line, const char* function, const char* format, va_list args) {
    fprintf(stderr, "panic at: %s:%d: %s: ", file, line, function);
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");

#ifdef BREAK_ON_PANIC
    raise(SIGTRAP);
#endif

    exit(4);
}

