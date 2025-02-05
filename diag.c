#include "diag.h"
#include "util.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

#define ANSI_RED 31
#define ANSI_PURPLE 35

Diagnostics newDiagnostics(const char* text) {
    return (Diagnostics) {
        .fileText = text,
        .errorCount = 0,
        .warningCount = 0,
    };
}

Position positionMin(Position pos0, Position pos1) {
    if (pos0 == NO_POSITION) {
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

static void parseStringLocation(const char* string, QualifiedPosition* into) {
    int newLine = 0;
    int fnameStart = 0;
    int fnameEnd = 0;
    int parsed = sscanf(string, "# %d \"%n%*s%n ", &newLine, &fnameStart, &fnameEnd);
    const char* newName = string + fnameStart;
    int newLength = fnameEnd - fnameStart - 1;

    //fprintf(stderr, "PARSED: %d, SETTING_TO: %.*s:%d\n", parsed, newLength, newName, newLine);
    if (parsed == 1 && newName[newLength] == '"') {
        into->line = newLine;
        into->fileName = newName;
        into->fileNameLength = newLength;
    }
}

static QualifiedPosition positionQualify(Diagnostics* diag, Position position) {
    QualifiedPosition qualified = {
        .fileName = defaultFilename,
        .fileNameLength = strlen(defaultFilename),
        .line = 1,
        .column = 1,
        .lineStart = 0
    };

    if (diag->fileText == NULL || position == NO_POSITION) {
        qualified.line = 0;
        qualified.column = 0;
        return qualified;
    }

    bool blankLine = true;
    bool settingLocation = true;

    for (const char* ch = diag->fileText; ch < diag->fileText + position; ++ch) {
        qualified.column++;
        switch (*ch) {
            case '\n':
                qualified.line++;
                blankLine = true;

                if (settingLocation) {
                    parseStringLocation(diag->fileText + qualified.lineStart, &qualified);
                }

                qualified.column = 1;
                qualified.lineStart = ch + 1 - diag->fileText;
                settingLocation = false;
                break;
            case ' ':
            case '\t':
                break;
            case '#':
                if (blankLine && ch[1] == ' ') {
                    settingLocation = true;
                    qualified.lineStart = ch - diag->fileText;
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

static void printPosition(FILE* file, const QualifiedPosition* pos, int partsToPrint) {
    if (partsToPrint >= 3) {
        fprintf(file, "%.*s:", pos->fileNameLength, pos->fileName);
    }

    if (partsToPrint >= 2) {
        fprintf(file, "%d.", pos->line);
    }

    if (partsToPrint >= 1) {
        fprintf(file, "%d", pos->column);
    }
}

void printLocation(FILE* file, Diagnostics* diag, Location location) {
    if (diag->fileText == NULL || location.start == NO_POSITION) {
        fprintf(file, "?:?.?");
        return;
    }

    QualifiedPosition start = positionQualify(diag, location.start);
    QualifiedPosition end = positionQualify(diag, location.end);

    printPosition(file, &start, 3);

    bool filesSame = (start.fileNameLength == end.fileNameLength)
        && (strncmp(start.fileName, end.fileName, start.fileNameLength) == 0);

    bool linesSame = start.line == end.line;
    bool columnsSame = start.column == end.column;

    int partsToPrint = MAX(MAX(!filesSame * 3, !linesSame * 2), !columnsSame * 1);

    if (partsToPrint > 0) {
        fprintf(file, "-");
        printPosition(file, &end, partsToPrint);
    }
}


static void diagnosticStart(Diagnostics* diag, int color, const char* level, Location location) {
    if (location.start != NO_POSITION) {
        printLocation(stderr, diag, location);
        fprintf(stderr, ": ");
    }
    fprintf(stderr, "\x1b[%dm%s:\x1b[0m ", color, level);
}

void errorStart(Diagnostics* diag, Location location) {
    diagnosticStart(diag, ANSI_RED, "error", location);
    diag->errorCount += 1;
    raise(SIGTRAP);
}

void warningStart(Diagnostics* diag, Location location) {
    diagnosticStart(diag, ANSI_PURPLE, "warning", location);
    diag->warningCount += 1;
}

static void diagnosticEnd(Diagnostics* diag, int color, Location location) {
    fprintf(stderr, "\n");
    return;

    if (location.start == NO_POSITION || location.end == NO_POSITION) {
        return;
    }

    QualifiedPosition start = positionQualify(diag, location.start);
    QualifiedPosition end = positionQualify(diag, location.end);

    const char* lineStr = start.lineStart + diag->fileText;

    int lineLength = 0;
    for (;;) {
        char ch = lineStr[lineLength];
        if (ch == '\n' || ch == '\0') {
            break;
        }
        lineLength++;
    }

    int lineOffset = fprintf(stderr, "| %d: ", start.line);
    fprintf(stderr, "%.*s\n", lineLength, lineStr);

    fprintf(stderr, "| ");
    for (int i = 2; i < lineOffset + start.column - 1; ++i) {
        fprintf(stderr, " ");
    }

    fprintf(stderr, "\x1b[%dm^", color);

    int spanLength = start.line == end.line ? end.column - start.column + 1: lineLength - start.column;

    for (int i = 1; i < spanLength; ++i) {
        fprintf(stderr, "~");
    }

    fprintf(stderr, "\x1b[0m\n");
}

void errorEnd(Diagnostics* diag, Location location) {
    diagnosticEnd(diag, ANSI_RED, location);
}

void warningEnd(Diagnostics* diag, Location location) {
    diagnosticEnd(diag, ANSI_PURPLE, location);
}

void vprintError(Diagnostics* diag, Location location, const char* format, va_list args) {
    errorStart(diag, location);
    vfprintf(stderr, format, args);
    errorEnd(diag, location);
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
    warningEnd(diag, location);
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

