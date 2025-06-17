#ifndef BAMBOO_LISP_SEXP_H_
#define BAMBOO_LISP_SEXP_H_

#include <stdint.h>
#include <stdbool.h>

#include <algds/vec.h>

struct sexp;
typedef struct sexp SExp;

typedef struct {
    int idx;
} SExpRef;

typedef struct {
    SExpRef car;
    SExpRef cdr;
} SExpPair;

typedef struct {
    SExpRef args;
    SExpRef body;
} SExpFunc;

typedef struct {
    SExpRef args;
    SExpRef body;
} SExpMacro;

typedef struct {
    SExpRef parent;
    SExpRef child;
    SExpRef bindings;
} SExpEnv;

typedef struct {
    SExpRef name;
    SExpRef value;
    SExpRef func;
    SExpRef next;
} SExpBinding;

typedef enum {
    kIntegerSExp,
    kRealSExp,
    kBooleanSExp,
    kNumberSExp,
    kCharSExp,
    kStringSExp,
    kSymbolSExp,
    kUserDataSExp,
    kPairSExp,
    kFuncSExp,
    kEnvSExp,
    kBindingSExp,
    kMacroSExp,
} SExpType;

struct sexp {
    SExpType type;
    union {
        int64_t integer;
        double real;
        bool boolean;
        char character;
        const char *str;
        const void *userdata;
        SExpPair pair;
        SExpFunc func;
        SExpEnv env;
    };
};

void SExp_show(SExp self, FILE* fp);

VECTOR_DEF(SExp);

#endif

