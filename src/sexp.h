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
    SExpRef env;
} SExpFunc;

struct interp;
typedef struct interp Interp;
typedef SExpRef (*LispUserFunc)(Interp *interp, SExpRef args);

typedef struct {
    SExpRef args;
    SExpRef body;
} SExpMacro;

typedef struct {
    SExpRef parent;
    SExpRef bindings;
} SExpEnv;

typedef struct {
    SExpRef name;
    SExpRef value;
    SExpRef func;
    SExpRef next;
} SExpBinding;

typedef struct {
    SExpRef fn;
    SExpRef args;
} SExpTailcall;

typedef enum {
    kEmptySExp,
    kIntegerSExp,
    kRealSExp,
    kBooleanSExp,
    kNilSExp,
    kCharSExp,
    kStringSExp,
    kSymbolSExp,
    kUserDataSExp,
    kPairSExp,
    kFuncSExp,
    kUserFuncSExp,
    kEnvSExp,
    kBindingSExp,
    kMacroSExp,
    kErrSignal,
    kReturnSignal,
    kBreakSignal,
    kContinueSignal,
    kTailcallSExp,
} SExpType;

struct sexp {
    bool marked;
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
        LispUserFunc userfunc;
        SExpEnv env;
        SExpBinding binding;
        SExpMacro macro;
        SExpRef ret;
        SExpTailcall tailcall;
    };
};

void SExp_show(SExp self, FILE* fp);
void SExpRef_show(SExpRef self, FILE* fp);

VECTOR_DEF(SExp);
VECTOR_DEF(SExpRef);

#endif

