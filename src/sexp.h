#ifndef BAMBOO_LISP_SEXP_H_
#define BAMBOO_LISP_SEXP_H_

#include <stdint.h>
#include <stdbool.h>

#include <algds/vec.h>

struct sexp;
typedef struct sexp SExp;

typedef struct {
    int32_t idx;
} SExpRef;

typedef struct {
    SExpRef car;
    SExpRef cdr;
    SExpRef filename;
    int32_t line;
} SExpPair;

typedef struct {
    SExpRef args;
    SExpRef body;
    SExpRef env;
} SExpFunc;

struct interp;
typedef struct interp Interp;
typedef SExpRef (*LispUserFunc)(Interp *interp, SExpRef args);
typedef SExpRef (*LispPrimitive)(Interp *interp, SExpRef sexp, bool istail);

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
    kPrimitiveSExp,
    kEnvSExp,
    kBindingSExp,
    kMacroSExp,
    kErrSignal,
    kReturnSignal,
    kBreakSignal,
    kContinueSignal,
    kTailcallSExp,
    kExceptionSignal,
} SExpType;

VECTOR_DEF(SExpRef);

typedef SExp *SExpPtr;
VECTOR_DEF(SExpPtr);

typedef struct {
    const char *type;
    void (*free)(void *self);
    void (*gcmark)(Interp *interp, SExpPtrVector *gcstack, void *self);
} LispUserdataMeta;

struct sexp {
    bool marked;
    SExpType type;
    union {
        int64_t integer;
        double real;
        bool boolean;
        char character;
        const char *str;
        struct {
            void *userdata;
            LispUserdataMeta *userdata_meta;
        };
        SExpPair pair;
        SExpFunc func;
        LispUserFunc userfunc;
        LispPrimitive primitive;
        SExpEnv env;
        SExpBinding binding;
        SExpMacro macro;
        SExpRef ret;
        SExpTailcall tailcall;
    };
};


void SExp_show(SExp self, FILE* fp);
void SExpRef_show(SExpRef self, FILE* fp);
void SExpPtr_show(SExpPtr self, FILE* fp);

VECTOR_DEF(SExp);

#endif

