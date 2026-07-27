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
    // Cache of the fully macro-expanded body, tagged with the interpreter
    // version at which it was computed. Rebuilt lazily on apply whenever the
    // version no longer matches. cache_version < 0 means "no cache yet".
    SExpRef body_cache;
    int32_t cache_version;
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

// Paged object heap.
//
// Objects live in fixed-size chunks that are allocated once and never moved.
// Growing the heap only allocates a fresh chunk (and possibly reallocates the
// small array of chunk pointers), so a `SExp *` obtained from SExpHeap_ref
// stays valid for the lifetime of the object even across later allocations.
// This is what makes it safe to hold a `SExp *` across a call that allocates
// (a flat, reallocating vector would move every object and dangle the pointer).
#define SEXP_HEAP_CHUNK 4096

typedef struct {
    SExp **chunks;      // array of chunk base pointers
    int chunk_count;    // number of chunks allocated
    int chunk_cap;      // capacity of the `chunks` array
    int size;           // number of slots handed out so far
} SExpHeap;

void SExpHeap_init(SExpHeap *heap);
void SExpHeap_free(SExpHeap *heap);
// Append `value` and return its index.
int SExpHeap_push(SExpHeap *heap, SExp value);

static inline SExp *SExpHeap_ref(SExpHeap *heap, int idx) {
    return &heap->chunks[idx / SEXP_HEAP_CHUNK][idx % SEXP_HEAP_CHUNK];
}

static inline int SExpHeap_len(SExpHeap *heap) {
    return heap->size;
}

#endif

