#ifndef BAMBOO_LISP_INTERP_H_
#define BAMBOO_LISP_INTERP_H_

#include <stdbool.h>

#include <algds/hash_table.h>

#include "algds/vec.h"
#include "sexp.h"


struct interp;
typedef struct interp Interp;

typedef SExpRef (*LispPrimitive)(Interp *interp, SExpRef sexp);

typedef struct {
    const char *name;
    LispPrimitive fn;
} PrimitiveEntry;

VECTOR_DEF(PrimitiveEntry);

struct interp {
    bool gc_paused;
    SExpVector objs;
    PrimitiveEntryVector primitives;
    IntVector empty_space;
    String2IntHashTable symbols;
    SExpRef stack;
    SExpRef evaluating;
    SExpRef top_level;
    SExpRef nil;
    char *errmsg_buf;
};


void Interp_init(Interp *self);
void Interp_free(Interp *self);
SExp* Interp_ref(Interp *self, SExpRef ref);
void Interp_gc(Interp *self);
void Interp_pause_gc(Interp *self);
void Interp_restart_gc(Interp *self);
void Interp_add_primitive(Interp *self, const char *name, LispPrimitive fn);

SExpRef primitive_car(Interp *interp, SExpRef sexp);
SExpRef primitive_cdr(Interp *interp, SExpRef sexp);
SExpRef primitive_cons(Interp *interp, SExpRef sexp);
SExpRef primitive_add(Interp *interp, SExpRef sexp);
SExpRef primitive_sub(Interp *interp, SExpRef sexp);

SExpRef lisp_cons(Interp *ctx, SExpRef car, SExpRef cdr);
SExpRef lisp_dup(Interp *ctx, SExpRef val);
SExpRef lisp_car(Interp *ctx, SExpRef val);
SExpRef lisp_cdr(Interp *ctx, SExpRef val);
SExpRef lisp_eval(Interp *ctx, SExpRef val);
SExpRef lisp_eval_args(Interp *ctx, SExpRef val);
SExpRef lisp_add(Interp *ctx, SExpRef lhs, SExpRef rhs);
SExpRef lisp_sub(Interp *ctx, SExpRef lhs, SExpRef rhs);
SExpRef lisp_mul(Interp *ctx, SExpRef lhs, SExpRef rhs);
SExpRef lisp_div(Interp *ctx, SExpRef lhs, SExpRef rhs);

SExpRef new_error(Interp *interp, const char *format, ...);
SExpRef new_sexp(Interp *ctx);
SExpRef new_boolean(Interp *ctx, bool val);
SExpRef new_char(Interp *ctx, char val);
SExpRef new_integer(Interp *ctx, int64_t val);
SExpRef new_real(Interp *ctx, double val);
SExpRef new_string(Interp *ctx, const char *val);
SExpRef new_symbol(Interp *ctx, const char *val);
SExpRef new_list1(Interp *ctx, SExpRef e1);
SExpRef new_list2(Interp *ctx, SExpRef e1, SExpRef e2);
SExpRef new_list3(Interp *ctx, SExpRef e1, SExpRef e2, SExpRef e3);
SExpRef new_list4(Interp *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4);
SExpRef new_list5(Interp *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4, SExpRef e5);

#endif

