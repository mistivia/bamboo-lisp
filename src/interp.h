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
    SExpVector objs;
    PrimitiveEntryVector primitives;
    IntVector empty_space;
    String2IntHashTable symbols;
    SExpRef stack;
    SExpRef reg;
    SExpRef top_level;
    SExpRef nil;
    char *errmsg_buf;
};


void Interp_init(Interp *self);
void Interp_free(Interp *self);
SExp* Interp_ref(Interp *self, SExpRef ref);
void Interp_gc(Interp *self, SExpRef tmp_root);
void Interp_add_primitive(Interp *self, const char *name, LispPrimitive fn);

SExpRef primitive_list(Interp *interp, SExpRef sexp);
SExpRef primitive_progn(Interp *interp, SExpRef sexp);
SExpRef primitive_setq(Interp *interp, SExpRef sexp);
SExpRef primitive_let(Interp *interp, SExpRef sexp);
SExpRef primitive_car(Interp *interp, SExpRef sexp);
SExpRef primitive_cdr(Interp *interp, SExpRef sexp);
SExpRef primitive_cons(Interp *interp, SExpRef sexp);
SExpRef primitive_add(Interp *interp, SExpRef sexp);
SExpRef primitive_sub(Interp *interp, SExpRef sexp);

void lisp_print(Interp *interp, SExpRef obj, FILE *fp);
SExpRef lisp_lookup(Interp *interp, const char *name);
SExpRef lisp_lookup_func(Interp *interp, const char *name);
SExpRef lisp_cons(Interp *interp, SExpRef a, SExpRef b);
SExpRef lisp_dup(Interp *interp, SExpRef arg);
bool lisp_nilp(Interp *interp, SExpRef arg);
SExpRef lisp_car(Interp *interp, SExpRef arg);
SExpRef lisp_cdr(Interp *interp, SExpRef arg);
SExpRef lisp_eval(Interp *interp, SExpRef arg);
SExpRef lisp_eval_args(Interp *interp, SExpRef args);
SExpRef lisp_add(Interp *interp, SExpRef args);
SExpRef lisp_sub(Interp *interp, SExpRef args);
SExpRef lisp_mul(Interp *interp, SExpRef args);
SExpRef lisp_div(Interp *interp, SExpRef args);

SExpRef new_error(Interp *interp, const char *format, ...);
SExpRef new_sexp(Interp *ctx);
SExpRef new_boolean(Interp *ctx, bool val);
SExpRef new_char(Interp *ctx, char val);
SExpRef new_integer(Interp *ctx, int64_t val);
SExpRef new_real(Interp *ctx, double val);
SExpRef new_string(Interp *ctx, const char *val);
SExpRef new_symbol(Interp *ctx, const char *val);
SExpRef new_env(Interp *ctx);
SExpRef new_binding(Interp *ctx, SExpRef name, SExpRef val);
SExpRef new_list1(Interp *ctx, SExpRef e1);
SExpRef new_list2(Interp *ctx, SExpRef e1, SExpRef e2);
SExpRef new_list3(Interp *ctx, SExpRef e1, SExpRef e2, SExpRef e3);
SExpRef new_list4(Interp *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4);
SExpRef new_list5(Interp *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4, SExpRef e5);

#endif

