#ifndef BAMBOO_LISP_INTERP_H_
#define BAMBOO_LISP_INTERP_H_

#include <stdbool.h>

#include <algds/hash_table.h>

#include "sexp.h"

typedef struct {
    bool gc_paused;
    SExpVector objs;
    IntVector empty_space;
    String2IntHashTable symbols;
    SExpRef stack;
    SExpRef evaluating;
    SExpRef top_level;
    SExpRef nil;
} Interp;

void Interp_init(Interp *self);
void Interp_free(Interp *self);
SExp* Interp_ref(Interp *self, SExpRef ref);
void Interp_gc(Interp *self);
void Interp_pause_gc(Interp *self);
void Interp_restart_gc(Interp *self);

SExpRef new_sexp(Interp *ctx);
SExpRef new_boolean(Interp *ctx, bool val);
SExpRef new_char(Interp *ctx, char val);
SExpRef new_integer(Interp *ctx, int64_t val);
SExpRef new_real(Interp *ctx, double val);
SExpRef new_string(Interp *ctx, const char *val);
SExpRef new_symbol(Interp *ctx, const char *val);
SExpRef cons(Interp *ctx, SExpRef car, SExpRef cdr);
SExpRef new_list1(Interp *ctx, SExpRef e1);
SExpRef new_list2(Interp *ctx, SExpRef e1, SExpRef e2);
SExpRef new_list3(Interp *ctx, SExpRef e1, SExpRef e2, SExpRef e3);
SExpRef new_list4(Interp *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4);
SExpRef new_list5(Interp *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4, SExpRef e5);

#endif

