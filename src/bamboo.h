#ifndef BAMBOO_LISP_BAMBOO_H_
#define BAMBOO_LISP_BAMBOO_H_

#include <algds/hash_table.h>

#include "sexp.h"

typedef struct {
    SExpVector objs;
    String2IntHashTable symbols;
} Bamboo;

void Bamboo_init(Bamboo *self);
SExp* Bamboo_ref(Bamboo *self, SExpRef ref);
// TODO: Heap_gc()

SExpRef new_integer(Bamboo *ctx, int64_t val);
SExpRef new_real(Bamboo *ctx, double val);
SExpRef new_string(Bamboo *ctx, const char *val);
SExpRef new_symbol(Bamboo *ctx, const char *val);
SExpRef cons(Bamboo *ctx, SExpRef car, SExpRef cdr);
SExpRef nil(Bamboo *ctx);
SExpRef new_list1(Bamboo *ctx, SExpRef e1);
SExpRef new_list2(Bamboo *ctx, SExpRef e1, SExpRef e2);
SExpRef new_list3(Bamboo *ctx, SExpRef e1, SExpRef e2, SExpRef e3);
SExpRef new_list4(Bamboo *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4);
SExpRef new_list5(Bamboo *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4, SExpRef e5);
SExpRef new_list6(Bamboo *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4, SExpRef e5, SExpRef e6);
SExpRef new_list7(Bamboo *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4, SExpRef e5, SExpRef e6, SExpRef e7);

#endif

