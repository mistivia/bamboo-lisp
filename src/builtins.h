#ifndef BAMBOO_LISP_BUILTINS_H_
#define BAMBOO_LISP_BUILTINS_H_

#include "interp.h"

SExpRef builtin_list(Interp *interp, SExpRef sexp);
SExpRef builtin_car(Interp *interp, SExpRef sexp);
SExpRef builtin_cdr(Interp *interp, SExpRef sexp);
SExpRef builtin_cons(Interp *interp, SExpRef sexp);

SExpRef builtin_add(Interp *interp, SExpRef sexp);
SExpRef builtin_sub(Interp *interp, SExpRef sexp);

SExpRef builtin_num_equal(Interp *interp, SExpRef sexp);
SExpRef builtin_gt(Interp *interp, SExpRef sexp);
SExpRef builtin_lt(Interp *interp, SExpRef sexp);
SExpRef builtin_ge(Interp *interp, SExpRef sexp);
SExpRef builtin_le(Interp *interp, SExpRef sexp);

#endif
