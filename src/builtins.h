#ifndef BAMBOO_LISP_BUILTINS_H_
#define BAMBOO_LISP_BUILTINS_H_

#include "interp.h"

SExpRef builtin_format(Interp *interp, SExpRef sexp);
SExpRef builtin_concat(Interp *interp, SExpRef sexp);
SExpRef builtin_print(Interp *interp, SExpRef sexp);
SExpRef builtin_exit(Interp *interp, SExpRef sexp);
SExpRef builtin_error(Interp *interp, SExpRef sexp);
SExpRef builtin_list(Interp *interp, SExpRef sexp);
SExpRef builtin_car(Interp *interp, SExpRef sexp);
SExpRef builtin_cdr(Interp *interp, SExpRef sexp);
SExpRef builtin_cons(Interp *interp, SExpRef sexp);
SExpRef builtin_not(Interp *interp, SExpRef sexp);
SExpRef builtin_add(Interp *interp, SExpRef sexp);
SExpRef builtin_sub(Interp *interp, SExpRef sexp);
SExpRef builtin_mul(Interp *interp, SExpRef sexp);
SExpRef builtin_div(Interp *interp, SExpRef sexp);
SExpRef builtin_idiv(Interp *interp, SExpRef sexp);
SExpRef builtin_mod(Interp *interp, SExpRef sexp);
SExpRef builtin_num_equal(Interp *interp, SExpRef sexp);
SExpRef builtin_num_neq(Interp *interp, SExpRef sexp);
SExpRef builtin_gt(Interp *interp, SExpRef sexp);
SExpRef builtin_lt(Interp *interp, SExpRef sexp);
SExpRef builtin_ge(Interp *interp, SExpRef sexp);
SExpRef builtin_le(Interp *interp, SExpRef sexp);
SExpRef builtin_princ(Interp *interp, SExpRef sexp);
SExpRef builtin_gcstat(Interp *interp, SExpRef sexp);

#endif
