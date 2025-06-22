#ifndef BAMBOO_LISP_BUILTINS_H_
#define BAMBOO_LISP_BUILTINS_H_

#include "interp.h"


SExpRef builtin_sqrt(Interp *interp, SExpRef sexp);
SExpRef builtin_cbrt(Interp *interp, SExpRef sexp);
SExpRef builtin_float(Interp *interp, SExpRef sexp);
SExpRef builtin_abs(Interp *interp, SExpRef sexp);
SExpRef builtin_pow(Interp *interp, SExpRef sexp);
SExpRef builtin_floor(Interp *interp, SExpRef sexp);
SExpRef builtin_truncate(Interp *interp, SExpRef sexp);
SExpRef builtin_ceiling(Interp *interp, SExpRef sexp);
SExpRef builtin_round(Interp *interp, SExpRef sexp);
SExpRef builtin_sin(Interp *interp, SExpRef sexp);
SExpRef builtin_cos(Interp *interp, SExpRef sexp);
SExpRef builtin_tan(Interp *interp, SExpRef sexp);
SExpRef builtin_asin(Interp *interp, SExpRef sexp);
SExpRef builtin_acos(Interp *interp, SExpRef sexp);
SExpRef builtin_atan(Interp *interp, SExpRef sexp);
SExpRef builtin_ln(Interp *interp, SExpRef sexp);
SExpRef builtin_log10(Interp *interp, SExpRef sexp);
SExpRef builtin_log2(Interp *interp, SExpRef sexp);
SExpRef builtin_exp(Interp *interp, SExpRef sexp);
SExpRef builtin_min(Interp *interp, SExpRef sexp);
SExpRef builtin_max(Interp *interp, SExpRef sexp);
SExpRef builtin_equal(Interp *interp, SExpRef sexp);
SExpRef builtin_eq(Interp *interp, SExpRef sexp);
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
