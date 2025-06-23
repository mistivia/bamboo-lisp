#ifndef BAMBOO_LISP_BUILTINS_H_
#define BAMBOO_LISP_BUILTINS_H_

#include "interp.h"

// - char=
// - char>
// - char<
// - char>=
// - char<=
// - char/=
// - ord
// - chr

SExpRef builtin_string(Interp *interp, SExpRef args);
SExpRef builtin_string_eq(Interp *interp, SExpRef args);
SExpRef builtin_string_gt(Interp *interp, SExpRef args);
SExpRef builtin_string_lt(Interp *interp, SExpRef args);
SExpRef builtin_string_ge(Interp *interp, SExpRef args);
SExpRef builtin_string_le(Interp *interp, SExpRef args);
SExpRef builtin_string_neq(Interp *interp, SExpRef args);
SExpRef builtin_split_string(Interp *interp, SExpRef args);
SExpRef builtin_strip_string(Interp *interp, SExpRef args);
SExpRef builtin_symbol2string(Interp *interp, SExpRef args);
SExpRef builtin_intern(Interp *interp, SExpRef args);
SExpRef builtin_gensym(Interp *interp, SExpRef args);
SExpRef builtin_sqrt(Interp *interp, SExpRef args);
SExpRef builtin_cbrt(Interp *interp, SExpRef args);
SExpRef builtin_float(Interp *interp, SExpRef args);
SExpRef builtin_abs(Interp *interp, SExpRef args);
SExpRef builtin_pow(Interp *interp, SExpRef args);
SExpRef builtin_floor(Interp *interp, SExpRef args);
SExpRef builtin_truncate(Interp *interp, SExpRef args);
SExpRef builtin_ceiling(Interp *interp, SExpRef args);
SExpRef builtin_round(Interp *interp, SExpRef args);
SExpRef builtin_sin(Interp *interp, SExpRef args);
SExpRef builtin_cos(Interp *interp, SExpRef args);
SExpRef builtin_tan(Interp *interp, SExpRef args);
SExpRef builtin_asin(Interp *interp, SExpRef args);
SExpRef builtin_acos(Interp *interp, SExpRef args);
SExpRef builtin_atan(Interp *interp, SExpRef args);
SExpRef builtin_ln(Interp *interp, SExpRef args);
SExpRef builtin_log10(Interp *interp, SExpRef args);
SExpRef builtin_log2(Interp *interp, SExpRef args);
SExpRef builtin_exp(Interp *interp, SExpRef args);
SExpRef builtin_min(Interp *interp, SExpRef args);
SExpRef builtin_max(Interp *interp, SExpRef args);
SExpRef builtin_equal(Interp *interp, SExpRef args);
SExpRef builtin_eq(Interp *interp, SExpRef args);
SExpRef builtin_format(Interp *interp, SExpRef args);
SExpRef builtin_concat(Interp *interp, SExpRef args);
SExpRef builtin_print(Interp *interp, SExpRef args);
SExpRef builtin_exit(Interp *interp, SExpRef args);
SExpRef builtin_error(Interp *interp, SExpRef args);
SExpRef builtin_list(Interp *interp, SExpRef args);
SExpRef builtin_car(Interp *interp, SExpRef args);
SExpRef builtin_cdr(Interp *interp, SExpRef args);
SExpRef builtin_cons(Interp *interp, SExpRef args);
SExpRef builtin_not(Interp *interp, SExpRef args);
SExpRef builtin_add(Interp *interp, SExpRef args);
SExpRef builtin_sub(Interp *interp, SExpRef args);
SExpRef builtin_mul(Interp *interp, SExpRef args);
SExpRef builtin_div(Interp *interp, SExpRef args);
SExpRef builtin_idiv(Interp *interp, SExpRef args);
SExpRef builtin_mod(Interp *interp, SExpRef args);
SExpRef builtin_num_equal(Interp *interp, SExpRef args);
SExpRef builtin_num_neq(Interp *interp, SExpRef args);
SExpRef builtin_gt(Interp *interp, SExpRef args);
SExpRef builtin_lt(Interp *interp, SExpRef args);
SExpRef builtin_ge(Interp *interp, SExpRef args);
SExpRef builtin_le(Interp *interp, SExpRef args);
SExpRef builtin_princ(Interp *interp, SExpRef args);
SExpRef builtin_gcstat(Interp *interp, SExpRef args);
SExpRef builtin_alwaysgc(Interp *interp, SExpRef args);

#endif
