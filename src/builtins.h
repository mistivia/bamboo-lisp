#ifndef BAMBOO_LISP_BUILTINS_H_
#define BAMBOO_LISP_BUILTINS_H_

#include "interp.h"

SExpRef builtin_logand(Interp *interp, SExpRef args);
SExpRef builtin_logior(Interp *interp, SExpRef args);
SExpRef builtin_logxor(Interp *interp, SExpRef args);
SExpRef builtin_lognot(Interp *interp, SExpRef args);
SExpRef builtin_lsh(Interp *interp, SExpRef args);
SExpRef builtin_ash(Interp *interp, SExpRef args);

SExpRef builtin_numberp(Interp *interp, SExpRef args);
SExpRef builtin_integerp(Interp *interp, SExpRef args);
SExpRef builtin_functionp(Interp *interp, SExpRef args);
SExpRef builtin_charp(Interp *interp, SExpRef args);
SExpRef builtin_listp(Interp *interp, SExpRef args);
SExpRef builtin_consp(Interp *interp, SExpRef args);
SExpRef builtin_atomp(Interp *interp, SExpRef args);
SExpRef builtin_nullp(Interp *interp, SExpRef args);
SExpRef builtin_floatp(Interp *interp, SExpRef args);

SExpRef builtin_char_eq(Interp *interp, SExpRef args);
SExpRef builtin_char_gt(Interp *interp, SExpRef args);
SExpRef builtin_char_lt(Interp *interp, SExpRef args);
SExpRef builtin_char_ge(Interp *interp, SExpRef args);
SExpRef builtin_char_le(Interp *interp, SExpRef args);
SExpRef builtin_char_neq(Interp *interp, SExpRef args);
SExpRef builtin_int2char(Interp *interp, SExpRef args);
SExpRef builtin_char2int(Interp *interp, SExpRef args);
SExpRef builtin_numericp(Interp *interp, SExpRef args);
SExpRef builtin_alphabeticp(Interp *interp, SExpRef args);
SExpRef builtin_alphanump(Interp *interp, SExpRef args);

SExpRef builtin_string(Interp *interp, SExpRef args);
SExpRef builtin_string_eq(Interp *interp, SExpRef args);
SExpRef builtin_string_gt(Interp *interp, SExpRef args);
SExpRef builtin_string_lt(Interp *interp, SExpRef args);
SExpRef builtin_string_ge(Interp *interp, SExpRef args);
SExpRef builtin_string_le(Interp *interp, SExpRef args);
SExpRef builtin_string_neq(Interp *interp, SExpRef args);
SExpRef builtin_split_string(Interp *interp, SExpRef args);
SExpRef builtin_strip_string(Interp *interp, SExpRef args);
SExpRef builtin_format(Interp *interp, SExpRef args);
SExpRef builtin_concat(Interp *interp, SExpRef args);
SExpRef builtin_print(Interp *interp, SExpRef args);
SExpRef builtin_princ(Interp *interp, SExpRef args);

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

SExpRef builtin_list(Interp *interp, SExpRef args);
SExpRef builtin_setnth(Interp *interp, SExpRef args);
SExpRef builtin_setnthcdr(Interp *interp, SExpRef args);
SExpRef builtin_foldl(Interp *interp, SExpRef args);
SExpRef builtin_append(Interp *interp, SExpRef args);
SExpRef builtin_nconc(Interp *interp, SExpRef args);
SExpRef builtin_memberp(Interp *interp, SExpRef args);
SExpRef builtin_nreverse(Interp *interp, SExpRef args);
SExpRef builtin_reverse(Interp *interp, SExpRef args);
SExpRef builtin_last(Interp *interp, SExpRef args);
SExpRef builtin_map(Interp *interp, SExpRef args);
SExpRef builtin_filter(Interp *interp, SExpRef args);
SExpRef builtin_remove(Interp *interp, SExpRef args);
SExpRef builtin_count(Interp *interp, SExpRef args);
SExpRef builtin_foreach(Interp *interp, SExpRef args);
SExpRef builtin_set_car(Interp *interp, SExpRef args);
SExpRef builtin_set_cdr(Interp *interp, SExpRef args);
SExpRef builtin_length(Interp *interp, SExpRef args);
SExpRef builtin_nth(Interp *interp, SExpRef args);
SExpRef builtin_nthcdr(Interp *interp, SExpRef args);
SExpRef builtin_car(Interp *interp, SExpRef args);
SExpRef builtin_cdr(Interp *interp, SExpRef args);
SExpRef builtin_cons(Interp *interp, SExpRef args);

SExpRef builtin_symbol2string(Interp *interp, SExpRef args);
SExpRef builtin_intern(Interp *interp, SExpRef args);
SExpRef builtin_gensym(Interp *interp, SExpRef args);

SExpRef builtin_not(Interp *interp, SExpRef args);
SExpRef builtin_equal(Interp *interp, SExpRef args);
SExpRef builtin_eq(Interp *interp, SExpRef args);

SExpRef builtin_exit(Interp *interp, SExpRef args);
SExpRef builtin_error(Interp *interp, SExpRef args);
SExpRef builtin_throw(Interp *interp, SExpRef args);

SExpRef builtin_gcstat(Interp *interp, SExpRef args);
SExpRef builtin_alwaysgc(Interp *interp, SExpRef args);

#endif
