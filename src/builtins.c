#include "builtins.h"
#include "interp.h"
#include "sexp.h"
#include <algds/str.h>
#include <stdint.h>
#include <float.h>
#include <math.h>

SExpRef builtin_set_car(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) {
        return new_error(interp, "set-car: args num error.\n");
    }
    SExpRef lst = CAR(args), elem = CADR(args);
    if (VALTYPE(lst) != kPairSExp) {
        return new_error(interp, "set-car: type error.");
    }
    REF(lst)->pair.car = elem;
    return NIL;
}

SExpRef builtin_set_cdr(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) {
        return new_error(interp, "set-cdr: args num error.\n");
    }
    SExpRef lst = CAR(args), elem = CADR(args);
    if (VALTYPE(lst) != kPairSExp) {
        return new_error(interp, "set-cdr: type error.");
    }
    REF(lst)->pair.cdr = elem;
    return NIL;
}

SExpRef builtin_length(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 1) {
        return new_error(interp, "length: args num error.\n");
    }
    int len = LENGTH(CAR(args));
    if (len < 0) {
        return new_error(interp, "length: type error.\n");
    }
    return new_integer(interp, len);
}

SExpRef builtin_nth(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) {
        return new_error(interp, "nth: args num error.\n");
    }
    SExpRef n = CAR(args), lst = CADR(args);
    if (VALTYPE(n) != kIntegerSExp) return new_error(interp, "nth: type error.\n");
    if (VALTYPE(lst) == kPairSExp) {
        if (REF(n)->integer >= LENGTH(lst)) {
            return new_error(interp, "nth: out of bound.\n");
        }
        for (int i = 0; i < REF(n)->integer; i++) {
            lst = CDR(lst);
        }
        return CAR(lst);
    } else if (VALTYPE(lst) == kStringSExp) {
        if (REF(n)->integer >= strlen(REF(lst)->str)) {
            return new_error(interp, "nth: out of bound\n");
        }
        return new_char(interp, REF(lst)->str[REF(n)->integer]);
    } else {
        return new_error(interp, "nth: type error.\n");
    }
}

SExpRef builtin_nthcdr(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) {
        return new_error(interp, "nth: args num error.\n");
    }
    SExpRef n = CAR(args), lst = CADR(args);
    if (VALTYPE(n) != kIntegerSExp) return new_error(interp, "nth: type error.\n");
    if (VALTYPE(lst) == kPairSExp) {
        if (REF(n)->integer >= LENGTH(lst)) {
            return new_error(interp, "nth: out of bound.\n");
        }
        for (int i = 0; i < REF(n)->integer; i++) {
            lst = CDR(lst);
        }
        return CDR(lst);
    } else {
        return new_error(interp, "nth: type error.\n");
    }
}

SExpRef builtin_string(Interp *interp, SExpRef args) {
    for (SExpRef i = args; !NILP(i); i = CDR(i)) {
        SExpRef x = CAR(i);
        if (VALTYPE(x) != kIntegerSExp && VALTYPE(x) != kCharSExp) {
            return new_error(interp, "string: type error.\n");    
        }
    }
    str_builder_t sb;
    init_str_builder(&sb);
    for (SExpRef i = args; !NILP(i); i = CDR(i)) {
        SExpRef x = CAR(i);
        if (VALTYPE(x) == kIntegerSExp) {
            str_builder_append_char(&sb, REF(x)->integer);
        } else {
            str_builder_append_char(&sb, REF(x)->character);
        }
    }
    str_builder_append_char(&sb, '\0');
    SExpRef ret = new_string(interp, sb.buf);
    free(sb.buf);
    return ret;
}

SExpRef builtin_string_eq(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "string=: arg num error.\n");
    SExpRef s1 = CAR(args), s2 = CADR(args);
    if (VALTYPE(s1) != kStringSExp || VALTYPE(s2) != kStringSExp) {
        return new_error(interp, "string=: type error.\n");
    }
    return new_boolean(interp, strcmp(REF(s1)->str, REF(s2)->str) == 0);
}

SExpRef builtin_string_gt(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "string>: arg num error.\n");
    SExpRef s1 = CAR(args), s2 = CADR(args);
    if (VALTYPE(s1) != kStringSExp || VALTYPE(s2) != kStringSExp) {
        return new_error(interp, "string>: type error.\n");
    }
    return new_boolean(interp, strcmp(REF(s1)->str, REF(s2)->str) > 0);

}

SExpRef builtin_string_lt(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "string<: arg num error.\n");
    SExpRef s1 = CAR(args), s2 = CADR(args);
    if (VALTYPE(s1) != kStringSExp || VALTYPE(s2) != kStringSExp) {
        return new_error(interp, "string<: type error.\n");
    }
    return new_boolean(interp, strcmp(REF(s1)->str, REF(s2)->str) < 0);
}

SExpRef builtin_string_ge(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "string>=: arg num error.\n");
    SExpRef s1 = CAR(args), s2 = CADR(args);
    if (VALTYPE(s1) != kStringSExp || VALTYPE(s2) != kStringSExp) {
        return new_error(interp, "string>=: type error.\n");
    }
    return new_boolean(interp, strcmp(REF(s1)->str, REF(s2)->str) >= 0);
}

SExpRef builtin_string_le(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "string<=: arg num error.\n");
    SExpRef s1 = CAR(args), s2 = CADR(args);
    if (VALTYPE(s1) != kStringSExp || VALTYPE(s2) != kStringSExp) {
        return new_error(interp, "string<=: type error.\n");
    }
    return new_boolean(interp, strcmp(REF(s1)->str, REF(s2)->str) <= 0);
}

SExpRef builtin_string_neq(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "string/=: arg num error.\n");
    SExpRef s1 = CAR(args), s2 = CADR(args);
    if (VALTYPE(s1) != kStringSExp || VALTYPE(s2) != kStringSExp) {
        return new_error(interp, "string/=: type error.\n");
    }
    return new_boolean(interp, strcmp(REF(s1)->str, REF(s2)->str) != 0);
}

SExpRef builtin_split_string(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "split-string: arg num error.\n");
    SExpRef s1 = CAR(args), s2 = CADR(args);
    if (VALTYPE(s1) != kStringSExp || VALTYPE(s2) != kCharSExp) {
        return new_error(interp, "split-string: type error.\n");
    }
    char **ss;
    ss = str_split((char*)REF(s1)->str, REF(s2)->character);
    SExpRef lst = NIL;
    for (char **i = ss; *i != NULL; i++) {
        lst = CONS(new_string(interp, *i), lst);
    }
    destroy_str_list(ss);
    return lisp_nreverse(interp, lst);
}

SExpRef builtin_strip_string(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 1) return new_error(interp, "strip-string: arg num error.\n");
    SExpRef s = CAR(args);
    if (VALTYPE(s) != kStringSExp) return new_error(interp, "strip-string: type error.\n");    
    char *news = str_strip((char*)REF(s)->str);
    SExpRef ret = new_string(interp, news);
    free(news);
    return ret;
}

SExpRef builtin_alwaysgc(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 1) return new_error(interp, "_alwaysgc: arg num error.\n");
    SExpRef arg = CAR(args);
    if (VALTYPE(arg) != kBooleanSExp) return new_error(interp, "alwaysgc: type error.\n");
    interp->alwaysgc = REF(arg)->boolean;
    return NIL;
}

SExpRef builtin_symbol2string(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 1) return new_error(interp, "symbol->string: arg num error.\n");
    SExpRef arg = CAR(args);
    if (VALTYPE(arg) != kSymbolSExp) return new_error(interp, "symbol->string: type error.\n");
    return new_string(interp, REF(arg)->str);
}

SExpRef builtin_intern(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 1) return new_error(interp, "intern: arg num error.\n");
    SExpRef arg = CAR(args);
    if (VALTYPE(arg) != kStringSExp) return new_error(interp, "intern: type error.\n");
    return new_symbol(interp, REF(arg)->str);
}

SExpRef builtin_gensym(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 0) return new_error(interp, "gensym: no arg.\n");
    char buf[16];
    snprintf(buf, 16, "sYyYm%d", interp->gensym_cnt);
    interp->gensym_cnt++;
    return new_symbol(interp, buf);
}

SExpRef builtin_float(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 1) return new_error(interp, "float: expect 1 arg.\n");
    SExpRef x = CAR(args);
    if (VALTYPE(x) != kIntegerSExp) return new_error(interp, "float: wrong type.\n");
    return new_real(interp, REF(x)->integer);
}

SExpRef builtin_abs(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 1) return new_error(interp, "abs: expect 1 arg.\n");
    SExpRef x = CAR(args);
    if (VALTYPE(x) != kIntegerSExp && VALTYPE(x) != kRealSExp) {
        return new_error(interp, "abs: wrong type.\n");
    }
    if (VALTYPE(x) == kIntegerSExp) {
        int64_t val = REF(x)->integer;
        if (val < 0) val = -val;
        return new_integer(interp, val);
    } else {
        double val = REF(x)->real;
        if (val < 0) val = -val;
        return new_real(interp, val);
    }
}

static double real_value(Interp *interp, SExpRef x) {
    if (VALTYPE(x) == kIntegerSExp) {
        return REF(x)->integer;
    } else {
        return REF(x)->real;
    }
}    

SExpRef builtin_pow(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "pow: expect 2 args.\n");
    SExpRef x = CAR(args), y = CADR(args);
    if (VALTYPE(x) != kIntegerSExp && VALTYPE(x) != kRealSExp) {
        return new_error(interp, "pow: wrong type.\n");
    }
    if (VALTYPE(y) != kIntegerSExp && VALTYPE(y) != kRealSExp) {
        return new_error(interp, "pow: wrong type.\n");
    }
    return new_real(interp, pow(real_value(interp, x), real_value(interp, y)));
}

#define GEN_MATH_FUNC(name, cfunc) \
SExpRef builtin_##name(Interp *interp, SExpRef args) { \
    if (LENGTH(args) != 1) return new_error(interp, #name": expect 1 args.\n"); \
    SExpRef x = CAR(args); \
    if (VALTYPE(x) != kIntegerSExp && VALTYPE(x) != kRealSExp) { \
        return new_error(interp, #name": wrong type.\n"); \
    } \
    return new_real(interp, cfunc(real_value(interp, x))); \
}

GEN_MATH_FUNC(sqrt, sqrt);
GEN_MATH_FUNC(cbrt, cbrt);
GEN_MATH_FUNC(floor, floor);
GEN_MATH_FUNC(truncate, trunc);
GEN_MATH_FUNC(ceiling, ceil);
GEN_MATH_FUNC(round, round);
GEN_MATH_FUNC(sin, sin);
GEN_MATH_FUNC(cos, cos);
GEN_MATH_FUNC(tan, tan);
GEN_MATH_FUNC(asin, asin);
GEN_MATH_FUNC(acos, acos);
GEN_MATH_FUNC(atan, atan);
GEN_MATH_FUNC(ln, log);
GEN_MATH_FUNC(log10, log10);
GEN_MATH_FUNC(log2, log2);
GEN_MATH_FUNC(exp, exp);

SExpRef builtin_min(Interp *interp, SExpRef args) {
    if (LENGTH(args) < 1) return new_error(interp, "min: wrong arg number.\n");
    bool hasReal = false;
    FOREACH(iter, args) {
        SExpRef x = CAR(iter);
        if (VALTYPE(x) == kRealSExp) hasReal = true;
        if (VALTYPE(x) != kRealSExp && VALTYPE(x) != kIntegerSExp) {
            return new_error(interp, "min: wrong type.\n");
        }
    }
    if (hasReal) {
        double min = DBL_MAX;
        FOREACH(iter, args) {
            SExpRef x = CAR(iter);
            if (VALTYPE(x) == kIntegerSExp) {
                if (REF(x)->integer < min) {
                    min = REF(x)->integer;
                }
            }
            if (VALTYPE(x) == kRealSExp) {
                if (REF(x)->real < min) {
                    min = REF(x)->real;
                }
            }
        }
        return new_integer(interp, min);
    } else {
        int64_t min = INT64_MAX;
        FOREACH(iter, args) {
            SExpRef x = CAR(iter);
            if (VALTYPE(x) == kIntegerSExp) {
                if (REF(x)->integer < min) {
                    min = REF(x)->integer;
                }
            }
        }
        return new_integer(interp, min);
    }
}

SExpRef builtin_max(Interp *interp, SExpRef args) {
    if (LENGTH(args) < 1) return new_error(interp, "min: wrong arg number.\n");
    bool hasReal = false;
    FOREACH(iter, args) {
        SExpRef x = CAR(iter);
        if (VALTYPE(x) == kRealSExp) hasReal = true;
        if (VALTYPE(x) != kRealSExp && VALTYPE(x) != kIntegerSExp) {
            return new_error(interp, "min: wrong type.\n");
        }
    }
    if (hasReal) {
        double max = -DBL_MAX;
        FOREACH(iter, args) {
            SExpRef x = CAR(iter);
            if (VALTYPE(x) == kIntegerSExp) {
                if (REF(x)->integer > max) {
                    max = REF(x)->integer;
                }
            }
            if (VALTYPE(x) == kRealSExp) {
                if (REF(x)->real > max) {
                    max = REF(x)->real;
                }
            }
        }
        return new_real(interp, max);
    } else {
        int64_t max = INT64_MIN;
        FOREACH(iter, args) {
            SExpRef x = CAR(iter);
            if (VALTYPE(x) == kIntegerSExp) {
                if (REF(x)->integer > max) {
                    max = REF(x)->integer;
                }
            }
        }
        return new_integer(interp, max);
    }
}

static bool equal_impl(Interp *interp, SExpRef x, SExpRef y) {
    if (VALTYPE(x) != VALTYPE(y)) return false;
    if (VALTYPE(x) == kIntegerSExp) {
        return REF(x)->integer== REF(y)->integer;
    } else if (VALTYPE(x) == kRealSExp) {
        return REF(x)->real == REF(y)->real;
    } else if (VALTYPE(x) == kStringSExp) {
        return strcmp(REF(x)->str, REF(y)->str) == 0;
    } else if (VALTYPE(x) == kPairSExp) {
        return equal_impl(interp, REF(x)->pair.car, REF(y)->pair.car)
                && equal_impl(interp, REF(x)->pair.cdr, REF(y)->pair.cdr);
    } else if (VALTYPE(x) == kCharSExp) {
        return REF(x)->character == REF(y)->character;
    } else if (VALTYPE(x) == kUserDataSExp) {
        return REF(x)->userdata == REF(y)->userdata;
    }
    return x.idx == y.idx;
}

SExpRef builtin_eq(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "eq: expect 2 args.\n");
    SExpRef x = CAR(args), y = CADR(args);
    if (VALTYPE(x) != VALTYPE(y)) return new_boolean(interp, false);
    if (VALTYPE(x) == kIntegerSExp
            || VALTYPE(x) == kCharSExp 
            || VALTYPE(x) == kRealSExp) {
        return new_boolean(interp, equal_impl(interp, x ,y));
    }
    return new_boolean(interp, x.idx == y.idx);
}


SExpRef builtin_equal(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "equal: expect 2 args.\n");
    SExpRef x = CAR(args), y = CADR(args);
    return new_boolean(interp, equal_impl(interp, x, y));
}

SExpRef builtin_format(Interp *interp, SExpRef args) {
    if (NILP(args)) {
        return new_error(interp, "format: too few arguments (missing format string).\n");
    }

    SExpRef format_string_sexp = CAR(args);
    SExpRef format_args = CDR(args);

    if (REF(format_string_sexp)->type != kStringSExp) {
        return new_error(interp, "format: first argument must be a string.\n");
    }

    const char *format_str = REF(format_string_sexp)->str;
    str_builder_t sb;
    SExpRef ret;
    init_str_builder(&sb);

    SExpRef current_format_arg = format_args;
    for (int i = 0; format_str[i] != '\0'; ++i) {
        if (format_str[i] == '%' && format_str[i+1] == 's') {
            if (NILP(current_format_arg)) {
                ret = new_error(interp, "format: wrong argument number.\n");
                goto end;
            } else {
                SExpRef s_arg = CAR(current_format_arg);
                if (REF(s_arg)->type != kStringSExp) {
                    const char *s = lisp_to_string(interp, s_arg);
                    str_builder_append(&sb, "%s", s);
                    free((void*)s);
                } else {
                    str_builder_append(&sb, "%s", REF(s_arg)->str);
                }
                current_format_arg = CDR(current_format_arg);
                i++;
            }
        } else if (format_str[i] == '%' && format_str[i+1] == '%') {
            str_builder_append_char(&sb, '%');
            i++;
        } else if (format_str[i] == '%') {
            ret = new_error(interp, "format: only %%s is supported.\n");
            goto end;
        } else {
            str_builder_append_char(&sb, format_str[i]);
        }
    }
    if (!NILP(current_format_arg)) {
        ret = new_error(interp, "format: wrong argument number.\n");
        goto end;
    }

    str_builder_append_char(&sb, '\0');
    ret = new_string(interp, sb.buf);
end:
    free(sb.buf);
    return ret;
}

SExpRef builtin_concat(Interp *interp, SExpRef args) {
    SExpRef cur = args;
    while (!NILP(cur)) {
        if (REF(CAR(cur))->type != kStringSExp) {
            return new_error(interp, "concat: wrong type.\n");
        }
        cur = CDR(cur);
    }
    str_builder_t sb;
    init_str_builder(&sb);
    cur = args;
    while (!NILP(cur)) {
        SExpRef s = CAR(cur);
        str_builder_append(&sb, "%s", REF(s)->str);
        cur = CDR(cur);
    }
    str_builder_append_char(&sb, '\0');
    SExpRef ret = new_string(interp, sb.buf);
    free(sb.buf);
    return ret;
}

SExpRef builtin_exit(Interp *interp, SExpRef args) {
    if (LENGTH(args) == 0) {
        Interp_free(interp);
        exit(0);
    }
    if (LENGTH(args) == 1) {
        SExpRef x = CAR(args);
        if (VALTYPE(x) != kIntegerSExp) goto error;
        int retcode = REF(x)->integer;
        Interp_free(interp);
        exit(retcode);
    }
error:
    return new_error(interp, "exit: argument error.\n");
}

SExpRef builtin_error(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 1) return new_error(interp, "err.\n");
    if (VALTYPE(CAR(args)) == kStringSExp || VALTYPE(CAR(args)) == kSymbolSExp) {
        return new_error(interp, "%s\n", REF(CAR(args))->str);
    }
    const char *str = lisp_to_string(interp, CAR(args));
    SExpRef ret = new_error(interp, "%s\n", REF(CAR(args))->str);
    free((void*)str);
    return ret;
}

SExpRef builtin_list(Interp *interp, SExpRef args) {
    return args;
}

SExpRef builtin_car(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 1) {
        return new_error(interp, "car: wrong argument number.\n");
    }
    if (CTL_FL(args)) return args;
    return CAR(CAR(args));
}

SExpRef builtin_princ(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 1) {
        return new_error(interp, "show wrong argument number.\n");
    }
    if (VALTYPE(CAR(args)) == kStringSExp) {
        printf("%s", REF(CAR(args))->str);
        return NIL;
    }
    const char *s = lisp_to_string(interp, CAR(args));
    printf("%s", s);
    free((void*)s);
    return NIL;
}

SExpRef builtin_print(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 1) {
        return new_error(interp, "show wrong argument number.\n");
    }
    lisp_print(interp, CAR(args), stdout);
    return NIL;
}

SExpRef builtin_cdr(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 1) {
        return new_error(interp, "cdr: wrong argument number.\n");
    }
    return CDR(CAR(args));
}

SExpRef builtin_cons(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 2) {
        return new_error(interp, "cons: wrong argument number.\n");
    }
    return CONS(CAR(args), CADR(args));
}

static SExp raw_add(SExp a, SExp b) {
    if (a.type == kRealSExp || b.type == kRealSExp) {
        double result = 0;
        if (a.type == kRealSExp) result += a.real;
        else result += a.integer;
        if (b.type == kRealSExp) result += b.real;
        else result += b.integer;
        return (SExp){ .type = kRealSExp, .real = result };
    } else {
        int64_t result;
        return (SExp){ .type = kIntegerSExp, .integer= a.integer + b.integer};
    }
}

static SExp raw_mul(SExp a, SExp b) {
    if (a.type == kRealSExp || b.type == kRealSExp) {
        double result = 1.0;
        if (a.type == kRealSExp) result *= a.real;
        else result *= a.integer;
        if (b.type == kRealSExp) result *= b.real;
        else result *= b.integer;
        return (SExp){ .type = kRealSExp, .real = result };
    } else {
        int64_t result;
        return (SExp){ .type = kIntegerSExp, .integer= a.integer * b.integer};
    }
}

static SExp raw_sub(SExp a, SExp b) {
    if (a.type == kRealSExp || b.type == kRealSExp) {
        double result = 0;
        if (a.type == kRealSExp) result += a.real;
        else result += a.integer;
        if (b.type == kRealSExp) result -= b.real;
        else result -= b.integer;
        return (SExp){ .type = kRealSExp, .real = result };
    } else {
        return (SExp){ .type = kIntegerSExp, .integer= a.integer - b.integer};
    }
}

static SExp raw_div(SExp a, SExp b) {
    double lhs, rhs;
    if (a.type == kRealSExp) lhs = a.real;
    else lhs = a.integer;
    if (b.type == kRealSExp) rhs = b.real;
    else rhs = b.integer;
    return (SExp){ .type = kRealSExp, .real = lhs / rhs};
}

static SExp raw_idiv(SExp a, SExp b) {
    int64_t lhs, rhs;
    lhs = a.integer;
    rhs = b.integer;
    return (SExp){ .type = kIntegerSExp, .integer = lhs / rhs};
}

static SExp raw_mod(SExp a, SExp b) {
    int64_t lhs, rhs;
    lhs = a.integer;
    rhs = b.integer;
    return (SExp){ .type = kIntegerSExp, .integer = lhs % rhs};
}

SExpRef builtin_add(Interp *interp, SExpRef args) {
    SExpRef ret;
    SExp acc = {.type = kIntegerSExp, .integer = 0};
    SExpRef cur = args;
    while (!NILP(cur)) {
        if (REF(CAR(cur))->type != kIntegerSExp && REF(CAR(cur))->type != kRealSExp) {
            return new_error(interp, "+: wrong argument type.\n");
        }
        cur = CDR(cur);
    }
    cur = args;
    while (!NILP(cur)) {
        acc = raw_add(acc, *REF(CAR(cur)));
        cur = CDR(cur);
    }
    ret = new_sexp(interp);
    *REF(ret) = acc;
    return ret;
}

SExpRef builtin_mul(Interp *interp, SExpRef args) {
    SExpRef ret;
    SExp acc = {.type = kIntegerSExp, .integer = 1};
    SExpRef cur = args;
    while (!NILP(cur)) {
        if (REF(CAR(cur))->type != kIntegerSExp && REF(CAR(cur))->type != kRealSExp) {
            return new_error(interp, "*: wrong argument type.\n");
        }
        cur = CDR(cur);
    }
    cur = args;
    while (!NILP(cur)) {
        acc = raw_mul(acc, *REF(CAR(cur)));
        cur = CDR(cur);
    }
    ret = new_sexp(interp);
    *REF(ret) = acc;
    return ret;
}

SExpRef builtin_sub(Interp *interp, SExpRef args) {
    SExpRef ret;
    SExpRef cur = args;
    while (!NILP(cur)) {
        if (REF(CAR(cur))->type != kIntegerSExp && REF(CAR(cur))->type != kRealSExp) {
            return new_error(interp, "-: wrong argument type.\n");
        }
        cur = CDR(cur);
    }
    int args_len = LENGTH(args);
    if (args_len == 1) {
        SExp num = *REF(CAR(args));
        if (num.type == kIntegerSExp) {
            return new_integer(interp, -num.integer);
        }
        return new_real(interp, -num.real);
    }
    if (args_len == 2) {
        SExp num = raw_sub(*REF(CAR(args)), *REF(CADR(args)));
        ret = new_sexp(interp);
        *REF(ret) = num;
        return ret;
    }
    return new_error(interp, "-: wrong argument number.\n");
}

SExpRef builtin_div(Interp *interp, SExpRef args) {
    SExpRef ret;
    SExpRef cur = args;
    while (!NILP(cur)) {
        if (REF(CAR(cur))->type != kIntegerSExp && REF(CAR(cur))->type != kRealSExp) {
            return new_error(interp, "/: wrong argument type.\n");
        }
        cur = CDR(cur);
    }
    int args_len = LENGTH(args);
    if (args_len == 1) {
        SExp num = *REF(CAR(args));
        if (num.type == kIntegerSExp) {
            return new_integer(interp, 1.0/num.integer);
        }
        return new_real(interp, 1.0/num.real);
    }
    if (args_len == 2) {
        SExp num = raw_div(*REF(CAR(args)), *REF(CADR(args)));
        ret = new_sexp(interp);
        *REF(ret) = num;
        return ret;
    }
    return new_error(interp, "/: wrong argument number.\n");
}

SExpRef builtin_idiv(Interp *interp, SExpRef args) {
    SExpRef ret;
    SExpRef cur = args;
    while (!NILP(cur)) {
        if (REF(CAR(cur))->type != kIntegerSExp) {
            return new_error(interp, "i/: wrong argument type.\n");
        }
        cur = CDR(cur);
    }
    int args_len = LENGTH(args);
    if (args_len == 2) {
        SExp num = raw_idiv(*REF(CAR(args)), *REF(CADR(args)));
        ret = new_sexp(interp);
        *REF(ret) = num;
        return ret;
    }
    return new_error(interp, "i/: wrong argument number.\n");
}

SExpRef builtin_mod(Interp *interp, SExpRef args) {
    SExpRef ret;
    SExpRef cur = args;
    while (!NILP(cur)) {
        if (REF(CAR(cur))->type != kIntegerSExp) {
            return new_error(interp, "mod: wrong argument type.\n");
        }
        cur = CDR(cur);
    }
    int args_len = LENGTH(args);
    if (args_len == 2) {
        SExp num = raw_mod(*REF(CAR(args)), *REF(CADR(args)));
        ret = new_sexp(interp);
        *REF(ret) = num;
        return ret;
    }
    return new_error(interp, "mod: wrong argument number.\n");
}

SExpRef builtin_not(Interp *interp, SExpRef args) {
    int args_len = LENGTH(args);
    if (args_len != 1) return new_error(interp, "not: wrong argument number.\n");
    if (TRUEP(CAR(args))) return interp->f;
    return interp->t;
}

SExpRef builtin_num_equal(Interp *interp, SExpRef args) {
    int args_len = LENGTH(args);
    if (args_len != 2) return new_error(interp, "=: wrong argument number.\n");
    SExpRef lhs = CAR(args);
    SExpRef rhs = CADR(args);
    if (VALTYPE(lhs) != kRealSExp && VALTYPE(lhs) != kIntegerSExp) {
        return new_error(interp, "=: type error.\n");
    }
    if (VALTYPE(rhs) != kRealSExp && VALTYPE(rhs) != kIntegerSExp) {
        return new_error(interp, "=: type error.\n");
    }
    if (VALTYPE(lhs) == kRealSExp || VALTYPE(rhs) == kRealSExp) {
        double flhs, frhs;
        if (VALTYPE(lhs) == kIntegerSExp) {
            flhs = REF(lhs)->integer;
        } else {
            flhs = REF(lhs)->real;
        }
        if (VALTYPE(rhs) == kIntegerSExp) {
            frhs = REF(rhs)->integer;
        } else {
            frhs = REF(rhs)->real;
        }
        return new_boolean(interp, flhs == frhs);
    } else {
        return new_boolean(interp, REF(lhs)->integer == REF(rhs)->integer);
    }
}


SExpRef builtin_num_neq(Interp *interp, SExpRef args) {
    int args_len = LENGTH(args);
    if (args_len != 2) return new_error(interp, "/=: wrong argument number.\n");
    SExpRef lhs = CAR(args);
    SExpRef rhs = CADR(args);
    if (VALTYPE(lhs) != kRealSExp && VALTYPE(lhs) != kIntegerSExp) {
        return new_error(interp, "/=: type error.\n");
    }
    if (VALTYPE(rhs) != kRealSExp && VALTYPE(rhs) != kIntegerSExp) {
        return new_error(interp, "/=: type error.\n");
    }
    if (VALTYPE(lhs) == kRealSExp || VALTYPE(rhs) == kRealSExp) {
        double flhs, frhs;
        if (VALTYPE(lhs) == kIntegerSExp) {
            flhs = REF(lhs)->integer;
        } else {
            flhs = REF(lhs)->real;
        }
        if (VALTYPE(rhs) == kIntegerSExp) {
            frhs = REF(rhs)->integer;
        } else {
            frhs = REF(rhs)->real;
        }
        return new_boolean(interp, flhs != frhs);
    } else {
        return new_boolean(interp, REF(lhs)->integer != REF(rhs)->integer);
    }
}

SExpRef builtin_gt(Interp *interp, SExpRef args) {
    int args_len = LENGTH(args);
    if (args_len != 2) return new_error(interp, ">: wrong argument number.\n");
    SExpRef lhs = CAR(args);
    SExpRef rhs = CADR(args);
    if (VALTYPE(lhs) != kRealSExp && VALTYPE(lhs) != kIntegerSExp) {
        return new_error(interp, ">: type error.\n");
    }
    if (VALTYPE(rhs) != kRealSExp && VALTYPE(rhs) != kIntegerSExp) {
        return new_error(interp, ">: type error.\n");
    }
    if (VALTYPE(lhs) == kRealSExp || VALTYPE(rhs) == kRealSExp) {
        double flhs, frhs;
        if (VALTYPE(lhs) == kIntegerSExp) {
            flhs = REF(lhs)->integer;
        } else {
            flhs = REF(lhs)->real;
        }
        if (VALTYPE(rhs) == kIntegerSExp) {
            frhs = REF(rhs)->integer;
        } else {
            frhs = REF(rhs)->real;
        }
        return new_boolean(interp, flhs > frhs);
    } else {
        return new_boolean(interp, REF(lhs)->integer > REF(rhs)->integer);
    }
}

SExpRef builtin_lt(Interp *interp, SExpRef args) {
    int args_len = LENGTH(args);
    if (args_len != 2) return new_error(interp, "<: wrong argument number.\n");
    SExpRef lhs = CAR(args);
    SExpRef rhs = CADR(args);
    if (VALTYPE(lhs) != kRealSExp && VALTYPE(lhs) != kIntegerSExp) {
        return new_error(interp, "<: type error.\n");
    }
    if (VALTYPE(rhs) != kRealSExp && VALTYPE(rhs) != kIntegerSExp) {
        return new_error(interp, "<: type error.\n");
    }
    if (VALTYPE(lhs) == kRealSExp || VALTYPE(rhs) == kRealSExp) {
        double flhs, frhs;
        if (VALTYPE(lhs) == kIntegerSExp) {
            flhs = REF(lhs)->integer;
        } else {
            flhs = REF(lhs)->real;
        }
        if (VALTYPE(rhs) == kIntegerSExp) {
            frhs = REF(rhs)->integer;
        } else {
            frhs = REF(rhs)->real;
        }
        return new_boolean(interp, flhs < frhs);
    } else {
        return new_boolean(interp, REF(lhs)->integer < REF(rhs)->integer);
    }
}

SExpRef builtin_ge(Interp *interp, SExpRef args) {
    int args_len = LENGTH(args);
    if (args_len != 2) return new_error(interp, ">=: wrong argument number.\n");
    SExpRef lhs = CAR(args);
    SExpRef rhs = CADR(args);
    if (VALTYPE(lhs) != kRealSExp && VALTYPE(lhs) != kIntegerSExp) {
        return new_error(interp, ">=: type error.\n");
    }
    if (VALTYPE(rhs) != kRealSExp && VALTYPE(rhs) != kIntegerSExp) {
        return new_error(interp, ">=: type error.\n");
    }
    if (VALTYPE(lhs) == kRealSExp || VALTYPE(rhs) == kRealSExp) {
        double flhs, frhs;
        if (VALTYPE(lhs) == kIntegerSExp) {
            flhs = REF(lhs)->integer;
        } else {
            flhs = REF(lhs)->real;
        }
        if (VALTYPE(rhs) == kIntegerSExp) {
            frhs = REF(rhs)->integer;
        } else {
            frhs = REF(rhs)->real;
        }
        return new_boolean(interp, flhs >= frhs);
    } else {
        return new_boolean(interp, REF(lhs)->integer >= REF(rhs)->integer);
    }
}

SExpRef builtin_gcstat(Interp *interp, SExpRef args) {
    int heapsize = SExpVector_len(&interp->objs);
    int freesize = IntVector_len(&interp->empty_space);
    fprintf(stderr, "heapsize: %d, free: %d\n", heapsize, freesize);
    return NIL;
}

SExpRef builtin_le(Interp *interp, SExpRef args) {
    int args_len = LENGTH(args);
    if (args_len != 2) return new_error(interp, "<=: wrong argument number.\n");
    SExpRef lhs = CAR(args);
    SExpRef rhs = CADR(args);
    if (VALTYPE(lhs) != kRealSExp && VALTYPE(lhs) != kIntegerSExp) {
        return new_error(interp, "<=: type error.\n");
    }
    if (VALTYPE(rhs) != kRealSExp && VALTYPE(rhs) != kIntegerSExp) {
        return new_error(interp, "<=: type error.\n");
    }
    if (VALTYPE(lhs) == kRealSExp || VALTYPE(rhs) == kRealSExp) {
        double flhs, frhs;
        if (VALTYPE(lhs) == kIntegerSExp) {
            flhs = REF(lhs)->integer;
        } else {
            flhs = REF(lhs)->real;
        }
        if (VALTYPE(rhs) == kIntegerSExp) {
            frhs = REF(rhs)->integer;
        } else {
            frhs = REF(rhs)->real;
        }
        return new_boolean(interp, flhs <= frhs);
    } else {
        return new_boolean(interp, REF(lhs)->integer <= REF(rhs)->integer);
    }
}

