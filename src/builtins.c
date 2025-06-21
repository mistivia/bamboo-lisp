#include "builtins.h"
#include "interp.h"
#include "sexp.h"
#include <algds/str.h>

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
        if (a.type == kRealSExp) result += a.real;
        else result *= a.integer;
        if (b.type == kRealSExp) result += b.real;
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

