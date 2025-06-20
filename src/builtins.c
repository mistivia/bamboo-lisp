#include "builtins.h"
#include "interp.h"
#include "sexp.h"

SExpRef builtin_list(Interp *interp, SExpRef args) {
    return args;
}

SExpRef builtin_car(Interp *interp, SExpRef args) {
    if (lisp_length(interp, args) != 1) {
        return new_error(interp, "car: wrong argument number.\n");
    }
    if (ERRORP(args)) return args;
    return CAR(CAR(args));
}

SExpRef builtin_show(Interp *interp, SExpRef args) {
    if (lisp_length(interp, args) != 1) {
        return new_error(interp, "show wrong argument number.\n");
    }
    lisp_print(interp, CAR(args), stdout);
    return NIL;
}

SExpRef builtin_cdr(Interp *interp, SExpRef args) {
    if (lisp_length(interp, args) != 1) {
        return new_error(interp, "cdr: wrong argument number.\n");
    }
    return CDR(CAR(args));
}

SExpRef builtin_cons(Interp *interp, SExpRef args) {
    if (lisp_length(interp, args) != 2) {
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

SExpRef builtin_sub(Interp *interp, SExpRef args) {
    SExpRef ret;
    SExpRef cur = args;
    while (!NILP(cur)) {
        if (REF(CAR(cur))->type != kIntegerSExp && REF(CAR(cur))->type != kRealSExp) {
            return new_error(interp, "-: wrong argument type.\n");
        }
        cur = CDR(cur);
    }
    int args_len = lisp_length(interp, args);
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

SExpRef builtin_num_equal(Interp *interp, SExpRef args) {
    int args_len = lisp_length(interp, args);
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

SExpRef builtin_gt(Interp *interp, SExpRef args) {
    int args_len = lisp_length(interp, args);
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
    int args_len = lisp_length(interp, args);
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
    int args_len = lisp_length(interp, args);
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

SExpRef builtin_le(Interp *interp, SExpRef args) {
    int args_len = lisp_length(interp, args);
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

