#include "primitives.h"
#include "interp.h"
#include "sexp.h"

SExpRef primitive_if(Interp *interp, SExpRef args, bool istail) {
    SExpRef cond, tb, fb;

    if (LENGTH(args) != 3) goto error;
    cond = CAR(args);
    tb = CADR(args);
    fb = CADDR(args);
    cond = EVAL(cond);
    if (CTL_FL(cond)) return cond;
    if (TRUEP(cond)) return lisp_eval(interp, tb, istail);
    else return lisp_eval(interp, fb, istail);
error:
    return new_error(interp, "if: syntax error.\n");
}

SExpRef primitive_cond(Interp *interp, SExpRef args, bool istail) {
    SExpRef pair, condition, exp, iter;

    if (LENGTH(args) < 1) goto error;
    iter = args;
    while (!NILP(iter)) {
        pair = CAR(iter);
        if (!lisp_check_list(interp, pair)) goto error;
        if (LENGTH(pair) != 2) goto error;
        condition = CAR(pair);
        exp = CADR(pair);
        condition = EVAL(condition);
        if (CTL_FL(condition)) return condition;
        if (TRUEP(condition)) return lisp_eval(interp, exp, istail);
        iter = CDR(iter);
    }
    return NIL;
error:
    return new_error(interp, "cond: syntax error.\n");
}

SExpRef primitive_progn(Interp *interp, SExpRef args, bool istail) {
    SExpRef iter = args;
    SExpRef ret;

    while (!NILP(iter)) {
        if (NILP(CDR(iter))) {
            return lisp_eval(interp, CAR(iter), istail);
        } else {
            ret = EVAL(CAR(iter));
        }
        if (CTL_FL(ret)) return ret;
        iter = CDR(iter);
    }
    return ret;
}

SExpRef primitive_setq(Interp *interp, SExpRef args, bool istail) {
    SExpRef name, exp, value;

    if (LENGTH(args) != 2) goto error;
    name = CAR(args);
    exp = CADR(args);
    if (REF(name)->type != kSymbolSExp) goto error;
    value = EVAL(exp);
    if (CTL_FL(value)) return value;
    return lisp_setq(interp, REF(name)->str, value);
error:
    return new_error(interp, "setq: syntax error.\n");
}

static const char *binding_name(Interp *interp, SExpRef binding) {
    SExpRef namesym = REF(binding)->binding.name;
    return REF(namesym)->str;
}

static bool is_binding_repeat(Interp *interp, SExpRef sym, SExpRef env) {
    SExpRef binding = REF(env)->env.bindings;

    while (!NILP(binding)) {
        if (strcmp(REF(sym)->str, binding_name(interp, binding)) == 0) return true;
        binding = REF(binding)->binding.next;
    }
    return false;
}

SExpRef primitive_let(Interp *interp, SExpRef args, bool istail) {
    SExpRef binding, iter, bindings, env, x,
            val, body, ret, exp;

    if (LENGTH(args) < 1) goto error;
    bindings = CAR(args);
    env = new_env(interp);
    REF(env)->env.parent = CAR(interp->stack);

    iter = bindings;
    while (!NILP(iter)) {
        x = CAR(iter);
        if (!lisp_check_list(interp, x)) goto error;
        if (LENGTH(x) != 2) goto error;
        if (REF(CAR(x))->type != kSymbolSExp) goto error;
        if (is_binding_repeat(interp, CAR(x), env)) goto error;
        binding = new_binding(interp, CAR(x), NIL);
        REF(binding)->binding.next = REF(env)->env.bindings;
        REF(env)->env.bindings = binding;
        iter = CDR(iter);
    }
    interp->stack = CONS(env, interp->stack);

    ret = NIL;
    iter = bindings;
    while (!NILP(iter)) {
        x = CAR(iter);
        val = EVAL(CADR(x));
        if (CTL_FL(val)) goto end;
        ret = lisp_setq(interp, REF(CAR(x))->str, val);
        if (CTL_FL(ret)) goto end;
        iter = CDR(iter);
    }

    body = CDR(args);
    iter = body;
    while (!NILP(iter)) {
        exp = CAR(iter);
        if (NILP(CDR(iter))) {
            ret = lisp_eval(interp, exp, istail);
            goto end;
        } else {
            ret = EVAL(exp);
        }
        if (CTL_FL(val)) goto end;
        iter = CDR(iter);
    }
end:
    interp->stack = CDR(interp->stack);
    return ret;

error:
    return new_error(interp, "let: syntax error. \n");
}

SExpRef primitive_while(Interp *interp, SExpRef args, bool istail) {
    SExpRef ret, pred, body, cond, iter, x;

    if (LENGTH(args) < 2) goto error;
    ret = NIL;
    pred = CAR(args);
    body = CDR(args);
    while (1) {
nextloop:
        cond = EVAL(pred);
        if (CTL_FL(cond)) {
            if (VALTYPE(cond) != kErrSignal) {
                return new_error(interp, "while: unexpected control flow.\n");
            }
            return cond;
        }
        if (!TRUEP(cond)) return ret;
        iter = body;
        while (!NILP(iter)) {
            x = CAR(iter);
            ret = EVAL(x);
            if (VALTYPE(ret) == kErrSignal || VALTYPE(ret) == kReturnSignal) {
                return ret;
            }
            if (VALTYPE(ret) == kBreakSignal) {
                return REF(ret)->ret;
            }
            if (VALTYPE(ret) == kContinueSignal) {
                goto nextloop;
            }
            iter = CDR(iter);
        }
    }
error:
    return new_error(interp, "while: syntax error.\n");
}

SExpRef primitive_lambda(Interp *interp, SExpRef args, bool istail) {
    SExpRef env, param, body;

    if (LENGTH(args) < 2) goto error;
    env = CAR(interp->stack);
    param = CAR(args);
    body = CDR(args);
    return new_lambda(interp, param, body, env);
error:
    return new_error(interp, "lambda: syntax error.\n");
}

SExpRef primitive_defun(Interp *interp, SExpRef args, bool istail) {
    SExpRef name, param, body, function;

    if (LENGTH(args) < 3) goto error;
    if (CAR(interp->stack).idx != interp->top_level.idx) {
        return new_error(interp, "defun: functions can only be defined in top level.\n");
    }
    name = CAR(args);
    if (VALTYPE(name) != kSymbolSExp) goto error;
    param = CADR(args);
    body = CDDR(args);
    function = new_lambda(interp, param, body, interp->top_level);
    lisp_defun(interp, REF(name)->str, function);
    return name;
error:
    return new_error(interp, "defun: syntax error.\n");
}
SExpRef primitive_defmacro(Interp *interp, SExpRef args, bool istail) {
    SExpRef param, name, body, macro;

    if (LENGTH(args) < 3) goto error;
    if (CAR(interp->stack).idx != interp->top_level.idx) {
        return new_error(interp, "defmacro: macros can only be defined in top level.\n");
    }
    name = CAR(args);
    if (VALTYPE(name) != kSymbolSExp) goto error;
    param = CADR(args);
    body = CDDR(args);
    macro = new_macro(interp, param, body);
    lisp_defun(interp, REF(name)->str, macro);
    return name;
error:
    return new_error(interp, "defmacro: syntax error.\n");
}

SExpRef primitive_defvar(Interp *interp, SExpRef args, bool istail) {
    SExpRef name, exp, val;

    if (LENGTH(args) != 2) goto error;
    if (CAR(interp->stack).idx != interp->top_level.idx) {
        return new_error(interp, "defvar: functions can only be defined in top level.\n");
    }
    name = CAR(args);
    if (VALTYPE(name) != kSymbolSExp) goto error;
    exp = CADR(args);
    val = EVAL(exp);
    if (CTL_FL(val)) return val;
    lisp_defvar(interp, REF(name)->str, val);
    return name;
error:
    return new_error(interp, "defvar: syntax error.\n");
}

SExpRef primitive_function(Interp *interp, SExpRef args, bool istail) {
    if (LENGTH(args) != 1) goto error;
    if (VALTYPE(CAR(args)) != kSymbolSExp) goto error;
    return lisp_lookup_func(interp, REF(CAR(args))->str);
error:
    return new_error(interp, "function: syntax error.\n");
}

static SExpRef build_function_env(Interp *interp, SExpRef func, SExpRef args) {
    SExpRef param = REF(func)->func.args;
    SExpRef iparam = param;
    SExpRef iargs = args;
    SExpRef env = new_env(interp);
    SExpRef binding, name;

    while (!NILP(iparam)) {
        if (VALTYPE(iparam) == kSymbolSExp) {
            binding = new_binding(interp, iparam, iargs);
            REF(binding)->binding.next = REF(env)->env.bindings;
            REF(env)->env.bindings = binding;
            return env;
        }
        name = CAR(iparam);
        if (VALTYPE(name) != kSymbolSExp) {
            return new_error(interp, "function syntax error: parameter must be a symbol.\n");
        }
        if (NILP(iargs)) return new_error(interp, "funcall: wrong argument number.\n");
        binding = new_binding(interp, name, CAR(iargs));
        REF(binding)->binding.next = REF(env)->env.bindings;
        REF(env)->env.bindings = binding;
        iargs = CDR(iargs);
        iparam = CDR(iparam);
    }
    if (!NILP(iargs)) return new_error(interp, "funcall: wrong argument number.\n");
    return env;
}

SExpRef primitive_funcall(Interp *interp, SExpRef args, bool istail) {
    if (LENGTH(args) < 1) goto error;
    args = lisp_eval_args(interp, args);
    if (CTL_FL(args)) return args;
    return lisp_apply(interp, CAR(args), CDR(args), istail);
error:
    return new_error(interp, "funcall: syntax error.\n");
}

SExpRef primitive_quote(Interp *interp, SExpRef args, bool istail) {
    if (LENGTH(args) != 1) return new_error(interp, "quote: syntax error.\n");
    return CAR(args);
}

SExpRef primitive_macroexpand1(Interp *interp, SExpRef args, bool istail) {
    SExpRef macro;

    if (LENGTH(args) != 1) goto error;
    args = CAR(args);
    if (VALTYPE(CAR(args)) != kSymbolSExp) goto error;
    macro = lisp_lookup_func(interp, REF(CAR(args))->str);
    if (VALTYPE(macro) != kMacroSExp) goto error;
    return lisp_macroexpand1(interp, macro, CDR(args));
error:
    return new_error(interp, "macroexpand-1: syntax error.\n");
}

SExpRef primitive_apply(Interp *interp, SExpRef args, bool istail) {
    SExpRef ret;

    if (LENGTH(args) != 2) goto error;
    args = lisp_eval_args(interp, args);
    if (CTL_FL(args)) return args;
    if (!lisp_check_list(interp, CADR(args))) goto error;
    PUSH_REG(args);
    ret = lisp_apply(interp, CAR(args), CADR(args), istail);
    POP_REG();
    return ret;
error:
    return new_error(interp, "apply: syntax error.\n");
}

static SExpRef quasi_on_list(Interp *interp, SExpRef lst);
static SExpRef quasi_impl(Interp *interp, SExpRef obj, bool *slicing);

static SExpRef quasi_impl(Interp *interp, SExpRef obj, bool *slicing) {
    SExpRef lst;

    *slicing = false;
    if (VALTYPE(obj) != kPairSExp) return obj;
    if (VALTYPE(CAR(obj)) == kSymbolSExp
            && strcmp("unquote", REF(CAR(obj))->str) == 0) {
        if (LENGTH(obj) != 2) {
            return new_error(interp, "unquote: syntax error.\n");
        }
        return EVAL(CADR(obj));
    }
    if (VALTYPE(CAR(obj)) == kSymbolSExp
            && strcmp("slicing-unquote", REF(CAR(obj))->str) == 0) {
        lst = EVAL(CADR(obj));
        if (CTL_FL(lst)) return lst;
        if (LENGTH(obj) != 2) {
            return new_error(interp, "slicing-unquote: syntax error.\n");
        }
        if (!lisp_check_list(interp, lst)) {
            return new_error(interp, "slicing-unquote: not a list.\n");
        }
        *slicing = true;
        return lst;
    }
    return quasi_on_list(interp, obj);
}

static SExpRef quasi_on_list(Interp *interp, SExpRef lst) {
    SExpRef newlst = NIL;
    SExpRef iter, j, x, newx;

    bool slicing;
    iter = lst;
    while (!NILP(iter)) {
        x = CAR(iter);
        newx = quasi_impl(interp, x, &slicing);
        if (CTL_FL(newx)) return newx;
        if (slicing) {
            j = newx;
            while (!NILP(j)) {
                newlst = CONS(CAR(j), newlst);
                j = CDR(j);
            }
        } else {
            newlst = CONS(newx, newlst);
        }
        iter = CDR(iter);
    }

    return lisp_reverse(interp, newlst);
}

SExpRef primitive_quasi(Interp *interp, SExpRef args, bool istail) {
    SExpRef ret;
    if (LENGTH(args) != 1) return new_error(interp, "quasiquote: syntax error.\n");
    bool slicing;
    ret = quasi_impl(interp, CAR(args), &slicing);
    if (slicing) return new_error(interp, "quasiquote: syntax error.\n");
    return ret;
}

SExpRef primitive_and(Interp *interp, SExpRef args, bool istail) {
    SExpRef ret;
    SExpRef i = args;
    if (LENGTH(args) < 1) return new_error(interp, "and: syntax error.\n");
    while (!NILP(i)) {
        if (!NILP(CDR(i))) {
            ret = EVAL(CAR(i));    
        } else {
            return lisp_eval(interp, CAR(i), istail);
        }
        if (!TRUEP(ret)) return ret;
        i = CDR(i);
    }
    return ret;
}

SExpRef primitive_or(Interp *interp, SExpRef args, bool istail) {
    SExpRef ret;
    SExpRef i = args;

    if (LENGTH(args) < 1) return new_error(interp, "or: syntax error.\n");
    while (!NILP(i)) {
        if (!NILP(CDR(i))) {
            ret = EVAL(CAR(i));    
        } else {
            return lisp_eval(interp, CAR(i), istail);
        }
        if (TRUEP(ret)) return ret;
        i = CDR(i);
    }
    return ret;
}

