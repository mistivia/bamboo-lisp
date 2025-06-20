#include "primitives.h"
#include "sexp.h"

SExpRef primitive_if(Interp *interp, SExpRef args) {
    if (lisp_length(interp, args) != 3) goto error;
    SExpRef cond = CAR(args);
    SExpRef tb = CADR(args);
    SExpRef fb = CADDR(args);
    cond = EVAL(cond);
    if (ERRORP(cond)) return cond;
    if (TRUEP(cond)) return EVAL(tb);
    else return EVAL(fb);
    return NIL;
error:
    return new_error(interp, "if: syntax error.\n");
}

SExpRef primitive_cond(Interp *interp, SExpRef args) {
    if (lisp_length(interp, args) < 1) goto error;
    SExpRef iter = args;
    while (!NILP(iter)) {
        SExpRef pair = CAR(iter);
        if (!lisp_check_list(interp, pair)) goto error;
        if (lisp_length(interp, pair) != 2) goto error;
        SExpRef condition = CAR(pair);
        SExpRef exp = CADR(pair);
        condition = EVAL(condition);
        if (ERRORP(condition)) return condition;
        if (TRUEP(condition)) return EVAL(exp);
        iter = CDR(iter);
    }
    return NIL;
error:
    return new_error(interp, "cond: syntax error.\n");
}

SExpRef primitive_progn(Interp *interp, SExpRef args) {
    SExpRef iter = args;
    SExpRef ret;
    while (!NILP(iter)) {
        ret = EVAL(CAR(iter));
        if (ERRORP(ret)) return ret;
        iter = CDR(iter);
    }
    return ret;
}

SExpRef primitive_setq(Interp *interp, SExpRef args) {
    if (lisp_length(interp, args) != 2) goto error;
    SExpRef name = CAR(args);
    SExpRef exp = CADR(args);
    if (REF(name)->type != kSymbolSExp) goto error;
    SExpRef value = EVAL(exp);
    if (ERRORP(value)) return value;
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

SExpRef primitive_let(Interp *interp, SExpRef args) {
    if (lisp_length(interp, args) < 1) goto error;
    SExpRef bindings = CAR(args);
    SExpRef env = new_env(interp);
    REF(env)->env.parent = CAR(interp->stack);

    SExpRef iter = bindings;
    while (!NILP(iter)) {
        SExpRef x = CAR(iter);
        if (!lisp_check_list(interp, x)) goto error;
        if (lisp_length(interp, x) != 2) goto error;
        if (REF(CAR(x))->type != kSymbolSExp) goto error;
        if (is_binding_repeat(interp, CAR(x), env)) goto error;
        SExpRef binding = new_binding(interp, CAR(x), NIL);
        REF(binding)->binding.next = REF(env)->env.bindings;
        REF(env)->env.bindings = binding;
        iter = CDR(iter);
    }
    interp->stack = CONS(env, interp->stack);

    SExpRef ret = NIL;
    iter = bindings;
    while (!NILP(iter)) {
        SExpRef x = CAR(iter);
        SExpRef val = EVAL(CADR(x));
        if (REF(val)->type == kErrSExp) goto end;
        ret = lisp_setq(interp, REF(CAR(x))->str, val);
        if (ERRORP(ret)) goto end;
        iter = CDR(iter);
    }

    SExpRef body = CDR(args);
    iter = body;
    while (!NILP(iter)) {
        SExpRef exp = CAR(iter);
        ret = EVAL(exp);
        if (REF(ret)->type == kErrSExp) goto end;
        iter = CDR(iter);
    }
end:
    interp->stack = CDR(interp->stack);
    return ret;

error:
    return new_error(interp, "let: syntax error. \n");
}

SExpRef primitive_while(Interp *interp, SExpRef args) {
    if (lisp_length(interp, args) < 2) goto error;
    SExpRef ret = NIL;
    SExpRef pred = CAR(args);
    SExpRef body = CDR(args);
    while (1) {
        SExpRef cond = EVAL(pred);
        if (ERRORP(cond)) return cond;
        if (!TRUEP(cond)) return ret;
        SExpRef iter = body;
        while (!NILP(iter)) {
            SExpRef x = CAR(iter);
            ret = EVAL(x);
            if (ERRORP(ret)) return ret;
            iter = CDR(iter);
        }
    }
error:
    return new_error(interp, "while: syntax error.\n");
}

SExpRef primitive_lambda(Interp *interp, SExpRef args) {
    if (lisp_length(interp, args) < 2) goto error;
    SExpRef env = CAR(interp->stack);
    SExpRef param = CAR(args);
    SExpRef body = CDR(args);
    return new_lambda(interp, param, body, env);
error:
    return new_error(interp, "lambda: syntax error.\n");
}

SExpRef primitive_defun(Interp *interp, SExpRef args) {
    if (lisp_length(interp, args) < 3) goto error;
    if (CAR(interp->stack).idx != interp->top_level.idx) {
        return new_error(interp, "defun: functions can only be defined in top level.\n");
    }
    SExpRef name = CAR(args);
    if (VALTYPE(name) != kSymbolSExp) goto error;
    SExpRef param = CADR(args);
    SExpRef body = CDDR(args);
    SExpRef function = new_lambda(interp, param, body, interp->top_level);
    lisp_defun(interp, REF(name)->str, function);
    return name;
error:
    return new_error(interp, "defun: syntax error.\n");
}

SExpRef primitive_defvar(Interp *interp, SExpRef args) {
    if (lisp_length(interp, args) != 2) goto error;
    if (CAR(interp->stack).idx != interp->top_level.idx) {
        return new_error(interp, "defvar: functions can only be defined in top level.\n");
    }
    SExpRef name = CAR(args);
    if (VALTYPE(name) != kSymbolSExp) goto error;
    SExpRef exp = CADR(args);
    SExpRef val = EVAL(exp);
    if (ERRORP(val)) return val;
    lisp_defvar(interp, REF(name)->str, val);
    return name;
error:
    return new_error(interp, "defvar: syntax error.\n");
}

SExpRef primitive_function(Interp *interp, SExpRef args) {
    if (lisp_length(interp, args) != 1) goto error;
    if (VALTYPE(CAR(args)) != kSymbolSExp) goto error;
    return lisp_lookup_func(interp, REF(CAR(args))->str);
error:
    return new_error(interp, "function: syntax error.\n");
}

// TODO:
// - funcall
// - apply
// - defvar
// - defmacro
// - macroexpand-1
