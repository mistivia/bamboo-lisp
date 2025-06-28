#ifndef BAMBOO_LISP_PRIMITIVIE_H_
#define BAMBOO_LISP_PRIMITIVIE_H_

#include "interp.h"

SExpRef primitive_assert_error(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_assert_exception(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_load(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_return(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_break(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_continue(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_assert(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_eval(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_if(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_cond(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_progn(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_setq(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_let(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_while(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_lambda(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_defun(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_defvar(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_defmacro(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_function(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_macroexpand1(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_funcall(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_apply(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_quote(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_quasi(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_and(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_or(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_unwind_protect(Interp *interp, SExpRef sexp, bool istail);
SExpRef primitive_try(Interp *interp, SExpRef sexp, bool istail);

#endif
