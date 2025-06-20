#ifndef BAMBOO_LISP_PRIMITIVIE_H_
#define BAMBOO_LISP_PRIMITIVIE_H_

#include "interp.h"

SExpRef primitive_if(Interp *interp, SExpRef sexp);
SExpRef primitive_cond(Interp *interp, SExpRef sexp);
SExpRef primitive_progn(Interp *interp, SExpRef sexp);
SExpRef primitive_setq(Interp *interp, SExpRef sexp);
SExpRef primitive_let(Interp *interp, SExpRef sexp);
SExpRef primitive_while(Interp *interp, SExpRef sexp);
SExpRef primitive_lambda(Interp *interp, SExpRef sexp);
SExpRef primitive_defun(Interp *interp, SExpRef sexp);
SExpRef primitive_defvar(Interp *interp, SExpRef sexp);
SExpRef primitive_function(Interp *interp, SExpRef sexp);
SExpRef primitive_funcall(Interp *interp, SExpRef sexp);
SExpRef primitive_apply(Interp *interp, SExpRef sexp);
SExpRef primitive_quote(Interp *interp, SExpRef sexp);

#endif
