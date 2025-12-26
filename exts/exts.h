#ifndef BAMBOO_LISP_EXTS_H_
#define BAMBOO_LISP_EXTS_H_

#include <bamboo_lisp/interp.h>

int bamboo_lisp_ext_vector_init(Interp *interp);
int bamboo_lisp_ext_io_init(Interp *interp);
int bamboo_lisp_ext_dict_init(Interp *interp);

#endif