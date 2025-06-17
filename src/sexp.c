#include "sexp.h"
#include "algds/vec.h"

void SExp_show(SExp self, FILE* fp) {
    fprintf(fp, "{SEXP}");
}

VECTOR_IMPL(SExp);
