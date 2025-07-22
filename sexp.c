#include "sexp.h"
#include "algds/vec.h"

#include <inttypes.h>

void SExpRef_show(SExpRef self, FILE* fp) {}
void SExpPtr_show(SExpPtr self, FILE* fp) {}
void SExp_show(SExp self, FILE* fp) {}

VECTOR_IMPL(SExp);
VECTOR_IMPL(SExpRef);
VECTOR_IMPL(SExpPtr);
