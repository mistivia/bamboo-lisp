#include "sexp.h"
#include "algds/vec.h"

#include <inttypes.h>

void SExpRef_show(SExpRef self, FILE* fp) { }

void SExp_show(SExp self, FILE* fp) {
    if (self.type == kEmptySExp) fprintf(fp, "<EMPTY>");
    else if (self.type == kIntegerSExp) fprintf(fp, "%"PRId64, self.integer);
    else if (self.type == kRealSExp) fprintf(fp, "%lf", self.real);
    else if (self.type == kBooleanSExp) {
        if (self.boolean) fprintf(fp, "#t");
        else fprintf(fp, "#f");
    } else if (self.type == kNilSExp) fprintf(fp, "()");
    else if (self.type == kCharSExp) fprintf(fp, "#\\%c", self.character);
    else if (self.type == kStringSExp) fprintf(fp, "\"%s\"", self.str);
    else if (self.type == kSymbolSExp) fprintf(fp, "'%s", self.str);
    else if (self.type == kUserDataSExp) fprintf(fp, "<%p>", self.userdata);
    else if (self.type == kFuncSExp) fprintf(fp, "<FUNCTION>");
    else if (self.type == kPairSExp) {
        fprintf(fp, "(<%d> . <%d>)", self.pair.car.idx, self.pair.cdr.idx);
    }
    else if (self.type == kEnvSExp) fprintf(fp, "<Env>");
    else if (self.type == kBindingSExp) fprintf(fp, "<BINDING>");
    else if (self.type == kMacroSExp) fprintf(fp, "<MACRO>");
}

VECTOR_IMPL(SExp);
VECTOR_IMPL(SExpRef);
