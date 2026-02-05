#include "basic_traits.h"

#include <string.h>
#include <inttypes.h>

#include "mmhash.h"

#define BASIC_TRAITS_IMPL(T) \
    bool T##_eq(T lhs, T rhs) { \
        return lhs == rhs; \
    } \
    int T##_cmp(T lhs, T rhs) { \
        if (lhs == rhs) return 0; \
        if (lhs < rhs) return -1; \
        return 1; \
    } \
    uint64_t T##_hash(T x) { \
        return mmhash(&x, sizeof(T), 0); \
    } \

BASIC_TRAITS_IMPL(Char);
BASIC_TRAITS_IMPL(Bool);
BASIC_TRAITS_IMPL(Int);
BASIC_TRAITS_IMPL(Long);
BASIC_TRAITS_IMPL(UInt);
BASIC_TRAITS_IMPL(ULong);
BASIC_TRAITS_IMPL(Double);
BASIC_TRAITS_IMPL(Float);
BASIC_TRAITS_IMPL(VoidPtr);

void Char_show(Char self, FILE* fp) {
    fprintf(fp, "%c", self);
}
void Bool_show(Bool self, FILE* fp) {
    if (self) fprintf(fp, "true");
    else fprintf(fp, "false");
}
void Int_show(Int self, FILE* fp) {
    fprintf(fp, "%"PRId32, self);
}
void Long_show(Long self, FILE* fp) {
    fprintf(fp, "%"PRId64, self);
}
void UInt_show(UInt self, FILE* fp) {
    fprintf(fp, "%"PRIu32, self);
}
void ULong_show(ULong self, FILE* fp) {
    fprintf(fp, "%"PRIu64, self);
}
void VoidPtr_show(VoidPtr self, FILE* fp) {
    fprintf(fp, "%p", self);
}
void Double_show(Double self, FILE* fp) {
    fprintf(fp, "%lf", self);
}
void Float_show(Float self, FILE* fp) {
    fprintf(fp, "%f", self);
}
void String_show(String self, FILE* fp) {
    fprintf(fp, "%s", self);
}

bool String_eq(String lhs, String rhs) {
    return strcmp(lhs, rhs) == 0;
}

int String_cmp(String lhs, String rhs) {
    return strcmp(lhs, rhs);
}

uint64_t String_hash(String x) {
    size_t len = strlen(x);
    return mmhash(x, len, 0);
}


