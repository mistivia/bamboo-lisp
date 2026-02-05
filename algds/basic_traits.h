#ifndef ALGDS_BAISC_TRAITS_H_
#define ALGDS_BAISC_TRAITS_H_

#include <stdio.h>

#include "type_alias.h"

// basic traits
#define BASIC_TRAITS_DEF(T) \
    Bool T##_eq(T lhs, T rhs); \
    Int T##_cmp(T lhs, T rhs); \
    uint64_t T##_hash(T x); \
    void T##_show(T x, FILE* fp); \

BASIC_TRAITS_DEF(Int);
BASIC_TRAITS_DEF(Bool);
BASIC_TRAITS_DEF(Long);
BASIC_TRAITS_DEF(Char);
BASIC_TRAITS_DEF(UInt);
BASIC_TRAITS_DEF(ULong);
BASIC_TRAITS_DEF(Double);
BASIC_TRAITS_DEF(Float);
BASIC_TRAITS_DEF(VoidPtr);
BASIC_TRAITS_DEF(String);


#endif
