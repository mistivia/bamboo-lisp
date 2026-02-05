#ifndef ALGDS_SORT_H_
#define ALGDS_SORT_H_

#include <stdlib.h>
#include <string.h>

#include "type_alias.h"

#define QSORT_DEF(T) \
    void T##_qsort(T* arr, int n);

#define QSORT_IMPL(T) \
    void T##_qsort_swap(T* arr, int lhs, int rhs) { \
        if (lhs == rhs) return; \
        T buf; \
        buf = arr[lhs]; \
        arr[lhs] = arr[rhs]; \
        arr[rhs] = buf; \
    } \
    void T##_qsort(T* arr, int n) { \
        if (n <= 1) return; \
        int pivot = rand() % n; \
        T##_qsort_swap(arr, 0, pivot); \
        int lp = 1, rp = n-1; \
        while (lp < rp) { \
            if (T##_cmp(arr[lp], arr[0]) < 0) { \
                lp++; \
                continue; \
            } \
            if (T##_cmp(arr[rp], arr[0]) >= 0) { \
                rp--; \
                continue; \
            } \
            T##_qsort_swap(arr, lp, rp); \
            lp++; \
            rp--; \
        } \
        if (T##_cmp(arr[rp], arr[0]) > 0) rp--; \
        T##_qsort_swap(arr, 0, rp); \
        T##_qsort(arr, rp); \
        T##_qsort(arr+rp+1, n-rp-1); \
    }

QSORT_DEF(Int);
QSORT_DEF(Bool);
QSORT_DEF(Long);
QSORT_DEF(Char);
QSORT_DEF(UInt);
QSORT_DEF(ULong);
QSORT_DEF(Double);
QSORT_DEF(Float);
QSORT_DEF(String);

#endif
