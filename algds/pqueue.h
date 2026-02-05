#ifndef PQUEUE_H_
#define PQUEUE_H_

#include "vec.h"

#define PQUEUE_DEF(T) \
    typedef struct { \
        T##Vector vec; \
    } T##PQueue; \
    void T##PQueue_init(T##PQueue *self); \
    void T##PQueue_push(T##PQueue *self, T elem); \
    void T##PQueue_pop(T##PQueue *self); \
    T* T##PQueue_top(T##PQueue *self); \
    T##PQueue T##PQueue_move(T##PQueue *self); \
    void T##PQueue_free(T##PQueue *self); \

PQUEUE_DEF(Int);
PQUEUE_DEF(Bool);
PQUEUE_DEF(Long);
PQUEUE_DEF(Char);
PQUEUE_DEF(UInt);
PQUEUE_DEF(ULong);
PQUEUE_DEF(Double);
PQUEUE_DEF(Float);
PQUEUE_DEF(String);
PQUEUE_DEF(VoidPtr);

#define PQUEUE_IMPL(T) \
    static int T##PQueue_cmp(T##PQueue *self, int a, int b) { \
        return T##_cmp(*T##Vector_ref(&self->vec, a), *T##Vector_ref(&self->vec, b)); \
    } \
    void T##PQueue_init(T##PQueue *self) { \
         T##Vector_init(&self->vec); \
    } \
    void T##PQueue_push(T##PQueue *self, T elem) { \
        T##Vector_push_back(&self->vec, elem); \
        int i = self->vec.size - 1; \
        while (i > 0 && T##PQueue_cmp(self, i, i / 2) > 0) { \
            T##Vector_swap(&self->vec, i, i / 2); \
            i /= 2; \
        } \
    } \
    static void T##PQueue_heapify(T##PQueue *self, int idx) { \
        int left, right, largest; \
        left = 2 * idx + 1; \
        right = 2 * idx + 2; \
        if (left < self->vec.size && T##PQueue_cmp(self, left, idx) > 0) { \
            largest = left; \
        } else { \
            largest = idx; \
        } \
        if (right < self->vec.size && T##PQueue_cmp(self, right, largest) > 0) { \
            largest = right; \
        } \
        if (largest != idx) { \
            T##Vector_swap(&self->vec, largest, idx); \
            T##PQueue_heapify(self, largest); \
        } \
    } \
    void T##PQueue_pop(T##PQueue *self) { \
        if (self->vec.size == 0) return; \
        memcpy(T##Vector_ref(&self->vec, 0), T##Vector_ref(&self->vec, self->vec.size - 1), sizeof(T)); \
        T##Vector_pop(&self->vec); \
        T##PQueue_heapify(self, 0); \
    } \
    T* T##PQueue_top(T##PQueue *self) { \
        if (self->vec.size == 0) return NULL; \
        return self->vec.buffer; \
    } \
    T##PQueue T##PQueue_move(T##PQueue *self) { \
        T##PQueue dup; \
        dup.vec = T##Vector_move(&self->vec); \
        return dup; \
    } \
    void T##PQueue_free(T##PQueue *self) { \
        T##Vector_free(&self->vec); \
    } \

#endif
