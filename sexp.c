#include "sexp.h"
#include "algds/vec.h"

#include <inttypes.h>
#include <stdlib.h>

void SExpRef_show(SExpRef self, FILE* fp) {}
void SExpPtr_show(SExpPtr self, FILE* fp) {}
void SExp_show(SExp self, FILE* fp) {}

VECTOR_IMPL(SExp);
VECTOR_IMPL(SExpRef);
VECTOR_IMPL(SExpPtr);

void SExpHeap_init(SExpHeap *heap) {
    heap->chunks = NULL;
    heap->chunk_count = 0;
    heap->chunk_cap = 0;
    heap->size = 0;
}

void SExpHeap_free(SExpHeap *heap) {
    for (int i = 0; i < heap->chunk_count; i++) {
        free(heap->chunks[i]);
    }
    free(heap->chunks);
    heap->chunks = NULL;
    heap->chunk_count = 0;
    heap->chunk_cap = 0;
    heap->size = 0;
}

int SExpHeap_push(SExpHeap *heap, SExp value) {
    int idx = heap->size;
    int chunk = idx / SEXP_HEAP_CHUNK;
    if (chunk >= heap->chunk_count) {
        // Grow the (small) array of chunk pointers if needed. Reallocating it
        // only moves the pointers, never the SExp objects the chunks hold.
        if (heap->chunk_count >= heap->chunk_cap) {
            int newcap = heap->chunk_cap == 0 ? 8 : heap->chunk_cap * 2;
            heap->chunks = realloc(heap->chunks, sizeof(SExp *) * newcap);
            heap->chunk_cap = newcap;
        }
        heap->chunks[heap->chunk_count] = malloc(sizeof(SExp) * SEXP_HEAP_CHUNK);
        heap->chunk_count++;
    }
    *SExpHeap_ref(heap, idx) = value;
    heap->size++;
    return idx;
}
