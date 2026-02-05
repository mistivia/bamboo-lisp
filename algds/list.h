#ifndef ALGDS_LIST_H_
#define ALGDS_LIST_H_

#include <stdlib.h>

#include "type_alias.h"


#define LIST_DEF(T) \
    struct T##List_node { \
        T val; \
        struct T##List_node *prev; \
        struct T##List_node *next; \
    }; \
    typedef struct T##List_node T##ListNode; \
    typedef T##ListNode *T##ListIter; \
    typedef struct { \
        T##ListNode *vhead; \
        T##ListNode *vtail; \
        size_t len; \
    } T##List; \
    void T##List_init(T##List *self); \
    void T##List_free(T##List *self); \
    void T##List_move(T##List *self); \
    T##ListIter T##List_insert_before(T##List *self, T##ListIter iter, T val); \
    T##ListIter T##List_insert_after(T##List *self, T##ListIter iter, T val); \
    void T##List_remove(T##List *self, T##ListIter iter); \
    T##ListIter T##List_begin(T##List *self); \
    T##ListIter T##List_last(T##List *self); \
    T##ListIter T##List_end(T##List *self); \
    T##ListIter T##List_next(T##ListIter iter); \
    T##ListIter T##List_prev(T##ListIter iter); \
    size_t T##List_len(T##List *self); \
    T##ListIter T##List_push_back(T##List *self, T val); \
    T##ListIter T##List_push_front(T##List *self, T val); \
    void T##List_pop_back(T##List *self); \
    void T##List_pop_front(T##List *self); \

LIST_DEF(Int);

#define LIST_IMPL(T) \
    void T##List_init(T##List *self) { \
        self->vhead = malloc(sizeof(T##ListNode)); \
        self->vtail = malloc(sizeof(T##ListNode)); \
        self->vhead->next = self->vtail; \
        self->vhead->prev = NULL; \
        self->vtail->next = NULL; \
        self->vtail->prev = self->vhead; \
        self->len = 0; \
    } \
    void T##List_free(T##List *self) { \
        T##ListIter cur = self->vhead; \
        while (cur != NULL) { \
            T##ListIter next = cur->next; \
            free(cur); \
            cur = next; \
        } \
    } \
    void T##List_move(T##List *self) { \
        T##List dup; \
        dup.vhead = self->vhead; \
        dup.vtail = self->vtail; \
        dup.len = self->len; \
        self->vhead = NULL; \
        self->vtail = NULL; \
        self->len = 0; \
    } \
    T##ListIter T##List_insert_before(T##List *self, T##ListIter iter, T val) { \
        if (iter->prev == NULL) return NULL; \
        T##ListIter newnode = malloc(sizeof(T##ListNode)); \
        newnode->prev = iter->prev; \
        newnode->next = iter; \
        newnode->val = val; \
        iter->prev->next = newnode; \
        iter->prev = newnode; \
        self->len++; \
        return newnode; \
    } \
    T##ListIter T##List_insert_after(T##List *self, T##ListIter iter, T val) { \
        if (iter->next == NULL) return NULL; \
        T##ListIter newnode = malloc(sizeof(T##ListNode)); \
        newnode->next = iter->next; \
        newnode->prev= iter; \
        newnode->val = val; \
        iter->next->prev= newnode; \
        iter->next = newnode; \
        self->len++; \
        return newnode; \
    } \
    void T##List_remove(T##List *self, T##ListIter iter) { \
        if (iter->prev == NULL || iter->next == NULL) return; \
        iter->prev->next = iter->next; \
        iter->next->prev = iter->prev; \
        free(iter); \
        self->len--; \
    } \
    T##ListIter T##List_begin(T##List *self) { \
        return self->vhead->next; \
    } \
    T##ListIter T##List_last(T##List *self) { \
        if (self->vtail->prev == self->vhead) return NULL; \
        return self->vtail->prev; \
    } \
    T##ListIter T##List_end(T##List *self) { \
        return self->vtail; \
    } \
    T##ListIter T##List_next(T##ListIter iter) { \
        if (iter == NULL) return NULL; \
        return iter->next; \
    } \
    T##ListIter T##List_prev(T##ListIter iter) { \
        if (iter == NULL) return NULL; \
        if (iter->prev == NULL) return NULL; \
        if (iter->prev->prev == NULL) return NULL; \
        return iter->prev; \
    } \
    size_t T##List_len(T##List *self) { \
        return self->len; \
    } \
    T##ListIter T##List_push_back(T##List *self, T val) { \
        return T##List_insert_before(self, self->vtail, val); \
    } \
    T##ListIter T##List_push_front(T##List *self, T val) { \
        return T##List_insert_after(self, self->vhead, val); \
    } \
    void T##List_pop_back(T##List *self) { \
        T##List_remove(self, self->vtail->prev); \
    } \
    void T##List_pop_front(T##List *self) { \
        T##List_remove(self, self->vhead->next); \
    } \

#endif

