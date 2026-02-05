/*
 * Copyright 2002 Niels Provos <provos@citi.umich.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef RB_TREE_H_
#define RB_TREE_H_

#include <stdlib.h>

#include "type_alias.h"

#define TREE_MAP_DEF(K, V) \
    typedef struct { \
        RBNode rbnode; \
        K key; \
        V value; \
    } K##2##V##TreeMapNode; \
    typedef K##2##V##TreeMapNode *K##2##V##TreeMapIter; \
    typedef struct { \
        RBTree tree; \
    } K##2##V##TreeMap; \
    void K##2##V##TreeMap_init(K##2##V##TreeMap *self); \
    K##2##V##TreeMapIter K##2##V##TreeMap_insert(K##2##V##TreeMap *self, K key, V value); \
    K##2##V##TreeMapIter K##2##V##TreeMap_find(K##2##V##TreeMap *self, K key); \
    V* K##2##V##TreeMap_get(K##2##V##TreeMap *self, K key); \
    K##2##V##TreeMapIter K##2##V##TreeMap_next(K##2##V##TreeMap *self, K##2##V##TreeMapIter iter); \
    K##2##V##TreeMapIter K##2##V##TreeMap_min(K##2##V##TreeMap *self); \
    K##2##V##TreeMapIter K##2##V##TreeMap_max(K##2##V##TreeMap *self); \
    void K##2##V##TreeMap_remove(K##2##V##TreeMap *self, K##2##V##TreeMapIter iter); \
    K##2##V##TreeMapIter K##2##V##TreeMap_left(K##2##V##TreeMapIter iter); \
    K##2##V##TreeMapIter K##2##V##TreeMap_right(K##2##V##TreeMapIter iter); \
    K##2##V##TreeMapIter K##2##V##TreeMap_parent(K##2##V##TreeMapIter iter); \
    void K##2##V##TreeMap_free(K##2##V##TreeMap *self); \

#define TREE_MAP_IMPL(K, V) \
    static int K##2##V##TreeMap_cmp(void *vlhs, void *vrhs) { \
        K *lhs = vlhs, *rhs = vrhs; \
        return K##_cmp(*lhs, *rhs); \
    } \
    void K##2##V##TreeMap_init(K##2##V##TreeMap *self) { \
        self->tree.rbh_root = NULL; \
        self->tree.cmp = K##2##V##TreeMap_cmp; \
        self->tree.augment = NULL; \
    } \
    K##2##V##TreeMapIter K##2##V##TreeMap_insert(K##2##V##TreeMap *self, K key, V value) { \
        K##2##V##TreeMapNode *newnode = malloc(sizeof(K##2##V##TreeMapNode)); \
        newnode->key = key; \
        newnode->value = value; \
        return rb_tree_insert(&self->tree, newnode); \
    } \
    K##2##V##TreeMapIter K##2##V##TreeMap_find(K##2##V##TreeMap *self, K key) { \
        return rb_tree_find(&self->tree, &key); \
    } \
    V* K##2##V##TreeMap_get(K##2##V##TreeMap *self, K key) { \
        K##2##V##TreeMapIter iter = rb_tree_find(&self->tree, &key); \
        if (iter == NULL) return NULL; \
        return &iter->value; \
    } \
    void K##2##V##TreeMap_remove(K##2##V##TreeMap *self, K##2##V##TreeMapIter iter) { \
        rb_tree_remove(&self->tree, iter); \
    } \
    K##2##V##TreeMapIter K##2##V##TreeMap_next(K##2##V##TreeMap *self, K##2##V##TreeMapIter iter) { \
        return rb_tree_next(&self->tree, iter); \
    } \
    K##2##V##TreeMapIter K##2##V##TreeMap_min(K##2##V##TreeMap *self) { \
        return rb_tree_min(&self->tree); \
    } \
    K##2##V##TreeMapIter K##2##V##TreeMap_max(K##2##V##TreeMap *self) { \
        return rb_tree_max(&self->tree); \
    } \
    K##2##V##TreeMapIter K##2##V##TreeMap_left(K##2##V##TreeMapIter iter) { \
        return rb_tree_left(iter); \
    } \
    K##2##V##TreeMapIter K##2##V##TreeMap_right(K##2##V##TreeMapIter iter) { \
        return rb_tree_right(iter); \
    } \
    K##2##V##TreeMapIter K##2##V##TreeMap_parent(K##2##V##TreeMapIter iter) { \
        return rb_tree_parent(iter); \
    } \
    void K##2##V##TreeMap_free(K##2##V##TreeMap *self) { \
        return destroy_rb_tree(&self->tree, NULL); \
    } \

struct rb_node {
    struct {
        struct rb_node *rbe_left;
        struct rb_node *rbe_right;
        struct rb_node *rbe_parent;
        int rbe_color;
    } entry;
    char content[0];
};
typedef struct rb_node RBNode;

struct rb_tree {
    RBNode *rbh_root;
    int (*cmp)(void *k1, void *k2);
    void (*augment)(void *elm);
};
typedef struct rb_tree RBTree;

void rb_tree_remove(RBTree *, void *iter);

// return a iterator
void *rb_tree_insert(RBTree *, void *treenode);
void *rb_tree_find(RBTree *, void *val);
void *rb_tree_next(RBTree *, void *iter);
void *rb_tree_min(RBTree *);
void *rb_tree_max(RBTree *);
void *rb_tree_left(void *node);
void *rb_tree_right(void *node);
void *rb_tree_parent(void *node);

void destroy_rb_tree(RBTree *, void (*freeCb)(void *));

TREE_MAP_DEF(String, Int);
TREE_MAP_DEF(String, String);
TREE_MAP_DEF(String, Double);
TREE_MAP_DEF(String, VoidPtr);
TREE_MAP_DEF(Int, Int);
TREE_MAP_DEF(Int, Double);
TREE_MAP_DEF(VoidPtr, Int);
TREE_MAP_DEF(VoidPtr, String);
TREE_MAP_DEF(VoidPtr, VoidPtr);

#endif

