#ifndef HTABLE_H_
#define HTABLE_H_

#include <stdbool.h>
#include <stdint.h>

#include "type_alias.h"

struct hash_table {
    void *buf;
    char *flagbuf;
    int64_t size;
    int64_t cap;
    int64_t taken;
    int64_t elemsz;
};
typedef struct hash_table HashTable;

#define HASH_TABLE_DEF(K, V) \
    typedef struct { \
        K key; \
        V val; \
    } K##2##V##HashTableEntry; \
    typedef K##2##V##HashTableEntry *K##2##V##HashTableIter; \
    typedef struct { \
        HashTable ht; \
    } K##2##V##HashTable; \
    void K##2##V##HashTable_init(K##2##V##HashTable *self); \
    bool K##2##V##HashTable_insert(K##2##V##HashTable *self, K key, V value); \
    void K##2##V##HashTable_remove(K##2##V##HashTable *ht, K##2##V##HashTableIter iter); \
    V* K##2##V##HashTable_get(K##2##V##HashTable *self, K key); \
    K##2##V##HashTableIter K##2##V##HashTable_find(K##2##V##HashTable *self, K key); \
    K##2##V##HashTableIter K##2##V##HashTable_begin(K##2##V##HashTable *self); \
    K##2##V##HashTableIter K##2##V##HashTable_next(K##2##V##HashTable *self, K##2##V##HashTableIter iter); \
    void K##2##V##HashTable_free(K##2##V##HashTable *self); \
    K##2##V##HashTable K##2##V##HashTable_move(K##2##V##HashTable *self); \

#define HASH_TABLE_IMPL(K, V) \
    static uint64_t K##2##V##HashTable_hash(void *vk) { \
        K *k = vk; \
        return K##_hash(*k); \
    } \
    static bool K##2##V##HashTable_eq(void *vk1, void *vk2) { \
        K *k1 = vk1, *k2 = vk2; \
        return K##_eq(*k1, *k2); \
    } \
    void K##2##V##HashTable_init(K##2##V##HashTable *self) { \
        init_hash_table(&self->ht, sizeof(K##2##V##HashTableEntry), 16); \
    } \
    bool K##2##V##HashTable_insert(K##2##V##HashTable *self, K key, V value) { \
        K##2##V##HashTableEntry entry; \
        entry.key = key; \
        entry.val = value; \
        return hash_table_insert(&self->ht, &entry, K##2##V##HashTable_hash, K##2##V##HashTable_eq); \
    } \
    K##2##V##HashTableIter K##2##V##HashTable_find(K##2##V##HashTable *self, K key) { \
        return hash_table_find(&self->ht, &key, K##2##V##HashTable_hash, K##2##V##HashTable_eq); \
    } \
    V* K##2##V##HashTable_get(K##2##V##HashTable *self, K key) { \
        K##2##V##HashTableEntry* entry = hash_table_find(&self->ht, &key, K##2##V##HashTable_hash, K##2##V##HashTable_eq); \
        if (entry == NULL) return NULL; \
        return &(entry->val); \
    } \
    void K##2##V##HashTable_remove(K##2##V##HashTable *self, K##2##V##HashTableIter iter) { \
        hash_table_remove(&self->ht, iter); \
    } \
    K##2##V##HashTableIter K##2##V##HashTable_begin(K##2##V##HashTable *self) { \
        return hash_table_begin(&self->ht); \
    } \
    K##2##V##HashTableIter K##2##V##HashTable_next(K##2##V##HashTable *self, K##2##V##HashTableIter iter) { \
        return hash_table_next(&self->ht, iter); \
    } \
    void K##2##V##HashTable_free(K##2##V##HashTable *self) { \
        destroy_hash_table(&self->ht); \
    } \
    K##2##V##HashTable K##2##V##HashTable_move(K##2##V##HashTable *self) { \
        K##2##V##HashTable dup; \
        dup.ht = self->ht; \
        self->ht.buf = NULL; \
        self->ht.flagbuf = NULL; \
        self->ht.size = 0; \
        self->ht.cap = 0; \
        self->ht.taken = 0; \
        return dup; \
    } \

HASH_TABLE_DEF(String, Int);
HASH_TABLE_DEF(String, String);
HASH_TABLE_DEF(String, Double);
HASH_TABLE_DEF(String, VoidPtr);
HASH_TABLE_DEF(Int, Int);
HASH_TABLE_DEF(Int, Double);
HASH_TABLE_DEF(VoidPtr, Int);
HASH_TABLE_DEF(VoidPtr, String);
HASH_TABLE_DEF(VoidPtr, VoidPtr);

void init_hash_table(HashTable *ht, int64_t elemsz, int64_t cap);
bool hash_table_insert(HashTable *ht, void *elem, uint64_t (*hash)(void*), bool (*eq)(void*, void*));
void hash_table_remove(HashTable *ht, void *iter);
void *hash_table_find(HashTable *ht, void *elem, uint64_t (*hash)(void*), bool (*eq)(void*, void*));
void *hash_table_begin(HashTable *ht);
void *hash_table_next(HashTable *ht, void *iter);
void destroy_hash_table(HashTable *ht);

#endif
