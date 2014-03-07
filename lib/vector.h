#ifndef VECTOR_H
#define VECTOR_H

#include <stdlib.h>

#define VECTOR_DEF(type)                                                                    \
    typedef struct type##Block_s {                                                          \
        type *content;                                                                      \
        struct type##Block_s *next;                                                         \
    } * type##Block;                                                                        \
                                                                                            \
    typedef struct type##Vector_s {                                                         \
        size_t blockSize;                                                                   \
        size_t last;                                                                        \
        void (*deleteContent)(type);                                                        \
        type (*cloneContent)(type);                                                         \
        type##Block data;                                                                   \
    } * type##Vector;                                                                       \
                                                                                            \
    type##Block __block##type##New(size_t blockSize) {                                      \
        type##Block newBlock;                                                               \
        type *newContent;                                                                   \
                                                                                            \
        newBlock = (type##Block)malloc(sizeof(struct type##Block_s));                       \
        newContent = (type *)malloc(sizeof(type) * blockSize);                              \
                                                                                            \
        if (!newBlock || !newContent) {                                                     \
            free(newBlock);                                                                 \
            free(newContent);                                                               \
            return NULL;                                                                    \
        }                                                                                   \
                                                                                            \
        newBlock->content = newContent;                                                     \
        newBlock->next = NULL;                                                              \
                                                                                            \
        return newBlock;                                                                    \
    }                                                                                       \
                                                                                            \
    type##Block __block##type##Clone(type##Vector vec, type##Block block) {                 \
        type##Block newBlock;                                                               \
        size_t i, blockSize;                                                                \
                                                                                            \
        if (!block)                                                                         \
            return NULL;                                                                    \
                                                                                            \
        blockSize = vec->blockSize;                                                         \
        newBlock = __block##type##New(blockSize);                                           \
                                                                                            \
        for (i = 0; i < blockSize; i++)                                                     \
            newBlock->content[i] = vec->cloneContent(block->content[i]);                    \
                                                                                            \
        newBlock->next = __block##type##Clone(vec, block->next);                            \
                                                                                            \
        return newBlock;                                                                    \
    }                                                                                       \
                                                                                            \
    type##Vector vec##type##New(size_t blockSize,                                           \
                                void (*deleteContent)(type),                                \
                                type (*cloneContent)(type)) {                               \
        type##Vector newVector;                                                             \
        type##Block newBlock;                                                               \
                                                                                            \
        newVector = (type##Vector)malloc(sizeof(struct type##Vector_s));                    \
        if (!newVector)                                                                     \
            return NULL;                                                                    \
                                                                                            \
        newBlock = __block##type##New(blockSize);                                           \
                                                                                            \
        if (!newBlock) {                                                                    \
            free(newVector);                                                                \
            return NULL;                                                                    \
        }                                                                                   \
                                                                                            \
        newVector->deleteContent = deleteContent;                                           \
        newVector->cloneContent = cloneContent;                                             \
        newVector->blockSize = blockSize;                                                   \
        newVector->last = -1;                                                               \
        newVector->data = newBlock;                                                         \
                                                                                            \
        return newVector;                                                                   \
    }                                                                                       \
                                                                                            \
    type##Vector vec##type##Clone(type##Vector vec) {                                       \
        type##Vector newVec;                                                                \
        size_t blockSize;                                                                   \
                                                                                            \
        blockSize = vec->blockSize;                                                         \
        newVec = (type##Vector)malloc(sizeof(struct type##Vector_s));                       \
                                                                                            \
        newVec->last = vec->last;                                                           \
        newVec->deleteContent = vec->deleteContent;                                         \
        newVec->cloneContent = vec->cloneContent;                                           \
        newVec->blockSize = blockSize;                                                      \
        newVec->data = __block##type##Clone(vec, vec->data);                                \
                                                                                            \
        return newVec;                                                                      \
    }                                                                                       \
                                                                                            \
    void vec##type##Destroy(type##Vector vec) {                                             \
        type##Block temp1;                                                                  \
        type##Block temp2;                                                                  \
        size_t blockSize, index, i;                                                         \
                                                                                            \
        index = vec->last;                                                                  \
        blockSize = vec->blockSize;                                                         \
        temp2 = vec->data;                                                                  \
                                                                                            \
        while(temp2) {                                                                      \
            temp1 = temp2->next;                                                            \
                                                                                            \
            if (index < blockSize)                                                          \
                blockSize = index;                                                          \
                                                                                            \
            for (i = 0; i < blockSize; i++)                                                 \
                vec->deleteContent(temp2->content[i]);                                      \
                                                                                            \
            free(temp2->content);                                                           \
            free(temp2);                                                                    \
            index -= blockSize;                                                             \
            temp2 = temp1;                                                                  \
        }                                                                                   \
                                                                                            \
        free(vec);                                                                          \
                                                                                            \
        return;                                                                             \
    }                                                                                       \
                                                                                            \
    int vec##type##Append(type##Vector vec, type item) {                                    \
        type##Block currBlock;                                                              \
        size_t index, blockSize;                                                            \
                                                                                            \
        blockSize = vec->blockSize;                                                         \
        index = vec->last + 1;                                                              \
        currBlock = vec->data;                                                              \
                                                                                            \
        while (index > blockSize) {                                                         \
            index -= blockSize;                                                             \
            currBlock = currBlock->next;                                                    \
        }                                                                                   \
                                                                                            \
        if (index == blockSize) {                                                           \
            index -= blockSize;                                                             \
            currBlock->next = __block##type##New(blockSize);                                \
            currBlock = currBlock->next;                                                    \
                                                                                            \
            if (!currBlock)                                                                 \
                return -1;                                                                  \
        }                                                                                   \
                                                                                            \
        vec->last += 1;                                                                     \
        currBlock->content[index] = vec->cloneContent(item);                                \
                                                                                            \
        return 0;                                                                           \
    }                                                                                       \
                                                                                            \
    int vec##type##Get(type##Vector vec, size_t index, type *ret) {                         \
        type##Block currBlock;                                                              \
        size_t blockSize;                                                                   \
                                                                                            \
        blockSize = vec->blockSize;                                                         \
        currBlock = vec->data;                                                              \
                                                                                            \
        if (index > vec->last + 1)                                                          \
            return -1;                                                                      \
                                                                                            \
        while (index >= blockSize) {                                                        \
            index -= blockSize;                                                             \
            currBlock = currBlock->next;                                                    \
        }                                                                                   \
                                                                                            \
        *ret = vec->cloneContent(currBlock->content[index]);                                \
                                                                                            \
        return 0;                                                                           \
    }                                                                                       \
                                                                                            \
    int vec##type##Update(type##Vector vec, size_t index, type item) {                      \
        type##Block currBlock;                                                              \
        size_t blockSize;                                                                   \
                                                                                            \
        blockSize = vec->blockSize;                                                         \
        currBlock = vec->data;                                                              \
                                                                                            \
        while (index >= blockSize) {                                                        \
            if (!currBlock->next) {                                                         \
                currBlock->next = __block##type##New(blockSize);                            \
                                                                                            \
                if (!currBlock->next)                                                       \
                    return -1;                                                              \
            }                                                                               \
            index -= blockSize;                                                             \
            currBlock = currBlock->next;                                                    \
        }                                                                                   \
                                                                                            \
        vec->deleteContent(currBlock->content[index]);                                      \
        currBlock->content[index] = vec->cloneContent(item);                                \
                                                                                            \
        return 0;                                                                           \
    }                                                                                       \

#define vecUpdate(type, index, item) vec##type##Update(index, item)
#define vecClone(type, vector) vec##type##Clone(vector)
#define vecNew(type, blockSize, del, clone) vec##type##New(blockSize, del, clone)
#define vecDestroy(type, vector) vec##type##Destroy(vector)
#define vecAppend(type, vector, item) vec##type##Append(vector, item)
#define vecGet(type, vector, index, ret) vec##type##Get(vector, index, ret)

#endif