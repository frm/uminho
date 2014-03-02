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
        size_t nextIndex;                                                                   \
        type##Block data;                                                                   \
    } * type##Vector;                                                                       \
                                                                                            \
    type##Vector vec##type##New(size_t blockSize) {                                         \
        type##Vector newVector;                                                             \
        type##Block newBlock;                                                               \
        type *newContent;                                                                   \
                                                                                            \
        newVector = (type##Vector)malloc(sizeof(struct type##Vector_s));                    \
        if (!newVector)                                                                     \
            return NULL;                                                                    \
                                                                                            \
        newBlock = (type##Block)malloc(sizeof(struct type##Block_s));                       \
        if (!newBlock) {                                                                    \
            free(newVector);                                                                \
            return NULL;                                                                    \
        }                                                                                   \
                                                                                            \
        newContent = (type *)malloc(sizeof(type) * blockSize);                              \
        if (!newContent) {                                                                  \
            free(newVector);                                                                \
            free(newBlock);                                                                 \
            return NULL;                                                                    \
        }                                                                                   \
                                                                                            \
        newVector->blockSize = blockSize;                                                   \
        newVector->nextIndex = 0;                                                           \
        newVector->data = newBlock;                                                         \
        newVector->data->next = NULL;                                                       \
        newVector->data->content = newContent;                                              \
                                                                                            \
        return newVector;                                                                   \
    }                                                                                       \
                                                                                            \
    void vec##type##Destroy(type##Vector vec) {                                             \
        type##Block temp1;                                                                  \
        type##Block temp2;                                                                  \
                                                                                            \
        temp1 = vec->data;                                                                  \
        temp2 = temp1;                                                                      \
        free(vec);                                                                          \
                                                                                            \
        while(temp1) {                                                                      \
            temp1 = temp2->next;                                                            \
            free(temp2->content);                                                           \
            free(temp2);                                                                    \
            temp2 = temp1;                                                                  \
        }                                                                                   \
                                                                                            \
        return;                                                                             \
    }                                                                                       \
                                                                                            \
    int vec##type##Append(type##Vector vec, type item) {                                    \
        type##Block currBlock;                                                              \
        size_t index, blockSize;                                                            \
                                                                                            \
        blockSize = vec->blockSize;                                                         \
        index = vec->nextIndex;                                                             \
        currBlock = vec->data;                                                              \
                                                                                            \
        while (index > blockSize) {                                                         \
            index -= blockSize;                                                             \
            currBlock = currBlock->next;                                                    \
        }                                                                                   \
                                                                                            \
        if (index == blockSize) {                                                           \
            index -= blockSize;                                                             \
            currBlock->next = (type##Block)malloc(sizeof(struct type##Block_s));            \
            currBlock = currBlock->next;                                                    \
            if (!currBlock) {                                                               \
                return -1;                                                                  \
            }                                                                               \
                                                                                            \
            currBlock->content = (type *)malloc(sizeof(type) * blockSize);                  \
            if (!currBlock->content) {                                                      \
                free(currBlock);                                                            \
                return -1;                                                                  \
            }                                                                               \
        }                                                                                   \
                                                                                            \
        vec->nextIndex += 1;                                                                \
        currBlock->content[index] = item;                                                   \
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
        if (index >= vec->nextIndex)                                                        \
            return -1;                                                                      \
                                                                                            \
        while (index >= blockSize) {                                                        \
            index -= blockSize;                                                             \
            currBlock = currBlock->next;                                                    \
        }                                                                                   \
                                                                                            \
        *ret = currBlock->content[index];                                                   \
                                                                                            \
        return 0;                                                                           \
    }                                                                                       \

#define vecNew(type, blockSize) vec##type##New(blockSize)
#define vecDestroy(type, vector) vec##type##Destroy(vector)
#define vecAppend(type, vector, item) vec##type##Append(vector, item)
#define vecGet(type, vector, index, ret) vec##type##Get(vector, index, ret)

#endif
