#ifndef HEAP_H
#define HEAP_H

#include <stdlib.h>

#define HEAP_PARENT(i) i / 2
#define HEAP_LEFT(i) i * 2 + 1
#define HEAP_RIGHT(i) i * 2 + 2

#define HEAP_DEF_HEADER(type)                                                               \
    typedef struct type##Heap_s {                                                           \
        int (*compareContent)(type, type);                                                  \
        void (*deleteContent)(type);                                                        \
        type (*cloneContent)(type);                                                         \
        size_t size;                                                                        \
        size_t used;                                                                        \
        type *content;                                                                      \
    } * type##Heap;                                                                         \
                                                                                            \
    type##Heap heap##type##New(size_t,                                                      \
                               int (*)(type, type),                                         \
                               void (*)(type),                                              \
                               type (*)(type));                                             \
    void heap##type##Destroy(type##Heap);                                                   \
    int heap##type##Insert(type##Heap, type);                                               \
    int heap##type##Get(type##Heap, type *);                                                \


#define HEAP_DEF(type)                                                                      \
    type##Heap heap##type##New(size_t size,                                                 \
                               int (*compareContent)(type, type),                           \
                               void (*deleteContent)(type),                                 \
                               type (*cloneContent)(type)) {                                \
        type##Heap newHeap;                                                                 \
                                                                                            \
        newHeap = (type##Heap)malloc(sizeof(struct type##Heap_s));                          \
        newHeap->content = (type *)malloc(sizeof(type) * size);                             \
                                                                                            \
        newHeap->compareContent = compareContent;                                           \
        newHeap->deleteContent = deleteContent;                                             \
        newHeap->cloneContent = cloneContent;                                               \
        newHeap->size = size;                                                               \
        newHeap->used = 0;                                                                  \
                                                                                            \
        return newHeap;                                                                     \
    }                                                                                       \
                                                                                            \
    void heap##type##Destroy(type##Heap heap) {                                             \
        size_t used, i;                                                                     \
                                                                                            \
        used = heap->used;                                                                  \
                                                                                            \
        if (heap->deleteContent)                                                            \
            for (i = 0; i < used; i++)                                                      \
                heap->deleteContent(heap->content[i]);                                      \
                                                                                            \
        free(heap->content);                                                                \
        free(heap);                                                                         \
    }                                                                                       \
                                                                                            \
    void heap##type##BubbleUp(type##Heap heap) {                                            \
        size_t index;                                                                       \
        type contentChild, contentParent;                                                   \
                                                                                            \
        index = heap->used - 1;                                                             \
        contentChild = heap->content[index];                                                \
        contentParent = heap->content[HEAP_PARENT(index)];                                  \
                                                                                            \
        while (heap->compareContent(contentChild, contentParent) > 0) {                     \
                                                                                            \
            heap->content[index] = contentParent;                                           \
            heap->content[HEAP_PARENT(index)] = contentChild;                               \
            index = HEAP_PARENT(index);                                                     \
                                                                                            \
            contentChild = heap->content[index];                                            \
            contentParent = heap->content[HEAP_PARENT(index)];                              \
        }                                                                                   \
    }                                                                                       \
                                                                                            \
    void heap##type##BubbleDown(type##Heap heap) {                                          \
        size_t index, candidate, max;                                                       \
        type contentCandidate, contentIndex;                                                \
                                                                                            \
        index = 0;                                                                          \
        candidate = HEAP_LEFT(index);                                                       \
        max = heap->used - 1;                                                               \
                                                                                            \
        while(candidate <= max) {                                                           \
            if (candidate + 1 <= max) {                                                     \
                if (heap->compareContent(heap->content[candidate],                          \
                                         heap->content[candidate + 1]) < 0)                 \
                    candidate++;                                                            \
            }                                                                               \
                                                                                            \
            contentIndex = heap->content[index];                                            \
            contentCandidate = heap->content[candidate];                                    \
                                                                                            \
            if (heap->compareContent(contentIndex, contentCandidate) < 0) {                 \
                heap->content[index] = contentCandidate;                                    \
                heap->content[candidate] = contentIndex;                                    \
                index = candidate;                                                          \
                candidate = HEAP_LEFT(index);                                               \
            }                                                                               \
            else {                                                                          \
                break;                                                                      \
            }                                                                               \
        }                                                                                   \
    }                                                                                       \
                                                                                            \
    int heap##type##Insert(type##Heap heap, type item) {                                    \
                                                                                            \
        if (heap->used == heap->size)                                                       \
            return -1;                                                                      \
                                                                                            \
        if (heap->cloneContent)                                                             \
            heap->content[heap->used++] = heap->cloneContent(item);                         \
        else                                                                                \
            heap->content[heap->used++] = item;                                             \
                                                                                            \
        heap##type##BubbleUp(heap);                                                         \
                                                                                            \
        return 0;                                                                           \
    }                                                                                       \
                                                                                            \
    int heap##type##Get(type##Heap heap, type *ret) {                                       \
                                                                                            \
        if (heap->used == 0)                                                                \
            return -1;                                                                      \
                                                                                            \
        *ret = heap->content[0];                                                            \
        heap->used--;                                                                       \
        heap->content[0] = heap->content[heap->used];                                       \
                                                                                            \
        heap##type##BubbleDown(heap);                                                       \
                                                                                            \
        return 0;                                                                           \
    }                                                                                       \

#define heapNewComplete(type, size, cmp, del, clone) heap##type##New(size, cmp, del, clone)
#define heapNew(type, size, cmp) heap##type##New(size, cmp, NULL, NULL)
#define heapDestroy(type, hp) heap##type##Destroy(hp)
#define heapInsert(type, hp, item) heap##type##Insert(hp, item)
#define heapGet(type, hp, ret) heap##type##Get(hp, ret)

#endif
