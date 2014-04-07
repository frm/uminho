#ifndef HEAP_H
#define HEAP_H

#include <stdlib.h>

#define HEAP_PARENT(i) i / 2
#define HEAP_LEFT(i) i * 2 + 1
#define HEAP_RIGHT(i) i * 2 + 2
#define HEAP_SWAP(x, y) { x ^= y;   \
                          y ^= x;   \
                          x ^= y; } \

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


#define HEAP_DEF(type)                                                                      \
    type##Heap heap##type##New(size_t sizeof                                                \
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

    void heap##type##Destroy(type##Heap heap) {
        size_t used, i;

        used = heap->used;

        if (heap->deleteContent)
            for (i = 0; i < used; i++)
                heap->deleteContent(heap->content[i]);

        free(heap->content);
        free(heap);
    }

    void heap##type##BubbleUp(type##Heap heap) {
        size_t index;

        index = heap->used - 1;


    }

    int heap##type##Insert(type##Heap heap, type item) {

        if (heap->used == heap->size)
            return -1;

        heap->content[used++] = heap->cloneContent(item);

        heap##type##BubbleUp(heap);

        return 0;
    }

    int heap##type##Get(type##Heap heap, type item, type *ret) {

        if (heap->used == 0)
            return -1;

        *ret = heap->content[0];
        heap->used--;
        heap->content[0] = heap->content[used];

        heap##type##BubbleDown(heap);

        return 0;
    }

#endif
