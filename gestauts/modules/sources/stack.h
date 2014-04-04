#ifndef STACK_H
#define STACK_H

#include <stdlib.h>

#define STACK_DEF(type)                                                             \
    typedef struct type##StackNode_s {                                              \
        type content;                                                               \
        struct type##StackNode_s *next;                                             \
    } * type##StackNode;                                                            \
                                                                                    \
    typedef struct type##Stack_s {                                                  \
        void (*deleteContent)(type);                                                \
        type (*cloneContent)(type);                                                 \
        type##StackNode top;                                                        \
    } * type##Stack;                                                                \
                                                                                    \
    type##StackNode __stackNode##type##New(type content, type##StackNode next) {    \
        type##StackNode node;                                                       \
                                                                                    \
        node = (type##StackNode)malloc(sizeof(struct type##StackNode_s));           \
                                                                                    \
        if (!node)                                                                  \
            return NULL;                                                            \
                                                                                    \
        node->content = content;                                                    \
        node->next = next;                                                          \
                                                                                    \
        return node;                                                                \
    }                                                                               \
                                                                                    \
    type##Stack stack##type##New(void (*deleteContent)(type),                       \
                                 type (*cloneContent)(type)) {                      \
        type##Stack stack;                                                          \
                                                                                    \
        stack = (type##Stack)malloc(sizeof(struct type##Stack_s));                  \
                                                                                    \
        if (!stack)                                                                 \
            return NULL;                                                            \
                                                                                    \
        stack->deleteContent = deleteContent;                                       \
        stack->cloneContent = cloneContent;                                         \
        stack->top = NULL;                                                          \
                                                                                    \
        return stack;                                                               \
    }                                                                               \
                                                                                    \
    int stack##type##Push(type##Stack stack, type content) {                        \
        type##StackNode newNode;                                                    \
                                                                                    \
        newNode = __stackNode##type##New(stack->cloneContent(content), stack->top); \
                                                                                    \
        if (!newNode)                                                               \
            return -1;                                                              \
                                                                                    \
        stack->top = newNode;                                                       \
                                                                                    \
        return 0;                                                                   \
    }                                                                               \
                                                                                    \
    int stack##type##Pull(type##Stack stack, type *ret) {                           \
        type##StackNode next;                                                       \
                                                                                    \
        if (!stack->top)                                                            \
            return -1;                                                              \
                                                                                    \
        next = stack->top->next;                                                    \
                                                                                    \
        *ret = stack->cloneContent(stack->top->content);                            \
        if (stack->deleteContent)                                                   \
            stack->deleteContent(stack->top->content);                              \
        free(stack->top);                                                           \
        stack->top = next;                                                          \
                                                                                    \
        return 0;                                                                   \
    }                                                                               \
                                                                                    \
    void stack##type##Clear(type##Stack stack) {                                    \
        type##StackNode node, nextNode;                                             \
                                                                                    \
        node = stack->top;                                                          \
                                                                                    \
        while(node) {                                                               \
            nextNode = node->next;                                                  \
            if (stack->deleteContent)                                               \
                stack->deleteContent(node->content);                                \
            free(node);                                                             \
            node = nextNode;                                                        \
        }                                                                           \
                                                                                    \
        stack->top = NULL;                                                          \
    }                                                                               \
                                                                                    \
    void stack##type##Destroy(type##Stack stack) {                                  \
                                                                                    \
        stack##type##Clear(stack);                                                  \
                                                                                    \
        free(stack);                                                                \
    }                                                                               \


#define stackNew(type, del, clone) stack##type##New(del, clone)
#define stackPush(type, st, item) stack##type##Push(st, item)
#define stackPull(type, st, ret) stack##type##Pull(st, ret)
#define stackDestroy(type, st) stack##type##Destroy(st)
#define stackClear(type, st) stack##type##Clear(st)

#endif