#ifndef AVL_H
#define AVL_H

#include <stdlib.h>
#include <stdio.h>
#include "stack.h"

#define AVL_DEF(type, keyType)                                                              \
    typedef struct type##AVLNode_s {                                                        \
        type content;                                                                       \
        int balance;                                                                        \
        struct type##AVLNode_s *left, *right;                                               \
    } * type##AVLNode;                                                                      \
                                                                                            \
    STACK_DEF(type##AVLNode)                                                                \
                                                                                            \
    typedef struct type##AVL_s {                                                            \
        struct {                                                                            \
            type##AVLNodeStack stack;                                                       \
            int initialized;                                                                \
        } generator;                                                                        \
        type##AVLNode root;                                                                 \
        int (*compare)(keyType *, type *, type);                                            \
        void (*collision)(type *, type *);                                                  \
        void (*deleteContent)(type);                                                        \
        type (*cloneContent)(type);                                                         \
    } * type##AVL;                                                                          \
                                                                                            \
                                                                                            \
    type##AVLNode __avl##type##StClone(type##AVLNode node) {                                \
        return node;                                                                        \
    }                                                                                       \
                                                                                            \
    void avl##type##StackMin(type##AVLNodeStack stack, type##AVLNode node) {                \
                                                                                            \
        while (node) {                                                                      \
            stackPush(type##AVLNode, stack, node);                                          \
            node = node->left;                                                              \
        }                                                                                   \
                                                                                            \
        return;                                                                             \
    }                                                                                       \
                                                                                            \
    int avl##type##Yield(type##AVL avl, type *ret) {                                        \
        int initialized = avl->generator.initialized;                                       \
        type##AVLNode node;                                                                 \
                                                                                            \
        if (!initialized && ret) {                                                          \
            avl->generator.stack = stackNew(type##AVLNode, NULL, &__avl##type##StClone);    \
            avl->generator.initialized = 1;                                                 \
            avl##type##StackMin(avl->generator.stack, avl->root);                           \
        }                                                                                   \
                                                                                            \
        if (!ret) {                                                                         \
            if (initialized) {                                                              \
                stackDestroy(type##AVLNode, avl->generator.stack);                          \
                avl->generator.initialized = 0;                                             \
            }                                                                               \
        } else {                                                                            \
            if (!stackPull(type##AVLNode, avl->generator.stack, &node)){                    \
                avl##type##StackMin(avl->generator.stack, node->right);                     \
                *ret = avl->cloneContent(node->content);                                    \
            } else {                                                                        \
                avl##type##StackMin(avl->generator.stack, avl->root);                       \
                return 1;                                                                   \
            }                                                                               \
        }                                                                                   \
                                                                                            \
        return 0;                                                                           \
    }                                                                                       \
                                                                                            \
    type##AVLNode __avlNode##type##New(type content) {                                      \
        type##AVLNode node;                                                                 \
                                                                                            \
        node = (type##AVLNode)malloc(sizeof(struct type##AVLNode_s));                       \
                                                                                            \
        if (!node)                                                                          \
            return NULL;                                                                    \
                                                                                            \
        node->content = content;                                                            \
        node->balance = 0;                                                                  \
        node->left = node->right = NULL;                                                    \
                                                                                            \
        return node;                                                                        \
    }                                                                                       \
                                                                                            \
    type##AVLNode __avlNode##type##Clone(type##AVL avl, type##AVLNode node) {               \
        type##AVLNode newNode;                                                              \
                                                                                            \
        newNode = NULL;                                                                     \
                                                                                            \
        if (node) {                                                                         \
            newNode = __avlNode##type##New(avl->cloneContent(node->content));               \
                                                                                            \
            newNode->balance = node->balance;                                               \
            newNode->right = __avlNode##type##Clone(avl, node->right);                      \
            newNode->left = __avlNode##type##Clone(avl, node->left);                        \
        }                                                                                   \
                                                                                            \
        return newNode;                                                                     \
    }                                                                                       \
                                                                                            \
    type##AVL avl##type##New(int (*compare)(keyType *, type *, type),                       \
                             void (*collision)(type *, type *),                             \
                             void (*deleteContent)(type),                                   \
                             type (*cloneContent)(type)) {                                  \
        type##AVL avl;                                                                      \
                                                                                            \
        avl = (type##AVL)malloc(sizeof(struct type##AVL_s));                                \
                                                                                            \
        if (!avl)                                                                           \
            return NULL;                                                                    \
                                                                                            \
        avl->root = NULL;                                                                   \
        avl->compare = compare;                                                             \
        avl->collision = collision;                                                         \
        avl->deleteContent = deleteContent;                                                 \
        avl->cloneContent = cloneContent;                                                   \
        avl->generator.initialized = 0;                                                     \
        avl->generator.stack = NULL;                                                        \
                                                                                            \
        return avl;                                                                         \
    }                                                                                       \
                                                                                            \
    type##AVL avl##type##Clone(type##AVL avl) {                                             \
        type##AVL newAvl;                                                                   \
                                                                                            \
        newAvl = avl##type##New(avl->compare,                                               \
                                avl->collision,                                             \
                                avl->deleteContent,                                         \
                                avl->cloneContent);                                         \
                                                                                            \
        newAvl->root = __avlNode##type##Clone(newAvl, avl->root);                           \
                                                                                            \
        return newAvl;                                                                      \
    }                                                                                       \
                                                                                            \
    void __avl##type##DestroyNode(void          (*deleteContent)(type),                     \
                                  type##AVLNode node) {                                     \
                                                                                            \
        if (node) {                                                                         \
            if (deleteContent)                                                              \
                deleteContent(node->content);                                               \
            __avl##type##DestroyNode(deleteContent, node->left);                            \
            __avl##type##DestroyNode(deleteContent, node->right);                           \
            free(node);                                                                     \
        }                                                                                   \
                                                                                            \
        return;                                                                             \
    }                                                                                       \
                                                                                            \
    void avl##type##Destroy(type##AVL avl) {                                                \
                                                                                            \
        __avl##type##DestroyNode(avl->deleteContent, avl->root);                            \
        avl->root = NULL;                                                                   \
        if (avl->generator.initialized)                                                     \
            avl##type##Yield(avl, NULL);                                                    \
        free(avl);                                                                          \
                                                                                            \
        return;                                                                             \
    }                                                                                       \
                                                                                            \
    type##AVLNode __avl##type##RotateRight(type##AVLNode node) {                            \
        type##AVLNode auxNode;                                                              \
                                                                                            \
        auxNode = node->left;                                                               \
        node->left = auxNode->right;                                                        \
        auxNode->right = node;                                                              \
                                                                                            \
        return auxNode;                                                                     \
    }                                                                                       \
                                                                                            \
    type##AVLNode __avl##type##RotateLeft(type##AVLNode node) {                             \
        type##AVLNode auxNode;                                                              \
                                                                                            \
        auxNode = node->right;                                                              \
        node->right = auxNode->left;                                                        \
        auxNode->left = node;                                                               \
                                                                                            \
        return auxNode;                                                                     \
    }                                                                                       \
                                                                                            \
    type##AVLNode __avl##type##BalanceRight(type##AVLNode node, int *growth) {              \
        int balance;                                                                        \
                                                                                            \
        balance = ++node->balance;                                                          \
                                                                                            \
        if (balance > 1) {                                                                  \
            if (node->right->balance == 1) {                                                \
                /* Simple left rotation */                                                  \
                node = __avl##type##RotateLeft(node);                                       \
                node->left->balance = 0;                                                    \
            } else {                                                                        \
                /* Double rotation (right child goes right, this goes left) */              \
                node->right = __avl##type##RotateRight(node->right);                        \
                node = __avl##type##RotateLeft(node);                                       \
                                                                                            \
                if (node->balance == -1) {                                                  \
                    node->left->balance = 0;                                                \
                    node->right->balance = 1;                                               \
                } else if (node->balance == 1) {                                            \
                    node->left->balance = -1;                                               \
                    node->right->balance = 0;                                               \
                } else {                                                                    \
                    node->left->balance = 0;                                                \
                    node->right->balance = 0;                                               \
                }                                                                           \
            }                                                                               \
                                                                                            \
            node->balance = 0;                                                              \
            *growth = 0;                                                                    \
        } else if (balance == 0) {                                                          \
            *growth = 0;                                                                    \
        }                                                                                   \
                                                                                            \
        return node;                                                                        \
    }                                                                                       \
                                                                                            \
    type##AVLNode __avl##type##BalanceLeft(type##AVLNode node, int *growth) {               \
        int balance;                                                                        \
                                                                                            \
        balance = --node->balance;                                                          \
                                                                                            \
        if (balance < -1) {                                                                 \
            if (node->left->balance == -1) {                                                \
                /* Simple right rotation */                                                 \
                node = __avl##type##RotateRight(node);                                      \
                node->right->balance = 0;                                                   \
            } else {                                                                        \
                /* Double rotation (left child goes left, this goes right) */               \
                node->left = __avl##type##RotateLeft(node->left);                           \
                node = __avl##type##RotateRight(node);                                      \
                                                                                            \
                if (node->balance == -1) {                                                  \
                    node->left->balance = 0;                                                \
                    node->right->balance = 1;                                               \
                } else if (node->balance == 1) {                                            \
                    node->left->balance = -1;                                               \
                    node->right->balance = 0;                                               \
                } else {                                                                    \
                    node->left->balance = 0;                                                \
                    node->right->balance = 0;                                               \
                }                                                                           \
            }                                                                               \
                                                                                            \
            node->balance = 0;                                                              \
            *growth = 0;                                                                    \
        } else if (balance == 0) {                                                          \
            *growth = 0;                                                                    \
        }                                                                                   \
                                                                                            \
        return node;                                                                        \
    }                                                                                       \
                                                                                            \
    type##AVLNode __avl##type##Insert(type##AVL       avl,                                  \
                                      type##AVLNode node,                                   \
                                      type##AVLNode newNode,                                \
                                      int           *growth,                                \
                                      int           *col) {                                 \
        int cmp;                                                                            \
                                                                                            \
        cmp = avl->compare(NULL, &(newNode->content), node->content);                       \
                                                                                            \
        if (cmp == 0){                                                                      \
            *col = 1;                                                                       \
            avl->collision(&(node->content), &(newNode->content));                          \
            __avl##type##DestroyNode(avl->deleteContent, newNode);                          \
            *growth = 0;                                                                    \
        } else if (cmp == 1) {                                                              \
                                                                                            \
            if (!node->right) {                                                             \
                *col = 0;                                                                   \
                node->right = newNode;                                                      \
                                                                                            \
                if (++node->balance)                                                        \
                    *growth = 1;                                                            \
                                                                                            \
            } else {                                                                        \
                node->right =__avl##type##Insert(avl, node->right, newNode, growth, col);   \
                                                                                            \
                if (*growth) {                                                              \
                    node = __avl##type##BalanceRight(node, growth);                         \
                }                                                                           \
            }                                                                               \
                                                                                            \
        } else {                                                                            \
                                                                                            \
            if (!node->left) {                                                              \
                *col = 0;                                                                   \
                node->left = newNode;                                                       \
                                                                                            \
                if (--node->balance)                                                        \
                    *growth = 1;                                                            \
                                                                                            \
            } else {                                                                        \
                node->left = __avl##type##Insert(avl, node->left, newNode, growth, col);    \
                                                                                            \
                if (*growth) {                                                              \
                    node = __avl##type##BalanceLeft(node, growth);                          \
                }                                                                           \
            }                                                                               \
        }                                                                                   \
                                                                                            \
        return node;                                                                        \
    }                                                                                       \
                                                                                            \
    int avl##type##Insert(type##AVL avl, type item) {                                       \
        type##AVLNode newNode;                                                              \
        int growth, col;                                                                    \
                                                                                            \
        growth = 0;                                                                         \
        col = 0;                                                                            \
        newNode = __avlNode##type##New(avl->cloneContent(item));                            \
                                                                                            \
        if (!newNode)                                                                       \
            return -1;                                                                      \
                                                                                            \
        if (!avl->root){                                                                    \
            col = 0;                                                                        \
            avl->root = newNode;                                                            \
        }                                                                                   \
        else {                                                                              \
            avl->root = __avl##type##Insert(avl, avl->root, newNode, &growth, &col);        \
        }                                                                                   \
                                                                                            \
        if (avl->generator.initialized)                                                     \
            avl##type##Yield(avl, NULL);                                                    \
                                                                                            \
        return col;                                                                         \
    }                                                                                       \
                                                                                            \
    type##AVLNode __avl##type##Find(int           (*compare)(keyType *, type *, type),      \
                                    type##AVLNode node,                                     \
                                    type *keyContent,                                       \
                                    keyType       *key) {                                   \
        int cmp;                                                                            \
                                                                                            \
        if (!node)                                                                          \
            return NULL;                                                                    \
                                                                                            \
        cmp = compare(key, keyContent, node->content);                                      \
                                                                                            \
        if (cmp == 1) {                                                                     \
            return __avl##type##Find(compare, node->right, keyContent, key);                \
        } else if (cmp == -1) {                                                             \
            return __avl##type##Find(compare, node->left, keyContent, key);                 \
        } else {                                                                            \
            return node;                                                                    \
        }                                                                                   \
    }                                                                                       \
                                                                                            \
    int avl##type##Find(type##AVL avl, keyType key, type *ret) {                            \
        type##AVLNode node;                                                                 \
                                                                                            \
        node = __avl##type##Find(avl->compare, avl->root, NULL, &key);                      \
                                                                                            \
        if (!node)                                                                          \
            return -1;                                                                      \
                                                                                            \
        *ret = avl->cloneContent(node->content);                                            \
                                                                                            \
        return 0;                                                                           \
    }                                                                                       \
                                                                                            \
    int avl##type##Update(type##AVL avl, type item) {                                       \
        type##AVLNode node;                                                                 \
                                                                                            \
        node = __avl##type##Find(avl->compare, avl->root, &item, NULL);                     \
                                                                                            \
        if (!node)                                                                          \
            return -1;                                                                      \
                                                                                            \
        node->content = avl->cloneContent(item);                                            \
                                                                                            \
        return 0;                                                                           \
    }                                                                                       \
                                                                                            \
    type##AVLNode avl##type##GetLeftChild(type##AVLNode node) {                             \
        return node->left;                                                                  \
    }                                                                                       \
                                                                                            \
    type##AVLNode avl##type##GetRightChild(type##AVLNode node) {                            \
        return node->right;                                                                 \
    }                                                                                       \
                                                                                            \
    type##AVLNode avl##type##GetRoot(type##AVL avl) {                                       \
        return avl->root;                                                                   \
    }                                                                                       \
                                                                                            \
    type avl##type##GetNodeContent(type##AVLNode node) {                                    \
        return node->content;                                                               \
    }                                                                                       \

#define avlGetLeftChild(type, node) avl##type##GetLeftChild(node)
#define avlGetRightChild(type, node) avl##type##GetRightChild(node)
#define avlGetRoot(type, tree) avl##type##GetRoot(tree)
#define avlGetNodeContent(type, node) avl##type##GetNodeContent(node)
#define avlInsert(type, tree, item) avl##type##Insert(tree, item)
#define avlNew(type, cmp, col, del, clone) avl##type##New(cmp, col, del, clone)
#define avlDestroy(type, tree) avl##type##Destroy(tree)
#define avlFind(type, tree, key, ret) avl##type##Find(tree, key, ret)
#define avlUpdate(type, tree, item) avl##type##Update(tree, item)
#define avlClone(type, tree) avl##type##Clone(tree)
#define avlYield(type, tree, ret) avl##type##Yield(tree, ret)
#define avlRewindGenerator(type, tree) avl##type##Yield(tree, NULL)

#endif