#ifndef AVL_H
#define AVL_H

#include <stdlib.h> 
#include <stdio.h>

#define AVL_DEF(type)                                                                   \
    typedef struct type##AVLNode_s {                                                    \
        type content;                                                                   \
        int balance;                                                                    \
        struct type##AVLNode_s *left, *right;                                           \
    } * type##AVLNode;                                                                  \
                                                                                        \
    typedef struct type##AVL_s {                                                        \
        type##AVLNode root;                                                             \
        int (*cmp)(type, type);                                                         \
    } * type##AVL;                                                                      \
                                                                                        \
    type##AVL avl##type##New(int (*cmp)(type, type)) {                                  \
        type##AVL avl;                                                                  \
                                                                                        \
        avl = (type##AVL)malloc(sizeof(struct type##AVL_s));                            \
                                                                                        \
        if (avl == NULL) {                                                              \
            return NULL;                                                                \
        }                                                                               \
                                                                                        \
        avl->root = NULL;                                                               \
        avl->cmp = cmp;                                                                 \
                                                                                        \
        return avl;                                                                     \
    }                                                                                   \
                                                                                        \
    void __avl##type##DestroyNodes(type##AVLNode node) {                                \
                                                                                        \
        if (node) {                                                                     \
            __avl##type##DestroyNodes(node->left);                                      \
            __avl##type##DestroyNodes(node->right);                                     \
            free(node);                                                                 \
        }                                                                               \
                                                                                        \
        return;                                                                         \
    }                                                                                   \
                                                                                        \
    void avl##type##Destroy(type##AVL avl) {                                            \
                                                                                        \
        __avl##type##DestroyNodes(avl->root);                                           \
        free(avl);                                                                      \
                                                                                        \
        return;                                                                         \
    }                                                                                   \
                                                                                        \
    type##AVLNode __avl##type##RotateRight(type##AVLNode node) {                        \
        type##AVLNode auxNode;                                                          \
                                                                                        \
        auxNode = node->left;                                                           \
        node->left = auxNode->right;                                                    \
        auxNode->right = node;                                                          \
                                                                                        \
        return auxNode;                                                                 \
    }                                                                                   \
                                                                                        \
    type##AVLNode __avl##type##RotateLeft(type##AVLNode node) {                         \
        type##AVLNode auxNode;                                                          \
                                                                                        \
        auxNode = node->right;                                                          \
        node->right = auxNode->left;                                                    \
        auxNode->left = node;                                                           \
                                                                                        \
        return auxNode;                                                                 \
    }                                                                                   \
                                                                                        \
    type##AVLNode __avl##type##BalanceRight(type##AVLNode node, int *growth) {          \
        int balance;                                                                    \
                                                                                        \
        balance = ++node->balance;                                                      \
                                                                                        \
        if (balance > 1) {                                                              \
            if (node->right->balance == 1) {                                            \
                /* Simple left rotation */                                              \
                node = __avl##type##RotateLeft(node);                                   \
                node->left->balance = 0;                                                \
            } else {                                                                    \
                /* Double rotation (right child goes right, this goes left) */          \
                node->right = __avl##type##RotateRight(node->right);                    \
                node = __avl##type##RotateLeft(node);                                   \
                                                                                        \
                if (node->balance == -1) {                                              \
                    node->left->balance = 0;                                            \
                    node->right->balance = 1;                                           \
                } else if (node->balance == 1) {                                        \
                    node->left->balance = -1;                                           \
                    node->right->balance = 0;                                           \
                } else {                                                                \
                    node->left->balance = 0;                                            \
                    node->right->balance = 0;                                           \
                }                                                                       \
            }                                                                           \
                                                                                        \
            node->balance = 0;                                                          \
            *growth = 0;                                                                \
        } else if (balance == 0) {                                                      \
            *growth = 0;                                                                \
        }                                                                               \
                                                                                        \
        return node;                                                                    \
    }                                                                                   \
                                                                                        \
    type##AVLNode __avl##type##BalanceLeft(type##AVLNode node, int *growth) {           \
        int balance;                                                                    \
                                                                                        \
        balance = --node->balance;                                                      \
                                                                                        \
        if (balance < -1) {                                                             \
            if (node->left->balance == -1) {                                            \
                /* Simple right rotation */                                             \
                node = __avl##type##RotateRight(node);                                  \
                node->right->balance = 0;                                               \
            } else {                                                                    \
                /* Double rotation (left child goes left, this goes right) */           \
                node->left = __avl##type##RotateLeft(node->left);                       \
                node = __avl##type##RotateRight(node);                                  \
                                                                                        \
                if (node->balance == -1) {                                              \
                    node->left->balance = 0;                                            \
                    node->right->balance = 1;                                           \
                } else if (node->balance == 1) {                                        \
                    node->left->balance = -1;                                           \
                    node->right->balance = 0;                                           \
                } else {                                                                \
                    node->left->balance = 0;                                            \
                    node->right->balance = 0;                                           \
                }                                                                       \
            }                                                                           \
                                                                                        \
            node->balance = 0;                                                          \
            *growth = 0;                                                                \
        } else if (balance == 0) {                                                      \
            *growth = 0;                                                                \
        }                                                                               \
                                                                                        \
        return node;                                                                    \
    }                                                                                   \
                                                                                        \
    type##AVLNode __avl##type##Insert(int           (*compare)(type, type),             \
                                      type##AVLNode node,                               \
                                      type##AVLNode newNode,                            \
                                      int           *growth) {                          \
        int cmp;                                                                        \
                                                                                        \
        cmp = compare(newNode->content, node->content);                                 \
                                                                                        \
        if (cmp == 0){                                                                  \
            node->content = newNode->content;                                           \
            free(newNode);                                                              \
            *growth = 0;                                                                \
        } else if (cmp == 1) {                                                          \
                                                                                        \
            if (!node->right) {                                                         \
                node->right = newNode;                                                  \
                                                                                        \
                if (++node->balance)                                                    \
                    *growth = 1;                                                        \
                                                                                        \
            } else {                                                                    \
                node->right =__avl##type##Insert(compare, node->right, newNode, growth);\
                                                                                        \
                if (*growth) {                                                          \
                    node = __avl##type##BalanceRight(node, growth);                     \
                }                                                                       \
            }                                                                           \
                                                                                        \
        } else {                                                                        \
                                                                                        \
            if (!node->left) {                                                          \
                node->left = newNode;                                                   \
                                                                                        \
                if (--node->balance)                                                    \
                    *growth = 1;                                                        \
                                                                                        \
            } else {                                                                    \
                node->left = __avl##type##Insert(compare, node->left, newNode, growth); \
                                                                                        \
                if (*growth) {                                                          \
                    node = __avl##type##BalanceLeft(node, growth);                      \
                }                                                                       \
            }                                                                           \
        }                                                                               \
                                                                                        \
        return node;                                                                    \
    }                                                                                   \
                                                                                        \
    int avl##type##Insert(type##AVL avl, type item) {                                   \
        type##AVLNode newNode;                                                          \
        int growth;                                                                     \
                                                                                        \
        growth = 0;                                                                     \
        newNode = (type##AVLNode)malloc(sizeof(struct type##AVLNode_s));                \
                                                                                        \
        if (!newNode)                                                                   \
            return -1;                                                                  \
                                                                                        \
        newNode->content = item;                                                        \
        newNode->balance = 0;                                                           \
        newNode->left = newNode->right = NULL;                                          \
                                                                                        \
        if (!avl->root)                                                                 \
            avl->root = newNode;                                                        \
        else                                                                            \
            avl->root = __avl##type##Insert(avl->cmp, avl->root, newNode, &growth);     \
                                                                                        \
        return 0;                                                                       \
    }                                                                                   \
                                                                                        \
    type##AVLNode avl##type##GetRoot(type##AVL avl) {                                   \
        return avl->root;                                                               \
    }                                                                                   \
                                                                                        \
    type##AVLNode avl##type##GetLeftChild(type##AVLNode node) {                         \
        return node->left;                                                              \
    }                                                                                   \
                                                                                        \
    type##AVLNode avl##type##GetRightChild(type##AVLNode node) {                        \
        return node->right;                                                             \
    }                                                                                   \
                                                                                        \
    type avl##type##GetNodeContent(type##AVLNode node) {                                \
        return node->content;                                                           \
    }                                                                                   \

#define avlGetRoot(type, tree) avl##type##GetRoot(tree)
#define avlGetLeftChild(type, node) avl##type##GetLeftChild(node)
#define avlGetRightChild(type, node) avl##type##GetRightChild(node)
#define avlGetNodeContent(type, node) avl##type##GetNodeContent(node)
#define avlInsert(type, tree, item) avl##type##Insert(tree, item)
#define avlNew(type, func) avl##type##New(func)
#define avlDestroy(type, tree) avl##type##Destroy(tree)

#endif