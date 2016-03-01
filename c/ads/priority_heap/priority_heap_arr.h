#ifndef _HEAP_H_
#define _HEAP_H_

#include <stdio.h>
#include "ds_utils.h"

/* This procedure sifts node i down so as to re-establish the heap property in
 * T[1..n]. We suppose that T would be a heap if T[i] were sufficiently large.
 * We also suppose that 1 <= i <= n.
 */
void sift_down(int T[], int n, int i);

/* This procedure percolates node i so as to re-establish the heap property in
 * T[1..n]. We suppose that T would be a heap if T[i] were sufficiently small. 
 * We also suppose that 1 <= i <= n.
 */
void percolate(int T[], int n, int i);

#endif

/*
 * A heap is a special kind of rooted tree that can be implemented efficiently
 * in an array without any explicit pointers. This interesting structure lends
 * itself to numerous applications, including a remarkable technique called
 * heapsort. It can also be used for the efficient representation of certain
 * dynamic priority lists, such as the event list in a simulation or the list of
 * tasks to be scheduled by an operating system. 
 *
 * A binary tree is essentially complete if each internal node, with the
 * possible exception of one special node, has exactly two children. The special
 * node, if there is one, is situated on level 1; it has a left child but no
 * right child. Moreover, either all the leaves are on level 0, or else they are
 * on levels 0 and 1, and no leaf on level 1 is to the left of an internal node
 * at the sae level. 
 *
 * Intuitively, an essentially complete tree is one where the internal nodes are
 * pushed up the tree as high as possible, the internal nodes on the last level
 * being pushed over to the left; the leaves fill the last level containing
 * internal nodes, if there is still any room, and then spill over onto the left
 * of level 0.
 *
 * If an essentially complete binary tree has height k, then there is one node
 * (the root) on level k, there are two nodes on level k-1, and so on; there are
 * 2^(k-1) nodes on level 1, and at least 1 but not more than 2^k on level 0.
 * If the tree contains n nodes in all, counting both internal nodes and leaves,
 * it follows that 2^k < n < 2^(k+1)
 * Equivalently, the height of a tree containing n nodes is k = floor(lg(n)).
 *
 * This kind of tree can be represented by an array T by putting the nodes of
 * depth k, from left to right, in the positions T[2^k], T[2^k+1], ..., 
 * T[2^(k-1)], with the possible exception of level 0, which may be incomplete.
 *
 *                 T[1]
 *                 |
 *           /-----+-----\
 *          /             \
 *         T[2]          T[3]
 *          |             |
 *      /---+----\      /-+-\
 *     /          \    /     \
 *    T[4]       T[5] T[6]  T[7]
 *     |          |
 *   /-+-\        |
 *  /     \       |
 * T[8]  T[9]   T[10]
 *
 * Using this representation, the parent of the node represented in T[i] is
 * found in T[i/2] (integer division) for i>2 (the root T[1] does not have
 * a parent), and the children of the node represented in T[I] are found in
 * T[2*i] and T[2*i+1], whenever they exist. 
 * The subtree whose root is in T[i] is also easy to identify.
 *
 * A heap is an essentially complete binary tree, each of whose nodes includes
 * an element of information called the value of the node, and which has the
 * property that the value of each internal node is greater than or equal to the
 * values of its children. This is called the heap proprty. 
 * 
 * Since the value of each internal node is greater than or equal to the values
 * of its children, which in turn have value greater than or equal to their
 * children, and so on, the heap property ensures that the value of each
 * internal node is greater than or equal to the values of all the nodes that
 * lie in the subtrees below it.
 * In particular, the value of the root is much greater than or equal to the
 * values of all the other nodes in the heap.
 *
 * [ 10 | 7 | 9 | 4 | 7 | 5 | 2 | 2 | 1 | 6 ]
 *
 *                 10
 *                 |
 *           /-----+-----\
 *          /             \
 *          7             9
 *          |             |
 *      /---+----\      /-+-\
 *     /          \    /     \
 *     4          7   5      2
 *     |          |
 *   /-+-\        |
 *  /     \       |
 *  2     1       6
 *
 * The crucial characteristic of this data structure is that the heap property
 * can be restored efficiently if the value of a node is modified. 
 * If the value of a node increases to the extent that it becomes greater than
 * the value of its parent, it suffices to exchange these two values, and then
 * continue the same process upwards in the tree until the heap property is
 * restored. 
 * We say that the modified value has been percolated up to its new position in
 * the heap.
 *
 * If on the contrary the value of a node it decreased so that it becomes less
 * than the value of at least one of its children, it suffices to exhange the
 * modified value with the larger of the values in the children, and then to
 * continue this process downwards in the tree if necessary until the heap
 * property is restored.
 * We say that the modified value has been sifted down to its new position.
 */

/* EOF: heap.h */
