/* From Fundamentals of Algorithmics, Brassard & Bratley, 1996 */

#include <stdio.h>
#include "heap.h"

/* This procedure sifts node i down so as to re-establish the heap property in
 * T[1..n]. We suppose that T would be a heap if T[i] were sufficiently large.
 * We also suppose that 1 <= i <= n.
 */
void sift_down(int T[], int n, int i) {

    int j = 0;
    int k = 0;
    int t = 0;
    
    k = i;
    do {
        j = k;
        // Find the larger child of node j.
        if ((2 * j <= n) && (T[2 * j]     > T[k])) { 
            k = 2 * j; 
        }
        if ((2 * j < n)  && (T[2 * j + 1] > T[k])) {
            k = 2 * j + 1;
        }
        // Exchange T[j] and T[k]
        t    = T[j];
        T[j] = T[k];
        T[k] = t;
    } while (j != k); // If j = k, then the node has arrived at its final position.
}

/* This procedure percolates node i so as to re-establish the heap property in
 * T[1..n]. We suppose that T would be a heap if T[i] were sufficiently small. 
 * We also suppose that 1 <= i <= n.
 */
void percolate(int T[], int n, int i) {

    int j = 0;
    int k = 0;
    int t = 0;

    k = i;
    do {
        j = k;
        if ((j > 1) && (T[j / 2] < T[k])) {
            k = j / 2;
        }
        // Exchange T[j] and T[k]
        t    = T[j];
        T[j] = T[k];
        T[k] = t;
    } while (j != k);
}

/* T[1..n] is a heap.
 * The value of T[i] is set to v and the heap property is re-established.
 * We supposed that 1 <= i <= n.
 */
void alter_heap(int T[], int n, int i, int v) {
    
    int x = 0;
    
    x = T[i];
    T[i] = v;
    if (v < x) {
        sift_down(T, n, i);
    }
    else {
        percolate(T, n, i);
    }
}

/* The heap is an ideal data structure for finding the largest element of a set,
 * removing, adding a new nodes, or modifying the node.
 * These are exactly the operations we need to implement dynamic priority lists
 * efficiently: the value of the node gives the priority of the corresponding
 * event, the event with the highest priority is always found at the root of the
 * heap, and the priority of an event can be changed dynamically at any time.
 * This is particularly useful in computer simulations and in the design of
 * schedulers for an operating system.
 */

/* Returns the largest element of the heap T[1..n] */
int find_max(int T[], int n) {
    return T[1];
}

/* Removes the largest element of the heap T[1..n] and restores the heap 
 * property in T[1..n-1].
 */
void delete_max(int T[], int n) {
    T[1] = T[n];
    sift_down(T, n - 1, 1);
}

/* Adds an element whose value is v to the heap T[1..n] and restores the heap
 * property in T[1..n+1].
 */
void insert_node(int T[], int n, int v) {
    T[n + 1] = v;
    percolate(T, n + 1, n + 1);
}

void slow_make_heap() {
}

void make_heap() {
}

void heapsort() {
}

/* EOF: heap.c */
