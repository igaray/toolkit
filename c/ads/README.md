All implementations using dynamic arrays use macros for genericity.
All implementations using linked structures use void pointers for the data components.

[X] linear/dynamic_array            A dynamic array implemented with macros for genericity.
[ ] linear/simply_linked_list       A simply linked list.
[ ] linear/doubly_linked_list       A doubly linked list.
[X] linear/stack_array              A stack implemented with a dynamic array.
[ ] linear/stack_list               A stack implemeneted with a simply linked list.
[ ] linear/queue_array              A FIFO queue implemented with a dynamic array.
[ ] linear/queue_list               A FIFO queue implemented with a simply linked list.
[ ] linear/priority_heap            A priority heap implemented with dynamic arrays.
[ ] 
[ ] hash/string_hashmap             
[ ] hash/int_hashmap                
[ ] hash/gen_hashmap                A generic hash map using macros. User must provide own hashing function.
[ ] 
[ ] sets/disjoint_sets_array        A disjoint set implemented with dynamic arrays.
[ ] sets/disjoint_sets_list         A disjoint set implemented with simply linked lists.
[ ] sets/set                        A set implemented with a hashmap
[ ] sets/frozen_set                 A set implementation that does not allow adding and removing elements after initialization.
[ ] 
[ ] binomial_heap                   A binomial heap implemented with linked lists.
[ ] 
[ ] trees/binary_tree               A binary tree.
[ ] trees/avl_tree                  An AVL tree.
[ ] trees/rb-tree                   A red-black tree.
[ ] 
[ ] graph/static_graph              An adjacency matrix implementation for graphs that do not add or remove nodes often.
[ ] graph/dynamic_graph             An adjacency list implementation for graphs that add and remove nodes often.

TODO:
    fix disjoint_sets
    fix linear heap
    implement linked_list_light
    implement linked_list_heavy
    implement stack_list
    implement queue_list
