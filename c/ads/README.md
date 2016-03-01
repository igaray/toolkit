# Algorithms and Data Structures

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

## TODO:

## Element

### Types:
```
element_t: element type
```

### Operations:

```
new
destroy
set
get
```

## List

### Types:

```
node_t:     linked list node type
position_t: linked list position type
list_t:     linked list typed
```

### Operations:

```
void create_list(list_t*);
void delete_list(list_t*);

position_t last(list_t);
position_t first(list_t);
position_t next(list_t, position_t);
position_t prev(list_t, position_t);
position_t find(list_t, element_t);
element_t getp(list_t, position_t);
element_t geti(list_t, int);
int is_empty(list_t);
int length(list_t);
void print(list_t, char *string, int *size);

void insertp(list_t, element_t, position_t);
void inserti(list_t, element_t, int);
void insert_first(list_t, element_t);
void insert_last(list_t, element_t);
void removep(list_t, position_t);
void removei(list_t, int);
```

### Implementations:
    
SIM: simply linked
When a list is simply linked, each node in the list contains a pointer to
the following node in the list.
Traversal of the list is efficient only going forward

DBL: doubly linked
When a list is doubly linked, each node in the list contains a pointer to
the following node and another pointer to the previous node, so that
traversal of the list is efficient in both forwards and backwards.

LIN: linear
When a list is linear it has distinguishable starting and ending nodes.

CIR: circular
When a list is circular the last node points to the first node as its
following node, making it a ring.

DP: direct positioning
With direct positiong the element pointed at is in the cell pointed at.

IP: indirect positioning
With indirect positioning the element pointed at is in the cell that the
pointed cell points at.
To solve some limit cases, such as accesing the first element, an empty
node is included at the beginning of the list.

NH: without head
WH: with head
A head is a special structure at the begnning of the list with additional
information such as the length of the list and pointers to the first and
last nodes.

## Stack

### Operations:

```
CrearPila
Vacia
Tope
Apilar
Desapilar
```

### Implementations:

* arreglos
* listas enlazadas

```
From Meyer, OOSD
    ADT: Stacks

    Types:
        Stacks[G]
        
    Functions:
        put:    Stack[G] x G -> Stack[G]
        remove: Stack[G] -> Stack[G]
        item:   Stack[G] -> G
        empty:  Stack[G] -> Boolean
        new:    Stack[G]

    Axioms:
        For any x : G, s : Stack[G]
        1) item(put(s,x)) = x
        2) remove(put(s,x)) = s
        3) empty(new)
        4) not empty(put(s,x))
        
    Preconditions:
        remove(s : Stack[G]) requires not empty(s)
        item(s : Stack[G]) requires not empty(s)
```
    
## Linear Queue

### Operations:

```
CrearCola
Vacia
Frente
InsertarEnCola
RetirarDeCola
```

### Implementaciones:

* arreglos circulares
* linear lista enlazada

## Tree

### Operations:

```
Crear
Padre
HijoExtremoIzquierdo
HijoDerecho
Etiqueta
Raiz
```

### Implementations:

ab (arboles binarios)
abb (arboles binarios de busqueda)
avl (arboles balanceados)
trie
red-black

## Conjunto

Operations:
    CrearConjunta
    Insertar
    Eliminar
    Miembro
    Union
    Interseccion
    Diferencia
    Minimo
    Maximo

## Diccionario

Operations:
    CrearDiccionario
    Insertar
    Eliminar
    Miembro

## Cola con prioridad

Operations:
    CrearColaConPrioridad
    Insertar
    EliminarMinimo

## Mapeo

Operations:
    CrearMapeo
    Asigna
    Calcula

## NodoTRIE

Operations:
    Crear
    Asigna
    Valor
    TomaNuevo

## Grafo

Node Vertex Arc Link Edge

Operations:
    CrearGrafo
    PrimerVertice
    SiguienteVertice
    PrimerAdyacente
    SiguienteAdyacente
    Vertice
    RotuloArco
    RotuloVertice
    InsertarArco
    InsertarVertice
    EliminarArco
    EliminarVertice

* directed/undirected
* sparse/dense

* adj matrix/adj list/incidence matrix
* static/dynamic vertices/edges
* labelled/unlabelled vertices/edges
* weighted/unweighted vertices/edges
* multigraph/graph

Generally, we first have to build a graph by starting with a set of nodes and adding in any edges we need, and then we want to extract information from it, such as "Is this graph connected?", "What is the shortest path in this graph from s to t?", or "How many edges can I remove from this graph before some nodes become unreachable from other nodes?" There are standard algorithms for answering all of these questions; the information these algorithms need is typically (a) given a vertex u, what successors does it have; and sometimes (b) given vertices u and v, does the edge (u,v) exist in the graph? 

AddVertex
AddEdge
IsReachable

Breadth-First Search
Depth-First Search


/* basic directed graph type */

typedef struct graph *Graph;

/* create a new graph with n vertices labeled 0..n-1 and no edges */
Graph graph_create(int n);

/* free all space used by graph */
void graph_destroy(Graph);

/* add an edge to an existing graph */
/* doing this more than once may have unpredictable results */
void graph_add_edge(Graph, int source, int sink);

/* return the number of vertices/edges in the graph */
int graph_vertex_count(Graph);
int graph_edge_count(Graph);

/* return the out-degree of a vertex */
int graph_out_degree(Graph, int source);

/* return 1 if edge (source, sink) exists), 0 otherwise */
int graph_has_edge(Graph, int source, int sink);

/* invoke f on all edges (u,v) with source u */
/* supplying data as final parameter to f */
/* no particular order is guaranteed */
void graph_foreach(Graph g, int source, void (*f)(Graph g, int source, int sink, void *data), void *data);


