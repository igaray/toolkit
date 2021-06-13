//! # Graphs
//! Heap, Union-Find and Graph data structures implemented for an Algorithms &
//! Complexity course.

use std::collections::VecDeque;

// ----------------------------------------------------------------------------

#[allow(dead_code)]
fn parent(i: usize) -> usize {
    if i > 0 {
        (i - 1) / 2
    } else {
        0
    }
}

fn left(i: usize) -> usize {
    2 * i + 1
}

fn right(i: usize) -> usize {
    2 * i + 2
}

/// A max-heap.
/// 
#[derive(Clone, Debug)]
pub struct MaxHeap {
    size: usize, // how many elements in the heap are stored within the array
    values: Vec<usize>,
}

impl MaxHeap {
    /// Creates a new, empty max heap.
    pub fn new() -> MaxHeap {
        MaxHeap {
            size: 0,
            values: Vec::new(),
        }
    }

    /// Creates a new, empty max heap with the specified capacity.
    pub fn with_capacity(cap: usize) -> MaxHeap {
        MaxHeap {
            size: 0,
            values: Vec::with_capacity(cap),
        }
    }

    /// Creates a max heap from a vector, which is consumed.
    pub fn from_vec(values: Vec<usize>) -> MaxHeap {
        let mut h = MaxHeap {
            size: values.len(),
            values,
        };
        let last_inner_node_idx = (h.values.len() / 2) - 1;
        for i in (0..last_inner_node_idx).rev() {
            h.max_heapify_rec(i);
        }
        return h;
    }

    /// Sorts a vector in-place by building a max heap.
    pub fn sort(v: &mut Vec<usize>) {
        println!("Building the heap");
        let mut h = MaxHeap::from_vec(v.to_owned());
        let n = h.values.len();
        println!("Sorting the array");
        for i in (1..n).rev() {
            h.values.swap(0, i);
            h.size -= 1;
            h.max_heapify_rec(0);
        }
    }

    /// Pushes a new value onto the heap.
    pub fn push(self: &mut MaxHeap, _k: usize) {
        unimplemented!();
    }

    /// Pops the heap's maximum value.
    pub fn pop(self: &mut MaxHeap) -> Option<&usize> {
        unimplemented!();
    }

    /// `max_heapify` maintains the heap property.
    /// Its inputs are the array A of values in the heap and an index i
    /// into the array.
    /// When called, it assumes that the binary trees rooted at left(i)
    /// and right(i) are max-heaps, but that A[i]
    #[allow(dead_code)]
    fn max_heapify_iter(self: &mut MaxHeap, i: usize) {
        // i is the initial node whose heap property we wish to restore.
        // j
        // k
        println!("max_heapify({})", i);
        let n = self.size;
        let mut k = i;
        println!("parent({}) = {}", i, crate::parent(i));
        loop {
            let j = k;
            let l = crate::left(j);
            let r = crate::right(j);
            println!("\t\ti: {}\tj: {}\tr: {}\tl: {}\t", i, j, r, l);
            // Se busca el hijo con mayor valor del nodo j
            // Verificar si el hijo izquierdo existe y su valor es mayor que el del nodo i
            if l < n && self.values[k] < self.values[l] {
                k = l;
            }
            // Verificar si el hijo derecho existe y su valor es mayor que el del nodo i
            if r < n && self.values[k] < self.values[r] {
                k = r;
            }
            // println!("\t\tswapping {} and {}", j, k);
            self.values.swap(j, k);
            // println!("\t\tvalues: {:?}", self.values);
            if j == k {
                break;
            }
        }
    }

    #[allow(dead_code)]
    fn max_heapify_rec(self: &mut MaxHeap, i: usize) {
        println!("max_heapify({})", i);
        let n = self.size;
        let l = crate::left(i);
        let r = crate::right(i);
        let max = if l < n && self.values[l] > self.values[i] {
            l
        } else if r < n && self.values[r] > self.values[i] {
            r
        } else {
            i
        };
        println!("\tn: {}, i: {}, l: {}, r: {}, max: {}", n, i, l, r, max);
        // println!(
        //     "of the three values: A[{}]={}, A[{}]={}, A[{}]={}, the index of the max is {}",
        //     i, self.values[i], l, self.values[l], r, self.values[r], max
        // );
        println!("\tamong these values:");
        println!("\t\tA[{}]={}", i, self.values[i]);
        if l < n {println!("\t\tA[{}]={}", l, self.values[l])};
        if r < n {println!("\t\tA[{}]={}", r, self.values[r])};
        println!("\t\tthe index of the max is {}", max);
        if i != max {
            println!(
                "\tswapping A[{}]={} and A[{}]={}",
                i, self.values[i], max, self.values[max]
            );
            self.values.swap(i, max);
            println!("\tA: {:?}", self.values);
            self.max_heapify_rec(max);
        }
    }
}

// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
pub struct UnionFind {}

// ----------------------------------------------------------------------------

#[derive(Clone, Debug)]
struct Node {
    value: u32,
    label: String,
}

#[derive(Clone, Copy, Debug)]
struct Edge {
    u: usize,
    v: usize,
}

#[derive(Clone, Copy, Debug)]
pub enum GraphKind {
    Directed,
    Undirected,
}

#[derive(Clone, Copy, Debug)]
pub enum GraphRepresentationKind {
    AdjMatrix,
    AdjList,
}

#[derive(Clone, Debug)]
enum GraphRepresentation {
    AdjMatrix { edges: Vec<Vec<usize>> },
    AdjList { edges: Vec<Vec<usize>> },
}

#[derive(Clone, Debug)]
pub struct Graph {
    size: usize,
    kind: GraphKind,
    repr: GraphRepresentation,
}

impl Graph {
    pub fn new(n: usize, k: GraphKind, r: GraphRepresentationKind) -> Graph {
        return match r {
            GraphRepresentationKind::AdjMatrix => {
                let edges = vec![vec![0; n]; n];
                Graph {
                    size: n,
                    kind: k,
                    repr: GraphRepresentation::AdjMatrix { edges },
                }
            }
            GraphRepresentationKind::AdjList => {
                let edges = vec![Vec::new(); n];
                Graph {
                    size: n,
                    kind: k,
                    repr: GraphRepresentation::AdjList { edges },
                }
            }
        };
    }

    pub fn with_edges(
        n: usize,
        k: GraphKind,
        r: GraphRepresentationKind,
        a: Vec<(usize, usize)>,
    ) -> Graph {
        let mut g = Graph::new(n, k, r);
        for (u, v) in a {
            g.insert_edge(u, v);
        }
        return g;
    }

    pub fn random(n: usize, k: GraphKind, r: GraphRepresentationKind) -> Graph {
        let g = Graph::new(n, k, r);
        return g;
    }

    pub fn insert_edge(self: &mut Graph, u: usize, v: usize) {
        match (self.kind, &mut self.repr) {
            (
                GraphKind::Directed,
                &mut GraphRepresentation::AdjMatrix {
                    edges: ref mut arcs,
                },
            ) => {
                arcs[u][v] = 1;
            }
            (GraphKind::Directed, &mut GraphRepresentation::AdjList { .. }) => {
                // TODO
            }
            (
                GraphKind::Undirected,
                &mut GraphRepresentation::AdjMatrix {
                    edges: ref mut arcs,
                },
            ) => {
                arcs[u][v] = 1;
                arcs[v][v] = 1;
            }
            (GraphKind::Undirected, &mut GraphRepresentation::AdjList { .. }) => {
                // TODO
            }
        }
    }

    #[allow(dead_code)]
    fn neighbours(self: &Graph, u: usize) -> Vec<usize> {
        match (self.kind, &self.repr) {
            (GraphKind::Directed, &GraphRepresentation::AdjMatrix { edges: ref arcs }) => {
                let _ = arcs;
                // let mut neighbours = Vec::new();
                // for (u, v) in arcs[u].iter().enumerate() {
                //     if v == 1 { neighbours.push_back(u) }
                // }
                Vec::new()
            }
            (GraphKind::Directed, &GraphRepresentation::AdjList { edges: ref arcs }) => {
                arcs[u].clone()
            }
            (GraphKind::Undirected, &GraphRepresentation::AdjMatrix { edges: ref arcs }) => {
                let _ = arcs;
                // arcs[u]
                Vec::new()
            }
            (GraphKind::Undirected, &GraphRepresentation::AdjList { edges: ref arcs }) => {
                arcs[u].clone()
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum Color {
    White,
    Grey,
    Black,
}

#[allow(dead_code)]
struct Traversal {
    parent: Vec<Option<usize>>,
    color: Vec<Color>,
}

impl Traversal {
    fn new(g: &Graph) -> Traversal {
        let parent = vec![None; g.size];
        let color = vec![Color::White; g.size];
        return Traversal { parent, color };
    }
}

/*
PROCEDURE bfs(G=N, A)         costo      veces
  FOR cada vertice v en N
    color[v] = blanco         c          n
  ENDFOR
  Q.cola_vacia()              c          1
  FOR cada vertice v en N
    IF color[v] == blanco     c          n
      color[v] = gris         c          <= n
      Q.insertar(v)           c          <= n
      visitar_bfs(G, Q)       Tvbf(n)    <= n
    ENDIF
  ENDFOR

PROCEDURE visitar_bf(G, Q)                   costo veces
  WHILE no Q.vacia()
    u = Q.primero()                          b     <= n
    IF existe (u,w) tq color[w] == blanco    b     <= n
      color[w] = gris
      Q.insertar(w)
    ELSE                                     b     <= a x n
      color[u] = negro
      Q.sacar_de_cola()
    ENDIF                                    b     <= a x n
  ENDWHILE
*/
#[allow(dead_code)]
fn bfs(g: &Graph) -> Traversal {
    let mut t = Traversal::new(g);
    let mut q = VecDeque::new();
    for v in 0..g.size {
        if let Color::White = t.color[v] {
            t.color[v] = Color::Grey;
            q.push_back(v);
            bfs_visit(g, &mut t, &mut q);
        }
    }
    return t;
}

#[allow(dead_code)]
fn bfs_visit(g: &Graph, t: &mut Traversal, q: &mut VecDeque<usize>) {
    fn white_neighbour(g: &Graph, u: usize) -> Option<usize> {
        let _x = g.neighbours(u);
        None
    }

    while !q.is_empty() {
        if let Some(u) = q.pop_front() {
            match white_neighbour(g, u) {
                Some(w) => {
                    t.color[w] = Color::Grey;
                    q.push_back(w);
                }
                None => {
                    t.color[u] = Color::Black;
                    q.pop_front();
                }
            }
        }
    }
}

// fn dfs(g: &Graph, root: usize) -> Traversal {
//     let t = Traversal::new(g);
//     return t
// }

// fn topological_sort(g: &Graph) {}

// fn strongly_connected_components(g: &Graph) {}

// fn dijkstra(g: &Graph) {}

// fn kruskal(g: &Graph) {}

// fn prim(g: &Graph) {}

// fn ford_fulkerson(g: &Graph) {}

// fn edmond_karp(g: &Graph) {}

// ----------------------------------------------------------------------------

#[cfg(test)]
#[allow(unused_imports)]
mod tests {
    use crate::bfs;
    use crate::Graph;
    use crate::GraphKind;
    use crate::GraphRepresentationKind;
    use crate::MaxHeap;

    // #[test]
    // fn scratch_test() {
    // let v = vec!['a', 'b', 'c', 'd', 'e'];
    // for (i, x) in v.iter().enumerate() {
    //     println!("{}: {}", i, x);
    // }
    // let n = 10;
    // for i in (0..((n/2)-1)).rev() {
    //     println!("{}", i);
    // }
    // }

    #[test]
    fn heap_test() {
        // println!("i\tp(i)\tl(i)\tr(i)");
        // for i in 0..=10 {
        //     println!("{}\t{}\t{}\t{}", i, Heap::parent(i), Heap::left(i), Heap::right(i));
        // }
        let mut v = vec![18, 14, 16, 20, 21, 26, 25, 2, 29, 4, 17];
        println!("A: {:?}", v);
        // let h = Heap::with_vec(v);
        // println!("{:?}", h);
        MaxHeap::sort(&mut v);
        assert_eq!(v, &[2, 4, 14, 16, 17, 18, 20, 21, 25, 26, 29]);
    }

    // #[test]
    // fn bfs_test() {
    /*
    0: r
    1: s
    2: t
    3: u
    4: v
    5: w
    6: x
    7: y
    8: z
    */
    /*
    let a = vec![ (0, 1), (0, 4), (1, 5), (2, 3), (2, 5), (2, 6), (3, 7), (5, 6), (7, 8), (1, 0), (4, 0), (5, 1), (3, 2), (5 ,2), (6 ,2), (7, 3), (6, 5), (8, 7)];
    let g = Graph::with_arcs(9, GraphKind::Undirected, GraphRepresentationKind::AdjMatrix, a);
    println!("{:?}", g);
    let t = bfs(&g);
    */
    // }
}
