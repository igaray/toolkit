use std::collections::VecDeque;

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
pub enum GraphProperty {
    Connected,
    Bipartite,
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

#[cfg(test)]
#[allow(unused_imports)]
mod tests {
    use super::*;

    /*
    #[test]
    fn scratch_test() {
    let v = vec!['a', 'b', 'c', 'd', 'e'];
    for (i, x) in v.iter().enumerate() {
        println!("{}: {}", i, x);
    }
    // let n = 10;
    // for i in (0..((n/2)-1)).rev() {
    //     println!("{}", i);
    // }
    }
    */

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
