// ----------------------------------------------------------------------------

struct Heap{
    data: Vec<i32>,
}

impl Heap {
    fn new(data: Vec<i32>) {
        let mut heap = Heap{data: data};
        max_heapify(&heap);
        return heap
    }

    fn max_heapify(heap: &mut Heap) {

    }

    fn insert() {

    }

    fn delete_max() {}
}
 
// ----------------------------------------------------------------------------

struct UnionFind {

}

// ----------------------------------------------------------------------------

struct Node {
    value: u32,
    label: String,
}

struct Arc {
    u: usize,
    v: usize,
}

enum GraphType {
    Directed,
    Undirected,
}

enum Graph {
    AdjMatrix{ arcs: Vec<Vec<usize>>},
    AdjList{ arcs: Vec<Vec<usize>>},
}

impl Graph {
    fn new() -> Graph {
    }

    fn random(n: usize) -> Graph {

    }
}

enum Color{
    Black,
    Grey,
    White,
}

fn bfs(g: &Graph) {

}

fn dfs(g: &Graph) {

}

fn topological_sort(g: &Graph) {

}

fn strongly_connected_components(g: &Graph) {

}

fn dijsktra(g: &Graph) {

}

fn kruskal(g: &Graph) {

}

fn prim(g: &Graph) {

}

fn ford_fulkerson(g: &Graph) {

}

fn edmond_karp(g: &Graph) {

}

// ----------------------------------------------------------------------------

fn main() {
    let d = vec![];
    let h = MinHeap{data: d};
}