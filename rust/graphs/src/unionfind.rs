#[derive(Clone, Debug)]
pub struct UnionFind {
    id: Vec<usize>,
    sz: Vec<usize>,
}

impl UnionFind {
    // The array id indicates to which set a node belongs to.
    // We initialize the array so as to indicate that every node is in its own set.
    pub fn new(size: usize) -> UnionFind {
        let mut id = vec![0; size];
        let sz = vec![1; size];
        for i in 0..id.len() {
            id[i] = i
        }
        UnionFind { id, sz }
    }

    pub fn from_pairs(size: usize, pairs: Vec<(usize, usize)>) -> UnionFind {
        let mut id = vec![0; size];
        let sz = vec![1; size];
        for i in 0..id.len() {
            id[i] = i
        }
        let mut uf = UnionFind { id, sz };
        for (p, q) in pairs.iter() {
            uf.union(*p, *q);
        }
        return uf;
    }

    // Number of connected components. Maximal set of objects that are mutually connected.
    pub fn count(self: &Self) -> usize {
        unimplemented!()
    }

    // We assume "is connected to" is an equivalence relation:
    // - Reflexive: p is connected to p.
    // - Symmetric: if p is connected to q, then q is connected to p.
    // - Transitive: if p is connected to q and q is connected to r, then p is connected to r.
    pub fn connected(self: &Self, p: usize, q: usize) -> bool {
        self.weighted_quick_union_connected(p, q)
    }

    // Union command. Replace components containing two objects with their union.
    pub fn union(self: &mut Self, p: usize, q: usize) {
        self.weighted_quick_union(p, q)
    }

    // Find query. Check if two objects are in the same component
    pub fn find(self: &Self, p: usize) -> usize {
        self.weighted_quick_union_find(p)
    }

    // ------------------------------------------------------------------------
    // Quick-Find
    #[allow(dead_code)]
    fn quick_find_connected(self: &Self, p: usize, q: usize) -> bool {
        if self.id[p] == self.id[q] {
            println!("{} and {} are connected", p, q)
        } else {
            println!("{} and {} are not connected", p, q)
        }
        self.id[p] == self.id[q]
    }

    #[allow(dead_code)]
    fn quick_find(self: &Self, p: usize) -> usize {
        self.id[p]
    }

    #[allow(dead_code)]
    fn quick_find_union(self: &mut Self, p: usize, q: usize) {
        if !self.connected(p, q) {
            println!("connecting {} and {}", p, q);
            println!("  {:?}", self.id);
            let qid = self.id[q];
            let pid = self.id[p];
            println!("  pid = id[p = {}] = {} , qid = id[q = {}] = {}", p, pid, q, qid);
            for i in 0..self.id.len() {
                if pid == self.id[i] {
                    println!("  setting id[{}] = {}", i, qid);
                    self.id[i] = qid;
                }
            }    
        } 
    }

    // ------------------------------------------------------------------------
    // Quick-Union
    #[allow(dead_code)]
    fn root(self: &Self, i: usize) -> usize {
        let mut r = i;
        while r != self.id[r] {
            r = self.id[r];
        }
        return r;
    }

    #[allow(dead_code)]
    fn quick_union_connected(self: &Self, p: usize, q: usize) -> bool {
        let rp = self.root(p);
        let rq = self.root(q);
        let connected = rp == rq;
        if connected {
            println!("{} and {} are connected", p, q)
        } else {
            println!("{} and {} are not connected", p, q)
        }
        connected
    }

    #[allow(dead_code)]
    fn quick_union_find(self: &Self, p: usize) -> usize {
        self.root(p)
    }

    #[allow(dead_code)]
    fn quick_union(self: &mut Self, p: usize, q: usize) {
        let rp = self.root(p);
        let rq = self.root(q);
        println!("connecting {} and {}", p, q);
        println!("  root[p = {}] = {} , root[q = {}] = {}", p, rp, q, rq);
        println!("  setting id[{}] = {}", rp, rq);
        self.id[rp] = rq;
    }

    // ------------------------------------------------------------------------
    // Weighted Quick-Union
    // root() and quick_union_find() are identical
    // quick_union() is modified to link root of smaller tree to root of larger tree and update the sz vector.
    #[allow(dead_code)]
    fn weighted_quick_union_connected(self: &Self, p: usize, q: usize) -> bool {
        self.quick_union_connected(p, q)
    }
        
    #[allow(dead_code)]
    fn weighted_quick_union_find(self: &Self, p: usize) -> usize {
        self.root(p)
    }

    #[allow(dead_code)]
    fn weighted_quick_union(self: &mut Self, p: usize, q: usize) {
        let i = self.root(p);
        let j = self.root(q);
        if self.sz[i] < self.sz[j] {
            self.id[i] = j;
            self.sz[j] += self.sz[i];
        } else {
            self.id[j] = i;
            self.sz[i] += self.sz[j];
        }
    }

    // ------------------------------------------------------------------------
    // Full Path Compression
    // add second loop to root() to set the id[] of each examined node to the root
    #[allow(dead_code)]
    fn path_compression_root(self: &mut Self, i: usize) -> usize {
        let mut r = i;
        while r != self.id[r] {
            r = self.id[r];
        }
        let mut i = i;
        while i != self.id[i] {
            i = self.id[i];
            self.id[i] = r;
        }
        return r;
    }

    // Simple Path Compression by Half
    // make every other node in path point to its grandparent, thereby halving path length.
    #[allow(dead_code)]
    fn full_path_compression_root(self: &mut Self, i: usize) -> usize {
        let mut r = i;
        while r != self.id[r] {
            self.id[r] = self.id[self.id[r]];
            r = self.id[r];
        }
        return r;
    }

    fn weighted_quick_union_full_path_compression() {}
    fn weighted_quick_union_half_path_compression() {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn union_find_test() {
        let pairs = Vec::from([
            (4, 3),
            (3, 8),
            (6, 5),
            (9, 4),
            (2, 1),
            (8, 9),
            (5, 0),
            (7, 2),
            (6, 1),
            (1, 0),
            (6, 7),
        ]);
        let uf = UnionFind::from_pairs(10, pairs);
        println!("{:?}", uf.id);
        assert!(uf.connected(4, 3));
        assert!(uf.connected(3, 8));
        assert!(uf.connected(6, 5));
        assert!(uf.connected(9, 4));
        assert!(uf.connected(2, 1));
        assert!(uf.connected(8, 9));
        assert!(uf.connected(5, 0));
        assert!(uf.connected(7, 2));
        assert!(uf.connected(6, 1));
        assert!(uf.connected(1, 0));
        assert!(uf.connected(6, 7));

        let uf = UnionFind::new(11);
        println!("{:?}", uf.id);
    }
}
