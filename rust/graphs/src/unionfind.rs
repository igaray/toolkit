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

    pub fn with_capacity(capacity: usize) -> UnionFind {
        UnionFind {
            id: Vec::with_capacity(capacity),
            sz: Vec::with_capacity(capacity),
        }
    }

    pub fn from_pairs(pairs: Vec<(usize, usize)>) -> UnionFind {
        let size = pairs.len();
        let mut id = vec![0; size];
        let sz = vec![1; size];
        for i in 0..id.len() {
            id[i] = i
        }
        let mut uf = UnionFind { id, sz };
        for (p, q) in pairs.iter() {
            uf.union(*p, *q);
        }
        return uf
    }

    pub fn union(self: &mut Self, _p: usize, _q: usize) {
        unimplemented!()
    }

    pub fn connected(self: &Self, p: usize, q: usize) -> bool {
        self.id[p] == self.id[q]
    }

    pub fn find(self: &Self, _p: usize) -> usize {
        unimplemented!()
    }

    pub fn count(self: &Self) -> usize {
        unimplemented!()
    }

    // Quick-Find Eager Approach
    #[allow(dead_code)]
    fn quick_find(self: &Self, p: usize) -> usize {
        self.id[p]
    }

    #[allow(dead_code)]
    fn quick_find_union(self: &mut Self, p: usize, q: usize) {
        let qid = self.id[q];
        let pid = self.id[p];
        for i in 0..self.id.len() {
            if pid == self.id[i] {
                self.id[i] = qid;
            }
        }
    }

    // Quick-Union Approach
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
        self.root(p) == self.root(q)
    }

    #[allow(dead_code)]
    fn quick_union_find(self: &Self, p: usize) -> usize {
        self.root(p)
    }

    #[allow(dead_code)]
    fn quick_union(self: &mut Self, p: usize, q: usize) {
        let i = self.root(p);
        let j = self.root(q);
        self.id[i] = j;
    }

    // Weighted Quick-Union
    // root() and quick_union_find() are identical
    // quick_union() is modified to link root of smaller tree to root of larger tree and update the sz vector.
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn union_find_test() {
        let uf = UnionFind::new(10);
        println!("{:?}", uf);
    }
}
