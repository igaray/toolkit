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
#[derive(Clone, Debug)]
pub struct MaxHeap {
    size: usize, // how many elements in the heap are stored within the array
    values: Vec<usize>,
}

impl From<MaxHeap> for Vec<usize> {
    fn from(h: MaxHeap) -> Self {
        h.values
    }
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
    pub fn with_capacity(capacity: usize) -> MaxHeap {
        MaxHeap {
            size: 0,
            values: Vec::with_capacity(capacity),
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
            h.max_heapify(i);
        }
        return h;
    }

    /// Sorts a vector in-place by building a max heap.
    pub fn sort(v: &mut Vec<usize>) -> Vec<usize> {
        let mut h = MaxHeap::from_vec(v.to_owned());
        let n = h.values.len();
        for i in (1..n).rev() {
            h.values.swap(0, i);
            h.size -= 1;
            h.max_heapify(0);
        }
        return h.values.clone();
    }

    /// Pushes a new value onto the heap.
    pub fn push(self: &mut MaxHeap, _k: usize) {
        unimplemented!();
    }

    /// Pops the heap's maximum value.
    pub fn pop(self: &mut MaxHeap) -> Option<&usize> {
        unimplemented!();
    }

    fn max_heapify(self: &mut MaxHeap, i: usize) {
        self.max_heapify_iter(i);
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
        let n = self.size;
        let mut j = i;
        loop {
            let k = j;
            let l = crate::heap::left(k);
            let r = crate::heap::right(k);
            // Se busca el hijo con mayor valor del nodo j
            // Verificar si el hijo izquierdo existe y su valor es mayor que el del nodo i
            if l < n && self.values[j] < self.values[l] {
                j = l;
            }
            // Verificar si el hijo derecho existe y su valor es mayor que el del nodo i
            if r < n && self.values[j] < self.values[r] {
                j = r;
            }
            // println!("\t\tswapping {} and {}", j, k);
            self.values.swap(k, j);
            // println!("\t\tvalues: {:?}", self.values);
            if k == j {
                break;
            }
        }
    }

    #[allow(dead_code)]
    fn max_heapify_rec(self: &mut MaxHeap, i: usize) {
        let n = self.size;
        let l = crate::heap::left(i);
        let r = crate::heap::right(i);
        let mut max;
        if l < n && self.values[l] > self.values[i] {
            max = l
        } else {
            max = i
        }
        if r < n && self.values[r] > self.values[max] {
            max = r
        }
        if i != max {
            self.values.swap(i, max);
            self.max_heapify_rec(max);
        }
    }
}

/// A min-heap.
#[derive(Clone, Debug)]
pub struct MinHeap {
    size: usize, // how many elements in the heap are stored within the array
    values: Vec<usize>,
}

impl From<MinHeap> for Vec<usize> {
    fn from(h: MinHeap) -> Self {
        h.values
    }
}

impl MinHeap {
    /// Creates a new, empty min heap.
    pub fn new() -> MinHeap {
        MinHeap {
            size: 0,
            values: Vec::new(),
        }
    }

    /// Creates a new, empty min heap with the specified capacity.
    pub fn with_capacity(cap: usize) -> MinHeap {
        MinHeap {
            size: 0,
            values: Vec::with_capacity(cap),
        }
    }

    /// Creates a min heap from a vector, which is consumed.
    pub fn from_vec(values: Vec<usize>) -> MinHeap {
        let mut h = MinHeap {
            size: values.len(),
            values,
        };
        let last_inner_node_idx = (h.values.len() / 2) - 1;
        for i in (0..last_inner_node_idx).rev() {
            h.min_heapify(i);
        }
        return h;
    }

    /// Sorts a vector in-place by building a max heap.
    pub fn sort(v: &mut Vec<usize>) -> Vec<usize> {
        let mut h = MinHeap::from_vec(v.to_owned());
        let n = h.values.len();
        for i in (1..n).rev() {
            h.values.swap(0, i);
            h.size -= 1;
            h.min_heapify(0);
        }
        return h.values.clone();
    }

    /// Pushes a new value onto the heap.
    pub fn push(self: &mut MinHeap, _k: usize) {
        unimplemented!();
    }

    /// Pops the heap's maximum value.
    pub fn pop(self: &mut MinHeap) -> Option<&usize> {
        unimplemented!();
    }

    fn min_heapify(self: &mut MinHeap, i: usize) {
        self.min_heapify_iter(i);
    }

    /// `max_heapify` maintains the heap property.
    /// Its inputs are the array A of values in the heap and an index i
    /// into the array.
    /// When called, it assumes that the binary trees rooted at left(i)
    /// and right(i) are max-heaps, but that A[i]
    #[allow(dead_code)]
    fn min_heapify_iter(self: &mut MinHeap, i: usize) {
        // i is the initial node whose heap property we wish to restore.
        // j
        // k
        let n = self.size;
        let mut j = i;
        loop {
            let k = j;
            let l = crate::heap::left(k);
            let r = crate::heap::right(k);
            // Se busca el hijo con mayor valor del nodo j
            // Verificar si el hijo izquierdo existe y su valor es mayor que el del nodo i
            if l < n && self.values[j] < self.values[l] {
                j = l;
            }
            // Verificar si el hijo derecho existe y su valor es mayor que el del nodo i
            if r < n && self.values[j] < self.values[r] {
                j = r;
            }
            // println!("\t\tswapping {} and {}", j, k);
            self.values.swap(k, j);
            // println!("\t\tvalues: {:?}", self.values);
            if k == j {
                break;
            }
        }
    }

    #[allow(dead_code)]
    fn min_heapify_rec(self: &mut MinHeap, i: usize) {
        let n = self.size;
        let l = crate::heap::left(i);
        let r = crate::heap::right(i);
        let mut max;
        if l < n && self.values[l] > self.values[i] {
            max = l
        } else {
            max = i
        }
        if r < n && self.values[r] > self.values[max] {
            max = r
        }
        if i != max {
            self.values.swap(i, max);
            self.min_heapify_rec(max);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn heap_index_test() {
        println!("heap_index_test");
        println!("i\tp(i)\tl(i)\tr(i)");
        for i in 0..=16 {
            println!("{}\t{}\t{}\t{}", i, parent(i), left(i), right(i));
        }
    }

    #[test]
    fn max_heapify_test() {
        let v = vec![18, 14, 16, 20, 21, 26, 25, 2, 29, 4, 17];
        let h = MaxHeap::from_vec(v);
        let v2: Vec<usize> = h.into();
        assert_eq!(v2, &[29, 21, 26, 20, 18, 16, 25, 2, 14, 4, 17]);
    }

    #[test]
    fn heapsort_test() {
        let mut v = vec![18, 14, 16, 20, 21, 26, 25, 2, 29, 4, 17];
        let v2 = MaxHeap::sort(&mut v);
        assert_eq!(v2, &[2, 4, 14, 16, 17, 18, 20, 21, 25, 26, 29]);
    }

    // #[test]
    // fn min_heap_test() {
    //     let v = vec![18, 14, 16, 20, 21, 26, 25, 2, 29, 4, 17];
    //     let h = MinHeap::from_vec(v);
    //     let v2: Vec<usize> = h.into();
    //     assert_eq!(v2, &[]);
    // }
}
