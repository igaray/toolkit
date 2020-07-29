/*
fn find() {}
fn make_set() {}
fn union() {}
*/

/*
struct DisjointSets {
    id: Vec<usize>,
    }
*/

fn quick_find(input: &[(usize, usize); 12]) {
    let n = 100;
    let mut id = [0; 100];

    // The array id indicates to which set a node belongs to.
    // We initialize the array so as to indicate that every node is in its own set.
    let mut i = 0;
    while i < n {
        id[i] = i;
        i += 1;
    }
    for (p, q) in input.iter() {
        let p = *p;
        let q = *q;
        if id[p] == id[q] {
            continue;
        }
        let t = id[p];
        let mut i = 0;
        while i < n {
            if id[i] == t {
                id[i] = id[q];
            }
            i += 1;
        }
        println!("{} {}", p, q);
    }
}

fn quick_union(input: &[(usize, usize); 12]) {
    let n = 100;
    let mut id = [0; 100];

    // The array id indicates to which set a node belongs to.
    // We initialize the array so as to indicate that every node is in its own set.
    let mut i = 0;
    while i < n {
        id[i] = i;
        i += 1;
    }
    for (p, q) in input.iter() {
        let p = *p;
        let q = *q;
        let mut i = p;
        while i != id[i] {
            i = id[i];
        }
        let mut j = q;
        while j != id[j] {
            j = id[j];
        }
        if i == j {
            continue;
        }
        id[i] = j;
        println!("{} {}", p, q);
    }
}

/*
fn weighted_quick_union(input: &[(usize, usize); 12]) {
}
*/

fn insertion_sort(a: &mut [usize; 10]) {
    let n = 10;
    let mut i = 1;
    while i < n {
        let mut j = i;
        while j > 0 && a[j-1] > a[j] {
            let t = a[j-1];
            a[j-1] = a[j];
            a[j] = t;
            j -= 1;
        }
        i += 1;
    }
}

fn main() {
    // let a: [u8; 32] = [42; 32];

    println!("Insertion Sort");
    let mut a = [2,6,4,7,5,8,3,9,1,0];
    println!("{:?}", a);
    insertion_sort(&mut a);
    println!("{:?}", a);

    let input = [(3,4), (4,9), (8,0), (2,3), (5,6), (2,9), (5,9), (7,3), (4,8), (5,6), (0,2), (6,1)];
    println!("Disjoint Sets: Quick Find");
    quick_find(&input);
    println!("Disjoint Sets: Quick Union");
    quick_union(&input);
    // println!("Disjoint Sets: Weighted Quick Union");
    // weighted_quick_union(&input);
}

