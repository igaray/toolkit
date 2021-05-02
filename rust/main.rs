enum E {
    id: i32,

    A{},
    B{},
    }
    
fn init() {
    let mut a = [0; 10];
    for i in 0..a.len() {
        a[i] = i;
    }
    println!("a: {:?}", a);
}

fn eratosthenes() {
    let mut a = [0; 100];
    let n = a.len();
    for i in 2..n {
        a[i] = 1;
    }
    for i in 2..n {
        if 0 != a[i] {
            let mut j = i;
            while i*j < n {
                a[i*j] = 0;
                j += 1;
            }
        }
    }
    for i in 2..n {
        if 0 != a[i] {
            print!("{} ", i);
        }
    }
    println!();
}
    
fn main() {
    init();
    eratosthenes();

    let _e = E::A;
}
