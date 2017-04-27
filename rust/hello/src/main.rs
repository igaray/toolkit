// This code is editable and runnable!
struct MyStruct {
    u: u64,
    a: [u64; 256],
    v: Vec<u64>
}

impl Default for MyStruct {
    fn default() -> MyStruct{
        MyStruct{u: 0, a: [0; 256], v: vec![0, 1024]}
    }
}

fn main() {
    // A simple integer calculator:
    // `+` or `-` means add or subtract by 1
    // `*` or `/` means multiply or divide by 2

    let program = "+ + * - /";
    let mut accumulator = 0;

    for token in program.chars() {
        match token {
            '+' => accumulator += 1,
            '-' => accumulator -= 1,
            '*' => accumulator *= 2,
            '/' => accumulator /= 2,
            _ => { /* ignore everything else */ }
        }
    }

    println!("The program \"{}\" calculates the value {}",
              program, accumulator);
}
