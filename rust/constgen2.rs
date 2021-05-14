const fn concat<const A: usize, const B: usize, const C: usize>(
    a: [i32; A],
    b: [i32; B],
) -> [i32; C] {
    // Assert that `A + B == C`.
    // These overflow if that is not the case, which produces an error at compile-time.
    let _ = C - (A + B); // Assert that `A + B <= C`
    let _ = (A + B) - C; // Assert that `A + B >= C`

    let mut result = [0; C];

    let mut i = 0;
    while i < A {
        result[i] = a[i];
        i += 1;
    }

    while i < A + B {
        result[i] = b[i - A];
        i += 1;
    }

    result
}

macro_rules! concat_array_const {
    ($($arr:expr),*) => {
        concat_array_const!(@concat
            $( [$arr ; $arr.len()] )*
        );
    };

    (@concat [$a:expr; $a_len:expr]) => {
        $a
    };

    (@concat [$a:expr; $a_len:expr] [$b:expr; $b_len:expr] $($tail:tt)*) => {
        concat_array_const!(
            @concat
            [concat::<{ $a_len }, { $b_len }, { $a_len + $b_len }>($a, $b); $a_len + $b_len]
            $($tail)*
        )
    };
}

const A: [i32; 3] = [1, 2, 3];
const B: [i32; 4] = [4, 5, 6, 7];
const C: [i32; 2] = [8, 9];

// Example usage using function:
const AB: [i32; 7] = concat(A, B);

// When concatenating multiple arrays the compiler can't figure out the correct const parameters.
// Using the macro does this for you automatically.
const ABC: [i32; 9] = concat_array_const!(A, B, C);

fn main() {
    // Can also be used in non-const contexts
    let ab = concat(A, B);
    let abc = concat_array_const!(A, B, C);

    println!("A   = {:?}", A);
    println!("B   = {:?}", B);
    println!("C   = {:?}", C);
    println!();
    println!("AB  = {:?}", AB);
    println!("ABC = {:?}", ABC);
    println!();
    println!("ab  = {:?}", ab);
    println!("abc = {:?}", abc);

    assert_eq!(AB, [1, 2, 3, 4, 5, 6, 7]);
    assert_eq!(ABC, [1, 2, 3, 4, 5, 6, 7, 8, 9]);

    assert_eq!(AB, ab);
    assert_eq!(ABC, abc);
}
