// Perfectly valid rust!

trait Unimplementable {}

impl<T: Unimplementable> Unimplementable for T {}


// Try uncommenting one of the following lines and see what happens


// It's impossible to implement the trait for anything
/*
impl Unimplementable for u8 {

}
*/


// No type implements Unimplementable
// const foo: &dyn Unimplementable = &0u8;


// The reason why you can't implement Unimplementable for u8
// is the same reason why you can't specialize trait implementations
// If u8 implemented Unimplementable, it would be implemented using the
// first implementation. Therefore the second, specializing implementation
// is disallowed. Try adding #![feature(min_specialization)] at the top
// of this file and uncommenting the second impl. Also try to uncomment the
// "const foo" line.

// The reason why the first impl doesn't get applied for any type
// is that when rustc needs to check if a type implements a trait, it looks
// at the avaliable impls, and cheks if the trait implementation applies.
// It first checks if the first impl applies. The first impl applies only if
// T implements Unimplementable. Rustc then checks if T implements
// Unimplementable and the cycle continues ad infinitum.

// More technical details at
// https://rustc-dev-guide.rust-lang.org/traits/resolution.html