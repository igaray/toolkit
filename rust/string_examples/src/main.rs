fn main() {
    // Creation
    println!("Hello, world!");
    let s = "Hello".to_string();
    println!("{}", s);
    let s = String::from("world");
    println!("{}", s);
    let s: String = "also this".into();
    println!("{}", s);
    let s = String::new();
    println!("{}", s);

    // Concatenation
    let s = "Hello".to_string();
    let message = s + " world!";
    println!("{}", message);

    // UTF8
    let sparkle_heart = vec![240, 159, 146, 150];
    let sparkle_heart = String::from_utf8(sparkle_heart).unwrap();
    assert_eq!("💖", sparkle_heart);
    let bytes = sparkle_heart.into_bytes();
    assert_eq!(bytes, [240, 159, 146, 150]);

    println!("火");

    // How can I convert a String or Vec<T> to a slice (&str and &[T])?
    // Usually, you can pass a reference to a String or Vec<T> wherever a slice
    // is expected.
    // Using Deref coercions, Strings and Vecs will automatically coerce to
    // their respective slices when passed by reference with & or & mut.

    // Methods implemented on &str and &[T] can be accessed directly on String
    // and Vec<T>. For example, some_string.char_at(0) will work even though
    // char_at is a method on &str and some_string is a String.

    // In some cases, such as generic code, it’s necessary to convert manually.
    // Manual conversions can be achieved using the slicing operator, like so:
    // &my_vec[..].

    // How can I convert from &str to String or the other way around?
    // The to_string() method converts from a &str into a String, and Strings
    // are automatically converted into &str when you borrow a reference to them.
    let s = "Jane Doe".to_string();
    println!("{}", s);

    // What are the differences between the two different string types?

    // String is an owned buffer of UTF-8 bytes allocated on the heap.
    // Mutable Strings can be modified, growing their capacity as needed.
    // &str is a fixed-capacity “view” into a String allocated elsewhere,
    // commonly on the heap, in the case of slices dereferenced from Strings,
    // or in static memory, in the case of string literals.

    // &str is a primitive type implemented by the Rust language, while String
    // is implemented in the standard library.


}
