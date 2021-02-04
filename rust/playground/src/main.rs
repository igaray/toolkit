extern crate clap;
extern crate regex;

use clap::{App, Arg};

fn main() {
    let command_arg = Arg::with_name("command")
        .required(true)
        .takes_value(true)
        .short("c")
        .long("command")
        .help("Run the given command");
    let args = App::new("playground")
        .version("0.0.1")
        .author("Iñaki Garay <igarai@gmail.com>")
        .about("A playground in which to play with rust code.")
        .arg(command_arg)
        .get_matches();

    match args.value_of("command") {
        None => {
            print!("No command!");
        }
        Some(command) => match command {
            "hello" => hello_world::run(),
            "struct" => struct_examples::run(),
            "vector" => vector_examples::run(),
            "hash" => hash_examples::run(),
            "string" => string_examples::run(),
            "regex" => regex_examples::run(),
            _ => {
                print!("Unknown command: {:?}", command);
            }
        },
    }
}

mod hello_world {
    pub fn run() {
        println!(
            "u32 [MIN, MAX] = [{},{}]",
            <u32>::min_value(),
            <u32>::max_value()
        );
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

        println!(
            "The program \"{}\" calculates the value {}",
            program, accumulator
        );
    }
}

mod struct_examples {
    #[derive(Default)]
    struct MyStruct {
        u: u64,
        a: [u64; 16],
        v: Vec<u64>,
    }

    /*
    impl Default for MyStruct {
        fn default() -> MyStruct{
            MyStruct{u: 0, a: [0; 16], v: vec![0, 16]}
        }
    }
    */

    pub fn run() {
        let ms: MyStruct = Default::default();
        println!("ms.u: {:?}", &ms.u);
        println!("ms.a: {:?}", &ms.a);
        println!("ms.v: {:?}", &ms.v);
    }
}

mod vector_examples {
    #[derive(Default, Debug)]
    struct Point {
        x: i32,
        y: i32,
    }

    fn f(v: &mut Vec<Point>, i: usize, x: i32, y: i32) {
        v[i].x = x;
        v[i].y = y;
    }

    fn read_slice(_slice: &[usize]) {
        // ...
    }

    pub fn run() {
        {
            let mut v: Vec<i32> = vec![0; 10];
            v[0] = 1;
        }
        {
            let p1: Point = Default::default();
            let p2: Point = Default::default();
            let mut v: Vec<Point> = Vec::new();
            v.push(p1);
            v.push(p2);

            v[0].x = 1;
            v[0].y = 2;
            v[1].x = 3;
            v[1].y = 4;
            println!("v[0]: {:?}", v[0]);
            println!("v[1]: {:?}", v[1]);

            v[0].x = 5;
            v[0].y = 6;
            v[1].x = 7;
            v[1].y = 8;
            println!("v[0]: {:?}", v[0]);
            println!("v[1]: {:?}", v[1]);

            f(&mut v, 0, 10, 11);
            println!("v[0]: {:?}", v[0]);

            // Slicing
            let v = vec![0, 1];
            read_slice(&v);
            let _x: &[usize] = &v;

            let mut v = Vec::<usize>::with_capacity(256);
            v.capacity();
            v.reserve(1024);
            v.reserve_exact(3);
            v.shrink_to_fit();

            let v = vec![1, 2, 3];
            let _slice = v.into_boxed_slice();
        }
    }
}

mod hash_examples {
    use std::collections::BTreeMap;
    pub fn run() {
        let mut m = BTreeMap::new();
        m.insert("k1", String::from("v1"));
        m.insert("k2", String::from("v2"));
        m.insert("k3", String::from("v3"));

        for (i, (k, v)) in m.iter_mut().enumerate() {
            v.push_str("-2");
            println!("{:?} : {:?} => {:?}", i, k, v);
        }
        // println!("{:?}", m);
    }
}

mod string_examples {
    pub fn run() {
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
        
        let nn: u8 = "1".parse().unwrap();
        let p = format!("{:0>2}", "1");
        println!("padded: {:?}", p);
    }
}

mod regex_examples {
    use regex::Regex;
    pub fn run() {
      // let s = "{A}. {D} - {A}, {A} {X} {N} {N} {Q} { } {}";

      let fn1 = "Dave Brubeck - 01. Take five";
      let mp1 = "{X} - {N}. {X}";
      let rp1 = "{1} {2} {3}";

      let fn2 = "Bahía Blanca, 21 October 2019";
      let mp2 = "{X}, {D}";
      let rp2 = "{1} {2}";

      let fn3 = "Foo 123 B_a_r";
      let mp3 = "{A} {N} {X}";
      let rp3 = "{3} {2} {1}";

      let fn4 = "Bahía Blanca, 21 October 2019";
      let mp4 = "{X}, {D}";
      let rp4 = "{2} {1}";

      let fn5 = "Bahía Blanca, 21 October 2019, FooBarBaz";
      let mp5 = "{X}, {D}, {X}";
      let rp5 = "{2} {1} {3}";

      let filename =                         fn5;
      let match_pattern =                    mp5;
      let mut replace_pattern = String::from(rp5);

      let date_regex = r"(\d|2\d|30|31) ([Jj]anuary|[Ff]ebruary|[Mm]arch|[Aa]pril|[Mm]ay|[Jj]une|[Jj]uly|[Aa]ugust|[Ss]eptember|[Oo]ctober|[Nn]ovember|[Dd]ecember) (\d{1,4})";
      let florb_regex = r"\{[aA]\}|\{[dD]\}|\{[nN]\}|\{[xX]\}";
      let florb_regex = Regex::new(florb_regex).unwrap();

      let florbs: Vec<&str> =
        florb_regex
        .captures_iter(&match_pattern)
        .map(
          |c: regex::Captures | {
            c.get(0).unwrap().as_str()
            })
        .collect();
      println!("florbs in match pattern: {:?}", florbs);
      println!();
      println!("match pattern before replacement: {:?}", match_pattern);
      let match_pattern = String::from(match_pattern);
      let match_pattern = match_pattern.replace(".", r"\.");
      let match_pattern = match_pattern.replace("[", r"\[");
      let match_pattern = match_pattern.replace("]", r"\]");
      let match_pattern = match_pattern.replace("(", r"\(");
      let match_pattern = match_pattern.replace(")", r"\)");
      let match_pattern = match_pattern.replace("?", r"\?");
      let match_pattern = match_pattern.replace("{A}", r"([[:alpha:]]*)"); // Alphabetic
      let match_pattern = match_pattern.replace("{N}", r"([[:digit:]]*)"); // Digits
      let match_pattern = match_pattern.replace("{X}", r"(.*)"); // Anything
      let match_pattern = match_pattern.replace("{D}", date_regex); // Date
      println!("match pattern after  replacement: {:?}", match_pattern);

      println!("replace pattern before replacement: {:?}", replace_pattern);
      let match_regex = Regex::new(&match_pattern).unwrap();
      let captures = match_regex.captures(&filename).unwrap();
      println!();
      println!("Captures: {:?}", captures);

      let nflorbs = florbs.len();
      let ncaps = captures.len() - 1;
      if nflorbs == ncaps {
        println!("Same amount of florbs ({}) and captures ({})!", nflorbs, ncaps);
      }
      else {
        println!("Different amount of florbs ({}) and captures ({})!", nflorbs, ncaps);
      }

      let mut ci = 1;
      let matches = captures.iter().skip(1);
      for (fi, f) in florbs.iter().enumerate() {

        println!("-----");
        let mark = format!("{{{}}}", fi + 1);
        match *f {
          "{A}" | "{N}" | "{X}" => {
            println!("  It's a normal florb!");
            println!("  match text: {:?}", captures.get(ci).unwrap().as_str());
            println!("  corresponding florb: {:?}", f);
            println!("  corresponding mark: {:?}", mark);
            let content = captures.get(ci).unwrap().as_str();
            replace_pattern = replace_pattern.replace(&mark, &content);
            println!("  replace pattern now: {:?}", replace_pattern);
            ci += 1;
            },
          "{D}" => {
            println!("  Ruh-roh, it's a date florb!");
            let day_text =  captures.get(ci).unwrap().as_str();
            let month_text = captures.get(ci + 1).unwrap().as_str();
            let year_text = captures.get(ci + 2).unwrap().as_str();
            let mut content = String::new();
            content.push_str(year_text);
            content.push_str("-");
            content.push_str(month_text);
            content.push_str("-");
            content.push_str(day_text);
            replace_pattern = replace_pattern.replace(&mark, &content);
            ci += 3;
          },
          _ => {
            println!("  Unrecognized florb!");
            },
        }
        /*
        println!("  florb: {:?}", f);
        */
      }
      println!("replace pattern after  replacement: {:?}", replace_pattern);

      /*
      let captures = regex.captures(&filename).unwrap();
      let matches: Vec<Option<regex::Match>> = capture.iter().skip(1).collect();
      for (i, _m) in matches.iter().enumerate() {
        let mark = format!("{{{}}}", i + 1);
        let content = matches.get(i).unwrap().unwrap().as_str();
        replace_pattern = replace_pattern.replace(&mark, &content);
      }
      */
    }
}

