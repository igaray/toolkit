#![allow(incomplete_features)]
#![feature(const_generics)]
#![feature(const_evaluatable_checked)]

/// A const expression that evalutes to a boolean.
/// Used in conjunction with `Satisfied`.
pub enum Predicate<const EXPRESSION: bool> {}

/// A trait implemented for `Predicate`s that are satisfied.
pub trait Satisfied {}
impl Satisfied for Predicate<true> {}

mod ranged {

    use super::{Predicate, Satisfied};

    pub trait Ranged {
        const MIN: usize;
        const MAX: usize;
    }

    // This is an Ada-like range type, based in a newtype for u16 that will be
    // checked over a constant range from A to B.
    // There are just two ways of instantiating RangedUsize:
    //    - Dynamically: RangedUsize::try_from(u16), where the u16 value is checked
    //                   in runtime, and stored in a Result object.
    //    - Statically: RangedUsize::new::<N>(), where N is an static constant that
    //                  will be checked in compile time
    // Note: I was unable to apply Predicate on this position, then I applied it
    //       on impl block
    //
    // We should implement std::ops traits to improve usability
    #[derive(Clone, Copy)]
    pub struct RangedUsize<const A: usize, const B: usize>(usize);

    impl<const A: usize, const B: usize> Ranged for RangedUsize<A, B> {
        const MIN: usize = A;
        const MAX: usize = B;
    }

    impl<const A: usize, const B: usize> RangedUsize<A, B> {

        pub const fn new<const N: usize>() -> Self
        where
            // We are checking just upper bound because type position constant
            // expressions don't support && operator
            Predicate<{ N <= B }>: Satisfied,
        {
            Self(N)
        }

        pub const fn coerce<const C: usize, const D: usize>(self) -> RangedUsize<C, D>
        where
            Predicate<{ B <= D }>: Satisfied,
        {
            RangedUsize(self.0)
        }

        pub const fn unpadded(self) -> usize {
            self.0 - A
        }
    }

    impl<const A: usize, const B: usize> std::fmt::Display for RangedUsize<A, B> {
        fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            let tmp = self.0.to_string();

            formatter.pad_integral(true, "", &tmp)
        }
    }

    impl<const A: usize, const B: usize> std::convert::TryFrom<usize> for RangedUsize<A, B> {
        type Error = &'static str;

        fn try_from(value: usize) -> Result<Self, Self::Error> {
            if value < A {
                Err("Only values >= A !")
            } else if value > B {
                Err("Only values <= B !")
            } else {
                Ok(Self(value))
            }
        }
    }

    impl<const A: usize, const B: usize> std::convert::From<RangedUsize<A, B>> for usize {
        fn from(value: RangedUsize<A, B>) -> Self {
            value.0
        }
    }

    pub struct RangedArray<T, const A: usize, const B: usize>([T; 1 + B - A])
    where
        [(); 1 + B - A]: Sized;

    impl<T, const A: usize, const B: usize> RangedArray<T, A, B>
    where
        [(); 1 + B - A]: Sized,
    {
        pub fn from_array(array: [T; 1 + B - A]) -> Self {
            Self(array)
        }
    }

    impl<T, const A: usize, const B: usize> std::ops::Index<RangedUsize<A, B>> for RangedArray<T, A, B>
    where
        [(); 1 + B - A]: Sized,
    {
        type Output = T;

        fn index(&self, index: RangedUsize<A, B>) -> &Self::Output {
            &self.0[index.unpadded()]
        }
    }
}

fn main() {
    use ranged::*;
    use std::convert::TryFrom;

    type DayOfYear = RangedUsize<1, 365>;
    type DayOfWeek = RangedUsize<1, 7>;

    // This compiles because 360 < upper bound
    let doy = DayOfYear::new::<360>();

    // Next won't compile because 400 is out of bounds
    // Try uncomment
    // let doy = DayOfYear::new::<400>();

    // This compiles since it is runtime checked
    let _checked_doy = DayOfYear::try_from(400);

    // Safe convertion to u16
    let _n: usize = doy.into();

    let dow = DayOfWeek::new::<5>();

    // Safe  convertion, upper bound of DayOfYear (365) > upper bound of
    // DayOfWeek (7)
    let doy: DayOfYear = dow.coerce();

    // Next won't compile because upper bound of DayOfWeek (7) < upper bound of
    // DayOfYear(365)
    // Try uncomment
    // let _dow: DayOfWeek = doy.coerce();

    println!("day of year is {}", doy);

    type OuterPlanet = RangedUsize<5, 8>;

    // this ranged array is indexed over OuterPlanet range
    let outer_planets = RangedArray::from_array(["Jupiter", "Saturn", "Uranus", "Neptune"]);

    let saturn = OuterPlanet::new::<6>();
    println!("planet at position {} is {}", saturn, outer_planets[saturn]);
}
