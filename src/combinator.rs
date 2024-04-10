use crate::display::DisplayDepth;
use crate::error::Error;
use crate::value;
use crate::value::Value;
use std::marker::PhantomData;

/// Generic combinator.
pub trait Combinator: DisplayDepth {
    /// Input type
    type In: Value;
    /// Output type
    type Out: Value;

    /// Execute the combinator on the given input value.
    /// Return an output value or an error.
    fn exec(&self, value: Self::In) -> Result<Self::Out, Error>;
}

/// Atomic unit combinator.
///
/// Takes any input and returns the unit value.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Unit<A: Value> {
    _i: PhantomData<A>,
}

/// Atomic iden combinator (identity).
///
/// Takes any input and returns it back.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Iden<A: Value> {
    _i: PhantomData<A>,
}

/// Take combinator (left projection).
///
/// Contains inner combinator of type `A → C`.
///
/// Takes an input `(a, b)` of type `A × B`,
/// passes the left value `a` of type `A` to the inner combinator,
/// and returns a value `c` of type `C`.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Take<T: Combinator, B: Value> {
    pub(crate) inner: T,
    _i: PhantomData<B>,
}

/// Drop combinator (right projection).
///
/// Contains inner combinator of type `B → C`.
///
/// Takes an input `(a, b)` of type `A × B`,
/// passes the right value `b` of type `B` to the inner combinator,
/// and returns a value `c` of type `C`.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Drop<T: Combinator, A: Value> {
    pub(crate) inner: T,
    _i: PhantomData<A>,
}

/// Injl combinator (left injection).
///
/// Contains inner combinator of type `A → B`.
///
/// Takes an input `a` of type `A`,
/// passes the value `a` to the inner combinator to obtain a value `b` of type `B`,
/// and returns the value `L(b)` of type `B + C`.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Injl<T: Combinator, C: Value> {
    pub(crate) inner: T,
    _i: PhantomData<C>,
}

/// Injr combinator (right injection).
///
/// Contains inner combinator of type `A → C`.
///
/// Takes an input `a` of type `A`,
/// passes the value `a` to the inner combinator to obtain a value `c` of type `C`,
/// and returns the value `R(c)` of type `B + C`.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Injr<T: Combinator, B: Value> {
    pub(crate) inner: T,
    _i: PhantomData<B>,
}

/// Pair combinator (product).
///
/// Contains left combinator of type `A → B`
/// and right combinator of type `A → C`.
///
/// Takes an input `a` of type `A`,
/// passes the value `a` to the left combinator to obtain a value `b` of type `B`,
/// passes the value `a` to the right combinator to obtain a value `c` of type `C`,
/// and returns the value `(b, c)` of type `B × C`.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Pair<S: Combinator, T: Combinator> {
    pub(crate) left: S,
    pub(crate) right: T,
}

/// Comp combinator (composition).
///
/// Contains left combinator of type `A → B`
/// and right combinator of type `B → C`.
///
/// Takes an input `a` of type `A`,
/// passes the value `a` to the left combinator to obtain a value `b` of type `B`,
/// passes the value `b` to the the right combinator to obtain a value `c` of type `C`,
/// and returns `c`.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Comp<S: Combinator, T: Combinator> {
    pub(crate) left: S,
    pub(crate) right: T,
}

/// Case combinator (conditional).
///
/// Contains left combinator of type `A × C → D`
/// and right combinator of type `B × C → D`.
///
/// Takes an input `(ab, c)` of type `(A + B) × C`.
///
/// If `ab` looks like `L(a)` where value `a` is of type `A`,
/// then it passes the value `(a, c)` to the left combinator to obtain a value `d` of type `D`
/// and returns `d`.
///
/// If `ab` looks like `R(b)` where value `b` is of type `B`,
/// then it passes the value `(b, c)` to the right combinator to obtain a value `d` of type `D`
/// and returns `d`.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Case<S: Combinator, T: Combinator> {
    pub(crate) left: S,
    pub(crate) right: T,
}

impl<A> Combinator for Unit<A>
where
    // Any input type
    A: Value,
{
    // Input type is anything
    type In = A;
    // Output type is always unit type
    type Out = value::Unit;

    fn exec(&self, _value: Self::In) -> Result<Self::Out, Error> {
        // Always return unit value
        Ok(value::Unit::Unit)
    }
}

impl<A> Combinator for Iden<A>
where
    // Any input type
    A: Value,
{
    // Input type is anything
    type In = A;
    // Output type equals input type
    type Out = A;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        // Always return input value
        Ok(value)
    }
}

impl<T, B> Combinator for Take<T, B>
where
    // Any inner combinator
    T: Combinator,
    // Any type that will be ignored
    B: Value,
{
    // Input type is product type of:
    // 1) Input type of inner combinator, and
    // 2) Something which will be ignored (`B`)
    type In = value::Product<T::In, B>;
    // Output type is output type of inner combinator
    type Out = T::Out;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        // Get left inner value of product value (ignore the right inner value)
        let (a, _) = value.as_product()?;
        // Execute inner combinator on left inner value
        let c = self.inner.exec(a.clone())?;
        // Return output of inner combinator
        Ok(c)
    }
}

impl<T, A> Combinator for Drop<T, A>
where
    // Any inner combinator
    T: Combinator,
    // Any type that will be ignored
    A: Value,
{
    // Input type is product type of:
    // 1) Something which will be ignored (`A`), and
    // 2) Input type of inner combinator
    type In = value::Product<A, T::In>;
    // Output type is output type of inner combinator
    type Out = T::Out;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        // Get right inner value of product value (ignore the left inner value)
        let (_, b) = value.as_product()?;
        // Execute inner combinator on right inner value
        let c = self.inner.exec(b.clone())?;
        // Return output of inner combinator
        Ok(c)
    }
}

impl<T, C> Combinator for Injl<T, C>
where
    // Any inner combinator
    T: Combinator,
    // Any type that will be added
    C: Value,
{
    // Input type is input of inner combinator
    type In = T::In;
    // Output type is sum type of:
    // 1) Output type of inner combinator, and
    // 2) Something which was added (`C`)
    type Out = value::Sum<T::Out, C>;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        // Execute inner combinator on input value
        let c = self.inner.exec(value)?;
        // Wrap output of inner combinator in left value
        Ok(value::Sum::Left(c))
    }
}

impl<T, B> Combinator for Injr<T, B>
where
    // Any inner combinator
    T: Combinator,
    // Any type that will be added
    B: Value,
{
    // Input type is input type of inner combinator
    type In = T::In;
    // Output type is sum type of:
    // 1) Something which was added (`B`), and
    // 2) Output type of inner combinator
    type Out = value::Sum<B, T::Out>;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        // Execute inner combinator on input value
        let c = self.inner.exec(value)?;
        // Wrap output of inner combinator in right value
        Ok(value::Sum::Right(c))
    }
}

impl<S, T> Combinator for Pair<S, T>
where
    // Any left inner combinator
    S: Combinator,
    // Any right inner combinator
    // whose input type equals the input type of the left inner combinator
    T: Combinator<In = S::In>,
{
    // Input type is input type of left inner combinator
    type In = S::In;
    // Output type is product type of
    // 1) Output type of left inner combinator, and
    // 2) Output type of right inner combinator
    type Out = value::Product<S::Out, T::Out>;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        // Execute left inner combinator on input value
        let b = self.left.exec(value.clone())?;
        // Execute right inner combinator on (same) input value
        let c = self.right.exec(value)?;
        // Return product value of output of both inner combinators
        Ok(value::Product::Product(b, c))
    }
}

impl<S, T> Combinator for Comp<S, T>
where
    // Any left inner combinator
    S: Combinator,
    // Any right inner combinator
    // whose output type equals the input type of the left inner combinator
    T: Combinator<In = S::Out>,
{
    // Input type is input type of left inner combinator
    type In = S::In;
    // Output type is output type of right inner combinator
    type Out = T::Out;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        // Execute left inner combinator on input value
        let b = self.left.exec(value)?;
        // Execute right inner combinator on output of left inner combinator
        let c = self.right.exec(b)?;
        // Return output of right inner combinator
        Ok(c)
    }
}

impl<S, T, AC, BC> Combinator for Case<S, T>
where
    // Any left inner combinator
    // whose input type is `AC`
    S: Combinator<In = AC>,
    // Any right inner combinator
    // whose input type is `BC` and
    // whose output type equals the output type of the left inner combinator
    T: Combinator<In = BC, Out = S::Out>,
    // Any type
    AC: Value,
    // Any type
    // whose right subtype equals the right subtype of `AC`
    BC: Value<B = AC::B>,
{
    // Input type is product type of:
    // 1) Sum type of:
    //    a) Left subtype of input type of left inner combinator
    //    b) Left subtype of input type of right inner combinator
    // 2) Right subtype of input type of left inner combinator
    //    (Equals right subtype of input type of right inner combinator)
    type In = value::Product<value::Sum<AC::A, BC::A>, AC::B>;
    // Output type is output type of left inner combinator
    // (Equals output type of right inner combinator)
    type Out = S::Out;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        // Get left and right inner value of input value
        let (ab, c) = value.as_product()?;
        // Get inner value if `ab` is a left value
        if let Ok(a) = ab.as_left() {
            // Construct input value for left inner combinator
            let ac = AC::product(a.clone(), c.clone())?;
            // Execute left inner combinator on input value
            let d = self.left.exec(ac)?;
            // Return output value of left inner combinator
            Ok(d)
        // Get inner value if `ab` is a right value
        } else if let Ok(b) = ab.as_right() {
            // Construct input value for right inner combinator
            let bc = BC::product(b.clone(), c.clone())?;
            // Execute right inner combinator on input value
            let d = self.right.exec(bc)?;
            // Return output value of right inner combinator
            Ok(d)
        // `ab` has to be either left or right value
        } else {
            // Unreachable if input value has correct input type
            Err(Error::CaseSum)
        }
    }
}

/// `unit : A → 1`
pub fn unit<A: Value>() -> Unit<A> {
    Unit { _i: PhantomData }
}

/// `iden : A → A`
pub fn iden<A: Value>() -> Iden<A> {
    Iden { _i: PhantomData }
}

/// `take t : A × B → C where t : A → C`
pub fn take<T: Combinator, B: Value>(t: T) -> Take<T, B> {
    Take {
        inner: t,
        _i: PhantomData,
    }
}

/// `drop t : A × B → C where t : B → C`
pub fn _drop<T: Combinator, A: Value>(t: T) -> Drop<T, A> {
    Drop {
        inner: t,
        _i: PhantomData,
    }
}

/// `injl t : A → B + C where t : A → B`
pub fn injl<T: Combinator, C: Value>(t: T) -> Injl<T, C> {
    Injl {
        inner: t,
        _i: PhantomData,
    }
}

/// `injr t : A → B + C where t : A → C`
pub fn injr<T: Combinator, B: Value>(t: T) -> Injr<T, B> {
    Injr {
        inner: t,
        _i: PhantomData,
    }
}

/// `pair s t : A → B × C where s : A → B and t : A → C`
pub fn pair<S: Combinator, T: Combinator>(s: S, t: T) -> Pair<S, T> {
    Pair { left: s, right: t }
}

/// `comp s t : A → C where s : A → B and t : B → C`
pub fn comp<S: Combinator, T: Combinator>(s: S, t: T) -> Comp<S, T> {
    Comp { left: s, right: t }
}

/// `case s t : (A + B) × C → D where s : A × C → D and t : B × C → D`
pub fn case<S: Combinator, T: Combinator>(s: S, t: T) -> Case<S, T> {
    Case { left: s, right: t }
}

#[allow(type_alias_bounds)]
pub type False<A: Value> = Injl<Unit<A>, value::Unit>;
#[allow(type_alias_bounds)]
pub type True<A: Value> = Injr<Unit<A>, value::Unit>;

/// `false : A → 2`
pub fn bit_false<A: Value>() -> False<A> {
    injl(unit())
}

/// `true : A → 2`
pub fn bit_true<A: Value>() -> True<A> {
    injr(unit())
}

#[allow(type_alias_bounds)]
pub type Cond<S: Combinator, T: Combinator> = Case<Drop<T, value::Unit>, Drop<S, value::Unit>>;
#[allow(type_alias_bounds)]
pub type Not<T: Combinator> =
    Comp<Pair<T, Unit<T::In>>, Cond<False<value::Unit>, True<value::Unit>>>;

/// `cond : 2 × A → B where s : A → B and t : A → B`
pub fn cond<S: Combinator, T: Combinator>(s: S, t: T) -> Cond<S, T> {
    case(_drop(t), _drop(s))
}

/// `not : A → 2 where t : A → 2`
pub fn not<T: Combinator>(t: T) -> Not<T> {
    comp(pair(t, unit()), cond(bit_false(), bit_true()))
}

pub type Maj1 =
    Cond<Cond<True<value::Bit>, Iden<value::Bit>>, Cond<Iden<value::Bit>, False<value::Bit>>>;
pub type Xor3 = Cond<
    Cond<Iden<value::Bit>, Not<Iden<value::Bit>>>,
    Cond<Not<Iden<value::Bit>>, Iden<value::Bit>>,
>;
pub type FullAdd1 = Pair<Maj1, Xor3>;
pub type HalfAdd1 =
    Cond<Pair<Iden<value::Bit>, Not<Iden<value::Bit>>>, Pair<False<value::Bit>, Iden<value::Bit>>>;

/// `maj : 2 × (2 × 2) → 2`
pub fn maj() -> Maj1 {
    cond(cond(bit_true(), iden()), cond(iden(), bit_false()))
}

/// `xor3 : 2 × (2 × 2) → 2`
pub fn xor3() -> Xor3 {
    cond(cond(iden(), not(iden())), cond(not(iden()), iden()))
}

/// `full_add_1 : 2 × (2 × 2) → 2 × 2`
pub fn full_add_1() -> FullAdd1 {
    pair(maj(), xor3())
}

/// `half_add1 : 2 × 2 → 2 × 2`
pub fn half_add_1() -> HalfAdd1 {
    cond(pair(iden(), not(iden())), pair(bit_false(), iden()))
}

#[allow(type_alias_bounds)]
pub type H<A: Value> = Iden<A>;
#[allow(type_alias_bounds)]
pub type O<T: Combinator, B: Value> = Take<T, B>;
#[allow(type_alias_bounds)]
pub type I<T: Combinator, A: Value> = Drop<T, A>;

/// `h : A → A`
///
/// Same as `iden`
pub fn h<A: Value>() -> H<A> {
    iden()
}

/// `o t : A × B → C where t : A → C`
///
/// Same as `take t`
pub fn o<T: Combinator, B: Value>(t: T) -> O<T, B> {
    take(t)
}

/// `i t : A × B → C where t : B → C`
///
/// Same as `drop t`
pub fn i<T: Combinator, A: Value>(t: T) -> I<T, A> {
    _drop(t)
}

macro_rules! full_add_2n {
    ($Wordn: path, $Word2n: path, $Word4n: path,
    $full_add_n: ident, $full_add_2n: ident,
    $FullAddn: ident, $FullAdd2n: ident,
    $FullAdd2nPart1a: ident, $FullAdd2nPart1b: ident, $FullAdd2nPart1: ident, $FullAdd2nPart2: ident, $FullAdd2nPart3: ident) => {
        type $FullAdd2nPart1a = Drop<
            Pair<O<O<H<$Wordn>, $Wordn>, $Word2n>, I<O<H<$Wordn>, $Wordn>, $Word2n>>,
            value::Bit,
        >;

        type $FullAdd2nPart1b = Pair<
            O<H<value::Bit>, $Word4n>,
            Drop<
                Pair<O<I<H<$Wordn>, $Wordn>, $Word2n>, I<I<H<$Wordn>, $Wordn>, $Word2n>>,
                value::Bit,
            >,
        >;

        type $FullAdd2nPart1 = Pair<$FullAdd2nPart1a, Comp<$FullAdd2nPart1b, $FullAddn>>;

        type $FullAdd2nPart2 = Pair<
            I<I<H<$Wordn>, value::Bit>, $Word2n>,
            Comp<
                Pair<
                    I<O<H<value::Bit>, $Wordn>, $Word2n>,
                    O<H<$Word2n>, value::Product<value::Bit, $Wordn>>,
                >,
                $FullAddn,
            >,
        >;

        type $FullAdd2nPart3 = Pair<
            I<O<H<value::Bit>, $Wordn>, $Wordn>,
            Pair<
                I<I<H<$Wordn>, value::Bit>, $Wordn>,
                O<H<$Wordn>, value::Product<value::Bit, $Wordn>>,
            >,
        >;

        type $FullAdd2n = Comp<$FullAdd2nPart1, Comp<$FullAdd2nPart2, $FullAdd2nPart3>>;

        /// `full_add_2n : 2 × (2^2n × 2^2n) → 2 × 2^2n`
        pub fn $full_add_2n() -> $FullAdd2n {
            /// `full_add_2n_part1a : 2 × (2^2n × 2^2n) → 2^n × 2^n`
            fn full_add_2n_part1a() -> $FullAdd2nPart1a {
                _drop(pair(o(o(h())), i(o(h()))))
            }

            /// `full_add_2n_part1b : 2 × (2^2n × 2^2n) → 2 × 2^n`
            fn full_add_2n_part1b() -> $FullAdd2nPart1b {
                pair(o(h()), _drop(pair(o(i(h())), i(i(h())))))
            }

            /// `full_add_2n_part1 : 2 × (2^2n × 2^2n) → 2^2n × (2 × 2^n)`
            fn full_add_2n_part1() -> $FullAdd2nPart1 {
                pair(
                    full_add_2n_part1a(),
                    comp(full_add_2n_part1b(), $full_add_n()),
                )
            }

            /// `full_add_2n_part2 : 2^2n × (2 × 2^n) → 2^n × (2 × 2^n)`
            fn full_add_2n_part2() -> $FullAdd2nPart2 {
                pair(i(i(h())), comp(pair(i(o(h())), o(h())), $full_add_n()))
            }

            /// `full_add_2n_part3 : 2^n × (2 × 2^n) → 2 × 2^2n`
            fn full_add_2n_part3() -> $FullAdd2nPart3 {
                pair(i(o(h())), pair(i(i(h())), o(h())))
            }

            comp(
                full_add_2n_part1(),
                comp(full_add_2n_part2(), full_add_2n_part3()),
            )
        }
    };
}

full_add_2n!(
    value::Word1,
    value::Word2,
    value::Word4,
    full_add_1,
    full_add_2,
    FullAdd1,
    FullAdd2,
    FullAdd2Part1a,
    FullAdd2Part1b,
    FullAdd2Part1,
    FullAdd2Part2,
    FullAdd2Part3
);

full_add_2n!(
    value::Word2,
    value::Word4,
    value::Word8,
    full_add_2,
    full_add_4,
    FullAdd2,
    FullAdd4,
    FullAdd4Part1a,
    FullAdd4Part1b,
    FullAdd4Part1,
    FullAdd4Part2,
    FullAdd4Part3
);

full_add_2n!(
    value::Word4,
    value::Word8,
    value::Word16,
    full_add_4,
    full_add_8,
    FullAdd4,
    FullAdd8,
    FullAdd8Part1a,
    FullAdd8Part1b,
    FullAdd8Part1,
    FullAdd8Part2,
    FullAdd8Part3
);

full_add_2n!(
    value::Word8,
    value::Word16,
    value::Word32,
    full_add_8,
    full_add_16,
    FullAdd8,
    FullAdd16,
    FullAdd16Part1a,
    FullAdd16Part1b,
    FullAdd16Part1,
    FullAdd16Part2,
    FullAdd16Part3
);

full_add_2n!(
    value::Word16,
    value::Word32,
    value::Word64,
    full_add_16,
    full_add_32,
    FullAdd16,
    FullAdd32,
    FullAdd32Part1a,
    FullAdd32Part1b,
    FullAdd32Part1,
    FullAdd32Part2,
    FullAdd32Part3
);

full_add_2n!(
    value::Word32,
    value::Word64,
    value::Word128,
    full_add_32,
    full_add_64,
    FullAdd32,
    FullAdd64,
    FullAdd64Part1a,
    FullAdd64Part1b,
    FullAdd64Part1,
    FullAdd64Part2,
    FullAdd64Part3
);
