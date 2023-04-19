use crate::display::DisplayDepth;
use crate::error::Error;
use crate::value;
use crate::value::Value;
use std::marker::PhantomData;

pub trait Combinator: DisplayDepth {
    type In: Value;
    type Out: Value;

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
    pub inner: T,
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
    pub inner: T,
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
    pub inner: T,
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
    pub inner: T,
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
    pub left: S,
    pub right: T,
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
    pub left: S,
    pub right: T,
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
    pub left: S,
    pub right: T,
}

impl<A> Combinator for Unit<A>
where
    A: Value,
{
    type In = A;
    type Out = value::Unit;

    fn exec(&self, _value: Self::In) -> Result<Self::Out, Error> {
        Ok(value::Unit::Unit)
    }
}

impl<A> Combinator for Iden<A>
where
    A: Value,
{
    type In = A;
    type Out = A;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        Ok(value)
    }
}

impl<T, B> Combinator for Take<T, B>
where
    T: Combinator,
    B: Value,
{
    type In = value::Product<T::In, B>;
    type Out = T::Out;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        let (a, _) = value.split_product()?;
        let c = self.inner.exec(a.clone())?;
        Ok(c)
    }
}

impl<T, A> Combinator for Drop<T, A>
where
    T: Combinator,
    A: Value,
{
    type In = value::Product<A, T::In>;
    type Out = T::Out;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        let (_, b) = value.split_product()?;
        let c = self.inner.exec(b.clone())?;
        Ok(c)
    }
}

impl<T, C> Combinator for Injl<T, C>
where
    T: Combinator,
    C: Value,
{
    type In = T::In;
    type Out = value::Sum<T::Out, C>;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        let c = self.inner.exec(value)?;
        Ok(value::Sum::Left(c))
    }
}

impl<T, B> Combinator for Injr<T, B>
where
    T: Combinator,
    B: Value,
{
    type In = T::In;
    type Out = value::Sum<B, T::Out>;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        let c = self.inner.exec(value)?;
        Ok(value::Sum::Right(c))
    }
}

impl<S, T> Combinator for Pair<S, T>
where
    S: Combinator<In = T::In>,
    T: Combinator,
{
    type In = S::In;
    type Out = value::Product<S::Out, T::Out>;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        let b = self.left.exec(value.clone())?;
        let c = self.right.exec(value)?;
        Ok(value::Product::Product(b, c))
    }
}

impl<S, T> Combinator for Comp<S, T>
where
    S: Combinator<Out = T::In>,
    T: Combinator,
{
    type In = S::In;
    type Out = T::Out;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        let b = self.left.exec(value)?;
        let c = self.right.exec(b)?;
        Ok(c)
    }
}

impl<S, T, AC, BC> Combinator for Case<S, T>
where
    S: Combinator<In = AC, Out = T::Out>,
    T: Combinator<In = BC>,
    AC: Value<B = BC::B>,
    BC: Value,
{
    type In = value::Product<value::Sum<AC::A, BC::A>, AC::B>;
    type Out = S::Out;

    fn exec(&self, value: Self::In) -> Result<Self::Out, Error> {
        let (ab, c) = value.split_product()?;
        if let Ok(a) = ab.split_left() {
            let ac = AC::join_product(a.clone(), c.clone())?;
            let d = self.left.exec(ac)?;
            Ok(d)
        } else if let Ok(b) = ab.split_right() {
            let bc = BC::join_product(b.clone(), c.clone())?;
            let d = self.right.exec(bc)?;
            Ok(d)
        } else {
            Err(Error::CaseSum)
        }
    }
}
