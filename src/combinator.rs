use crate::display::DisplayDepth;
use crate::value;
use crate::value::Value;
use std::marker::PhantomData;

pub trait Combinator: DisplayDepth {
    type In: Value;
    type Out: Value;

    fn exec(&self, value: Self::In) -> Result<Self::Out, ()>;
}

/// Atomic unit combinator.
///
/// Takes any input and returns the unit value.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Unit<I: Value> {
    _i: PhantomData<I>,
}

/// Atomic iden combinator (identity).
///
/// Takes any input and returns it back.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Iden<I: Value> {
    _i: PhantomData<I>,
}

/// Take combinator (left projection).
///
/// Contains inner combinator of type `A → C`.
///
/// Takes an input `(a, b)` of type `A × B`,
/// passes the left value `a` of type `A` to the inner combinator,
/// and returns a value `c` of type `C`.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Take<A: Combinator, I: Value> {
    pub inner: A,
    _i: PhantomData<I>,
}

/// Drop combinator (right projection).
///
/// Contains inner combinator of type `B → C`.
///
/// Takes an input `(a, b)` of type `A × B`,
/// passes the right value `b` of type `B` to the inner combinator,
/// and returns a value `c` of type `C`.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Drop<A: Combinator, I: Value> {
    pub inner: A,
    _i: PhantomData<I>,
}

/// Injl combinator (left injection).
///
/// Contains inner combinator of type `A → B`.
///
/// Takes an input `a` of type `A`,
/// passes the value `a` to the inner combinator to obtain a value `b` of type `B`,
/// and returns the value `L(b)` of type `B + C`.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Injl<A: Combinator, I: Value> {
    pub inner: A,
    _i: PhantomData<I>,
}

/// Injr combinator (right injection).
///
/// Contains inner combinator of type `A → C`.
///
/// Takes an input `a` of type `A`,
/// passes the value `a` to the inner combinator to obtain a value `c` of type `C`,
/// and returns the value `R(c)` of type `B + C`.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Injr<A: Combinator, I: Value> {
    pub inner: A,
    _i: PhantomData<I>,
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
pub struct Pair<A: Combinator, B: Combinator> {
    pub left: A,
    pub right: B,
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
pub struct Comp<A: Combinator, B: Combinator> {
    pub left: A,
    pub right: B,
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
pub struct Case<A: Combinator, B: Combinator> {
    pub left: A,
    pub right: B,
}

impl<I> Combinator for Unit<I>
where
    I: Value,
{
    type In = I;
    type Out = value::Unit;

    fn exec(&self, _value: Self::In) -> Result<Self::Out, ()> {
        Ok(value::Unit::Unit)
    }
}

impl<I> Combinator for Iden<I>
where
    I: Value,
{
    type In = I;
    type Out = I;

    fn exec(&self, value: Self::In) -> Result<Self::Out, ()> {
        Ok(value)
    }
}

impl<A, I> Combinator for Take<A, I>
where
    A: Combinator,
    I: Value,
{
    type In = value::Product<A::In, I>;
    type Out = A::Out;

    fn exec(&self, value: Self::In) -> Result<Self::Out, ()> {
        let (a, _) = value.split_product().ok_or(())?;
        let c = self.inner.exec(a.clone())?;
        Ok(c)
    }
}

impl<A, I> Combinator for Drop<A, I>
where
    A: Combinator,
    I: Value,
{
    type In = value::Product<I, A::In>;
    type Out = A::Out;

    fn exec(&self, value: Self::In) -> Result<Self::Out, ()> {
        let (_, b) = value.split_product().ok_or(())?;
        let c = self.inner.exec(b.clone())?;
        Ok(c)
    }
}

impl<A, I> Combinator for Injl<A, I>
where
    A: Combinator,
    I: Value,
{
    type In = A::In;
    type Out = value::Sum<A::Out, I>;

    fn exec(&self, value: Self::In) -> Result<Self::Out, ()> {
        let c = self.inner.exec(value)?;
        Ok(value::Sum::Left(c))
    }
}

impl<A, I> Combinator for Injr<A, I>
where
    A: Combinator,
    I: Value,
{
    type In = A::In;
    type Out = value::Sum<I, A::Out>;

    fn exec(&self, value: Self::In) -> Result<Self::Out, ()> {
        let c = self.inner.exec(value)?;
        Ok(value::Sum::Right(c))
    }
}

impl<A, B> Combinator for Pair<A, B>
where
    A: Combinator<In = B::In>,
    B: Combinator,
{
    type In = A::In;
    type Out = value::Product<A::Out, B::Out>;

    fn exec(&self, value: Self::In) -> Result<Self::Out, ()> {
        let b = self.left.exec(value.clone())?;
        let c = self.right.exec(value)?;
        Ok(value::Product::Product(b, c))
    }
}

impl<A, B> Combinator for Comp<A, B>
where
    A: Combinator<Out = B::In>,
    B: Combinator,
{
    type In = A::In;
    type Out = B::Out;

    fn exec(&self, value: Self::In) -> Result<Self::Out, ()> {
        let b = self.left.exec(value)?;
        let c = self.right.exec(b)?;
        Ok(c)
    }
}

impl<A, B, I, J> Combinator for Case<A, B>
where
    A: Combinator<In = I, Out = B::Out>,
    B: Combinator<In = J>,
    I: Value<R = J::R>,
    J: Value,
{
    type In = value::Product<value::Sum<I::L, J::L>, I::R>;
    type Out = A::Out;

    fn exec(&self, value: Self::In) -> Result<Self::Out, ()> {
        let (ab, c) = value.split_product().ok_or(())?;
        if let Some(a) = ab.split_left() {
            let ac = I::join_product(a.clone(), c.clone()).ok_or(())?;
            let d = self.left.exec(ac)?;
            Ok(d)
        } else if let Some(b) = ab.split_right() {
            let bc = J::join_product(b.clone(), c.clone()).ok_or(())?;
            let d = self.right.exec(bc)?;
            Ok(d)
        } else {
            Err(())
        }
    }
}
