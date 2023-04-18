use crate::display::DisplayDepth;

pub trait Combinator: DisplayDepth {}

/// Atomic unit combinator.
///
/// Takes any input and returns the unit value.
pub struct Unit {}

/// Atomic iden combinator (identity).
///
/// Takes any input and returns it back.
pub struct Iden {}

/// Take combinator (left projection).
///
/// Contains inner combinator of type `A → C`.
///
/// Takes an input `(a, b)` of type `A × B`,
/// passes the left value `a` of type `A` to the inner combinator,
/// and returns a value `c` of type `C`.
pub struct Take<A: Combinator> {
    pub inner: A,
}

/// Drop combinator (right projection).
///
/// Contains inner combinator of type `B → C`.
///
/// Takes an input `(a, b)` of type `A × B`,
/// passes the right value `b` of type `B` to the inner combinator,
/// and returns a value `c` of type `C`.
pub struct Drop<A: Combinator> {
    pub inner: A,
}

/// Injl combinator (left injection).
///
/// Contains inner combinator of type `A → B`.
///
/// Takes an input `a` of type `A`,
/// passes the value `a` to the inner combinator to obtain a value `b` of type `B`,
/// and returns the value `L(b)` of type `B + C`.
pub struct Injl<A: Combinator> {
    pub inner: A,
}

/// Injr combinator (right injection).
///
/// Contains inner combinator of type `A → C`.
///
/// Takes an input `a` of type `A`,
/// passes the value `a` to the inner combinator to obtain a value `c` of type `C`,
/// and returns the value `R(c)` of type `B + C`.
pub struct Injr<A: Combinator> {
    pub inner: A,
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
pub struct Case<A: Combinator, B: Combinator> {
    pub left: A,
    pub right: B,
}

impl Combinator for Unit {}

impl Combinator for Iden {}

impl<A: Combinator> Combinator for Take<A> {}

impl<A: Combinator> Combinator for Drop<A> {}

impl<A: Combinator> Combinator for Injl<A> {}

impl<A: Combinator> Combinator for Injr<A> {}

impl<A: Combinator, B: Combinator> Combinator for Pair<A, B> {}

impl<A: Combinator, B: Combinator> Combinator for Comp<A, B> {}

impl<A: Combinator, B: Combinator> Combinator for Case<A, B> {}
