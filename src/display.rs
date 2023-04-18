use crate::combinator;
use crate::value::{Product, Sum, Unit};
use std::fmt;

const INDENT_FACTOR: usize = 2;

pub trait DisplayDepth: fmt::Display {
    fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result;
}

fn fmt_depth_leaf(name: &str, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:indent$}{}", "", name, indent = depth * INDENT_FACTOR)
}

fn fmt_depth_single<A>(name: &str, depth: usize, f: &mut fmt::Formatter, a: &A) -> fmt::Result
where
    A: DisplayDepth,
{
    writeln!(
        f,
        "{:indent$}{}({}",
        "",
        name,
        depth,
        indent = depth * INDENT_FACTOR
    )?;
    a.fmt_depth(depth + 1, f)?;
    f.write_str("\n")?;
    write!(f, "{:indent$})", "", indent = depth * INDENT_FACTOR)
}

fn fmt_depth_double<A, B>(
    name: &str,
    depth: usize,
    f: &mut fmt::Formatter,
    a: &A,
    b: &B,
) -> fmt::Result
where
    A: DisplayDepth,
    B: DisplayDepth,
{
    writeln!(
        f,
        "{:indent$}{}({}",
        "",
        name,
        depth,
        indent = depth * INDENT_FACTOR
    )?;
    a.fmt_depth(depth + 1, f)?;
    f.write_str(",\n")?;
    b.fmt_depth(depth + 1, f)?;
    f.write_str("\n")?;
    write!(f, "{:indent$})", "", indent = depth * INDENT_FACTOR)
}

macro_rules! impl_display_leaf {
    ($structure:path, $name:expr) => {
        impl DisplayDepth for $structure {
            fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
                fmt_depth_leaf($name, depth, f)
            }
        }

        impl fmt::Display for $structure {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.fmt_depth(0, f)
            }
        }
    };
}

// Doesn't work for value::Sum because of different structure
// TODO: Change structure for value::Sum
macro_rules! impl_display_single {
    ($structure:path, $name:expr, $inner_trait:path) => {
        impl<A: $inner_trait> DisplayDepth for $structure {
            fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
                fmt_depth_single($name, depth, f, &self.inner)
            }
        }

        impl<A: $inner_trait> fmt::Display for $structure {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.fmt_depth(0, f)
            }
        }
    };
}

// Doesn't work for value::Product because of `&self.left` and `&self.right`
// TODO: Add macros that add getters?
macro_rules! impl_display_double {
    ($structure:path, $name:expr, $inner_trait:path) => {
        impl<A: $inner_trait, B: $inner_trait> DisplayDepth for $structure {
            fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
                fmt_depth_double($name, depth, f, &self.left, &self.right)
            }
        }

        impl<A: $inner_trait, B: $inner_trait> fmt::Display for $structure {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.fmt_depth(0, f)
            }
        }
    };
}

impl_display_leaf!(Unit, "unit");

impl<A, B> DisplayDepth for Sum<A, B>
where
    A: DisplayDepth,
    B: DisplayDepth,
{
    fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Sum::Left(a) => fmt_depth_single("L", depth, f, a),
            Sum::Right(b) => fmt_depth_single("R", depth, f, b),
        }
    }
}

impl<A, B> fmt::Display for Sum<A, B>
where
    A: DisplayDepth,
    B: DisplayDepth,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_depth(0, f)
    }
}

impl<A, B> DisplayDepth for Product<A, B>
where
    A: DisplayDepth,
    B: DisplayDepth,
{
    fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Product::Product(a, b) => fmt_depth_double("", depth, f, a, b),
        }
    }
}

impl<A, B> fmt::Display for Product<A, B>
where
    A: DisplayDepth,
    B: DisplayDepth,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_depth(0, f)
    }
}

impl_display_leaf!(combinator::Unit, "unit");

impl_display_leaf!(combinator::Iden, "iden");

impl_display_single!(combinator::Take<A>, "take", combinator::Combinator);

impl_display_single!(combinator::Drop<A>, "drop", combinator::Combinator);

impl_display_single!(combinator::Injl<A>, "injl", combinator::Combinator);

impl_display_single!(combinator::Injr<A>, "injr", combinator::Combinator);

impl_display_double!(combinator::Pair<A, B>, "pair", combinator::Combinator);

impl_display_double!(combinator::Comp<A, B>, "comp", combinator::Combinator);

impl_display_double!(combinator::Case<A, B>, "case", combinator::Combinator);
