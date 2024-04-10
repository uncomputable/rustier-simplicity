use crate::combinator::*;
use crate::value::*;
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
        impl<I: Value> DisplayDepth for $structure {
            fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
                fmt_depth_leaf($name, depth, f)
            }
        }

        impl<I: Value> fmt::Display for $structure {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.fmt_depth(0, f)
            }
        }
    };
}

// Doesn't work for Sum because of different structure
// TODO: Change structure for Sum
macro_rules! impl_display_single {
    ($structure:path, $name:expr) => {
        impl<A: Combinator, I: Value> DisplayDepth for $structure {
            fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
                fmt_depth_single($name, depth, f, &self.inner)
            }
        }

        impl<A: Combinator, I: Value> fmt::Display for $structure {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.fmt_depth(0, f)
            }
        }
    };
}

// Doesn't work for Product because of `&self.left` and `&self.right`
// TODO: Add macros that add getters?
macro_rules! impl_display_double {
    ($structure:path, $name:expr) => {
        impl<A: Combinator, B: Combinator> DisplayDepth for $structure {
            fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
                fmt_depth_double($name, depth, f, &self.left, &self.right)
            }
        }

        impl<A: Combinator, B: Combinator> fmt::Display for $structure {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.fmt_depth(0, f)
            }
        }
    };
}

impl DisplayDepth for Unit {
    fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_depth_leaf("●", depth, f)
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_depth(0, f)
    }
}

impl<A: Value, B: Value> DisplayDepth for Sum<A, B> {
    fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Sum::Left(a) => fmt_depth_single("L", depth, f, a),
            Sum::Right(b) => fmt_depth_single("R", depth, f, b),
        }
    }
}

impl<A: Value, B: Value> fmt::Display for Sum<A, B> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_depth(0, f)
    }
}

impl<A: Value, B: Value> DisplayDepth for Product<A, B> {
    fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Product::Product(a, b) => fmt_depth_double("", depth, f, a, b),
        }
    }
}

impl<A: Value, B: Value> fmt::Display for Product<A, B> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_depth(0, f)
    }
}

impl_display_leaf!(Only<I>, "only");

impl_display_leaf!(Iden<I>, "iden");

impl_display_single!(Take<A, I>, "take");

impl_display_single!(Drop<I, A>, "drop");

impl_display_single!(Injl<A, I>, "injl");

impl_display_single!(Injr<I, A>, "injr");

impl_display_double!(Pair<A, B>, "pair");

impl_display_double!(Comp<A, B>, "comp");

impl_display_double!(Case<A, B>, "case");
