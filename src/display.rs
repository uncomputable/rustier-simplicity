use crate::types::{Product, Sum, Unit};
use std::fmt;

const INDENT_FACTOR: usize = 2;

trait DisplayDepth: fmt::Display {
    fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result;
}

impl DisplayDepth for Unit {
    fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:indent$}·", "", indent = depth * INDENT_FACTOR)
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_depth(0, f)
    }
}

impl<A, B> DisplayDepth for Sum<A, B>
where
    A: DisplayDepth,
    B: DisplayDepth,
{
    fn fmt_depth(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Sum::Left(a) => {
                writeln!(
                    f,
                    "{:indent$}L({}",
                    "",
                    depth,
                    indent = depth * INDENT_FACTOR
                )?;
                a.fmt_depth(depth + 1, f)?;
                f.write_str("\n")?;
                write!(f, "{:indent$})", "", indent = depth * INDENT_FACTOR)
            }
            Sum::Right(b) => {
                writeln!(
                    f,
                    "{:indent$}R({}",
                    "",
                    depth,
                    indent = depth * INDENT_FACTOR
                )?;
                b.fmt_depth(depth + 1, f)?;
                f.write_str("\n")?;
                write!(f, "{:indent$})", "", indent = depth * INDENT_FACTOR)
            }
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
            Product::Product(a, b) => {
                writeln!(
                    f,
                    "{:indent$}({}",
                    "",
                    depth,
                    indent = depth * INDENT_FACTOR
                )?;
                a.fmt_depth(depth + 1, f)?;
                f.write_str(",\n")?;
                b.fmt_depth(depth + 1, f)?;
                f.write_str("\n")?;
                write!(f, "{:indent$})", "", indent = depth * INDENT_FACTOR)
            }
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
