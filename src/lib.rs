pub mod combinator;
mod display;
mod error;
pub mod value;

/// Print type of any given Rust value.
///
/// Useful for debugging.
pub fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

/// Compute maximum size at compile time.
pub const fn usize_max(a: usize, b: usize) -> usize {
    if a < b {
        b
    } else {
        a
    }
}
