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
