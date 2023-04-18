use crate::types::{bit_to_value, byte_to_value, u64_to_value, Product, Sum, Unit};

mod combinator;
mod display;
#[allow(dead_code)]
mod types;

fn main() {
    // Unit value
    let unit = Unit::Unit;
    println!("Unit value:\n{}\n", unit);

    // Left value
    // Left type is given by inner value
    // Right type is unknown and must be specified
    let left_unit: Sum<_, Unit> = Sum::Left(Unit::Unit);
    println!("Left value:\n{}\n", left_unit);

    // Right value
    // Right type is given by inner value
    // Left type is unknown and must be specified
    let right_unit: Sum<Unit, _> = Sum::Right(Unit::Unit);
    println!("Right value:\n{}\n", right_unit);

    assert_ne!(
        left_unit, right_unit,
        "Left values are distinct from right values, even when their inner value is the same"
    );

    // Product value
    // Left type is given by left inner value
    // Right type is given by right inner value
    let unit_times_unit = Product::Product(Unit::Unit, Unit::Unit);
    println!("Product value:\n{}\n", unit_times_unit);

    // Bit values
    let false_bit = bit_to_value(false);
    println!("False bit value:\n{}\n", false_bit);

    let true_bit = bit_to_value(true);
    println!("True bit value:\n{}\n", true_bit);

    // Byte values
    let byte = byte_to_value(12);
    println!("Byte value:\n{}\n", byte);

    // Word values
    let word = u64_to_value(1337);
    println!("Word value:\n{}", word);
}
