use rustier_simplicity::combinator::*;
use rustier_simplicity::value::*;

fn main() {
    // Unit Constant

    let unit_program = only::<Bit>();
    println!("Unit program:\n{}\n", unit_program);

    for bit in [false, true] {
        let bit_value = Bit::from(bit);
        let output_value = unit_program.exec(bit_value).expect("exec unit");
        println!("unit({}):\n{}\n", bit, output_value);
    }

    // Boolean Identity

    let iden_program = iden::<Bit>();
    println!("Iden program:\n{}\n", iden_program);

    for bit in [false, true] {
        let bit_value = Bit::from(bit);
        let output_value = iden_program.exec(bit_value).expect("exec iden");
        println!("iden({}):\n{}\n", bit, output_value);
    }

    // Boolean Negation

    let not_program = not(iden::<Bit>());
    println!("Not program:\n{}\n", not_program);

    for bit in [false, true] {
        let bit_value = Bit::from(bit);
        let output_value = not_program.exec(bit_value).expect("exec not");
        println!("not({}):\n{}\n", bit, output_value);
    }

    // 1-Bit Half Adder

    let half_adder = half_add_1();
    println!("Half adder:\n{}\n", half_adder);

    for bit0 in [false, true] {
        let bit0_value = Bit::from(bit0);

        for bit1 in [false, true] {
            let bit1_value = Bit::from(bit1);
            let input_value = Product::Product(bit0_value, bit1_value);
            let output_value = half_adder.exec(input_value).expect("exec half_adder");
            // First bit is carry, second bit is sum
            println!("{} + {} = {}\n", bit0, bit1, output_value);
        }
    }

    // 2-Bit Full Adder

    let full_adder = full_add_2();
    println!("Full adder:\n{}\n", full_adder);

    for carry_in in 0..2 {
        let carry_in_value = Word1::from_unwrap(carry_in);

        for a in 0..4 {
            let a_value = Word2::from_unwrap(a);

            for b in 0..4 {
                let b_value = Word2::from_unwrap(b);
                let input_value =
                    Product::Product(carry_in_value, Product::Product(a_value, b_value));
                let output_value = full_adder.exec(input_value).expect("Execute full adder");
                let (carry_out_value, sum_value) = match output_value {
                    Product::Product(x, y) => (x, y),
                };
                let carry_out = u8::from(&carry_out_value);
                let sum = u8::from(&sum_value);

                assert_eq!(a + b + carry_in, sum + (carry_out << 2));
            }
        }
    }

    // 64-Bit Full Adder

    let big_adder = full_add_64();
    // 10000 lines output
    // println!("Big adder:\n{}\n", big_adder);

    for carry_in in 0..1 {
        let carry_in_value = Word1::from_unwrap(carry_in);

        for a in 0..100 {
            let a_value = Word64::from(a);

            for b in 0..100 {
                let b_value = Word64::from(b);
                let input_value =
                    Product::Product(carry_in_value, Product::Product(a_value, b_value));
                let output_value = big_adder.exec(input_value).expect("Execute big adder");
                let (carry_out_value, sum_value) = match output_value {
                    Product::Product(x, y) => (x, y),
                };
                let carry_out = u8::from(&carry_out_value);
                let sum = u64::from(&sum_value);

                assert_eq!(
                    a as u128 + b as u128 + carry_in as u128,
                    sum as u128 + ((carry_out as u128) << 64)
                );
            }
        }
    }
}
