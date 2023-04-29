use simplicity_playground::combinator::{
    _drop, case, comp, half_add1, iden, injl, injr, not, pair, unit, Combinator,
};
use simplicity_playground::{combinator, value};

fn main() {
    // Unit constant

    let unit_program = unit::<value::Bit>();
    println!("Unit program:\n{}\n", unit_program);

    for bit in [false, true] {
        let bit_value = value::bit_to_value(bit);
        let output_value = unit_program.exec(bit_value).expect("exec unit");
        println!("unit({}):\n{}\n", bit, output_value);
    }

    // Boolean identity

    let iden_program = iden::<value::Bit>();
    println!("Iden program:\n{}\n", iden_program);

    for bit in [false, true] {
        let bit_value = value::bit_to_value(bit);
        let output_value = iden_program.exec(bit_value).expect("exec iden");
        println!("iden({}):\n{}\n", bit, output_value);
    }

    // Boolean negation

    let not_program = not(iden::<value::Bit>());
    println!("Not program:\n{}\n", not_program);

    for bit in [false, true] {
        let bit_value = value::bit_to_value(bit);
        let output_value = not_program.exec(bit_value).expect("exec not");
        println!("not({}):\n{}\n", bit, output_value);
    }

    // 1-Bit Half adder

    let half_adder = half_add1();
    println!("Half adder:\n{}\n", half_adder);

    for bit0 in [false, true] {
        let bit0_value = value::bit_to_value(bit0);

        for bit1 in [false, true] {
            let bit1_value = value::bit_to_value(bit1);
            let input_value = value::Product::Product(bit0_value, bit1_value);
            let output_value = half_adder.exec(input_value).expect("exec half_adder");
            // First bit is carry, second bit is sum
            println!("{} + {} = {}\n", bit0, bit1, output_value);
        }
    }
}
