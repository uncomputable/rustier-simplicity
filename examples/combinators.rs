use simplicity_playground::combinator::{
    _drop, case, comp, iden, injl, injr, pair, unit, Combinator,
};
use simplicity_playground::{combinator, value};

fn main() {
    // Unit program

    let unit_program = unit::<value::Bit>();
    println!("Unit program:\n{}\n", unit_program);

    for bit in [false, true] {
        let bit_value = value::bit_to_value(bit);
        let output_value = unit_program.exec(bit_value).expect("exec unit");
        println!("unit({}):\n{}\n", bit, output_value);
    }

    // Iden program

    let iden_program = iden::<value::Bit>();
    println!("Iden program:\n{}\n", iden_program);

    for bit in [false, true] {
        let bit_value = value::bit_to_value(bit);
        let output_value = iden_program.exec(bit_value).expect("exec iden");
        println!("iden({}):\n{}\n", bit, output_value);
    }

    // Boolean negation

    // not_iden0: 2 → 2 × 1
    let not_iden0 = pair(iden::<value::Bit>(), unit());
    // bit_false: 1 × 1 → 2
    let bit_false = injl::<_, value::Unit>(unit::<value::Product<value::Unit, value::Unit>>());
    // bit_true: 1 × 1 → 2
    let bit_true = injr::<_, value::Unit>(unit::<value::Product<value::Unit, value::Unit>>());
    // not_iden1: 2 × 1 → 2
    let not_iden1 = case(bit_true, bit_false);
    // not_iden: 2 → 2
    let not_iden = comp(not_iden0, not_iden1);

    println!("Not program:\n{}\n", not_iden);

    for bit in [false, true] {
        let bit_value = value::bit_to_value(bit);
        let output_value = not_iden.exec(bit_value).expect("exec not");
        println!("not({}):\n{}\n", bit, output_value);
    }

    // Half adder

    // bit_false: 2 → 2
    let bit_false = injl::<_, value::Unit>(unit::<value::Bit>());
    // half_adder0: 2 → 2 × 2
    let half_adder0 = pair(bit_false, iden::<value::Bit>());
    // half_adder1: 2 → 2 × 2
    let half_adder1 = pair(iden::<value::Bit>(), not_iden);
    // drop0: 1 × 2 → 2
    let drop0 = _drop::<_, value::Unit>(half_adder0);
    // drop1: 1 × 2 → 2
    let drop1 = _drop::<_, value::Unit>(half_adder1);
    // half_adder: 2 × 2 → 2 × 2
    let half_adder = case(drop0, drop1);

    println!("Half adder:\n{}\n", half_adder);

    for bit0 in [false, true] {
        let bit0_value = value::bit_to_value(bit0);

        for bit1 in [false, true] {
            let bit1_value = value::bit_to_value(bit1);
            let input_value = value::Product::Product(bit0_value, bit1_value);
            let output_value = half_adder.exec(input_value).expect("exec half_adder");
            println!("{} + {} = {}\n", bit0, bit1, output_value);
        }
    }
}
