use simplicity_playground::combinator::{Iden, Injl, Injr, Unit};

fn main() {
    // Unit program
    let unit = Unit {};
    println!("Unit program:\n{}\n", unit);

    // Iden program
    let iden = Iden {};
    println!("Iden program:\n{}\n", iden);

    // Bit false program
    let bit_false = Injl { inner: Unit {} };
    println!("Bit false program:\n{}\n", bit_false);

    // Bit true program
    let bit_true = Injr { inner: Unit {} };
    println!("Bit true program:\n{}\n", bit_true);
}
