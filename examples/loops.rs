use rustier_simplicity::combinator::*;
use rustier_simplicity::value::*;

fn main() {
    // Sum all counter values from 0 to 2^8 - 1
    //
    // f : (1 × 2^8) × 2^16 → 1 + 2^16
    // ```
    // f(_ctx: (), counter: u8, acc: u16) -> u16 {
    //     (counter as u16).wrapping_add(acc)
    // }
    // ```
    let f = injr::<Unit, _>(comp(
        pair(
            o::<_, Word16>(i::<Unit, _>(pair(scribe_0u8::<Word8>(), iden::<Word8>()))),
            i::<Product<Unit, Word8>, _>(h::<Word16>()),
        ),
        wrapping_add_16(),
    ));
    // A loop for at most 2^8 iterations is compatible with f, which expects Word8 counters
    let for_while = for_while_8(f);
    // The initial accumulator is zero
    let acc_in = Word16::from(0);
    // Run the loop
    let res = for_while
        .exec(Product::Product(Unit::Unit, acc_in))
        .unwrap();
    // The loop output is the final accumulator
    let acc_out = res.as_right().unwrap();
    // The final accumulator is the sum of all counter values
    assert_eq!(32640, u16::from(acc_out));
}
