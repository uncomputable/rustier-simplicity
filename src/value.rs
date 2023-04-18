use crate::display::DisplayDepth;

pub trait Value: DisplayDepth {}

/// Atomic unit type.
///
/// Unit values contain no information whatsoever.
/// Information stems from the way unit values are combined inside sum and product values.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Unit {
    /// Unit value
    Unit,
}

/// Sum type of a left type `A` and a right type `B`.
///
/// Either a `Left` value wraps an inner value of type `A`,
/// or a `Right` value wraps an inner value of type `B`.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Sum<A: Value, B: Value> {
    /// Left value
    Left(A),
    /// Right value
    Right(B),
}

/// Product type of a left type `A` and a right type `B`.
///
/// The `Product` value wraps both a left inner value of type `A`
/// and a right inner value of type `B`.
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Product<A: Value, B: Value> {
    /// Product value
    Product(A, B),
}

impl Value for Unit {}

impl<A: Value, B: Value> Value for Sum<A, B> {}

impl<A: Value, B: Value> Value for Product<A, B> {}

/// Bits are sums of unit
pub type Bit = Sum<Unit, Unit>;

pub fn bit_to_value(bit: bool) -> Bit {
    match bit {
        // False bit becomes left value that wraps unit value
        false => Sum::Left(Unit::Unit),
        // True bit becomes right value that wraps unit value
        true => Sum::Right(Unit::Unit),
    }
}

/// Bytes are nested products of eighth bits
pub type Byte = Product<
    Product<Product<Bit, Bit>, Product<Bit, Bit>>,
    Product<Product<Bit, Bit>, Product<Bit, Bit>>,
>;

pub fn byte_to_value(n: u8) -> Byte {
    let first_bit = bit_to_value(n & 128 != 0);
    let second_bit = bit_to_value(n & 64 != 0);
    let third_bit = bit_to_value(n & 32 != 0);
    let forth_bit = bit_to_value(n & 16 != 0);
    let fifth_bit = bit_to_value(n & 8 != 0);
    let sixth_bit = bit_to_value(n & 4 != 0);
    let seventh_bit = bit_to_value(n & 2 != 0);
    let eighth_bit = bit_to_value(n & 1 != 0);

    Product::Product(
        Product::Product(
            Product::Product(first_bit, second_bit),
            Product::Product(third_bit, forth_bit),
        ),
        Product::Product(
            Product::Product(fifth_bit, sixth_bit),
            Product::Product(seventh_bit, eighth_bit),
        ),
    )
}

pub type Word8 = Byte;
pub type Word16 = Product<Word8, Word8>;
pub type Word32 = Product<Word16, Word16>;
pub type Word64 = Product<Word32, Word32>;
pub type Word128 = Product<Word64, Word64>;
pub type Word258 = Product<Word128, Word128>;

pub fn u16_to_value(n: u16) -> Word16 {
    let left = (n >> 8) as u8;
    let right = (n & 0xff) as u8;
    Product::Product(byte_to_value(left), byte_to_value(right))
}

pub fn u32_to_value(n: u32) -> Word32 {
    let left = (n >> 16) as u16;
    let right = (n & 0xffff) as u16;
    Product::Product(u16_to_value(left), u16_to_value(right))
}

pub fn u64_to_value(n: u64) -> Word64 {
    let left = (n >> 32) as u32;
    let right = (n & 0xffff_ffff) as u32;
    Product::Product(u32_to_value(left), u32_to_value(right))
}

pub fn u128_to_value(n: u128) -> Word128 {
    let left = (n >> 64) as u64;
    // Cast picks last bytes
    let right = n as u64;
    Product::Product(u64_to_value(left), u64_to_value(right))
}
