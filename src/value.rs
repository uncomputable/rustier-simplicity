use crate::display::DisplayDepth;
use crate::error::Error;

/// Type for a range of values.
pub trait Value: DisplayDepth + Clone {
    /// Left subtype
    type A: Value;
    /// Right subtype
    type B: Value;

    /// Access the inner value of a left value.
    fn as_left(&self) -> Result<&Self::A, Error>;

    /// Access the inner value of a right value.
    fn as_right(&self) -> Result<&Self::B, Error>;

    /// Access the inner values of a product value.
    fn as_product(&self) -> Result<(&Self::A, &Self::B), Error>;

    /// Create a product value of `a` and `b`.
    /// Fails on non-product types.
    fn product(a: Self::A, b: Self::B) -> Result<Self, Error>;
}

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

impl Value for Unit {
    type A = Unit;
    type B = Unit;

    fn as_left(&self) -> Result<&Self::A, Error> {
        Err(Error::SplitLeft)
    }

    fn as_right(&self) -> Result<&Self::B, Error> {
        Err(Error::SplitRight)
    }

    fn as_product(&self) -> Result<(&Self::A, &Self::B), Error> {
        Err(Error::SplitProduct)
    }

    fn product(_a: Self::A, _b: Self::B) -> Result<Self, Error> {
        Err(Error::JoinProduct)
    }
}

impl<A: Value, B: Value> Value for Sum<A, B> {
    type A = A;
    type B = B;

    fn as_left(&self) -> Result<&Self::A, Error> {
        match self {
            Sum::Left(a) => Ok(a),
            Sum::Right(_b) => Err(Error::SplitLeft),
        }
    }

    fn as_right(&self) -> Result<&Self::B, Error> {
        match self {
            Sum::Left(_a) => Err(Error::SplitRight),
            Sum::Right(b) => Ok(b),
        }
    }

    fn as_product(&self) -> Result<(&Self::A, &Self::B), Error> {
        Err(Error::SplitProduct)
    }

    fn product(_a: Self::A, _b: Self::B) -> Result<Self, Error> {
        Err(Error::JoinProduct)
    }
}

impl<A: Value, B: Value> Value for Product<A, B> {
    type A = A;
    type B = B;

    fn as_left(&self) -> Result<&Self::A, Error> {
        Err(Error::SplitLeft)
    }

    fn as_right(&self) -> Result<&Self::B, Error> {
        Err(Error::SplitRight)
    }

    fn as_product(&self) -> Result<(&Self::A, &Self::B), Error> {
        match self {
            Product::Product(a, b) => Ok((a, b)),
        }
    }

    fn product(a: Self::A, b: Self::B) -> Result<Self, Error> {
        Ok(Product::Product(a, b))
    }
}

/// Bit type.
///
/// Bits are sums of unit.
pub type Bit = Sum<Unit, Unit>;

/// Convert a bit to a value.
pub fn from_bit(bit: bool) -> Bit {
    match bit {
        // False bit becomes left value that wraps unit value
        false => Sum::Left(Unit::Unit),
        // True bit becomes right value that wraps unit value
        true => Sum::Right(Unit::Unit),
    }
}

/// Byte type.
///
/// Bytes are nested products of eighth bits.
pub type Byte = Product<
    Product<Product<Bit, Bit>, Product<Bit, Bit>>,
    Product<Product<Bit, Bit>, Product<Bit, Bit>>,
>;

/// Convert a byte to a value.
pub fn from_byte(n: u8) -> Byte {
    let first_bit = from_bit(n & 128 != 0);
    let second_bit = from_bit(n & 64 != 0);
    let third_bit = from_bit(n & 32 != 0);
    let forth_bit = from_bit(n & 16 != 0);
    let fifth_bit = from_bit(n & 8 != 0);
    let sixth_bit = from_bit(n & 4 != 0);
    let seventh_bit = from_bit(n & 2 != 0);
    let eighth_bit = from_bit(n & 1 != 0);

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

/// 1-bit word type.
pub type Word1 = Bit;
/// 2-bit word type.
pub type Word2 = Product<Word1, Word1>;
/// 4-bit word type.
pub type Word4 = Product<Word2, Word2>;
/// 8-bit word type.
pub type Word8 = Product<Word4, Word4>;
/// 16-bit word type.
pub type Word16 = Product<Word8, Word8>;
/// 32-bit word type.
pub type Word32 = Product<Word16, Word16>;
/// 64-bit word type.
pub type Word64 = Product<Word32, Word32>;
/// 128-bit word type.
pub type Word128 = Product<Word64, Word64>;
/// 256-bit word type.
pub type Word258 = Product<Word128, Word128>;

/// Convert 1 bit into a 1-bit word value.
pub fn from_u1(n: u8) -> Word1 {
    match n {
        0 => Sum::Left(Unit::Unit),
        1 => Sum::Right(Unit::Unit),
        _ => panic!("{} out of range for u1", n),
    }
}

/// Convert 2 bits into a 2-bit word value.
pub fn from_u2(n: u8) -> Word2 {
    if n > 3 {
        panic!("{} out of range for u2", n)
    }
    let n1 = (n & 2) / 2;
    let n2 = n & 1;
    Product::Product(from_u1(n1), from_u1(n2))
}

/// Convert 4 bits into a 4-bit word value.
pub fn from_u4(n: u8) -> Word4 {
    if n > 15 {
        panic!("{} out of range for u4", n)
    }
    let n1 = (n & 12) / 4;
    let n2 = n & 3;
    Product::Product(from_u2(n1), from_u2(n2))
}

/// Convert 8 bits into a 8-bit word value.
pub fn from_u8(n: u8) -> Word8 {
    let n1 = n >> 4;
    let n2 = n & 0xf;
    Product::Product(from_u4(n1), from_u4(n2))
}

/// Convert 16 bits into a 16-bit word value.
pub fn from_u16(n: u16) -> Word16 {
    let n1 = (n >> 8) as u8;
    let n2 = (n & 0xff) as u8;
    Product::Product(from_u8(n1), from_u8(n2))
}

/// Convert 32 bits into a 32-bit word value.
pub fn from_u32(n: u32) -> Word32 {
    let n1 = (n >> 16) as u16;
    let n2 = (n & 0xffff) as u16;
    Product::Product(from_u16(n1), from_u16(n2))
}

/// Convert 64 bits into a 64-bit word value.
pub fn from_u64(n: u64) -> Word64 {
    let n1 = (n >> 32) as u32;
    let n2 = (n & 0xffff_ffff) as u32;
    Product::Product(from_u32(n1), from_u32(n2))
}

/// Convert 128 bits into a 64-bit word value.
pub fn from_u128(n: u128) -> Word128 {
    let n1 = (n >> 64) as u64;
    // Cast picks last bytes
    let n2 = n as u64;
    Product::Product(from_u64(n1), from_u64(n2))
}

/// Convert a 1-bit word into 1 bit.
pub fn to_u1(n: Word1) -> u8 {
    match n {
        Word1::Left(_) => 0,
        Word1::Right(_) => 1,
    }
}

/// Convert a 2-bit word into 2 bits.
pub fn to_u2(n: Word2) -> u8 {
    match n {
        Word2::Product(n1, n2) => {
            let m1 = to_u1(n1);
            let m2 = to_u1(n2);
            (m1 << 1) + m2
        }
    }
}

/// Convert a 4-bit word into 4 bits.
pub fn to_u4(n: Word4) -> u8 {
    match n {
        Word4::Product(n1, n2) => {
            let m1 = to_u2(n1);
            let m2 = to_u2(n2);
            (m1 << 2) + m2
        }
    }
}

/// Convert a 8-bit word into 8 bits.
pub fn to_u8(n: Word8) -> u8 {
    match n {
        Word8::Product(n1, n2) => {
            let m1 = to_u4(n1);
            let m2 = to_u4(n2);
            (m1 << 4) + m2
        }
    }
}

/// Convert a 16-bit word into 16 bits.
pub fn to_u16(n: Word16) -> u16 {
    match n {
        Word16::Product(n1, n2) => {
            let m1 = to_u8(n1) as u16;
            let m2 = to_u8(n2) as u16;
            (m1 << 8) + m2
        }
    }
}

/// Convert a 32-bit word into 32 bits.
pub fn to_u32(n: Word32) -> u32 {
    match n {
        Word32::Product(n1, n2) => {
            let m1 = to_u16(n1) as u32;
            let m2 = to_u16(n2) as u32;
            (m1 << 16) + m2
        }
    }
}

/// Convert a 64-bit word into 64 bits.
pub fn to_u64(n: Word64) -> u64 {
    match n {
        Word64::Product(n1, n2) => {
            let m1 = to_u32(n1) as u64;
            let m2 = to_u32(n2) as u64;
            (m1 << 32) + m2
        }
    }
}

/// Convert a 128-bit word into 128 bits.
pub fn to_u128(n: Word128) -> u128 {
    match n {
        Word128::Product(n1, n2) => {
            let m1 = to_u64(n1) as u128;
            let m2 = to_u64(n2) as u128;
            (m1 << 64) + m2
        }
    }
}
