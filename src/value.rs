use crate::display::DisplayDepth;
use crate::error::Error;

pub trait Value: DisplayDepth + Clone {
    type A: Value;
    type B: Value;

    fn split_left(&self) -> Result<&Self::A, Error>;

    fn split_right(&self) -> Result<&Self::B, Error>;

    fn split_product(&self) -> Result<(&Self::A, &Self::B), Error>;

    fn join_product(a: Self::A, b: Self::B) -> Result<Self, Error>;
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

    fn split_left(&self) -> Result<&Self::A, Error> {
        Err(Error::SplitLeft)
    }

    fn split_right(&self) -> Result<&Self::B, Error> {
        Err(Error::SplitRight)
    }

    fn split_product(&self) -> Result<(&Self::A, &Self::B), Error> {
        Err(Error::SplitProduct)
    }

    fn join_product(_a: Self::A, _b: Self::B) -> Result<Self, Error> {
        Err(Error::JoinProduct)
    }
}

impl<A: Value, B: Value> Value for Sum<A, B> {
    type A = A;
    type B = B;

    fn split_left(&self) -> Result<&Self::A, Error> {
        match self {
            Sum::Left(a) => Ok(a),
            Sum::Right(_b) => Err(Error::SplitLeft),
        }
    }

    fn split_right(&self) -> Result<&Self::B, Error> {
        match self {
            Sum::Left(_a) => Err(Error::SplitRight),
            Sum::Right(b) => Ok(b),
        }
    }

    fn split_product(&self) -> Result<(&Self::A, &Self::B), Error> {
        Err(Error::SplitProduct)
    }

    fn join_product(_a: Self::A, _b: Self::B) -> Result<Self, Error> {
        Err(Error::JoinProduct)
    }
}

impl<A: Value, B: Value> Value for Product<A, B> {
    type A = A;
    type B = B;

    fn split_left(&self) -> Result<&Self::A, Error> {
        Err(Error::SplitLeft)
    }

    fn split_right(&self) -> Result<&Self::B, Error> {
        Err(Error::SplitRight)
    }

    fn split_product(&self) -> Result<(&Self::A, &Self::B), Error> {
        match self {
            Product::Product(a, b) => Ok((a, b)),
        }
    }

    fn join_product(a: Self::A, b: Self::B) -> Result<Self, Error> {
        Ok(Product::Product(a, b))
    }
}

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

pub type Word1 = Bit;
pub type Word2 = Product<Word1, Word1>;
pub type Word4 = Product<Word2, Word2>;
pub type Word8 = Product<Word4, Word4>;
pub type Word16 = Product<Word8, Word8>;
pub type Word32 = Product<Word16, Word16>;
pub type Word64 = Product<Word32, Word32>;
pub type Word128 = Product<Word64, Word64>;
pub type Word258 = Product<Word128, Word128>;

pub fn from_u1(n: u8) -> Word1 {
    match n {
        0 => Sum::Left(Unit::Unit),
        1 => Sum::Right(Unit::Unit),
        _ => panic!("{} out of range for u1", n),
    }
}

pub fn from_u2(n: u8) -> Word2 {
    if n > 3 {
        panic!("{} out of range for u2", n)
    }
    let n1 = (n & 2) / 2;
    let n2 = n & 1;
    Product::Product(from_u1(n1), from_u1(n2))
}

pub fn from_u4(n: u8) -> Word4 {
    if n > 15 {
        panic!("{} out of range for u4", n)
    }
    let n1 = (n & 12) / 4;
    let n2 = n & 3;
    Product::Product(from_u2(n1), from_u2(n2))
}

pub fn from_u8(n: u8) -> Word8 {
    let n1 = n >> 4;
    let n2 = n & 0xf;
    Product::Product(from_u4(n1), from_u4(n2))
}

pub fn from_u16(n: u16) -> Word16 {
    let n1 = (n >> 8) as u8;
    let n2 = (n & 0xff) as u8;
    Product::Product(from_u8(n1), from_u8(n2))
}

pub fn from_u32(n: u32) -> Word32 {
    let n1 = (n >> 16) as u16;
    let n2 = (n & 0xffff) as u16;
    Product::Product(from_u16(n1), from_u16(n2))
}

pub fn from_u64(n: u64) -> Word64 {
    let n1 = (n >> 32) as u32;
    let n2 = (n & 0xffff_ffff) as u32;
    Product::Product(from_u32(n1), from_u32(n2))
}

pub fn from_u128(n: u128) -> Word128 {
    let n1 = (n >> 64) as u64;
    // Cast picks last bytes
    let n2 = n as u64;
    Product::Product(from_u64(n1), from_u64(n2))
}

pub fn to_u1(n: Word1) -> u8 {
    match n {
        Word1::Left(_) => 0,
        Word1::Right(_) => 1,
    }
}

pub fn to_u2(n: Word2) -> u8 {
    match n {
        Word2::Product(n1, n2) => {
            let m1 = to_u1(n1);
            let m2 = to_u1(n2);
            (m1 << 1) + m2
        }
    }
}

pub fn to_u4(n: Word4) -> u8 {
    match n {
        Word4::Product(n1, n2) => {
            let m1 = to_u2(n1);
            let m2 = to_u2(n2);
            (m1 << 2) + m2
        }
    }
}

pub fn to_u8(n: Word8) -> u8 {
    match n {
        Word8::Product(n1, n2) => {
            let m1 = to_u4(n1);
            let m2 = to_u4(n2);
            (m1 << 4) + m2
        }
    }
}

pub fn to_u16(n: Word16) -> u16 {
    match n {
        Word16::Product(n1, n2) => {
            let m1 = to_u8(n1) as u16;
            let m2 = to_u8(n2) as u16;
            (m1 << 8) + m2
        }
    }
}

pub fn to_u32(n: Word32) -> u32 {
    match n {
        Word32::Product(n1, n2) => {
            let m1 = to_u16(n1) as u32;
            let m2 = to_u16(n2) as u32;
            (m1 << 16) + m2
        }
    }
}

pub fn to_u64(n: Word64) -> u64 {
    match n {
        Word64::Product(n1, n2) => {
            let m1 = to_u32(n1) as u64;
            let m2 = to_u32(n2) as u64;
            (m1 << 32) + m2
        }
    }
}

pub fn to_u128(n: Word128) -> u128 {
    match n {
        Word128::Product(n1, n2) => {
            let m1 = to_u64(n1) as u128;
            let m2 = to_u64(n2) as u128;
            (m1 << 64) + m2
        }
    }
}
