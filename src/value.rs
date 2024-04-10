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
pub enum Sum<A, B> {
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
pub enum Product<A, B> {
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

impl From<bool> for Bit {
    fn from(bit: bool) -> Self {
        match bit {
            // False bit becomes left value that wraps unit value
            false => Sum::Left(Unit::Unit),
            // True bit becomes right value that wraps unit value
            true => Sum::Right(Unit::Unit),
        }
    }
}

impl From<Bit> for bool {
    fn from(bit: Bit) -> Self {
        match bit {
            Bit::Left(_) => false,
            Bit::Right(_) => true,
        }
    }
}

/// Byte type.
///
/// Bytes are nested products of eighth bits.
pub type Byte = Product<
    Product<Product<Bit, Bit>, Product<Bit, Bit>>,
    Product<Product<Bit, Bit>, Product<Bit, Bit>>,
>;

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

impl Word1 {
    /// Convert an integer into a 1-bit word.
    ///
    /// ## Panics
    ///
    /// 1 < n
    pub fn from_unwrap(n: u8) -> Word1 {
        match n {
            0 => Sum::Left(Unit::Unit),
            1 => Sum::Right(Unit::Unit),
            _ => panic!("{n} has more than 1 bits"),
        }
    }
}

impl Word2 {
    /// Convert an integer into a 2-bit word.
    ///
    /// ## Panics
    ///
    /// 3 < n
    pub fn from_unwrap(n: u8) -> Word2 {
        if 3 < n {
            panic!("{n} has more than 2 bits")
        }
        let n1 = (n & 2) / 2;
        let n2 = n & 1;
        Product::Product(Word1::from_unwrap(n1), Word1::from_unwrap(n2))
    }
}

impl Word4 {
    /// Convert an integer into a 4-bit word.
    pub fn from_unwrap(n: u8) -> Word4 {
        if 15 < n {
            panic!("{n} has more than 4 bits")
        }
        let n1 = (n & 12) / 4;
        let n2 = n & 3;
        Product::Product(Word2::from_unwrap(n1), Word2::from_unwrap(n2))
    }
}

impl From<u8> for Word8 {
    fn from(n: u8) -> Self {
        let n1 = n >> 4;
        let n2 = n & 0xf;
        Product::Product(Word4::from_unwrap(n1), Word4::from_unwrap(n2))
    }
}

impl From<u16> for Word16 {
    fn from(n: u16) -> Word16 {
        let n1 = (n >> 8) as u8;
        let n2 = (n & 0xff) as u8;
        Product::Product(Word8::from(n1), Word8::from(n2))
    }
}

impl From<u32> for Word32 {
    fn from(n: u32) -> Word32 {
        let n1 = (n >> 16) as u16;
        let n2 = (n & 0xffff) as u16;
        Product::Product(Word16::from(n1), Word16::from(n2))
    }
}

impl From<u64> for Word64 {
    fn from(n: u64) -> Self {
        let n1 = (n >> 32) as u32;
        let n2 = (n & 0xffff_ffff) as u32;
        Product::Product(Word32::from(n1), Word32::from(n2))
    }
}

impl From<u128> for Word128 {
    fn from(n: u128) -> Self {
        let n1 = (n >> 64) as u64;
        // Cast picks last bytes
        let n2 = n as u64;
        Product::Product(Word64::from(n1), Word64::from(n2))
    }
}

impl From<&Word1> for u8 {
    fn from(n: &Word1) -> Self {
        match n {
            Word1::Left(_) => 0,
            Word1::Right(_) => 1,
        }
    }
}

impl From<&Word2> for u8 {
    fn from(n: &Word2) -> Self {
        match n {
            Word2::Product(n1, n2) => {
                let m1 = u8::from(n1);
                let m2 = u8::from(n2);
                (m1 << 1) + m2
            }
        }
    }
}

impl From<&Word4> for u8 {
    fn from(n: &Word4) -> Self {
        match n {
            Word4::Product(n1, n2) => {
                let m1 = u8::from(n1);
                let m2 = u8::from(n2);
                (m1 << 2) + m2
            }
        }
    }
}

impl From<&Word8> for u8 {
    fn from(n: &Word8) -> Self {
        match n {
            Word8::Product(n1, n2) => {
                let m1 = u8::from(n1);
                let m2 = u8::from(n2);
                (m1 << 4) + m2
            }
        }
    }
}

impl From<&Word16> for u16 {
    fn from(n: &Word16) -> Self {
        match n {
            Word16::Product(n1, n2) => {
                let m1 = u8::from(n1) as u16;
                let m2 = u8::from(n2) as u16;
                (m1 << 8) + m2
            }
        }
    }
}

impl From<&Word32> for u32 {
    fn from(n: &Word32) -> Self {
        match n {
            Word32::Product(n1, n2) => {
                let m1 = u16::from(n1) as u32;
                let m2 = u16::from(n2) as u32;
                (m1 << 16) + m2
            }
        }
    }
}

impl From<&Word64> for u64 {
    fn from(n: &Word64) -> Self {
        match n {
            Word64::Product(n1, n2) => {
                let m1 = u32::from(n1) as u64;
                let m2 = u32::from(n2) as u64;
                (m1 << 32) + m2
            }
        }
    }
}

impl From<&Word128> for u128 {
    fn from(n: &Word128) -> Self {
        match n {
            Word128::Product(n1, n2) => {
                let m1 = u64::from(n1) as u128;
                let m2 = u64::from(n2) as u128;
                (m1 << 64) + m2
            }
        }
    }
}
