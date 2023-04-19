use std::fmt;
use thiserror::Error;

#[derive(Error)]
pub enum Error {
    #[error("Cannot split left because it is not a left value")]
    SplitLeft,
    #[error("Cannot split right because it is not a right value")]
    SplitRight,
    #[error("Cannot split the product because it is not a product value")]
    SplitProduct,
    #[error("Cannot join as a product because it is not a product type")]
    JoinProduct,
    #[error("Case combinator expects a left or right value as input")]
    CaseSum,
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
