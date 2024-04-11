use std::fmt;
use thiserror::Error;

#[derive(Error)]
pub enum Error {
    #[error("Cannot join as a product because it is not a product type")]
    JoinProduct,
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
