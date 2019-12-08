use std::fmt;

/// An error while accessing a flatbuffer
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// Returned if accessing the flatbuffer triggers an out-of-bounds access
    OutOfBounds,
    /// Returned if a string is not null-terminated,
    NonNullTerminatedString,
    // Returned if a buffer refers to an unknown union variant
    UnknownUnionVariant,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::OutOfBounds => write!(f, "memory access is out of bounds"),
            Error::NonNullTerminatedString => write!(f, "string is not terminated with null"),
            Error::UnknownUnionVariant => write!(f, "unknown union variant"),
        }
    }
}

impl std::error::Error for Error {}
