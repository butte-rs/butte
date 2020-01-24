use std::fmt;

/// An error while accessing a flatbuffer
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Error {
    /// Returned if accessing the flatbuffer triggers an out-of-bounds access
    OutOfBounds,
    /// Returned if a string is not UTF8
    NonUtf8String,
    /// Returned if a required field is missing
    RequiredFieldMissing(&'static str),
    /// Returned if a string is not null-terminated,
    NonNullTerminatedString,
    // Returned if a buffer refers to an unknown enum variant
    UnknownEnumVariant,
    // Returned if a buffer refers to an unknown union variant
    UnknownUnionVariant,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::OutOfBounds => write!(f, "flatbuffer access is out of bounds"),
            Error::RequiredFieldMissing(m) => write!(f, "required field is missing: {}", m),
            Error::NonUtf8String => write!(f, "string is not UTF8 encoded"),
            Error::NonNullTerminatedString => write!(f, "string is not terminated with null"),
            Error::UnknownEnumVariant => write!(f, "unknown enum variant"),
            Error::UnknownUnionVariant => write!(f, "unknown union variant"),
        }
    }
}

impl std::error::Error for Error {}
