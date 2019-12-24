pub mod codegen;
mod compile;

mod macros;

#[cfg(test)]
mod parser_macros;

pub mod parse;

pub use crate::compile::{compile_fbs, compile_fbs_generic};
pub(crate) use crate::parse::parser;
