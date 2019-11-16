pub mod codegen;
mod compile;

mod macros;

#[cfg(test)]
mod parser_macros;

pub mod parser;
pub mod types;

pub use crate::compile::{compile_fbs, compile_fbs_generic};
