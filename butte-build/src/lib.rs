pub mod codegen;
mod compile;

mod macros;

pub mod ir;
pub mod parse;
pub use crate::compile::{compile_fbs, compile_fbs_generic};
pub(crate) use crate::parse::parser;
