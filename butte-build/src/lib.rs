pub mod codegen;
mod compile;

mod macros;

pub mod ast;
pub mod ir;

pub(crate) use crate::ast::parser;
pub use crate::compile::{compile_fbs, compile_fbs_generic};
