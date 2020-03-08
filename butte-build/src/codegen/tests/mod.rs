pub use super::*;
pub use pretty_assertions::assert_eq;
use proc_macro2::TokenStream;
use quote::quote;
use std::{
    io::Write,
    process::{Command, Stdio},
};

// Utility functions for tests

macro_rules! parse_expected_file {
    ($file:expr) => { {
        let src = include_str!($file);
        let syntax = syn::parse_file(src).expect("Unable to parse file");
        let formatted = rustfmt(&quote!(#syntax));

        // Split the String per lines to make debugging/diffing
        // viable (it also improves diff performance at test time)
        let v : Vec<_> = formatted.lines().map(ToString::to_string).collect();
        v
    } }
}

pub fn fb_to_rust(input: &str) -> Vec<String> {
    let (_, schema) = crate::parser::schema_decl(input).unwrap();
    let root = crate::ir::Builder::build(schema).expect("Cannot build IR");
    let mut generator = CodeGenerator {
        root,
        rpc_gen: None,
    };

    let formatted = rustfmt(&generator.build_token_stream());

    // Split the String per lines to make debugging/diffing
    // viable (it also improves diff performance at test time)
    formatted.lines().map(ToString::to_string).collect()
}

// Used to print code as part of debugging process
// and to normalize token streams -- as they cannot be
// compared otherwise
pub fn rustfmt(code: &TokenStream) -> String {
    let mut cmd = Command::new("rustfmt")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .arg("--edition")
        .arg("2018")
        .arg("--config")
        .arg("normalize_doc_attributes=true")
        .spawn()
        .unwrap();
    cmd.stdin
        .as_mut()
        .unwrap()
        .write_all(code.to_string().as_bytes())
        .unwrap();
    let v = cmd.wait_with_output().unwrap().stdout;

    String::from_utf8(v).unwrap()
}

// Tests start here

#[test]
fn test_array_in_table() {
    let input = "\
table Entry {}

table MyTable {                                                                                                                                                                                             
    entries: [Entry];                                                                                                                                                                                       
}";

    // TODO Issue #39 exhibits the generated code is incorrect
    let expected = parse_expected_file!("expected/test_table_simple.rs");
    let actual = fb_to_rust(input);
    assert_eq!(actual, expected);
}
