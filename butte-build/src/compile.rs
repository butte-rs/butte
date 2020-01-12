/// Compile flatbuffers files
use std::io::{self, Write};
use std::{
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use anyhow::{anyhow, Result};
use quote::ToTokens;

/// Generate Rust code for a single flatbuffer schema file from arbitrary input and to arbitrary
/// output.
pub fn compile_fbs_generic(
    ugly: bool,
    mut input: Box<dyn io::Read>,
    mut output: Box<dyn io::Write>,
) -> Result<()> {
    let mut schema_text = String::new();
    input.read_to_string(&mut schema_text)?;

    // parse the schema
    let (_, schema) =
        crate::parser::schema_decl(schema_text.as_str()).map_err(|_| anyhow!("parse failed"))?;
    let ir_root =
        crate::ir::IrBuilder::build(schema).map_err(|_| anyhow!("semantic analysis failed"))?;

    let code = format!("{}", ir_root.to_token_stream());

    let text_output = if !ugly {
        let mut cmd = Command::new("rustfmt")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .arg("--edition")
            .arg("2018")
            .arg("--config")
            .arg("normalize_doc_attributes=true")
            .spawn()?;
        cmd.stdin
            .as_mut()
            .ok_or_else(|| anyhow!("cannot access stdin"))?
            .write_all(code.as_bytes())?;
        cmd.wait_with_output()?.stdout
    } else {
        Vec::from(code)
    };
    output.write_all(&text_output[..])?;
    Ok(())
}

/// Generate Rust code for a single flatbuffer schema file.
pub fn compile_fbs(path: impl AsRef<Path>) -> Result<()> {
    let out_dir = PathBuf::from(std::env::var("OUT_DIR")?);
    let path_ref = path.as_ref();
    let output_path = out_dir.join(
        path_ref
            .with_extension("rs")
            .file_name()
            .ok_or_else(|| anyhow!("path has no file_name: {:?}", path_ref))?,
    );
    let ugly = false;
    compile_fbs_generic(
        ugly,
        Box::new(std::fs::File::open(path_ref)?),
        Box::new(std::fs::File::create(output_path)?),
    )?;
    Ok(())
}
