use anyhow::Result;
use assert_cmd::prelude::*;
use std::process::Command;

#[test]
fn test_greeter() -> Result<()> {
    let mut cmd = Command::cargo_bin("greeter")?;
    cmd.assert().success();
    Ok(())
}
