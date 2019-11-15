use anyhow::Result;
use assert_cmd::prelude::*; // Add methods on commands
use std::process::Command; // Run programs // Used for writing assertions

#[test]
fn test_greeter() -> Result<()> {
    let mut cmd = Command::cargo_bin("greeter")?;
    cmd.assert().success();
    Ok(())
}
