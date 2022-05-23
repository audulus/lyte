use assert_cmd::prelude::*; // Add methods on commands
use predicates::prelude::*; // Used for writing assertions
use std::process::Command; // Run programs

#[test]
fn file_doesnt_exist() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("lyte")?;

    cmd.arg("test/file/doesnt/exist");
    cmd.assert()
         .failure()
         .stderr(predicate::str::contains("could not read file"));

    Ok(())
}