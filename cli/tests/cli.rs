use assert_cmd::prelude::*; // Add methods on commands
use predicates::prelude::*; // Used for writing assertions
use std::process::Command; // Run programs
use std::fs;
use std::path::Path;

#[test]
fn file_doesnt_exist() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("lyte")?;

    cmd.arg("test/file/doesnt/exist");
    cmd.assert()
         .failure()
         .stderr(predicate::str::contains("could not read file"));

    Ok(())
}

#[test]
fn cases() -> Result<(), Box<dyn std::error::Error>> {

    if let Ok(entries) = fs::read_dir("../tests/cases/") {
        for entry in entries {
            let path = entry.unwrap().path();

            let mut cmd = Command::cargo_bin("lyte")?;
            cmd.arg(path.into_os_string().into_string().unwrap());
            // cmd.assert().success();
            println!("{:?}", cmd.output());

        }
    }

    Ok(())
}