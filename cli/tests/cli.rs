use goldentests::{TestConfig, TestResult};

#[test]
fn run_golden_tests() -> TestResult<()> {
    let config = TestConfig::new("../target/debug/lyte", "../tests/cases", "// ");
    config.run_tests()
}

/// When built with `--features llvm`, run every test that uses `-c` or `-t`
/// through the LLVM backend (`-l`) and verify the program output matches
/// what the Cranelift JIT produces.
#[test]
#[cfg(feature = "llvm")]
fn run_llvm_golden_tests() {
    use std::process::Command;

    let binary = "../target/debug/lyte";
    let cases_dir = "../tests/cases";

    let mut failures: Vec<String> = Vec::new();
    let mut passed = 0u32;

    for entry in walkdir(cases_dir) {
        let contents = match std::fs::read_to_string(&entry) {
            Ok(c) => c,
            Err(_) => continue,
        };

        // Only test files that run JIT (args contain -c or -t).
        let args_line = contents.lines().find(|l| l.starts_with("// args:"));
        let args_line = match args_line {
            Some(l) => l.trim_start_matches("// args:").trim(),
            None => continue,
        };
        if !(args_line.contains("-c") || args_line.contains("-t")) {
            continue;
        }
        // Skip tests that expect errors (no main, etc.) — they don't reach codegen.
        if contents.contains("expected stderr:") {
            continue;
        }

        // Run with Cranelift JIT to get reference output.
        let jit_out = Command::new(binary)
            .arg(&entry)
            .arg("-c")
            .output()
            .expect("failed to run binary");
        // If JIT itself fails (e.g. error tests), skip — those don't test codegen.
        if !jit_out.status.success() {
            continue;
        }

        // Extract program-visible lines from JIT output (drop framework messages).
        let jit_stdout = String::from_utf8_lossy(&jit_out.stdout);
        let jit_lines: Vec<&str> = jit_stdout
            .lines()
            .filter(|l| *l != "compilation successful")
            .collect();

        // Run with LLVM backend.
        let llvm_out = Command::new(binary)
            .arg(&entry)
            .arg("-l")
            .output()
            .expect("failed to run binary");

        if !llvm_out.status.success() {
            let stderr = String::from_utf8_lossy(&llvm_out.stderr);
            failures.push(format!(
                "FAIL (exit {}): {}\n  stderr: {}",
                llvm_out.status.code().unwrap_or(-1),
                entry,
                stderr.lines().next().unwrap_or("")
            ));
            continue;
        }

        let llvm_stdout = String::from_utf8_lossy(&llvm_out.stdout);
        let llvm_lines: Vec<&str> = llvm_stdout
            .lines()
            .filter(|l| *l != "compilation successful")
            .collect();

        if jit_lines != llvm_lines {
            failures.push(format!(
                "MISMATCH: {}\n  JIT:  {:?}\n  LLVM: {:?}",
                entry, jit_lines, llvm_lines
            ));
            continue;
        }

        passed += 1;
    }

    println!("\nLLVM golden tests: {} passed, {} failed", passed, failures.len());
    if !failures.is_empty() {
        for f in &failures {
            println!("{}", f);
        }
        panic!("{} LLVM golden test(s) failed", failures.len());
    }
}

#[cfg(feature = "llvm")]
fn walkdir(dir: &str) -> Vec<String> {
    let mut result = Vec::new();
    walkdir_rec(dir, &mut result);
    result
}

#[cfg(feature = "llvm")]
fn walkdir_rec(dir: &str, result: &mut Vec<String>) {
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walkdir_rec(path.to_str().unwrap(), result);
            } else if path.extension().map(|e| e == "lyte").unwrap_or(false) {
                result.push(path.to_string_lossy().to_string());
            }
        }
    }
}
