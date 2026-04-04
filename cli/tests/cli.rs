/// Golden tests: run the suite once per backend.
/// The --backend flag selects the backend (default: jit).
/// Each backend produces identical output, so the same expected stdout works for all.
///
/// Tests can opt out of specific backends with `// skip-backend: vm` (or `llvm`, `jit`).
///
/// Backend is passed via CLI args (base_args) rather than env vars, so the three
/// test functions can safely run in parallel without environment variable races.

fn run_golden_tests(backend: &str) -> goldentests::TestResult<()> {
    // On macOS ARM64, the binary needs ad-hoc code signing when the __TEXT segment
    // is large (e.g., the 2MB VM16 ARM64 handler table). Sign before running tests.
    #[cfg(target_os = "macos")]
    {
        let _ = std::process::Command::new("codesign")
            .args(["-s", "-", "-f", "../target/debug/lyte"])
            .output();
    }
    let mut config = goldentests::TestConfig::new("../target/debug/lyte", "../tests/cases", "// ");
    config.base_args = format!("--backend {} --skip-backend {}", backend, backend);
    config.run_tests()
}

#[test]
#[cfg(feature = "cranelift")]
fn golden_tests_jit() -> goldentests::TestResult<()> {
    run_golden_tests("jit")
}

#[test]
#[cfg(feature = "llvm")]
fn golden_tests_llvm() -> goldentests::TestResult<()> {
    run_golden_tests("llvm")
}

#[test]
fn golden_tests_vm() -> goldentests::TestResult<()> {
    run_golden_tests("vm")
}

#[test]
#[cfg(target_arch = "aarch64")]
fn golden_tests_asm() -> goldentests::TestResult<()> {
    run_golden_tests("asm")
}

#[test]
fn golden_tests_vm16() -> goldentests::TestResult<()> {
    run_golden_tests("vm16")
}
