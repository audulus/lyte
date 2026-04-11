/// Golden tests: run the suite once per backend.
/// The --backend flag selects the backend (default: jit).
/// Each backend produces identical output, so the same expected stdout works for all.
///
/// Tests can opt out of specific backends with `// skip-backend: vm` (or `llvm`, `jit`).
///
/// Backend is passed via CLI args (base_args) rather than env vars, so the three
/// test functions can safely run in parallel without environment variable races.

/// Path to the just-built `lyte` binary. `CARGO_BIN_EXE_lyte` is set by
/// Cargo for integration tests and resolves correctly regardless of any
/// `target-dir` override in `.cargo/config.toml`.
const LYTE_BIN: &str = env!("CARGO_BIN_EXE_lyte");

fn run_golden_tests(backend: &str) -> goldentests::TestResult<()> {
    let mut config = goldentests::TestConfig::new(LYTE_BIN, "../tests/cases", "// ");
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
#[cfg(target_arch = "aarch64")]
fn golden_tests_stack() -> goldentests::TestResult<()> {
    run_golden_tests("stack")
}
