/// Golden tests: run the suite once per backend.
/// The LYTE_BACKEND env var selects which backend `-c` uses.
/// Each backend produces identical output, so the same expected stdout works for all.
///
/// Tests can opt out of specific backends with `// skip-backend: vm` (or `llvm`, `jit`).

fn run_golden_tests(backend: &str) -> goldentests::TestResult<()> {
    std::env::set_var("LYTE_BACKEND", backend);
    std::env::set_var("LYTE_SKIP_BACKEND", backend);
    let config = goldentests::TestConfig::new("../target/debug/lyte", "../tests/cases", "// ");
    let result = config.run_tests();
    std::env::remove_var("LYTE_BACKEND");
    std::env::remove_var("LYTE_SKIP_BACKEND");
    result
}

#[test]
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
