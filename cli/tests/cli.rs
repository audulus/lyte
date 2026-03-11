/// Golden tests check expected stdout against actual output.
/// When built with `--features llvm`, `-t` runs all three backends
/// (JIT, LLVM, VM) and the expected output includes LLVM lines.
/// Without `--features llvm`, `-t` only runs JIT + VM, so the golden
/// tests are skipped (unit tests still run; the LLVM CI job covers
/// golden tests).
#[test]
#[cfg(feature = "llvm")]
fn run_golden_tests() -> goldentests::TestResult<()> {
    let config = goldentests::TestConfig::new("../target/debug/lyte", "../tests/cases", "// ");
    config.run_tests()
}
