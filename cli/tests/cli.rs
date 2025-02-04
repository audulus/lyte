use goldentests::{TestConfig, TestResult};

#[test]
fn run_golden_tests() -> TestResult<()> {
    let config = TestConfig::new("../target/debug/lyte", "../tests/cases", "// ");
    config.run_tests()
}
