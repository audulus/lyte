#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: String| {
    let mut compiler = lyte::Compiler::new();
    if compiler.parse(&data, "fuzz") {
        let _ = compiler.check();
    }
});
