#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: String| {
    let mut compiler = lyte::Compiler::new();
    if !compiler.parse(&data, "fuzz") {
        return;
    }
    if !compiler.check() {
        return;
    }
    if !compiler.has_decls() {
        return;
    }
    if compiler.specialize().is_err() {
        return;
    }
    // Compile to VM bytecode only — don't execute. Execution can trigger
    // assert() failures from fuzzed programs, which abort under libfuzzer.
    let _ = compiler.compile_vm();
});
