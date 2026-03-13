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
    // Compile only — don't execute. Execution can trigger assert() failures
    // from fuzzed programs, which abort under libfuzzer's panic hook.
    // We still exercise the full compile pipeline (parse, check, specialize, JIT codegen).
    let _ = compiler.jit();
});
