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
    // Compile and run via Cranelift JIT.
    // catch_unwind guards against panics in codegen.
    let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        compiler.run();
    }));
});
