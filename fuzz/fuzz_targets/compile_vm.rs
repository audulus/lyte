#![no_main]
use libfuzzer_sys::fuzz_target;
use lyte::vm::VM;

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
    // Compile to VM bytecode and execute.
    if let Ok(program) = compiler.compile_vm() {
        let mut vm = VM::new();
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            vm.run(&program);
        }));
    }
});
