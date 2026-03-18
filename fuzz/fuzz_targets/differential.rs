#![no_main]
use libfuzzer_sys::fuzz_target;
use std::io::Write;
use std::os::unix::io::AsRawFd;

/// Byte-driven generator for valid Lyte programs.
struct Gen<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> Gen<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }

    fn next(&mut self) -> u8 {
        if self.pos >= self.data.len() {
            0
        } else {
            let b = self.data[self.pos];
            self.pos += 1;
            b
        }
    }

    fn gen_program(&mut self) -> String {
        let mut lines = vec!["main {".to_string()];
        let mut vars: Vec<String> = Vec::new();

        // 1-4 initial variables
        let n_vars = (self.next() % 4 + 1) as usize;
        for i in 0..n_vars {
            let name = format!("v{}", i);
            let val = (self.next() % 200) as i32 - 100; // -100..99
            lines.push(format!("    var {} = {}", name, self.format_int(val)));
            vars.push(name);
        }

        // 1-5 computed values, each printed
        let n_stmts = (self.next() % 5 + 1) as usize;
        for i in 0..n_stmts {
            let name = format!("r{}", i);
            let expr = self.gen_expr(&vars, 3);
            lines.push(format!("    let {} = {}", name, expr));
            lines.push(format!("    print({})", name));
            vars.push(name);
        }

        lines.push("}".to_string());
        lines.join("\n")
    }

    fn format_int(&self, val: i32) -> String {
        if val < 0 {
            // Use parenthesized negation to avoid ambiguity
            format!("(0 - {})", -val)
        } else {
            format!("{}", val)
        }
    }

    fn gen_expr(&mut self, vars: &[String], depth: u8) -> String {
        if depth == 0 || vars.is_empty() {
            return self.gen_literal();
        }

        match self.next() % 10 {
            // Literal (20%)
            0..=1 => self.gen_literal(),

            // Variable reference (30%)
            2..=4 => {
                let idx = self.next() as usize % vars.len();
                vars[idx].clone()
            }

            // Binary arithmetic (30%) — no division to avoid safety errors
            5..=7 => {
                let ops = ["+", "-", "*"];
                let op = ops[self.next() as usize % ops.len()];
                let l = self.gen_expr(vars, depth - 1);
                let r = self.gen_expr(vars, depth - 1);
                format!("({} {} {})", l, op, r)
            }

            // Conditional (10%)
            8 => {
                let cond_idx = self.next() as usize % vars.len();
                let t = self.gen_expr(vars, depth - 1);
                let e = self.gen_expr(vars, depth - 1);
                format!("if {} > 0 {{ {} }} else {{ {} }}", vars[cond_idx], t, e)
            }

            // For-loop accumulator (10%)
            9 => {
                let bound = (self.next() % 8 + 1) as i32;
                let step_expr = self.gen_expr(vars, depth - 1);
                format!(
                    "{{\n        var acc = 0\n        \
                     for i in 0 .. {} {{\n            \
                     acc = acc + {}\n        \
                     }}\n        acc\n    }}",
                    bound, step_expr
                )
            }

            _ => unreachable!(),
        }
    }

    fn gen_literal(&mut self) -> String {
        let val = (self.next() % 200) as i32 - 100;
        self.format_int(val)
    }
}

/// Capture everything written to stdout (fd 1) during `f()`.
///
/// Redirects fd 1 to a temp file, runs `f()`, restores fd 1, reads the file.
/// The only unsafe is the dup/dup2 for fd redirection (no safe Rust API for that).
fn capture_stdout<F: FnOnce()>(f: F) -> String {
    std::io::stdout().flush().ok();

    let tmp = std::env::temp_dir().join("lyte_diff_fuzz_capture.txt");
    let file = match std::fs::File::create(&tmp) {
        Ok(f) => f,
        Err(_) => { f(); return String::new(); }
    };

    // Save stdout fd, redirect to temp file.
    // as_raw_fd() borrows — `file` keeps ownership and will close the fd on drop.
    let saved_fd = unsafe { libc::dup(1) };
    if saved_fd < 0 {
        drop(file);
        f();
        return String::new();
    }
    unsafe { libc::dup2(file.as_raw_fd(), 1) };
    // Drop the File now — fd 1 is a dup'd copy, so closing the original is fine.
    drop(file);

    f();

    std::io::stdout().flush().ok();

    // Restore original stdout.
    unsafe { libc::dup2(saved_fd, 1) };
    unsafe { libc::close(saved_fd) };

    // Read the captured output with normal file I/O.
    std::fs::read_to_string(&tmp).unwrap_or_default()
}

/// Run a program on a given backend, return stdout or None if compilation fails.
fn run_backend(program: &str, backend: &str) -> Option<String> {
    let mut compiler = lyte::Compiler::new();
    if !compiler.parse(program, "fuzz") {
        return None;
    }
    if !compiler.check() {
        return None;
    }
    if compiler.specialize().is_err() {
        return None;
    }

    match backend {
        "jit" => {
            let output = capture_stdout(|| {
                compiler.run();
            });
            // Strip the "compilation successful" prefix line
            let output = strip_prefix_line(&output, "compilation successful");
            Some(output)
        }
        "vm" => {
            let output = capture_stdout(|| {
                let _ = compiler.run_vm();
            });
            Some(output)
        }
        #[cfg(target_arch = "aarch64")]
        "asm" => {
            let output = capture_stdout(|| {
                if let Ok(program) = compiler.compile_vm() {
                    let mut vm = lyte::vm::VM::new();
                    vm.run_asm(&program);
                }
            });
            Some(output)
        }
        #[cfg(feature = "llvm")]
        "llvm" => {
            let output = capture_stdout(|| {
                compiler.run_llvm();
            });
            // Strip the "compilation successful" prefix line
            let output = strip_prefix_line(&output, "compilation successful");
            Some(output)
        }
        _ => None,
    }
}

fn strip_prefix_line(s: &str, prefix: &str) -> String {
    if let Some(rest) = s.strip_prefix(prefix) {
        rest.strip_prefix('\n').unwrap_or(rest).to_string()
    } else {
        s.to_string()
    }
}

fn assert_same(
    name_a: &str,
    out_a: &Option<String>,
    name_b: &str,
    out_b: &Option<String>,
    program: &str,
) {
    match (out_a, out_b) {
        (Some(a), Some(b)) => {
            if a != b {
                panic!(
                    "DIFFERENTIAL BUG: {} and {} produce different output!\n\
                     \n=== Program ===\n{}\n\
                     \n=== {} output ===\n{}\n\
                     \n=== {} output ===\n{}",
                    name_a, name_b, program, name_a, a, name_b, b
                );
            }
        }
        (Some(_), None) | (None, Some(_)) => {
            panic!(
                "DIFFERENTIAL BUG: one backend compiled but the other didn't!\n\
                 \n=== Program ===\n{}\n\
                 \n{}: {}\n{}: {}",
                program,
                name_a,
                out_a.as_deref().unwrap_or("FAILED"),
                name_b,
                out_b.as_deref().unwrap_or("FAILED"),
            );
        }
        (None, None) => {}
    }
}

fuzz_target!(|data: &[u8]| {
    if data.len() < 8 {
        return;
    }

    let mut gen = Gen::new(data);
    let program = gen.gen_program();

    // Quick parse+check to filter out invalid programs in-process (fast).
    {
        let mut compiler = lyte::Compiler::new();
        if !compiler.parse(&program, "fuzz") {
            return;
        }
        if !compiler.check() {
            return;
        }
    }

    // Run on all backends.
    let jit_output = run_backend(&program, "jit");
    let vm_output = run_backend(&program, "vm");

    assert_same("JIT", &jit_output, "VM", &vm_output, &program);

    #[cfg(target_arch = "aarch64")]
    {
        let asm_output = run_backend(&program, "asm");
        assert_same("VM", &vm_output, "ASM", &asm_output, &program);
    }

    #[cfg(feature = "llvm")]
    {
        let llvm_output = run_backend(&program, "llvm");
        assert_same("VM", &vm_output, "LLVM", &llvm_output, &program);
    }
});
