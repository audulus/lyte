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
        let mut decls = Vec::new();
        let mut main_lines = vec!["main {".to_string()];
        let mut vars: Vec<(String, VarType)> = Vec::new();

        // Optionally emit a struct definition
        let has_struct = self.next() % 3 == 0;
        if has_struct {
            let n_fields = (self.next() % 3 + 2) as usize; // 2-4 fields
            let mut fields = Vec::new();
            for j in 0..n_fields {
                fields.push(format!("    f{}: i32", j));
            }
            decls.push(format!("struct S {{\n{}\n}}", fields.join(",\n")));
        }

        // Optionally emit an enum definition
        let has_enum = self.next() % 3 == 0;
        if has_enum {
            let n_variants = (self.next() % 3 + 2) as usize; // 2-4 variants
            let variants: Vec<String> = (0..n_variants)
                .map(|j| format!("V{}", j))
                .collect();
            decls.push(format!("enum E {{ {} }}", variants.join(", ")));
        }

        // Optionally emit a helper function
        let has_func = self.next() % 3 == 0;
        if has_func {
            let body_expr = self.gen_simple_expr(2);
            decls.push(format!(
                "helper(a: i32, b: i32) -> i32 {{\n    {}\n}}",
                body_expr
            ));
        }

        // Optionally emit an `a * b + c` helper for each float type. The call
        // exercises float argument/return-value bridging — i.e. saving and
        // restoring the relevant float window (f0..f3 / d0..d3) around a call.
        let mut has_fma = [false; FLOAT_KINDS.len()];
        for (k, kind) in FLOAT_KINDS.iter().enumerate() {
            if self.next() % 3 == 0 {
                has_fma[k] = true;
                decls.push(format!(
                    "{}(a: {t}, b: {t}, c: {t}) -> {t} {{\n    return a * b + c\n}}",
                    kind.fma,
                    t = kind.ty
                ));
            }
        }

        // 1-4 initial integer variables
        let n_vars = (self.next() % 4 + 1) as usize;
        for i in 0..n_vars {
            let name = format!("v{}", i);
            let val = (self.next() % 200) as i32 - 100;
            main_lines.push(format!("    var {} = {}", name, self.format_int(val)));
            vars.push((name, VarType::Int));
        }

        // Optionally declare a struct variable
        if has_struct {
            let name = "s0".to_string();
            main_lines.push(format!("    var {}: S", name));
            // Initialize fields
            let n_fields = (self.next() % 3 + 2) as usize;
            for j in 0..n_fields.min(4) {
                let val = (self.next() % 200) as i32 - 100;
                main_lines.push(format!("    {}.f{} = {}", name, j, self.format_int(val)));
            }
            vars.push((name, VarType::Struct));
        }

        // Optionally declare an array
        let has_array = self.next() % 3 == 0;
        if has_array {
            let size = (self.next() % 4 + 2) as usize; // 2-5
            let name = "arr".to_string();
            main_lines.push(format!("    var {}: [i32; {}]", name, size));
            // Initialize elements
            for j in 0..size {
                let val = (self.next() % 200) as i32 - 100;
                main_lines.push(format!("    {}[{}] = {}", name, j, self.format_int(val)));
            }
            vars.push((name, VarType::Array(size)));
        }

        // Optionally declare an enum variable
        if has_enum {
            let n_variants = (self.next() % 3 + 2) as usize;
            let name = "e0".to_string();
            let variant = self.next() as usize % n_variants;
            main_lines.push(format!("    var {}: E", name));
            main_lines.push(format!("    {} = .V{}", name, variant));
            vars.push((name, VarType::Enum(n_variants)));
        }

        // Optionally declare 1-2 variables of each float type. f32 drives the
        // stack VM's single float window (f0..f3, spilling through fsp); f64
        // drives the double window (d0..d3, spilling through dfsp). Values are
        // kept small so downstream `as i32` casts stay well inside i32 range.
        let mut has_floats = [false; FLOAT_KINDS.len()];
        for (k, kind) in FLOAT_KINDS.iter().enumerate() {
            if self.next() % 2 == 0 {
                has_floats[k] = true;
                let n_fvars = (self.next() % 2 + 1) as usize; // 1-2
                for i in 0..n_fvars {
                    let name = format!("{}{}", kind.var_prefix, i);
                    main_lines
                        .push(format!("    var {} = {}", name, self.gen_float_literal(kind.ty)));
                    vars.push((name, VarType::Float(kind.ty)));
                }
            }
        }

        // 1-5 computed values, each printed
        let n_stmts = (self.next() % 5 + 1) as usize;
        for i in 0..n_stmts {
            let name = format!("r{}", i);
            let expr = self.gen_expr(&vars, has_func, 3);
            main_lines.push(format!("    let {} = {}", name, expr));
            main_lines.push(format!("    print({})", name));
            vars.push((name, VarType::Int));
        }

        // Float computations, each printed as i32. Printing the truncated
        // integer (rather than the raw float) keeps output comparable across
        // backends — Rust's float Display ("3") and the C interpreter's printf
        // ("3.0") format integral floats differently, so raw float prints
        // would diverge on formatting alone. The float arithmetic itself is
        // still fully exercised before the cast.
        for (k, kind) in FLOAT_KINDS.iter().enumerate() {
            if !has_floats[k] {
                continue;
            }
            let n_fstmts = (self.next() % 3 + 1) as usize; // 1-3
            for i in 0..n_fstmts {
                let name = format!("{}r{}", kind.var_prefix, i);
                let expr = self.gen_float_expr(&vars, kind, has_fma[k], 2);
                main_lines.push(format!("    let {} = {}", name, expr));
                main_lines.push(format!("    print({} as i32)", name));
            }
        }

        main_lines.push("}".to_string());

        let mut parts = decls;
        parts.push(main_lines.join("\n"));
        parts.join("\n\n")
    }

    fn format_int(&self, val: i32) -> String {
        if val < 0 {
            format!("(0 - {})", -val)
        } else {
            format!("{}", val)
        }
    }

    /// Generate a simple expression using only a, b parameters (for helper function body).
    fn gen_simple_expr(&mut self, depth: u8) -> String {
        if depth == 0 {
            return if self.next() % 2 == 0 { "a".into() } else { "b".into() };
        }
        match self.next() % 5 {
            0 => "a".into(),
            1 => "b".into(),
            2 => {
                let ops = ["+", "-", "*"];
                let op = ops[self.next() as usize % ops.len()];
                let l = self.gen_simple_expr(depth - 1);
                let r = self.gen_simple_expr(depth - 1);
                format!("({} {} {})", l, op, r)
            }
            3 => {
                let t = self.gen_simple_expr(depth - 1);
                let e = self.gen_simple_expr(depth - 1);
                format!("if a > b {{ {} }} else {{ {} }}", t, e)
            }
            4 => { let v = (self.next() % 200) as i32 - 100; self.format_int(v) }
            _ => unreachable!(),
        }
    }

    fn gen_expr(&mut self, vars: &[(String, VarType)], has_func: bool, depth: u8) -> String {
        if depth == 0 {
            return self.gen_leaf(vars);
        }

        let max_choice = if has_func { 16 } else { 14 };
        match self.next() % max_choice {
            // Literal (15%)
            0..=1 => self.gen_literal(),

            // Variable reference — int only (20%)
            2..=4 => self.gen_int_var(vars),

            // Binary arithmetic (20%) — no division to avoid safety errors
            5..=7 => {
                let ops = ["+", "-", "*"];
                let op = ops[self.next() as usize % ops.len()];
                let l = self.gen_expr(vars, has_func, depth - 1);
                let r = self.gen_expr(vars, has_func, depth - 1);
                format!("({} {} {})", l, op, r)
            }

            // Conditional (10%)
            8 => {
                let cond = self.gen_int_var(vars);
                let t = self.gen_expr(vars, has_func, depth - 1);
                let e = self.gen_expr(vars, has_func, depth - 1);
                format!("if {} > 0 {{ {} }} else {{ {} }}", cond, t, e)
            }

            // For-loop accumulator (5%)
            9 => {
                let bound = (self.next() % 6 + 1) as i32;
                let step_expr = self.gen_expr(vars, has_func, depth - 1);
                format!(
                    "{{\n        var acc = 0\n        \
                     for i in 0 .. {} {{\n            \
                     acc = acc + {}\n        \
                     }}\n        acc\n    }}",
                    bound, step_expr
                )
            }

            // Struct field access (5%)
            10 => {
                if let Some((name, VarType::Struct)) =
                    vars.iter().find(|(_, t)| matches!(t, VarType::Struct))
                {
                    let field = self.next() as usize % 4;
                    format!("{}.f{}", name, field)
                } else {
                    self.gen_literal()
                }
            }

            // Array element access (5%)
            11 => {
                if let Some((name, VarType::Array(size))) =
                    vars.iter().find(|(_, t)| matches!(t, VarType::Array(_)))
                {
                    let idx = self.next() as usize % size;
                    format!("{}[{}]", name, idx)
                } else {
                    self.gen_literal()
                }
            }

            // While loop (5%)
            12 => {
                let limit = (self.next() % 5 + 1) as i32;
                let step_expr = self.gen_expr(vars, has_func, depth - 1);
                format!(
                    "{{\n        var wc = 0\n        var wa = 0\n        \
                     while wc < {} {{\n            \
                     wa = wa + {}\n            \
                     wc = wc + 1\n        \
                     }}\n        wa\n    }}",
                    limit, step_expr
                )
            }

            // Comparison to int (5%)
            13 => {
                let a = self.gen_expr(vars, has_func, depth - 1);
                let b = self.gen_expr(vars, has_func, depth - 1);
                let ops = ["==", "!=", "<", ">"];
                let op = ops[self.next() as usize % ops.len()];
                // Comparisons return bool; cast to int via if
                format!("if {} {} {} {{ 1 }} else {{ 0 }}", a, op, b)
            }

            // Function call (only if has_func)
            14..=15 => {
                let a = self.gen_expr(vars, has_func, depth - 1);
                let b = self.gen_expr(vars, has_func, depth - 1);
                format!("helper({}, {})", a, b)
            }

            _ => self.gen_literal(),
        }
    }

    fn gen_leaf(&mut self, vars: &[(String, VarType)]) -> String {
        if self.next() % 3 == 0 {
            self.gen_int_var(vars)
        } else {
            self.gen_literal()
        }
    }

    fn gen_int_var(&mut self, vars: &[(String, VarType)]) -> String {
        let int_vars: Vec<&str> = vars
            .iter()
            .filter_map(|(name, ty)| {
                if matches!(ty, VarType::Int) {
                    Some(name.as_str())
                } else {
                    None
                }
            })
            .collect();
        if int_vars.is_empty() {
            self.gen_literal()
        } else {
            let idx = self.next() as usize % int_vars.len();
            int_vars[idx].to_string()
        }
    }

    fn gen_literal(&mut self) -> String {
        let val = (self.next() % 200) as i32 - 100;
        self.format_int(val)
    }

    /// A small float literal in [0.0, 9.9], typed as `ty` ("f32"/"f64").
    /// Bare float literals are already f32, so an `as f32` cast would be an
    /// unsupported identity conversion — emit the bare literal for f32 and an
    /// explicit `as f64` conversion for f64. Values are bounded so that
    /// products of a few of these stay far inside i32 range after the final
    /// `as i32` cast.
    fn gen_float_literal(&mut self, ty: &str) -> String {
        let whole = self.next() % 10;
        let frac = self.next() % 10;
        if ty == "f32" {
            format!("{}.{}", whole, frac)
        } else {
            format!("({}.{} as {})", whole, frac, ty)
        }
    }

    fn gen_float_expr(
        &mut self,
        vars: &[(String, VarType)],
        kind: &FloatKind,
        has_fma: bool,
        depth: u8,
    ) -> String {
        if depth == 0 {
            return self.gen_float_leaf(vars, kind);
        }
        let max_choice = if has_fma { 8 } else { 6 };
        match self.next() % max_choice {
            // Float literal
            0..=1 => self.gen_float_literal(kind.ty),
            // Float variable reference (of this type)
            2..=3 => self.gen_float_var(vars, kind.ty),
            // Binary arithmetic (no division — avoids safety errors)
            4..=5 => {
                let ops = ["+", "-", "*"];
                let op = ops[self.next() as usize % ops.len()];
                let l = self.gen_float_expr(vars, kind, has_fma, depth - 1);
                let r = self.gen_float_expr(vars, kind, has_fma, depth - 1);
                format!("({} {} {})", l, op, r)
            }
            // Helper call (only if its `a * b + c` helper was emitted) —
            // exercises float argument/return-value bridging across a call.
            6..=7 => {
                let a = self.gen_float_expr(vars, kind, has_fma, depth - 1);
                let b = self.gen_float_expr(vars, kind, has_fma, depth - 1);
                let c = self.gen_float_expr(vars, kind, has_fma, depth - 1);
                format!("{}({}, {}, {})", kind.fma, a, b, c)
            }
            _ => self.gen_float_literal(kind.ty),
        }
    }

    fn gen_float_leaf(&mut self, vars: &[(String, VarType)], kind: &FloatKind) -> String {
        if self.next() % 2 == 0 {
            self.gen_float_var(vars, kind.ty)
        } else {
            self.gen_float_literal(kind.ty)
        }
    }

    fn gen_float_var(&mut self, vars: &[(String, VarType)], ty: &str) -> String {
        let fvars: Vec<&str> = vars
            .iter()
            .filter_map(|(name, t)| match t {
                VarType::Float(vt) if *vt == ty => Some(name.as_str()),
                _ => None,
            })
            .collect();
        if fvars.is_empty() {
            self.gen_float_literal(ty)
        } else {
            let idx = self.next() as usize % fvars.len();
            fvars[idx].to_string()
        }
    }
}

/// A float type the generator can emit, paired with the names it uses for
/// that type's variables and `a * b + c` helper.
struct FloatKind {
    ty: &'static str,
    var_prefix: &'static str,
    fma: &'static str,
}

const FLOAT_KINDS: [FloatKind; 2] = [
    FloatKind { ty: "f32", var_prefix: "sv", fma: "sfma" },
    FloatKind { ty: "f64", var_prefix: "fv", fma: "ffma" },
];

#[derive(Clone)]
enum VarType {
    Int,
    Float(&'static str),
    Struct,
    Array(usize),
    Enum(usize),
}

/// Capture everything written to stdout (fd 1) during `f()`.
fn capture_stdout<F: FnOnce()>(f: F) -> String {
    std::io::stdout().flush().ok();

    let tmp = std::env::temp_dir().join("lyte_diff_fuzz_capture.txt");
    let file = match std::fs::File::create(&tmp) {
        Ok(f) => f,
        Err(_) => { f(); return String::new(); }
    };

    let saved_fd = unsafe { libc::dup(1) };
    if saved_fd < 0 {
        drop(file);
        f();
        return String::new();
    }
    unsafe { libc::dup2(file.as_raw_fd(), 1) };
    drop(file);

    f();

    std::io::stdout().flush().ok();
    // The stack backend's C interpreter prints via C stdio (printf), which
    // buffers independently of Rust's stdout. Flush all C streams before
    // restoring fd 1, or the buffered output is lost and the capture comes
    // back empty.
    unsafe { libc::fflush(std::ptr::null_mut()) };

    unsafe { libc::dup2(saved_fd, 1) };
    unsafe { libc::close(saved_fd) };

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
        #[cfg(feature = "cranelift")]
        "jit" => {
            let output = capture_stdout(|| {
                compiler.run();
            });
            let output = strip_prefix_line(&output, "compilation successful");
            Some(output)
        }
        "vm" => {
            let output = capture_stdout(|| {
                let _ = compiler.run_vm();
            });
            Some(output)
        }
        #[cfg(has_stack_interp)]
        "stack" => {
            // Compile to the stack VM and run it through the C interpreter
            // (the same path cli/src/main.rs uses for `--backend stack`).
            let output = capture_stdout(|| {
                if let Ok(program) = compiler.compile_stack() {
                    let _ = lyte::stack_interp_bridge::run(&program);
                }
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
    if data.len() < 10 {
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
    let vm_output = run_backend(&program, "vm");

    #[cfg(feature = "cranelift")]
    {
        let jit_output = run_backend(&program, "jit");
        assert_same("JIT", &jit_output, "VM", &vm_output, &program);
    }

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

    #[cfg(has_stack_interp)]
    {
        let stack_output = run_backend(&program, "stack");
        assert_same("VM", &vm_output, "STACK", &stack_output, &program);
    }
});
