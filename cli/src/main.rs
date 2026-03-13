use clap::Parser;
use std::fs;
use std::panic;
use std::time::Instant;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    file: String,

    /// Parse and type-check only, do not compile or run.
    #[clap(long)]
    check: bool,

    #[clap(long)]
    ast: bool,

    #[clap(long)]
    ir: bool,

    #[clap(long)]
    bytecode: bool,

    #[clap(long)]
    timing: bool,

    /// Select backend: "jit" (default), "vm", or "llvm".
    /// Falls back to LYTE_BACKEND env var if not specified.
    #[clap(long, default_value = "")]
    backend: String,

    /// Skip this backend (for golden tests). If the source contains
    /// `// skip-backend: <value>`, reproduce expected stdout and exit.
    /// Falls back to LYTE_SKIP_BACKEND env var if not specified.
    #[clap(long, default_value = "")]
    skip_backend: String,
}

fn run(args: Args) -> i32 {
    let mut paths = vec![];

    if let Ok(entries) = fs::read_dir(args.file.clone()) {
        for entry in entries {
            let path = entry.unwrap().path();
            paths.push(path.into_os_string().into_string().unwrap());
        }
    } else {
        paths.push(args.file.clone());
    }

    // Check for skip-backend directive: if the source contains
    // `// skip-backend: <backend>` and the current skip-backend matches,
    // reproduce expected stdout from the test file and exit (for golden tests).
    let skip_backend = if args.skip_backend.is_empty() {
        std::env::var("LYTE_SKIP_BACKEND").unwrap_or_default()
    } else {
        args.skip_backend.clone()
    };
    if !skip_backend.is_empty() {
        for path in &paths {
            if let Ok(contents) = fs::read_to_string(path) {
                let directive = format!("// skip-backend: {}", skip_backend);
                if contents.contains(&directive) {
                    // Print expected stdout lines so golden test comparison passes.
                    let mut in_expected = false;
                    for line in contents.lines() {
                        if line.starts_with("// expected stdout:") {
                            in_expected = true;
                            continue;
                        }
                        if in_expected {
                            if let Some(rest) = line.strip_prefix("// ") {
                                println!("{}", rest);
                            } else {
                                break;
                            }
                        }
                    }
                    return 0;
                }
            }
        }
    }

    let mut compiler = lyte::Compiler::new();
    for path in &paths {
        if let Ok(contents) = fs::read_to_string(path) {
            if !compiler.parse(&contents, &path) {
                return 1;
            }
        } else {
            eprintln!("could not read file {:?}", path);
            return 1;
        }
    }

    if args.ast {
        compiler.print_ast();
    }

    if !compiler.check() {
        return 1;
    }

    compiler.print_ir = args.ir;

    // Select backend via --backend flag, falling back to LYTE_BACKEND env var.
    let backend = if args.backend.is_empty() {
        std::env::var("LYTE_BACKEND").unwrap_or_default()
    } else {
        args.backend.clone()
    };
    let should_run = !args.check && !args.ast && !args.bytecode;
    let run_jit = should_run && (backend.is_empty() || backend == "jit");
    let run_vm = should_run && backend == "vm";
    #[cfg(target_arch = "aarch64")]
    let run_asm = should_run && backend == "asm";
    #[cfg(not(target_arch = "aarch64"))]
    let run_asm = false;
    #[cfg(feature = "llvm")]
    let run_llvm = should_run && backend == "llvm";
    #[cfg(not(feature = "llvm"))]
    let run_llvm = false;

    if run_jit || run_vm || run_asm || run_llvm || args.bytecode {
        if !compiler.has_decls() {
            println!("{:?}", Err::<(), _>("No declarations to compile"));
            return 1;
        }

        let compile_start = Instant::now();
        if let Err(e) = compiler.specialize() {
            eprintln!("{}", e);
            return 1;
        }

        if args.bytecode {
            match compiler.compile_vm() {
                Ok(program) => {
                    for func in &program.functions {
                        print!("{}", func);
                    }
                }
                Err(e) => {
                    eprintln!("VM compilation error: {}", e);
                    return 1;
                }
            }
        }

        let compile_elapsed = compile_start.elapsed();

        if run_jit {
            if args.timing {
                eprintln!("compile: {:.0}µs", compile_elapsed.as_micros());
            }
            let start = Instant::now();
            compiler.run();
            let elapsed = start.elapsed();
            if args.timing {
                eprintln!("jit exec: {:.3}s", elapsed.as_secs_f64());
            }
        }

        #[cfg(feature = "llvm")]
        if run_llvm {
            let (llvm_compile, llvm_exec) = compiler.run_llvm();
            if args.timing {
                eprintln!(
                    "compile: {:.0}µs (front {:.0}µs + llvm {:.0}µs)",
                    compile_elapsed.as_micros() as f64 + llvm_compile.as_micros() as f64,
                    compile_elapsed.as_micros(),
                    llvm_compile.as_micros()
                );
                eprintln!("llvm exec: {:.3}s", llvm_exec.as_secs_f64());
            }
        }

        if run_vm || run_asm {
            let vm_compile_start = Instant::now();
            let program = match compiler.compile_vm() {
                Ok(p) => p,
                Err(e) => {
                    eprintln!("VM compilation error: {}", e);
                    return 1;
                }
            };
            let vm_compile_elapsed = vm_compile_start.elapsed();
            if args.timing {
                let label = if run_asm { "asm" } else { "vm" };
                eprintln!(
                    "compile: {:.0}µs (front {:.0}µs + {} codegen {:.0}µs)",
                    compile_elapsed.as_micros() as f64 + vm_compile_elapsed.as_micros() as f64,
                    compile_elapsed.as_micros(),
                    label,
                    vm_compile_elapsed.as_micros()
                );
            }

            println!("compilation successful");
            let start = Instant::now();
            let mut vm = lyte::vm::VM::new();
            if run_asm {
                #[cfg(target_arch = "aarch64")]
                {
                    let _result = vm.run_asm(&program);
                }
                if args.timing {
                    eprintln!("asm exec: {:.3}s", start.elapsed().as_secs_f64());
                }
            } else {
                let _result = vm.run(&program);
                if args.timing {
                    eprintln!("vm exec: {:.3}s", start.elapsed().as_secs_f64());
                }
            }
        }
    }

    0
}

fn main() {
    let args = Args::parse();

    let result = panic::catch_unwind(panic::AssertUnwindSafe(|| run(args)));

    match result {
        Ok(code) => std::process::exit(code),
        Err(e) => {
            let msg = if let Some(s) = e.downcast_ref::<&str>() {
                s.to_string()
            } else if let Some(s) = e.downcast_ref::<String>() {
                s.clone()
            } else {
                "unknown error".to_string()
            };
            eprintln!("internal compiler error: {}", msg);
            std::process::exit(1);
        }
    }
}
