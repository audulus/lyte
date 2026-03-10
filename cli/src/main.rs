use clap::Parser;
use std::fs;
use std::panic;
use std::time::Instant;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    file: String,

    #[clap(long)]
    ast: bool,

    #[clap(long)]
    ir: bool,

    #[clap(long)]
    bytecode: bool,

    #[clap(short)]
    c: bool,

    #[clap(short)]
    r: bool,

    #[clap(short, long)]
    test: bool,

    #[clap(long)]
    timing: bool,
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

    // Only specialize when compiling or running - specialize requires a main function
    if args.c || args.r || args.test || args.bytecode {
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

        if args.c || args.test {
            if args.test {
                println!("executing compiled code");
            }
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

        if args.r || args.test {
            if args.test {
                println!("executing VM code");
            }
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
                eprintln!(
                    "compile: {:.0}µs (front {:.0}µs + vm codegen {:.0}µs)",
                    compile_elapsed.as_micros() as f64 + vm_compile_elapsed.as_micros() as f64,
                    compile_elapsed.as_micros(),
                    vm_compile_elapsed.as_micros()
                );
            }

            let start = Instant::now();
            let mut vm = lyte::vm::VM::new();
            let _result = vm.run(&program);
            let elapsed = start.elapsed();
            if args.timing {
                eprintln!("vm exec: {:.3}s", elapsed.as_secs_f64());
            }
            println!("vm execution successful");
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
