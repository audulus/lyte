use clap::Parser;
use std::fs;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    file: String,

    #[clap(long)]
    ast: bool,

    #[clap(long)]
    ir: bool,

    #[clap(short)]
    c: bool,

    #[clap(short)]
    r: bool,
}

fn main() {
    let args = Args::parse();

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
                std::process::exit(1);
            }
        } else {
            eprintln!("could not read file {:?}", path);
            std::process::exit(1)
        }
    }

    if !compiler.check() {
        std::process::exit(1);
    }

    compiler.print_ir = args.ir;

    if args.c {
        if !compiler.has_decls() {
            println!("{:?}", Err::<(), _>("No declarations to compile"));
            std::process::exit(1);
        }
        compiler.run();
    }

    if args.r {
        if !compiler.has_decls() {
            println!("{:?}", Err::<(), _>("No declarations to compile"));
            std::process::exit(1);
        }
        match compiler.run_vm() {
            Ok(_) => println!("vm execution successful"),
            Err(e) => {
                eprintln!("VM error: {}", e);
                std::process::exit(1);
            }
        }
    }

    std::process::exit(0)
}
