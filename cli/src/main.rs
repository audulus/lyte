use clap::Parser;
use std::fs;
use std::path::Path;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    file: String,

    #[clap(long)]
    ast: bool,
}

fn main() {
    println!("🎸");

    let args = Args::parse();
    let mut compiler = lyte::Compiler::new();
    let mut paths = vec![];

    if let Ok(entries) = fs::read_dir(args.file.clone()) {
        for entry in entries {
            let path = entry.unwrap().path();
            paths.push(path.into_os_string().into_string().unwrap());
        }
    } else {
        paths.push(args.file.clone());
    }

    for path in &paths {
        compiler.update_path(&path);
    }

    compiler.set_paths(paths);

    if args.ast {
        compiler.print_ast();
    }

    if !compiler.check() {
        std::process::exit(1)
    }
}
