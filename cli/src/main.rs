use clap::Parser;
use std::fs;
use std::path::Path;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    file: String,
}

fn main() {
    println!("🎸");

    let args = Args::parse();
    let mut compiler = lyte::Compiler::new();

    if let Ok(paths) = fs::read_dir(args.file.clone()) {
        for path in paths {
            if !compiler.parse_file(&path.unwrap().path()) {
                std::process::exit(1)
            }
        }
    } else {
        let file = args.file;
        if !compiler.parse_file(Path::new(&file)) {
            std::process::exit(1)
        }
    }

    compiler.check();

    for decl in &compiler.decls {
        println!("decl: {:?}", decl);
    }

    compiler.print_exprs();

}
