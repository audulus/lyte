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
            compiler.parse_file(&path.unwrap().path());
        }
    } else {
        let file = args.file;
        compiler.parse_file(Path::new(&file));
    }

    for decl in &compiler.decls {
        println!("decl: {:?}", decl);
    }

    let mut i = 0;
    for expr in &compiler.exprs.exprs {
        println!("{}: {:?}", i, expr);
        i += 1;
    }

}
