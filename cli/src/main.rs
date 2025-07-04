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

    #[clap(long)]
    c: bool,
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

    let mut compiler = lyte::Compiler2::new();
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

    if args.c {
        compiler.run();
    }

    std::process::exit(0)
}
