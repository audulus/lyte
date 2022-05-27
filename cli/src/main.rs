use clap::Parser;
use std::fs;
use std::path::Path;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    file: String,

    #[clap(long)]
    ast: bool,

    #[clap(long)]
    test: bool,
}

fn main() {
    println!("🎸");

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

    if args.test {

        let mut any_failed = false;

        for path in &paths {
            
            let mut compiler = lyte::Compiler::new();
            compiler.set_paths(vec![path.clone()]);

            let mut expect_failure = false;
            
            if let Ok(contents) = fs::read_to_string(path) {
                expect_failure = contents.starts_with("// expect failure");
                compiler.update_path(&path, contents);
            } else {
                eprintln!("could not read file {:?}", path);
                std::process::exit(1)
            }

            let mut success = compiler.parsed() && compiler.check();
            if expect_failure {
                success = !success;
            }

            println!("{} {} {}", if success { "✅" } else { "❌"}, path, if expect_failure { "(expected failure)" } else { "" });

            if !success {
                any_failed = true;
            }

        }

        if any_failed {
            println!("some tests failed");
        } else {
            println!("all tests passed");
        }
        
        std::process::exit(if any_failed { 1 } else { 0 });

    } else {

        let mut compiler = lyte::Compiler::new();
        for path in &paths {
            if let Ok(contents) = fs::read_to_string(path) {
                compiler.update_path(&path, contents);
            } else {
                eprintln!("could not read file {:?}", path);
                std::process::exit(1)
            }
        }
    
        compiler.set_paths(paths);
    
        if args.ast {
            compiler.print_ast();
        }
    
        let mut result = 0;
    
        if !compiler.parsed() {
            result = 1;
        }
    
        if !compiler.check() {
            result = 1;
        }
    
        std::process::exit(result)

    }
}
