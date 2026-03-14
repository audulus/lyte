use std::io::Write;
use std::process::Command;

fn main() {
    // Determine sizeof(jmp_buf) on this platform by compiling and running a C snippet.
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let c_path = format!("{}/jmpbuf_size.c", out_dir);
    let bin_path = format!("{}/jmpbuf_size", out_dir);

    std::fs::File::create(&c_path)
        .and_then(|mut f| {
            f.write_all(
                b"#include <setjmp.h>\n#include <stdio.h>\n\
                  int main() { printf(\"%zu\", sizeof(jmp_buf)); return 0; }\n",
            )
        })
        .expect("failed to write jmpbuf_size.c");

    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".to_string());
    let compile = Command::new(&cc)
        .args([&c_path, "-o", &bin_path])
        .status();

    if let Ok(status) = compile {
        if status.success() {
            if let Ok(output) = Command::new(&bin_path).output() {
                if let Ok(size_str) = std::str::from_utf8(&output.stdout) {
                    if let Ok(size) = size_str.trim().parse::<usize>() {
                        println!("cargo:rustc-cfg=jmpbuf_size=\"{}\"", size);
                        println!("cargo:rustc-env=PLATFORM_JMPBUF_SIZE={}", size);
                        return;
                    }
                }
            }
        }
    }

    // Cross-compiling or no C compiler — skip the check.
    eprintln!("warning: could not determine sizeof(jmp_buf), skipping size check");
}
