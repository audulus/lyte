use std::io::Write;
use std::process::Command;

fn main() {
    // Compile the C stack interpreter with preserve_none + musttail.
    // - Clang-only features (skip on GCC).
    // - The handler signature has 13 register arguments (ctx, pc, sp, locals,
    //   lm, l0, l1, l2, t0, t1, t2, t3, nh). preserve_none on x86-64 exposes
    //   ~12 argument registers, which isn't enough — the register allocator
    //   fails with "ran out of registers". AArch64 has enough, so gate on that.
    let target_arch = std::env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default();
    let compiler = cc::Build::new().try_get_compiler();
    let is_clang = compiler
        .as_ref()
        .map(|c| c.is_like_clang())
        .unwrap_or(false);
    let is_aarch64 = target_arch == "aarch64";
    println!("cargo:rustc-check-cfg=cfg(has_stack_interp)");
    if is_clang && is_aarch64 {
        cc::Build::new()
            .file("src/stack_interp.c")
            .include("src")
            .opt_level(3)
            .flag("-std=c11")
            .flag("-Wno-unused-parameter")
            .compile("stack_interp");
        println!("cargo:rustc-cfg=has_stack_interp");
    }
    println!("cargo:rerun-if-changed=src/stack_interp.c");
    println!("cargo:rerun-if-changed=src/stack_interp.h");
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
    let compile = Command::new(&cc).args([&c_path, "-o", &bin_path]).status();

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
