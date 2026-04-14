use std::io::Write;
use std::process::Command;

fn main() {
    // Compile the C stack interpreter with preserve_none + musttail.
    // Clang-only feature (skip on GCC). The handler signature keeps
    // the TOS window (t0..t3), the int stack pointer, the float
    // window (f0..f3), fsp, locals, ctx, pc, and nh all in
    // preserve_none arg registers across the entire dispatch chain.
    let compiler = cc::Build::new().try_get_compiler();
    let is_clang = compiler
        .as_ref()
        .map(|c| c.is_like_clang())
        .unwrap_or(false);
    println!("cargo:rustc-check-cfg=cfg(has_stack_interp)");
    if is_clang {
        cc::Build::new()
            .file("src/stack_interp.c")
            .include("src")
            .opt_level(3)
            .flag("-std=c11")
            .flag("-Wno-unused-parameter")
            // Strip the frame-pointer prologue/epilogue from every
            // handler. In direct-threaded tail-call dispatch every
            // handler is entered as "a new function", so a 3-insn
            // rbp setup/teardown runs on every instruction — ~20% of
            // a hot float handler's body. aarch64 already omits it at
            // -O3; this flag forces it off on macOS x86-64 (where
            // the Darwin ABI defaults to keeping rbp) and on Ubuntu
            // 24.04's patched clang (which defaults to
            // -fno-omit-frame-pointer for system-wide profiling). The
            // interpreter TU does no setjmp/longjmp and no native
            // unwinding runs through it, so dropping rbp is safe.
            .flag("-fomit-frame-pointer")
            // Promote `unknown attribute ignored` to a hard error. The
            // stack VM's correctness *depends* on `preserve_none` — it
            // pins hot state (TOS window, pc, sp, locals, fsp, nh) to
            // GPRs across the entire musttail dispatch chain. Clang
            // before 19 silently ignored the attribute and fell back
            // to System V cc, which quietly corrupted interpreter
            // state mid-loop (seen in Ubuntu CI with clang 18). Turning
            // this into an error means the next time someone builds
            // with a clang that doesn't understand preserve_none, the
            // build fails loudly instead of producing a subtly broken
            // interpreter.
            .flag("-Werror=unknown-attributes")
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
