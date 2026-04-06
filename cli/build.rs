fn main() {
    // Propagate has_stack_interp cfg if the C compiler is Clang.
    let compiler = cc::Build::new().try_get_compiler();
    let is_clang = compiler
        .as_ref()
        .map(|c| c.is_like_clang())
        .unwrap_or(false);
    if is_clang {
        println!("cargo:rustc-cfg=has_stack_interp");
    }
    println!("cargo:rustc-check-cfg=cfg(has_stack_interp)");
}
