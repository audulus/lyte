/// Golden tests: run the suite once per backend.
/// The --backend flag selects the backend (default: jit).
/// Each backend produces identical output, so the same expected stdout works for all.
///
/// Tests can opt out of specific backends with `// skip-backend: vm` (or `llvm`, `jit`).
///
/// Backend is passed via CLI args (base_args) rather than env vars, so the three
/// test functions can safely run in parallel without environment variable races.

/// Path to the just-built `lyte` binary. `CARGO_BIN_EXE_lyte` is set by
/// Cargo for integration tests and resolves correctly regardless of any
/// `target-dir` override in `.cargo/config.toml`.
const LYTE_BIN: &str = env!("CARGO_BIN_EXE_lyte");

fn run_golden_tests(backend: &str) -> goldentests::TestResult<()> {
    let mut config = goldentests::TestConfig::new(LYTE_BIN, "../tests/cases", "// ");
    config.base_args = format!("--backend {} --skip-backend {}", backend, backend);
    config.run_tests()
}

#[test]
#[cfg(feature = "cranelift")]
fn golden_tests_jit() -> goldentests::TestResult<()> {
    run_golden_tests("jit")
}

#[test]
#[cfg(feature = "llvm")]
fn golden_tests_llvm() -> goldentests::TestResult<()> {
    run_golden_tests("llvm")
}

#[test]
fn golden_tests_vm() -> goldentests::TestResult<()> {
    run_golden_tests("vm")
}

#[test]
#[cfg(target_arch = "aarch64")]
fn golden_tests_asm() -> goldentests::TestResult<()> {
    run_golden_tests("asm")
}

#[test]
#[cfg(has_stack_interp)]
fn golden_tests_stack() -> goldentests::TestResult<()> {
    run_golden_tests("stack")
}

#[test]
#[cfg(feature = "llvm")]
fn aot_emits_arm64_ios_object_and_header() {
    use std::io::Write;

    let tmp = std::env::temp_dir().join("lyte_aot_smoke");
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();

    let src = tmp.join("smoke.lyte");
    let mut f = std::fs::File::create(&src).unwrap();
    writeln!(
        f,
        "var freq: f32\n\
         var sample_rate: f32\n\
         \n\
         init(rate: f32, f: f32) {{\n\
             sample_rate = rate\n\
             freq = f\n\
         }}\n\
         \n\
         phase() -> f32 {{\n\
             return freq / sample_rate\n\
         }}"
    )
    .unwrap();
    drop(f);

    let out_o = tmp.join("smoke.o");
    let status = std::process::Command::new(LYTE_BIN)
        .arg(&src)
        .arg("--no-recursion")
        .arg("--entry")
        .arg("init,phase")
        .arg("--aot")
        .arg(&out_o)
        .status()
        .expect("failed to invoke lyte");
    assert!(status.success(), "--aot exited with {}", status);
    assert!(out_o.exists(), "object file not written");

    let out_h = tmp.join("smoke.h");
    assert!(out_h.exists(), "header file not written");

    // Check Mach-O 64-bit arm64 magic + cputype rather than shelling out to
    // `file`, so the test runs anywhere.
    let bytes = std::fs::read(&out_o).expect("read .o");
    assert!(bytes.len() >= 8, ".o too short");
    let magic = u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
    assert_eq!(magic, 0xfeed_facf, "expected Mach-O 64-bit magic");
    let cputype = u32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]);
    assert_eq!(cputype, 0x0100_000c, "expected arm64 cputype (got {:#x})", cputype);

    let header = std::fs::read_to_string(&out_h).expect("read .h");
    assert!(header.contains("#define SMOKE_H"), "missing include guard");
    assert!(header.contains("SMOKE_STATE_SIZE"), "missing state-size macro");
    assert!(header.contains("smoke_init"), "missing init wrapper decl");
    assert!(header.contains("smoke_phase"), "missing phase wrapper decl");
    assert!(header.contains("SMOKE_OFFSET_freq"), "missing freq global offset");
    assert!(header.contains("smoke_globals"), "missing metadata table");
    assert!(header.contains("smoke_assert"), "missing assert hook decl");
}

#[test]
#[cfg(feature = "llvm")]
fn aot_header_macros_keep_global_case() {
    // Globals `r` and `R` are distinct, so their macros must be too. Uppercasing
    // the name would redefine `CASE_OFFSET_R` (a -Wmacro-redefined warning in
    // every TU that includes the header).
    use std::io::Write;

    let tmp = std::env::temp_dir().join("lyte_aot_case");
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();

    let src = tmp.join("case.lyte");
    let mut f = std::fs::File::create(&src).unwrap();
    writeln!(
        f,
        "var r: f32\n\
         var R: f32\n\
         \n\
         init(a: f32, b: f32) {{\n\
             r = a\n\
             R = b\n\
         }}\n\
         \n\
         sum() -> f32 {{\n\
             return r + R\n\
         }}"
    )
    .unwrap();
    drop(f);

    let status = std::process::Command::new(LYTE_BIN)
        .arg(&src)
        .arg("--no-recursion")
        .arg("--entry")
        .arg("init,sum")
        .arg("--aot")
        .arg(tmp.join("case.o"))
        .status()
        .expect("failed to invoke lyte");
    assert!(status.success(), "--aot exited with {}", status);

    let header = std::fs::read_to_string(tmp.join("case.h")).expect("read .h");
    assert!(header.contains("#define CASE_OFFSET_r "), "missing macro for global r");
    assert!(header.contains("#define CASE_OFFSET_R "), "missing macro for global R");
    assert!(header.contains("{ \"r\","), "metadata table lost the real name r");
    assert!(header.contains("{ \"R\","), "metadata table lost the real name R");

    let mut defined = std::collections::HashSet::new();
    for line in header.lines() {
        if let Some(rest) = line.strip_prefix("#define ") {
            let name = rest.split_whitespace().next().unwrap();
            assert!(defined.insert(name), "macro {} defined twice", name);
        }
    }
}

#[test]
#[cfg(feature = "llvm")]
fn aot_header_macros_suffix_sanitized_name_collisions() {
    // An overloaded extern is mangled to `send$f32`, which sanitizes to the
    // same macro name as a global called `send_f32`. The repeat gets a `_2`
    // suffix instead of redefining the first macro.
    use std::io::Write;

    let tmp = std::env::temp_dir().join("lyte_aot_overload");
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();

    let src = tmp.join("ovl.lyte");
    let mut f = std::fs::File::create(&src).unwrap();
    writeln!(
        f,
        "var send_f32: f32\n\
         extern fn send(x: f32)\n\
         extern fn send(x: i32)\n\
         \n\
         both(a: f32, b: i32) {{\n\
             send_f32 = a\n\
             send(a)\n\
             send(b)\n\
         }}"
    )
    .unwrap();
    drop(f);

    let status = std::process::Command::new(LYTE_BIN)
        .arg(&src)
        .arg("--no-recursion")
        .arg("--entry")
        .arg("both")
        .arg("--aot")
        .arg(tmp.join("ovl.o"))
        .status()
        .expect("failed to invoke lyte");
    assert!(status.success(), "--aot exited with {}", status);

    let header = std::fs::read_to_string(tmp.join("ovl.h")).expect("read .h");
    assert!(header.contains("#define OVL_OFFSET_send_f32 "), "missing macro for the global");
    assert!(header.contains("#define OVL_OFFSET_send_f32_2 "), "missing suffixed macro for the extern");
    assert!(header.contains("#define OVL_OFFSET_send_i32 "), "missing macro for the other overload");
    assert!(header.contains("{ \"send_f32\","), "metadata table lost the global");
    assert!(header.contains("{ \"send$f32\","), "metadata table lost the mangled extern");

    let mut defined = std::collections::HashSet::new();
    for line in header.lines() {
        if let Some(rest) = line.strip_prefix("#define ") {
            let name = rest.split_whitespace().next().unwrap();
            assert!(defined.insert(name), "macro {} defined twice", name);
        }
    }
}

#[test]
#[cfg(feature = "llvm")]
fn aot_two_objects_have_disjoint_public_symbols() {
    // Two .lyte programs that both define internal helpers and entry points
    // with the same names must produce link-disjoint .o files when given
    // different prefixes.
    use std::io::Write;

    let tmp = std::env::temp_dir().join("lyte_aot_collision");
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();

    for prefix in &["nodea", "nodeb"] {
        let src = tmp.join(format!("{}.lyte", prefix));
        let mut f = std::fs::File::create(&src).unwrap();
        writeln!(
            f,
            "helper(x: f32) -> f32 {{\n\
                 return x * x\n\
             }}\n\
             \n\
             run(input: f32) -> f32 {{\n\
                 return helper(input)\n\
             }}"
        )
        .unwrap();
        drop(f);
        let out_o = tmp.join(format!("{}.o", prefix));
        let status = std::process::Command::new(LYTE_BIN)
            .arg(&src)
            .arg("--no-recursion")
            .arg("--entry")
            .arg("run")
            .arg("--aot")
            .arg(&out_o)
            .status()
            .expect("failed to invoke lyte");
        assert!(status.success(), "--aot exited with {}", status);
    }

    // Read both .o files and grep their byte content for the public symbols.
    // We don't parse Mach-O; we just confirm that the *other* program's
    // wrapper name does NOT appear in this .o.
    let a = std::fs::read(tmp.join("nodea.o")).unwrap();
    let b = std::fs::read(tmp.join("nodeb.o")).unwrap();
    assert!(contains_substr(&a, b"nodea_run"), "nodea.o missing its wrapper");
    assert!(contains_substr(&b, b"nodeb_run"), "nodeb.o missing its wrapper");
    assert!(!contains_substr(&a, b"nodeb_run"), "nodea.o leaks nodeb_run");
    assert!(!contains_substr(&b, b"nodea_run"), "nodeb.o leaks nodea_run");
    // Internal helper from either program should NOT appear as an external
    // symbol with the bare name; either it's inlined away or set to private
    // linkage. We can't tell without a Mach-O parser, but the public-name
    // disjointness check above is the load-bearing one for "can they link".
}

#[cfg(feature = "llvm")]
fn contains_substr(haystack: &[u8], needle: &[u8]) -> bool {
    haystack
        .windows(needle.len())
        .any(|w| w == needle)
}

#[test]
#[cfg(feature = "llvm")]
fn aot_target_macos_x86_64_emits_x86_object() {
    use std::io::Write;
    let tmp = std::env::temp_dir().join("lyte_aot_x86");
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();
    let src = tmp.join("sx.lyte");
    let mut f = std::fs::File::create(&src).unwrap();
    writeln!(f, "go(x: f32) -> f32 {{ return x * x }}").unwrap();
    drop(f);
    let out_o = tmp.join("sx.o");
    let status = std::process::Command::new(LYTE_BIN)
        .arg(&src)
        .arg("--no-recursion")
        .arg("--entry")
        .arg("go")
        .arg("--target")
        .arg("macos-x86_64")
        .arg("--aot")
        .arg(&out_o)
        .status()
        .expect("failed to invoke lyte");
    assert!(status.success(), "--aot exited with {}", status);
    // Mach-O 64-bit x86_64: same magic 0xfeedfacf, cputype 0x01000007.
    let bytes = std::fs::read(&out_o).expect("read .o");
    let magic = u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
    let cputype = u32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]);
    assert_eq!(magic, 0xfeed_facf);
    assert_eq!(cputype, 0x0100_0007, "expected x86_64 cputype (got {:#x})", cputype);
}
