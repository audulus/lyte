//! Complete 240-sample processing calls, ordinary optimized Stack versus native loops.
//! Run: cargo run --release --example native_loop_bench -- [blocks-per-round]
//! Times include input/control copies, Lyte setup, host invocation/status checks,
//! and output copying/consumption. Recurrence history persists between calls. Inputs
//! are prepared outside timing; processing allocates nothing. A cancellation
//! callback is installed, but a fresh 1,024-backedge budget covers each call.
//! Five warmed, alternating paired rounds report median and min..max ns/block.
//! Frontend, Stack lowering, and backend construction are reported separately.
//! All ten recipes run in both precisions; the three headline cases appear first.
//! These are small buffered kernels, not full Audulus graphs or coefficient design.

#[cfg(not(has_stack_interp))]
fn main() {
    eprintln!("This example requires the C Stack interpreter (has_stack_interp).");
}

#[cfg(has_stack_interp)]
fn main() {
    bench::run();
}

#[cfg(has_stack_interp)]
mod bench {
    use lyte::stack_interp_bridge::StackBackend;
    use lyte::stack_ir::{NativeStreamBody, StackOp};
    use lyte::{Compiler, Name, CANCEL_FLAG_RESERVED, TRAP_NONE};
    use std::{hint::black_box, ops::Range, time::Instant};

    const SAMPLES: usize = 240;
    const ROUNDS: usize = 5;

    struct Host {
        backend: StackBackend,
        globals: Vec<u8>,
        entry: u32,
        input: Range<usize>,
        controls: Range<usize>,
        output: Range<usize>,
        history: Range<usize>,
        copied_output: Vec<u8>,
        polls: Box<usize>,
    }

    unsafe extern "C" fn cancel(data: *mut u8) -> bool {
        *data.cast::<usize>() += 1;
        false
    }

    impl Host {
        fn build(compiler: &Compiler, native: bool, workload: &str) -> (Self, f64, f64) {
            let start = Instant::now();
            let program = if native {
                compiler.compile_stack_native_loops()
            } else {
                compiler.compile_stack()
            }
            .unwrap();
            let lowering_ms = start.elapsed().as_secs_f64() * 1e3;
            let kernels: Vec<_> = program
                .functions
                .iter()
                .flat_map(|f| &f.ops)
                .filter_map(|op| {
                    if let StackOp::NativeLoop(kernel) = op {
                        Some(&kernel.body)
                    } else {
                        None
                    }
                })
                .collect();
            assert_eq!(kernels.len(), usize::from(native));
            if native {
                assert!(
                    match kernels[0] {
                        NativeStreamBody::Pointwise { recipe, .. } =>
                            recipe.recipe().name == workload,
                        NativeStreamBody::Biquad { .. } => workload == "biquad",
                        NativeStreamBody::OnePole { .. } => workload == "one_pole",
                    },
                    "unexpected native recipe for {}",
                    workload
                );
            }
            let start = Instant::now();
            let mut backend = StackBackend::new(&program);
            let backend_ms = start.elapsed().as_secs_f64() * 1e3;
            let mut polls = Box::new(0);
            backend.set_cancel_callback(Some(cancel), (&mut *polls as *mut usize).cast());
            let info = compiler.globals_info_with_offset(CANCEL_FLAG_RESERVED as usize);
            let range = |name| {
                let (_, offset, size, _, _) = info.iter().find(|x| x.0 == name).unwrap();
                *offset..*offset + *size
            };
            let output = range("output");
            (
                Self {
                    backend,
                    globals: vec![0; program.globals_size],
                    entry: program.entry_points[&Name::str("process")],
                    input: range("input"),
                    controls: range("controls"),
                    history: range("history"),
                    copied_output: vec![0; output.len()],
                    output,
                    polls,
                },
                lowering_ms,
                backend_ms,
            )
        }

        fn process(&mut self, input: &[u8], controls: &[u8]) {
            self.globals[self.input.clone()].copy_from_slice(input);
            self.globals[self.controls.clone()].copy_from_slice(controls);
            assert_eq!(
                self.backend
                    .call_entry(self.entry, self.globals.as_mut_ptr()),
                0
            );
            assert!(!self.backend.cancelled());
            assert_eq!(self.backend.trap_reason(), TRAP_NONE);
            self.copied_output
                .copy_from_slice(&self.globals[self.output.clone()]);
            black_box(&self.copied_output);
        }

        fn reset(&mut self) {
            self.globals.fill(0);
            *self.polls = 0;
        }

        fn measure(&mut self, bank: &[(Vec<u8>, Vec<u8>)], blocks: usize) -> f64 {
            self.reset();
            for block in 0..2048 {
                let (input, controls) = &bank[block % bank.len()];
                self.process(input, controls);
            }
            let start = Instant::now();
            for block in 0..blocks {
                let (input, controls) = &bank[block % bank.len()];
                self.process(input, controls);
            }
            let ns = start.elapsed().as_secs_f64() * 1e9 / blocks as f64;
            assert_eq!(*self.polls, 0);
            ns
        }
    }

    fn source(scalar: &str, workload: &str) -> String {
        let (count, setup, body, finish) = match workload {
            "gain" => (1, "let gain = controls[0]", "let y = x*gain", ""),
            "offset" => (1, "let bias = controls[0]", "let y = x+bias", ""),
            // Lyte arrays are nonempty; square leaves its host control unused.
            "square" => (1, "", "let y = x*x", ""),
            "affine_map" => (
                2,
                "let gain = controls[0]; let bias = controls[1]",
                "let y = x*gain+bias",
                "",
            ),
            "offset_scale" => (
                2,
                "let bias = controls[0]; let gain = controls[1]",
                "let y = (x-bias)*gain",
                "",
            ),
            "cubic" => (1, "let k = controls[0]", "let y = x-k*((x*x)*x)", ""),
            "horner" => (
                4,
                "let a = controls[0]; let b = controls[1]
                 let c = controls[2]; let d = controls[3]",
                "let y = ((a*x+b)*x+c)*x+d",
                "",
            ),
            "one_pole" => (
                2,
                "let feed = controls[0]; let feedback = controls[1]; var state = history[0]",
                "state = x*feed+state*feedback; let y = state",
                "history[0] = state",
            ),
            "smootherstep" => (
                3,
                "let a = controls[0]; let b = controls[1]; let c = controls[2]",
                "let cube = (x*x)*x; let curve = x*(x*a-b)+c; let y = cube*curve",
                "",
            ),
            "biquad" => (
                5,
                "let b0 = controls[0]; let b1 = controls[1]; let b2 = controls[2]
                 let a1 = controls[3]; let a2 = controls[4]
                 var x1 = history[0]; var x2 = history[1]
                 var y1 = history[2]; var y2 = history[3]",
                "var y = b0*x; y = y+b1*x1; y = y+b2*x2; y = y-a1*y1; y = y-a2*y2
                 x2 = x1; x1 = x; y2 = y1; y1 = y",
                "history[0] = x1; history[1] = x2; history[2] = y1; history[3] = y2",
            ),
            _ => unreachable!(),
        };
        format!(
            "var input: [{scalar}; {SAMPLES}]
                 var output: [{scalar}; {SAMPLES}]
                 var controls: [{scalar}; {count}]
                 var history: [{scalar}; 4]
                 process() -> i32 {{
                     {setup}
                     for i in 0 .. {SAMPLES} {{
                         let x = input[i]
                         {body}
                         output[i] = y
                     }}
                     {finish}
                     0
                 }}"
        )
    }

    fn bank(scalar: &str, workload: &str) -> Vec<(Vec<u8>, Vec<u8>)> {
        let bytes = |values: &[f64]| -> Vec<u8> {
            values
                .iter()
                .flat_map(|&value| {
                    if scalar == "f32" {
                        (value as f32).to_ne_bytes().to_vec()
                    } else {
                        value.to_ne_bytes().to_vec()
                    }
                })
                .collect()
        };
        (0..16)
            .map(|block| {
                let input: Vec<_> = (0..SAMPLES)
                    .map(|i| ((i * 17 + block * 11) % 241) as f64 / 240.0)
                    .collect();
                let controls = match workload {
                    "gain" => vec![0.5 + block as f64 / 64.0],
                    "offset" => vec![0.25 + block as f64 / 64.0],
                    "square" => vec![0.0],
                    "affine_map" => vec![0.5 + block as f64 / 64.0, 0.25],
                    "offset_scale" => vec![0.25, 0.5 + block as f64 / 64.0],
                    "cubic" => vec![0.25 + block as f64 / 128.0],
                    "horner" => vec![0.5 + block as f64 / 64.0, -0.25, 0.125, 0.0625],
                    "one_pole" => vec![0.25 + block as f64 / 1024.0, 0.75 - block as f64 / 1024.0],
                    "smootherstep" => vec![6.0, 15.0, 10.0],
                    "biquad" => vec![0.5 + block as f64 / 1024.0, 0.25, 0.125, 0.0625, 0.03125],
                    _ => unreachable!(),
                };
                (bytes(&input), bytes(&controls))
            })
            .collect()
    }

    pub fn run() {
        let blocks = std::env::args()
            .nth(1)
            .map(|s| s.parse::<usize>().expect("blocks must be an integer"))
            .unwrap_or(32768);
        assert!(blocks > 0);
        println!("240 samples/call; callback installed; {ROUNDS} paired rounds of {blocks} blocks");
        println!("ns/block: median [min..max]; setup times are single observations in ms");
        for scalar in ["f32", "f64"] {
            for workload in [
                "gain",
                "biquad",
                "smootherstep",
                "one_pole",
                "affine_map",
                "offset_scale",
                "cubic",
                "horner",
                "offset",
                "square",
            ] {
                let source = source(scalar, workload);
                let start = Instant::now();
                let mut compiler = Compiler::new();
                compiler.quiet = true;
                compiler.set_entry_points(&["process"]);
                assert!(
                    compiler.parse(&source, "native_loop_bench.lyte"),
                    "{:?}",
                    compiler.last_errors
                );
                assert!(compiler.check(), "{:?}", compiler.last_errors);
                compiler.specialize().unwrap();
                let frontend_ms = start.elapsed().as_secs_f64() * 1e3;
                let (mut ordinary, lower, backend) = Host::build(&compiler, false, workload);
                let (mut native, native_lower, native_backend) =
                    Host::build(&compiler, true, workload);
                let bank = bank(scalar, workload);
                for block in 0..128 {
                    let (input, controls) = &bank[block % bank.len()];
                    ordinary.process(input, controls);
                    native.process(input, controls);
                    assert_eq!(
                        ordinary.copied_output, native.copied_output,
                        "{scalar} {workload}, block {block}"
                    );
                    assert_eq!(
                        ordinary.globals[ordinary.history.clone()],
                        native.globals[native.history.clone()]
                    );
                }
                assert_eq!((*ordinary.polls, *native.polls), (0, 0));
                let mut times = [[0.0; ROUNDS]; 2];
                for round in 0..ROUNDS {
                    for variant in if round % 2 == 0 { [0, 1] } else { [1, 0] } {
                        let host = if variant == 0 {
                            &mut ordinary
                        } else {
                            &mut native
                        };
                        times[variant][round] = host.measure(&bank, blocks);
                    }
                }
                for values in &mut times {
                    values.sort_by(f64::total_cmp);
                }
                let [a, b] = times;
                println!("{scalar} {workload:12} Stack {:8.1} [{:.1}..{:.1}]  native {:8.1} [{:.1}..{:.1}]  {:5.2}x",
                    a[2], a[0], a[4], b[2], b[0], b[4], a[2]/b[2]);
                println!("    setup: frontend {frontend_ms:.2}; Stack lower {lower:.2} + backend {backend:.2}; native lower {native_lower:.2} + backend {native_backend:.2}");
            }
        }
    }
}
