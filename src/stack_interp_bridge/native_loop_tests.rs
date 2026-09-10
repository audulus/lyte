use super::*;
use crate::stack_ir::{NativeStreamSlots, StackFunction};

const LENGTH: usize = 240;
const STORAGE: usize = LENGTH + 4;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Edge {
    Mixed,
    SeparateRounding,
    NegativeZero,
}

#[derive(Clone, Copy, Debug)]
struct Case {
    double: bool,
    recurrence: bool,
    biquad: bool,
    edge: Edge,
    // None is disjoint, Some(0) is in-place; +/-1 overlap by one sample.
    overlap: Option<isize>,
    start: i64,
    end: i64,
    input_len: i64,
    output_len: i64,
    prefix: i64,
    callback: bool,
    cancel: bool,
    mutate: bool,
}

fn bits(double: bool, value: f64) -> u64 {
    if double {
        value.to_bits()
    } else {
        (value as f32).to_bits() as u64
    }
}

fn program(case: Case, native: bool, input: *const u8, output: *mut u8) -> StackProgram {
    let mut function = StackFunction::new(if case.biquad {
        "biquad_kernel"
    } else {
        "stream_kernel"
    });
    function.local_count = if case.biquad { 20 } else { 12 };
    function.has_return_value = true;
    let epsilon = if case.double {
        f64::EPSILON
    } else {
        f32::EPSILON as f64
    };
    let (coefficients, states) = if case.biquad {
        match case.edge {
            Edge::Mixed => (
                [0.125, 0.25, 0.125, -0.75, 0.25],
                [-0.25, 0.125, 0.0625, -0.125],
            ),
            Edge::SeparateRounding => (
                [1.0 + epsilon, 1.0, 0.0, 0.0, 0.0],
                [-1.0 - 2.0 * epsilon, 0.0, 0.0, 0.0],
            ),
            Edge::NegativeZero => ([1.0, 1.0, 1.0, -1.0, -1.0], [-0.0; 4]),
        }
    } else {
        let (first, second) = match (case.edge, case.recurrence) {
            (Edge::NegativeZero, true) => (1.0, 1.0),
            (Edge::NegativeZero, false) => (1.0, -0.0),
            (Edge::SeparateRounding, true) => (1.0 + epsilon, 1.0),
            (Edge::Mixed, true) => (0.5, 0.25),
            (_, false) => (1.0 + epsilon, -1.0 - 2.0 * epsilon),
        };
        let initial_state = if case.edge == Edge::SeparateRounding {
            -1.0 - 2.0 * epsilon
        } else {
            -0.0
        };
        (
            [first, second, 0.0, 0.0, 0.0],
            [initial_state, 0.0, 0.0, 0.0],
        )
    };
    let locals = [
        case.start as u64,
        case.end as u64,
        input as u64,
        output as u64,
        bits(case.double, coefficients[0]),
        bits(case.double, coefficients[1]),
        bits(case.double, states[0]),
        case.input_len as u64,
        case.output_len as u64,
        0,
        0,
        case.prefix as u64,
        bits(case.double, coefficients[2]),
        bits(case.double, coefficients[3]),
        bits(case.double, coefficients[4]),
        bits(case.double, states[1]),
        bits(case.double, states[2]),
        bits(case.double, states[3]),
        0,
        0,
    ];
    for (slot, &value) in locals[..usize::from(function.local_count)]
        .iter()
        .enumerate()
    {
        function.emit(StackOp::I64Const(value as i64));
        function.emit(StackOp::LocalSet(slot as u16));
    }
    function.emit(StackOp::FusedGetGetILtJumpIfZero(10, 11, 2));
    function.emit(StackOp::FusedGetAddImmSet(10, 1, 10));
    function.emit(StackOp::Jump(-3));
    function.emit(StackOp::I64Const(17));
    function.emit(StackOp::F32ConstF(19.0));
    function.emit(StackOp::F64ConstD(23.0));
    let gateway = function.pos();
    if native {
        function.emit(StackOp::NativeLoop(NativeLoopKernel {
            scalar: if case.double {
                NativeScalar::F64
            } else {
                NativeScalar::F32
            },
            counter: 0,
            end: 1,
            done: 0,
            spans: NativeStreamSlots {
                input: 2,
                output: 3,
                input_len: 7,
                output_len: 8,
            },
            body: if case.biquad {
                NativeStreamBody::Biquad {
                    coefficients: [4, 5, 12, 13, 14],
                    states: [6, 15, 16, 17],
                }
            } else if case.recurrence {
                NativeStreamBody::OnePole {
                    feed: 4,
                    feedback: 5,
                    state: 6,
                }
            } else {
                NativeStreamBody::Pointwise {
                    recipe: crate::pointwise::AFFINE,
                    coefficients: [4, 5, 0, 0],
                }
            },
        }));
    }
    let guard = function.pos();
    function.emit(StackOp::FusedGetGetILtJumpIfZero(0, 1, 0));
    if case.biquad {
        emit_scalar_biquad(&mut function, case.double);
    } else {
        emit_scalar_stream(&mut function, case.double, case.recurrence);
    }
    function.emit(StackOp::FusedGetAddImmSet(0, 1, 0));
    function.emit(StackOp::Jump(guard as i32 - function.pos() as i32 - 1));
    let done = function.pos();
    function.ops[guard] = StackOp::FusedGetGetILtJumpIfZero(0, 1, done as i32 - guard as i32 - 1);
    if native {
        match &mut function.ops[gateway] {
            StackOp::NativeLoop(kernel) => kernel.done = done as i32 - gateway as i32 - 1,
            _ => unreachable!(),
        }
    }
    for op in [
        StackOp::I64Const(17),
        StackOp::IEq,
        StackOp::JumpIfNotZero(2),
        StackOp::I64Const(0),
        StackOp::Assert,
        StackOp::F32ConstF(19.0),
        StackOp::FEqF,
        StackOp::JumpIfNotZero(2),
        StackOp::I64Const(0),
        StackOp::Assert,
        StackOp::F64ConstD(23.0),
        StackOp::DEqD,
        StackOp::JumpIfNotZero(2),
        StackOp::I64Const(0),
        StackOp::Assert,
        StackOp::FusedConstSet(1, 9),
        StackOp::LocalGet(if case.biquad { 16 } else { 6 }),
        StackOp::Return,
    ] {
        function.emit(op);
    }
    let mut program = StackProgram::new();
    program.entry = program.add_function(function);
    program
}

// Keep these scalar witnesses independent of native recipe matching/generation.
fn emit_scalar_biquad(function: &mut StackFunction, double: bool) {
    let get = |slot| {
        if double {
            StackOp::LocalGetD(slot)
        } else {
            StackOp::LocalGetF(slot)
        }
    };
    let set = |slot| {
        if double {
            StackOp::LocalSetD(slot)
        } else {
            StackOp::LocalSetF(slot)
        }
    };
    function.emit(StackOp::LocalGet(2));
    function.emit(StackOp::LocalGet(0));
    function.emit(StackOp::I64Const(if double { 8 } else { 4 }));
    function.emit(StackOp::IMul);
    function.emit(StackOp::IAdd);
    function.emit(if double {
        StackOp::LoadF64D
    } else {
        StackOp::LoadF32F
    });
    function.emit(set(18));
    for (term, (coefficient, state)) in [(4, 18), (5, 6), (12, 15), (13, 16), (14, 17)]
        .iter()
        .copied()
        .enumerate()
    {
        function.emit(get(coefficient));
        function.emit(get(state));
        function.emit(if double {
            StackOp::DMulD
        } else {
            StackOp::FMulF
        });
        if term != 0 {
            function.emit(match (double, term >= 3) {
                (true, true) => StackOp::DSubD,
                (true, false) => StackOp::DAddD,
                (false, true) => StackOp::FSubF,
                (false, false) => StackOp::FAddF,
            });
        }
    }
    function.emit(set(19));
    for (source, destination) in [(6, 15), (18, 6), (16, 17), (19, 16)] {
        function.emit(get(source));
        function.emit(set(destination));
    }
    function.emit(StackOp::LocalGet(3));
    function.emit(StackOp::LocalGet(0));
    function.emit(StackOp::I64Const(if double { 8 } else { 4 }));
    function.emit(StackOp::IMul);
    function.emit(StackOp::IAdd);
    function.emit(get(19));
    function.emit(if double {
        StackOp::StoreF64D
    } else {
        StackOp::StoreF32F
    });
}

fn emit_scalar_stream(function: &mut StackFunction, double: bool, recurrence: bool) {
    for slot in [3, 2] {
        function.emit(StackOp::LocalGet(slot));
        function.emit(StackOp::LocalGet(0));
        function.emit(StackOp::I64Const(if double { 8 } else { 4 }));
        function.emit(StackOp::IMul);
        function.emit(StackOp::IAdd);
    }
    function.emit(if double {
        StackOp::LoadF64D
    } else {
        StackOp::LoadF32F
    });
    function.emit(if double {
        StackOp::LocalGetD(4)
    } else {
        StackOp::LocalGetF(4)
    });
    function.emit(if double {
        StackOp::DMulD
    } else {
        StackOp::FMulF
    });
    if recurrence {
        function.emit(if double {
            StackOp::LocalGetD(6)
        } else {
            StackOp::LocalGetF(6)
        });
    }
    function.emit(if double {
        StackOp::LocalGetD(5)
    } else {
        StackOp::LocalGetF(5)
    });
    if recurrence {
        function.emit(if double {
            StackOp::DMulD
        } else {
            StackOp::FMulF
        });
    }
    function.emit(if double {
        StackOp::DAddD
    } else {
        StackOp::FAddF
    });
    if recurrence {
        function.emit(if double {
            StackOp::LocalTeeD(6)
        } else {
            StackOp::LocalTeeF(6)
        });
    }
    function.emit(if double {
        StackOp::StoreF64D
    } else {
        StackOp::StoreF32F
    });
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Snapshot {
    counter: i64,
    end: i64,
    state: u64,
    additional_states: [u64; 3],
    finished: u64,
    primary: Vec<u8>,
    secondary: Vec<u8>,
}

struct Probe {
    case: Case,
    frame: *mut u64,
    primary: *mut u8,
    secondary: *mut u8,
    size: usize,
    snapshots: Vec<Snapshot>,
}

impl Probe {
    unsafe fn snapshot(&self) -> Snapshot {
        Snapshot {
            counter: *self.frame as i64,
            end: *self.frame.add(1) as i64,
            state: *self.frame.add(6),
            additional_states: if self.case.biquad {
                [
                    *self.frame.add(15),
                    *self.frame.add(16),
                    *self.frame.add(17),
                ]
            } else {
                [0; 3]
            },
            finished: *self.frame.add(9),
            primary: std::slice::from_raw_parts(self.primary, self.size).to_vec(),
            secondary: std::slice::from_raw_parts(self.secondary, self.size).to_vec(),
        }
    }
}

unsafe extern "C" fn observe(data: *mut u8) -> bool {
    let probe = &mut *(data as *mut Probe);
    probe.snapshots.push(probe.snapshot());
    if probe.case.mutate {
        let width = if probe.case.double { 8 } else { 4 };
        *probe.frame -= 1;
        *probe.frame.add(1) = (LENGTH - 1) as u64;
        *probe.frame.add(2) = probe.secondary.add(1 + 2 * width) as u64;
        *probe.frame.add(3) = probe.primary.add(1 + 2 * width) as u64;
        *probe.frame.add(4) = bits(probe.case.double, 0.75);
        *probe.frame.add(5) = bits(probe.case.double, -0.25);
        *probe.frame.add(6) = bits(probe.case.double, -0.5);
        if probe.case.biquad {
            for (slot, value) in [
                (12, 0.25),
                (13, -0.5),
                (14, 0.125),
                (15, 0.125),
                (16, -0.25),
                (17, 0.5),
            ] {
                *probe.frame.add(slot) = bits(probe.case.double, value);
            }
        }
        let changed = bits(probe.case.double, 0.125).to_le_bytes();
        std::ptr::copy_nonoverlapping(changed.as_ptr(), probe.secondary.add(1 + 3 * width), width);
    }
    probe.case.cancel
}

#[derive(Debug, PartialEq, Eq)]
struct Outcome {
    result: i64,
    cancelled: bool,
    trap: u32,
    budget: i32,
    observed: Vec<Snapshot>,
    finished: Snapshot,
}

fn run(case: Case, native: bool) -> Outcome {
    let width = if case.double { 8 } else { 4 };
    let size = 1 + STORAGE * width;
    // Deliberately unaligned streams with padding for invalid-guard cases.
    let mut primary = vec![0xa5; size];
    let mut secondary = vec![0x5a; size];
    let epsilon = if case.double {
        f64::EPSILON
    } else {
        f32::EPSILON as f64
    };
    let values = [-0.0, 1.0 + epsilon, -1.0, 0.125, -0.25, 2.0];
    for index in 0..STORAGE {
        primary[1 + index * width..1 + (index + 1) * width].copy_from_slice(
            &bits(case.double, values[index % values.len()]).to_le_bytes()[..width],
        );
        secondary[1 + index * width..1 + (index + 1) * width]
            .copy_from_slice(&bits(case.double, 7.0).to_le_bytes()[..width]);
    }
    if case.edge != Edge::Mixed {
        let value = if case.edge == Edge::NegativeZero {
            -0.0
        } else {
            1.0 + epsilon
        };
        primary[1 + 2 * width..1 + 3 * width]
            .copy_from_slice(&bits(case.double, value).to_le_bytes()[..width]);
    }
    let (input, output) = if case.start >= case.end {
        (std::ptr::null(), std::ptr::null_mut())
    } else {
        let input = unsafe { primary.as_mut_ptr().add(1 + 2 * width) };
        let output = if let Some(offset) = case.overlap {
            unsafe { input.offset(offset * width as isize) }
        } else {
            unsafe { secondary.as_mut_ptr().add(1 + 2 * width) }
        };
        (input.cast_const(), output)
    };
    let program = program(case, native, input, output);
    let mut backend = StackBackend::new(&program);
    let mut probe = Probe {
        case,
        frame: backend.frame_stack.as_mut_ptr(),
        primary: primary.as_mut_ptr(),
        secondary: secondary.as_mut_ptr(),
        size,
        snapshots: Vec::new(),
    };
    if case.callback {
        backend.set_cancel_callback(Some(observe), (&mut probe as *mut Probe).cast());
    }
    let result = backend.call_entry(program.entry, [0u8].as_mut_ptr());
    let finished = unsafe { probe.snapshot() };
    Outcome {
        result,
        cancelled: backend.cancelled(),
        trap: backend.trap_reason(),
        budget: backend.ctx.cancel_counter,
        observed: probe.snapshots,
        finished,
    }
}

fn case(double: bool, recurrence: bool) -> Case {
    Case {
        double,
        recurrence,
        biquad: false,
        edge: Edge::Mixed,
        overlap: None,
        start: 0,
        end: LENGTH as i64,
        input_len: LENGTH as i64,
        output_len: LENGTH as i64,
        prefix: 0,
        callback: true,
        cancel: false,
        mutate: false,
    }
}

fn poll_cases(base: Case) -> Vec<Case> {
    let mut cases = Vec::new();
    for overlap in [None, Some(0), Some(-1), Some(1)] {
        // Full budget, expiry on final sample, penultimate sample, and first sample.
        for prefix in [0, 784, 785, 1023] {
            for (callback, cancel) in [(false, false), (true, false), (true, true)] {
                cases.push(Case {
                    overlap,
                    prefix,
                    callback,
                    cancel,
                    ..base
                });
            }
        }
    }
    cases
}

fn assert_poll_case(case: Case) {
    let ordinary = run(case, false);
    let native = run(case, true);
    assert_eq!(native, ordinary, "{case:?}");
    assert_eq!(
        native.cancelled,
        case.callback && case.cancel && case.prefix >= 784
    );
    assert_eq!(native.finished.finished, u64::from(!native.cancelled));
    assert_eq!(
        native.observed.len(),
        usize::from(case.callback && case.prefix >= 784)
    );
    if let Some(snapshot) = native.observed.first() {
        assert_eq!(snapshot.counter, 1024 - case.prefix);
    }
}

fn guard_and_mutation_cases(base: Case) -> [Case; 11] {
    [
        Case { end: 0, ..base },
        Case {
            start: 4,
            end: 0,
            ..base
        },
        Case {
            start: -1,
            end: 1,
            ..base
        },
        Case {
            end: LENGTH as i64 + 1,
            ..base
        },
        Case {
            input_len: 0,
            ..base
        },
        Case {
            output_len: -1,
            ..base
        },
        Case {
            start: 3,
            end: 239,
            ..base
        },
        Case {
            prefix: 785,
            mutate: true,
            ..base
        },
        Case {
            prefix: 1023,
            mutate: true,
            ..base
        },
        Case {
            prefix: 1023,
            mutate: true,
            overlap: Some(0),
            ..base
        },
        Case {
            prefix: 1023,
            mutate: true,
            cancel: true,
            ..base
        },
    ]
}

#[test]
fn biquad_kernels_preserve_four_states_aliasing_and_poll_boundaries() {
    for double in [false, true] {
        let base = Case {
            biquad: true,
            ..case(double, true)
        };
        for case in poll_cases(base) {
            assert_poll_case(case);
        }
        for case in guard_and_mutation_cases(base) {
            assert_eq!(run(case, true), run(case, false), "{case:?}");
        }
        for edge in [Edge::NegativeZero, Edge::SeparateRounding] {
            let case = Case {
                edge,
                end: 1,
                ..base
            };
            let native = run(case, true);
            assert_eq!(native, run(case, false), "{case:?}");
            // y1, the third carried state, holds the output after one step.
            assert_eq!(
                native.finished.additional_states[1],
                bits(
                    double,
                    if edge == Edge::NegativeZero {
                        -0.0
                    } else {
                        0.0
                    }
                )
            );
        }
    }
}

#[test]
fn native_operand_arena_survives_source_ir_drop_and_backend_move() {
    let input = [bits(false, 2.0), bits(true, 2.0)];
    let mut output = [0u64; 2];
    let backend = {
        let mut program = StackProgram::new();
        for (index, double) in [false, true].iter().copied().enumerate() {
            let case = Case {
                biquad: true,
                end: 1,
                ..case(double, true)
            };
            let mut source = self::program(
                case,
                true,
                (&input[index] as *const u64).cast(),
                (&mut output[index] as *mut u64).cast(),
            );
            program.add_function(source.functions.remove(0));
        }
        crate::native_loop_tests::forbid_fallback(&mut program);
        StackBackend::new(&program)
    };
    let mut moved = Box::new(backend);
    for (function, double) in [false, true].iter().copied().enumerate() {
        assert_eq!(
            moved.call_entry(function as u32, [0u8].as_mut_ptr()),
            bits(double, 0.28125) as i64,
        );
        assert_eq!(moved.trap_reason(), crate::cancel::TRAP_NONE);
        assert_eq!(output[function], bits(double, 0.28125));
    }
}

#[test]
fn stream_kernels_preserve_negative_zero_and_separate_multiply_rounding() {
    for double in [false, true] {
        for recurrence in [false, true] {
            for edge in [Edge::SeparateRounding, Edge::NegativeZero] {
                for overlap in [None, Some(0)] {
                    let case = Case {
                        edge,
                        overlap,
                        end: 1,
                        ..case(double, recurrence)
                    };
                    let native = run(case, true);
                    assert_eq!(native, run(case, false), "{case:?}");
                    let expected = bits(
                        double,
                        if edge == Edge::NegativeZero {
                            -0.0
                        } else {
                            0.0
                        },
                    );
                    let width = if double { 8 } else { 4 };
                    let output = if overlap.is_some() {
                        &native.finished.primary
                    } else {
                        &native.finished.secondary
                    };
                    assert_eq!(
                        &output[1 + 2 * width..1 + 3 * width],
                        &expected.to_le_bytes()[..width]
                    );
                    if recurrence {
                        assert_eq!(native.finished.state, expected);
                    }
                }
            }
        }
    }
}

#[test]
fn stream_kernels_match_240_sample_outputs_state_and_inherited_polls() {
    for double in [false, true] {
        for recurrence in [false, true] {
            for case in poll_cases(case(double, recurrence)) {
                assert_poll_case(case);
            }
        }
    }
}

#[test]
fn stream_kernel_guards_and_callback_mutations_keep_forward_loop_semantics() {
    for double in [false, true] {
        for recurrence in [false, true] {
            let base = case(double, recurrence);
            for case in guard_and_mutation_cases(base) {
                assert_eq!(run(case, true), run(case, false), "{case:?}");
            }
        }
    }
}
