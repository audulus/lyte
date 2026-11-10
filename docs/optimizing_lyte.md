# Optimization Tips for Lyte

## Contents

- [Saving CPU With Control-Rate Processing](#saving-cpu-with-control-rate-processing)
- [Why Go Through the Trouble?](#why-go-through-the-trouble)
- [Basic Pattern](#basic-pattern)
- [Terminology](#terminology)
- [Lyte Terms Used Here](#lyte-terms-used-here)
- [Clocked Sequencer Example](#example-clocked-sequencer)
- [Full Lyte Example](#full-lyte-example)
- [Vectorization and SIMD](#vectorization-and-simd)
- [Measuring Optimizations](#measuring-optimizations)
- [Increasing Resolution for More Precision](#increasing-resolution-for-more-precision)
- [Audio Outputs and Canvas Messages](#audio-outputs-and-canvas-messages)

## Saving CPU With Control-Rate Processing

This document explains how to use control-rate / event-rate processing in Lyte for work that does not need to run at audio rate. It is for readers who may understand clocks, gates, sequencers, modular synths, or music hardware, but may be new to Lyte code.

Use this method for clocking, sequencing, pattern decisions, trigger/gate generation, UI/canvas snapshots, and low-rate musical state machines. Examples include sequencer steps, clock dividers, gate logic, pattern selection, random choices, UI state, meters, and Canvas updates.

This method can drive audio-rate modules by outputting gates, triggers, CV-style control signals, or held values, but it is not itself an audio-rate synthesis method. For example, you would not use this shape for filters, oscillators, audio-rate envelopes, or any DSP where the sound itself must evolve every sample.

If you are new to the Audulus Lyte programming language, it helps to think in three timing domains:

- **Audio/sample rate:** signal processing that may run for every sample in an audio block.
- **Control/event rate:** musical or logical decisions that run only when a clock, gate, trigger, step, or other control event happens.
- **Canvas send rate:** `send()` packets used to update Canvas graphics. These are not audio buffers and should usually be throttled to visible UI changes.

## Why Go Through the Trouble?

The point of using control rate is to reduce CPU usage. Savings depend on how much work is moved and what the compiler already removes: at audio rate, a decision may run tens of thousands of times per second, while the same decision at clock or sequencer rate may run only a few times per second. Connected ports are part of the Audulus real-time signal graph, so the work done to produce those port buffers matters. Moving sequencer logic, pattern lookups, random choices, and UI decisions out of the per-sample loop reduces how often that work runs.

At 44100 Hz:

- Audio-rate code may run 44100 times per second.
- A 256-frame block runs about 172 times per second.
- A MIDI clock tick at 120 BPM and 24 PPQ happens about 48 times per second.
- A sixteenth-note sequencer step at 120 BPM happens about 8 times per second.

Running the same expensive decision once per sixteenth note requires fewer evaluations than running it every sample or block. This does not predict the total CPU saving: output writes, event detection, and downstream DSP still have costs. The audio loop still writes valid output every sample; it just writes simple held values.

## Basic Pattern

The basic control-rate pattern separates musical decisions from audio-buffer writing:

1. Detect when a control event is due. This might come from a sample counter, clock edge, gate edge, sequencer step, or UI/control change.
2. Update musical state once for that event: choose the next step, gate, pitch, pattern, random value, or display state.
3. Store the current output values in ordinary state variables.
4. Write the output ports from those stored values for every frame that needs output.

When possible, keep musical decision-making out of the sample loop. The sample loop should usually be small and predictable: copy held values to ports, apply simple gate state, or do other work that truly must happen for every frame. When an input must remain available for audio-rate modulation, start with direct per-sample processing.

This guide uses `held_*` as a naming convention for state variables. Think of a held value as "the most recent value chosen by the control-rate logic."

### Signal Flow

In an Audulus graph, signals flow from output ports to input ports. A Lyte node reads input port buffers, runs `process`, and writes output port buffers. Those buffers feed whatever is connected downstream in the Audulus audio/control graph.

`process` runs once per block. Inside each block, Lyte code often loops over every sample frame:

```lyte
for i in 0 .. frames {
    output[i] = input[i]
}
```

That sample-by-sample style is necessary for most audio processes, but a sequencer step, clock tick, pattern choice, UI update, or gate decision usually does not need to happen for every sample.

> **Tip:** A good habit is to keep the audio-rate output loop for control-rate outputs small and predictable.

### Recommended Priority

Choose the simplest method that preserves the intended contract of the port:

1. If the input is intentionally control-rate only, try a multi-block held value or block-rate held fill first
2. If the input accepts audio-rate modulation, start with direct per-sample calculations
3. If timing needs to be finer than one block but not fully sample-accurate, use sub-block passes
4. If a rare control change needs a smooth audio-rate path, use transition-only audio-rate interpolation
5. If an edge must land at an exact frame, use sample-accurate edge fill

These are choices based on behavior, not a CPU ranking. A module should not pass through every method. Start with the first method that preserves its musical interface, then add complexity only when listening, timing, or modulation requirements demand it.

## Terminology

**Audio rate** means work that happens for every audio sample. Oscillators, filters, waveshapers, audio envelopes, and feedback structures usually need audio-rate processing because their output can change every sample.

**Control rate** means work that happens less often than audio rate. In music hardware, modular synths, MIDI devices, and digital instruments, control-rate signals are commonly used for knobs, clocks, gates, triggers, sequencer steps, parameter changes, UI state, and other musical decisions.

**Event-rate** is a specific kind of control-rate processing. The code runs when a meaningful event occurs: a clock tick, sequencer step, gate edge, trigger, pattern change, or UI update. Between events, the module holds the last chosen values.

In this document, **control-rate processing** means:

1. Wait until a clock/control event is due.
2. Update musical state once.
3. Store the result in held values.
4. Copy those held values to the audio outputs until the next event changes them.

## Lyte Terms Used Here

This guide follows the Lyte DSP docs' terminology:

- `process { ... }` runs once per audio block.
- `frames` is the number of samples in the current block.
- Ports such as `input`, `output`, `pitch`, and `gate` are slices bound to Audulus engine buffers.
- A `for i in 0 .. frames` loop visits sample indices `0` through `frames - 1`.
- Global `var` state survives from one block to the next.
- `sampleRate` is supplied by Audulus.

Lyte uses **slice** as a formal language term for a view into an array, such as `[f32]`. Audulus ports are slices. This document does not use "slice" to mean part of an audio block. For subdivisions of a block, it uses **frame range** or **sub-block pass**.

Use `port[i]` for normal sample-by-sample input. Use `port[0]` only when you intentionally want one control value for the whole block, and guard against an empty block before accessing it.

## Example: Clocked Sequencer

This sequencer uses a MIDI-style clock resolution: 24 PPQ, meaning 24 pulses per quarter note.

At 120 BPM and 44100 Hz:

```text
samples per tick = (44100 * 60 / 120) / 24 = 918.75 samples, about 20.8 ms
sixteenth note   = 6 ticks                  = 5512.5 samples, exactly 125 ms
eighth note      = 12 ticks
quarter note     = 24 ticks
```

It outputs:

- `pitch`: a held pitch in Hz
- `gate`: a held gate, where `0.0` is low/off and `1.0` is high/on

To use it in Audulus, add output ports named `pitch` and `gate`. To change tempo, subdivision, gate length, or pitches, edit `init` and reload the node.

## Full Lyte Example

This version checks tick timing every sample but updates sequencer state only on ticks. It writes each sample after handling any tick due at that frame, so mid-block transitions are preserved. The first step starts immediately at C4 with its gate high.

The fractional countdown carries the remainder into the next tick rather than rounding every tick to a fixed integer length. Edges land on the first sample at or after the scheduled time. This avoids systematic tempo error from integer-rounded tick lengths; it is not a host-synchronized transport or an exact long-term clock.

```lyte
var held_pitch: f32
var held_gate: f32
var tick_len: f32
var tick_remaining: f32
var tick_in_step: i32
var gate_ticks_left: i32
var seq_step: i32

init {
    let bpm = 120.0
    let ppq = 24.0
    tick_len = sampleRate * 60.0 / bpm / ppq
    // This example supports no more than one tick per sample
    if tick_len < 1.0 { tick_len = 1.0 }
    tick_remaining = tick_len
    tick_in_step = 0
    gate_ticks_left = 3
    seq_step = 0
    held_pitch = 261.63
    held_gate = 1.0
}

process {
    for i in 0 .. frames {
        if tick_remaining <= 0.0 {
            tick_remaining = tick_remaining + tick_len

            if gate_ticks_left > 0 {
                gate_ticks_left = gate_ticks_left - 1
                if gate_ticks_left == 0 { held_gate = 0.0 }
            }

            tick_in_step = tick_in_step + 1
            if tick_in_step >= 6 {
                tick_in_step = 0
                seq_step = seq_step + 1
                if seq_step >= 4 { seq_step = 0 }
                if seq_step == 0 { held_pitch = 261.63 }
                if seq_step == 1 { held_pitch = 329.63 }
                if seq_step == 2 { held_pitch = 392.00 }
                if seq_step == 3 { held_pitch = 523.25 }
                held_gate = 1.0
                gate_ticks_left = 3
            }
        }

        pitch[i] = held_pitch
        gate[i] = held_gate
        tick_remaining = tick_remaining - 1.0
    }
}
```

This deliberately favors clear timing over minimum scheduler overhead. An event-offset implementation can fill held ranges between ticks instead, preserving the same timing with fewer scheduler checks. A block-held version is also valid when block timing is intentional: write the current state for the block before advancing the scheduler for the next block. Transitions inside the current block then become visible at the next block boundary, and short gates can be lost.

## Vectorization and SIMD

SIMD performs the same operation on multiple values at once. Lyte has a built-in four-lane float type, `f32x4`, alongside scalar `f32`. An ordinary `[f32; 4]` array or a custom vector struct does not by itself request SIMD arithmetic.

```lyte
let signals = f32x4(0.1, 0.2, 0.3, 0.4)
let gain = f32x4_splat(0.5)
let scaled = signals * gain
let first = scaled.x
```

`f32x4(a, b, c, d)` constructs four lanes; `f32x4_splat(x)` repeats one value. Arithmetic such as `+`, `-`, `*`, and `/` operates lane by lane on two vectors; unary negation is supported. Fields `.x`, `.y`, `.z`, and `.w` access individual lanes. Do not assume that every scalar math function has a vector overload.

There are three relevant approaches:

| Approach | Suitable work | Limits |
| --- | --- | --- |
| Explicit SIMD across samples | Independent gain, mixing, or memoryless arithmetic on four adjacent samples | Handle remaining frames; packing and unpacking have costs |
| Explicit SIMD across independent streams | Four voices, channels, oscillators, or parallel filter bands, each with its own lane state | Different modes or branches per lane complicate the design |
| Compiler auto-vectorization | Straightforward scalar loops with independent iterations | LLVM may vectorize legal, profitable loops; source shape alone does not guarantee it |

A recursive filter cannot generally process four consecutive samples as independent lanes: each sample needs the previous sample's state. Four independent filters can instead keep their separate states in four lanes and advance together once per frame. A chain of serial filters is also not four independent filters.

Lyte source reviewed at commit `1c7294e` (October 2026) implements native vector arithmetic in Cranelift and LLVM, and vector handlers in the Stack VM. This does not establish which implementation ships in a particular Audulus release. Check the selected release and execution backend before relying on a feature or performance result. LLVM's optimization pipeline can also optimize scalar code; other backends need separate measurement.

Four lanes do not promise four times the speed. Lane construction/extraction, memory traffic, divergent control flow, scalar math calls, and unused lanes can consume the saving. Compare scalar code and explicit SIMD only where each preserves the same behavior. No speedup is established by the examples here; they illustrate supported operations.

## Measuring Optimizations

Start with clear code and preserve the port's timing and modulation contract. Move genuinely invariant work into `init` or outside the sample loop when its dependencies permit it. Current Lyte includes invariant field-read hoisting; LLVM JIT/AOT uses an O3 optimization pipeline.

For an optimization comparison:

1. Use the same patch, sample rate, block size, connected outputs, and execution backend
2. Compare a direct baseline with one changed strategy at a time
3. Exercise static controls, occasional changes, smoothed controls, and audio-rate modulation
4. Confirm equivalent output and state behavior, including the first sample and block boundaries
5. Repeat short measurements enough to distinguish a consistent difference from CPU-meter noise; also check transition peaks

An isolated hot-loop benchmark helps identify processing costs; whole-patch measurements show whether those costs matter in use. Record the Lyte release/backend with results. Publish optimization recommendations only when representative measurements support them. Little or no improvement is a reason to prefer the simpler implementation.

Implementation references in the Lyte repository: `src/hoist.rs` (field-read hoisting), `src/llvm_jit.rs` and `src/llvm_aot.rs` (LLVM optimization), `src/jit.rs` (Cranelift vectors), and `src/stack_interp.c` (Stack VM vectors). These describe the reviewed source, not a performance guarantee for a shipped Audulus version.

## Increasing Resolution for More Precision

Increasing resolution means choosing smaller time units for control-rate changes. Smaller time units can place changes closer to their intended moment. Scheduler checks may become more frequent, while the total number of output samples written remains the same.

**Block-rate held output** fills the block using state available at its start. Events scheduled inside the block are deferred to the next block boundary. Do not advance through future events and then use their final state to fill the current block: that applies changes early and collapses intermediate transitions. This is a simple block-local version of the method, but output changes can only happen at block boundaries. Block timing may suit UI signals and slow controls. For sequencers and gates, judge the timing error at the actual sample rate and block size; pulses shorter than a block can disappear. If tighter timing or smoother transitions are needed, there are several higher-resolution options.

> From Lua to Lyte: this block-rate scheduling method is the Lyte equivalent of the Audulus Lua DSP `fill(port, value)` pattern. Lua `fill()` writes one value across the current output buffer. It avoids repeating the decision that chose the value, but the buffer still needs to be written. Lyte expands the concept: the simple version fills the whole block with one held value, while more advanced versions can write smaller frame ranges inside the block when higher timing resolution is needed.

Timing and update methods differ in active cost, timing resolution, and duty cycle:

```text
Multi-block held value:
  lowest decision rate, output stays the same across many blocks

Block-rate held fill:
  one held value per block, block-level timing resolution

Sub-block passes:
  adjustable timing resolution; measure scheduler and loop overhead

Transition-only audio-rate interpolation:
  held-value path while idle, sample-rate smoothing while moving

Sample-accurate edge fill:
  highest edge precision, cost depends on how often events occur
```

A held value can also span many blocks. For example, a sequencer pitch might change once per sixteenth note, while dozens of audio blocks pass in between. The output port still needs valid samples every block, but the musical decision does not need to be recalculated until the next event.

### Option 1: Sub-Block Passes

Sub-block processing divides the current `frames` block into a small number of contiguous frame ranges and runs the scheduler once per range. The output is still held inside each range, but a gate edge can land closer to its intended time.

In Lyte code, this is still ordinary indexed writes to output port slices. Instead of one loop over `0 .. frames`, use several loops with explicit start and end indices inside the same block.

For a 256-frame block:

```text
1 pass   = full block      = edge error up to 256 frames
2 passes = half blocks     = edge error up to 128 frames
4 passes = quarter blocks  = edge error up to 64 frames
8 passes = eighth blocks   = edge error up to 32 frames
```

Sub-block passes increase scheduler invocation counts (two passes invoke it twice per block, four passes four times). They do not imply proportional total CPU cost: output writes still cover the same number of frames, event work depends on event count, and compiler optimization can differ between loop shapes.

Conceptually:

```text
for each frame range inside 0 .. frames:
  apply events due at the range start
  fill that frame range with held_*
  advance scheduler by that range length for the next range
```

When this is factored into helper functions that write to output buffers by index, use Lyte `require` clauses or nearby guards when the safety checker needs proof that the index is inside the port slice.

### Option 2: Transition-Only Audio-Rate Interpolation

Some processes spend most of their time holding a value and need sample-rate behavior only for a short transition. Mute and solo de-click ramps, parameter smoothing, bypass fades, and mode crossfades are common examples.

Use **event-activated audio-rate interpolation** for this shape. The control logic remains block-rate or event-rate while idle. An event starts a short linear ramp, the ramp advances once per sample until it reaches its target, and the process immediately returns to its held-value path.

Conceptually:

```text
idle:
  fill output with held value

on control event:
  choose target
  calculate ramp length and per-sample increment
  mark ramp active

while ramp is active:
  advance the value once per sample
  write the changing value to output
  stop exactly at the target

idle again:
  fill output with the new held value
```

This is sometimes called **transition-only audio-rate smoothing**, **conditional audio-rate processing**, or simply a **linear parameter ramp**. The important property is its duty cycle: sample-rate arithmetic runs only while the value is moving.

This method and sub-block passes solve different problems:

- Sub-block passes create a continuously available timing grid finer than the host block
- Transition-only interpolation creates a smooth sample-rate path for rare, finite movements
- Sub-block passes suit recurring clocks, gates, and scheduled events
- Transition-only interpolation suits de-clicking, smoothing, and short crossfades

The ramp provides smooth transitions. Its CPU cost depends on the implementation and how often it runs; no performance comparison with sub-block processing is established here.

If a transition begins at a known offset inside the current block, start the ramp at that offset. If the event is only detected from `port[0]`, the ramp still starts on a block boundary, but its movement after that boundary is sample-rate smooth.

### Option 3: Sample-Accurate Edge Fill

For exact mid-block edges, calculate the event offset inside the block and split the output fill around that offset.

Conceptually:

```text
fill from block start to event offset with old held_*
update held_* at the event
fill from event offset to the next event or block end with new held_*
```

This preserves exact frame placement. With sparse events it can also avoid per-sample scheduler checks; extra control flow and short fill ranges can offset that benefit. Choose it for timing requirements and measure its cost.

### Practical Rule

Use the cheapest processing method that sounds and behaves correctly:

```text
Multi-block held value:
  least frequent decision-making, useful for slow clocks and long-held states

Block-rate held fill:
  one held value per block, edges quantized to the full block

Sub-block passes:
  more scheduler checks as pass count rises, edges quantized to a block subdivision

Transition-only audio-rate interpolation:
  sample-rate smoothing during active ramps, held-value cost while idle

Sample-accurate edge fill:
  most precise, most useful when exact edge placement matters
```

The principle is the same in all cases: avoid repeating work that does not need repeating. Keep musical decisions out of the per-sample loop when their timing permits it. Keep audio-rate inputs available per sample.

## Audio Outputs and Canvas Messages

Keep audio/control outputs separate from canvas messaging.

Audio ports are slices, so every frame that should produce output must be written. Canvas `send()` packets are different: they are messages strictly for Canvas node vector graphics, not audio buffers. They should describe only visible UI state and should be emitted only when that state changes.

For a sequencer with canvas output, compare a compact canvas snapshot against the last sent snapshot before calling `send()`.

*Technical note:* Lyte DSP code runs at audio block rate, while a Canvas node redraws at the graphics frame rate. These are different clocks. Building repeated snapshots can waste DSP work even if the receiver coalesces messages. Do not assume that every intermediate packet will be displayed; transport and delivery behavior depend on the host.

For visual feedback, throttle the send rate to what the display can use. A sequencer usually only needs to send when the visible state changes: step index, active gates, fill state, pattern, selection, or meter state. If a control-rate event happens faster than the graphics frame rate, consider sending the newest compact snapshot and dropping intermediate visual states.

Packet size matters too. A `send()` packet can be small, such as a step number and a few gate bits, or large, such as an audio waveform, oscilloscope trace, spectrum data, or a full grid state. Large packets cost more to build in DSP code, more to transfer, more to parse in Canvas Lua, and more to draw. For waveform-style displays, send only the resolution the graphic can actually show, and avoid sending a new large packet when the previous one is still visually current.
