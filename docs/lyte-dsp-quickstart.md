# Lyte DSP Quick Start

*A practical starting guide for building audio tools in Audulus Lyte. The examples are small, easy to paste in, and meant to be changed.*

---

## The DSP Node

When you create a new Lyte DSP node in Audulus, it starts with this template:

```lyte
// Inputs, outputs, `frames`, `sampleRate`, and `storage` are globals.
// Inputs, outputs, and `storage` are slices bound to the engine buffers.
// `storage` is persistent host storage, not the normal place for large DSP buffers.
// MAX_FRAMES is the max processing frames (used to declare temporary arrays).

init {}

process {
    for i in 0 .. frames {
        output[i] = input[i]
    }
}
```

That is the basic shape of a Lyte DSP node. Everything below builds on that pattern.

## Terms for New Users

These are the main words you will see right away:

- `input`, `output`: the audio coming in and going out for the current block. `input[i]` means “sample number `i`”
- `frames`: how many samples are in this block
- `sampleRate`: how many samples happen each second, usually `44100.0` or `48000.0`
- `storage`: persistent host storage provided by Audulus. Use it for node-owned values that should live outside the audio block, not as the default location for large delay lines or scratch buffers
- `globals`: variables declared outside `process`. They keep their value from one block to the next
- `slice`: a buffer you read with `[...]`
- `MAX_FRAMES`: the biggest block size Audulus may send to the node. Use it when you need a temporary array

You do not create `frames`, `sampleRate`, or `storage` yourself. Audulus gives them to the node automatically. For large DSP buffers, declare a dedicated global array with the size you need.

| Global | Type | Description |
|---|---|---|
| `input`, `output` | `[f32]` | Default port slices. Add ports as needed above the code viewer in the Inspector. |
| `frames` | `i32` | Number of samples in this processing block. |
| `sampleRate` | `f32` | Current sample rate, e.g. `44100.0`. |
| `storage` | `[f32]` | Persistent host storage supplied by Audulus. Use dedicated global arrays for large DSP buffers such as delay lines. |
| `MAX_FRAMES` | `i32` | Maximum possible block size — use to declare stack arrays. |

**`init {}`** runs once when the node loads. Use it for setup values.

**`process { ... }`** runs once per block. Most audio work happens here inside `for i in 0 .. frames`.

**Global `var`** is for state that should survive between blocks. Globals start at zero.

**Indexing starts at zero.** In `for i in 0 .. frames`, valid indices are `0` through
`frames - 1`. Use `freq[i]` for normal sample-by-sample processing. Use `freq[0]` only when you
intentionally want one control value for the whole block. If you are coming from Lua, this is different because Lua starts at `1`.

---

## Using `sampleRate`

`sampleRate` is a float. A common pattern is to compute `1.0 / sampleRate` once in
`init` and then multiply by that inside `process`.

```lyte
var inv_sr: f32

init {
    inv_sr = 1.0 / sampleRate
}
```

This also avoids doing a division on every sample.

---

## Do's and Don'ts

These are good default habits for writing Lyte in Audulus.

**Do:**
- Prefer `f32` almost everywhere in DSP code.
- Treat `sampleRate` like a normal float in math.
- Start from the fresh-node block template and change one thing at a time when debugging.
- Use `sin`, `cos`, `tan`, and other unsuffixed math builtins.
- Read a port sample into a scratch `f32` first if the compiler gets ambiguous about expressions like `input[i] * inv_sr`.
- Keep slice index proofs inline near buffer accesses when the safety checker complains.
- Use `require` on slice helpers when the caller can prove the index is valid.
- Use `freq[i]`, `cutoff[i]`, and similar forms for normal per-sample processing. Use `freq[0]` only when you intentionally want one control value for the whole block.
- Move expensive math out of the sample loop when true audio-rate updates are not needed.
- Try block-rate control first when it sounds good enough. It is usually simpler and cheaper.

**Don't:**
- Don't assume every Expr-node feature maps over exactly. Lyte does have built-in trig/math functions and stdlib helpers like `clamp(x, lo, hi)` and `mix(a, b, t)`, but constants like `pi` are still not built in.
- Don't use `sinf`, `cosf`, or other suffixed math names in Audulus Lyte. Use unsuffixed names like `sin`, `cos`, `tan`, `atan2`, `sqrt`, `clamp`, and `mix`.
- Don't rely on helper functions to prove slice indices by context alone. Use inline guards, or add explicit `require` clauses to the helper.
- Don't use `assume` in node code — it is only allowed in the standard library or prelude.
- Don't assume examples from the standalone Lyte repo will drop into Audulus unchanged.
- Don't rely on block-form inline `if` expressions in assignments such as `let x = if ...` or `output[i] = if ...`. In Lyte those can trigger confusing parser errors. Use a normal assignment first, then a plain `if` block.

---

## Slice Helper Preconditions

Audulus beta 983 and newer Lyte builds support `require` clauses on helper functions. Use them when a helper indexes into a slice and every caller must prove the index is valid.

```lyte
set(arr: [i32], idx: i32, value: i32) require idx >= 0 require idx < arr.len {
    arr[idx] = value
}
```

The `require` clauses do two jobs. Inside the helper, they let the safety checker treat `idx` as a valid index for `arr`. At each call site, the checker makes sure the caller has already proved those conditions.

You can also combine the bounds into one clause:

```lyte
set(arr: [i32], idx: i32, value: i32) require idx >= 0 && idx < arr.len {
    arr[idx] = value
}
```

If the caller cannot prove a clause, Lyte reports that it `couldn't prove require clause` for that call. Add a nearby guard such as `if idx >= 0 && idx < arr.len { ... }`, or restructure the loop so the range proves the bound.

The release-note form places `require` immediately after the parameter list. Until a return-valued helper form is confirmed for the Audulus build you are targeting, keep `require` examples to setter-style helpers like `set` and `write_delay`, and keep reads guarded inline.

---

## Debug Printing

`println` is useful for simple debugging. For numbers, write into a
small text buffer first with `itoa` or `ftoa`, then print the buffer.

### Hello World

```lyte
init {
    println("hello world")
}

process {
    for i in 0 .. frames {
        output[i] = input[i]
    }
}
```

### Block Counter

This prints how many times `process` has run so far.

```lyte
var calls: i32

init {}

process {
    var buf: [i8; 32]
    itoa(buf, calls)
    println(buf)
    calls = calls + 1

    for i in 0 .. frames {
        output[i] = input[i]
    }
}
```

Use debug printing sparingly. Printing every block gets noisy fast.

---

## Start Here

### Gain

*Requires a `gain` input port added in the Inspector.*

```lyte
init {}

process {
    for i in 0 .. frames {
        output[i] = input[i] * gain[i]
    }
}
```

### Sine Oscillator — Input-Driven

*`input` supplies frequency in Hz.*

```lyte
var phase: f32
var inv_sr: f32
var hz: f32

init {
    inv_sr = 1.0 / sampleRate
}

process {
    for i in 0 .. frames {
        hz = input[i]
        output[i] = sin(phase * 6.28318530)
        phase = phase + hz * inv_sr
        if phase >= 1.0 {
            phase = phase - 1.0
        }
    }
}
```

`phase` remembers where the oscillator is in its cycle. `hz` is just a temporary variable that makes the math clearer and avoids compiler confusion around `input[i]`.

---

## A Note on State

Anything that should survive from one block to the next must be a global `var`. Globals
start at zero, so you usually do not need `= 0.0`.

```lyte
var phase: f32
var prev: f32
var inv_sr: f32

init {
    inv_sr = 1.0 / sampleRate
}

process {
    for i in 0 .. frames {
        phase = phase + 0.01
        prev = input[i]
        output[i] = sin(phase)
    }
}
```

Use `let` for a value you set once and do not change. Use `var` for a value that changes.

---
## 1. Timer

Outputs elapsed time in seconds. A `reset` input above `0.0` restarts the count.

*Ports: `reset` in, `output` out.*

```lyte
var time: f32
var inv_sr: f32
var prev_reset: f32

init {
    inv_sr = 1.0 / sampleRate
}

process {
    for i in 0 .. frames {
        let r = reset[i]
        if r > 0.0 && prev_reset <= 0.0 {
            time = 0.0
        }
        prev_reset = r
        output[i] = time
        time = time + inv_sr
    }
}
```

`reset` is a rising-edge trigger, so the timer restarts only when the signal goes from
low to high. The output is written before `time` advances, so the first sample is `0.0`.

---

## 2. Phasor

A ramp that rises from `0.0` to `1.0` at a given frequency, then wraps. The foundation
of most oscillators.

*Ports: `freq` in, `sync` in, `output` out.*

```lyte
var phase: f32
var inv_sr: f32
var hz: f32
var prev_sync: f32

init {
    inv_sr = 1.0 / sampleRate
}

process {
    for i in 0 .. frames {
        let s = sync[i]
        if s > 0.0 && prev_sync <= 0.0 {
            phase = 0.0
        }
        prev_sync = s
        output[i] = phase
        hz = freq[i]
        phase = phase + hz * inv_sr
        if phase >= 1.0 {
            phase = phase - 1.0
        }
    }
}
```

`sync` is a rising-edge trigger. `hz * inv_sr` is the fraction of one cycle that passes
per sample.

Note for Audulus users: the built-in Audulus Phasor outputs `0` to `2π`. This example uses `0` to
`1`, which is often easier to work with in Lyte code.

### Clock

A clock is just a phasor turned into a square wave.

*Ports: `freq` in, `sync` in, `output` out.*

```lyte
var phase: f32
var inv_sr: f32
var hz: f32
var prev_sync: f32

init {
    inv_sr = 1.0 / sampleRate
}

process {
    for i in 0 .. frames {
        let s = sync[i]
        if s > 0.0 && prev_sync <= 0.0 {
            phase = 0.0
        }
        prev_sync = s

        if phase < 0.5 { output[i] = 1.0 } else { output[i] = 0.0 }

        hz = freq[i]
        phase = phase + hz * inv_sr
        if phase >= 1.0 {
            phase = phase - 1.0
        }
    }
}
```

This is the same phasor idea, just turned into a high/low pulse.

### Counter

Counts rising edges from a clock input.

*Ports: `clock`, `reset`, `cycle`, `hi_bound` in, `output` out.*

```lyte
var count: f32
var prev_clock: f32
var prev_reset: f32

init {}

process {
    for i in 0 .. frames {
        let r = reset[i]
        if r > 0.0 && prev_reset <= 0.0 {
            count = 0.0
        }
        prev_reset = r

        let c = clock[i]
        if c > 0.0 && prev_clock <= 0.0 {
            count = count + 1.0
            if cycle[i] > 0.0 {
                if count >= hi_bound[i] {
                    count = 0.0
                }
            }
        }
        prev_clock = c

        output[i] = count
    }
}
```

`hi_bound` is exclusive, so if `hi_bound` is `4.0` the counter steps through `0, 1, 2, 3`.
When `cycle` is low, the counter keeps going up without wrapping.

---

## 3. Sine Oscillator

Builds on the phasor: convert the 0–1 phase to 0–2π radians for `sin`.

*Ports: `freq` in, `sync` in, `output` out.*

```lyte
var phase: f32
var inv_sr: f32
var hz: f32

init {
    inv_sr = 1.0 / sampleRate
}

process {
    for i in 0 .. frames {
        if sync[i] > 0.0 {
            phase = 0.0
        }
        hz = freq[i]
        output[i] = sin(phase * 6.28318530)
        phase = phase + hz * inv_sr
        if phase >= 1.0 {
            phase = phase - 1.0
        }
    }
}
```

**Other waveshapes** — replace the `output[i]` line:

```lyte
// sawtooth: scale 0..1 → -1..+1
output[i] = phase * 2.0 - 1.0

// triangle: fold the sawtooth
output[i] = abs(phase * 2.0 - 1.0) * 2.0 - 1.0

// square
var pw: f32   // global pulse width; 0.5 = 50% duty cycle
output[i] = -1.0
if phase < pw {
    output[i] = 1.0
}
```

This plain assignment style is a safe habit in Lyte and avoids parser trouble from inline `if` expressions.

---
## 4. Sample and Hold

Captures and holds the input signal on the rising edge of a trigger.

*Ports: `input` in, `trig` in, `output` out.*

```lyte
var held: f32
var prev_trig: f32

init {}

process {
    for i in 0 .. frames {
        let t = trig[i]
        if t > 0.0 && prev_trig <= 0.0 {
            held = input[i]
        }
        prev_trig = t
        output[i] = held
    }
}
```

The edge check fires only when the trigger goes low to high, not while it stays high.

---

## 5. Random (0 to 1)

Outputs a new pseudo-random value on each rising trigger edge.

*Ports: `trig` in, `output` out.*

```lyte
var held: f32
var prev_trig: f32
var seed: f32

next_rand(s: f32) -> f32 {
    let x = sin(s * 12.9898 + 78.233) * 43758.5453
    x - floor(x)
}

init {
    seed = 0.1234567
}

process {
    for i in 0 .. frames {
        let t = trig[i]
        if t > 0.0 && prev_trig <= 0.0 {
            seed = next_rand(seed)
            held = seed
        }
        prev_trig = t
        output[i] = held
    }
}
```

`next_rand` is just a small helper function. The sequence is deterministic, so changing
the starting `seed` gives you a different repeatable sequence.

---

## 6. Filters

Filters work by remembering old samples and mixing them into the current one. In DSP
notation that one-sample memory is written as **z⁻¹**.

> **Unit delay in Audulus vs. Lyte:** In an Audulus graph, feedback delay is handled for
> you. In Lyte, you store that state yourself in globals like `y1`, `x1`, or `s`.

### One-Pole Low Pass

One multiply, one add per sample. Smooth and efficient.

*Ports: `input` in, `cutoff` in, `output` out.*

```lyte
var y1: f32
var inv_sr: f32

init {
    inv_sr = 1.0 / sampleRate
}

process {
    for i in 0 .. frames {
        let x = input[i]
        let fc = cutoff[i]
        let coeff = 1.0 - exp(0.0 - 6.28318530 * fc * inv_sr)
        y1 = y1 + coeff * (x - y1)
        output[i] = y1
    }
}
```

Smaller `coeff` means heavier smoothing.

---
### ZDF One-Pole Low Pass

Zero-delay feedback: solves for the output algebraically within the same sample, keeping
the cutoff accurate near Nyquist.

*Ports: `input` in, `cutoff` in, `output` out.*

```lyte
var s: f32
var inv_sr: f32

init {
    inv_sr = 1.0 / sampleRate
}

process {
    for i in 0 .. frames {
        let x   = input[i]
        let fc  = cutoff[i]
        let g   = tan(3.14159265 * fc * inv_sr)
        let k   = 1.0 / (1.0 + g)
        let v   = (x - s) * g * k
        let out = v + s
        s = out + v
        output[i] = out
    }
}
```

`s` is the only stored state here.

### Biquad Low Pass

A second-order (2-pole) filter: 12 dB/oct slope. Stores four z⁻¹ elements — two past
inputs, two past outputs.

*Ports: `input` in, `cutoff` in, `q` in, `output` out.*

```lyte
var inv_sr: f32
var x1: f32
var x2: f32
var y1: f32
var y2: f32

init {
    inv_sr = 1.0 / sampleRate
}

process {
    for i in 0 .. frames {
        let x   = input[i]
        let fc  = cutoff[i]
        var res = q[i]
        if res < 0.001 {
            res = 0.001
        }
        let w0    = 2.0 * 3.14159265 * fc * inv_sr
        let alpha = sin(w0) / (2.0 * res)
        let cs    = cos(w0)
        let a0    = 1.0 + alpha
        let inv   = 1.0 / a0
        let b1    = (1.0 - cs) * inv
        let b0    = b1 / 2.0
        let b2    = b0
        let a1    = (0.0 - 2.0 * cs) * inv
        let a2    = (1.0 - alpha) * inv

        let y = b0 * x + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2
        x2 = x1
        x1 = x
        y2 = y1
        y1 = y
        output[i] = y
    }
}
```

This is direct form I using plain `f32` globals for the saved state. This flatter version
is the one used in this guide.

### DC Blocker

Removes constant offset from a signal. Two z⁻¹ elements: one on input, one on output.

*Ports: `input` in, `output` out.*

```lyte
var x1: f32
var y1: f32

init {}

process {
    for i in 0 .. frames {
        let x   = input[i]
        let out = x - x1 + 0.995 * y1
        x1 = x
        y1 = out
        output[i] = out
    }
}
```

---
## 7. Delay Line

A delay line stores past samples in a dedicated memory block and reads them back later.
This is the core of echo, comb filters, chorus, flanging, and many reverb designs.

`storage` is persistent Audulus node storage, but it is not the preferred place for a
large delay buffer. For delay lines, declare a global array with the length you need.
This example dedicates 65536 samples of memory to the delay line.

This is different from unit delay. Unit delay is always one sample. A delay line is a
larger buffer measured in samples, milliseconds, or seconds.

*Ports: `input`, `secs` in, `output` out.*

```lyte
var delay_mem: [f32; 65536]
var write: i32

lerp(a: f32, b: f32, t: f32) -> f32 {
    a + (b - a) * t
}

write_delay(buffer: [f32], idx: i32, value: f32) require idx >= 0 require idx < buffer.len {
    buffer[idx] = value
}

init {}

process {
    for i in 0 .. frames {
        let x = input[i]
        let len = delay_mem.len

        var delay = secs[i] * sampleRate
        if delay < 0.0 {
            delay = 0.0
        }
        if delay > (len - 2) as f32 {
            delay = (len - 2) as f32
        }

        if write < 0 {
            write = 0
        }
        if write >= len {
            write = 0
        }

        if write >= 0 && write < len {
            write_delay(delay_mem, write, x)
        }

        let read_pos_0 = write as f32 - delay
        var read_pos = read_pos_0
        if read_pos < 0.0 {
            read_pos = read_pos + len as f32
        }

        let i0 = floor(read_pos) as i32
        let frac = read_pos - (i0 as f32)

        var i1 = i0 + 1
        if i1 >= len {
            i1 = 0
        }

        if i0 >= 0 && i0 < len {
            if i1 >= 0 && i1 < len {
                let a = delay_mem[i0]
                let b = delay_mem[i1]
                output[i] = lerp(a, b, frac)
            } else {
                output[i] = 0.0
            }
        } else {
            output[i] = 0.0
        }

        write = write + 1
        if write >= len {
            write = 0
        }
    }
}
```

This is a ring buffer. The write head moves forward one sample at a time and wraps to the
start. The read position trails behind by the amount set by `secs`, and the output is
linearly interpolated for fractional delays.

The `write_delay` helper uses `require` clauses so the checker knows its index is valid
inside the helper body. The call site still keeps a nearby guard, which proves the
`require` clauses before the helper call. The reads stay inline under explicit guards.

This assumes `delay_mem` is longer than the maximum delay you want. At 48 kHz, 65536
samples is about 1.36 seconds.

---
## 8. ADSR Envelope

Attack, Decay, Sustain, Release — the standard four-stage amplitude envelope.

*Ports: `gate`, `attack`, `decay`, `sustain`, `release` in, `output` out.*

```lyte
const IDLE = 0
const ATTACK = 1
const DECAY = 2
const SUSTAIN = 3
const RELEASE = 4

var stage: i32
var level: f32
var velocity: f32
var prev_gate: f32
var inv_sr: f32

init {
    inv_sr = 1.0 / sampleRate
}

process {
    for i in 0 .. frames {
        let g = gate[i]
        var a = attack[i]
        var d = decay[i]
        let s = sustain[i]
        var r = release[i]
        let min_time = inv_sr
        if a < min_time { a = min_time }
        if d < min_time { d = min_time }
        if r < min_time { r = min_time }

        if g > 0.0 && prev_gate <= 0.0 {
            velocity = g
            stage = ATTACK
        }

        if g <= 0.0 && prev_gate > 0.0 {
            stage = RELEASE
        }

        prev_gate = g

        if stage == ATTACK {
            level = level + (1.0 / (a * sampleRate))
            if level >= 1.0 {
                level = 1.0
                stage = DECAY
            }
        }

        if stage == DECAY {
            level = level - ((1.0 - s) / (d * sampleRate))
            if level <= s {
                level = s
                stage = SUSTAIN
            }
        }

        if stage == RELEASE {
            level = level - (level / (r * sampleRate))
            if level < 0.0001 {
                level = 0.0
                stage = IDLE
            }
        }

        output[i] = level * velocity
    }
}
```

Attack rises toward `1.0`, decay falls to `sustain`, sustain holds while the gate stays
high, and release falls back toward zero. The gate value is captured on the rising edge
and used as velocity.

---
## 9. Zero Crossing Detector (outputs Hz)

Counts how often the input signal crosses zero from negative to positive, and outputs the
frequency of those crossings in Hz.

*Ports: `input` in, `output` out.*

```lyte
var prev_sample: f32
var count: f32
var timer: f32
var hz_out: f32

init {}

process {
    let window = sampleRate

    for i in 0 .. frames {
        let x = input[i]

        if prev_sample < 0.0 && x >= 0.0 {
            count = count + 1.0
        }

        prev_sample = x
        timer = timer + 1.0

        if timer >= window {
            hz_out = count * (sampleRate / window)
            count = 0.0
            timer = 0.0
        }

        output[i] = hz_out
    }
}
```

Rising zero crossings per second equals the frequency of a periodic signal. The
measurement window trades off smoothness against response time.

---
## 10. Mux / Demux

A mux selects one of several inputs based on a control signal. A demux routes one input
to one of several outputs.

### Mux (many inputs → one output)

*Ports: `in0`, `in1`, `in2`, `in3`, `sel` in, `output` out.*

```lyte
init {}

process {
    for i in 0 .. frames {
        var index = floor(sel[i] * 4.0) as i32

        if index < 0 { index = 0 }
        if index > 3 { index = 3 }

        if index == 0 { output[i] = in0[i] }
        if index == 1 { output[i] = in1[i] }
        if index == 2 { output[i] = in2[i] }
        if index == 3 { output[i] = in3[i] }
    }
}
```

### Demux (one input → one of several outputs, rest zeroed)

*Ports: `input`, `sel` in, `out0`, `out1`, `out2`, `out3` out.*

```lyte
init {}

process {
    for i in 0 .. frames {
        var index = floor(sel[i] * 4.0) as i32

        if index < 0 { index = 0 }
        if index > 3 { index = 3 }

        if index == 0 { out0[i] = input[i] } else { out0[i] = 0.0 }
        if index == 1 { out1[i] = input[i] } else { out1[i] = 0.0 }
        if index == 2 { out2[i] = input[i] } else { out2[i] = 0.0 }
        if index == 3 { out3[i] = input[i] } else { out3[i] = 0.0 }
    }
}
```

The `sel` input is multiplied by the channel count and floored to get an integer index.
The mux picks one input. The demux sends the input to one output and zeros the others.

---

## 11. Four-Step Sequencer

A very simple sequencer is just a counter driving a mux.

*Ports: `clock`, `reset`, `steps`, `step0`, `step1`, `step2`, `step3` in, `output` out.*

```lyte
var index: f32
var prev_clock: f32
var prev_reset: f32

init {}

process {
    for i in 0 .. frames {
        var limit = steps[i]
        if limit < 1.0 { limit = 1.0 }
        if limit > 4.0 { limit = 4.0 }

        let r = reset[i]
        if r > 0.0 && prev_reset <= 0.0 {
            index = 0.0
        }
        prev_reset = r

        let c = clock[i]
        if c > 0.0 && prev_clock <= 0.0 {
            index = index + 1.0
            if index >= limit {
                index = 0.0
            }
        }
        prev_clock = c

        output[i] = step0[i]
        if index >= 0.5 { output[i] = step1[i] }
        if index >= 1.5 { output[i] = step2[i] }
        if index >= 2.5 { output[i] = step3[i] }
    }
}
```

`steps` is the exclusive upper bound for the counter, clamped here to `1` through `4`.
The last four `if` lines are the mux part.

---

*These examples are intended as starting points. DSP is iterative — adjust, listen, compare.*

*For language reference, see the companion tutorial: Learning Lyte — A Beginner's Guide.*
