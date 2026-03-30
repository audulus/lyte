# Lyte DSP Quick Start — Building Audio DSP in Lyte

*A practical guide to common DSP building blocks in Audulus Lyte. The examples are meant to be
small, pasteable, and easy to adapt.*

---

## The DSP Node

When a new Lyte DSP node is created in Audulus, it shows this template:

```lyte
// Inputs, outputs, `frames`, `sampleRate`, and `storage` are globals.
// Inputs, outputs, and `storage` are slices bound to the engine buffers.
// MAX_FRAMES is the max processing frames (used to declare temporary arrays).

init {}

process {
    for i in 0 .. frames {
        output[i] = input[i]
    }
}
```

That template is the whole interface. Everything below builds from it.

## Terms for New Users

These words show up right away in the fresh-node comments:

- `input`, `output`: audio buffers for the current block. `input[i]` means “sample `i`.”
- `frames`: how many samples are in the current block.
- `sampleRate`: how many samples happen per second, usually `44100.0` or `48000.0`.
- `storage`: extra sample memory for things like delay lines.
- `globals`: variables declared outside `process`. They remember values between blocks.
- `slice`: a buffer you index with `[...]`.
- `MAX_FRAMES`: the largest block size Audulus may give the node. Use it for temporary arrays when needed.

You do not declare `input`, `output`, `frames`, `sampleRate`, or `storage` yourself.
Audulus provides them.

| Global | Type | Description |
|---|---|---|
| `input`, `output` | `[f32]` | Default port slices. Additional named ports are added in the Inspector. |
| `frames` | `i32` | Number of samples in this processing block. |
| `sampleRate` | `f32` | Current sample rate, e.g. `44100.0`. |
| `storage` | `[f32]` | Pre-allocated buffer for delay lines etc., sized in the Inspector. |
| `MAX_FRAMES` | `i32` | Maximum possible block size — use to declare stack arrays. |

**`init {}`** runs once when the node loads. Use it for setup.

**`process { ... }`** runs once per block. Loop over `for i in 0 .. frames` and read or
write the buffers.

**Global `var`** is for state that should survive between blocks. Globals start at zero.

**Indexing is zero-based.** In `for i in 0 .. frames`, valid indices are `0` through
`frames - 1`. Use `freq[i]` for normal per-sample processing. Use `freq[0]` only when you
intentionally want the first sample in the block.

---

## A Note on `sampleRate`

`sampleRate` now behaves like a normal float. The usual pattern is to compute
`1.0 / sampleRate` once in `init` and multiply by that inside `process`.

```lyte
var inv_sr: f32

init {
    inv_sr = 1.0 / sampleRate
}
```

This also avoids doing a division every sample.

---

## Do's and Don'ts

These habits gave the most reliable results in the current Audulus build.

**Do:**
- Prefer `f32` almost everywhere in DSP code.
- Treat `sampleRate` like a normal float in math.
- Start from the fresh-node block template and change one thing at a time when debugging.
- Use `sin`, `cos`, `tan`, and other unsuffixed math builtins.
- Read a port sample into a scratch `f32` first if the compiler gets ambiguous about expressions like `input[i] * inv_sr`.
- Keep slice index proofs inline near `storage[...]` accesses when the safety checker complains.
- Use `freq[i]`, `cutoff[i]`, and similar forms for normal per-sample processing. Use `freq[0]` only when you intentionally want one control value for the whole block.

**Don't:**
- Don't assume Expr-node conveniences exist in Lyte. For example, `pi` is not built in here.
- Don't use `sinf`, `cosf`, or other suffixed math names in Audulus Lyte.
- Don't rely on helper functions to prove slice indices are safe; the checker usually wants the bounds checks inline.
- Don't use `assume` in node code — it is only allowed in the standard library or prelude.
- Don't assume examples from the standalone Lyte repo will drop into Audulus unchanged.

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

`phase` remembers where the oscillator is in its cycle. `hz` is a scratch `f32` used to
keep the compiler happy when doing math with `input[i]`.

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

Use `let` for one-time values inside the loop. Use `var` for values that change.

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

> 🔄 *Audulus users: The built-in Audulus Phasor outputs `0` to `2π`. This one uses `0` to
> `1`, which is often simpler in DSP code.*

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

// square: if/else is an expression in Lyte
var pw: f32   // global pulse width; 0.5 = 50% duty cycle
output[i] = if phase < pw { 1.0 } else { -1.0 }
```

`if/else` can return a value directly, so there is no ternary operator to learn.

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
        let res = if q[i] < 0.001 { 0.001 } else { q[i] }
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
is the one verified to behave correctly in the current Audulus build.

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

A delay line stores past samples in `storage` and reads them back later. This is the core
of echo, comb filters, chorus, flanging, and many reverb designs.

This is different from unit delay. Unit delay is always one sample. A delay line is a
larger buffer measured in samples, milliseconds, or seconds.

*Ports: `input`, `secs` in, `output` out.*

```lyte
var write: i32

lerp(a: f32, b: f32, t: f32) -> f32 {
    a + (b - a) * t
}

init {}

process {
    for i in 0 .. frames {
        let x = input[i]
        let len = storage.len

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
            storage[write] = x
        }

        let read_pos_0 = write as f32 - delay
        let read_pos = if read_pos_0 < 0.0 {
            read_pos_0 + len as f32
        } else {
            read_pos_0
        }

        let i0 = floor(read_pos) as i32
        let frac = read_pos - (i0 as f32)

        var i1 = i0 + 1
        if i1 >= len {
            i1 = 0
        }

        if i0 >= 0 && i0 < len {
            if i1 >= 0 && i1 < len {
                let a = storage[i0]
                let b = storage[i1]
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

This assumes `storage` is longer than the maximum delay you want. Set the Inspector
Samples value high enough to cover the longest delay time in your patch.

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
