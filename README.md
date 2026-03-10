# lyte

![build status](https://github.com/audulus/lyte/actions/workflows/rust.yml/badge.svg)
[![dependency status](https://deps.rs/repo/github/audulus/lyte/status.svg)](https://deps.rs/repo/github/audulus/lyte)

Lyte is a simple, statically-typed programming language designed for writing [Audulus](https://audulus.com) nodes. It compiles to native code via Cranelift JIT, with a VM backend for environments where JIT isn't available (iOS).

## Goals

- Familiar syntax (a mix of Rust and Swift)
- Function and operator overloading
- Generics constrained by interfaces
- Type inference
- No GC — no recursive data structures, no heap allocation
- Memory safety with static bounds checking
- Cranelift JIT backend
- VM backend for sandboxed environments (iOS)
- Safe cancellation of long-running programs

## Quick Start

```bash
# Build
cargo build

# Run a file with JIT
cargo run --bin lyte hello.lyte -c

# Run a file with VM
cargo run --bin lyte hello.lyte -r

# Run all tests
cargo test --workspace
```

## Language Tour

### Hello World

```
main {
    println("hello world")
}
```

### Variables and Control Flow

```
main {
    var x = 42
    let y = x + 1    // immutable binding

    if x > 0 {
        println("positive")
    } else {
        println("non-positive")
    }

    var sum = 0
    for i in 0 .. 10 {
        sum = sum + i
    }
}
```

### Functions

Functions support type annotations, overloading, and implicit returns (the last expression is the return value).

```
add(a: i32, b: i32) -> i32 {
    a + b
}

add(a: f32, b: f32) -> f32 {
    a + b
}

fact(x: i32) -> i32 {
    if x == 1 { return 1 }
    x * fact(x - 1)
}

main {
    assert(add(2, 3) == 5)
    assert(add(1.0, 2.0) == 3.0)
    assert(fact(5) == 120)
}
```

### Structs

```
struct Point {
    x: f32,
    y: f32
}

length(p: Point) -> f32 {
    sqrtf(p.x * p.x + p.y * p.y)
}

main {
    var p: Point
    p.x = 3.0
    p.y = 4.0
    assert(length(p) == 5.0)
}
```

### Operator Overloading

Define `__add`, `__sub`, `__mul`, etc. for custom types:

```
__add(lhs: Point, rhs: Point) -> Point {
    var p: Point
    p.x = lhs.x + rhs.x
    p.y = lhs.y + rhs.y
    p
}

__mul(lhs: Point, rhs: f32) -> Point {
    var p: Point
    p.x = lhs.x * rhs
    p.y = lhs.y * rhs
    p
}
```

### Enums

```
enum Direction { Up, Down, Left, Right }

is_vertical(d: Direction) -> bool {
    (d == .Up) || (d == .Down)
}

main {
    var d = Direction.Up
    assert(is_vertical(d))
    assert(!is_vertical(.Left))
}
```

### Arrays

Fixed-size arrays with static bounds checking:

```
main {
    var a = [1, 2, 3, 4, 5]
    assert(a[0] == 1)
    assert(a.len == 5)

    var grid: [f32; 64]
    for i in 0 .. 64 {
        grid[i] = 0.0
    }
}
```

### Tuples

```
main {
    var pair = (1, 2)
    assert(pair.0 == 1)
    assert(pair.1 == 2)
}
```

### Lambdas and Closures

```
apply(x: i32, f: i32 -> i32) -> i32 {
    f(x)
}

main {
    var f = |x| x * 2
    assert(f(3) == 6)

    assert(apply(5, |x| x + 1) == 6)

    // closures capture variables by reference
    var count = 0
    var inc = || count = count + 1
    inc()
    inc()
    assert(count == 2)
}
```

### Generics

Generic functions infer type parameters from arguments:

```
id<T>(x: T) -> T { x }

map<T0, T1, N>(a: [T0; N], f: T0 -> T1) -> [T1; N] {
    var i = 0
    var b: [T1; N]
    while i < a.len {
        b[i] = f(a[i])
        i = i + 1
    }
    b
}

main {
    assert(id(42) == 42)
    assert(id("hello") == "hello")
    assert(map([1, 2, 3], |x| x + 1) == [2, 3, 4])
}
```

### Generic Structs

```
struct Pair<A, B> {
    first: A,
    second: B
}

main {
    var p: Pair<i32, f32>
    p.first = 1
    p.second = 2.0
}
```

### Interfaces

Interfaces constrain generics and enable static dispatch:

```
interface Compare<A> {
    cmp(lhs: A, rhs: A) -> i32
}

sort<T, N>(array: [T; N]) -> [T; N] where Compare<T> {
    var a = array
    var i = 0
    while i < a.len {
        var j = 0
        while j < a.len - 1 - i {
            if cmp(a[j], a[j + 1]) > 0 {
                var tmp = a[j]
                a[j] = a[j + 1]
                a[j + 1] = tmp
            }
            j = j + 1
        }
        i = i + 1
    }
    a
}

// Implement Compare for i32
cmp(lhs: i32, rhs: i32) -> i32 { lhs - rhs }

main {
    assert(sort([3, 1, 2]) == [1, 2, 3])
}
```

### A Real-World Example: Biquad Filter

```
struct Biquad {
    b0: f32, b1: f32, b2: f32,
    a1: f32, a2: f32,
    x1: f32, x2: f32,
    y1: f32, y2: f32
}

lpf(fc: f32, fs: f32, q: f32) -> Biquad {
    var w0 = 2.0 * 3.14159265 * fc / fs
    var alpha = sinf(w0) / (2.0 * q)
    var cs = cosf(w0)
    var a0 = 1.0 + alpha
    var inv = 1.0 / a0

    var bq: Biquad
    bq.b1 = (1.0 - cs) * inv
    bq.b0 = bq.b1 / 2.0
    bq.b2 = bq.b0
    bq.a1 = (0.0 - 2.0 * cs) * inv
    bq.a2 = (1.0 - alpha) * inv
    return bq
}

process(bq: Biquad, x: f32) -> (Biquad, f32) {
    var y = bq.b0*x + bq.b1*bq.x1 + bq.b2*bq.x2
              - bq.a1*bq.y1 - bq.a2*bq.y2
    bq.x2 = bq.x1
    bq.x1 = x
    bq.y2 = bq.y1
    bq.y1 = y
    (bq, y)
}
```

## Architecture

The compiler pipeline:

1. **Lexer** — tokenizes source text
2. **Parser** — builds an AST
3. **Type Checker** — Hindley-Milner inference with constraint solving
4. **Safety Checker** — static bounds checking and division-by-zero detection via abstract interpretation
5. **Code Generation** — Cranelift JIT or VM bytecode

See [`src/README.md`](src/README.md) for detailed design notes.

## Benchmarks

The `benchmark/` directory contains a biquad filter DSP benchmark comparing Lyte (JIT and VM) against C, Lua 5.5, and LuaJIT. Run with:

```bash
./benchmark/run.sh
```
