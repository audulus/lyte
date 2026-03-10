# Learning Lyte — A Beginner's Guide

*A walkthrough of the Lyte programming language for Audulus users familiar with Lua.*

*Based on the official Lyte README: https://github.com/audulus/lyte*

---

## Table of Contents

- [Learning Lyte — A Beginner's Guide](#learning-lyte--a-beginners-guide)
  - [Table of Contents](#table-of-contents)
  - [1. What is Lyte?](#1-what-is-lyte)
  - [2. Types — The Biggest Difference from Lua](#2-types--the-biggest-difference-from-lua)
    - [The basic types in Lyte](#the-basic-types-in-lyte)
    - [Why does static typing matter?](#why-does-static-typing-matter)
  - [3. Your First Program — Hello World](#3-your-first-program--hello-world)
  - [4. Variables — `var` and `let`](#4-variables--var-and-let)
    - [Why have two kinds?](#why-have-two-kinds)
    - [Type inference](#type-inference)
  - [5. Control Flow — `if`, `else`, `for`, `while`](#5-control-flow--if-else-for-while)
    - [If / Else](#if--else)
    - [For loops](#for-loops)
    - [While loops](#while-loops)
  - [6. Functions](#6-functions)
    - [Basic function syntax](#basic-function-syntax)
    - [Implicit returns](#implicit-returns)
    - [Function overloading](#function-overloading)
    - [What is `assert`?](#what-is-assert)
  - [7. Structs — Grouping Data Together](#7-structs--grouping-data-together)
    - [Functions that use structs](#functions-that-use-structs)
  - [8. Operator Overloading](#8-operator-overloading)
  - [9. Enums — Named Choices](#9-enums--named-choices)
    - [Dot shorthand](#dot-shorthand)
    - [Using enums in functions](#using-enums-in-functions)
  - [10. Arrays and Tuples](#10-arrays-and-tuples)
    - [Arrays](#arrays)
    - [Static bounds checking](#static-bounds-checking)
    - [Tuples](#tuples)
  - [11. Slices — Flexible Views into Arrays](#11-slices--flexible-views-into-arrays)
  - [12. Lambdas and Closures](#12-lambdas-and-closures)
    - [Passing lambdas to functions](#passing-lambdas-to-functions)
    - [Closures](#closures)
  - [13. Generics — Writing Flexible Code](#13-generics--writing-flexible-code)
    - [A more complex example: `map`](#a-more-complex-example-map)
    - [Generic structs](#generic-structs)
  - [14. Interfaces — Rules for Generics](#14-interfaces--rules-for-generics)
    - [Using interfaces with `where`](#using-interfaces-with-where)
    - [Implementing an interface](#implementing-an-interface)
  - [15. A Real-World Example: Biquad Filter](#15-a-real-world-example-biquad-filter)
    - [Defining the filter state](#defining-the-filter-state)
    - [Creating a low-pass filter](#creating-a-low-pass-filter)
    - [Processing a sample](#processing-a-sample)
  - [16. Quick Reference](#16-quick-reference)
    - [Variable declaration](#variable-declaration)
    - [Basic types](#basic-types)
    - [Control flow](#control-flow)
    - [Functions](#functions)
    - [Collections](#collections)
    - [Operators](#operators)

---

## 1. What is Lyte?

If you've used Lua inside Audulus to write logic for nodes — like "take this input signal, do some math, output a result" — then you already understand what Lyte is for. Lyte is a new language that does the same job, but it's **faster and safer** than Lua for that purpose.

Think of it as: *Lua's replacement inside Audulus, with stricter rules that help you avoid bugs.*

Lyte was designed with a few clear goals in mind:

- **Familiar syntax** — it looks like a mix of two popular languages, Rust and Swift, but you don't need to know either of those to use it
- **Fast** — it compiles directly to native machine code, which is much faster than Lua's interpreter
- **Safe** — it checks for common mistakes (like going out of bounds on an array) *before* your code runs, not during
- **No memory management** — you never have to think about allocating or freeing memory; Lyte handles this through its strict rules

---

## 2. Types — The Biggest Difference from Lua

This is the biggest conceptual shift when coming from Lua. Understanding it early will make everything else click.

In Lua, variables have no fixed type. You can do this:

```lua
x = 42         -- a number
x = "hello"    -- now x is text. totally fine in Lua!
```

Lyte **won't allow that.** Once `x` holds a number, it always holds a number. This is called **static typing** — the *type* of every variable is locked in and checked before your code even runs.

### The basic types in Lyte

| Type | What it is | Example values |
|------|------------|----------------|
| `i32` | A whole number (integer) | `42`, `-7`, `0` |
| `f32` | A decimal number (float) | `3.14`, `-0.5`, `1.0` |
| `bool` | True or false | `true`, `false` |
| `str` | Text | `"hello"` |

**A note on `i32` vs `f32`:** The "32" refers to how many bits of memory the number uses — you don't need to worry about that detail. Just remember:
- `i32` = whole numbers only (no decimal point)
- `f32` = numbers with a decimal point

In audio and DSP work, you'll use `f32` constantly — audio signals, frequencies, and most math will be floating point values.

### Why does static typing matter?

In audio DSP, you're doing thousands of math operations per second. If the language has to guess what type something is while running (like Lua does), that takes extra time. By knowing the types upfront, Lyte can generate much faster code.

It also catches mistakes *before* they cause weird audio glitches. Instead of a bug silently producing wrong output, Lyte just refuses to compile and tells you what went wrong.

---

## 3. Your First Program — Hello World

```lyte
main {
    println("hello world")
}
```

A few things to notice right away:

**`main` is a special block, not a function.** In many languages, `main` is a function you have to define carefully. In Lyte, it's just a block of code — the starting point of your program. No parentheses, no return type, just `main { ... }`.

**`println` prints text to the output.** It's a built-in function. You pass it a string (text in quotes) and it prints it.

**Comments use `//`.** Anything after `//` on a line is ignored by Lyte — it's just a note for the reader:

```lyte
main {
    println("hello world")   // this is a comment
}
```

---

## 4. Variables — `var` and `let`

Lyte has two ways to create a variable, and they have an important difference:

```lyte
main {
    var x = 42      // mutable: x can be changed later
    let y = x + 1   // immutable: y can never be changed
}
```

- **`var`** = the value *can* change. Use this when you need to update something (like a running total, or a signal value).
- **`let`** = the value *cannot* change after it's set. Use this when something is fixed.

### Why have two kinds?

This is a safety feature. If you mark something as `let`, Lyte guarantees it will never be accidentally overwritten. In complex DSP code with lots of variables, this helps prevent subtle bugs.

### Type inference

Notice that in the examples above, we didn't write the type — Lyte figured it out automatically. `var x = 42` — Lyte sees `42` and knows `x` must be an `i32`. This is called **type inference**.

You *can* write the type explicitly if you want, and sometimes you have to (when declaring a variable without immediately giving it a value):

```lyte
var x: i32          // declared as i32, no value yet
var signal: f32     // declared as f32, no value yet
x = 10
signal = 0.5
```

---

## 5. Control Flow — `if`, `else`, `for`, `while`

Control flow is how your program makes decisions and repeats things. This will feel familiar from Lua.

### If / Else

```lyte
main {
    var x = 42

    if x > 0 {
        println("positive")
    } else {
        println("non-positive")
    }
}
```

This works just like Lua's `if/else`, except:
- No `then` keyword
- No `end` keyword — curly braces `{ }` mark the beginning and end of blocks
- Parentheses around the condition are optional

### For loops

```lyte
main {
    var sum = 0
    for i in 0 .. 10 {
        sum = sum + i
    }
}
```

The `0 .. 10` is a **range**. It means "from 0 up to but not including 10" — so it goes 0, 1, 2, 3, 4, 5, 6, 7, 8, 9. This is called an *exclusive* range (the end value is excluded).

`i` is automatically created as the loop variable — you don't need `var i` beforehand.

### While loops

```lyte
main {
    var i = 0
    while i < 10 {
        i = i + 1
    }
}
```

`while` keeps looping as long as the condition is true. Unlike `for`, you manage the loop variable yourself.

---

## 6. Functions

Functions are reusable blocks of code that take inputs and produce an output.

### Basic function syntax

```lyte
add(a: i32, b: i32) -> i32 {
    a + b
}
```

Breaking this down:
- `add` — the name of the function
- `(a: i32, b: i32)` — the **parameters**: two inputs, both whole numbers. Each parameter is written as `name: type`
- `-> i32` — the **return type**: what type of value this function sends back
- `{ a + b }` — the **body**: the code that runs

**No `fn` keyword** — unlike many other languages, Lyte functions don't need a special keyword. The name followed by parentheses is enough.

### Implicit returns

Notice there's no `return` statement. In Lyte, **the last expression in a function is automatically returned.** So `a + b` at the end just gets returned without needing `return a + b`.

You *can* use `return` explicitly if you need to exit early:

```lyte
fact(x: i32) -> i32 {
    if x == 1 { return 1 }   // early exit
    x * fact(x - 1)           // otherwise, return this
}
```

This is a **recursive** function — `fact` calls itself. It calculates a factorial: `fact(5)` = 5 × 4 × 3 × 2 × 1 = 120.

### Function overloading

Lyte allows you to define the same function name multiple times, as long as the parameter types are different. Lyte picks the right version automatically:

```lyte
add(a: i32, b: i32) -> i32 {
    a + b
}

add(a: f32, b: f32) -> f32 {
    a + b
}

main {
    assert(add(2, 3) == 5)         // uses the i32 version
    assert(add(1.0, 2.0) == 3.0)   // uses the f32 version
}
```

This is called **overloading** — the same name, different types.

### What is `assert`?

`assert` is a built-in that checks whether something is true. If it's not, your program stops and reports an error. It's used heavily in examples to verify that code works correctly. Think of it as: "make sure this is true, otherwise something is wrong."

---

## 7. Structs — Grouping Data Together

A **struct** is a way to bundle related pieces of data into a single named unit.

Imagine you're working with a 2D point. It has an X coordinate and a Y coordinate. Instead of keeping two separate variables, you can define a struct:

```lyte
struct Point {
    x: f32,
    y: f32
}
```

Now `Point` is a new type you can use anywhere. To create one and use it:

```lyte
main {
    var p: Point    // create a Point variable
    p.x = 3.0       // set the x field
    p.y = 4.0       // set the y field
}
```

You access the fields of a struct using a dot: `p.x`, `p.y`.

### Functions that use structs

In Lyte, there's no concept of methods *inside* a struct (unlike some other languages). Instead, you write regular functions that take the struct as a parameter:

```lyte
length(p: Point) -> f32 {
    sqrtf(p.x * p.x + p.y * p.y)
}

main {
    var p: Point
    p.x = 3.0
    p.y = 4.0
    assert(length(p) == 5.0)   // 3-4-5 right triangle!
}
```

`sqrtf` is a built-in function that computes a square root. The `f` at the end stands for float — it works on `f32` values.

---

## 8. Operator Overloading

In Lua, if you have two tables representing points, you can't just write `p1 + p2` — Lua doesn't know what that means for tables. Lyte lets you define exactly what `+`, `-`, `*`, and other operators mean for your custom types. This is called **operator overloading**.

You do it by defining special functions with names like `__add`, `__sub`, `__mul`:

```lyte
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

- `lhs` means "left-hand side" (the thing on the left of the operator)
- `rhs` means "right-hand side" (the thing on the right)

Once defined, you can write natural math with your custom types:

```lyte
main {
    var a: Point
    a.x = 1.0
    a.y = 2.0

    var b: Point
    b.x = 3.0
    b.y = 4.0

    var c = a + b    // calls __add automatically
    var d = a * 2.0  // calls __mul automatically
}
```

This is especially powerful in DSP, where you often want to do math on custom signal or vector types.

---

## 9. Enums — Named Choices

An **enum** (short for "enumeration") lets you define a type that can only be one of a fixed set of named values. It's useful when something has a limited number of states or options.

```lyte
enum Direction { Up, Down, Left, Right }
```

Now `Direction` is a type, and it can only ever be one of those four values:

```lyte
main {
    var d = Direction.Up
}
```

### Dot shorthand

When Lyte can figure out the type from context, you can use a shorthand with just a dot:

```lyte
assert(!is_vertical(.Left))   // Lyte knows this must be a Direction
```

### Using enums in functions

```lyte
is_vertical(d: Direction) -> bool {
    (d == .Up) || (d == .Down)
}
```

This function takes a `Direction` and returns `true` if it's `Up` or `Down`. The `||` means "or", and `==` checks equality.

---

## 10. Arrays and Tuples

### Arrays

An **array** is an ordered list of values, all of the same type. In Lyte, arrays have a **fixed size** — you decide the size when you create the array, and it never changes.

```lyte
main {
    var a = [1, 2, 3, 4, 5]   // array of 5 integers
    assert(a[0] == 1)          // access by index (starts at 0!)
    assert(a.len == 5)         // .len gives you the length
}
```

**Indexing starts at 0** — the first element is `a[0]`, the second is `a[1]`, and so on. This is standard in most programming languages (Lua is unusual in starting at 1).

To declare an array with a specific type and size without filling it in right away:

```lyte
var grid: [f32; 64]    // 64 floats, all starting at 0.0
```

The syntax `[f32; 64]` means "an array of 64 `f32` values." You can fill it in with a loop:

```lyte
for i in 0 .. 64 {
    grid[i] = 0.0
}
```

### Static bounds checking

One of Lyte's safety features is that it checks array access *before* your code runs where possible. If you try to access `a[10]` on an array that only has 5 elements, Lyte will catch it — no mysterious crashes at runtime.

### Tuples

A **tuple** is like a mini-struct — a fixed collection of values that can be *different* types. You don't need to define it in advance.

```lyte
main {
    var pair = (1, 2)      // a tuple with two integers
    assert(pair.0 == 1)    // access with .0, .1, etc.
    assert(pair.1 == 2)
}
```

Tuples are useful when a function needs to return more than one value (since a function can only have one return type, but that type can be a tuple):

```lyte
min_max(a: i32, b: i32) -> (i32, i32) {
    if a < b { (a, b) } else { (b, a) }
}
```

---

## 11. Slices — Flexible Views into Arrays

A **slice** is a window into an array. It doesn't own the data — it just points to part (or all) of an existing array. This is useful for writing functions that work on arrays of *any* size.

When you write `[i32]` (without a size), that means a slice:

```lyte
sum(a: [i32]) -> i32 {
    var s = 0
    for i in 0 .. a.len {
        s = s + a[i]
    }
    s
}
```

This `sum` function works on any array of integers, regardless of how big it is. When you call it, Lyte automatically passes your fixed array as a slice:

```lyte
main {
    assert(sum([1, 2, 3]) == 6)

    var data = [10, 20, 30, 40, 50]
    assert(sum(data) == 150)
}
```

The key difference:
- `[i32; 5]` — a fixed array of exactly 5 integers (size known at compile time)
- `[i32]` — a slice, a view into any array of integers (size known at runtime)

---

## 12. Lambdas and Closures

A **lambda** is a small, unnamed function you can write inline. Instead of defining a full named function, you just write the logic right where you need it.

```lyte
main {
    var f = |x| x * 2    // a lambda: takes x, returns x * 2
    assert(f(3) == 6)
}
```

The `|x|` syntax means "a function that takes `x` as input." Everything after it is the body.

### Passing lambdas to functions

Lambdas are really useful when you want to pass behavior as an argument. Here's a function that applies any function to a value:

```lyte
apply(x: i32, f: i32 -> i32) -> i32 {
    f(x)
}

main {
    assert(apply(5, |x| x + 1) == 6)
}
```

The parameter `f: i32 -> i32` means "a function that takes an `i32` and returns an `i32`." You're passing the lambda `|x| x + 1` as that function.

### Closures

A **closure** is a lambda that *captures* variables from the surrounding code — it can remember and use variables defined outside of it:

```lyte
main {
    var count = 0
    var inc = || count = count + 1   // no parameters (||), captures 'count'
    inc()
    inc()
    assert(count == 2)   // count was modified by the closure!
}
```

`|| count = count + 1` is a closure with no parameters (`||` means no inputs). Every time you call `inc()`, it increments `count`. Closures capture variables **by reference**, meaning they modify the original variable, not a copy.

---

## 13. Generics — Writing Flexible Code

Sometimes you want to write a function that works the same way for multiple types. For example, an "identity" function that just returns whatever you give it:

```lyte
id<T>(x: T) -> T { x }
```

The `<T>` is a **type parameter** — a placeholder for "whatever type you pass in." When you call `id(42)`, Lyte figures out that `T` must be `i32`. When you call `id("hello")`, `T` becomes `str`. You write the function once, and it works for any type.

```lyte
main {
    assert(id(42) == 42)
    assert(id("hello") == "hello")
}
```

### A more complex example: `map`

```lyte
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
    assert(map([1, 2, 3], |x| x + 1) == [2, 3, 4])
}
```

`map` takes an array and a function, applies the function to every element, and returns a new array. Here:
- `T0` is the input element type
- `T1` is the output element type
- `N` is the size of the array

### Generic structs

Structs can also be generic:

```lyte
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

`Pair` can hold any two types. Here it holds an `i32` and an `f32`.

---

## 14. Interfaces — Rules for Generics

Generics are powerful, but sometimes you need to say "this generic type has to support certain operations." That's what **interfaces** are for.

An interface defines a set of functions that a type must have:

```lyte
interface Compare<A> {
    cmp(lhs: A, rhs: A) -> i32
}
```

This says: "Any type `A` that implements `Compare` must have a `cmp` function that takes two values of type `A` and returns an `i32`."

### Using interfaces with `where`

You can then write a generic function that requires the type to implement an interface:

```lyte
sort<T, N>(array: [T; N]) -> [T; N] where Compare<T> {
    // ... sorting code ...
}
```

The `where Compare<T>` part means "this only works for types `T` that have the `cmp` function defined."

### Implementing an interface

You implement an interface simply by defining the required function — no special keyword needed:

```lyte
cmp(lhs: i32, rhs: i32) -> i32 { lhs - rhs }
```

By defining `cmp` for `i32`, you've automatically made `i32` satisfy the `Compare` interface. Now you can sort arrays of integers:

```lyte
main {
    assert(sort([3, 1, 2]) == [1, 2, 3])
}
```

---

## 15. A Real-World Example: Biquad Filter

This is where Lyte really shines for Audulus users. A **biquad filter** is a fundamental building block in audio DSP — it's used to make low-pass filters, high-pass filters, EQs, and much more. Here's how you'd write one in Lyte, using everything we've learned.

### Defining the filter state

```lyte
struct Biquad {
    b0: f32, b1: f32, b2: f32,   // feedforward coefficients
    a1: f32, a2: f32,             // feedback coefficients
    x1: f32, x2: f32,             // previous input samples
    y1: f32, y2: f32              // previous output samples
}
```

The struct stores all the state the filter needs between audio samples.

### Creating a low-pass filter

```lyte
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
```

- `fc` = cutoff frequency
- `fs` = sample rate
- `q` = resonance (Q factor)
- `sinf` and `cosf` are built-in math functions (sine and cosine for floats)

### Processing a sample

```lyte
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

This takes a `Biquad` state and one input sample `x`, and returns a **tuple**: the updated filter state and the output sample `y`. Because Lyte has no heap allocation, you pass the state in and get an updated copy back out.

This example brings together structs, `f32` math, tuples, and functions — everything you've learned!

---

## 16. Quick Reference

### Variable declaration

| Syntax | Meaning |
|--------|---------|
| `var x = 42` | Mutable variable, type inferred |
| `let y = 3.14` | Immutable variable, type inferred |
| `var x: f32` | Mutable variable, explicit type, no value yet |

### Basic types

| Type | Description |
|------|-------------|
| `i32` | Whole number |
| `f32` | Decimal number |
| `bool` | `true` or `false` |
| `str` | Text |

### Control flow

| Syntax | Meaning |
|--------|---------|
| `if x > 0 { ... } else { ... }` | Conditional |
| `for i in 0 .. 10 { ... }` | Loop from 0 to 9 (exclusive) |
| `while x < 10 { ... }` | Loop while condition is true |

### Functions

| Syntax | Meaning |
|--------|---------|
| `add(a: i32, b: i32) -> i32 { ... }` | Function definition |
| `\|x\| x * 2` | Lambda (inline function) |
| `\|\| count = count + 1` | Closure (no parameters) |

### Collections

| Syntax | Meaning |
|--------|---------|
| `[1, 2, 3]` | Array literal |
| `var a: [f32; 64]` | Fixed array of 64 floats |
| `[f32]` | Slice (flexible array view, in function parameters) |
| `(1, 2.0)` | Tuple |

### Operators

| Operator | Meaning |
|----------|---------|
| `+`, `-`, `*`, `/` | Arithmetic |
| `==`, `!=` | Equality / inequality |
| `<`, `>`, `<=`, `>=` | Comparison |
| `&&` | And |
| `\|\|` | Or |
| `!` | Not |

---

*This tutorial was written as a companion to the official Lyte README.*
*Source: https://github.com/audulus/lyte*
