# Learning Lyte

*A beginner-friendly guide to Lyte for Audulus users.*

---

## Table of Contents

1. [What is Lyte?](#1-what-is-lyte)
2. [Types — Static vs Dynamic Typing](#2-types--static-vs-dynamic-typing)
3. [Your First Program — Hello World](#3-your-first-program--hello-world)
4. [Variables — var and let](#4-variables--var-and-let)
5. [Control Flow — if, else, for, while](#5-control-flow--if-else-for-while)
6. [Functions](#6-functions)
7. [Structs — Grouping Data Together](#7-structs--grouping-data-together)
8. [Operator Overloading](#8-operator-overloading)
9. [Enums — Named Choices](#9-enums--named-choices)
10. [Arrays and Tuples](#10-arrays-and-tuples)
11. [Slices — Flexible Views into Arrays](#11-slices--flexible-views-into-arrays)
12. [Lambdas and Closures](#12-lambdas-and-closures)
13. [Generics — Writing Flexible Code](#13-generics--writing-flexible-code)
14. [Interfaces — Rules for Generics](#14-interfaces--rules-for-generics)
15. [A Real-World Example: Biquad Filter](#15-a-real-world-example-biquad-filter)
16. [Quick Reference](#16-quick-reference)
17. [Standard Library](#17-standard-library)
18. [Macros](#18-macros)
19. [Common Errors](#19-common-errors)
20. [Features in the Grammar Not Yet Covered](#20-features-in-the-grammar-not-yet-covered)

---

## 1. What is Lyte?

Audulus has both a **Lyte DSP node** and a **DSP node**. The DSP node is for Lua scripting. The Lyte DSP node is for Lyte.

Lyte is a language made for writing DSP code in Audulus. Like Lua in a DSP node, it takes input signals, does math, and produces output. The difference is mainly in how the language works and what it is best at.

Both Lyte and Lua can essentially do the same core job in Audulus: take input signals, process frames of audio, and produce output. In practice, both use a similar `process` style where audio is handled sample by sample across a block of frames.

They have different strengths:

- **Lyte** is built for speed and optimized audio DSP.
- **Lua** is a friendly, higher-level language with its own useful features, such as tables and string formatting.

Lyte was designed with a few clear goals in mind:

- **Familiar syntax** — it looks a bit like other modern languages, but you do not need to know them first
- **Fast** — Lyte turns your code into machine code ahead of time instead of interpreting it as it runs, which is helpful for heavy DSP math
- **Safe** — it checks for common mistakes, like out-of-range array access, before the code runs
- **Predictable** — it uses strict rules so the system knows what memory is needed up front

As a rough starting point, Lyte is a strong fit for sample-rate DSP: filters, oscillators, waveshapers, and other code that processes audio sample by sample at high speed. Lua is often a good fit when you want a more flexible scripting style.

---

## 2. Types — Static vs Dynamic Typing

One of the biggest differences from Lua is how Lyte handles types. This comes up everywhere, so it is worth getting comfortable with early.

In Lua, variables have no fixed type — you can reassign a variable to a completely different kind of value at any time:

```lua
x = 42         -- a number
x = "hello"    -- now x is text. totally fine in Lua!
```

Lyte **won't allow that.** Once `x` is a number, it stays a number. This is called **static typing**. It means Lyte decides what kind of value each variable holds before the code runs.

Note for Lua users: Lua lets one variable hold different kinds of values over time. Lyte does not.

### The basic types in Lyte

| Type | What it is | Example values |
|------|------------|----------------|
| `i32` | A whole number (integer) | `42`, `-7`, `0` |
| `f32` | A decimal number (float) | `3.14`, `-0.5`, `1.0` |
| `bool` | True or false | `true`, `false` |
| `str` | Text (a string in quotes) | `"hello"` |
| `i8` | A very small whole number, used for individual characters | `65` (the letter `A`) |
| `u32` | A whole number that cannot be negative (unsigned) | `42`, `0` |

Single characters can also be written with single quotes: `'a'`, `'Z'`, `'\n'` (newline). This is called a character literal.

Boolean values support the standard logical operators:

```lyte
main {
    var t = true
    var f = false
    assert(t != f)       // true and false are not equal
    assert(!f)           // ! means "not"
    assert(t || f)       // || means "or"
    assert(t && t)       // && means "and"
    assert(!f == t)      // ! binds tighter than ==
}
```

**A note on `i32` vs `f32`:** The "32" is just part of the type name. You do not need to worry much about the low-level detail. The useful part is:
- `i32` = 'i' is for integer, whole numbers only (no decimal point)
- `f32` = 'f' is for float, numbers with a decimal point

In audio and DSP work, you will use `f32` constantly. Audio signals, frequencies, and most DSP math are floating-point values. This is especially true in Audulus, where normal input and output ports are `f32`.

**A note on `f64`:** The language grammar also defines an `f64` type, which is a higher-precision decimal number. In Audulus DSP work, `f32` is the type you'll usually use.

### Why does static typing matter?

In audio DSP, you are doing a lot of math very quickly. If the language has to keep figuring out what kind of value something is while it runs, that adds overhead. By knowing the types up front, Lyte can build faster code. Here, **compile** just means “turn your code into something the computer can run.”

It also catches many mistakes before they turn into weird behavior or broken audio. Instead of running bad code, Lyte stops and tells you what went wrong.

For example, assigning an `f32` value to an `i32` variable is a type error:

```lyte
f {
    var x: i32
    x = 42.0    // ❌ no solution for i32 == f32
}
```

The phrase "no solution for X == Y" is Lyte's general way of saying "I couldn't make these two types agree." You'll see this same phrasing for other type mismatches too — mismatched array sizes, wrong argument types, and so on. If you ever see it, it means something on the left side of an operation doesn't match the type on the right.

---

## 3. Your First Program — Hello World

```lyte
init {}

process {
    for i in 0 .. frames {
        output[i] = input[i]
    }
    println("Hello world!")
}
```

A few things to notice right away:

**In an Audulus DSP node, `process` is where the audio work happens.** This example passes the input straight to the output, then prints a simple debug message once per processing block.

**`println` prints strings.** A plain text message is the simplest way to test that debug output is working.

**Comments use `//`.** Anything after `//` on a line is ignored by Lyte. It is just a note for the reader.

Note for Lua users: Lua comments use `--` instead of `//`.

```lyte
init {}

process {
    for i in 0 .. frames {
        output[i] = input[i]
    }
    println("Hello world!")   // this is a comment
}
```

If you want to print a changing number, use a small counter and convert it to text:

```lyte
var calls: i32

init {}

process {
    var buf: [i8; 16]

    for i in 0 .. frames {
        output[i] = input[i]
    }

    itoa(buf, calls)
    println(buf)
    calls = calls + 1
}
```

---

## 4. Variables — `var` and `let`

Lyte has two ways to create a variable, and the difference is simple:

```lyte
main {
    var x = 42      // can change later
    let y = x + 1   // stays fixed after this line
}
```

- **`var`** = the value can change later
- **`let`** = the value stays fixed after you set it

### Why have two kinds?

This helps prevent mistakes. If something is written as `let`, Lyte knows it should not change later.

### Type inference

Notice that in the examples above, we did not write the type. Lyte figured it out automatically. `var x = 42` means Lyte sees `42` and knows `x` must be an `i32`. This is called **type inference**.

You *can* write the type explicitly if you want, and sometimes you have to (when declaring a variable without immediately giving it a value):

```lyte
var x: i32          // declared as i32, no value yet
var signal: f32     // declared as f32, no value yet
x = 10
signal = 0.5
```

When you declare a variable without a value, Lyte starts it at zero. Numbers start at `0` or `0.0`, booleans start at `false`, and struct fields start at their own zero values too:

```lyte
main {
    var i: i32
    assert(i == 0)   // always true — i starts at zero
}
```

This is a useful safety feature. You do not get random leftover memory values.

### Variable shadowing

You can declare a new variable with the same name inside an inner block, like a loop or an `if`. Inside that block, the new variable temporarily takes over the name:

```lyte
main {
    var x = 42
    while x == 0 {
        var x = 0   // this x is separate from the outer x
    }
    // outer x is still 42 here
}
```

The outer `x` is unchanged. Lyte allows this, but it can be confusing, so use it carefully.

---

### Global variables

A variable declared outside of any function or `main` block is a **global**. It exists for the whole program and any function can use it:

```lyte
var global: i32

f {
    global = 42       // functions can read and write globals
}

main {
    assert(global == 0)  // zero-initialized, like locals
    f()
    assert(global == 42)
}
```

Global variables must be declared with an explicit type (`var global: i32`, not `var global = 0`). Like local variables, they start at zero. Any function can access them by name.

In Audulus DSP nodes, globals are the normal place to store state that must survive between processing blocks, such as filter memory, oscillator phase, or envelope position. Named ports also appear as globals, and DSP code usually loops over `frames` inside `process`.

---

## 5. Control Flow — `if`, `else`, `for`, `while`

Control flow is how your program makes decisions and repeats things.

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

There is no `then` and no `end`. Curly braces `{ }` mark the block. Parentheses around the condition are optional.

Note for Lua users: Lyte uses `{ }` instead of `then` and `end`.

### For loops

```lyte
main {
    var sum = 0
    for i in 0 .. 10 {
        sum = sum + i
    }
}
```

The `0 .. 10` is a range. It means "from 0 up to but not including 10" — so it goes 0, 1, 2, 3, 4, 5, 6, 7, 8, 9.

Note for Lua users: `for i in 0 .. 10 { }` is similar to `for i = 0, 9 do ... end`.

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

This is a **recursive** function — meaning it calls itself as part of its own definition. Each call to `fact` triggers another call with a smaller number, until it reaches `fact(1)` and stops. It calculates a factorial: `fact(5)` = 5 × 4 × 3 × 2 × 1 = 120. Recursion can be a tricky concept at first, but the key idea is just: a function that solves a problem by calling a simpler version of itself.

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

Note for Lua users: in Lua you would usually use a table for this, like `point = { x = 3.0, y = 4.0 }`. Lyte structs fill a similar role, but the fields and their types are fixed.

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
    sqrt(p.x * p.x + p.y * p.y)
}

main {
    var p: Point
    p.x = 3.0
    p.y = 4.0
    assert(length(p) == 5.0)   // 3-4-5 right triangle!
}
```

`sqrt` is a built-in function that computes a square root.

---

## 8. Operator Overloading

Lyte lets you define exactly what `+`, `-`, `*`, and other operators mean for your custom types. This is called **operator overloading**.

Note for Lua users: this is similar in spirit to Lua metatables. Names like `__add` and `__mul` will probably look familiar.

You do it by defining special functions with names like `__add`, `__sub`, `__mul`:

```lyte
struct Point {
    x: i32,
    y: i32
}

__add(lhs: Point, rhs: Point) -> Point {
    var p: Point
    p.x = lhs.x + rhs.x
    p.y = lhs.y + rhs.y
    return p
}

__sub(lhs: Point, rhs: Point) -> Point {
    var p: Point
    p.x = lhs.x - rhs.x
    p.y = lhs.y - rhs.y
    return p
}

__mul(lhs: Point, rhs: i32) -> Point {
    var p: Point
    p.x = lhs.x * rhs
    p.y = lhs.y * rhs
    return p
}
```

- `lhs` means "left-hand side" (the thing on the left of the operator)
- `rhs` means "right-hand side" (the thing on the right)

Once defined, you can write natural math with your custom types:

```lyte
main {
    var p0: Point
    p0.x = 1
    p0.y = 2
    var p1: Point
    p1.x = 3
    p1.y = 4

    var sum = p0 + p1    // calls __add automatically
    assert(sum.x == 4)
    assert(sum.y == 6)

    var s = p0 * 2       // calls __mul automatically
    assert(s.x == 2)
    assert(s.y == 4)
}
```

Note that `__mul` here takes a `Point` on the left and an `i32` scalar on the right — the types of `lhs` and `rhs` don't have to match, and they don't have to be the same as the struct type.

### Unary operator overloading

Overloading the unary `-` operator (negation, via `__neg`) is **not currently working** in Lyte. The feature exists in the design but is commented out in the test suite. Stick to binary operators (`__add`, `__sub`, `__mul`, etc.) for now.

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
    var d: Direction
    d = .Up           // assign with dot shorthand
    assert(d == .Up)
    d = .Left
    assert(d != .Up)
}
```

### Dot shorthand

When Lyte can figure out the type from context, you can use a shorthand with just a dot — you don't need to write the enum name:

```lyte
assert(!is_vertical(.Left))   // Lyte knows this must be a Direction
```

### Using enums in functions

```lyte
is_vertical(d: Direction) -> bool {
    (d == .Up) || (d == .Down)
}

main {
    assert(is_vertical(.Up))
    assert(!is_vertical(.Left))
}
```

This function takes a `Direction` and returns `true` if it's `Up` or `Down`.

### Enums inside structs

Enum types work as struct fields like any other type:

```lyte
enum Status { Active, Inactive }

struct Item {
    value: i32,
    status: Status
}

main {
    var item: Item
    item.value = 42
    item.status = .Active
    assert(item.status == .Active)
    assert(item.value == 42)
}
```

---

## 10. Arrays and Tuples

### Arrays

An **array** is an ordered list of values, all of the same type. In Lyte, arrays have a **fixed size** — you decide the size when you create the array, and it never changes.

Note for Lua users: Lua tables can grow and shrink. Lyte arrays have a fixed size.

```lyte
main {
    var a = [1, 2, 3, 4, 5]   // array of 5 integers
    assert(a[0] == 1)          // access by index (starts at 0!)
    assert(a.len == 5)         // .len gives you the length
}
```

**Indexing starts at 0** — the first element is `a[0]`, the second is `a[1]`, and so on. This is standard in most programming languages.

Note for Lua users: Lua starts at `1`, but Lyte starts at `0`.

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

### Returning arrays from functions

A function can return an array. The return type uses the same `[type; size]` syntax:

```lyte
make_triple() -> [i32; 3] {
    return [1, 2, 3]
}

main {
    var a = make_triple()
    assert(a[0] == 1)
    assert(a.len == 3)
}
```

### Reassigning a whole array

You can replace all the contents of an array variable at once by assigning a new literal — as long as the type and size match:

```lyte
main {
    var a = [1, 2, 3]
    a = [42, 0, 0]       // replace all elements at once
    assert(a[0] == 42)
    assert(a.len == 3)   // size stays the same
}
```

### Static bounds checking

One of Lyte's safety features is that it checks array access *before* your code runs. If you write an index that can't be proven to be within bounds, Lyte rejects it at compile time with a specific message:

```
❌ couldn't prove index is less than array length
```

For example, accessing `array[1]` on a `[i32; 1]` array (which only has index `0`) is caught before your program ever runs. No mysterious crashes at runtime.

### Arrays are copied by value

When you assign an array to a new variable, you get an independent copy. Changing one doesn't affect the other:

```lyte
main {
    var a = ['x']
    var b = a
    b[0] = 'y'
    assert(b[0] == 'y')
    assert(a[0] == 'x')   // a is unchanged
}
```

This also applies when arrays are inside structs — assigning a struct copies the whole thing, including any arrays it contains.

Note for Lua users: Lua tables are shared by reference. Lyte arrays are copied by value, so changing one copy does not change the other.

### Arrays inside structs

Arrays work as struct fields using the same type syntax:

```lyte
struct Grid {
    data: [f32; 16]
}
```

Assigning a struct copies the array too:

```lyte
struct Buffer {
    array: [i8; 1]
}

main {
    var a: Buffer
    a.array[0] = 'x'
    var b = a            // full copy, including the array
    b.array[0] = 'y'
    assert(b.array[0] == 'y')
    assert(a.array[0] == 'x')   // a is unchanged
}
```

### Nested arrays (2D arrays)

You can create arrays of arrays using nested type syntax. A 4×4 grid of `i8` values looks like this:

```lyte
main {
    var a: [[i8; 4]; 4]   // 4 rows, each row is 4 i8 values
    a[0][0] = 'x'
    assert(a[0][0] == 'x')
}
```

The outer index selects the row; the inner index selects the element within that row. Nested arrays are also copied by value — assigning to `b` gives a fully independent copy:

```lyte
    var b = a
    b[0][0] = 'y'
    assert(b[0][0] == 'y')
    assert(a[0][0] == 'x')   // original unchanged
```

### Array equality

Arrays of the same type and size can be compared with `==` and `!=` — this is confirmed by the test suite (e.g. `map([1,2,3], add_one) == [2,3,4]`). Arrays of *different* sizes cannot be compared — Lyte treats this as a type error:

```
❌ no solution for [i32; 2] == [i32; 3]
```

String equality (`[i8]` slices) also works by content — see Section 11 for details.

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
- `[i32; 5]` — a fixed array of exactly 5 integers. The size is known at *compile time* — meaning Lyte knows it before your program ever runs.
- `[i32]` — a slice, a view into any array of integers. The size is only known at *runtime* — meaning when your program is actually running and the data exists.

### Default slices have length zero

A slice variable declared without a value is safe to use — its `.len` is `0`, not garbage:

```lyte
main {
    var s: [i32]
    assert(s.len == 0)   // safe — not undefined
}
```

### Slices cannot be returned from functions

Slices are only valid as function *parameters*, not return types. Attempting to return a slice is a compile error:

```
❌ slice type [i32] is not allowed as a return type
    f(a: [i32]) -> [i32] { a }
```

If you need to return array data from a function, use a fixed array with an explicit size (`[i32; N]`) instead.

### String equality

Strings (`[i8]` slices) support `==` and `!=`, which compare by **content** — two separate string variables with the same text are considered equal:

```lyte
main {
    var a = "test"
    var b = "test"
    assert(a == b)      // same content — equal

    var c = "foo"
    var d = "bar"
    assert(c != d)      // different content — not equal

    assert("x" == "x")
    assert("x" != "y")
}
```

This is different from some languages where string equality compares identity (whether two variables point to the *same* object in memory). In Lyte, it's always content comparison.

---

## 12. Lambdas and Closures

A **lambda** is a small, unnamed function you can write inline. Instead of defining a full named function, you just write the logic right where you need it.

```lyte
main {
    var f = |x| x + 1    // a lambda: takes x, returns x + 1
    assert(f(1) == 2)
}
```

The `|x|` syntax means "a function that takes `x` as input." Everything after it is the body.

### Passing lambdas to functions

Lambdas are really useful when you want to pass behavior as an argument. Here's a function that applies any function to a value:

```lyte
apply(f: i32 -> i32, x: i32) -> i32 { return f(x) }

main {
    assert(apply(|x| x + 1, 3) == 4)
}
```

The parameter `f: i32 -> i32` means "a function that takes an `i32` and returns an `i32`."

### Named functions as arguments

You can pass a named function anywhere a lambda is expected — just use the function name without calling it:

```lyte
add_one(x: i32) -> i32 { return x + 1 }
double(x: i32) -> i32 { return x * 2 }
apply(f: i32 -> i32, x: i32) -> i32 { return f(x) }

main {
    assert(apply(add_one, 3) == 4)
    assert(apply(double, 3) == 6)
    assert(apply(add_one, apply(double, 5)) == 11)  // composing calls
}
```

### Immediately-invoked lambdas

You can define and call a lambda in one step by wrapping it in parentheses. This is mostly useful in more complex code — as a beginner you probably won't need it often, but it's good to recognize when you see it:

```lyte
main {
    var x = (|x| x + 1)(1)   // define and call immediately — result is 2
    assert(x == 2)
}
```

### Lambdas with a block body

If a lambda needs more than one expression, wrap the body in braces. This lets you put multiple steps inside a lambda:

```lyte
main {
    var f = |x| {
        var result = x + 1    // multiple steps
        result                // last value is returned
    }
    assert(f(1) == 2)
}
```

### No-argument lambdas and `void`

A lambda with no parameters uses `| |` (with a space between the pipes). The `void` type represents "no value" — used for functions that take no arguments or return nothing:

```lyte
call(f: void -> i32) {
    f()
}

main {
    var x = 0
    call(| | x = 1)    // no-arg lambda: | | not ||
    assert(x == 1)
}
```

The type `void -> i32` means "a function that takes no arguments and returns an `i32`." Similarly, `void -> void` means a function that takes nothing and returns nothing.

### Closures

A **closure** is a lambda that *captures* variables from the surrounding code:

```lyte
main {
    var count = 0
    var inc = | | count = count + 1   // captures 'count'
    inc()
    inc()
    assert(count == 2)
}
```

Closures capture variables **by reference** — the closure points to the original variable. When `inc()` changes `count`, it changes the same `count` defined outside.

### Closures cannot be returned from functions

You cannot return a closure that captures variables from the surrounding scope. Lyte enforces this strictly — even indirect attempts are caught:

```
❌ closure with captured variables cannot be returned
   (captured addresses would dangle after the frame exits)
```

This applies directly:
```lyte
get_fn() -> void -> i32 {
    var x = 0
    return (| | x)    // ❌ x would no longer exist after get_fn returns
}
```

And also when the closure is stored in a variable first, or passed through another function — Lyte's escape analysis catches all of these cases.

If you need to preserve state across calls, use a global variable or a struct instead of a closure.

### Lambda type errors

Passing a lambda with the wrong return type produces a clear error. For example, passing a `void -> void` lambda where a `void -> i32` is expected:

```
❌ no solution for (() → void) → void == (() → i32) → void
```

The error shows both the expected and actual function signatures so you can see exactly what doesn't match.

---

## 13. Generics — Writing Flexible Code

Sometimes you want to write a function that works the same way for multiple types. For example, an "identity" function that just returns whatever you give it:

```lyte
id<T>(x: T) -> T { x }
```

The `<T>` is a **type parameter** — a placeholder for "whatever type you pass in." When you call `id(42)`, Lyte figures out that `T` must be `i32`. When you call `id(true)`, `T` becomes `bool`. You write the function once, and it works for any type:

```lyte
main {
    assert(id(42) == 42)
    assert(id(true) == true)
}
```

### N as a generic array size

Type parameters don't have to be types — they can also be **array sizes**. This lets you write functions that work on arrays of any size:

```lyte
sum<N>(a: [i32; N]) -> i32 {
    var s = 0
    for i in 0 .. N {
        s = s + a[i]
    }
    return s
}

main {
    assert(sum([1, 2, 3]) == 6)
}
```

Here `N` is inferred from the array you pass in. You can also use `N` directly as a value in loops (`for i in 0 .. N`), not just as a type. You can also index up to `N - 1`:

```lyte
last<T, N>(a: [T; N]) -> T {
    a[N - 1]
}

main {
    var a: [i32; 128]
    for i in 0 .. 128 {
        a[i] = 1
    }
    assert(sum(a) == 128)
    assert(last(a) == 1)
}
```

### The `map` function — a fuller example

`map` is a classic generic function that applies a function to every element of an array and returns a new array. It uses three type parameters:

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
```

- `T0` — the input element type
- `T1` — the output element type
- `N` — the array size

You can call it with a named function, a lambda, or a stored lambda:

```lyte
add_one(x: i32) -> i32 { x + 1 }

main {
    let a = map([1, 2, 3], add_one)      // named function
    assert(a == [2, 3, 4])

    let b = map([1, 2, 3], |x| x + 2)   // inline lambda
    assert(b == [3, 4, 5])

    let f = |x| x + 3
    let c = map([1, 2, 3], f)            // stored lambda
    assert(c == [4, 5, 6])
}
```

This also confirms that **same-size array equality works** — `a == [2, 3, 4]` is valid.

### Generic structs

Structs can also be generic:

```lyte
struct Wrapper<T> {
    value: T
}

main {
    var wi: Wrapper<i32>
    wi.value = 42
    assert(wi.value == 42)

    var wa: Wrapper<[i32; 8]>   // T can even be an array type
    for i in 0 .. 8 {
        wa.value[i] = i
    }
    assert(wa.value[7] == 7)
}
```

You can instantiate a generic struct with any type — including array types. Generic structs can also be used as function parameters and return types:

```lyte
struct GenericStruct<T> {
    x: T
}

return_generic_struct() -> GenericStruct<i32> {
    var s: GenericStruct<i32>
    s.x = 42
    return s
}

generic_struct_check<A>(s: GenericStruct<A>, f: GenericStruct<A> -> i32) {
    assert(f(s) == 42)
}

main {
    var s = return_generic_struct()
    assert(s.x == 42)
    generic_struct_check(s, |s| s.x)   // lambda taking a generic struct
}
```

### Unconstrained generics can't use concrete operators

A type parameter `T` without any interface constraint doesn't support operations like `==` or `+` — Lyte doesn't know what type `T` will be, so it can't know how to compare or add it:

```
❌ no solution for T == i32
    t == 0
      ^
```

This comes from trying to compare a generic `T` directly with `0`. To use operators on a generic type, you need an interface constraint (see Section 14).

> *Aside — how generics work under the hood: Lyte doesn't keep generic functions generic at runtime. Instead, it automatically generates a separate concrete version for each type combination you actually use. So `id<T>` called with both `i32` and `bool` becomes two separate functions internally. This process is called **monomorphization** (from "mono" meaning one, and "morph" meaning form — each version has one fixed form). The practical benefit is zero runtime cost — generics are as fast as if you had written separate functions by hand.*

---

## 14. Interfaces — Rules for Generics

Generics are powerful, but sometimes you need to say "this generic type has to support certain operations." That's what **interfaces** are for.

An interface defines a set of functions that a type must have. This allows Lyte to check at compile time — before your program runs — that the type you're using actually supports the operations you need. This is sometimes called **static dispatch**: Lyte figures out which specific function to call based on the type, during the compile step, rather than having to figure it out while running.

```lyte
interface Compare<A> {
    cmp(lhs: A, rhs: A) -> i32
}
```

This says: "Any type `A` that implements `Compare` must have a `cmp` function that takes two values of type `A` and returns an `i32`."

### Using interfaces with `where`

You can then write a generic function that requires the type to implement an interface. Here's a complete working bubble sort:

```lyte
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
```

The `where Compare<T>` part means "this only works for types `T` that have the `cmp` function defined." Inside the function body, `cmp` can be called directly — Lyte knows it exists because of the constraint.

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

The `cmp` function returns a negative number if `lhs < rhs`, zero if equal, and positive if `lhs > rhs` — a standard comparison convention. Here `lhs - rhs` handles all three cases for integers.

### Why interfaces matter for DSP

In audio work, you might define interfaces like `Processable` (requiring a `process` function) or `Scalable` (requiring `__mul`) to write generic signal processing functions that work across multiple custom types. The constraint system ensures Lyte catches mismatches at compile time rather than producing wrong audio at runtime.

---

## 15. A Real-World Example: Biquad Filter

This is where Lyte really shines for Audulus users. A **biquad filter** is a fundamental building block in audio DSP — it's used for low-pass filters, high-pass filters, EQs, and much more. Here's the basic Lyte shape.

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
    var alpha = sin(w0) / (2.0 * q)
    var cs = cos(w0)
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
- `sin` and `cos` are built-in math functions — see Section 17 for the full list

### Processing a sample

The core calculation a biquad filter performs on each incoming audio sample is:

```
y = b0*x + b1*x1 + b2*x2 - a1*y1 - a2*y2
```

Where `x` is the current input sample, `x1` and `x2` are the two previous input samples, and `y1` and `y2` are the two previous output samples. Each call to this helper function computes one sample of output, then shuffles the history values along by one step ready for the next sample.

In Lyte:

```lyte
step_biquad(bq: Biquad, x: f32) -> (Biquad, f32) {
    var y = bq.b0*x + bq.b1*bq.x1 + bq.b2*bq.x2
              - bq.a1*bq.y1 - bq.a2*bq.y2
    bq.x2 = bq.x1
    bq.x1 = x
    bq.y2 = bq.y1
    bq.y1 = y
    (bq, y)
}
```

This helper takes a `Biquad` state and one input sample `x`, and returns a **tuple**: the updated filter state and the output sample `y`. This is a clean way to express the algorithm in plain Lyte code.

**Audulus note:** in real Audulus DSP nodes, the most reliable shape is often simpler than this example:
- keep the saved filter state in plain global `f32` variables
- loop over `for i in 0 .. frames` inside `process`
- use the block buffers (`input[i]`, `output[i]`) directly

So this section is best read as the language idea behind a biquad. For practical Audulus node code, the quickstart examples are the better reference.

> *Aside — why this runs efficiently: the multiply-add chains in the biquad formula (like `b0*x + b1*x1`) map directly to a hardware instruction called FMA, or fused multiply-add, which performs a multiplication and an addition in a single step rather than two. Lyte's compiler targets these instructions automatically when generating code for this kind of expression, which is one reason DSP code in Lyte can run efficiently.*

This example brings together structs, `f32` math, tuples, and functions — all of the major concepts covered in this tutorial.

> *Performance note: Lyte's repository includes a benchmark that runs this exact filter — 10 million samples of a 440 Hz sine wave through a 1 kHz lowpass biquad — and compares the results against C, Lua 5.5, and LuaJIT. It's designed to measure how close Lyte's performance gets to C for a representative DSP workload.*

---

## 16. Quick Reference

### Variable declaration

| Syntax | Meaning |
|--------|---------|
| `var x = 42` | Mutable variable, type inferred |
| `let y = 3.14` | Immutable (fixed) variable, type inferred |
| `var x: f32` | Mutable variable, explicit type, no value yet |

### Basic types

| Type | Description |
|------|-------------|
| `i32` | Whole number |
| `f32` | Decimal number |
| `bool` | `true` or `false` |
| `str` | Text in double quotes |
| `i8` | Very small whole number (used for characters) |
| `u32` | Whole number, cannot be negative |
| `f64` | Higher-precision decimal — present in the language but may not apply in Audulus |

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
| `\| \| count = count + 1` | No-arg closure (space between pipes) |

### Collections

| Syntax | Meaning |
|--------|---------|
| `[1, 2, 3]` | Array literal |
| `[0.0; 64]` | Repeat literal — array of 64 elements all set to `0.0` |
| `var a: [f32; 64]` | Fixed array of 64 floats, declared with a type |
| `[f32]` | Slice (flexible array view, in function parameters) |
| `(1, 2.0)` | Tuple |

**The repeat literal** `[0.0; 64]` is a handy shorthand — instead of declaring an array and filling it with a loop, you can create it already filled with a single value in one step.

### Operators

| Operator | Meaning |
|----------|---------|
| `+`, `-`, `*`, `/` | Arithmetic |
| `%` | Modulo (remainder after division) |
| `^` | Power (raise to an exponent) |
| `==`, `!=` | Equality / inequality |
| `<`, `>`, `<=`, `>=` | Comparison |
| `&&` | And |
| `\|\|` | Or |
| `!` | Not (prefix) |
| `as` | Convert a value from one type to another |

**Modulo (`%`)** gives you the remainder left over after a division. For example, `10 % 3` is `1`, because 10 divided by 3 is 3 with 1 left over. In DSP this is useful for things like wrapping a value around a range — keeping a phase counter cycling between 0 and some maximum, for instance.

**Power (`^`)** raises a number to an exponent. For example, `2 ^ 8` is 256 (2 multiplied by itself 8 times).

### Operator precedence

When you write an expression with multiple operators, Lyte follows a defined order for which operations happen first. This is called **operator precedence**. The following table is taken directly from the official Lyte grammar, from lowest priority (evaluated last) to highest (evaluated first):

| Priority | Operators | Notes |
|----------|-----------|-------|
| 1 — lowest | `=` | Assignment |
| 2 | `\|\|` `&&` | Logical and/or |
| 3 | `==` `!=` | Equality checks |
| 4 | `<` `>` `<=` `>=` | Comparisons |
| 5 | `+` `-` | Addition and subtraction |
| 6 | `*` `/` `%` | Multiplication, division, modulo |
| 7 | `^` | Power / exponent |
| 8 | `-` `+` `!` (prefix) | Unary operators (negation, not) |
| 9 — highest | `()` `[]` `.` `as` | Function calls, indexing, field access, type conversion |

The following examples are taken directly from Lyte's test suite, so they are confirmed correct:

```lyte
// multiplication happens before addition
assert(2 + 3 * 4 == 14)

// division happens before subtraction
assert(10 - 6 / 2 == 7)

// modulo has the same precedence as multiplication
assert(2 + 10 % 3 == 3)

// parentheses override precedence — evaluated first
assert((2 + 3) * 4 == 20)

// when precedence is equal, evaluation goes left to right
assert(10 - 3 - 2 == 5)

// mixed multiplication and division, left to right
assert(12 / 3 * 2 == 8)

// comparisons are evaluated after all the arithmetic is done
assert(2 + 3 == 1 + 4)

// parentheses can be nested
assert((2 + 3) * (4 - 1) == 15)
```

When in doubt, use parentheses — they always make the intent clear and override any precedence rules.

---

## 17. Standard Library

Lyte comes with a small set of built-in functions. These fall into three groups: low-level output functions built into the language itself, math functions also built into the language, and string utility functions defined in `stdlib.lyte`.

### Character and output functions (built into the language)

**`putc(x: i32)`**

Outputs a single character to the console. The argument is the ASCII code of the character as an `i32`. Because character literals are `i8`, you need to convert them with `as i32` first:

```lyte
main {
    var x = 'A'
    putc(x as i32)   // prints: A
    putc(10)         // prints a newline (ASCII 10)
}
```

Calling `putc(10)` is how you move to the next line — `10` is the ASCII code for the newline character. You can also write this as `putc('\n' as i32)`.

#### Character literals and escape sequences

Character literals are written with single quotes and have type `i8`. The following escape sequences are supported:

| Literal | Meaning | ASCII value |
|---------|---------|-------------|
| `'A'` .. `'Z'`, `'a'` .. `'z'` | Letters | 65–90, 97–122 |
| `'0'` .. `'9'` | Digit characters | 48–57 |
| `'\n'` | Newline | 10 |
| `'\\'` | Backslash | 92 |

Because character literals are `i8`, you can compare them directly to `i8` values using their ASCII codes:

```lyte
assert('0' == 48 as i8)    // digit zero is ASCII 48
assert('\n' == 10 as i8)   // newline is ASCII 10
assert('\\' == 92 as i8)   // backslash is ASCII 92
```

#### String literals are mutable character arrays

A string literal like `"hello"` creates a mutable `[i8]` array. You can index into it, read characters, and reassign individual elements:

```lyte
main {
    var buf = "hello"
    assert(buf[0] == 'h')
    assert(buf[1] == 'e')
    buf[0] = 'H'            // mutate in place
    assert(buf[0] == 'H')
    putc(buf[0] as i32)     // prints: H
    putc(10)
}
```

Note for Lua users: Lua strings cannot be changed in place. In Lyte, a string is just an array of `i8` values, so you can read and write individual characters.

### Math functions

Lyte math comes from two places:

- core builtins that are part of the language itself, such as `sin`, `cos`, `tan`, `atan2`, `sqrt`, `floor`, and `ceil`
- helper functions from `stdlib.lyte`, such as `clamp`, `mix`, `fract`, `mod`, `step`, and `smoothstep`

The function names follow the same unsuffixed style used by the Audulus Expr node: `sin`, `cos`, `atan2`, and so on.

Unary math functions (`f32`/`f64` in, same type out):

| Function | What it does |
|----------|--------------|
| `sin(x)` | Sine of `x` (angle in radians) |
| `cos(x)` | Cosine of `x` (angle in radians) |
| `tan(x)` | Tangent of `x` (angle in radians) |
| `asin(x)` | Arcsine of `x` |
| `acos(x)` | Arccosine of `x` |
| `atan(x)` | Arctangent of `x` |
| `sinh(x)` | Hyperbolic sine |
| `cosh(x)` | Hyperbolic cosine |
| `tanh(x)` | Hyperbolic tangent |
| `asinh(x)` | Inverse hyperbolic sine |
| `acosh(x)` | Inverse hyperbolic cosine |
| `atanh(x)` | Inverse hyperbolic tangent |
| `ln(x)` | Natural logarithm |
| `exp(x)` | *e* raised to the power `x` |
| `exp2(x)` | `2^x` |
| `log10(x)` | Base-10 logarithm |
| `log2(x)` | Base-2 logarithm |
| `sqrt(x)` | Square root |
| `abs(x)` | Absolute value |
| `floor(x)` | Round down to the nearest whole number |
| `ceil(x)` | Round up to the nearest whole number |

Binary math functions (`f32`/`f64` in, same type out):

| Function | What it does |
|----------|--------------|
| `pow(x, y)` | `x` raised to the power `y` |
| `atan2(y, x)` | Two-argument arctangent |
| `min(x, y)` | Smaller of the two values |
| `max(x, y)` | Larger of the two values |

Unary predicates (return `i32`, where nonzero means true):

| Function | What it does |
|----------|--------------|
| `isinf(x)` | Returns nonzero if `x` is infinite |
| `isnan(x)` | Returns nonzero if `x` is NaN |

This list is confirmed against the compiler and test suite. A few notes:

- `ln` and `exp` are inverses: `ln(exp(x))` gives back `x`.
- `atan2` takes **two** arguments — `y` first, then `x`. This is the standard convention for two-argument arctangent, useful for converting Cartesian coordinates to an angle.
- `isinf` and `isnan` currently return an integer flag rather than a `bool`, so the tests use checks like `isnan(x) != 0`.
- `pi` is not a built-in constant in Audulus Lyte. If you want it, define your own `var pi: f32` and set it to `3.14159265`.
- These map directly to compiler/runtime intrinsics, so they're fast.

Here are some common math helpers from `stdlib.lyte`:

| Function | What it does |
|----------|--------------|
| `fract(x)` | Fractional part of `x` |
| `mod(x, y)` | Floating-point modulo |
| `clamp(x, lo, hi)` | Clamp `x` into a range |
| `step(edge, x)` | `0` below the edge, `1` at or above it |
| `smoothstep(edge0, edge1, x)` | Smooth curve between two edges |
| `mix(a, b, t)` | Blend from `a` to `b` by amount `t` |

One small difference from the Audulus Expr node: Lyte uses `mix(a, b, t)`, while Audulus Expr uses `mix(x, a, b)`.

### String functions (from `stdlib.lyte`)

As covered in the character section above, strings in Lyte are arrays of `i8` — so when you see `[i8]` as a parameter type, it means "a string." The `as` keyword converts between types, so `s[i] as i32` turns an `i8` character into a regular integer, which is sometimes needed for arithmetic.

With that in mind, here are the available string functions:

---

**`println(s: [i8])`**

Prints a string to the output, followed by a line break. This is the function used in the Hello World example. It steps through each character in the string and outputs it, skipping any null (zero) characters that signal the end of the string.

---

**`print(n: i32)`**

Prints an integer directly to the output, without a line break. Unlike `println`, this takes a number rather than a string — no buffer or conversion needed:

```lyte
main {
    var sum = 0
    for x in 0 .. 42 {
        sum = sum + x
    }
    print(sum)   // prints the number directly
}
```

---

**`strcpy(dst: [i8], src: [i8])`**

Copies one string (`src`) into another (`dst`). If the source is shorter than the destination, the remaining space is filled with zeros. Useful when you need to duplicate or move text between string variables.

*(The name comes from "string copy" — a naming convention borrowed from the C programming language, which Lyte draws on for its low-level string handling.)*

---

**`strlen(s: [i8]) -> i32`**

Returns the number of characters in a string, not counting any trailing zeros. In other words, it tells you how long the string is.

---

**`itoa(dst: [i8], n: i32)`**

Converts a whole number into its text form and writes it into `dst`. For example, the number `42` becomes the string `"42"`. The name is short for "integer to array." Useful when you want to display a number as part of a printed message.

---

**`ftoa(dst: [i8], n: f32)`**

Converts a decimal number into its text form and writes it into `dst`. It always writes six decimal places. For example:

```lyte
main {
    var buf: [i8; 16]
    ftoa(buf, 3.141592)   // buf becomes "3.141592"
    println(buf)
    ftoa(buf, -1.5)       // buf becomes "-1.500000"
    println(buf)
    ftoa(buf, 42.0)       // buf becomes "42.000000"
    println(buf)
}
```

The name is short for "float to array." Like `itoa`, this is useful for printing numeric values as readable text. Note that the buffer must be large enough to hold the result — `[i8; 16]` is a safe size for typical values.

---

**`strcat(dst: [i8], src: [i8])`**

Appends one string onto the end of another. For example, if `dst` contains `"hello "` and `src` contains `"world"`, after calling `strcat` the `dst` will contain `"hello world"`. The name is short for "string concatenate" — concatenate just means to join two things end to end.

---

### A note on what's not here

The stdlib is still fairly small, but it does include useful math helpers as well as string utilities. Core functions like `sin`, `cos`, and `sqrt` are handled directly by the compiler, while helpers like `clamp`, `mix`, and `smoothstep` live in `stdlib.lyte`. `putc` is also built in, sitting one level below `println`.

---

## 18. Macros

A **macro** is a reusable block of code that gets substituted directly into the call site — like a find-and-replace that happens before your code compiles. Unlike a function, a macro doesn't create a new scope or have a return value; the code is literally inserted where you call it, with your arguments swapped in.

Macros are defined with the `macro` keyword and called with the `@` symbol:

```lyte
macro inc(x) {
    x = x + 1
}

main {
    var c = 10
    @inc(c)
    assert(c == 11)
}
```

When Lyte sees `@inc(c)`, it expands it to `c = c + 1` before compiling. The variable `c` is modified directly — there's no copy, no return value to assign.

### Why macros instead of functions?

The key difference is that macros can modify their arguments in place. A regular function receives a *copy* of its arguments, so changes inside the function don't affect the original. A macro expands inline, so it works directly on whatever you pass.

A good example is `swap` — you can't write a swap function in Lyte (because you'd only get copies), but you can write a swap macro:

```lyte
macro swap(x, y) {
    let tmp = x
    x = y
    y = tmp
}

main {
    var a = 1
    var b = 2
    @swap(a, b)
    assert(a == 2)
    assert(b == 1)
}
```

After `@swap(a, b)` expands, it's as if you wrote:

```lyte
    let tmp = a
    a = b
    b = tmp
```

The original variables `a` and `b` are swapped directly.

### Macro rules

- Macros cannot be overloaded (you can't define two macros with the same name)
- Macros expand before code generation, so they work identically in both the JIT and VM backends
- The `@` prefix at the call site makes it clear you're calling a macro, not a function

---

## 19. Common Errors

Lyte's error messages follow a consistent pattern. Understanding them makes it much easier to fix mistakes quickly.

### Reading error messages

All type errors use the same form:

```
❌ filename.lyte:line:col: no solution for X == Y
```

The `==` here is Lyte's internal notation for "these two types must agree" — it doesn't mean the `==` operator. It means Lyte tried to unify two types and couldn't. The caret (`^`) points to where in your code the conflict was detected.

### `if` and `while` require a boolean condition

Lyte will not accept a number or other non-boolean value as a condition:

```
❌ no solution for i32 == bool
    if 42 { }
       ^
```

Unlike some languages (where any non-zero value counts as "true"), Lyte requires the condition to be explicitly `bool`. Write `if x != 0 { }` instead of `if x { }`.

This applies equally to `while`:

```
❌ no solution for i32 == bool
    while 42 { }
          ^
```

### Wrong argument types in a function call

If you call a function with arguments of the wrong type, Lyte reports the mismatch between the function's expected signature and what you provided:

```
❌ no solution for (i32, i32) → i32 == (f32, f32) → i32
    add(1.0, 2.0)
    ^
```

The error shows both the expected signature and the inferred signature from your call. Here `add` expects `(i32, i32)` but received `(f32, f32)`.

### Mixed types in an array literal

All elements of an array must be the same type. Mixing `i32` and `f32` is an error:

```
❌ no solution for i32 == f32
    var x = [1, 2.0]
                ^
```

Either use all integers (`[1, 2, 3]`) or all floats (`[1.0, 2.0, 3.0]`).

### `for` ranges must use integers

The `..` range operator requires integer bounds. Using floats is an error:

```
❌ no solution for f32 == i32
    for i in 1.0 .. 10.0 { }
             ^
```

Use `for i in 1 .. 10 { }` instead. If you need to iterate with a float step, use a `while` loop with manual increment.

### Wrong type on assignment

Any type mismatch on assignment — not just `i32`/`f32` — produces the same error form. For example, assigning a `bool` to an `f32`:

```
❌ no solution for f32 == bool
    x = true
      ^
```

The left side of `==` in the message is always the *expected* type (the variable's declared type); the right side is the *actual* type of the value you tried to assign.

### Inline `if` expressions can cause misleading parser errors

In Lyte, block-form inline `if` expressions are not reliable in every context. Patterns like these can cause a long chain of parser errors:

```lyte
let x = if cond {
    a
} else {
    b
}
```

```lyte
output[i] = if cond {
    a
} else {
    b
}
```

You may see messages like:

```
Expected expression
Expected declaration, got Let
Expected declaration, got Assign
Expected declaration, got Lbracket
```

The safer pattern is:

```lyte
var x = b
if cond {
    x = a
}
```

Or for an output:

```lyte
output[i] = b
if cond {
    output[i] = a
}
```

This is a little more verbose, but it is much more dependable in current Lyte.

### Calling an undefined macro

Unlike unknown functions (which produce a type error), calling a macro that doesn't exist produces a distinct message:

```
❌ unknown macro: unknown
    @unknown(1)
    ^
```

If you see this, check the spelling of your macro name — and remember that macros are called with `@`, not without it.

### Returning a slice from a function

Slices can only be used as function parameters, not return types:

```
❌ slice type [i32] is not allowed as a return type
    f(a: [i32]) -> [i32] { a }
```

Use a fixed-size array (`[i32; N]`) as the return type instead.

### Lambda return type mismatch

When a lambda has the wrong return type for a function parameter, the error shows both the expected and actual signatures:

```
❌ no solution for (() → void) → void == (() → i32) → void
    call_void(| | x = 1)
    ^
```

Here `call_void` expects a `void -> void` function, but the lambda `| | x = 1` returns `i32` (the result of the assignment). The fix is to make sure the lambda's return type matches what the function expects.

### Returning a closure that captures variables

Attempting to return a closure that captures local variables always fails, even through indirection:

```
❌ closure with captured variables cannot be returned
   (captured addresses would dangle after the frame exits)
```

Use a global variable or struct to hold state that needs to outlive a function call.

---

### Using `typevar` outside of generics

The keyword `typevar` appears in Lyte's internal type system but is not valid in user-written struct definitions:

```
❌ unknown type variable: T
    x: typevar T
    ^
```

If you want a generic struct, declare the type parameter in the struct name: `struct Wrapper<T>`, then use `T` directly as a field type — not `typevar T`.

---

## 20. Features in the Grammar Not Yet Covered

The official Lyte grammar file reveals a few features that exist in the language but aren't documented in the README and aren't yet covered in this tutorial. They're listed here so you're aware they exist, without going into detail that could become outdated as Lyte develops.

**`arena`** — reserved for future use in the current Lyte repo. If you try to use it today, the compiler reports `arena is reserved for future use`.

**`fn` keyword** — the grammar shows that `fn` is an optional keyword you can place before a function name. So `fn add(a: i32, b: i32) -> i32 { ... }` is valid, as is `add(a: i32, b: i32) -> i32 { ... }`. Both are the same — the keyword is optional.

**Unicode operator alternatives** — the grammar defines a few alternate ways to write operators using special characters:
- `→` (the Unicode right-arrow character) can be used instead of `->`
- `⟨` and `⟩` can be used instead of `<` and `>` for type parameters
- `⋅` (a centered dot) can be used instead of `*` for multiplication

These are cosmetic alternatives — the standard ASCII versions work just as well and are easier to type.

---

*This tutorial was written as a companion to the official Lyte README and grammar.*
*Source: https://github.com/audulus/lyte*
