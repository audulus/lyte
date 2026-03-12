# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Lyte is a simple programming language designed for writing Audulus nodes. It features:
- Function and operator overloading  
- Generics constrained by "interfaces"
- Familiar syntax (mix of Rust and Swift)
- Type inference with Hindley-Milner style checking
- Cranelift JIT backend
- VM backend for environments where JIT isn't allowed (iOS)

## Development Commands

### Building and Testing
```bash
# Build the project
cargo build --verbose

# Run all tests (includes both library and CLI tests)
cargo test --workspace

# Quick test script
./test.sh

# Build release version
cargo build --release
```

### Running the CLI
```bash
# Compile and run a single file (Cranelift JIT by default)
cargo run --bin lyte <file.lyte>

# Compile and run with the VM backend
LYTE_BACKEND=vm cargo run --bin lyte <file.lyte>

# Compile and run with the LLVM backend (requires llvm feature)
LYTE_BACKEND=llvm cargo run --bin lyte <file.lyte>

# Parse and type-check only (no compilation or execution)
cargo run --bin lyte <file.lyte> --check

# Parse and show AST
cargo run --bin lyte <file.lyte> --ast

# Show IR output
cargo run --bin lyte <file.lyte> --ir

# Show VM bytecode
cargo run --bin lyte <file.lyte> --bytecode

# Process entire directory
cargo run --bin lyte <directory/>
```

### Fuzzing
```bash
# Fuzz the lexer
cargo fuzz run lexer

# Fuzz the parser  
cargo fuzz run parser
```

## Architecture

The compiler is structured as a multi-pass system:

### Core Components
- **Lexer** (`src/lexer.rs`) - Tokenizes input
- **Parser** (`src/parser.rs`) - Builds AST from tokens
- **Type Checker** (`src/checker.rs`) - Performs type inference and constraint solving
- **Safety Checker** (`src/safety_checker.rs`) - Static safety checks (array bounds, division by zero)
- **Compiler** (`src/compiler.rs`) - Orchestrates compilation pipeline
- **JIT** (`src/jit.rs`) - Cranelift-based code generation and execution
- **VM** (`src/vm.rs`, `src/vm_codegen`, `VM.md`) - Virtual machine for iOS and code generation

### Type System
- **Types** (`src/types.rs`) - Type representations and utilities
- **Solver** (`src/solver.rs`) - Constraint solver for type inference
- **Declaration Table** (`src/decl_table.rs`) - Symbol table management

### Key Design Principles
- No recursive data structures (use arrays with indices instead)
- Static safety checking via abstract interpretation (`src/safety_checker.rs`)
- Interfaces for constraining generics and static dependency injection
- Escape analysis to prevent returning functions with closures

### Test Structure (Golden Tests)
Tests use the `goldentests` crate (v1.4.1). The test runner is in `cli/tests/cli.rs` and invokes the `../target/debug/lyte` binary against all `.lyte` files in `tests/cases/`. You must `cargo build` before running tests since the binary is invoked directly.

Each test file uses comment directives:
- `// args: <flags>` — CLI args (e.g., `--check` for parse-only, `--bytecode` for bytecode output)
- `// expected stdout:` — followed by `// <line>` lines for expected output
- Omit `args:` for tests that compile and run (default behavior)
- Use `// args: --check` for error tests that only parse and type-check

Example passing test:
```
// expected stdout:
// compilation successful
```

Test cases are organized in `tests/cases/` by feature:
- `arith/` - Arithmetic operations
- `arrays/` - Array operations and bounds checking
- `enums/` - Enum types
- `generics/` - Generic types and constraints
- `globals/` - Global variables
- `lambdas/` - Lambda expressions and closures
- `loops/` - While and for loops
- `slices/` - Slice operations
- `structs/` - Structure definitions and usage

### Benchmarks (`benchmark/`)
Biquad filter DSP benchmark comparing Lyte (JIT and VM) against C, Lua 5.5, and LuaJIT (JIT and interpreter). Processes 10M samples of a 440Hz sine wave through a 1kHz lowpass filter.

```bash
# Run the full benchmark suite (requires lua and luajit installed)
./benchmark/run.sh        # defaults to 5 runs averaged
./benchmark/run.sh 3      # 3 runs averaged
```

Files:
- `run.sh` — benchmark runner, builds everything and prints a comparison table
- `biquad.lyte` — Lyte version of the benchmark
- `biquad.lua` — Lua/LuaJIT version
- `biquad.c` — C reference implementation
- `hotloop_lyte.txt` / `hotloop_luajit.txt` — annotated bytecode for the hot loop (35 instructions each)
- `vm_arm64.dasc` — LuaJIT's hand-written ARM64 interpreter (reference for VM optimization)
- `LuaJIT_Instructions.md` — LuaJIT bytecode format reference

### Workspace Structure
- Root crate: Core lyte compiler library
- `cli/`: Command-line interface binary
- `fuzz/`: Fuzzing targets for lexer and parser

## Language Features

Key language constructs:
- Functions with optional type annotations
- Structs and enums
- Arrays with static bounds checking
- Generics with interface constraints
- Lambda expressions
- Conditional expressions (`if/else`)

Refer to `src/README.md` for detailed design notes and references on type inference, memory management, and compiler architecture.