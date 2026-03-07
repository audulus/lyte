# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Lyte is a simple programming language designed for writing Audulus nodes. It features:
- Function and operator overloading  
- Generics constrained by "interfaces"
- Familiar syntax (mix of Rust and Swift)
- Type inference with Hindley-Milner style checking
- Cranelift JIT backend

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
# Compile and run a single file
cargo run --bin lyte <file.lyte> -c

# Compile and run a single file with the VM
cargo run --bin lyte <file.lyte> -r

# Parse and show AST
cargo run --bin lyte <file.lyte> --ast

# Show IR output
cargo run --bin lyte <file.lyte> --ir

# Process entire directory
cargo run --bin lyte <directory/> --c
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
- **Array Checker** (`src/array_checker.rs`) - Static bounds checking for arrays
- **Compiler** (`src/compiler.rs`) - Orchestrates compilation pipeline
- **JIT** (`src/jit.rs`) - Cranelift-based code generation and execution

### Type System
- **Types** (`src/types.rs`) - Type representations and utilities
- **Solver** (`src/solver.rs`) - Constraint solver for type inference
- **Declaration Table** (`src/decl_table.rs`) - Symbol table management

### Key Design Principles
- No recursive data structures (use arrays with indices instead)
- Dynamic ownership with runtime "owned" bit rather than compile-time ownership
- Static array bounds checking via abstract interpretation
- Safe memory transmutation by restricting type system
- Unboxed function closures with computed max closure sizes

### Test Structure (Golden Tests)
Tests use the `goldentests` crate (v1.4.1). The test runner is in `cli/tests/cli.rs` and invokes the `../target/debug/lyte` binary against all `.lyte` files in `tests/cases/`. You must `cargo build` before running tests since the binary is invoked directly.

Each test file uses comment directives:
- `// args: <flags>` — CLI args (e.g., `-c` to compile, `-r` for VM)
- `// expected stdout:` — followed by `// <line>` lines for expected output
- Omit `args:` for error tests (binary runs with just the file path)

Example passing test:
```
// args: -c
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