# TODO

## Language Features

### String Support
- [ ] Add `Type::String` runtime representation
- [ ] Implement string literal codegen in JIT (`jit.rs` — currently panics at line 1194)
- [ ] Implement string literal codegen in VM (`vm_codegen.rs` — no handling at all)
- [ ] Add string operation support (concatenation, length, indexing)
- [ ] Add string test cases

### Match Expressions / Pattern Matching
- [ ] Design grammar for match expressions
- [ ] Add parser support
- [ ] Implement type checking for match exhaustiveness
- [ ] Implement JIT codegen
- [ ] Implement VM codegen
- [ ] Add test cases

### Macros
- [ ] Implement macro support in VM backend (only works in JIT currently)
- [ ] Add VM macro test cases (`macro_vm.lyte`)
- [ ] Consider overloaded macros (`checker.rs:443` — `XXX: overloaded macros?`)

## Backend Parity (JIT vs VM)

- [ ] Audit all `Expr` variants handled in JIT but missing from VM
- [ ] Macro support in VM (see above)
- [ ] Verify character literal support in VM codegen

## Error Handling

### Replace panics with proper errors
- [ ] `jit.rs:1194` — unimplemented expression panic (should be a compiler error)
- [ ] `jit.rs:1470` — unimplemented binary operation panic
- [ ] `jit.rs:512, 538, 676, 749, 779` — "should be caught by type checker" panics
- [ ] `decl.rs:138` — field offset panic if field not found
- [ ] `types.rs:17-21` — size calculation panics on size variables
- [ ] `array_checker.rs:210, 234` — array checking panics
- [ ] `compiler.rs:219` — panic when `main` not found (should be a user-facing error)
- [ ] `vm.rs:658, 666` — division by zero panics (should be runtime error)
- [ ] `vm_codegen.rs:1795` — hack comment for high register usage

### General
- [ ] Move toward `Result<T, E>` instead of panics throughout the pipeline
- [ ] Improve error recovery in the parser (currently stops at first error in many cases)

## Code Quality

- [ ] `vm_codegen.rs:541-544` — function ID loading emits `LoadImm 0` as placeholder
- [ ] `jit.rs:401` — empty panic with no message
- [ ] Clean up vague panic messages throughout codebase

## Test Coverage

- [ ] String literals (0 tests)
- [ ] Macro VM tests (no `*_vm.lyte` variants)
- [ ] Error recovery tests
- [ ] Boundary condition tests (large arrays, deeply nested types)

## Design (Long-term)

- [ ] Unboxed function closures — compute max closure size per signature (noted as not planned in `src/README.md`)
- [ ] Safe transmutation — restrict type system to make transmute safe (noted as not planned in `src/README.md`)
- [ ] Optimization passes between AST and codegen
