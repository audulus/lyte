# TODO

## Language Features

### String Support
- [x] Add string operation support (concatenation, length, indexing)
- [x] Add string test cases

### Match Expressions / Pattern Matching (NOT PLANNED)

While pattern matching is very powerful, we're going to leave it out of 1.0.

- [ ] Design grammar for match expressions
- [ ] Add parser support
- [ ] Implement type checking for match exhaustiveness
- [ ] Implement JIT codegen
- [ ] Implement VM codegen
- [ ] Add test cases

### Macros
- [x] Implement macro support in VM backend (macros expand before codegen, works in both backends)
- [x] Add VM macro test cases (`macro_vm.lyte`)
- [x] Disallow overloaded macros

## Backend Parity (JIT vs VM)

- [x] Audit all `Expr` variants handled in JIT but missing from VM
- [x] Macro support in VM (see above)
- [x] Verify character literal support in VM codegen

## Error Handling

Panics are ok because they are caught and turned into ICE

### General
- [ ] Improve error recovery in the parser (currently stops at first error in many cases)

## Code Quality

- [x] Clean up vague panic messages throughout codebase

## Test Coverage

- [x] String literals
- [x] Macro VM tests (`macro_vm.lyte`)
- [ ] Error recovery tests
- [x] Boundary condition tests (large arrays, deeply nested types)

## Design (Long-term)

- [ ] Unboxed function closures — compute max closure size per signature (noted as not planned in `src/README.md`)
- [ ] Safe transmutation — restrict type system to make transmute safe (noted as not planned in `src/README.md`)
- [ ] Optimization passes between AST and codegen
