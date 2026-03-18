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

  2. Differential fuzz the full pipeline at the Lyte source level — the existing differential fuzzer generates simple programs (int arithmetic, for loops). Extending the program generator to emit structs, arrays, enums, and function calls
  would catch codegen bugs the current generator can't reach.
  4. Round-trip the type checker — fuzz with random valid ASTs, type-check, serialize the checked AST, re-parse and re-check, verify same result. Would catch inconsistencies between the parser and checker.
  5. Fuzz the packed bytecode encoding — generate random Vec<Opcode>, pack into LinkedProgram, unpack back, verify round-trip. The packing has edge cases (wide variants, 2-word instructions, jump offset fixups) that could silently corrupt
  programs.