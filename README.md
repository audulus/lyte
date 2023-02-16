# lyte

![build status](https://github.com/audulus/lyte/actions/workflows/rust.yml/badge.svg)

A simple programming langauge for writing Audulus nodes, and maybe other things too :)

Goals:

- "dynamic ownership" memory management, no GC
- function and operator overloading
- generics constrained by "interfaces"
- familiar syntax (a mix of rust and swift)
- arena allocation for realtime
- memory safety
- type inference
- incremental query-based compiler
- LLVM backend
- VM for environments where you can't JIT (iOS)
