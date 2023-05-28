
# Notes

## No recursive data structures

No recursive data structures are allowed. In order to create recursive data, use an array and indices to other elements of the array. This simplifies memory management, though more complicated data structures will need their own allocators for array elements.

## Dynamic Ownership

Instead of ownership of arrays being expressed in the type system, it is expressed dynamically using an "owned" bit, which could be packed into a pointer. So an array (or slice) is represented as a pointer to the beginning, the length, and a bit indicating ownership. Slices are never owned. If an array is owned, it is deallocated when it goes out of scope. Ownership may be transferred if the array is passed to a function before it goes out of scope. This may elide more copies than C++ or Rust, and is very cheap to determine at runtime.

## Interfaces

Interfaces are used to provide functions that must exist when another function is called. Typically this is used to constrain generics. It can also be used for a static form of dependency injection, if the interface is not itself generic.

## Static array bounds checking

Static analysis (probably via abstract interpretation) should be used to ensure that array indexing is in bounds. If the compiler fails to prove the index is in bounds, the programmer can put a conditional around the array access.

## Safe transmutation of memory

In rust, `transmute` is unsafe. Perhaps we can restrict our type system such that transmute is always safe (no pointers, etc.). With safe transmutation, we could implement a memory allocator safely. This may conflict with dynamic ownership which would involve storing pointers in memory. Also, function pointers would have to
be stored as indices into function tables (each table would be all functions of a particular signature).

## Unboxed Function closures

To avoid having to use generics for closures, we can compute a max closure size for each function signature by analyzing the whole program.

## References

[Exponential time complexity in the Swift type checker](https://www.cocoawithlove.com/blog/2016/07/12/type-checker-issues.html)

[Hindley-Milner Inference](http://dev.stephendiehl.com/fun/006_hindley_milner.html)

[Swift Type Checker](https://github.com/apple/swift/blob/main/docs/TypeChecker.md)

[NP-Hard Overload Resolution](https://docs.microsoft.com/en-us/archive/blogs/ericlippert/lambda-expressions-vs-anonymous-methods-part-five)

[Bidirectional Typing](https://arxiv.org/pdf/1908.05839.pdf)

[Query-Based Compilers](https://ollef.github.io/blog/posts/query-based-compilers.html)

[Rust Analyzer Architecture](https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/architecture.md)

[Modern Parser Generator](https://matklad.github.io/2018/06/06/modern-parser-generator.html)

[Abstract Interpretation](https://www.youtube.com/watch?v=IBlfJerAcRw)

[Refinement Types](https://arxiv.org/pdf/2010.07763.pdf)

[Clang IR](https://facebookincubator.github.io/clangir/)

[Lessons from Writing a Compiler](https://borretti.me/article/lessons-writing-compiler)

[The Solid-State Register Allocator](https://www.mattkeeter.com/blog/2022-10-04-ssra/)

[Compilers and IRs](https://www.lei.chat/posts/compilers-and-irs-llvm-ir-spirv-and-mlir/)

[Cranelift JIT Demo](https://github.com/bytecodealliance/cranelift-jit-demo)

[MLIR Tutorial](https://llvm.org/devmtg/2020-09/slides/MLIR_Tutorial.pdf)
