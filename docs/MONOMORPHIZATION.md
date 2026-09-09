# Checked programs and specialization

Specialization consumes checked templates and produces concrete function/global
targets for safety analysis and code generation. The authoritative ownership,
lifecycle, editor and mutation rules are in the
[checked-program contract](CHECKED_PROGRAM.md).

```text
source syntax → CheckedProgram → SpecializedProgram → backend IR
                 checking       specialization,
                 and source     concrete safety,
                 safety         field hoisting,
                                final validation
```

`Compiler` retains checked templates alongside optional concrete output. Changing
effective entry points invalidates only concrete output and its diagnostics;
different roots can specialize without rechecking. Parsing invalidates all derived
results. Checking replaces templates; changes to validation policy such as
`no_recursion` require revalidation before execution. Code generation borrows a
successfully specialized program and consumes neither artifact.

`analyze()` runs the shared checker with partial editor publication. Its separate
`SourceAnalysis` owns recovered source facts for hover/navigation; incomplete facts
never serve as executable input. Parsed syntax remains available for diagnostics
and editing, but backends consume checked bodies.

## Bodies and identities

`CheckedBody` owns operation/type/location nodes, local records and interface
requirements. Binding types belong to `LocalId` records; a checked `let`/`var`
statement is `void` and has no remaining source annotation. Source and checked
expressions share syntax shape and child traversal with distinct reference,
binder and parameter payloads.

`DefId` identifies a checked definition, including an interface member.
`InstanceId` identifies a concrete function/global inventory entry. `ExprID`,
`LocalId` and `RequirementId` belong to one body. Equal numeric coordinates in
different owners are unrelated; spelling and symbol mangling are not semantic
identity. A size binder links its `LocalId` to the type-level `ArraySize::Var`
symbol, so diagnostic renaming does not affect size substitution.

`DeclTable` owns definition indexing over a `DeclarationList`. Concrete programs
own a declaration list and instance inventory. The common list provides nominal
layout and host symbol lookup without source identity APIs. Declaration sorting
preserves definition/instance identities by remapping storage coordinates.

## Specialization and publication

`MonomorphPass::monomorphize_multi` validates its checked input and starts from
entry definitions. It interns a `MonomorphKey` of definition, concrete type
arguments and size arguments, reserving an instance before visiting its body.
Recursive calls and repeated reachability share that instance. The recursion
guard still rejects increasingly complex recursive type specializations.

Templates can retain named generics, symbolic sizes and deferred interface
obligations, but no anonymous inference variables. `Reference::Functions` stores
ordered candidates, not a selected callee. Ordinary specialization retains its
inference/coercion and size-generic precedence rules over those recorded IDs.
Interface selection separately uses the first exact signature match, without
generic overload unification or coercions. Interface selections remain in the
owning body's specialization frame while recursive callees are instantiated.

Specialization substitutes node/local types, resolves size parameters, selects
candidates/requirements and rewrites nonlocal references to `InstanceId`s.
Generic globals use the same interning mechanism: repeated use of one concrete
global shares storage. Non-generic globals remain present even when unreachable.
Final sorting preserves the existing host layout order.

Fulfilled requirements, interfaces and macros are removed. Function/global
signatures and bodies are concrete, and all retained references are `Local` or
`Instance`, including nodes outside runtime roots. Generic struct definitions
remain for layout; concrete global assumptions remain body/condition records.
No specialized body returns to source syntax for another type check.

Fallible constructors validate structure and complete instance inventories;
origin validation checks definition kind and type/size argument counts against
the retained templates. All retained concrete function bodies then run through
the existing safety traversal, including ordinary callers of generics. Direct
function instances supply exact contracts without signature filtering. This
precedes field hoisting; structural validation runs again before compiler
publication. Failure retains templates and publishes no concrete program.

## Analyses and lowering

Safety owns its existing interval/constraint proofs. Identified storage roots
and fresh callee contexts prevent equal numeric locals in different bodies from
sharing facts. Indirect function values carry no direct-call contract, and
unsupported precondition syntax can still fail conservatively.

Field hoisting uses instance-keyed may-write summaries and accounts for borrowed/
global aliases, captured storage, transitive writes and opaque calls. Binding
identity alone proves neither disjoint memory nor safe code motion. Copy elision
retains its own liveness/escape rules. Mutating public checked data requires
validation and recomputation of affected analyses; coordinates and source
locations do not keep old results valid.

Backends use local/instance maps and shared ordered free-local discovery. They
retain representation recovery, implicit conversions, captured storage, closure
ABI and generated-lambda queues/patches. Extracted lambdas clone enclosing arenas;
they are not additional source instances. VM expression inlining switches the
complete callee body/storage context and restores its caller. Runtime cancellation
belongs to backend lowering; LLVM AOT retains its omission of callback cancellation.

## Maintaining the contract

Boundary tests live in `src/checked.rs` and `src/checked/validate.rs`; compiler
lifecycle/safety/assumption tests and LSP recovery tests exercise phase consumers.
The CLI golden corpus covers backend execution and emitted-code checks, with
per-case check-only flags and backend exclusions. Build the CLI before each
workspace golden run; the runner uses Cargo's `CARGO_BIN_EXE_lyte`, including with
a custom target directory:

```sh
cargo build --workspace
cargo test --workspace
cargo build --workspace --features llvm
cargo test --workspace --features llvm
cargo check --lib --no-default-features
```

LLVM requires the configured LLVM toolchain. Production C Stack runtime/golden
suites require the supported Clang-built interpreter; Rust StackVM tests are
separate. The assembly VM requires AArch64. AOT emission tests establish
object/header generation, not execution on the target device.
