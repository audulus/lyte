# Checked-program contract

The compiler records lexical identity, solved types and source locations once in
checked bodies. Specialization supplies concrete function/global targets, and
consumers retain their own analyses, storage and execution rules. The compiler
keeps checked templates alongside optional specialized output; editor recovery
uses a separate `SourceAnalysis` snapshot.

## Identity and ownership

| Handle | Owner and meaning |
| --- | --- |
| `ExprID` | A node coordinate in one `CheckedBody`. Shared coordinates do not imply a value snapshot or single evaluation. |
| `LocalId` | A binding in that body, introduced by a parameter, size binder, declaration, loop or lambda parameter. |
| `RequirementId` | A coordinate in that body's interface-requirement inventory; member references also record a `DefId`. |
| `DefId` | A definition in one declaration table, including separately indexed interface members, independent of sorted storage coordinates. |
| `InstanceId` | An entry in one specialized inventory, keyed by definition plus type and size arguments, independent of declaration sorting. |

Handles have no historical meaning across reparsing, checking or independent
owners. They contain no generation tag. Equal numeric local IDs in different
bodies are expected; callers must carry the owning body/program. Validation
checks coordinates against that owner, but cannot detect a caller substituting
an equal numeric handle from another owner. Names support diagnostics, entry
selection, host layout lookup and emitted symbols; they do not establish local
or concrete target identity.

Each `CheckedNode` owns its operation, result type and source location. Each
`Local` owns its binding type and mutability. Checked `let`/`var` nodes have a
`void` result and no source annotation, even when the binding stores an array.
Reading a reference parameter can have type `T` while its local record has type
`&T`; result, storage and target-signature types need not be identical.

## Publication and validation

`CheckedProgram::try_new` and `SpecializedProgram::try_from_instances` perform
fallible structural validation. Their `new`/`from_instances` convenience forms
panic on invalid input. Public data remains editable: constructing or mutating
it directly does not produce an immutable proof object or rerun type inference.

Phase-specific `validate()` checks all retained nodes, including nodes outside
runtime roots: roots/edges are in range and acyclic, referenced locals have
body-owned binders, binders and binding occurrences are unique, and references
and types obey their phase. Declaration tables validate interface-member
inventories; body requirements must belong to their recorded interfaces.
Preconditions and global assumption roots must be boolean. Calls need function
types and matching arity; direct function-instance calls also match the actual
target's parameter count. Validation does not repeat lexical resolution, choose
overloads, prove every typing rule or certify safety or transformation legality.

The compiler validates templates before specialization, concrete output and its
origins before safety/analysis, and the result again after hoisting. Direct
consumers of mutable public program data must validate before passing it to
another phase and reestablish any semantic or safety guarantees their edits affect.

## Checked templates

`Compiler::check()` publishes `CheckedProgram` only when all accumulated inputs
parse and all source bodies and prototypes pass type/resolution and structural
checking. A neighboring invalid function prevents whole-program publication.
Safety-only failures retain checked type facts, but `check()` returns false and
execution remains blocked. An independently constructed `CheckedProgram` only
has the structural guarantees of its constructor.

Template references identify locals, size parameters, global definitions, ordered
overload sets or members of recorded interface requirements. They contain no
instance references. Named generic types and symbolic array sizes are allowed;
anonymous inference variables are not. An unused generic template may retain an
interface obligation with no current implementation, including no candidates.

`Reference::Functions` is an overload set. Checking fixes candidate order from
source declaration order; specialization consumes it with the existing inference,
coercion and size-generic precedence rules, without repeating lexical lookup.
Candidates may include function-valued globals; borrowed-call checks use the
solved callee signature when the set contains no function declarations.
Interface selection has a separate policy: first exact signature match in the
recorded order. Generic implementations and reference/array-to-slice coercions do
not qualify merely because they unify. Its top-level `Var`/`Anon` deferral rule
does not permit anonymous inference variables in published templates.

## Specialized programs

Every retained function/global declaration has exactly one instance record, every
record owns one such declaration, and keys are unique. Function signatures,
locals, node types, embedded type arguments/annotations, instance type arguments
and global types contain no type variables or symbolic array sizes. Functions
and globals have no generic binders. Interface requirements are discharged and
removed; all retained references are `Local` or `Instance`.

An instance's inventory target is authoritative. Callable locals and
function-valued globals can still be indirect calls. Operand types and recorded
callee types may differ from the selected function's signature through permitted
coercions; backends still materialize implicit conversions.

The declaration list also retains generic struct layout definitions, enums,
constants and concrete global assumptions. These are not function/global
instances; struct fields need not be concrete. `ArraySize::Known(0)` retains its
existing unknown/unspecified-size convention. Interfaces and macro declarations
do not survive specialization. Non-generic globals remain present even when
unreachable, and host storage follows the existing sorted declaration order.

`validate_origins(templates)` checks each instance's source definition kind and
type/size argument counts. `Compiler` retains the owning template inventory, so
origin `DefId`s remain resolvable through `checked_program()`. Standalone concrete
validation cannot resolve origins; clients keeping concrete output independently
must also retain its templates if they need origin queries or validation.

## Compiler lifecycle and diagnostics

| Operation | Retained results and execution gate |
| --- | --- |
| `parse(contents, path)` | Appends an input and invalidates templates, concrete output and editor facts, even on failure. Clears derived diagnostics and retains parse errors from all inputs; its return value describes only the new input. |
| `check()` | Revalidates all inputs with current options, replaces templates and clears concrete/editor output. Parse/type failures publish no templates; safety-only failures retain type facts but block specialization. |
| `analyze()` | Uses the same checking pipeline with recovery and all-body diagnostic collection, replacing the editor snapshot and clearing concrete output. Only complete successful type checking can also publish templates. |
| Change effective entry points | Retains templates/editor facts; clears concrete output and specialization diagnostics. Empty roots and explicit `main` are equivalent; order remains significant. Missing entries retain the existing skip policy. |
| `specialize()` | Borrows templates; publishes only after specialization, concrete safety, hoisting and structural validation succeed. Failure publishes no output and leaves templates usable for retry or different roots without rechecking. |
| Compile a backend | Borrows concrete output for current roots and validation options. Requires successful source validation and specialization; consumes neither artifact. |

`check()` stops at accumulated parse errors before constructing checked bodies,
even with `check_all`. `analyze()` visits recovered syntax for editor facts but
retains the same execution gate. Parsing is additive even for repeated paths;
replacing or removing an input requires rebuilding the compiler.

Source-validation and specialization diagnostics have separate owners. Public
`last_errors` and `last_safety_errors` combine their current messages;
`last_parse_errors` covers all inputs and `last_type_errors` the last checking
pass. These mutable lists are views, not validation authority: clearing them
cannot authorize execution. Each failed specialization attempt replaces its own
diagnostics; root changes clear only that phase's diagnostics.

Validation options are snapshotted during checking. Currently `no_recursion` is
the validation-affecting option; future such options must join `ValidationOptions`.
`specialize()` and `specialized_program()` reject reuse while its current value
differs from the checked snapshot and require revalidation. Restoring the checked
value permits reuse; writes alone do not create a generation. Rechecking clears
old output even on failure. `quiet`, `check_all` and `print_ir` control reporting,
diagnostic collection or code generation, not validation policy. Already returned
compiled programs are independently owned and are not revoked.

Use `checked_program()` for source-semantic queries and `specialized_program()`
for concrete consumers. The compatibility `decls()` view prefers current concrete
declarations, otherwise templates, and panics before type publication.
Executable layout users must call `globals_info_with_offset()` after successful
specialization, with the backend's reserved offset. FFI compilation continues to
check again. Retained templates extend memory lifetime through code generation;
no automatic FFI reuse or additional caching is provided.

## Partial editor facts

The LSP calls `analyze()` and queries immutable `SourceAnalysis` for hover and
definition requests. Ordinary `check()` does not retain the extra source inventory
and fact vectors. Parsing and either checking entry point replace old snapshots;
root changes preserve them. The LSP rebuilds from all open documents on edits and
publishes diagnostics for every document, including empty lists after repair.

A snapshot owns the normalized, macro-expanded source declaration inventory and
its `DefId -> BodyAnalysis` map. Early macro failure retains only the unexpanded
inventory. Expression coordinates belong to the snapshot's source function arena,
not a checked/concrete arena. Local records retain names, optional types and
binder locations; requirements identify interface declarations and can have gaps
after errors. Cloning a snapshot clones its owners together. No IDs survive edits.

`ExpressionFacts` separates optional type, recorded reference and declaration
binder facts. Missing/unvisited facts are `None`. An empty unresolved overload
set supplies neither a target nor inferred type; a nonempty set still represents
candidates. Arithmetic without a named overload supplies no target reference.
Type admission is deliberately bounded:

- Successful, unrecovered bodies expose solved types without anonymous variables
  and with valid named annotations, including declared generic/size parameters.
- Failed bodies expose no types justified only by solver substitutions. A concrete
  type after failed unification, a bad call/cast or incomplete struct literal is
  insufficient; inferred local types are withheld too.
- Independently established facts can survive body failure: range-checked suffixed
  integers, fixed-type real/string/character/boolean literals and reads of valid
  explicitly typed parameters/locals or fixed loop/size bindings. These use
  original annotations, not failed substitutions. Unsuffixed integer inference,
  unresolved/malformed annotations and void value bindings supply no type.
- All body types from a parse-damaged file are withheld, including expansions with
  that provenance. Clean files retain useful facts, but dependencies on recovered
  signatures/type declarations invalidate inferred caller facts. Independently
  established facts can still survive in those callers.

Established references can support navigation even when a type is unavailable.
Local navigation follows the recorded binder; overload navigation retains its
exact-type/first-candidate heuristic, not semantic target selection. Field
navigation requires a known base type. Parameter locations currently identify
the enclosing function/lambda. Missing facts never trigger a substitute resolver.

Partial facts cannot authorize specialization. Complete safety-error-only input
retains type/reference facts, but incomplete programs receive no safety analysis.
Parser synchronization, lost/unvisited declarations, precise token spans and
richer overload presentation remain editor limitations. There is no cross-edit
identity/cache scheme or completion handler.

## Safety and identified body operations

Source safety checks definitions, including unreachable ones. Its template call
policy uses the first recorded function with matching arity and exact signature;
explicit applications, unmatched generic/interface candidates and size-generic
bodies defer to concrete checking. Source safety failure blocks specialization.

After specialization, the existing safety traversal checks every retained concrete
function body before field hoisting or backend lowering, including ordinary
callers/wrappers and type/size instances. Lambda bodies use the enclosing
traversal's existing rules. A direct function `InstanceId` supplies the exact
callee and its `require` clauses, including externs and selected interface
implementations. Safety retains an arity guard but performs no signature-equality
selection or assertion. Explicit applications become instance reads as well.

For example, specialization rejects `bounded(-1, true)` when
`bounded<T>(x: i32, value: T) require x >= 0 {}`. An ordinary wrapper must establish
that requirement through its own contract or guard; a valid value at the wrapper's
caller does not specialize the wrapper on that value.

Concrete checking uses the existing proof language: call-site proofs support
`true`, conjunctions and the existing less-than/greater-or-equal interval and
array-length/size patterns. Unsupported clauses fail conservatively; even a valid
`require x == 0` call can be unprovable. Local function values and function-valued
globals remain indirect and carry no direct-call contract. Structural validation
is not a safety proof, and this boundary supplies no general higher-order analysis.

Equivalent failed requirements are deduplicated by source call location,
requirement location and concrete clause text; the first diagnostic retains its
concrete callee name. Distinct sites, requirement origins or substituted bounds
remain distinct. Other safety diagnostics use location/message deduplication.

Safety storage roots use identities and field paths. Call substitution maps
callee parameter IDs to caller arguments and analyzes callee-only expressions
in a fresh context. Only nonlocal assumption facts cross body boundaries; equal
numeric locals cannot transfer proofs. Normalization, checking, specialization
and safety take assumption bodies/roots directly. Function signature,
borrowed-return and escape restrictions stay at function boundaries; lvalue and
borrowed-call checks use the owning body. No synthetic assumption function or
name-based body adapter is needed.

Borrowed-argument assignability and no-alias checking share one classification.
Recorded overloads conservatively union borrowed positions; explicit type
arguments are substituted before classification. Indirect/function-valued calls
use solved signatures, so a local callee follows its own signature under
shadowing. References require assignability; references and slices participate
in the existing syntactic base/field-path alias check. These operations do not
select another overload or establish general memory disjointness.

## Captures, transformations and consumer responsibilities

Shared free-local discovery uses `BindingFacts` over checked bodies or in-progress
checker records, without requiring solved types or whole-program publication.
It excludes internal binders by identity, visits nested lambdas for construction
dependencies, and retains first-use order without duplicate captures. Size
parameters are compile-time values. Incomplete results are explicit: an empty
list with missing facts cannot prove noncapture; known free locals still establish
capture. Escape taint uses `LocalId` and capturing/noncapturing/unknown states and
continues to diagnose established capture after type errors.

Escape checking retains its existing scope: explicit returns, initializer taint,
block/if combinations and conservative propagation from callees/arguments (which
can reject scalar results). Implicit returns, assignment propagation, returns
inside lambda bodies, aggregate escapes and broader escape soundness are separate
work. Backends share capture discovery/extraction and own addressable storage,
closure ABI and linking. Extracted lambdas are `CheckedFunction`s with cloned
enclosing arenas and backend-managed queues/patches. They are backend extraction
views, not independently published program bodies, new source instances or a
program-wide lowered lambda inventory.

Whole-body clones preserve expression/local/requirement coordinates and source
locations under a new owner. `CheckedBody::duplicate` freshens nodes and internal
binders, remaps their uses and preserves enclosing references and source locations.
Shared reads are allowed; shared subtrees declaring bindings need freshening.
Macro occurrence normalization precedes checking so expansions resolve in their
actual lexical scopes. `replace` keeps a coordinate/location and takes the new
result type; `replace_node` can also change provenance.

Derived analyses must be recomputed after input changes. Validation and preserved
coordinates/locations neither refresh analyses nor prove behavior preservation.
Hoisting refreshes capture/alias/read/write facts and preserves the global-write/
call information in its precomputed may-write summary; that summary is not a
purity proof. Consumers still own value snapshots, effect order, alias/dependence
proofs, code-motion legality and cancellation behavior.

| Consumer | Facts consumed; responsibility retained |
| --- | --- |
| Specialization | Definition/argument keys and recorded candidates; reachability, selection, recursion limits and unique emitted symbols. |
| Safety | Identified storage and concrete callees; proof grammar, call obligations and conservative failure. |
| Field hoisting | Instance-keyed global-write summaries; borrowed/global aliases, captured storage, opaque calls and movement legality. |
| Copy elision | Local identities and binding types; live ranges and escape conditions. |
| Cranelift, LLVM, register VM, Stack | Local/instance maps and ordered captures; representation recovery, implicit conversions, aggregate copies, storage, ABI and runtime checks. |
| VM expression inlining | Complete callee `BodyContext`, restored after emission; eligibility, allocation and code emission. Current eligibility excludes declarations, loops, calls, returns and lambdas. |
| LSP and host APIs | Source facts/concrete inventories; presentation, entry policy, layout offsets, buffer validity and compiled-program lifetime. |

Runtime cancellation remains a backend responsibility; LLVM AOT omits callback
cancellation. Fully selected ordinary template callees, explicit conversion nodes,
shared closure lowering, broader safety proofs and new optimization/value IRs are
separate work.
