#!/usr/bin/env python3
"""
Generate Lyte Stack VM fused handlers from base-op bodies in
src/stack_interp.c. Two output modes:

  --mode c     C handler bodies (concatenated base-op bodies with
               pc->imm[0] rewrites and intermediate NEXT() stripped).
               Relies on clang's store-to-load forwarding + DCE to
               fold intermediate PUSH/POP spills — see the discussion
               in docs/HOT_LOCALS_FUSED_HANDLERS_PLAN.md. Default.

  --mode rust  Rust peephole-fusion clauses for stack_optimize.rs.
               Emits one `if let (StackOp::Base, ..., StackOp::Base)`
               match per fusion that has `gen_rust_rule=True`, which
               fuses the pattern into the target StackOp variant and
               Nops the remaining slots.

The same fusion spec (FUSIONS) drives both modes. Fusions that
can't be expressed as straight-line concatenation are excluded
entirely (e.g. ilt_jiz's conditional DISPATCH, slice/struct-field
ops with no matching base op, the int-window f32 arithmetic path).
Fusions with `gen_rust_rule=False` still generate a C handler but
no Rust rule — used when codegen emits the fused op directly and
the peephole fuser never sees the unfused sequence.

Usage:
    python3 scripts/gen_fused_handlers.py -o src/stack_interp_fused.gen.c
    python3 scripts/gen_fused_handlers.py --mode rust \\
        -o src/stack_optimize_fused.gen.rs
"""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional

ROOT = Path(__file__).resolve().parent.parent
STACK_INTERP_C = ROOT / "src" / "stack_interp.c"

# Suffix for generated handler names. Empty by default — the generated
# file is compiled as its own translation unit alongside stack_interp.c
# and replaces the hand-written forms of these ops. Set to something
# like "_gen" temporarily if you want to A/B the generated handlers
# against hand-written versions without a symbol clash.
NAME_SUFFIX = ""


# ----------------------------------------------------------------------
# Op-name → Rust StackOp variant mapping
# ----------------------------------------------------------------------
#
# The C handler name (e.g. `op_iadd`) doesn't mechanically convert to
# the Rust variant name (e.g. `IAdd`): `IAdd` is two capitalized
# fragments "I" + "Add" with no underscore in the C name. Rather than
# guess with heuristics that will break on `op_f32_const` vs `op_iadd`,
# we keep an explicit table. Add to it when you add a new base or
# fused op that this script needs to reference.
VARIANTS: Dict[str, str] = {
    # --- base ops referenced by fusions below ---
    "op_local_get": "LocalGet",
    "op_local_set": "LocalSet",
    "op_local_get_f": "LocalGetF",
    "op_local_set_f": "LocalSetF",
    "op_i64_const": "I64Const",
    "op_iadd": "IAdd",
    "op_isub": "ISub",
    "op_imul": "IMul",
    "op_iadd_imm": "IAddImm",
    "op_ilt": "ILt",
    "op_fadd_f": "FAddF",
    "op_fsub_f": "FSubF",
    "op_fmul_f": "FMulF",
    # --- fused target ops produced by fusions below ---
    "op_fused_get_get_iadd": "FusedGetGetIAdd",
    "op_fused_get_get_ilt": "FusedGetGetILt",
    "op_fused_get_addimm_set": "FusedGetAddImmSet",
    "op_fused_const_set": "FusedConstSet",
    "op_fused_get_set": "FusedGetSet",
    "op_fused_get_get_iadd_set": "FusedGetGetIAddSet",
    "op_fused_get_get_isub_set": "FusedGetGetISubSet",
    "op_fused_get_get_imul_set": "FusedGetGetIMulSet",
    "op_fused_get_get_fadd_f": "FusedGetGetFAddF",
    "op_fused_get_get_fsub_f": "FusedGetGetFSubF",
    "op_fused_get_get_fmul_f": "FusedGetGetFMulF",
    "op_fused_get_fmul_f": "FusedGetFMulF",
    "op_fused_get_fadd_f": "FusedGetFAddF",
    "op_fused_get_fsub_f": "FusedGetFSubF",
}


def rust_variant(op: str) -> str:
    if op not in VARIANTS:
        raise SystemExit(
            f"no Rust variant known for {op!r}; add it to the VARIANTS "
            f"table at the top of {Path(__file__).name}"
        )
    return VARIANTS[op]


# ----------------------------------------------------------------------
# Fusion spec
# ----------------------------------------------------------------------

@dataclass
class BaseRef:
    """Reference to a base handler inside a fusion.

    `op`  — the HANDLER(op_xxx) name in src/stack_interp.c.
    `imm` — which fused-op immediate slot this base op's pc->imm[0]
            should map to, or None if the base op has no immediate.
    """
    op: str
    imm: Optional[int] = None

    def label(self) -> str:
        return f"{self.op}@{self.imm}" if self.imm is not None else self.op


@dataclass
class Fusion:
    name: str
    bases: List[BaseRef]
    note: str = ""
    # If False, the script skips the Rust peephole rule for this
    # fusion. Use for fusions that the Rust codegen emits directly
    # (so the peephole fuser never sees the unfused sequence) — e.g.
    # FusedGetGetIAddSet, which stack_codegen.rs emits directly for
    # `x = a + b` patterns.
    gen_rust_rule: bool = True


def b(op: str, imm: Optional[int] = None) -> BaseRef:
    return BaseRef(op, imm)


# Every entry below generates one fused handler. The bases list is
# read left-to-right; their bodies are concatenated in order.
FUSIONS: List[Fusion] = [
    # ---- Int-window ----
    Fusion("op_fused_get_get_iadd", [
        b("op_local_get", 0),
        b("op_local_get", 1),
        b("op_iadd"),
    ]),
    Fusion("op_fused_get_get_ilt", [
        b("op_local_get", 0),
        b("op_local_get", 1),
        b("op_ilt"),
    ]),
    Fusion("op_fused_get_addimm_set", [
        b("op_local_get", 0),
        b("op_iadd_imm", 1),
        b("op_local_set", 2),
    ]),
    Fusion("op_fused_const_set", [
        b("op_i64_const", 0),
        b("op_local_set", 1),
    ]),
    Fusion("op_fused_get_set", [
        b("op_local_get", 0),
        b("op_local_set", 1),
    ]),
    # The three _set variants below are emitted directly by
    # stack_codegen.rs for `x = a <op> b` patterns, so the peephole
    # fuser never sees the unfused sequence. We still generate the
    # C handlers, but gen_rust_rule=False skips the Rust peephole
    # clause.
    Fusion("op_fused_get_get_iadd_set", [
        b("op_local_get", 0),
        b("op_local_get", 1),
        b("op_iadd"),
        b("op_local_set", 2),
    ], gen_rust_rule=False),
    Fusion("op_fused_get_get_isub_set", [
        b("op_local_get", 0),
        b("op_local_get", 1),
        b("op_isub"),
        b("op_local_set", 2),
    ], gen_rust_rule=False),
    Fusion("op_fused_get_get_imul_set", [
        b("op_local_get", 0),
        b("op_local_get", 1),
        b("op_imul"),
        b("op_local_set", 2),
    ], gen_rust_rule=False),

    # ---- F-window ----
    Fusion("op_fused_get_get_fadd_f", [
        b("op_local_get_f", 0),
        b("op_local_get_f", 1),
        b("op_fadd_f"),
    ]),
    Fusion("op_fused_get_get_fsub_f", [
        b("op_local_get_f", 0),
        b("op_local_get_f", 1),
        b("op_fsub_f"),
    ]),
    Fusion("op_fused_get_get_fmul_f", [
        b("op_local_get_f", 0),
        b("op_local_get_f", 1),
        b("op_fmul_f"),
    ]),
    Fusion("op_fused_get_fmul_f", [
        b("op_local_get_f", 0),
        b("op_fmul_f"),
    ]),
    Fusion("op_fused_get_fadd_f", [
        b("op_local_get_f", 0),
        b("op_fadd_f"),
    ]),
    Fusion("op_fused_get_fsub_f", [
        b("op_local_get_f", 0),
        b("op_fsub_f"),
    ]),
]


# ----------------------------------------------------------------------
# Parsing src/stack_interp.c
# ----------------------------------------------------------------------

HANDLER_RE = re.compile(r"HANDLER\(\s*(?P<name>op_\w+)\s*\)\s*\{")
IMM0_RE = re.compile(r"pc->imm\[\s*0\s*\]")
IMM_HIGH_RE = re.compile(r"pc->imm\[\s*[1-9]\s*\]")
TRAILING_NEXT_RE = re.compile(r"\s*NEXT\(\);\s*$")

# Several comparison ops (op_ieq, op_ilt, ...) are defined via the
# CMP_OP() macro. Before we brace-parse handler bodies, expand those
# invocations inline so the handlers become visible to extract_handlers.
CMP_OP_RE = re.compile(
    r"CMP_OP\(\s*(?P<name>op_\w+)\s*,"
    r"\s*(?P<type>[\w ]+?)\s*,"
    r"\s*(?P<cast>\([^)]+\)|\w+)\s*,"
    r"\s*(?P<op>[^)]+?)\s*\)"
)

CMP_OP_TEMPLATE = """HANDLER({name}) {{
    {type} b = {cast}(t0); {type} a = {cast}(t1);
    t0 = (a {op} b) ? 1 : 0;
    BINOP_SHIFT();
    NEXT();
}}"""


def preprocess_cmp_ops(source: str) -> str:
    """Expand CMP_OP(...) invocations into literal HANDLER(...) blocks
    so the brace-counting extractor can see them."""
    def repl(m: re.Match) -> str:
        return CMP_OP_TEMPLATE.format(
            name=m.group("name"),
            type=m.group("type"),
            cast=m.group("cast"),
            op=m.group("op"),
        )
    return CMP_OP_RE.sub(repl, source)


def extract_handlers(source: str) -> Dict[str, str]:
    """Return {handler_name: body_text} for every HANDLER(...) block.

    `body_text` is the text between the outermost `{` and matching
    `}`, exclusive of those braces. A brace counter walks the source
    — none of the handler bodies contain `{` or `}` inside string or
    character literals, so the counter is reliable here.
    """
    out: Dict[str, str] = {}
    pos = 0
    while True:
        m = HANDLER_RE.search(source, pos)
        if not m:
            break
        name = m.group("name")
        i = m.end()
        depth = 1
        while i < len(source) and depth > 0:
            c = source[i]
            if c == "{":
                depth += 1
            elif c == "}":
                depth -= 1
            i += 1
        body = source[m.end():i - 1]
        out[name] = body
        pos = i
    return out


# ----------------------------------------------------------------------
# Body rewriting
# ----------------------------------------------------------------------

def rewrite_imm0(body: str, new_idx: int) -> str:
    return IMM0_RE.sub(f"pc->imm[{new_idx}]", body)


def strip_trailing_next(body: str) -> str:
    return TRAILING_NEXT_RE.sub("", body).rstrip()


def normalize_indent(body: str) -> str:
    """Reduce to a common 4-space indent. Input bodies come straight
    from the source file and are already indented with 4 spaces, so
    we just trim leading/trailing blank lines."""
    lines = body.splitlines()
    while lines and not lines[0].strip():
        lines.pop(0)
    while lines and not lines[-1].strip():
        lines.pop()
    return "\n".join(lines)


# ----------------------------------------------------------------------
# Emit
# ----------------------------------------------------------------------

def emit_fusion(f: Fusion, handlers: Dict[str, str]) -> str:
    parts: List[str] = []
    for base in f.bases:
        if base.op not in handlers:
            raise SystemExit(
                f"{f.name}: unknown base handler {base.op!r} — "
                f"not found in {STACK_INTERP_C}"
            )
        body = handlers[base.op]
        body = strip_trailing_next(body)
        if IMM_HIGH_RE.search(body):
            raise SystemExit(
                f"{f.name}: base {base.op} references pc->imm[>=1]; "
                "this generator only rewrites pc->imm[0]. Extend the "
                "script if you really need a multi-imm base op."
            )
        if base.imm is not None:
            body = rewrite_imm0(body, base.imm)
        elif IMM0_RE.search(body):
            raise SystemExit(
                f"{f.name}: base {base.op} reads pc->imm[0] but no "
                "imm slot was provided (pass imm=N to b(...))."
            )
        body = normalize_indent(body)
        label = f"    // --- {base.op}"
        if base.imm is not None:
            label += f" (pc->imm[{base.imm}])"
        label += " ---"
        parts.append(f"{label}\n{body}")

    recipe = " + ".join(br.label() for br in f.bases)
    note = f"\n// note: {f.note}" if f.note else ""
    joined = "\n\n".join(parts)
    return (
        f"// {recipe}{note}\n"
        f"HANDLER({f.name}{NAME_SUFFIX}) {{\n"
        f"{joined}\n"
        f"    NEXT();\n"
        f"}}"
    )


def emit_all_c(handlers: Dict[str, str]) -> str:
    header = (
        "// ============================================================================\n"
        "// Generated by scripts/gen_fused_handlers.py from src/stack_interp.c.\n"
        "// DO NOT EDIT MANUALLY — regenerate via the script.\n"
        "//\n"
        "// Each handler below is the concatenation of one or more base-op bodies,\n"
        "// with pc->imm[0] references rewritten per the fusion spec and intermediate\n"
        "// NEXT() tail-calls stripped. Correctness relies on clang's store-to-load\n"
        "// forwarding + DCE folding the PUSH/POP spills that appear between base-op\n"
        "// bodies into the same machine code as the hand-written combined forms.\n"
        "//\n"
        "// This file is a standalone translation unit — the shared handler macros\n"
        "// (HANDLER, NEXT, PUSH/POP, READ_I/WRITE_I, HOT_L*_MAGIC, ...) and type\n"
        "// definitions live in stack_interp.h.\n"
        "// ============================================================================\n"
        "\n"
        '#include "stack_interp.h"\n'
        "\n"
    )
    return header + "\n\n".join(emit_fusion(f, handlers) for f in FUSIONS) + "\n"


# ----------------------------------------------------------------------
# Rust peephole-rule emit
# ----------------------------------------------------------------------

# Variable names handed out in order for each base op that carries an
# immediate. Matches the hand-written rules' convention (a, b, then
# dst/v/c for a third slot).
RUST_BIND_NAMES = ["a", "b", "c", "d", "e"]


def emit_fusion_rust(f: Fusion) -> str:
    """Emit a Rust peephole-fusion clause matching this fusion.

    Produces an `if let (StackOp::..., ...) = (&ops[i], ...) { ... }`
    block that, when matched, writes the target Fused variant into
    ops[i], Nops out ops[i+1..i+n], sets `i += n`, and `continue`s
    the enclosing while loop.

    Must be embedded inside the body of `fuse()` in stack_optimize.rs
    where `ops`, `i`, `len`, and `spans_target` are in scope.
    """
    n = len(f.bases)
    target = rust_variant(f.name)
    recipe = " + ".join(br.label() for br in f.bases)

    # Hand out bind names only to base ops that carry an imm (and
    # therefore a field in the base variant's tuple form). The bind
    # order is the traversal order of the bases list, which matches
    # the order of u16/i32 fields in the target variant.
    binds: List[str] = []
    pats: List[str] = []
    for base in f.bases:
        variant = rust_variant(base.op)
        if base.imm is not None:
            name = RUST_BIND_NAMES[len(binds)]
            binds.append(name)
            pats.append(f"StackOp::{variant}({name})")
        else:
            pats.append(f"StackOp::{variant}")

    # "(&ops[i], &ops[i+1], &ops[i+2])"
    refs = ", ".join(f"&ops[i+{k}]" if k > 0 else "&ops[i]" for k in range(n))
    pat_tuple = ", ".join(pats)
    len_check = f"i + {n - 1} < len"
    span_check = f"!spans_target(i, {n})"

    # Body lines (16-space indent, nested inside `if let { ... }`).
    body_lines: List[str] = []
    for name in binds:
        body_lines.append(f"                let {name} = *{name};")
    target_args = ", ".join(binds)
    target_expr = (
        f"StackOp::{target}({target_args})" if binds else f"StackOp::{target}"
    )
    body_lines.append(f"                ops[i] = {target_expr};")
    for k in range(1, n):
        body_lines.append(f"                ops[i + {k}] = StackOp::Nop;")
    body_lines.append(f"                i += {n};")
    body_lines.append("                continue;")
    body = "\n".join(body_lines)

    return (
        f"        // {recipe} → {target}\n"
        f"        if {len_check} && {span_check} {{\n"
        f"            if let ({pat_tuple}) = ({refs}) {{\n"
        f"{body}\n"
        f"            }}\n"
        f"        }}"
    )


def emit_all_rust() -> str:
    header = (
        "// ============================================================================\n"
        "// Generated by scripts/gen_fused_handlers.py from the FUSIONS spec.\n"
        "// DO NOT EDIT MANUALLY — regenerate via the script.\n"
        "//\n"
        "// Each block below is a peephole-fusion clause that matches a window of\n"
        "// base StackOps and replaces it with the corresponding fused StackOp. This\n"
        "// file is embedded via include!() inside stack_optimize.rs's fuse() loop,\n"
        "// where it relies on the surrounding scope's `ops`, `i`, `len`, and\n"
        "// `spans_target` closure. Generated rules match the same patterns (and in\n"
        "// the same priority order — longest first) as the hand-written rules they\n"
        "// replaced.\n"
        "// ============================================================================\n"
        "\n"
    )
    # Longest match first — avoids a 2-op rule swallowing the leading
    # half of a 3-op sequence.
    ordered = [f for f in FUSIONS if f.gen_rust_rule]
    ordered.sort(key=lambda f: -len(f.bases))
    body = "\n\n".join(emit_fusion_rust(f) for f in ordered)
    return header + "{\n" + body + "\n}\n"


# ----------------------------------------------------------------------
# CLI
# ----------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--mode",
        choices=["c", "rust"],
        default="c",
        help="Which target to emit: 'c' (handler bodies) or 'rust' "
             "(peephole rules). Default: c.",
    )
    parser.add_argument(
        "-o", "--output",
        help="Write output to this file (default: stdout).",
    )
    parser.add_argument(
        "--source",
        default=str(STACK_INTERP_C),
        help=f"Path to stack_interp.c (default: {STACK_INTERP_C})",
    )
    args = parser.parse_args()

    if args.mode == "c":
        source = Path(args.source).read_text()
        source = preprocess_cmp_ops(source)
        handlers = extract_handlers(source)
        out = emit_all_c(handlers)
        emitted = len(FUSIONS)
    else:
        out = emit_all_rust()
        emitted = sum(1 for f in FUSIONS if f.gen_rust_rule)

    if args.output:
        Path(args.output).write_text(out)
        print(
            f"Wrote {emitted} {args.mode} fusion entries to {args.output}"
        )
    else:
        print(out, end="")


if __name__ == "__main__":
    main()
