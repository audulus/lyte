#!/usr/bin/env python3
"""
Generate Lyte Stack VM fused handlers by concatenating base-op
bodies from src/stack_interp.c.

To add a new fused handler, append an entry to FUSIONS below.
A fusion is a list of base-op references; each base op contributes
its body to the concatenated fused handler. The generator

  1. pulls each base op's body out of src/stack_interp.c,
  2. strips the trailing NEXT(); tail-call,
  3. rewrites pc->imm[0] references to the fused op's chosen imm
     slot (so e.g. the second local.get in a "get,get,iadd" fusion
     reads pc->imm[1] instead of pc->imm[0]),
  4. concatenates the rewritten bodies with one final NEXT().

This relies on clang's store-to-load forwarding + DCE to fold the
intermediate PUSH/POP spills on `sp` that show up between base-op
bodies — see the discussion in docs/HOT_LOCALS_FUSED_HANDLERS_PLAN.md.

Fusions that can't be expressed as straight-line concatenation of
base ops are deliberately excluded from this generator (e.g.
ilt_jiz's conditional DISPATCH, slice/struct-field ops with no
matching base op, the int-window f32 arithmetic path that no
longer has base ops). Those stay hand-written.

Usage:
    python3 scripts/gen_fused_handlers.py
    python3 scripts/gen_fused_handlers.py -o src/stack_interp_fused.gen.c
"""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional

ROOT = Path(__file__).resolve().parent.parent
STACK_INTERP_C = ROOT / "src" / "stack_interp.c"

# Suffix for generated handler names, so they can coexist with the
# hand-written originals during A/B verification. Set to "" to
# produce drop-in replacements.
NAME_SUFFIX = "_gen"


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
    Fusion("op_fused_get_get_iadd_set", [
        b("op_local_get", 0),
        b("op_local_get", 1),
        b("op_iadd"),
        b("op_local_set", 2),
    ]),
    Fusion("op_fused_get_get_isub_set", [
        b("op_local_get", 0),
        b("op_local_get", 1),
        b("op_isub"),
        b("op_local_set", 2),
    ]),
    Fusion("op_fused_get_get_imul_set", [
        b("op_local_get", 0),
        b("op_local_get", 1),
        b("op_imul"),
        b("op_local_set", 2),
    ]),

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


def emit_all(handlers: Dict[str, str]) -> str:
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
        "// ============================================================================\n"
        "\n"
    )
    return header + "\n\n".join(emit_fusion(f, handlers) for f in FUSIONS) + "\n"


# ----------------------------------------------------------------------
# CLI
# ----------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
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

    source = Path(args.source).read_text()
    source = preprocess_cmp_ops(source)
    handlers = extract_handlers(source)
    out = emit_all(handlers)

    if args.output:
        Path(args.output).write_text(out)
        print(f"Wrote {len(FUSIONS)} fused handlers to {args.output}")
    else:
        print(out, end="")


if __name__ == "__main__":
    main()
