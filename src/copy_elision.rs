//! Value semantics for `let` bindings, and the analysis that lets each backend
//! skip the copy when it isn't observable.
//!
//! `let x = <aggregate>` binds a *value*. Arrays, structs, enums and tuples are
//! copied out of whatever storage the initializer names, so a later write
//! through the source is invisible to `x` and — the case that actually bites —
//! a slice coerced from `x` can't write back into the source. Slices,
//! references and function values are reference types and are never copied;
//! scalars and `f32x4` live in registers.
//!
//! Without the copy, `let` launders immutability away: `let row = g[k]` followed
//! by `f(row)` where `f` takes `[f32]` hands `f` write access to `g`, because
//! the `[T; N]` -> `[T]` coercion produces a mutable reference regardless of
//! what it was coerced from. `var` has always copied; `let` now matches it.
//!
//! The copy is a codegen concern rather than a language one. When nothing in
//! the binding's live range can write to memory or capture an address, aliasing
//! the source is observationally identical to copying it, and the backend is
//! free to skip the copy. `elidable_let_copies` finds those bindings.

use crate::decl::FuncDecl;
use crate::defs::{Binop, ExprID, Name};
use crate::expr::Expr;
use crate::types::{Type, TypeID};
use std::collections::HashSet;

/// Types that `let` binds by value, and so must copy out of the initializer's
/// storage.
///
/// Deliberately narrower than [`TypeID::is_ptr`]: that also covers `Slice`,
/// `Reference`, `Func` and `Float32x4`, which are references or register
/// values and must keep referring to the same storage.
pub fn is_value_aggregate(ty: &TypeID) -> bool {
    matches!(**ty, Type::Array(_, _) | Type::Name(_, _) | Type::Tuple(_))
}

/// The `Expr::Let` ids in `decl` whose copy a backend may skip, binding the
/// initializer's address directly instead.
///
/// A copy is elidable when nothing that runs while the binding is live can
/// observe the difference — see [`live_range_is_read_only`].
pub fn elidable_let_copies(decl: &FuncDecl) -> HashSet<ExprID> {
    let mut elidable = HashSet::new();
    if let Some(body) = decl.body {
        scan_blocks(body, decl, &mut elidable);
    }
    elidable
}

fn scan_blocks(id: ExprID, decl: &FuncDecl, elidable: &mut HashSet<ExprID>) {
    if let Expr::Block(stmts) = &decl.arena.exprs[id] {
        for (i, &stmt) in stmts.iter().enumerate() {
            if !matches!(&decl.arena.exprs[stmt], Expr::Let(..)) {
                continue;
            }
            if !is_value_aggregate(&decl.types[stmt]) {
                continue;
            }
            // A `let` in tail position is the block's value, so the binding
            // outlives the block. Never elide those.
            if i + 1 >= stmts.len() {
                continue;
            }
            let rest = &stmts[i + 1..];
            let name = match &decl.arena.exprs[stmt] {
                Expr::Let(name, _, _) => *name,
                _ => unreachable!(),
            };
            // The block's value escapes it, so the binding must not reach the
            // final statement.
            let last = *stmts.last().unwrap();
            if mentions(last, name, decl) {
                continue;
            }
            if rest.iter().all(|&s| live_range_is_read_only(s, decl)) {
                elidable.insert(stmt);
            }
        }
    }

    for sub in decl.arena.exprs[id].subexprs() {
        scan_blocks(sub, decl, elidable);
    }
}

/// True if evaluating this subtree can neither write to storage the binding
/// might alias nor let an address escape.
///
/// Calls are excluded because any aggregate argument may be coerced to a slice
/// and written through, and because a call can write to a global the binding
/// was read from. Lambdas capture by address, and `return` and arena allocation
/// let an address outlive the block.
///
/// Assignment is allowed in the one shape that provably can't reach other
/// storage: a whole scalar variable. `total = total + row[i]` writes only
/// `total`'s own slot, so an accumulator loop — the case worth eliding for —
/// stays eligible. `x[i] = v` and `x.f = v` write through a pointer that may
/// alias the source, so they are not.
///
/// What's left — indexing, field reads, arithmetic, control flow, fresh
/// `let`/`var` bindings — only reads.
fn live_range_is_read_only(id: ExprID, decl: &FuncDecl) -> bool {
    match &decl.arena.exprs[id] {
        Expr::Call(_, _)
        | Expr::Macro(_, _)
        | Expr::Lambda { .. }
        | Expr::Return(_)
        | Expr::Arena(_) => false,
        Expr::Binop(Binop::Assign, lhs, rhs) => {
            matches!(&decl.arena.exprs[*lhs], Expr::Id(_))
                && !decl.types[*lhs].is_ptr()
                && live_range_is_read_only(*rhs, decl)
        }
        expr => expr
            .subexprs()
            .into_iter()
            .all(|sub| live_range_is_read_only(sub, decl)),
    }
}

/// True if `name` is referenced anywhere in this subtree.
fn mentions(id: ExprID, name: Name, decl: &FuncDecl) -> bool {
    if let Expr::Id(n) = &decl.arena.exprs[id] {
        if *n == name {
            return true;
        }
    }
    decl.arena.exprs[id]
        .subexprs()
        .into_iter()
        .any(|sub| mentions(sub, name, decl))
}
