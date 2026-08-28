use crate::*;
use std::collections::{HashMap, HashSet};

/// The globals a function may write, directly or through anything it calls.
/// `None` means "may write any global" — the function reaches a callee we
/// can't see through (an indirect call, or an `extern` body).
type GlobalWrites = Option<HashSet<Name>>;

/// Whole-module may-write summary, used to decide whether a call inside a loop
/// can invalidate a hoisted field read.
///
/// Lyte function parameters aren't assignable, so a callee's only channel for
/// mutating state its caller can observe is a global. That makes a
/// global-granularity summary exact enough to be useful: a loop calling a
/// function that touches no globals keeps all of its hoists.
pub struct SideEffects {
    globals: HashSet<Name>,
    per_func: HashMap<Name, GlobalWrites>,
}

impl SideEffects {
    /// Build the summary by taking each function's direct global writes and
    /// closing over the call graph to a fixpoint.
    ///
    /// Overloads are merged: the graph is keyed by name, so a name resolves to
    /// the union of every overload's writes. That over-approximates, which is
    /// the safe direction.
    pub fn analyze(decls: &DeclTable) -> Self {
        let globals: HashSet<Name> = decls
            .decls
            .iter()
            .filter_map(|d| match d {
                Decl::Global { name, .. } => Some(*name),
                _ => None,
            })
            .collect();

        let known_funcs: HashSet<Name> = decls
            .decls
            .iter()
            .filter_map(|d| match d {
                Decl::Func(f) => Some(f.name),
                _ => None,
            })
            .collect();

        let mut direct: HashMap<Name, HashSet<Name>> = HashMap::new();
        let mut callees: HashMap<Name, HashSet<Name>> = HashMap::new();
        let mut opaque: HashSet<Name> = HashSet::new();

        for d in &decls.decls {
            let f = match d {
                Decl::Func(f) => f,
                _ => continue,
            };
            let writes = direct.entry(f.name).or_default();
            let calls = callees.entry(f.name).or_default();
            let body = match f.body {
                Some(b) => b,
                None => {
                    // No body: either a builtin (print/assert/putc/math), which
                    // touches no globals, or an `extern` we can't see into.
                    if f.is_extern {
                        opaque.insert(f.name);
                    }
                    continue;
                }
            };
            let mut is_opaque = false;
            scan_effects(
                body,
                f,
                &globals,
                &known_funcs,
                &shadowed_names(f, &globals),
                writes,
                calls,
                &mut is_opaque,
            );
            if is_opaque {
                opaque.insert(f.name);
            }
        }

        // Fixpoint: propagate writes and opacity along call edges. Cycles just
        // stop changing the sets, so recursion terminates without special
        // handling.
        loop {
            let mut changed = false;
            let names: Vec<Name> = direct.keys().copied().collect();
            for name in names {
                let targets: Vec<Name> = callees[&name].iter().copied().collect();
                for t in targets {
                    if opaque.contains(&t) && !opaque.contains(&name) {
                        opaque.insert(name);
                        changed = true;
                    }
                    let t_writes: Vec<Name> = direct
                        .get(&t)
                        .map(|w| w.iter().copied().collect())
                        .unwrap_or_default();
                    let w = direct.get_mut(&name).unwrap();
                    for g in t_writes {
                        if w.insert(g) {
                            changed = true;
                        }
                    }
                }
            }
            if !changed {
                break;
            }
        }

        let per_func = direct
            .into_iter()
            .map(|(name, writes)| {
                if opaque.contains(&name) {
                    (name, None)
                } else {
                    (name, Some(writes))
                }
            })
            .collect();

        SideEffects { globals, per_func }
    }

    /// Globals possibly written by a call whose callee expression is `func`.
    /// An indirect callee (a local or parameter of function type) is opaque,
    /// including one whose name shadows a function of the same name: the
    /// summary is keyed by name, so consulting it there would describe a
    /// callee this call never reaches.
    fn writes_of_call(
        &self,
        func: ExprID,
        fdecl: &FuncDecl,
        shadowed: &HashSet<Name>,
    ) -> GlobalWrites {
        match &fdecl.arena.exprs[func] {
            Expr::Id(name) if !shadowed.contains(name) => self.writes_of(*name),
            _ => None,
        }
    }

    /// Globals possibly written by calling `name`. `None` means "any global".
    fn writes_of(&self, name: Name) -> GlobalWrites {
        match self.per_func.get(&name) {
            Some(w) => w.clone(),
            // Not a function we know about — a call through a local or
            // parameter of function type. Assume the worst.
            None => None,
        }
    }
}

/// Collect one function's direct global writes and its call edges.
///
/// Sets `is_opaque` when the function calls something that isn't a statically
/// known function name, since we can't summarize such a callee.
#[allow(clippy::too_many_arguments)]
fn scan_effects(
    expr_id: ExprID,
    fdecl: &FuncDecl,
    globals: &HashSet<Name>,
    known_funcs: &HashSet<Name>,
    shadowed: &HashSet<Name>,
    writes: &mut HashSet<Name>,
    calls: &mut HashSet<Name>,
    is_opaque: &mut bool,
) {
    match &fdecl.arena.exprs[expr_id] {
        Expr::Binop(Binop::Assign, lhs, _) => {
            if let Some(base) = assigned_root(*lhs, fdecl) {
                if globals.contains(&base) {
                    writes.insert(base);
                }
            }
        }
        Expr::Call(func, _) => match &fdecl.arena.exprs[*func] {
            // A name that's also bound locally names the binding, not the
            // function, so the call edge would point at the wrong callee.
            Expr::Id(name) if known_funcs.contains(name) && !shadowed.contains(name) => {
                calls.insert(*name);
            }
            _ => *is_opaque = true,
        },
        _ => {}
    }

    for sub in fdecl.arena.exprs[expr_id].subexprs() {
        scan_effects(
            sub,
            fdecl,
            globals,
            known_funcs,
            shadowed,
            writes,
            calls,
            is_opaque,
        );
    }
}

/// Names that a call site can't be summarized through: every name the function
/// binds itself — parameters, each `let`, `var`, lambda parameter and `for`
/// variable — plus every module-level global.
///
/// Calls are summarized by callee name, so a call through a name bound to a
/// value has to be treated as indirect: the summary for the *function* of that
/// name describes a callee the call never reaches. Globals count because a
/// global of function type shadows a same-named function everywhere, not just
/// in the function that declares a local. The local half is function-wide
/// rather than scope-precise: shadowing a function name is rare, and the extra
/// conservatism only costs hoists.
fn shadowed_names(fdecl: &FuncDecl, globals: &HashSet<Name>) -> HashSet<Name> {
    let mut names: HashSet<Name> = fdecl.params.iter().map(|p| p.name).collect();
    names.extend(globals.iter().copied());
    if let Some(body) = fdecl.body {
        collect_bound_names(body, fdecl, &mut names);
    }
    names
}

/// What the hoist walk needs to know about one function's names.
struct LocalNames {
    /// Callee names that don't resolve to the function of the same name.
    shadowed: HashSet<Name>,

    /// Names mentioned inside a lambda body in this function. A `var` a lambda
    /// captures is shared by address, so a call the summary can't see through
    /// may be that lambda, writing one of these behind the loop's back.
    captured: HashSet<Name>,
}

fn collect_bound_names(expr_id: ExprID, fdecl: &FuncDecl, names: &mut HashSet<Name>) {
    match &fdecl.arena.exprs[expr_id] {
        Expr::Let(name, _, _) | Expr::Var(name, _, _) => {
            names.insert(*name);
        }
        Expr::For { var, .. } => {
            names.insert(*var);
        }
        Expr::Lambda { params, .. } => {
            for p in params {
                names.insert(p.name);
            }
        }
        _ => {}
    }

    for sub in fdecl.arena.exprs[expr_id].subexprs() {
        collect_bound_names(sub, fdecl, names);
    }
}

/// The variable at the root of an assignment target: `g`, `g.f`, `g[i].f`, ...
fn assigned_root(expr_id: ExprID, fdecl: &FuncDecl) -> Option<Name> {
    match &fdecl.arena.exprs[expr_id] {
        Expr::Id(name) => Some(*name),
        Expr::Field(base, _) | Expr::ArrayIndex(base, _) => assigned_root(*base, fdecl),
        _ => None,
    }
}

/// Hoist loop-invariant struct field loads out of loops.
///
/// For each loop (For/While), finds struct field accesses (`expr.field`) where:
/// - The base expression is a simple local variable (`Expr::Id`)
/// - The field is never written to inside the loop body
/// - The field type is a scalar (not a struct/array/slice)
///
/// Each such access is replaced with a reference to a hoisted `let` binding
/// inserted just before the loop.
pub fn hoist_loop_invariant_fields(fdecl: &mut FuncDecl, effects: &SideEffects) {
    if fdecl.body.is_none() {
        return;
    }
    let body = fdecl.body.unwrap();
    let names = LocalNames {
        shadowed: shadowed_names(fdecl, &effects.globals),
        captured: fdecl.names_referenced_in_lambdas(),
    };
    hoist_in_expr(body, fdecl, effects, &names);
}

/// Recursively walk the AST looking for loops inside blocks.
/// When we find a loop inside a block, we can insert hoisted bindings before it.
fn hoist_in_expr(expr_id: ExprID, fdecl: &mut FuncDecl, effects: &SideEffects, names: &LocalNames) {
    match fdecl.arena.exprs[expr_id].clone() {
        Expr::Block(stmts) => {
            // First, recurse into each statement.
            for &s in &stmts {
                hoist_in_expr(s, fdecl, effects, names);
            }
            // Now look for loops in this block and hoist their invariant fields.
            hoist_loops_in_block(expr_id, fdecl, effects, names);
        }
        Expr::For { body, .. } => {
            hoist_in_expr(body, fdecl, effects, names);
        }
        Expr::While(_, body) => {
            hoist_in_expr(body, fdecl, effects, names);
        }
        Expr::If(_, then_branch, else_branch) => {
            hoist_in_expr(then_branch, fdecl, effects, names);
            if let Some(e) = else_branch {
                hoist_in_expr(e, fdecl, effects, names);
            }
        }
        _ => {}
    }
}

/// For each loop statement in a block, hoist invariant struct field reads.
fn hoist_loops_in_block(
    block_id: ExprID,
    fdecl: &mut FuncDecl,
    effects: &SideEffects,
    names: &LocalNames,
) {
    let stmts = if let Expr::Block(ref stmts) = fdecl.arena.exprs[block_id] {
        stmts.clone()
    } else {
        return;
    };

    let mut new_stmts = Vec::with_capacity(stmts.len());

    for &stmt_id in &stmts {
        let loop_body = match &fdecl.arena.exprs[stmt_id] {
            Expr::For { body, .. } => Some(*body),
            Expr::While(_, body) => Some(*body),
            _ => None,
        };

        if let Some(body_id) = loop_body {
            // Find all fields written in the loop body.
            let mut written_fields: HashSet<(Name, Name)> = HashSet::new();
            collect_written_fields(body_id, fdecl, effects, names, &mut written_fields);
            // The hoisted binding is inserted before the whole loop statement,
            // so anything the loop's own header evaluates runs after it: a
            // `while` condition, and a `for` range, both have to be scanned.
            match fdecl.arena.exprs[stmt_id].clone() {
                Expr::While(cond, _) => {
                    collect_written_fields(cond, fdecl, effects, names, &mut written_fields);
                }
                Expr::For {
                    var, start, end, ..
                } => {
                    written_fields.insert((var, Name::str("*")));
                    collect_written_fields(start, fdecl, effects, names, &mut written_fields);
                    collect_written_fields(end, fdecl, effects, names, &mut written_fields);
                }
                _ => {}
            }

            // Find all field reads that are loop-invariant.
            let mut field_reads: Vec<FieldRead> = Vec::new();
            collect_invariant_field_reads(body_id, fdecl, &written_fields, &mut field_reads);
            if let Expr::While(cond, _) = &fdecl.arena.exprs[stmt_id] {
                collect_invariant_field_reads(*cond, fdecl, &written_fields, &mut field_reads);
            }

            // Deduplicate.
            let mut seen = HashSet::new();
            field_reads.retain(|r| seen.insert((r.var, r.field)));

            if !field_reads.is_empty() {
                // Create hoisted let bindings and a substitution map.
                let mut subst: HashMap<(Name, Name), Name> = HashMap::new();
                let loc = fdecl.arena.locs[stmt_id];

                for read in &field_reads {
                    let hoisted_name =
                        Name::new(format!("__hoisted_{}_{}", &**read.var, &**read.field));

                    // Build: let __hoisted_var_field = var.field
                    //
                    // The types come from the nodes we're copying, not from a
                    // by-name lookup: a same-named binding in a sibling scope
                    // would otherwise supply the wrong struct type, and the
                    // hoisted read would use that type's field offset.
                    let id_expr = fdecl.arena.add(Expr::Id(read.var), loc);
                    fdecl.types.push(read.base_type); // type for the Id expr

                    let field_expr = fdecl.arena.add(Expr::Field(id_expr, read.field), loc);
                    fdecl.types.push(read.field_type); // type for the Field expr

                    let let_expr = fdecl
                        .arena
                        .add(Expr::Let(hoisted_name, field_expr, None), loc);
                    fdecl.types.push(read.field_type); // type for the Let expr (must match init)

                    new_stmts.push(let_expr);
                    subst.insert((read.var, read.field), hoisted_name);
                }

                // Replace field accesses in the loop body with hoisted variable references.
                replace_field_reads(body_id, fdecl, &subst);
                if let Expr::While(cond, _) = fdecl.arena.exprs[stmt_id].clone() {
                    replace_field_reads(cond, fdecl, &subst);
                }
            }
        }

        new_stmts.push(stmt_id);
    }

    if new_stmts.len() != stmts.len() {
        fdecl.arena.exprs[block_id] = Expr::Block(new_stmts);
    }
}

/// Collect all (variable_name, field_name) pairs that are written to in the
/// expression tree. `(name, "*")` means the whole variable is clobbered.
///
/// This has to be complete: a write we miss becomes a stale hoisted read. The
/// traversal therefore handles the shapes it cares about and then recurses
/// into every subexpression, so a new `Expr` variant can't silently escape it.
fn collect_written_fields(
    expr_id: ExprID,
    fdecl: &FuncDecl,
    effects: &SideEffects,
    names: &LocalNames,
    written: &mut HashSet<(Name, Name)>,
) {
    match &fdecl.arena.exprs[expr_id] {
        Expr::Binop(Binop::Assign, lhs, _) => match &fdecl.arena.exprs[*lhs] {
            // `var = ...` replaces the whole variable.
            Expr::Id(var_name) => {
                written.insert((*var_name, Name::str("*")));
            }
            // `var.field = ...` clobbers exactly that field.
            //
            // Deeper targets need nothing: `var.a.b = ...` and `var.arr[i] = ...`
            // write through an aggregate field, and `slice[i] = ...` writes
            // elements rather than the slice's `len`. Only scalar fields are
            // ever hoisted, and none of those writes can reach one.
            Expr::Field(base, field_name) => {
                if let Expr::Id(var_name) = &fdecl.arena.exprs[*base] {
                    written.insert((*var_name, *field_name));
                }
            }
            _ => {}
        },
        Expr::Call(func, args) => {
            // Parameters aren't assignable in Lyte, so a callee we can name
            // reaches its caller's state only through globals.
            match effects.writes_of_call(*func, fdecl, &names.shadowed) {
                Some(gs) => {
                    for g in gs {
                        written.insert((g, Name::str("*")));
                    }
                }
                None => {
                    // A callee we can't name may be a lambda holding the
                    // address of one of our own locals, so those go too.
                    for g in &effects.globals {
                        written.insert((*g, Name::str("*")));
                    }
                    for c in &names.captured {
                        written.insert((*c, Name::str("*")));
                    }
                }
            }
            // Aggregates handed to a call stay tainted as before.
            for &arg in args {
                if let Expr::Id(var_name) = &fdecl.arena.exprs[arg] {
                    written.insert((*var_name, Name::str("*")));
                }
            }
        }
        Expr::Var(name, _, _) => {
            // A binding introduced inside the loop is a fresh variable on every
            // iteration, and it doesn't exist before the loop at all — nothing
            // about it can be hoisted.
            written.insert((*name, Name::str("*")));
        }
        Expr::Let(name, _, _) => {
            written.insert((*name, Name::str("*")));
        }
        Expr::Lambda { params, .. } => {
            // Lambda parameters shadow anything of the same name in the
            // enclosing scope, so reads through them aren't invariant either.
            for p in params {
                written.insert((p.name, Name::str("*")));
            }
        }
        Expr::For { var, .. } => {
            written.insert((*var, Name::str("*")));
        }
        _ => {}
    }

    for sub in fdecl.arena.exprs[expr_id].subexprs() {
        collect_written_fields(sub, fdecl, effects, names, written);
    }
}

/// A field read the hoister decided is loop-invariant, carrying the types of
/// the nodes it was found on so the hoisted copy reproduces them exactly.
struct FieldRead {
    var: Name,
    field: Name,
    base_type: TypeID,
    field_type: TypeID,
}

/// Collect loop-invariant scalar field reads.
fn collect_invariant_field_reads(
    expr_id: ExprID,
    fdecl: &FuncDecl,
    written: &HashSet<(Name, Name)>,
    reads: &mut Vec<FieldRead>,
) {
    match &fdecl.arena.exprs[expr_id] {
        Expr::Field(base, field_name) => {
            if let Expr::Id(var_name) = &fdecl.arena.exprs[*base] {
                let pair = (*var_name, *field_name);
                let wildcard = (*var_name, Name::str("*"));
                // Only hoist if the field is never written and the variable isn't wholly reassigned.
                if !written.contains(&pair) && !written.contains(&wildcard) {
                    // Only hoist scalar fields (not sub-structs, arrays, etc.)
                    let field_type = fdecl.types[expr_id];
                    if !is_ptr_type(&field_type) {
                        reads.push(FieldRead {
                            var: *var_name,
                            field: *field_name,
                            base_type: fdecl.types[*base],
                            field_type,
                        });
                    }
                }
            }
            collect_invariant_field_reads(*base, fdecl, written, reads);
        }
        Expr::Binop(Binop::Assign, _lhs, rhs) => {
            // Don't collect reads from the LHS of assignments.
            collect_invariant_field_reads(*rhs, fdecl, written, reads);
        }
        Expr::Binop(_, lhs, rhs) => {
            collect_invariant_field_reads(*lhs, fdecl, written, reads);
            collect_invariant_field_reads(*rhs, fdecl, written, reads);
        }
        Expr::Unop(_, arg) => {
            collect_invariant_field_reads(*arg, fdecl, written, reads);
        }
        Expr::Call(func, args) => {
            collect_invariant_field_reads(*func, fdecl, written, reads);
            for &arg in args {
                collect_invariant_field_reads(arg, fdecl, written, reads);
            }
        }
        Expr::Block(stmts) => {
            for &s in stmts {
                collect_invariant_field_reads(s, fdecl, written, reads);
            }
        }
        Expr::If(cond, then_b, else_b) => {
            collect_invariant_field_reads(*cond, fdecl, written, reads);
            collect_invariant_field_reads(*then_b, fdecl, written, reads);
            if let Some(e) = else_b {
                collect_invariant_field_reads(*e, fdecl, written, reads);
            }
        }
        Expr::While(cond, body) => {
            collect_invariant_field_reads(*cond, fdecl, written, reads);
            collect_invariant_field_reads(*body, fdecl, written, reads);
        }
        Expr::For {
            start, end, body, ..
        } => {
            collect_invariant_field_reads(*start, fdecl, written, reads);
            collect_invariant_field_reads(*end, fdecl, written, reads);
            collect_invariant_field_reads(*body, fdecl, written, reads);
        }
        Expr::ArrayIndex(base, idx) => {
            collect_invariant_field_reads(*base, fdecl, written, reads);
            collect_invariant_field_reads(*idx, fdecl, written, reads);
        }
        Expr::Return(e) | Expr::Assume(e) | Expr::AsTy(e, _) => {
            collect_invariant_field_reads(*e, fdecl, written, reads);
        }
        Expr::Var(_, init, _) => {
            if let Some(e) = init {
                collect_invariant_field_reads(*e, fdecl, written, reads);
            }
        }
        Expr::Let(_, init, _) => {
            collect_invariant_field_reads(*init, fdecl, written, reads);
        }
        // Deliberately not descending into lambda bodies: capture lists are
        // fixed before this pass runs, so a body rewritten to mention a
        // hoisted binding would reference something it doesn't capture.
        Expr::Lambda { .. } => {}
        Expr::Tuple(elems) | Expr::ArrayLiteral(elems) => {
            for &e in elems {
                collect_invariant_field_reads(e, fdecl, written, reads);
            }
        }
        Expr::StructLit(_, fields) => {
            for (_, fval) in fields {
                collect_invariant_field_reads(*fval, fdecl, written, reads);
            }
        }
        _ => {}
    }
}

/// Replace field accesses in the expression tree with references to hoisted variables.
fn replace_field_reads(expr_id: ExprID, fdecl: &mut FuncDecl, subst: &HashMap<(Name, Name), Name>) {
    match fdecl.arena.exprs[expr_id].clone() {
        Expr::Field(base, field_name) => {
            if let Expr::Id(var_name) = &fdecl.arena.exprs[base] {
                let pair = (*var_name, field_name);
                if let Some(hoisted_name) = subst.get(&pair) {
                    // Replace this Field expression with an Id referencing the hoisted variable.
                    fdecl.arena.exprs[expr_id] = Expr::Id(*hoisted_name);
                    return;
                }
            }
            replace_field_reads(base, fdecl, subst);
        }
        Expr::Binop(Binop::Assign, _lhs, rhs) => {
            // Don't replace in LHS of assignments.
            replace_field_reads(rhs, fdecl, subst);
        }
        Expr::Binop(_, lhs, rhs) => {
            replace_field_reads(lhs, fdecl, subst);
            replace_field_reads(rhs, fdecl, subst);
        }
        Expr::Unop(_, arg) => {
            replace_field_reads(arg, fdecl, subst);
        }
        Expr::Call(func, args) => {
            replace_field_reads(func, fdecl, subst);
            for arg in args {
                replace_field_reads(arg, fdecl, subst);
            }
        }
        Expr::Block(stmts) => {
            for s in stmts {
                replace_field_reads(s, fdecl, subst);
            }
        }
        Expr::If(cond, then_b, else_b) => {
            replace_field_reads(cond, fdecl, subst);
            replace_field_reads(then_b, fdecl, subst);
            if let Some(e) = else_b {
                replace_field_reads(e, fdecl, subst);
            }
        }
        Expr::While(cond, body) => {
            replace_field_reads(cond, fdecl, subst);
            replace_field_reads(body, fdecl, subst);
        }
        Expr::For {
            start, end, body, ..
        } => {
            replace_field_reads(start, fdecl, subst);
            replace_field_reads(end, fdecl, subst);
            replace_field_reads(body, fdecl, subst);
        }
        Expr::ArrayIndex(base, idx) => {
            replace_field_reads(base, fdecl, subst);
            replace_field_reads(idx, fdecl, subst);
        }
        Expr::Return(e) | Expr::Assume(e) | Expr::AsTy(e, _) => {
            replace_field_reads(e, fdecl, subst);
        }
        Expr::Var(_, init, _) => {
            if let Some(e) = init {
                replace_field_reads(e, fdecl, subst);
            }
        }
        Expr::Let(_, init, _) => {
            replace_field_reads(init, fdecl, subst);
        }
        // Not descended into, matching collect_invariant_field_reads: a lambda
        // body must never be rewritten to mention a hoisted binding.
        Expr::Lambda { .. } => {}
        Expr::Tuple(elems) | Expr::ArrayLiteral(elems) => {
            for e in elems {
                replace_field_reads(e, fdecl, subst);
            }
        }
        Expr::StructLit(_, fields) => {
            for (_, fval) in fields {
                replace_field_reads(fval, fdecl, subst);
            }
        }
        _ => {}
    }
}

/// Check if a type is a pointer type (struct, array, slice, tuple).
fn is_ptr_type(ty: &TypeID) -> bool {
    matches!(
        &**ty,
        Type::Name(_, _) | Type::Tuple(_) | Type::Array(_, _) | Type::Slice(_)
    )
}
