use crate::*;
use std::collections::{HashMap, HashSet};

/// A hoistable field read: which binding and field, plus the types the
/// checker assigned to the base and to the field at the read site.
struct FieldRead {
    var_name: Name,
    field_name: Name,
    var_type: TypeID,
    field_type: TypeID,
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
pub fn hoist_loop_invariant_fields(fdecl: &mut FuncDecl) {
    if fdecl.body.is_none() {
        return;
    }
    let body = fdecl.body.unwrap();
    hoist_in_expr(body, fdecl);
}

/// Recursively walk the AST looking for loops inside blocks.
/// When we find a loop inside a block, we can insert hoisted bindings before it.
fn hoist_in_expr(expr_id: ExprID, fdecl: &mut FuncDecl) {
    match fdecl.arena.exprs[expr_id].clone() {
        Expr::Block(stmts) => {
            // First, recurse into each statement.
            for &s in &stmts {
                hoist_in_expr(s, fdecl);
            }
            // Now look for loops in this block and hoist their invariant fields.
            hoist_loops_in_block(expr_id, fdecl);
        }
        Expr::For { body, .. } => {
            hoist_in_expr(body, fdecl);
        }
        Expr::While(_, body) => {
            hoist_in_expr(body, fdecl);
        }
        Expr::If(_, then_branch, else_branch) => {
            hoist_in_expr(then_branch, fdecl);
            if let Some(e) = else_branch {
                hoist_in_expr(e, fdecl);
            }
        }
        _ => {}
    }
}

/// For each loop statement in a block, hoist invariant struct field reads.
fn hoist_loops_in_block(block_id: ExprID, fdecl: &mut FuncDecl) {
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
            collect_written_fields(body_id, fdecl, &mut written_fields);
            // Also collect for the condition of while loops.
            if let Expr::While(cond, _) = &fdecl.arena.exprs[stmt_id] {
                collect_written_fields(*cond, fdecl, &mut written_fields);
            }

            // Find all field reads that are loop-invariant.
            let mut field_reads: Vec<FieldRead> = Vec::new();
            collect_invariant_field_reads(body_id, fdecl, &written_fields, &mut field_reads);
            if let Expr::While(cond, _) = &fdecl.arena.exprs[stmt_id] {
                collect_invariant_field_reads(*cond, fdecl, &written_fields, &mut field_reads);
            }

            // Bindings created inside the loop can't be hoisted out of it:
            // the hoisted `let` would sit before the loop, where the name
            // isn't in scope yet, and the binding is remade on every
            // iteration anyway. Shadowed outer names are dropped too, since
            // reads inside the loop see the inner binding.
            let mut bound: HashSet<Name> = HashSet::new();
            collect_bound_names(body_id, fdecl, &mut bound);
            if let Expr::For { var, .. } = &fdecl.arena.exprs[stmt_id] {
                bound.insert(*var);
            }
            field_reads.retain(|read| !bound.contains(&read.var_name));

            // Deduplicate. Every surviving read of a given name resolves to the
            // same binding — the ones bound in the loop were just dropped — so
            // the first read's types describe all of them.
            let mut seen = HashSet::new();
            field_reads.retain(|read| seen.insert((read.var_name, read.field_name)));

            if !field_reads.is_empty() {
                // Create hoisted let bindings and a substitution map.
                let mut subst: HashMap<(Name, Name), Name> = HashMap::new();
                let loc = fdecl.arena.locs[stmt_id];

                for read in &field_reads {
                    let FieldRead {
                        var_name,
                        field_name,
                        var_type,
                        field_type,
                    } = *read;
                    let hoisted_name =
                        Name::new(format!("__hoisted_{}_{}", &*var_name, &*field_name));

                    // Build: let __hoisted_var_field = var.field
                    let id_expr = fdecl.arena.add(Expr::Id(var_name), loc);
                    fdecl.types.push(var_type); // type for the Id expr

                    let field_expr = fdecl.arena.add(Expr::Field(id_expr, field_name), loc);
                    fdecl.types.push(field_type); // type for the Field expr

                    let let_expr = fdecl
                        .arena
                        .add(Expr::Let(hoisted_name, field_expr, None), loc);
                    fdecl.types.push(field_type); // type for the Let expr (must match init)

                    new_stmts.push(let_expr);
                    subst.insert((var_name, field_name), hoisted_name);
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

/// Collect the names bound inside an expression tree: `var` and `let`
/// declarations, `for` loop variables, and lambda parameters.
fn collect_bound_names(expr_id: ExprID, fdecl: &FuncDecl, bound: &mut HashSet<Name>) {
    match &fdecl.arena.exprs[expr_id] {
        Expr::Var(name, _, _) | Expr::Let(name, _, _) => {
            bound.insert(*name);
        }
        Expr::For { var, .. } => {
            bound.insert(*var);
        }
        Expr::Lambda { params, .. } => {
            for param in params {
                bound.insert(param.name);
            }
        }
        _ => {}
    }
    for sub in fdecl.arena.exprs[expr_id].subexprs() {
        collect_bound_names(sub, fdecl, bound);
    }
}

/// Collect all (variable_name, field_name) pairs that are written to in the expression tree.
fn collect_written_fields(expr_id: ExprID, fdecl: &FuncDecl, written: &mut HashSet<(Name, Name)>) {
    match &fdecl.arena.exprs[expr_id] {
        Expr::Binop(Binop::Assign, lhs, rhs) => {
            // Check if LHS is a field access: var.field = ...
            if let Expr::Field(base, field_name) = &fdecl.arena.exprs[*lhs] {
                if let Expr::Id(var_name) = &fdecl.arena.exprs[*base] {
                    written.insert((*var_name, *field_name));
                }
            }
            // Also mark if the whole variable is reassigned.
            if let Expr::Id(var_name) = &fdecl.arena.exprs[*lhs] {
                // If the variable itself is assigned, all its fields are tainted.
                written.insert((*var_name, Name::str("*")));
            }
            collect_written_fields(*rhs, fdecl, written);
        }
        Expr::Block(stmts) => {
            for &s in stmts {
                collect_written_fields(s, fdecl, written);
            }
        }
        Expr::If(cond, then_b, else_b) => {
            collect_written_fields(*cond, fdecl, written);
            collect_written_fields(*then_b, fdecl, written);
            if let Some(e) = else_b {
                collect_written_fields(*e, fdecl, written);
            }
        }
        Expr::While(cond, body) => {
            collect_written_fields(*cond, fdecl, written);
            collect_written_fields(*body, fdecl, written);
        }
        Expr::For {
            start, end, body, ..
        } => {
            collect_written_fields(*start, fdecl, written);
            collect_written_fields(*end, fdecl, written);
            collect_written_fields(*body, fdecl, written);
        }
        Expr::Call(func, args) => {
            // A function call could mutate structs passed by reference.
            // Conservatively, mark all variables passed to calls as tainted.
            for &arg in args {
                if let Expr::Id(var_name) = &fdecl.arena.exprs[arg] {
                    written.insert((*var_name, Name::str("*")));
                }
            }
            collect_written_fields(*func, fdecl, written);
            for &arg in args {
                collect_written_fields(arg, fdecl, written);
            }
        }
        Expr::Binop(_, lhs, rhs) => {
            collect_written_fields(*lhs, fdecl, written);
            collect_written_fields(*rhs, fdecl, written);
        }
        Expr::Unop(_, arg) => {
            collect_written_fields(*arg, fdecl, written);
        }
        Expr::Field(base, _) => {
            collect_written_fields(*base, fdecl, written);
        }
        Expr::ArrayIndex(base, idx) => {
            collect_written_fields(*base, fdecl, written);
            collect_written_fields(*idx, fdecl, written);
        }
        Expr::Return(e) | Expr::Assume(e) => {
            collect_written_fields(*e, fdecl, written);
        }
        Expr::Var(_, init, _) => {
            if let Some(e) = init {
                collect_written_fields(*e, fdecl, written);
            }
        }
        Expr::Let(_, init, _) => {
            collect_written_fields(*init, fdecl, written);
        }
        Expr::Lambda { body, .. } => {
            collect_written_fields(*body, fdecl, written);
        }
        Expr::AsTy(e, _) => {
            collect_written_fields(*e, fdecl, written);
        }
        Expr::Tuple(elems) | Expr::ArrayLiteral(elems) => {
            for &e in elems {
                collect_written_fields(e, fdecl, written);
            }
        }
        Expr::StructLit(_, fields) => {
            for (_, fval) in fields {
                collect_written_fields(*fval, fdecl, written);
            }
        }
        _ => {}
    }
}

/// Collect (variable_name, field_name) pairs for loop-invariant scalar field reads.
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
                        // Take both types from this read site. Resolving them
                        // by name instead would pick up an unrelated binding
                        // that happens to share the name.
                        reads.push(FieldRead {
                            var_name: *var_name,
                            field_name: *field_name,
                            var_type: fdecl.types[*base],
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
        Expr::Lambda { body, .. } => {
            collect_invariant_field_reads(*body, fdecl, written, reads);
        }
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
        Expr::Lambda { body, .. } => {
            replace_field_reads(body, fdecl, subst);
        }
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
