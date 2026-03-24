use crate::*;
use std::collections::{HashMap, HashSet};

/// Hoist loop-invariant struct field loads out of loops.
///
/// For each loop (For/While), finds struct field accesses (`expr.field`) where:
/// - The base expression is a simple local variable (`Expr::Id`)
/// - The field is never written to inside the loop body
/// - The field type is a scalar (not a struct/array/slice)
///
/// Each such access is replaced with a reference to a hoisted `let` binding
/// inserted just before the loop.
pub fn hoist_loop_invariant_fields(fdecl: &mut FuncDecl, decls: &DeclTable) {
    if fdecl.body.is_none() {
        return;
    }
    let body = fdecl.body.unwrap();
    hoist_in_expr(body, fdecl, decls);
}

/// Recursively walk the AST looking for loops inside blocks.
/// When we find a loop inside a block, we can insert hoisted bindings before it.
fn hoist_in_expr(expr_id: ExprID, fdecl: &mut FuncDecl, decls: &DeclTable) {
    match fdecl.arena.exprs[expr_id].clone() {
        Expr::Block(stmts) => {
            // First, recurse into each statement.
            for &s in &stmts {
                hoist_in_expr(s, fdecl, decls);
            }
            // Now look for loops in this block and hoist their invariant fields.
            hoist_loops_in_block(expr_id, fdecl, decls);
        }
        Expr::For { body, .. } => {
            hoist_in_expr(body, fdecl, decls);
        }
        Expr::While(_, body) => {
            hoist_in_expr(body, fdecl, decls);
        }
        Expr::If(_, then_branch, else_branch) => {
            hoist_in_expr(then_branch, fdecl, decls);
            if let Some(e) = else_branch {
                hoist_in_expr(e, fdecl, decls);
            }
        }
        _ => {}
    }
}

/// For each loop statement in a block, hoist invariant struct field reads.
fn hoist_loops_in_block(block_id: ExprID, fdecl: &mut FuncDecl, decls: &DeclTable) {
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
            let mut field_reads: Vec<(Name, Name)> = Vec::new();
            collect_invariant_field_reads(body_id, fdecl, decls, &written_fields, &mut field_reads);
            if let Expr::While(cond, _) = &fdecl.arena.exprs[stmt_id] {
                collect_invariant_field_reads(
                    *cond,
                    fdecl,
                    decls,
                    &written_fields,
                    &mut field_reads,
                );
            }

            // Deduplicate.
            let mut seen = HashSet::new();
            field_reads.retain(|pair| seen.insert(*pair));

            if !field_reads.is_empty() {
                // Create hoisted let bindings and a substitution map.
                let mut subst: HashMap<(Name, Name), Name> = HashMap::new();
                let loc = fdecl.arena.locs[stmt_id];

                for (var_name, field_name) in &field_reads {
                    let hoisted_name =
                        Name::new(format!("__hoisted_{}_{}", &**var_name, &**field_name));

                    // Build: let __hoisted_var_field = var.field
                    let id_expr = fdecl.arena.add(Expr::Id(*var_name), loc);
                    let var_type = find_var_type(*var_name, fdecl);
                    fdecl.types.push(var_type); // type for the Id expr

                    let field_expr = fdecl.arena.add(Expr::Field(id_expr, *field_name), loc);
                    let field_type = find_field_type(var_type, *field_name, decls);
                    fdecl.types.push(field_type); // type for the Field expr

                    let let_expr = fdecl
                        .arena
                        .add(Expr::Let(hoisted_name, field_expr, None), loc);
                    fdecl.types.push(field_type); // type for the Let expr (must match init)

                    new_stmts.push(let_expr);
                    subst.insert((*var_name, *field_name), hoisted_name);
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
    decls: &DeclTable,
    written: &HashSet<(Name, Name)>,
    reads: &mut Vec<(Name, Name)>,
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
                        reads.push(pair);
                    }
                }
            }
            collect_invariant_field_reads(*base, fdecl, decls, written, reads);
        }
        Expr::Binop(Binop::Assign, _lhs, rhs) => {
            // Don't collect reads from the LHS of assignments.
            collect_invariant_field_reads(*rhs, fdecl, decls, written, reads);
        }
        Expr::Binop(_, lhs, rhs) => {
            collect_invariant_field_reads(*lhs, fdecl, decls, written, reads);
            collect_invariant_field_reads(*rhs, fdecl, decls, written, reads);
        }
        Expr::Unop(_, arg) => {
            collect_invariant_field_reads(*arg, fdecl, decls, written, reads);
        }
        Expr::Call(func, args) => {
            collect_invariant_field_reads(*func, fdecl, decls, written, reads);
            for &arg in args {
                collect_invariant_field_reads(arg, fdecl, decls, written, reads);
            }
        }
        Expr::Block(stmts) => {
            for &s in stmts {
                collect_invariant_field_reads(s, fdecl, decls, written, reads);
            }
        }
        Expr::If(cond, then_b, else_b) => {
            collect_invariant_field_reads(*cond, fdecl, decls, written, reads);
            collect_invariant_field_reads(*then_b, fdecl, decls, written, reads);
            if let Some(e) = else_b {
                collect_invariant_field_reads(*e, fdecl, decls, written, reads);
            }
        }
        Expr::While(cond, body) => {
            collect_invariant_field_reads(*cond, fdecl, decls, written, reads);
            collect_invariant_field_reads(*body, fdecl, decls, written, reads);
        }
        Expr::For {
            start, end, body, ..
        } => {
            collect_invariant_field_reads(*start, fdecl, decls, written, reads);
            collect_invariant_field_reads(*end, fdecl, decls, written, reads);
            collect_invariant_field_reads(*body, fdecl, decls, written, reads);
        }
        Expr::ArrayIndex(base, idx) => {
            collect_invariant_field_reads(*base, fdecl, decls, written, reads);
            collect_invariant_field_reads(*idx, fdecl, decls, written, reads);
        }
        Expr::Return(e) | Expr::Assume(e) | Expr::AsTy(e, _) => {
            collect_invariant_field_reads(*e, fdecl, decls, written, reads);
        }
        Expr::Var(_, init, _) => {
            if let Some(e) = init {
                collect_invariant_field_reads(*e, fdecl, decls, written, reads);
            }
        }
        Expr::Let(_, init, _) => {
            collect_invariant_field_reads(*init, fdecl, decls, written, reads);
        }
        Expr::Lambda { body, .. } => {
            collect_invariant_field_reads(*body, fdecl, decls, written, reads);
        }
        Expr::Tuple(elems) | Expr::ArrayLiteral(elems) => {
            for &e in elems {
                collect_invariant_field_reads(e, fdecl, decls, written, reads);
            }
        }
        Expr::StructLit(_, fields) => {
            for (_, fval) in fields {
                collect_invariant_field_reads(*fval, fdecl, decls, written, reads);
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

/// Find the type of a variable by scanning the function's parameter list and body.
fn find_var_type(var_name: Name, fdecl: &FuncDecl) -> TypeID {
    // Check parameters first.
    for param in &fdecl.params {
        if param.name == var_name {
            if let Some(ty) = param.ty {
                return ty;
            }
        }
    }

    // Scan the arena for a Var or Let binding with this name.
    for (_i, expr) in fdecl.arena.exprs.iter().enumerate() {
        match expr {
            Expr::Var(name, _, _) | Expr::Let(name, _, _) => {
                if *name == var_name {
                    // For Var/Let, the type in the types array is Void (statement type).
                    // We need to look at the initializer's type or the annotated type.
                    if let Expr::Var(_, _, Some(ty)) = expr {
                        return *ty;
                    }
                    if let Expr::Let(_, _, Some(ty)) = expr {
                        return *ty;
                    }
                    if let Expr::Var(_, Some(init), _) = expr {
                        return fdecl.types[*init];
                    }
                    if let Expr::Let(_, init, _) = expr {
                        return fdecl.types[*init];
                    }
                }
            }
            _ => {}
        }
    }

    // Fallback: search for any Id with this name and use its type.
    for (i, expr) in fdecl.arena.exprs.iter().enumerate() {
        if let Expr::Id(name) = expr {
            if *name == var_name && i < fdecl.types.len() {
                return fdecl.types[i];
            }
        }
    }

    mk_type(Type::Void)
}

/// Check if a type is a pointer type (struct, array, slice, tuple).
fn is_ptr_type(ty: &TypeID) -> bool {
    matches!(
        &**ty,
        Type::Name(_, _) | Type::Tuple(_) | Type::Array(_, _) | Type::Slice(_)
    )
}

/// Find the type of a field on a struct type.
fn find_field_type(struct_type: TypeID, field_name: Name, decls: &DeclTable) -> TypeID {
    if let Type::Name(struct_name, _type_args) = &*struct_type {
        let found = decls.find(*struct_name);
        for decl in found {
            if let Decl::Struct(sd) = decl {
                if let Some(field) = sd.find_field(&field_name) {
                    return field.ty;
                }
            }
        }
    }
    mk_type(Type::Void)
}
