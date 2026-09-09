use crate::*;
use std::collections::{HashMap, HashSet};

/// Storage identities establish binding identity, not disjoint pointees.
/// Borrowed arguments and closure captures are invalidated separately below.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
enum Root {
    Local(LocalId),
    Global(InstanceId),
}

type GlobalWrites = Option<HashSet<InstanceId>>;

/// Global may-write summaries over concrete function instances. This is not a
/// purity analysis: a builtin can print while writing no module global.
pub struct SideEffects {
    globals: HashSet<InstanceId>,
    per_function: HashMap<InstanceId, GlobalWrites>,
}

impl SideEffects {
    pub fn analyze(program: &SpecializedProgram) -> Result<Self, String> {
        let mut globals = HashSet::new();
        let mut direct = HashMap::<InstanceId, HashSet<InstanceId>>::new();
        let mut callees = HashMap::<InstanceId, HashSet<InstanceId>>::new();
        let mut opaque = HashSet::new();
        for index in 0..program.instances.len() {
            let instance = InstanceId(index as u32);
            match program.instance(instance) {
                Decl::Global { .. } => {
                    globals.insert(instance);
                }
                Decl::Func(function) => {
                    let writes = direct.entry(instance).or_default();
                    let calls = callees.entry(instance).or_default();
                    let mut is_opaque = function.is_extern;
                    if let Some(body) = function.body {
                        scan_effects(body, function, writes, calls, &mut is_opaque);
                    }
                    if is_opaque {
                        opaque.insert(instance);
                    }
                }
                _ => return Err("Concrete instance is neither a function nor global".into()),
            }
        }
        for (&function, calls) in &callees {
            if calls.iter().any(|callee| !direct.contains_key(callee)) {
                opaque.insert(function);
            }
        }
        loop {
            let mut changed = false;
            for (&function, calls) in &callees {
                for callee in calls {
                    if opaque.contains(callee) && opaque.insert(function) {
                        changed = true;
                    }
                    let writes = direct.get(callee).cloned().unwrap_or_default();
                    let own = direct.get_mut(&function).unwrap();
                    let previous_len = own.len();
                    own.extend(writes);
                    changed |= own.len() != previous_len;
                }
            }
            if !changed {
                break;
            }
        }
        Ok(Self {
            globals,
            per_function: direct
                .into_iter()
                .map(|(function, writes)| {
                    (function, (!opaque.contains(&function)).then_some(writes))
                })
                .collect(),
        })
    }

    fn writes_of_call(&self, callee: ExprID, function: &CheckedFunction) -> GlobalWrites {
        function_target(callee, function)
            .and_then(|target| self.per_function.get(&target).cloned().flatten())
    }
}

fn function_target(expr: ExprID, function: &CheckedFunction) -> Option<InstanceId> {
    match function.arena.reference(expr) {
        Some(Reference::Instance(instance)) => Some(*instance),
        _ => None,
    }
}

fn storage_root(expr: ExprID, function: &CheckedFunction) -> Option<Root> {
    match &function.arena[expr] {
        Expr::Id(_) => match function.arena.reference(expr)? {
            Reference::Local(local) => Some(Root::Local(*local)),
            Reference::Instance(instance) => Some(Root::Global(*instance)),
            _ => None,
        },
        Expr::Field(base, _) | Expr::ArrayIndex(base, _) => storage_root(*base, function),
        _ => None,
    }
}

fn scan_effects(
    expr: ExprID,
    function: &CheckedFunction,
    writes: &mut HashSet<InstanceId>,
    calls: &mut HashSet<InstanceId>,
    opaque: &mut bool,
) {
    match &function.arena[expr] {
        Expr::Binop(Binop::Assign, lhs, _) => {
            if let Some(Root::Global(global)) = storage_root(*lhs, function) {
                writes.insert(global);
            }
        }
        Expr::Call(callee, args) => {
            if let Some(target) = function_target(*callee, function) {
                calls.insert(target);
            } else {
                *opaque = true;
            }
            // A wrapper can pass module storage to a callee that only sees a
            // parameter. Its summary must account for that borrowed write.
            for root in written_argument_roots(*callee, args, function) {
                if let Root::Global(global) = root {
                    writes.insert(global);
                }
            }
        }
        _ => {}
    }
    // Scanning lambda bodies conservatively includes their eventual effects.
    for child in function.arena[expr].subexprs() {
        scan_effects(child, function, writes, calls, opaque);
    }
}

/// Scalar field hoisting, using checked roots and concrete callees.
/// Lambda bodies retain their own evaluation boundary and are never rewritten.
pub fn hoist_loop_invariant_fields(
    function: &mut CheckedFunction,
    effects: &SideEffects,
) -> Result<(), String> {
    if let Some(body) = function.body {
        let captured = function.arena.captured_locals();
        hoist_in_expr(body, function, effects, &captured);
    }
    Ok(())
}

fn hoist_in_expr(
    expr: ExprID,
    function: &mut CheckedFunction,
    effects: &SideEffects,
    captured: &HashSet<LocalId>,
) {
    match function.arena[expr].clone() {
        Expr::Block(statements) => {
            for statement in statements {
                hoist_in_expr(statement, function, effects, captured);
            }
            hoist_loops_in_block(expr, function, effects, captured);
        }
        Expr::For { body, .. } | Expr::While(_, body) => {
            hoist_in_expr(body, function, effects, captured)
        }
        Expr::If(_, then_branch, else_branch) => {
            hoist_in_expr(then_branch, function, effects, captured);
            if let Some(other) = else_branch {
                hoist_in_expr(other, function, effects, captured);
            }
        }
        _ => {}
    }
}

type WrittenFields = HashSet<(Root, Option<Name>)>;

struct FieldRead {
    root: Root,
    field: Name,
    expr: ExprID,
}

/// Reference/slice bindings and aggregate parameters can designate storage
/// owned outside this body. Their identities do not exclude overlap with globals.
fn aliased_local_roots(function: &CheckedFunction) -> HashSet<Root> {
    let mut roots: HashSet<_> = function
        .arena
        .locals
        .iter()
        .enumerate()
        .filter(|(_, local)| matches!(&*local.ty, Type::Reference(_) | Type::Slice(_)))
        .map(|(index, _)| Root::Local(LocalId(index as u32)))
        .collect();
    roots.extend(function.params.iter().filter_map(|parameter| {
        let ty = function.arena.local(parameter.local).ty;
        (is_ptr_type(ty) || matches!(&*ty, Type::Reference(_) | Type::Float32x4))
            .then_some(Root::Local(parameter.local))
    }));
    roots
}

fn invalidate_aliased_writes(
    written: &mut WrittenFields,
    aliases: &HashSet<Root>,
    effects: &SideEffects,
) {
    let writes_global = written
        .iter()
        .any(|(root, _)| matches!(root, Root::Global(_)));
    let writes_borrowed = written.iter().any(|(root, _)| aliases.contains(root));
    if writes_global || writes_borrowed {
        written.extend(aliases.iter().map(|root| (*root, None)));
    }
    if writes_borrowed {
        written.extend(
            effects
                .globals
                .iter()
                .map(|global| (Root::Global(*global), None)),
        );
    }
}

fn hoist_loops_in_block(
    block: ExprID,
    function: &mut CheckedFunction,
    effects: &SideEffects,
    captured: &HashSet<LocalId>,
) {
    let Expr::Block(statements) = function.arena[block].clone() else {
        return;
    };
    let aliases = aliased_local_roots(function);
    let mut replacement = Vec::with_capacity(statements.len());
    for statement in statements {
        let body = match function.arena[statement] {
            Expr::For { body, .. } | Expr::While(_, body) => body,
            _ => {
                replacement.push(statement);
                continue;
            }
        };
        let mut written = WrittenFields::new();
        collect_written_fields(body, function, effects, captured, &mut written);
        // The new initializer precedes both range evaluation and testing the
        // condition. Either can invalidate the prospective hoist's source.
        match function.arena[statement].clone() {
            Expr::While(condition, _) => {
                collect_written_fields(condition, function, effects, captured, &mut written)
            }
            Expr::For { start, end, .. } => {
                invalidate_binders(statement, function, &mut written);
                collect_written_fields(start, function, effects, captured, &mut written);
                collect_written_fields(end, function, effects, captured, &mut written);
            }
            _ => unreachable!(),
        }
        invalidate_aliased_writes(&mut written, &aliases, effects);
        let mut reads = vec![];
        collect_invariant_field_reads(body, function, &written, &mut reads);
        if let Expr::While(condition, _) = function.arena[statement] {
            collect_invariant_field_reads(condition, function, &written, &mut reads);
        }
        let mut seen = HashSet::new();
        reads.retain(|read| seen.insert((read.root, read.field)));
        let mut substitutions = HashMap::new();
        for read in reads {
            let (local, declaration) = create_hoisted_binding(&read, &mut function.arena);
            replacement.push(declaration);
            substitutions.insert((read.root, read.field), local);
        }
        replace_field_reads(body, function, &substitutions);
        if let Expr::While(condition, _) = function.arena[statement] {
            replace_field_reads(condition, function, &substitutions);
        }
        replacement.push(statement);
    }
    function
        .arena
        .replace(block, Expr::Block(replacement), function.arena.ty(block));
}

fn create_hoisted_binding(read: &FieldRead, arena: &mut CheckedBody) -> (LocalId, ExprID) {
    let Expr::Field(base, _) = arena[read.expr] else {
        unreachable!();
    };
    let Expr::Id(base_name) = arena[base] else {
        unreachable!();
    };
    let base_reference = arena.reference(base).cloned().expect("checked reference");
    let (base_ty, base_loc) = (arena.ty(base), arena.loc(base));
    let (field_ty, field_loc) = (arena.ty(read.expr), arena.loc(read.expr));
    // These are fresh evaluations with fresh ExprIDs, while the copied
    // outer reference retains its LocalId or global InstanceId.
    let base = arena.add_id(base_name, base_reference, base_ty, base_loc);
    let initializer = arena.add(Expr::Field(base, read.field), field_ty, field_loc);
    let local = arena.add_local(
        Name::new(format!("__hoisted_{}", read.field)),
        field_ty,
        false,
    );
    let declaration = arena.add_let(local, initializer, field_loc);
    (local, declaration)
}

fn invalidate_binders(expr: ExprID, function: &CheckedFunction, written: &mut WrittenFields) {
    written.extend(
        function
            .arena
            .binders(expr)
            .iter()
            .map(|&local| (Root::Local(local), None)),
    );
}

fn collect_written_fields(
    expr: ExprID,
    function: &CheckedFunction,
    effects: &SideEffects,
    captured: &HashSet<LocalId>,
    written: &mut WrittenFields,
) {
    match &function.arena[expr] {
        Expr::Binop(Binop::Assign, lhs, _) => match &function.arena[*lhs] {
            Expr::Id(_) => {
                if let Some(root) = storage_root(*lhs, function) {
                    written.insert((root, None));
                }
            }
            Expr::Field(base, field) if matches!(function.arena[*base], Expr::Id(_)) => {
                if let Some(root) = storage_root(*base, function) {
                    written.insert((root, Some(*field)));
                }
            }
            // Deeper aggregate writes cannot affect the scalar direct fields
            // eligible for this pass's existing read grammar.
            _ => {}
        },
        Expr::Call(callee, args) => {
            match effects.writes_of_call(*callee, function) {
                Some(globals) => written.extend(
                    globals
                        .into_iter()
                        .map(|global| (Root::Global(global), None)),
                ),
                None => {
                    written.extend(
                        effects
                            .globals
                            .iter()
                            .map(|global| (Root::Global(*global), None)),
                    );
                    written.extend(captured.iter().map(|local| (Root::Local(*local), None)));
                    written.extend(
                        aliased_local_roots(function)
                            .into_iter()
                            .map(|root| (root, None)),
                    );
                }
            }
            written
                .extend(written_argument_roots(*callee, args, function).map(|root| (root, None)));
        }
        Expr::Let(..) | Expr::Var(..) | Expr::For { .. } | Expr::Lambda { .. } => {
            invalidate_binders(expr, function, written);
        }
        _ => {}
    }
    for child in function.arena[expr].subexprs() {
        collect_written_fields(child, function, effects, captured, written);
    }
}

/// Conservatively include direct bindings and borrowed projections. Scalar
/// projections passed by value only read their root.
fn written_argument_roots<'a>(
    callee: ExprID,
    args: &'a [ExprID],
    function: &'a CheckedFunction,
) -> impl Iterator<Item = Root> + 'a {
    args.iter().enumerate().filter_map(move |(position, &arg)| {
        if matches!(function.arena[arg], Expr::Id(_))
            || borrows_argument(callee, position, function)
        {
            storage_root(arg, function)
        } else {
            None
        }
    })
}

fn borrows_argument(callee: ExprID, position: usize, function: &CheckedFunction) -> bool {
    let Type::Func(domain, _) = &*function.arena.ty(callee) else {
        return true;
    };
    let Type::Tuple(params) = &**domain else {
        return true;
    };
    match params.get(position).map(|ty| &**ty) {
        Some(
            Type::Reference(_) | Type::Slice(_) | Type::Array(..) | Type::Name(..) | Type::Tuple(_),
        ) => true,
        Some(_) => false,
        None => true,
    }
}

fn read_subexprs(expr: &Expr) -> Vec<ExprID> {
    match expr {
        Expr::Lambda { .. } | Expr::Arena(_) | Expr::Array(..) | Expr::Macro(..) => vec![],
        Expr::Binop(Binop::Assign, _, rhs) => vec![*rhs],
        _ => expr.subexprs(),
    }
}

fn collect_invariant_field_reads(
    expr: ExprID,
    function: &CheckedFunction,
    written: &WrittenFields,
    reads: &mut Vec<FieldRead>,
) {
    if let Expr::Field(base, field) = &function.arena[expr] {
        if matches!(function.arena[*base], Expr::Id(_)) {
            if let Some(root) = storage_root(*base, function) {
                if !written.contains(&(root, Some(*field)))
                    && !written.contains(&(root, None))
                    && !is_ptr_type(function.arena.ty(expr))
                {
                    reads.push(FieldRead {
                        root,
                        field: *field,
                        expr,
                    });
                }
            }
        }
    }
    for child in read_subexprs(&function.arena[expr]) {
        collect_invariant_field_reads(child, function, written, reads);
    }
}

fn replace_field_reads(
    expr: ExprID,
    function: &mut CheckedFunction,
    substitutions: &HashMap<(Root, Name), LocalId>,
) {
    if let Expr::Field(base, field) = function.arena[expr] {
        if matches!(function.arena[base], Expr::Id(_)) {
            if let Some(local) =
                storage_root(base, function).and_then(|root| substitutions.get(&(root, field)))
            {
                let (name, ty) = (function.arena.local(*local).name, function.arena.ty(expr));
                function.arena.replace(expr, Expr::Id(name), ty);
                function.arena.set_reference(expr, Reference::Local(*local));
                return;
            }
        }
    }
    for child in read_subexprs(&function.arena[expr]) {
        replace_field_reads(child, function, substitutions);
    }
}

fn is_ptr_type(ty: TypeID) -> bool {
    matches!(
        &*ty,
        Type::Name(_, _) | Type::Tuple(_) | Type::Array(_, _) | Type::Slice(_)
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn specialized(source: &str) -> SpecializedProgram {
        let mut compiler = Compiler::new();
        compiler.quiet = true;
        assert!(compiler.parse(source, "checked-hoisting.lyte"));
        assert!(compiler.check(), "{:?}", compiler.last_errors);
        MonomorphPass::new()
            .monomorphize(compiler.checked_program().unwrap(), Name::str("main"))
            .unwrap()
    }

    fn main_mut(program: &mut SpecializedProgram) -> &mut CheckedFunction {
        program
            .decls
            .decls
            .iter_mut()
            .find_map(|declaration| match declaration {
                Decl::Func(function) if function.name == Name::str("main") => Some(function),
                _ => None,
            })
            .unwrap()
    }

    #[test]
    fn hoist_binding_identity_is_independent_of_diagnostic_spelling() {
        let mut program = specialized("struct P { x: i32 } main { let __hoisted_x = 90; var p: P; p.x = 7; for i in 0 .. 2 { let value = p.x + __hoisted_x } }");
        let effects = SideEffects::analyze(&program).unwrap();
        let function = main_mut(&mut program);
        let before = function.arena.locals.len();
        let original = function
            .arena
            .locals
            .iter()
            .position(|local| local.name == Name::str("__hoisted_x"))
            .unwrap();
        hoist_loop_invariant_fields(function, &effects).unwrap();
        assert_eq!(function.arena.locals.len(), before + 1);
        assert_eq!(
            function.arena.locals[before].name,
            function.arena.locals[original].name
        );
        for local in [original, before] {
            assert!(function
                .arena
                .ids()
                .any(|id| function.arena.reference(id)
                    == Some(&Reference::Local(LocalId(local as u32)))));
        }
    }

    #[test]
    fn borrowed_field_writes_prevent_hoisting() {
        let mut program = specialized("struct P { x: i32 } bump(x: &i32) { x = x + 1 } main { var p: P; p.x = 1; for i in 0 .. 2 { let value = p.x; bump(p.x) } }");
        let effects = SideEffects::analyze(&program).unwrap();
        let function = main_mut(&mut program);
        let before = function.clone();
        hoist_loop_invariant_fields(function, &effects).unwrap();
        assert_eq!(function, &before);
    }

    #[test]
    fn an_inner_shadow_does_not_invalidate_outer_storage() {
        let mut program = specialized("struct P { x: i32 } main { var p: P; p.x = 7; for i in 0 .. 2 { if true { var p: P; p.x = i; let inner = p.x }; let outer = p.x } }");
        let effects = SideEffects::analyze(&program).unwrap();
        let function = main_mut(&mut program);
        let before = function.arena.locals.len();
        hoist_loop_invariant_fields(function, &effects).unwrap();
        assert_eq!(function.arena.locals.len(), before + 1);
    }

    #[test]
    fn borrowed_parameter_and_global_writes_invalidate_aliases() {
        // Call-site no-alias checking rejects overlapping arguments, but a
        // single borrowed argument may still designate a module global.
        for (read, written) in [("p", "g"), ("g", "p")] {
            let source = format!(
                "struct P {{ x: i32 }} var g: P
                sum(p: &P) -> i32 {{ var result = 0
                    for i in 0 .. 2 {{ result = result + {read}.x; {written}.x = {written}.x + 1 }}
                    result }}
                main() -> i32 {{ g.x = 1; sum(g) }}"
            );
            let mut compiler = Compiler::new();
            compiler.quiet = true;
            assert!(compiler.parse(&source, "aliased-hoisting.lyte"));
            assert!(compiler.check());
            compiler.specialize().unwrap();
            assert_eq!(crate::vm::VM::new().run(&compiler.compile_vm().unwrap()), 3);
            #[cfg(has_stack_interp)]
            assert_eq!(
                crate::stack_interp_bridge::run(&compiler.compile_stack().unwrap()),
                3
            );
        }
    }
}
