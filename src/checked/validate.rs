//! Structural validation at publication. This does not repeat type inference,
//! overload selection, safety proofs, or backend representation decisions.
use super::*;

enum Phase<'a> {
    Template(&'a CheckedDeclTable),
    Concrete(&'a SpecializedProgram),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum BinderKind {
    Value,
    Size,
}

impl CheckedProgram {
    /// Validate body-owned handles and recorded definition/requirement targets.
    /// Generic types, symbolic sizes and deferred overloads are valid templates.
    /// Call again after editing public program data, before consuming it.
    pub fn validate(&self) -> Result<(), String> {
        let phase = Phase::Template(&self.decls);
        for declaration in &self.decls.decls {
            phase.declaration(declaration)?;
        }
        Ok(())
    }
}

impl SpecializedProgram {
    /// Validate concrete bodies and the complete function/global inventory.
    /// Generic struct definitions remain layout templates in this phase too.
    /// Origin DefIds require the source inventory; see `validate_origins`.
    pub fn validate(&self) -> Result<(), String> {
        let mut owned = HashSet::new();
        let mut keys = HashSet::new();
        for (id, instance) in self.instances.iter().enumerate() {
            let declaration = self
                .decls
                .decls
                .get(instance.declaration)
                .ok_or_else(|| format!("instance {} declaration is outside the program", id))?;
            if !matches!(declaration, Decl::Func(_) | Decl::Global { .. }) {
                return Err(format!("instance {} is neither a function nor global", id));
            }
            if !owned.insert(instance.declaration) {
                return Err("multiple instances own the same declaration".into());
            }
            if !keys.insert((
                instance.definition,
                &instance.type_args,
                &instance.size_args,
            )) {
                return Err("duplicate concrete instance key".into());
            }
            for &ty in &instance.type_args {
                validate_type(ty, true)?;
            }
        }
        let phase = Phase::Concrete(self);
        for (index, declaration) in self.decls.decls.iter().enumerate() {
            if matches!(declaration, Decl::Func(_) | Decl::Global { .. }) && !owned.contains(&index)
            {
                return Err(format!("declaration {} has no concrete instance", index));
            }
            phase.declaration(declaration)?;
        }
        Ok(())
    }

    /// Check provenance against the owning checked definition inventory.
    /// Compiler retains that inventory alongside its specialized output;
    /// standalone clients must retain it to resolve or validate origins.
    pub fn validate_origins(&self, templates: &CheckedProgram) -> Result<(), String> {
        for instance in &self.instances {
            let target = self
                .decls
                .decls
                .get(instance.declaration)
                .ok_or("instance declaration is outside the program")?;
            let (types, sizes) = match (templates.decls.definition(instance.definition), target) {
                (Some(Decl::Func(source)), Decl::Func(_)) => {
                    (source.typevars.len(), source.size_vars.len())
                }
                (Some(Decl::Global { typevars, .. }), Decl::Global { .. }) => (typevars.len(), 0),
                _ => {
                    return Err(
                        "instance origin is missing or has the wrong declaration kind".into(),
                    )
                }
            };
            if instance.type_args.len() != types || instance.size_args.len() != sizes {
                return Err("instance arguments do not match its definition's parameters".into());
            }
        }
        Ok(())
    }
}

fn validate_type(ty: TypeID, concrete: bool) -> Result<(), String> {
    match &*ty {
        Type::Anon(_) => return Err(format!("unsolved type {}", ty.pretty_print())),
        Type::Var(_) if concrete => return Err(format!("non-concrete type {}", ty.pretty_print())),
        Type::Array(element, size) => {
            if concrete && matches!(size, ArraySize::Var(_)) {
                return Err(format!("non-concrete array size in {}", ty.pretty_print()));
            }
            validate_type(*element, concrete)?;
        }
        Type::Slice(element) | Type::Reference(element) => validate_type(*element, concrete)?,
        Type::Tuple(elements) | Type::Name(_, elements) => {
            for &element in elements {
                validate_type(element, concrete)?;
            }
        }
        Type::Func(domain, result) => {
            validate_type(*domain, concrete)?;
            validate_type(*result, concrete)?;
        }
        _ => {}
    }
    Ok(())
}

impl Phase<'_> {
    fn concrete(&self) -> bool {
        matches!(self, Self::Concrete(_))
    }

    fn declaration(&self, declaration: &CheckedDecl) -> Result<(), String> {
        let result = match declaration {
            Decl::Func(function) => self.function(function),
            Decl::Assume { arena, cond } => {
                self.body(arena, &[], &[], &[], &[*cond])?;
                if arena.ty(*cond) != mk_type(Type::Bool) {
                    return Err("global assumption is not boolean".into());
                }
                Ok(())
            }
            Decl::Interface(interface) if !self.concrete() => {
                for function in &interface.funcs {
                    self.function(function)?;
                }
                Ok(())
            }
            Decl::Interface(_) | Decl::Macro(_) => Err("source-only declaration in program".into()),
            Decl::Global { typevars, ty, .. } => {
                if self.concrete() && !typevars.is_empty() {
                    return Err("generic global in concrete program".into());
                }
                validate_type(*ty, self.concrete())
            }
            Decl::Struct(structure) => {
                for field in &structure.fields {
                    validate_type(field.ty, false)?;
                }
                Ok(())
            }
            Decl::Enum { .. } | Decl::Const { .. } => Ok(()),
        };
        result.map_err(|error| format!("{}: {}", declaration.name(), error))
    }

    fn function(&self, function: &CheckedFunction) -> Result<(), String> {
        if self.concrete() && (!function.typevars.is_empty() || !function.size_vars.is_empty()) {
            return Err("generic function in concrete program".into());
        }
        validate_type(function.ret, self.concrete())?;
        let roots: Vec<_> = function
            .requires
            .iter()
            .copied()
            .chain(function.body)
            .collect();
        self.body(
            &function.arena,
            &function.params,
            &function.size_vars,
            &function.closure_vars,
            &roots,
        )?;
        for &root in &function.requires {
            if function.arena.ty(root) != mk_type(Type::Bool) {
                return Err("precondition is not boolean".into());
            }
        }
        Ok(())
    }

    fn body(
        &self,
        body: &CheckedBody,
        params: &[CheckedParam],
        sizes: &[SizeParameter],
        captures: &[LocalId],
        roots: &[ExprID],
    ) -> Result<(), String> {
        validate_edges(body, roots)?;
        self.requirements(body)?;
        let bindings = collect_bindings(body, params, sizes, captures)?;
        // Unused records, e.g. substituted size binders, can remain after
        // transformation; their types must still obey the phase.
        for local in &body.locals {
            validate_type(local.ty, self.concrete())?;
        }
        for (id, node) in body.nodes().iter().enumerate() {
            self.node(node, body, &bindings)
                .map_err(|error| format!("expression {}: {}", id, error))?;
        }
        Ok(())
    }

    fn node(
        &self,
        node: &CheckedNode,
        body: &CheckedBody,
        bindings: &[Option<BinderKind>],
    ) -> Result<(), String> {
        validate_type(node.ty, self.concrete())?;
        match &node.kind {
            Expr::Id(reference) => self.reference(reference, body, bindings)?,
            Expr::TypeApp(reference, args) => {
                self.reference(reference, body, bindings)?;
                for &ty in args {
                    validate_type(ty, self.concrete())?;
                }
            }
            Expr::AsTy(_, ty) => validate_type(*ty, self.concrete())?,
            Expr::Let(_, _, annotation) | Expr::Var(_, _, annotation) => {
                if annotation.is_some() {
                    return Err("checked declaration retains a source annotation".into());
                }
                if node.ty != mk_type(Type::Void) {
                    return Err("checked declaration result type is not void".into());
                }
            }
            Expr::Macro(..) | Expr::Error => return Err("unexpanded or invalid expression".into()),
            Expr::Call(callee, args) => {
                let Type::Func(domain, _) = &*body.ty(*callee) else {
                    return Err("call target has no function type".into());
                };
                let Type::Tuple(parameters) = &**domain else {
                    return Err("call target has no parameter tuple".into());
                };
                if parameters.len() != args.len() {
                    return Err("call arity does not match checked signature".into());
                }
                if let Self::Concrete(program) = self {
                    if let Expr::Id(reference @ Reference::Instance(instance))
                    | Expr::TypeApp(reference @ Reference::Instance(instance), _) =
                        &body[*callee]
                    {
                        // The callee node can occur later in the arena.
                        self.reference(reference, body, bindings)?;
                        if let Some(target) = program.function_instance(*instance) {
                            if target.params.len() != args.len() {
                                return Err("call arity does not match function instance".into());
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn requirements(&self, body: &CheckedBody) -> Result<(), String> {
        let Self::Template(decls) = self else {
            return if body.requirements.is_empty() {
                Ok(())
            } else {
                Err("unresolved interface requirements in concrete body".into())
            };
        };
        for (index, requirement) in body.requirements.iter().enumerate() {
            if requirement.id.index() != index {
                return Err("requirement identity does not match its body coordinate".into());
            }
            let Some(Decl::Interface(interface)) = decls.definition(requirement.interface) else {
                return Err("requirement interface is outside the definition inventory".into());
            };
            if requirement.type_args.len() != interface.typevars.len() {
                return Err("requirement arguments do not match its interface".into());
            }
            let members = decls.interface_members(requirement.interface);
            if !requirement
                .members
                .iter()
                .map(|member| member.definition)
                .eq(members.iter().copied())
            {
                return Err("requirement members do not belong to its interface".into());
            }
            for &ty in &requirement.type_args {
                validate_type(ty, false)?;
            }
            for member in &requirement.members {
                validate_type(member.signature, false)?;
                // An empty candidate list can be a deferred template obligation.
                validate_candidates(&member.candidates, decls)?;
            }
        }
        Ok(())
    }

    fn reference(
        &self,
        reference: &Reference,
        body: &CheckedBody,
        bindings: &[Option<BinderKind>],
    ) -> Result<(), String> {
        match (self, reference) {
            (_, Reference::Local(local)) => {
                if bindings.get(local.index()) != Some(&Some(BinderKind::Value)) {
                    return Err("local reference has no value binder in its body".into());
                }
            }
            (Self::Template(_), Reference::SizeParameter(local)) => {
                if bindings.get(local.index()) != Some(&Some(BinderKind::Size)) {
                    return Err("size reference has no size binder in its body".into());
                }
            }
            (Self::Template(decls), Reference::Global(id)) => {
                if !matches!(decls.definition(*id), Some(Decl::Global { .. })) {
                    return Err("global reference has no global definition".into());
                }
            }
            (Self::Template(decls), Reference::Functions(candidates)) => {
                if candidates.is_empty() {
                    return Err("empty checked overload set".into());
                }
                validate_candidates(candidates, decls)?;
            }
            (
                Self::Template(_),
                Reference::InterfaceMember {
                    requirement,
                    member,
                },
            ) => {
                if !body
                    .requirements
                    .get(requirement.index())
                    .map_or(false, |requirement| {
                        requirement
                            .members
                            .iter()
                            .any(|candidate| candidate.definition == *member)
                    })
                {
                    return Err("interface member reference has no owning requirement".into());
                }
            }
            (Self::Concrete(program), Reference::Instance(id)) => {
                if id.index() >= program.instances.len() {
                    return Err("instance reference is outside its program".into());
                }
            }
            (Self::Template(_), Reference::Instance(_)) => {
                return Err("instance reference in checked template".into())
            }
            (Self::Concrete(_), _) => {
                return Err("Unresolved checked reference in concrete body".into())
            }
        }
        Ok(())
    }
}

/// A referenced local must have one binder in this body, including formal
/// parameters and captures. Collect all binders before validating references.
fn collect_bindings(
    body: &CheckedBody,
    params: &[CheckedParam],
    sizes: &[SizeParameter],
    captures: &[LocalId],
) -> Result<Vec<Option<BinderKind>>, String> {
    let mut bindings = vec![None; body.locals.len()];
    let mut bind = |local: LocalId, kind: BinderKind| -> Result<(), String> {
        let slot = bindings
            .get_mut(local.index())
            .ok_or("local binder is outside its body")?;
        if slot.replace(kind).is_some() {
            return Err(format!("local {} has multiple binders", local.0));
        }
        Ok(())
    };
    for local in params
        .iter()
        .map(|param| param.local)
        .chain(captures.iter().copied())
    {
        bind(local, BinderKind::Value)?;
    }
    for size in sizes {
        bind(size.local, BinderKind::Size)?;
    }
    for node in body.nodes() {
        match &node.kind {
            Expr::Let(local, ..) | Expr::Var(local, ..) | Expr::For { var: local, .. } => {
                bind(*local, BinderKind::Value)?
            }
            Expr::Lambda { params, .. } => {
                for param in params {
                    bind(param.local, BinderKind::Value)?;
                }
            }
            _ => {}
        }
    }
    Ok(bindings)
}

fn validate_candidates(candidates: &[DefId], decls: &CheckedDeclTable) -> Result<(), String> {
    let mut seen = HashSet::new();
    for &candidate in candidates {
        if !matches!(
            decls.definition(candidate),
            Some(Decl::Func(_) | Decl::Global { .. })
        ) {
            return Err("candidate is not a function/global definition".into());
        }
        if !seen.insert(candidate) {
            return Err("duplicate overload candidate".into());
        }
    }
    Ok(())
}

/// Validate every retained edge, including nodes outside the current roots.
/// Forward edges are valid (operator publication appends callees); cycles are
/// not. Use an explicit stack so malformed data cannot recurse indefinitely.
fn validate_edges(body: &CheckedBody, roots: &[ExprID]) -> Result<(), String> {
    if roots.iter().any(|&root| root >= body.len()) {
        return Err("expression root is outside its body".into());
    }
    let mut uses = vec![0usize; body.len()];
    for &root in roots {
        uses[root] += 1;
    }
    for node in body.nodes() {
        for child in node.kind.subexprs() {
            *uses
                .get_mut(child)
                .ok_or("expression edge is outside its body")? += 1;
        }
    }
    let mut state = vec![0u8; body.len()];
    let mut contains_binder = vec![false; body.len()];
    for start in 0..body.len() {
        if state[start] == 2 {
            continue;
        }
        let mut pending = vec![(start, false)];
        while let Some((id, leaving)) = pending.pop() {
            let visit = state
                .get_mut(id)
                .ok_or("expression edge is outside its body")?;
            if leaving {
                contains_binder[id] = match &body[id] {
                    Expr::Let(..) | Expr::Var(..) | Expr::For { .. } => true,
                    Expr::Lambda { params, .. } if !params.is_empty() => true,
                    _ => body[id]
                        .subexprs()
                        .iter()
                        .any(|&child| contains_binder[child]),
                };
                // Sharing reads is allowed. Sharing a declaration-containing
                // subtree would give distinct lexical occurrences one binder,
                // undoing the normalization required before checking/duplication.
                if uses[id] > 1 && contains_binder[id] {
                    return Err("shared binding occurrence in checked body".into());
                }
                *visit = 2;
                continue;
            }
            match *visit {
                2 => continue,
                1 => return Err("cyclic checked expression graph".into()),
                _ => *visit = 1,
            }
            pending.push((id, true));
            pending.extend(body[id].subexprs().into_iter().map(|child| (child, false)));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_function() -> CheckedFunction {
        let mut arena = CheckedBody::new();
        let ty = mk_type(Type::Int32);
        let local = arena.add_local(Name::str("x"), ty, false);
        let body = arena.add(Expr::Id(Reference::Local(local)), ty, test_loc());
        CheckedFunction {
            name: Name::str("main"),
            typevars: vec![],
            size_vars: vec![],
            params: vec![CheckedParam { local }],
            body: Some(body),
            ret: ty,
            requires: vec![],
            loc: test_loc(),
            arena,
            closure_vars: vec![],
            is_extern: false,
        }
    }

    fn record(declaration: usize) -> InstanceRecord {
        InstanceRecord {
            definition: DefId(declaration as u32),
            type_args: vec![],
            size_args: vec![],
            declaration,
        }
    }

    fn concrete(function: CheckedFunction) -> Result<SpecializedProgram, String> {
        SpecializedProgram::try_from_instances(vec![Decl::Func(function)], vec![record(0)])
    }

    fn checked(source: &str) -> CheckedProgram {
        let mut compiler = Compiler::new();
        compiler.quiet = true;
        assert!(
            compiler.parse(source, "boundary.lyte"),
            "{:?}",
            compiler.last_errors
        );
        assert!(compiler.check(), "{:?}", compiler.last_errors);
        compiler.checked_program().unwrap().clone()
    }

    #[test]
    fn publication_rejects_invalid_body_handles_in_both_phases() {
        let corruptions: &[(fn(&mut CheckedFunction), &str)] = &[
            (|f| f.body = Some(f.arena.len()), "root is outside"),
            (|f| f.requires.push(f.arena.len()), "root is outside"),
            (
                |f| {
                    f.arena.add(Expr::Return(999), f.ret, test_loc());
                },
                "edge is outside",
            ),
            (
                |f| {
                    let id = f.arena.len();
                    f.arena.add(Expr::Return(id), f.ret, test_loc());
                },
                "cyclic",
            ),
            (|f| f.params[0].local = LocalId(999), "binder is outside"),
            (|f| f.params.push(f.params[0].clone()), "multiple binders"),
            (
                |f| {
                    f.arena
                        .add(Expr::Id(Reference::Local(LocalId(999))), f.ret, test_loc());
                },
                "no value binder",
            ),
            (
                |f| {
                    let local = f.arena.add_local(Name::str("orphan"), f.ret, false);
                    f.arena
                        .add(Expr::Id(Reference::Local(local)), f.ret, test_loc());
                },
                "no value binder",
            ),
            (
                |f| {
                    f.arena.add(
                        Expr::Lambda {
                            params: vec![CheckedParam {
                                local: LocalId(999),
                            }],
                            body: 0,
                        },
                        f.ty(),
                        test_loc(),
                    );
                },
                "binder is outside",
            ),
            (
                |f| {
                    f.arena.add(Expr::Error, f.ret, test_loc());
                },
                "invalid expression",
            ),
            (
                |f| {
                    f.arena.add(
                        Expr::Macro(Name::str("unexpanded"), vec![]),
                        f.ret,
                        test_loc(),
                    );
                },
                "unexpanded",
            ),
            (
                |f| f.arena.locals[0].ty = mk_type(Type::Anon(100)),
                "unsolved type",
            ),
        ];
        for &(corrupt, expected) in corruptions {
            let mut function = sample_function();
            corrupt(&mut function);
            let error =
                CheckedProgram::try_new(CheckedDeclTable::new(vec![Decl::Func(function.clone())]))
                    .unwrap_err();
            assert!(error.contains(expected), "{}: {}", expected, error);
            let error = concrete(function).unwrap_err();
            assert!(error.contains(expected), "{}: {}", expected, error);
        }
    }

    #[test]
    fn declaration_nodes_require_consumed_annotations_and_void_results_in_both_phases() {
        for declaration in ["let stored = x", "var stored = x", "var stored: i32"] {
            let templates = checked(&format!("main(x: i32) -> i32 {{ {}; x }}", declaration));
            let definition = templates.decls.named_ids(Name::str("main"))[0];
            let function = templates.function(definition).unwrap();
            let id = function
                .arena
                .nodes()
                .iter()
                .position(|node| matches!(node.kind, Expr::Let(..) | Expr::Var(..)))
                .unwrap();
            concrete(function.clone()).unwrap();
            for (annotation, result, expected) in [
                (
                    Some(function.ret),
                    mk_type(Type::Void),
                    "retains a source annotation",
                ),
                (None, function.ret, "result type is not void"),
            ] {
                let mut invalid = function.clone();
                let mut kind = invalid.arena[id].clone();
                match &mut kind {
                    Expr::Let(_, _, ty) | Expr::Var(_, _, ty) => *ty = annotation,
                    _ => unreachable!(),
                }
                invalid.arena.replace(id, kind, result);
                let errors = [
                    CheckedProgram::try_new(CheckedDeclTable::new(vec![Decl::Func(
                        invalid.clone(),
                    )]))
                    .unwrap_err(),
                    concrete(invalid).unwrap_err(),
                ];
                for error in errors {
                    assert!(error.contains(expected), "{}: {}", declaration, error);
                }
            }
        }
    }

    #[test]
    fn direct_calls_validate_selected_instance_arity_even_with_stale_node_types() {
        for type_application in [false, true] {
            for (target_id, target_arity, expected_error) in [
                (InstanceId(1), 1, None),
                (
                    InstanceId(1),
                    2,
                    Some("call arity does not match function instance"),
                ),
                (
                    InstanceId(2),
                    1,
                    Some("instance reference is outside its program"),
                ),
            ] {
                let mut caller = sample_function();
                // Equal diagnostic names cannot substitute for the selected ID.
                let mut target = caller.clone();
                if target_arity == 2 {
                    let local = target
                        .arena
                        .add_local(Name::str("second"), target.ret, false);
                    target.params.push(CheckedParam { local });
                }
                let recorded_type = caller.ty(); // Still says one parameter.
                let argument = caller.body.unwrap();
                let callee = caller.arena.len() + 1;
                caller.body = Some(caller.arena.add(
                    Expr::Call(callee, vec![argument]),
                    caller.ret,
                    test_loc(),
                ));
                let reference = Reference::Instance(target_id);
                let kind = if type_application {
                    Expr::TypeApp(reference, vec![])
                } else {
                    Expr::Id(reference)
                };
                // Validate the forward reference before looking up its target.
                assert_eq!(caller.arena.add(kind, recorded_type, test_loc()), callee);
                let result = SpecializedProgram::try_from_instances(
                    vec![Decl::Func(caller), Decl::Func(target)],
                    vec![record(0), record(1)],
                );
                if let Some(expected) = expected_error {
                    let error = result.unwrap_err();
                    assert!(error.contains(expected), "{}", error);
                } else {
                    result.unwrap();
                }
            }
        }
    }

    #[test]
    fn instance_arity_validation_preserves_indirect_calls_and_deferred_overloads() {
        let templates = checked("var callback: i32 -> i32
            choose() -> i32 { 0 }
            choose(value: i32) -> i32 { value }
            main { callback = choose; let local = |value: i32| { value }; callback(1); local(1); choose() }");
        let main = templates
            .function(templates.decls.named_ids(Name::str("main"))[0])
            .unwrap();
        assert!(main.arena.nodes().iter().any(|node| {
            matches!(&node.kind, Expr::Id(Reference::Functions(candidates)) if candidates.len() == 2)
        }));
        MonomorphPass::new()
            .monomorphize(&templates, Name::str("main"))
            .unwrap()
            .validate()
            .unwrap();
    }

    #[test]
    fn shared_reads_are_valid_but_shared_binding_occurrences_need_duplication() {
        let mut function = sample_function();
        let read = function.body.unwrap();
        let root = function
            .arena
            .add(Expr::Block(vec![read, read]), function.ret, test_loc());
        function.body = Some(root);
        concrete(function.clone()).unwrap();
        let local = function
            .arena
            .add_local(Name::str("y"), function.ret, false);
        let binding = function.arena.add(
            Expr::Let(local, read, None),
            mk_type(Type::Void),
            test_loc(),
        );
        let block = function
            .arena
            .add(Expr::Block(vec![binding]), mk_type(Type::Void), test_loc());
        function
            .arena
            .replace(root, Expr::Block(vec![block, block, read]), function.ret);
        assert!(concrete(function.clone())
            .unwrap_err()
            .contains("shared binding occurrence"));
        let copy = function.arena.duplicate(block);
        function
            .arena
            .replace(root, Expr::Block(vec![block, copy, read]), function.ret);
        concrete(function).unwrap();
    }

    #[test]
    #[should_panic(expected = "invalid member inventory")]
    fn declaration_table_rejects_missing_interface_member_handles() {
        CheckedDeclTable::from_records(vec![DeclRecord {
            definition: DefId(0),
            declaration: Decl::Interface(Interface {
                name: Name::str("Value"),
                typevars: vec![],
                funcs: vec![sample_function()],
                loc: test_loc(),
            }),
            members: vec![],
        }]);
    }

    #[test]
    fn templates_validate_reference_domains_without_selecting_overloads() {
        let references = [
            Reference::Instance(InstanceId(0)),
            Reference::Global(DefId(0)), // This definition is a function.
            Reference::Functions(vec![]),
            Reference::Functions(vec![DefId(999)]),
            Reference::Functions(vec![DefId(0), DefId(0)]),
            Reference::SizeParameter(LocalId(0)), // This binder is a value parameter.
            Reference::InterfaceMember {
                requirement: RequirementId(0),
                member: DefId(0),
            },
        ];
        for reference in references {
            let mut function = sample_function();
            function
                .arena
                .add(Expr::Id(reference), function.ty(), test_loc());
            assert!(
                CheckedProgram::try_new(CheckedDeclTable::new(vec![Decl::Func(function)])).is_err()
            );
        }
        let mut function = sample_function();
        function.arena.add(
            Expr::Id(Reference::Functions(vec![DefId(0)])),
            function.ty(),
            test_loc(),
        );
        let template =
            CheckedProgram::new(CheckedDeclTable::new(vec![Decl::Func(function.clone())]));
        assert!(template.validate().is_ok());
        // Even an orphan node must satisfy the concrete phase contract.
        assert!(concrete(function)
            .unwrap_err()
            .contains("Unresolved checked reference"));
    }

    #[test]
    fn template_generics_sizes_and_unfulfilled_requirements_remain_valid() {
        let templates = checked("struct Box<T> { value: T } interface Missing<T> { missing(x: T) -> T } deferred<T>(x: T) -> T where Missing<T> { let copy = x; var value: T; value = copy; missing(value) } sized<N>(xs: [i32; N]) -> i32 { N } main {} ");
        let deferred = templates.decls.named_ids(Name::str("deferred"))[0];
        assert!(
            templates.function(deferred).unwrap().arena.requirements[0].members[0]
                .candidates
                .is_empty()
        );
        let program = MonomorphPass::new()
            .monomorphize(&templates, Name::str("main"))
            .unwrap();
        assert!(program.validate().is_ok());
        assert!(program
            .decls
            .decls
            .iter()
            .any(|decl| matches!(decl, Decl::Struct(structure) if !structure.typevars.is_empty())));
        assert!(program
            .functions()
            .all(|(_, function)| function.arena.requirements.is_empty()));
    }

    #[test]
    fn requirement_handles_and_candidate_kinds_are_owned_by_templates() {
        let templates = checked("interface Value<T> { value(x: T) -> T } value(x: i32) -> i32 { x } use<T>(x: T) -> T where Value<T> { value(x) } main {} ");
        let corruptions: &[fn(&mut InterfaceRequirement)] = &[
            |r| r.id = RequirementId(1),
            |r| r.interface = DefId(99999),
            |r| r.members[0].definition = r.members[0].candidates[0],
            |r| r.members[0].candidates = vec![r.interface],
            |r| r.type_args.clear(),
        ];
        for corrupt in corruptions {
            let mut records: Vec<_> = templates.decls.records().collect();
            for record in &mut records {
                if let Decl::Func(function) = &mut record.declaration {
                    if function.name == Name::str("use") {
                        corrupt(&mut function.arena.requirements[0]);
                    }
                }
            }
            assert!(CheckedProgram::try_new(CheckedDeclTable::from_records(records)).is_err());
        }
    }

    #[test]
    fn concrete_types_include_locals_annotations_and_instance_arguments() {
        let symbolic = mk_type(Type::Array(
            mk_type(Type::Int32),
            ArraySize::Var(Name::str("N")),
        ));
        for ty in [typevar("T"), symbolic] {
            let mut function = sample_function();
            function.arena.locals[0].ty = ty;
            assert!(concrete(function).unwrap_err().contains("non-concrete"));
            let mut function = sample_function();
            function
                .arena
                .add(Expr::AsTy(0, ty), function.ret, test_loc());
            assert!(concrete(function).unwrap_err().contains("non-concrete"));
            let mut instance = record(0);
            instance.type_args.push(ty);
            assert!(SpecializedProgram::try_from_instances(
                vec![Decl::Func(sample_function())],
                vec![instance]
            )
            .unwrap_err()
            .contains("non-concrete"));
        }
    }

    #[test]
    fn concrete_inventory_is_complete_and_one_to_one() {
        let declarations = vec![Decl::Func(sample_function())];
        for records in [vec![], vec![record(1)], vec![record(0), record(0)]] {
            assert!(SpecializedProgram::try_from_instances(declarations.clone(), records).is_err());
        }
        let mut other = record(1);
        other.definition = DefId(0);
        let error = SpecializedProgram::try_from_instances(
            vec![Decl::Func(sample_function()), Decl::Func(sample_function())],
            vec![record(0), other],
        )
        .unwrap_err();
        assert!(error.contains("duplicate concrete instance key"));
        assert!(SpecializedProgram::try_from_instances(
            vec![Decl::Const {
                name: Name::str("constant"),
                value: 1
            }],
            vec![record(0)]
        )
        .is_err());
        let mut function = sample_function();
        function.arena.add(
            Expr::Id(Reference::Instance(InstanceId(1))),
            function.ty(),
            test_loc(),
        );
        assert!(concrete(function)
            .unwrap_err()
            .contains("instance reference is outside"));
    }

    #[test]
    fn instance_origins_are_validated_while_templates_are_available() {
        let templates =
            CheckedProgram::new(CheckedDeclTable::new(vec![Decl::Func(sample_function())]));
        let mut program = concrete(sample_function()).unwrap();
        program.validate_origins(&templates).unwrap();
        program.instances[0].definition = DefId(100);
        assert!(program.validate_origins(&templates).is_err());
        program.instances[0].definition = DefId(0);
        program.instances[0].type_args.push(mk_type(Type::Int32));
        assert!(program.validate_origins(&templates).is_err());
    }

    #[test]
    fn concrete_validation_preserves_coercing_calls_and_reference_storage_types() {
        let templates = checked("read(xs: [i32]) -> i32 { 42 } update(x: &i32) { x = 1 } main { var x = 0; update(x); read([1, 2]) }");
        let program = MonomorphPass::new()
            .monomorphize(&templates, Name::str("main"))
            .unwrap();
        program.validate().unwrap();
        let update = program
            .functions()
            .find(|(_, function)| function.name == Name::str("update"))
            .unwrap()
            .1;
        assert!(matches!(
            *update.arena.local(update.params[0].local).ty,
            Type::Reference(_)
        ));
        assert!(update
            .arena
            .nodes()
            .iter()
            .any(|node| matches!(node.kind, Expr::Id(Reference::Local(_)))
                && node.ty == mk_type(Type::Int32)));
    }

    #[test]
    fn unsuccessful_concretization_publishes_no_program() {
        let mut compiler = Compiler::new();
        compiler.quiet = true;
        assert!(compiler.parse("generic<T>(x: T) -> T { x }", "generic-root.lyte"));
        assert!(compiler.check());
        compiler.set_entry_points(&["generic"]);
        assert!(compiler.specialize().unwrap_err().contains("non-concrete"));
        assert!(compiler.specialized_program().is_err());
        assert!(compiler.checked_program().is_some());
    }

    #[test]
    fn duplicated_loop_and_lambda_binders_keep_only_their_outer_captures() {
        let templates = checked("main { let outer = 2; for i in 0 .. 2 { let apply = |value: i32| { outer + i + value }; apply(i) } }");
        let mut records: Vec<_> = templates.decls.records().collect();
        let function = records
            .iter_mut()
            .find_map(|record| match &mut record.declaration {
                Decl::Func(function) if function.name == Name::str("main") => Some(function),
                _ => None,
            })
            .unwrap();
        let original = function
            .arena
            .nodes()
            .iter()
            .position(|node| matches!(node.kind, Expr::For { .. }))
            .unwrap();
        let captures = function.arena.captures(original, &[]);
        assert_eq!(captures.len(), 1);
        let previous_locals = function.arena.locals.len();
        let copy = function.arena.duplicate(original);
        assert_eq!(function.arena.captures(copy, &[]), captures);
        // The loop variable, local function value and lambda parameter all freshen.
        assert_eq!(function.arena.locals.len(), previous_locals + 3);
        assert_eq!(function.arena.ty(copy), function.arena.ty(original));
        assert_eq!(function.arena.loc(copy), function.arena.loc(original));
        let root = function.body.unwrap();
        let Expr::Block(mut statements) = function.arena[root].clone() else {
            panic!()
        };
        statements.push(copy);
        function
            .arena
            .replace(root, Expr::Block(statements), mk_type(Type::Void));
        let duplicated = CheckedProgram::try_new(CheckedDeclTable::from_records(records)).unwrap();
        MonomorphPass::new()
            .monomorphize(&duplicated, Name::str("main"))
            .unwrap()
            .validate()
            .unwrap();
    }

    #[test]
    fn deferred_size_safety_failure_blocks_publication_and_retains_templates() {
        let mut compiler = Compiler::new();
        compiler.quiet = true;
        assert!(compiler.parse("struct P { x: i32 } probe<N>(xs: [i32; N]) { var p: P; for i in 0 .. 2 { let value = p.x; xs[5] } } main { probe([1, 2]) }", "safety-before-motion.lyte"));
        assert!(compiler.check(), "{:?}", compiler.last_errors);
        assert!(compiler
            .specialize()
            .unwrap_err()
            .contains("safety check failed"));
        assert!(!compiler.last_safety_errors.is_empty());
        assert!(compiler.specialized_program().is_err());
        assert!(compiler.checked_program().is_some());
    }
}
