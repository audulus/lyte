use crate::*;
use std::collections::HashMap;

/// Converts checked definitions into concrete instances. The cache is reserved
/// before visiting a body, so recursive calls and generic globals always name
/// the same instance, regardless of the path by which they are reached.
pub struct MonomorphPass {
    cache: HashMap<MonomorphKey, InstanceId>,
    records: Vec<InstanceRecord>,
    declarations: Vec<Option<CheckedDecl>>,
    recursion: RecursionDetector,
}

impl Default for MonomorphPass {
    fn default() -> Self {
        Self {
            cache: HashMap::new(),
            records: vec![],
            declarations: vec![],
            recursion: RecursionDetector::new(),
        }
    }
}

impl MonomorphPass {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn monomorphize(
        &mut self,
        program: &CheckedProgram,
        entry_point: Name,
    ) -> Result<SpecializedProgram, String> {
        self.monomorphize_multi(program, &[entry_point])
    }

    pub fn monomorphize_multi(
        &mut self,
        program: &CheckedProgram,
        entry_points: &[Name],
    ) -> Result<SpecializedProgram, String> {
        program.validate()?;
        let decls = &program.decls;
        for &entry_point in entry_points {
            let roots: Vec<_> = decls
                .named_ids(entry_point)
                .into_iter()
                .filter(|id| decls.function(*id).is_some())
                .collect();
            if roots.len() > 1 {
                return Err(format!(
                    "Multiple overloads found for entry point function '{}'",
                    entry_point
                ));
            }
            if let Some(&definition) = roots.first() {
                let source = decls.function(definition).unwrap();
                self.instantiate_function(definition, vec![], vec![], source, decls)?;
            }
        }
        // Non-generic globals exist even when no reached function names them:
        // their storage is part of the host-visible module layout.
        for (index, declaration) in decls.decls.iter().enumerate() {
            match declaration {
                Decl::Global { typevars, ty, .. } if typevars.is_empty() => {
                    self.instantiate_global(decls.id_at(index), None, *ty, decls)?;
                }
                Decl::Func(_) | Decl::Global { .. } | Decl::Interface(_) | Decl::Macro(_) => {}
                Decl::Assume { arena, cond } => {
                    let mut body = arena.clone();
                    self.process_body(&mut body, std::iter::once(*cond), &[], decls)?;
                    self.declarations.push(Some(Decl::Assume {
                        arena: body,
                        cond: *cond,
                    }));
                }
                _ => self.declarations.push(Some(declaration.clone())),
            }
        }
        let declarations = self
            .declarations
            .iter()
            .cloned()
            .map(|decl| decl.ok_or_else(|| "Unfinished concrete instance".to_string()))
            .collect::<Result<Vec<_>, _>>()?;
        let specialized = SpecializedProgram::try_from_instances(
            declarations,
            self.records.clone(),
        )?;
        specialized.validate_origins(program)?;
        Ok(specialized)
    }

    fn reserve(&mut self, key: MonomorphKey) -> InstanceId {
        let id = InstanceId(self.records.len() as u32);
        self.records.push(InstanceRecord {
            definition: key.definition,
            type_args: key.type_args.clone(),
            size_args: key.size_args.clone(),
            declaration: self.declarations.len(),
        });
        self.declarations.push(None);
        self.cache.insert(key, id);
        id
    }

    fn finish(&mut self, id: InstanceId, declaration: CheckedDecl) {
        let slot = self.records[id.0 as usize].declaration;
        self.declarations[slot] = Some(declaration);
    }

    fn process_body(
        &mut self,
        body: &mut CheckedBody,
        roots: impl IntoIterator<Item = ExprID>,
        size_vars: &[SizeParameter],
        decls: &DeclTable<CheckedFunction>,
    ) -> Result<(), String> {
        // Requirements are body-local. Keep their concrete selection in this
        // stack frame while recursively specializing any selected callees.
        let mut selections = HashMap::new();
        for requirement in &body.requirements {
            let selected = requirement
                .select(&Instance::new(), decls)
                .map_err(|error| error.message(requirement.interface, decls))?
                .ok_or("Unresolved body interface requirement")?;
            for member in selected {
                selections.insert((requirement.id, member.member), member.implementation);
            }
        }
        for root in roots {
            self.process_expr(root, body, size_vars, decls, &selections)?;
        }
        // Publication validates every retained node, including nodes outside
        // these roots, without instantiating otherwise unreachable code.
        // Selected implementations are now explicit Instance references. Source
        // requirement/member IDs have no owner in the concrete program.
        body.requirements.clear();
        Ok(())
    }

    fn process_expr(
        &mut self,
        id: ExprID,
        body: &mut CheckedBody,
        size_vars: &[SizeParameter],
        decls: &DeclTable<CheckedFunction>,
        selections: &HashMap<(RequirementId, DefId), DefId>,
    ) -> Result<(), String> {
        match body[id].clone() {
            Expr::Id(_) | Expr::TypeApp(_, _) => {
                self.process_reference(id, &[], body, size_vars, decls, selections)
            }
            Expr::Call(callee, arguments) => {
                // Preserve the prior specialization walk's argument order.
                for &arg in &arguments {
                    self.process_expr(arg, body, size_vars, decls, selections)?;
                }
                if matches!(body[callee], Expr::Id(_) | Expr::TypeApp(_, _)) {
                    self.process_reference(callee, &arguments, body, size_vars, decls, selections)
                } else {
                    self.process_expr(callee, body, size_vars, decls, selections)
                }
            }
            expression => {
                for child in expression.subexprs() {
                    self.process_expr(child, body, size_vars, decls, selections)?;
                }
                Ok(())
            }
        }
    }

    fn process_reference(
        &mut self,
        id: ExprID,
        arguments: &[ExprID],
        body: &mut CheckedBody,
        size_vars: &[SizeParameter],
        decls: &DeclTable<CheckedFunction>,
        selections: &HashMap<(RequirementId, DefId), DefId>,
    ) -> Result<(), String> {
        let explicit = match body[id].clone() {
            Expr::Id(_) => None,
            Expr::TypeApp(_, arguments) => Some(arguments),
            _ => return Err("Expected a checked reference".into()),
        };
        let reference = body
            .reference(id)
            .cloned()
            .ok_or("Expected a checked reference")?;
        let solved = body.ty(id);
        let candidates = match reference {
            Reference::Local(_) | Reference::Instance(_) => return Ok(()),
            Reference::SizeParameter(_) => {
                return Err(format_error(body.loc(id), "Unsubstituted size parameter"));
            }
            Reference::Global(definition) => {
                let instance = self.instantiate_global(definition, explicit, solved, decls)?;
                select_instance(body, id, instance);
                return Ok(());
            }
            Reference::Functions(candidates) => candidates,
            Reference::InterfaceMember {
                requirement,
                member,
            } => vec![*selections
                .get(&(requirement, member))
                .ok_or("Missing checked interface selection")?],
        };

        // Size-generic calls historically precede ordinary overload inference.
        // Candidate order is the checker's order, never another name lookup.
        if explicit.is_none() {
            if let Some((definition, target)) = candidates.iter().find_map(|definition| {
                decls
                    .function(*definition)
                    .filter(|target| !target.size_vars.is_empty())
                    .map(|target| (*definition, target))
            }) {
                let bindings = infer_size_bindings(target, arguments, body);
                if !bindings.is_empty() {
                    let sizes = target
                        .size_vars
                        .iter()
                        .map(|parameter| bindings.get(&parameter.symbol).copied().unwrap_or(0))
                        .collect();
                    let types = infer_type_arguments(target, solved)?;
                    let instance =
                        self.instantiate_function(definition, types, sizes, target, decls)?;
                    select_instance(body, id, instance);
                    substitute_body_sizes(body, &bindings, size_vars);
                    return Ok(());
                }
            }
        }

        // Preserve the old ordinary-overload diagnostic policy: every generic
        // candidate must infer successfully for an implicit function reference.
        let mut inferred = HashMap::new();
        if explicit.is_none() && matches!(*solved, Type::Func(_, _)) {
            for &definition in &candidates {
                if let Some(target) = decls.function(definition) {
                    if !target.typevars.is_empty() {
                        inferred.insert(definition, infer_type_arguments(target, solved)?);
                    }
                }
            }
        }
        for definition in candidates {
            if let Some(Decl::Global { ty, .. }) = decls.definition(definition) {
                if !unify_with_vars(*ty, solved, &mut Instance::new()) {
                    continue;
                }
                let instance =
                    self.instantiate_global(definition, explicit.clone(), solved, decls)?;
                select_instance(body, id, instance);
                return Ok(());
            }
            let target = decls
                .function(definition)
                .ok_or("Resolved function declaration is missing")?;
            let types = if let Some(types) = &explicit {
                if target.typevars.len() != types.len() {
                    continue;
                }
                let substitution: Instance = target
                    .typevars
                    .iter()
                    .zip(types)
                    .map(|(variable, ty)| (mk_type(Type::Var(*variable)), *ty))
                    .collect();
                if !unify(
                    target.ty().subst(&substitution),
                    solved,
                    &mut Instance::new(),
                ) {
                    continue;
                }
                types.clone()
            } else if target.typevars.is_empty() {
                if !unify(target.ty(), solved, &mut Instance::new()) {
                    continue;
                }
                vec![]
            } else {
                let Some(types) = inferred.get(&definition) else {
                    continue;
                };
                types.clone()
            };
            let bindings = infer_size_bindings(target, arguments, body);
            let sizes = target
                .size_vars
                .iter()
                .map(|parameter| bindings.get(&parameter.symbol).copied().unwrap_or(0))
                .collect();
            let instance = self.instantiate_function(definition, types, sizes, target, decls)?;
            select_instance(body, id, instance);
            substitute_body_sizes(body, &bindings, size_vars);
            return Ok(());
        }
        Err(format_error(
            body.loc(id),
            &format!("No checked function candidate matches expression {}", id),
        ))
    }

    fn instantiate_global(
        &mut self,
        definition: DefId,
        explicit: Option<Vec<TypeID>>,
        solved: TypeID,
        decls: &DeclTable<CheckedFunction>,
    ) -> Result<InstanceId, String> {
        let Some(Decl::Global { name, typevars, ty }) = decls.definition(definition) else {
            return Err("Resolved global declaration is missing".into());
        };
        let mut substitution = Instance::new();
        let types = if let Some(types) = explicit {
            if types.len() != typevars.len() {
                return Err(format!(
                    "Wrong number of checked global arguments for '{}'",
                    name
                ));
            }
            for (variable, ty) in typevars.iter().zip(&types) {
                substitution.insert(mk_type(Type::Var(*variable)), *ty);
            }
            types
        } else if typevars.is_empty() {
            vec![]
        } else {
            if !unify_with_vars(*ty, solved, &mut substitution) {
                return Err(format!(
                    "Cannot infer checked global arguments for '{}'",
                    name
                ));
            }
            typevars
                .iter()
                .map(|variable| {
                    let variable = mk_type(Type::Var(*variable));
                    substitution.get(&variable).copied().unwrap_or(variable)
                })
                .collect()
        };
        let key = MonomorphKey::new(definition, types.clone(), vec![]);
        if let Some(&id) = self.cache.get(&key) {
            return Ok(id);
        }
        let id = self.reserve(key);
        self.finish(
            id,
            Decl::Global {
                name: if types.is_empty() {
                    *name
                } else {
                    crate::mangle::mangle_name(*name, &types)
                },
                typevars: vec![],
                ty: ty.subst(&substitution),
            },
        );
        Ok(id)
    }

    fn instantiate_function(
        &mut self,
        definition: DefId,
        types: Vec<TypeID>,
        sizes: Vec<i32>,
        source: &CheckedFunction,
        decls: &DeclTable<CheckedFunction>,
    ) -> Result<InstanceId, String> {
        let key = MonomorphKey::new(definition, types.clone(), sizes.clone());
        if let Some(&id) = self.cache.get(&key) {
            return Ok(id);
        }
        let substitution: Instance = source
            .typevars
            .iter()
            .zip(&types)
            .map(|(variable, ty)| (mk_type(Type::Var(*variable)), *ty))
            .collect();
        let size_bindings: HashMap<_, _> = source
            .size_vars
            .iter()
            .map(|parameter| parameter.symbol)
            .zip(sizes)
            .collect();
        self.recursion.check(&key, source.name)?;
        self.recursion.begin_instantiation(key.clone());
        let id = self.reserve(key);
        // Whole-body copies retain local indices. Their containing InstanceId
        // supplies the distinct owner; no occurrence or binding remap is needed.
        let mut function = source.clone();
        function.name = instance_symbol(source, &types, &size_bindings, &substitution, decls);
        function.typevars.clear();
        function.size_vars.clear();
        function.ret = subst_size_vars(function.ret.subst(&substitution), &size_bindings);
        function.arena.substitute(&substitution);
        substitute_body_sizes(&mut function.arena, &size_bindings, &source.size_vars);
        let roots = function.requires.iter().copied().chain(function.body);
        let result = self
            .process_body(&mut function.arena, roots, &function.size_vars, decls)
            .map_err(|error| format!("{} in '{}'", error, function.name));
        self.recursion.end_instantiation();
        result?;
        self.finish(id, Decl::Func(function));
        Ok(id)
    }
}

/// Symbols are a backend/diagnostic concern, never a specialization key.
fn instance_symbol(
    source: &CheckedFunction,
    types: &[TypeID],
    sizes: &HashMap<Name, i32>,
    substitution: &Instance,
    decls: &DeclTable<CheckedFunction>,
) -> Name {
    if source.typevars.is_empty() && source.size_vars.is_empty() {
        let overloads = decls.find(source.name).iter().filter(|declaration| {
            matches!(declaration, Decl::Func(function) if function.typevars.is_empty())
        }).count();
        return if overloads > 1 {
            crate::mangle::mangle_overload(source.name, &source.param_types())
        } else {
            source.name
        };
    }
    let mut name = crate::mangle::mangle_name(source.name, types).to_string();
    for parameter in &source.size_vars {
        name.push_str(&format!(
            "${}",
            sizes.get(&parameter.symbol).copied().unwrap_or(0)
        ));
    }
    let overloads = decls
        .find(source.name)
        .iter()
        .filter(|declaration| {
            matches!(declaration, Decl::Func(function) if !function.typevars.is_empty()
            && function.typevars.len() == source.typevars.len())
        })
        .count();
    if overloads > 1 {
        let parameters: Vec<_> = source
            .param_types()
            .iter()
            .map(|ty| ty.subst(substitution))
            .collect();
        let suffix = crate::mangle::mangle_name(Name::str(""), &parameters);
        name.push_str(&format!("#{}", suffix));
    }
    Name::new(name)
}

fn infer_type_arguments(function: &CheckedFunction, solved: TypeID) -> Result<Vec<TypeID>, String> {
    if function.typevars.is_empty() {
        return Ok(vec![]);
    }
    let mut fresh_index = 1000;
    let mut fresh_substitution = Instance::new();
    let fresh = function
        .ty()
        .fresh_aux(&mut fresh_index, &mut fresh_substitution);
    let mut substitution = Instance::new();
    if !unify(fresh, solved, &mut substitution) {
        return Err(format!("Cannot infer type arguments for {}", function.name));
    }
    Ok(function
        .typevars
        .iter()
        .map(|variable| {
            let ty = mk_type(Type::Var(*variable));
            find(
                fresh_substitution.get(&ty).copied().unwrap_or(ty),
                &substitution,
            )
        })
        .collect())
}

fn subst_size_vars(ty: TypeID, bindings: &HashMap<Name, i32>) -> TypeID {
    if bindings.is_empty() {
        return ty;
    }
    match &*ty {
        Type::Array(element, size) => mk_type(Type::Array(
            subst_size_vars(*element, bindings),
            match size {
                ArraySize::Var(name) => bindings
                    .get(name)
                    .copied()
                    .map(ArraySize::Known)
                    .unwrap_or_else(|| size.clone()),
                _ => size.clone(),
            },
        )),
        Type::Slice(element) => mk_type(Type::Slice(subst_size_vars(*element, bindings))),
        Type::Reference(element) => mk_type(Type::Reference(subst_size_vars(*element, bindings))),
        Type::Tuple(types) => mk_type(Type::Tuple(
            types
                .iter()
                .map(|ty| subst_size_vars(*ty, bindings))
                .collect(),
        )),
        Type::Func(domain, result) => mk_type(Type::Func(
            subst_size_vars(*domain, bindings),
            subst_size_vars(*result, bindings),
        )),
        Type::Name(name, types) => mk_type(Type::Name(
            *name,
            types
                .iter()
                .map(|ty| subst_size_vars(*ty, bindings))
                .collect(),
        )),
        _ => ty,
    }
}

/// Record the selected concrete target. An explicit application collapses to
/// a plain identifier: its type arguments are consumed by the selection.
fn select_instance(body: &mut CheckedBody, id: ExprID, instance: InstanceId) {
    if let Expr::TypeApp(name, _) = body[id].clone() {
        let ty = body.ty(id);
        body.replace(id, Expr::Id(name), ty);
    }
    body.set_reference(id, Reference::Instance(instance));
}

fn substitute_body_sizes(
    body: &mut CheckedBody,
    bindings: &HashMap<Name, i32>,
    parameters: &[SizeParameter],
) {
    if bindings.is_empty() {
        return;
    }
    let values: HashMap<_, _> = parameters
        .iter()
        .filter_map(|parameter| {
            bindings
                .get(&parameter.symbol)
                .map(|value| (parameter.local, *value))
        })
        .collect();
    for id in 0..body.len() {
        let mut kind = body[id].clone();
        match &mut kind {
            Expr::Id(_) => {
                if let Some(Reference::SizeParameter(local)) = body.reference(id) {
                    if let Some(value) = values.get(local) {
                        kind = Expr::Int(i64::from(*value), None);
                    }
                }
            }
            Expr::TypeApp(_, types) => {
                for ty in types {
                    *ty = subst_size_vars(*ty, bindings);
                }
            }
            Expr::AsTy(_, ty) => *ty = subst_size_vars(*ty, bindings),
            Expr::Let(_, _, Some(ty)) | Expr::Var(_, _, Some(ty)) => {
                *ty = subst_size_vars(*ty, bindings)
            }
            _ => {}
        }
        body.replace(id, kind, subst_size_vars(body.ty(id), bindings));
    }
    for local in &mut body.locals {
        local.ty = subst_size_vars(local.ty, bindings);
    }
    for requirement in &mut body.requirements {
        for ty in &mut requirement.type_args {
            *ty = subst_size_vars(*ty, bindings);
        }
        for member in &mut requirement.members {
            member.signature = subst_size_vars(member.signature, bindings);
        }
    }
}

fn infer_size_bindings_pair(generic: TypeID, concrete: TypeID, out: &mut HashMap<Name, i32>) {
    match (&*generic, &*concrete) {
        (
            Type::Array(generic, ArraySize::Var(name)),
            Type::Array(concrete, ArraySize::Known(size)),
        ) if *size != 0 => {
            out.insert(*name, *size);
            infer_size_bindings_pair(*generic, *concrete, out);
        }
        (Type::Array(generic, _), Type::Array(concrete, _)) => {
            infer_size_bindings_pair(*generic, *concrete, out)
        }
        (Type::Tuple(generic), Type::Tuple(concrete)) => {
            for (generic, concrete) in generic.iter().zip(concrete) {
                infer_size_bindings_pair(*generic, *concrete, out);
            }
        }
        (Type::Func(gd, gr), Type::Func(cd, cr)) => {
            infer_size_bindings_pair(*gd, *cd, out);
            infer_size_bindings_pair(*gr, *cr, out);
        }
        _ => {}
    }
}

fn infer_size_bindings(
    target: &CheckedFunction,
    args: &[ExprID],
    caller: &CheckedBody,
) -> HashMap<Name, i32> {
    let mut bindings = HashMap::new();
    for (parameter, &argument) in target.params.iter().zip(args) {
        infer_size_bindings_pair(
            target.arena.local(parameter.local).ty,
            caller.ty(argument),
            &mut bindings,
        );
    }
    bindings
}

#[cfg(test)]
mod tests {
    use super::*;

    fn checked(source: &str) -> CheckedProgram {
        let mut compiler = Compiler::new();
        compiler.quiet = true;
        assert!(compiler.parse(source, "checked-specialization.lyte"));
        assert!(compiler.check(), "{:?}", compiler.last_errors);
        compiler.checked_program().unwrap().clone()
    }

    fn specialize(source: &str) -> SpecializedProgram {
        MonomorphPass::new()
            .monomorphize(&checked(source), Name::str("main"))
            .unwrap()
    }

    fn targets(function: &CheckedFunction) -> Vec<InstanceId> {
        function
            .arena
            .ids()
            .filter_map(|id| match function.arena.reference(id) {
                Some(Reference::Instance(id)) => Some(*id),
                _ => None,
            })
            .collect()
    }

    #[test]
    fn concrete_program_drops_fulfilled_interfaces_and_rejects_orphan_source_references() {
        let mut source = checked("interface Identity<T> { identify(x: T) -> T } identify(x: i32) -> i32 { x } forward<T>(x: T) -> T where Identity<T> { identify(x) } main { forward(1) }");
        let output = MonomorphPass::new()
            .monomorphize(&source, Name::str("main"))
            .unwrap();
        assert!(!output
            .decls
            .decls
            .iter()
            .any(|decl| matches!(decl, Decl::Interface(_) | Decl::Macro(_))));
        assert!(output
            .functions()
            .all(|(_, function)| function.arena.requirements.is_empty()));
        let definition = source.decls.named_ids(Name::str("identify"))[0];
        let ty = source.function(definition).unwrap().ty();
        let mut records: Vec<_> = source.decls.records().collect();
        let main = records
            .iter_mut()
            .find_map(|record| match &mut record.declaration {
                Decl::Func(function) if function.name == Name::str("main") => Some(function),
                _ => None,
            })
            .unwrap();
        main.arena.add_id(
            Name::str("target"),
            Reference::Functions(vec![definition]),
            ty,
            test_loc(),
        );
        source.decls = DeclTable::from_records(records);
        let error = MonomorphPass::new()
            .monomorphize(&source, Name::str("main"))
            .unwrap_err();
        assert!(error.contains("Unresolved checked reference"), "{}", error);
    }

    #[test]
    fn size_parameter_diagnostic_names_do_not_control_substitution() {
        let mut source = checked("probe<N>(a: [i32; N]) -> i32 { N } main { probe([1, 2, 3]) }");
        let mut records: Vec<_> = source.decls.records().collect();
        let probe = records
            .iter_mut()
            .find_map(|record| match &mut record.declaration {
                Decl::Func(function) if function.name == Name::str("probe") => Some(function),
                _ => None,
            })
            .unwrap();
        let parameter = probe.size_vars[0];
        assert_eq!(parameter.symbol, Name::str("N"));
        probe.arena.locals[parameter.local.index()].name = Name::str("diagnostic_only");
        source.decls = DeclTable::from_records(records);
        let output = MonomorphPass::new()
            .monomorphize(&source, Name::str("main"))
            .unwrap();
        let specialized = output.find_entry_point(Name::str("probe$3")).unwrap();
        assert!(specialized
            .arena
            .exprs()
            .iter()
            .any(|expr| *expr == Expr::Int(3, None)));
        assert_eq!(
            specialized.param_types(),
            vec![mk_type(Type::Array(
                mk_type(Type::Int32),
                ArraySize::Known(3)
            ))]
        );
        assert_eq!(
            specialized.arena.local(parameter.local).name,
            Name::str("diagnostic_only")
        );
    }

    #[test]
    fn nested_specialization_keeps_interface_selection_body_local() {
        let source = checked("interface Printable<T> { to_int(x: T) -> i32 } to_int(x: i32) -> i32 { x } to_int(x: bool) -> i32 { if x { 1 } else { 0 } } nested<U>(x: U) -> i32 where Printable<U> { to_int(x) } show<T>(x: T) -> i32 where Printable<T> { let other = nested(true); to_int(x) } main { show(42) }");
        let integer_implementation = source
            .decls
            .named_ids(Name::str("to_int"))
            .into_iter()
            .find(|definition| {
                source.function(*definition).unwrap().param_types() == vec![mk_type(Type::Int32)]
            })
            .unwrap();
        let output = MonomorphPass::new()
            .monomorphize(&source, Name::str("main"))
            .unwrap();
        let show = output.find_entry_point(Name::str("show$i32")).unwrap();
        assert!(targets(show)
            .iter()
            .any(|target| output.instances[target.index()].definition == integer_implementation));
    }

    #[test]
    fn global_assumptions_follow_the_concrete_storage_instance() {
        let mut source = checked("var limit: i32 main {}");
        let global = source.decls.named_ids(Name::str("limit"))[0];
        let mut arena = CheckedBody::new();
        let reference = arena.add_id(
            Name::str("limit"),
            Reference::Global(global),
            mk_type(Type::Int32),
            test_loc(),
        );
        let zero = arena.add(Expr::Int(0, None), mk_type(Type::Int32), test_loc());
        let condition = arena.add(
            Expr::Binop(Binop::Geq, reference, zero),
            mk_type(Type::Bool),
            test_loc(),
        );
        let mut records: Vec<_> = source.decls.records().collect();
        records.push(DeclRecord {
            definition: DefId(source.decls.definition_count() as u32),
            declaration: Decl::Assume {
                arena,
                cond: condition,
            },
            members: vec![],
        });
        source.decls = DeclTable::from_records(records);
        let output = MonomorphPass::new()
            .monomorphize(&source, Name::str("main"))
            .unwrap();
        let assumption = output
            .decls
            .decls
            .iter()
            .find_map(|declaration| match declaration {
                Decl::Assume { arena, .. } => Some(arena),
                _ => None,
            })
            .unwrap();
        let Some(&Reference::Instance(instance)) = assumption.reference(reference) else {
            panic!("assumption retained a generic-phase reference");
        };
        assert_eq!(output.instances[instance.index()].definition, global);
        assert!(matches!(output.instance(instance), Decl::Global { .. }));
    }

    #[test]
    fn recursive_calls_and_multiple_roots_share_the_reserved_instance() {
        let program = checked("recur<T>(x: T, n: i32) -> T { if n > 0 { recur(x, n - 1) } else { x } } first { recur(1, 2) } second { recur(2, 3) }");
        let output = MonomorphPass::new()
            .monomorphize_multi(
                &program,
                &[
                    Name::str("missing"),
                    Name::str("first"),
                    Name::str("second"),
                ],
            )
            .unwrap();
        let recur = output.instance_for_entry(Name::str("recur$i32")).unwrap();
        assert_eq!(output.find(Name::str("recur$i32")).len(), 1);
        assert!(targets(output.function_instance(recur).unwrap()).contains(&recur));
        for name in ["first", "second"] {
            let function = output.find_entry_point(Name::str(name)).unwrap();
            assert!(targets(function).contains(&recur));
        }
    }

    #[test]
    fn generic_global_storage_is_shared_across_function_instances() {
        let source = checked("var pool<T>: [T; 4] use<T>(x: T) { let a = pool⟨T⟩ } main { use(1); use(true); let b = pool⟨i32⟩ }");
        let definition = source.decls.named_ids(Name::str("pool"))[0];
        let output = MonomorphPass::new()
            .monomorphize(&source, Name::str("main"))
            .unwrap();
        let globals: Vec<_> = output
            .instances
            .iter()
            .enumerate()
            .filter(|(_, record)| record.definition == definition)
            .collect();
        assert_eq!(globals.len(), 2);
        let integer_global = globals
            .iter()
            .find(|(_, record)| record.type_args == vec![mk_type(Type::Int32)])
            .unwrap()
            .0;
        let integer_global = InstanceId(integer_global as u32);
        assert!(
            targets(output.find_entry_point(Name::str("main")).unwrap()).contains(&integer_global)
        );
        assert!(
            targets(output.find_entry_point(Name::str("use$i32")).unwrap())
                .contains(&integer_global)
        );
    }

    #[test]
    fn local_function_references_never_reenter_overload_resolution() {
        let output = specialize("bump(x: i32) -> i32 { x } bump(x: f32) -> f32 { x } inc(x: i32) -> i32 { x + 1 } main { let bump = inc; bump(0) }");
        let main = output.find_entry_point(Name::str("main")).unwrap();
        let binding = main
            .arena
            .locals
            .iter()
            .position(|local| local.name == Name::str("bump"))
            .unwrap();
        assert!(main
            .arena
            .ids()
            .any(|id| main.arena.reference(id) == Some(&Reference::Local(LocalId(binding as u32)))));
        assert!(output.find(Name::str("bump$i32")).is_empty());
        assert!(output.find(Name::str("bump$f32")).is_empty());
    }

    #[test]
    fn size_substitution_respects_local_shadowing() {
        let output = specialize("probe<N>(a: [i32; N]) { let size = N; if true { let N = 99; let local = N }; let again = N } main { probe([1,2,3]) }");
        let function = output.find_entry_point(Name::str("probe$3")).unwrap();
        assert_eq!(
            function
                .arena
                .exprs()
                .iter()
                .filter(|expr| **expr == Expr::Int(3, None))
                .count(),
            2
        );
        assert!(function.arena.ids().any(|id| matches!(function.arena.reference(id),
            Some(Reference::Local(local)) if function.arena.local(*local).name == Name::str("N"))));
        assert!(!function
            .arena
            .ids()
            .any(|id| matches!(function.arena.reference(id), Some(Reference::SizeParameter(_)))));
    }

    #[test]
    fn ordinary_generic_overload_diagnostics_are_preserved() {
        let source = checked(
            "choose<T>(x: T) -> i32 { 1 } choose<T>(x: [T; 2]) -> i32 { 2 } main { choose(1) }",
        );
        let error = MonomorphPass::new()
            .monomorphize(&source, Name::str("main"))
            .unwrap_err();
        assert!(
            error.contains("Cannot infer type arguments for choose"),
            "{}",
            error
        );
    }
}
