//! The program after lexical and type checking.
//!
//! Nodes own their operation, result type and source provenance. Expression and
//! local IDs are handles in one body, not historical identities: cloning a whole
//! body preserves the handles, while duplication inside that body freshens its
//! declarations. Analyses must be recomputed after mutation.
//!
//! See `docs/CHECKED_PROGRAM.md` for the separate template and concrete contracts.
use crate::*;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::ops::{Deref, Index};

mod validate;

macro_rules! identity {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
        pub struct $name(pub u32);
        impl $name {
            pub fn index(self) -> usize {
                self.0 as usize
            }
        }
    };
}
identity!(DefId);
identity!(InstanceId);
identity!(LocalId);
identity!(RequirementId);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Reference {
    Local(LocalId),
    SizeParameter(LocalId),
    Global(DefId),
    /// An ordered overload set, not a selected callee. Specialization consumes
    /// these recorded candidates without repeating source-name lookup.
    Functions(Vec<DefId>),
    InterfaceMember {
        requirement: RequirementId,
        member: DefId,
    },
    Instance(InstanceId),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct CheckedParam {
    pub local: LocalId,
}

/// Links a checked size binder to the existing type-level array-size symbol.
/// Local diagnostic spelling is independent of this semantic correspondence.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct SizeParameter {
    pub symbol: Name,
    pub local: LocalId,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Local {
    /// Diagnostic spelling; references never resolve this name again.
    pub name: Name,
    pub ty: TypeID,
    pub mutable: bool,
}

pub type CheckedExpr = Expr<Reference, LocalId, CheckedParam>;
pub type CheckedDecl = Decl<CheckedFunction>;
pub type CheckedDeclTable = DeclTable<CheckedFunction>;
pub type CheckedDeclarations = DeclarationList<CheckedFunction>;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct CheckedNode {
    pub kind: CheckedExpr,
    pub ty: TypeID,
    pub loc: Loc,
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Hash)]
pub struct CheckedBody {
    nodes: Vec<CheckedNode>,
    pub locals: Vec<Local>,
    pub requirements: Vec<InterfaceRequirement>,
}

impl CheckedBody {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn from_parts(
        nodes: Vec<CheckedNode>,
        locals: Vec<Local>,
        requirements: Vec<InterfaceRequirement>,
    ) -> Self {
        Self {
            nodes,
            locals,
            requirements,
        }
    }
    pub fn len(&self) -> usize {
        self.nodes.len()
    }
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }
    pub fn node(&self, id: ExprID) -> &CheckedNode {
        &self.nodes[id]
    }
    pub fn nodes(&self) -> &[CheckedNode] {
        &self.nodes
    }
    pub fn ty(&self, id: ExprID) -> TypeID {
        self.nodes[id].ty
    }
    pub fn loc(&self, id: ExprID) -> Loc {
        self.nodes[id].loc
    }
    pub fn local(&self, id: LocalId) -> &Local {
        &self.locals[id.index()]
    }
    pub fn add_local(&mut self, name: Name, ty: TypeID, mutable: bool) -> LocalId {
        let id = LocalId(self.locals.len().try_into().expect("too many locals"));
        self.locals.push(Local { name, ty, mutable });
        id
    }
    pub fn add(&mut self, kind: CheckedExpr, ty: TypeID, loc: Loc) -> ExprID {
        let id = self.nodes.len();
        self.nodes.push(CheckedNode { kind, ty, loc });
        id
    }
    /// Replacing a node retains its handle and provenance, not any analysis of
    /// the previous node. Callers provide the replacement's checked result type.
    pub fn replace(&mut self, id: ExprID, kind: CheckedExpr, ty: TypeID) {
        self.nodes[id].kind = kind;
        self.nodes[id].ty = ty;
    }
    pub fn replace_node(&mut self, id: ExprID, node: CheckedNode) {
        self.nodes[id] = node;
    }
    pub fn substitute(&mut self, instance: &Instance) {
        for node in &mut self.nodes {
            node.ty = node.ty.subst(instance);
            match &mut node.kind {
                Expr::AsTy(_, ty) => *ty = ty.subst(instance),
                Expr::TypeApp(_, args) => {
                    for ty in args {
                        *ty = ty.subst(instance);
                    }
                }
                Expr::Let(_, _, ty) | Expr::Var(_, _, ty) => {
                    if let Some(ty) = ty {
                        *ty = ty.subst(instance);
                    }
                }
                _ => {}
            }
        }
        for local in &mut self.locals {
            local.ty = local.ty.subst(instance);
        }
        for requirement in &mut self.requirements {
            *requirement = requirement.subst(instance);
        }
    }

    /// Source-oriented diagnostics over checked nodes; spellings are presentation
    /// data and are never converted back into unresolved syntax.
    pub fn pretty_print(&self, id: ExprID, indent: usize) -> String {
        self.pretty_print_with(id, indent, &|reference| match reference {
            Reference::Local(local) | Reference::SizeParameter(local) => self.local(*local).name,
            Reference::Global(id) => Name::new(format!("global#{}", id.0)),
            Reference::Functions(ids) => Name::new(format!("function#{:?}", ids)),
            Reference::InterfaceMember { member, .. } => Name::new(format!("member#{}", member.0)),
            Reference::Instance(id) => Name::new(format!("instance#{}", id.0)),
        })
    }
    pub fn pretty_print_with(
        &self,
        id: ExprID,
        indent: usize,
        reference_name: &impl Fn(&Reference) -> Name,
    ) -> String {
        let child = |id| self.pretty_print_with(id, indent, reference_name);
        let list = |ids: &[ExprID]| {
            ids.iter()
                .map(|id| child(*id))
                .collect::<Vec<_>>()
                .join(", ")
        };
        match &self[id] {
            Expr::Id(reference) => reference_name(reference).to_string(),
            Expr::TypeApp(reference, args) => format!(
                "{}⟨{}⟩",
                reference_name(reference),
                args.iter()
                    .map(|ty| ty.pretty_print())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expr::Int(value, suffix) => format!(
                "{}{}",
                value,
                suffix.map(|suffix| suffix.to_string()).unwrap_or_default()
            ),
            Expr::Real(value, suffix) => format!(
                "{}{}",
                value,
                suffix.map(|suffix| suffix.to_string()).unwrap_or_default()
            ),
            Expr::String(value) => format!("\"{}\"", value),
            Expr::Char(value) => format!("'{}'", value),
            Expr::True => "true".into(),
            Expr::False => "false".into(),
            Expr::Enum(name) => format!(".{}", name),
            Expr::Error => "<error>".into(),
            Expr::Call(function, args) => format!("{}({})", child(*function), list(args)),
            Expr::Macro(name, args) => format!("@{}({})", name, list(args)),
            Expr::Binop(op, lhs, rhs) => {
                format!("{} {} {}", child(*lhs), format_binop(*op), child(*rhs))
            }
            Expr::Unop(op, value) => format!("{}{}", format_unop(*op), child(*value)),
            Expr::Lambda { params, body } => format!(
                "|{}| {}",
                params
                    .iter()
                    .map(|param| format!(
                        "{}: {}",
                        self.local(param.local).name,
                        self.local(param.local).ty.pretty_print()
                    ))
                    .collect::<Vec<_>>()
                    .join(", "),
                child(*body)
            ),
            Expr::Field(base, name) => format!("{}.{}", child(*base), name),
            Expr::Array(element, size) => format!("[{}; {}]", child(*element), child(*size)),
            Expr::ArrayLiteral(elements) => format!("[{}]", list(elements)),
            Expr::ArrayIndex(array, index) => format!("{}[{}]", child(*array), child(*index)),
            Expr::AsTy(value, ty) => format!("{}:{}", child(*value), ty.pretty_print()),
            Expr::Let(local, init, annotation) => {
                let annotation = annotation
                    .map(|ty| format!(": {}", ty.pretty_print()))
                    .unwrap_or_default();
                format!(
                    "let {}{} = {}",
                    self.local(*local).name,
                    annotation,
                    child(*init)
                )
            }
            Expr::Var(local, init, annotation) => {
                let annotation = annotation
                    .map(|ty| format!(": {}", ty.pretty_print()))
                    .unwrap_or_default();
                let init = init
                    .map(|init| format!(" = {}", child(init)))
                    .unwrap_or_default();
                format!("var {}{}{}", self.local(*local).name, annotation, init)
            }
            Expr::If(cond, yes, no) => {
                let no = no
                    .map(|no| {
                        format!(
                            " else {}",
                            self.pretty_print_with(no, indent + 1, reference_name)
                        )
                    })
                    .unwrap_or_default();
                format!(
                    "if {} {}{}",
                    child(*cond),
                    self.pretty_print_with(*yes, indent + 1, reference_name),
                    no
                )
            }
            Expr::While(cond, body) => format!(
                "while {} {}",
                child(*cond),
                self.pretty_print_with(*body, indent + 1, reference_name)
            ),
            Expr::For {
                var,
                start,
                end,
                body,
            } => format!(
                "for {} in {} .. {} {}",
                self.local(*var).name,
                child(*start),
                child(*end),
                self.pretty_print_with(*body, indent + 1, reference_name)
            ),
            Expr::Block(exprs) => {
                if exprs.is_empty() {
                    return "{}".into();
                }
                let expressions = exprs
                    .iter()
                    .map(|expr| {
                        format!(
                            "{}{}",
                            "    ".repeat(indent + 1),
                            self.pretty_print_with(*expr, indent + 1, reference_name)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n");
                format!("{{\n{}\n{}}}", expressions, "    ".repeat(indent))
            }
            Expr::Return(value) => format!("return {}", child(*value)),
            Expr::Assume(value) => format!("assume {}", child(*value)),
            Expr::Break => "break".into(),
            Expr::Continue => "continue".into(),
            Expr::Tuple(values) => format!("({})", list(values)),
            Expr::StructLit(name, fields) => format!(
                "{}({})",
                name,
                fields
                    .iter()
                    .map(|(name, value)| format!("{}: {}", name, child(*value)))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expr::Arena(value) => format!("arena {}", child(*value)),
        }
    }

    /// Duplicate evaluations. Bindings declared in the copied subtree receive
    /// fresh local IDs; references to enclosing bindings keep their identities.
    /// Whole-body cloning instead preserves every body-local index.
    /// The source subtree must have unique binding occurrences, as required by
    /// program validation; shared reads can occur more than once.
    pub fn duplicate(&mut self, root: ExprID) -> ExprID {
        fn declarations(body: &CheckedBody, id: ExprID, found: &mut HashSet<LocalId>) {
            match &body[id] {
                Expr::Let(local, ..) | Expr::Var(local, ..) | Expr::For { var: local, .. } => {
                    found.insert(*local);
                }
                Expr::Lambda { params, .. } => {
                    found.extend(params.iter().map(|param| param.local));
                }
                _ => {}
            }
            for child in body[id].subexprs() {
                declarations(body, child, found);
            }
        }
        let mut declared = HashSet::new();
        declarations(self, root, &mut declared);
        let mut declared: Vec<_> = declared.into_iter().collect();
        declared.sort();
        let mut locals = HashMap::new();
        for old in declared {
            let local = self.local(old).clone();
            locals.insert(old, self.add_local(local.name, local.ty, local.mutable));
        }
        fn copy(body: &mut CheckedBody, id: ExprID, locals: &HashMap<LocalId, LocalId>) -> ExprID {
            let mut node = body.node(id).clone();
            let remap = |local: &mut LocalId| {
                if let Some(new) = locals.get(local) {
                    *local = *new;
                }
            };
            match &mut node.kind {
                Expr::Id(Reference::Local(local))
                | Expr::TypeApp(Reference::Local(local), _)
                | Expr::Let(local, ..)
                | Expr::Var(local, ..)
                | Expr::For { var: local, .. } => remap(local),
                Expr::Lambda { params, .. } => {
                    for param in params {
                        remap(&mut param.local);
                    }
                }
                _ => {}
            }
            node.kind.map_children(|child| copy(body, child, locals));
            body.add(node.kind, node.ty, node.loc)
        }
        copy(self, root, &locals)
    }

    /// Free local references of a lambda, in source evaluation order. Descending
    /// into nested lambdas includes the captures needed to construct them.
    pub fn captures(&self, root: ExprID, params: &[CheckedParam]) -> Vec<LocalId> {
        crate::free_locals::free_locals(self, root, params.iter().map(|param| param.local)).locals
    }
    pub fn captured_locals(&self) -> HashSet<LocalId> {
        let mut captured = HashSet::new();
        for node in &self.nodes {
            if let Expr::Lambda { params, body } = &node.kind {
                captured.extend(self.captures(*body, params));
            }
        }
        captured
    }
}
impl Index<ExprID> for CheckedBody {
    type Output = CheckedExpr;
    fn index(&self, id: ExprID) -> &Self::Output {
        &self.nodes[id].kind
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct CheckedFunction {
    pub name: Name,
    pub typevars: Vec<Name>,
    pub size_vars: Vec<SizeParameter>,
    pub params: Vec<CheckedParam>,
    pub body: Option<ExprID>,
    pub ret: TypeID,
    pub requires: Vec<ExprID>,
    pub loc: Loc,
    pub arena: CheckedBody,
    pub closure_vars: Vec<LocalId>,
    pub is_extern: bool,
}
impl CheckedFunction {
    pub fn param_types(&self) -> Vec<TypeID> {
        self.params
            .iter()
            .map(|param| self.arena.local(param.local).ty)
            .collect()
    }
    pub fn domain(&self) -> TypeID {
        mk_type(Type::Tuple(self.param_types()))
    }
    pub fn ty(&self) -> TypeID {
        func(self.domain(), self.ret)
    }
    pub fn captured_locals(&self) -> HashSet<LocalId> {
        self.arena.captured_locals()
    }
    pub fn extract_lambda(&self, expression: ExprID, name: Name) -> Self {
        let Expr::Lambda { params, body } = &self.arena[expression] else {
            panic!("expected lambda");
        };
        let Type::Func(_, ret) = &*self.arena.ty(expression) else {
            panic!("checked lambda type");
        };
        Self {
            name,
            typevars: Vec::new(),
            size_vars: Vec::new(),
            params: params.clone(),
            body: Some(*body),
            ret: *ret,
            requires: Vec::new(),
            loc: self.arena.loc(expression),
            arena: self.arena.clone(),
            closure_vars: self.arena.captures(*body, params),
            is_extern: false,
        }
    }
}
impl FunctionInfo for CheckedFunction {
    type Arena = CheckedBody;
    fn name(&self) -> Name {
        self.name
    }
    fn ty(&self) -> TypeID {
        self.ty()
    }
}

#[derive(Clone, Debug)]
pub struct CheckedProgram {
    pub decls: CheckedDeclTable,
}
impl CheckedProgram {
    pub fn new(decls: CheckedDeclTable) -> Self {
        Self::try_new(decls).expect("invalid checked program")
    }
    pub fn try_new(decls: CheckedDeclTable) -> Result<Self, String> {
        let program = Self { decls };
        program.validate()?;
        Ok(program)
    }
    pub fn function(&self, definition: DefId) -> Option<&CheckedFunction> {
        self.decls.function(definition)
    }
}
impl Deref for CheckedProgram {
    type Target = CheckedDeclarations;
    fn deref(&self) -> &Self::Target {
        &self.decls
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct InstanceRecord {
    pub definition: DefId,
    pub type_args: Vec<TypeID>,
    pub size_args: Vec<i32>,
    /// Storage coordinate only; semantic references use InstanceId.
    pub declaration: usize,
}

/// Concrete bodies and instance targets. Source declaration IDs remain origins;
/// they are never used to choose among several concrete specializations.
#[derive(Clone, Debug)]
pub struct SpecializedProgram {
    pub decls: CheckedDeclarations,
    pub instances: Vec<InstanceRecord>,
}
impl SpecializedProgram {
    pub fn from_instances(
        declarations: Vec<CheckedDecl>,
        instances: Vec<InstanceRecord>,
    ) -> Self {
        Self::try_from_instances(declarations, instances).expect("invalid specialized program")
    }
    pub fn try_from_instances(
        declarations: Vec<CheckedDecl>,
        mut instances: Vec<InstanceRecord>,
    ) -> Result<Self, String> {
        let mut sorted: Vec<_> = declarations.into_iter().enumerate().collect();
        sorted.sort_by_key(|(_, declaration)| declaration.name());
        let mut remap = vec![0; sorted.len()];
        for (new, (old, _)) in sorted.iter().enumerate() {
            remap[*old] = new;
        }
        for instance in &mut instances {
            instance.declaration = *remap
                .get(instance.declaration)
                .ok_or("instance declaration is outside the program")?;
        }
        let decls = DeclarationList::from_sorted(
            sorted
                .into_iter()
                .map(|(_, declaration)| declaration)
                .collect(),
        );
        let program = Self { decls, instances };
        program.validate()?;
        Ok(program)
    }
    pub fn instance(&self, id: InstanceId) -> &CheckedDecl {
        &self.decls.decls[self.instances[id.index()].declaration]
    }
    pub fn function_instance(&self, id: InstanceId) -> Option<&CheckedFunction> {
        match self.instance(id) {
            Decl::Func(function) => Some(function),
            _ => None,
        }
    }
    pub fn instance_name(&self, id: InstanceId) -> Name {
        self.instance(id).name()
    }
    pub fn functions(&self) -> impl Iterator<Item = (InstanceId, &CheckedFunction)> {
        self.instances
            .iter()
            .enumerate()
            .filter_map(move |(index, _)| {
                let id = InstanceId(index as u32);
                self.function_instance(id).map(|function| (id, function))
            })
    }
    pub fn globals(&self) -> impl Iterator<Item = (InstanceId, &CheckedDecl)> {
        self.instances
            .iter()
            .enumerate()
            .filter_map(move |(index, _)| {
                let id = InstanceId(index as u32);
                let decl = self.instance(id);
                matches!(decl, Decl::Global { .. }).then_some((id, decl))
            })
    }
    /// Instance inventory in storage order, preserving host global/extern layout.
    pub fn storage_instances(&self) -> impl Iterator<Item = (InstanceId, &CheckedDecl)> {
        let mut ids: Vec<_> = self
            .instances
            .iter()
            .enumerate()
            .map(|(index, record)| (record.declaration, InstanceId(index as u32)))
            .collect();
        ids.sort_by_key(|(coordinate, _)| *coordinate);
        ids.into_iter().map(move |(_, id)| (id, self.instance(id)))
    }
    pub fn instance_for_entry(&self, name: Name) -> Option<InstanceId> {
        self.functions()
            .find_map(|(id, function)| (function.name == name).then_some(id))
    }
}
impl Deref for SpecializedProgram {
    type Target = CheckedDeclarations;
    fn deref(&self) -> &Self::Target {
        &self.decls
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn duplication_freshens_internal_bindings_and_keeps_enclosing_references() {
        let mut body = CheckedBody::new();
        let ty = mk_type(Type::Int32);
        let outer = body.add_local(Name::str("x"), ty, false);
        let inner = body.add_local(Name::str("x"), ty, false);
        let outer_read = body.add(Expr::Id(Reference::Local(outer)), ty, test_loc());
        let binding = body.add(
            Expr::Let(inner, outer_read, None),
            mk_type(Type::Void),
            test_loc(),
        );
        let inner_read = body.add(Expr::Id(Reference::Local(inner)), ty, test_loc());
        let root = body.add(Expr::Block(vec![binding, inner_read]), ty, test_loc());
        let copy = body.duplicate(root);
        let Expr::Block(children) = &body[copy] else {
            panic!()
        };
        let Expr::Let(fresh, initializer, _) = body[children[0]] else {
            panic!()
        };
        assert_ne!(fresh, inner);
        assert_eq!(body[initializer], Expr::Id(Reference::Local(outer)));
        assert_eq!(body[children[1]], Expr::Id(Reference::Local(fresh)));
        assert_eq!(body.local(fresh), body.local(inner));
        assert_eq!(body.ty(copy), body.ty(root));
        assert_eq!(body.loc(copy), body.loc(root));
        assert_eq!(body[inner_read], Expr::Id(Reference::Local(inner)));
    }

    #[test]
    fn captures_follow_bindings_through_shadowing_and_nested_closures() {
        let mut body = CheckedBody::new();
        let ty = mk_type(Type::Int32);
        let callable = func(tuple(vec![]), ty);
        let outer = body.add_local(Name::str("x"), callable, true);
        let inner = body.add_local(Name::str("x"), ty, false);
        let read_outer = body.add(Expr::Id(Reference::Local(outer)), callable, test_loc());
        let read_inner = body.add(Expr::Id(Reference::Local(inner)), ty, test_loc());
        let applied_outer = body.add(
            Expr::TypeApp(Reference::Local(outer), vec![]),
            callable,
            test_loc(),
        );
        let nested_root = body.add(
            Expr::Block(vec![read_inner, applied_outer, read_outer, read_inner]),
            ty,
            test_loc(),
        );
        let nested = body.add(
            Expr::Lambda {
                params: vec![],
                body: nested_root,
            },
            func(mk_type(Type::Tuple(vec![])), ty),
            test_loc(),
        );
        // Preserve first use, including explicit applications and repeated reads,
        // rather than sorting captures by ID or diagnostic spelling.
        assert_eq!(body.captures(nested_root, &[]), vec![inner, outer]);
        assert_eq!(
            body.captures(nested, &[CheckedParam { local: inner }]),
            vec![outer]
        );
    }

    #[test]
    fn whole_body_substitution_retains_local_and_requirement_handles() {
        let mut body = CheckedBody::new();
        let generic = typevar("T");
        let local = body.add_local(Name::str("x"), generic, false);
        let read = body.add(Expr::Id(Reference::Local(local)), generic, test_loc());
        body.requirements.push(InterfaceRequirement {
            id: RequirementId(0),
            interface: DefId(1),
            type_args: vec![generic],
            members: vec![InterfaceMember {
                definition: DefId(2),
                signature: generic,
                candidates: vec![DefId(3)],
            }],
        });
        let mut copy = body.clone();
        let instance: Instance = [(generic, mk_type(Type::Int32))].iter().copied().collect();
        copy.substitute(&instance);
        assert_eq!(copy[read], body[read]);
        assert_eq!(copy.ty(read), mk_type(Type::Int32));
        assert_eq!(copy.local(local).ty, mk_type(Type::Int32));
        assert_eq!(body.ty(read), generic);
        assert_eq!(copy.requirements[0].id, body.requirements[0].id);
        assert_eq!(copy.requirements[0].members[0].definition, DefId(2));
        assert_eq!(copy.requirements[0].members[0].candidates, vec![DefId(3)]);
        assert_eq!(copy.requirements[0].type_args, vec![mk_type(Type::Int32)]);
        assert_eq!(
            copy.requirements[0].members[0].signature,
            mk_type(Type::Int32)
        );
    }

    #[cfg(any(feature = "cranelift", feature = "llvm"))]
    #[test]
    fn checked_captures_preserve_native_parameter_storage() {
        for source in [
            "capture(value: i32, create: bool) -> i32 { if create { let read = || { value }; }; value } main() -> i32 { capture(42, false) }",
            "capture(values: [i32; 2]) -> i32 { var total = 0; for i in 0 .. 2 { let read = || { values[0] }; total = total + read() }; total } main() -> i32 { capture([21, 0]) }",
            "capture(value: &i32) -> i32 { for i in 0 .. 2 { let increment = || { value = value + 1 }; increment() }; value } main() -> i32 { var value = 40; capture(value) }",
        ] {
            let mut compiler = Compiler::new();
            compiler.quiet = true;
            assert!(compiler.parse(source, "."));
            assert!(compiler.check());
            compiler.specialize().unwrap();
            let program = compiler.specialized_program().unwrap();
            #[cfg(feature = "cranelift")]
            {
                let mut jit = crate::JIT::default();
                let (entry, size) = jit.compile(program).unwrap();
                let mut globals = vec![0u8; size];
                unsafe {
                    crate::cancel::set_cancel_callback(globals.as_mut_ptr(), None, std::ptr::null_mut());
                    let entry: extern "C" fn(*mut u8, *mut u8) -> i32 = std::mem::transmute(entry);
                    assert_eq!(entry(globals.as_mut_ptr(), std::ptr::null_mut()), 42, "Cranelift: {}", source);
                }
                jit.free_memory();
            }
            #[cfg(feature = "llvm")]
            {
                let jit = crate::LLVMJIT::new();
                let compiled = jit.compile_only(program, &[Name::str("main")]).unwrap();
                let mut globals = vec![0u8; compiled.globals_size];
                unsafe {
                    crate::cancel::set_cancel_callback(globals.as_mut_ptr(), None, std::ptr::null_mut());
                    let entry: extern "C" fn(*mut u8, *mut u8) -> i32 = std::mem::transmute(compiled.entry_points[&Name::str("main")]);
                    assert_eq!(entry(globals.as_mut_ptr(), std::ptr::null_mut()), 42, "LLVM: {}", source);
                }
            }
        }
    }

    /// Host buffers are rebound for every invocation while the DSP state stays
    /// in the same globals allocation. Guards detect writes outside each slice.
    fn exercise_audio_buffers(
        compiler: &Compiler,
        globals_size: usize,
        globals_base: usize,
        backend: &str,
        mut process: impl FnMut(*mut u8),
    ) {
        let metadata = compiler.globals_info_with_offset(globals_base);
        let offset = |name: &str| metadata.iter().find(|global| global.0 == name).unwrap().1;
        let mut globals = vec![0u8; globals_size];
        let mut expected_state = 0.0f32;
        let mut sample_count = 0usize;
        for frames in [0usize, 1, 17, 239, 240, 255, 256, 257] {
            let mut input = vec![-8192.0f32; frames + 2];
            for (index, sample) in input[1..frames + 1].iter_mut().enumerate() {
                *sample = ((sample_count + index) % 7) as f32 * 0.125 - 0.25;
            }
            let original_input = input.clone();
            let mut output = vec![-16384.0f32; frames + 2];
            let expected: Vec<_> = input[1..frames + 1]
                .iter()
                .map(|sample| {
                    expected_state += sample;
                    expected_state * 0.5
                })
                .collect();
            unsafe {
                crate::ffi::lyte_globals_bind_slice(
                    globals.as_mut_ptr(),
                    offset("input"),
                    input.as_ptr().add(1).cast(),
                    frames as i32,
                );
                crate::ffi::lyte_globals_bind_slice(
                    globals.as_mut_ptr(),
                    offset("output"),
                    output.as_mut_ptr().add(1).cast(),
                    frames as i32,
                );
                std::ptr::write_unaligned(
                    globals.as_mut_ptr().add(offset("frames")).cast::<i32>(),
                    frames as i32,
                );
            }
            process(globals.as_mut_ptr());
            assert_eq!(
                &output[1..frames + 1],
                expected.as_slice(),
                "{} at {} frames",
                backend,
                frames
            );
            assert_eq!(output[0], -16384.0, "{} prefix guard", backend);
            assert_eq!(output[frames + 1], -16384.0, "{} suffix guard", backend);
            assert_eq!(input, original_input, "{} changed input", backend);
            let actual_state = unsafe {
                std::ptr::read_unaligned(globals.as_ptr().add(offset("state")).cast::<f32>())
            };
            assert_eq!(
                actual_state, expected_state,
                "{} persistent state at {} frames",
                backend, frames
            );
            sample_count += frames;
        }
    }

    #[test]
    fn checked_dsp_preserves_arbitrary_buffer_lengths_and_persistent_state() {
        let mut compiler = Compiler::new();
        compiler.quiet = true;
        compiler.no_recursion = true;
        compiler.set_entry_points(&["process"]);
        assert!(compiler.parse(
            r#"
            var frames: i32
            var input: [f32]
            var output: [f32]
            var state: f32
            assume frames >= 0 && frames <= input.len && frames <= output.len
        "#,
            "<prelude>"
        ));
        assert!(compiler.parse(
            r#"
            step(input: f32) -> f32 {
                state = state + input
                state * 0.5
            }
            process {
                for i in 0 .. frames { output[i] = step(input[i]) }
            }
        "#,
            "audio.lyte"
        ));
        assert!(compiler.check(), "{:?}", compiler.last_errors);
        compiler.specialize().unwrap();
        #[cfg(any(feature = "cranelift", feature = "llvm"))]
        let program = compiler.specialized_program().unwrap();
        let entry_name = Name::str("process");

        let vm_program = compiler.compile_vm().unwrap();
        let linked = crate::vm::LinkedProgram::from_program(&vm_program);
        let mut vm = crate::vm::VM::new();
        exercise_audio_buffers(
            &compiler,
            vm_program.globals_size,
            0,
            "register VM",
            |globals| unsafe {
                vm.call_with_external_globals(
                    &linked,
                    &vm_program,
                    vm_program.entry_points[&entry_name],
                    globals,
                    vm_program.globals_size,
                );
                assert!(!vm.cancelled);
            },
        );
        #[cfg(has_stack_interp)]
        {
            let stack_program = compiler.compile_stack().unwrap();
            let mut stack = crate::stack_interp_bridge::StackBackend::new(&stack_program);
            exercise_audio_buffers(
                &compiler,
                stack_program.globals_size,
                crate::cancel::CANCEL_FLAG_RESERVED as usize,
                "C Stack",
                |globals| {
                    stack.call_entry(stack_program.entry_points[&entry_name], globals);
                    assert_eq!(stack.trap_reason(), crate::cancel::TRAP_NONE);
                },
            );
        }
        #[cfg(feature = "cranelift")]
        {
            let mut jit = crate::JIT::default();
            jit.no_recursion = true;
            let (entries, size) = jit.compile_multi(program, &[entry_name]).unwrap();
            let entry: unsafe extern "C" fn(*mut u8, *mut u8) =
                unsafe { std::mem::transmute(entries[&entry_name]) };
            exercise_audio_buffers(
                &compiler,
                size,
                crate::cancel::CANCEL_FLAG_RESERVED as usize,
                "Cranelift",
                |globals| unsafe {
                    crate::cancel::set_cancel_callback(globals, None, std::ptr::null_mut());
                    entry(globals, std::ptr::null_mut());
                },
            );
            jit.free_memory();
        }
        #[cfg(feature = "llvm")]
        {
            let mut jit = crate::LLVMJIT::new();
            jit.no_recursion = true;
            let compiled = jit.compile_only(program, &[entry_name]).unwrap();
            let entry: unsafe extern "C" fn(*mut u8, *mut u8) =
                unsafe { std::mem::transmute(compiled.entry_points[&entry_name]) };
            exercise_audio_buffers(
                &compiler,
                compiled.globals_size,
                crate::cancel::CANCEL_FLAG_RESERVED as usize,
                "LLVM",
                |globals| unsafe {
                    crate::cancel::set_cancel_callback(globals, None, std::ptr::null_mut());
                    entry(globals, std::ptr::null_mut());
                },
            );
        }
    }

    #[cfg(has_stack_interp)]
    #[test]
    fn checked_stack_loop_cancels_and_reenters_through_c_interpreter() {
        unsafe extern "C" fn cancel(user_data: *mut u8) -> bool {
            *user_data.cast::<u32>() += 1;
            true
        }

        let mut compiler = Compiler::new();
        compiler.quiet = true;
        assert!(compiler.parse(
            r#"
            var progress: i32
            var completed: i32
            main {
                for i in 0 .. 8192 { progress = progress + 1 }
                completed = 1
            }
        "#,
            "cancel.lyte"
        ));
        assert!(compiler.check(), "{:?}", compiler.last_errors);
        compiler.specialize().unwrap();
        let metadata =
            compiler.globals_info_with_offset(crate::cancel::CANCEL_FLAG_RESERVED as usize);
        let offset = |name: &str| metadata.iter().find(|global| global.0 == name).unwrap().1;
        let program = compiler.compile_stack().unwrap();
        let entry = program.entry_points[&Name::str("main")];
        let mut stack = crate::stack_interp_bridge::StackBackend::new(&program);
        let mut globals = vec![0u8; program.globals_size];
        let globals_ptr = globals.as_mut_ptr();
        let progress = unsafe { globals_ptr.add(offset("progress")).cast::<i32>() };
        let completed = unsafe { globals_ptr.add(offset("completed")).cast::<i32>() };
        drop(compiler);

        let mut callbacks = 0u32;
        stack.set_cancel_callback(Some(cancel), (&mut callbacks as *mut u32).cast());
        stack.call_entry(entry, globals_ptr);
        assert_eq!(callbacks, 1);
        assert!(stack.cancelled());
        assert_eq!(stack.trap_reason(), crate::cancel::TRAP_CANCELLED);
        let interrupted = unsafe { std::ptr::read_unaligned(progress) };
        assert!(interrupted > 0 && interrupted < 8192);
        assert_eq!(unsafe { std::ptr::read_unaligned(completed) }, 0);

        stack.set_cancel_callback(None, std::ptr::null_mut());
        stack.call_entry(entry, globals_ptr);
        assert!(!stack.cancelled());
        assert_eq!(stack.trap_reason(), crate::cancel::TRAP_NONE);
        assert_eq!(
            unsafe { std::ptr::read_unaligned(progress) },
            interrupted + 8192
        );
        assert_eq!(unsafe { std::ptr::read_unaligned(completed) }, 1);
        assert_eq!(callbacks, 1);
    }

    #[cfg(feature = "llvm")]
    #[test]
    fn checked_llvm_loop_cancels_and_reenters_through_host_api() {
        use crate::ffi::*;
        use std::ffi::{CStr, CString};
        unsafe extern "C" fn cancel(user_data: *mut u8) -> bool {
            *user_data.cast::<u32>() += 1;
            true
        }
        unsafe {
            let compiler = lyte_compiler_new(std::ptr::null(), 0);
            let source = CString::new("var progress: i32\nvar completed: i32\nmain { for i in 0 .. 8192 { progress = progress + 1 }; completed = 1 }").unwrap();
            let filename = CString::new("cancel.lyte").unwrap();
            assert!(lyte_compiler_add_source(
                compiler,
                source.as_ptr(),
                filename.as_ptr()
            ));
            let program = lyte_compiler_compile(compiler);
            assert!(!program.is_null(), "LLVM FFI compilation failed");
            lyte_compiler_free(compiler);
            let size = lyte_program_get_globals_size(program);
            let globals = lyte_globals_alloc(program);
            assert!(!globals.is_null());
            let offset = |name: &str| {
                let index = (0..lyte_program_get_globals_count(program))
                    .find(|index| {
                        CStr::from_ptr(lyte_program_get_global_name(program, *index))
                            .to_str()
                            .unwrap()
                            == name
                    })
                    .unwrap();
                lyte_program_get_global_offset(program, index)
            };
            let progress = globals.add(offset("progress")).cast::<i32>();
            let completed = globals.add(offset("completed")).cast::<i32>();
            let mut callbacks = 0u32;
            lyte_program_set_cancel_callback(
                program,
                Some(cancel),
                (&mut callbacks as *mut u32).cast(),
            );
            assert!(!lyte_entry_point_call(program, 0, globals));
            assert_eq!(callbacks, 1);
            assert_eq!(
                crate::cancel::read_trap_reason(globals),
                crate::cancel::TRAP_CANCELLED
            );
            let interrupted = std::ptr::read_unaligned(progress);
            assert!(interrupted > 0 && interrupted < 8192);
            assert_eq!(std::ptr::read_unaligned(completed), 0);

            lyte_program_set_cancel_callback(program, None, std::ptr::null_mut());
            assert!(lyte_entry_point_call(program, 0, globals));
            assert_eq!(
                crate::cancel::read_trap_reason(globals),
                crate::cancel::TRAP_NONE
            );
            assert_eq!(std::ptr::read_unaligned(progress), interrupted + 8192);
            assert_eq!(std::ptr::read_unaligned(completed), 1);
            lyte_globals_free(globals, size);
            lyte_program_free(program);
        }
    }

    #[test]
    fn concrete_instance_targets_survive_symbol_sorting() {
        let declarations = vec![
            Decl::Global {
                name: Name::str("z"),
                typevars: vec![],
                ty: mk_type(Type::Int32),
            },
            Decl::Global {
                name: Name::str("a"),
                typevars: vec![],
                ty: mk_type(Type::Bool),
            },
        ];
        let records = vec![
            InstanceRecord {
                definition: DefId(0),
                type_args: vec![],
                size_args: vec![],
                declaration: 0,
            },
            InstanceRecord {
                definition: DefId(0),
                type_args: vec![mk_type(Type::Bool)],
                size_args: vec![],
                declaration: 1,
            },
        ];
        let program = SpecializedProgram::from_instances(declarations, records);
        assert_eq!(program.instance_name(InstanceId(0)), Name::str("z"));
        assert_eq!(program.instance_name(InstanceId(1)), Name::str("a"));
        assert_eq!(
            program
                .storage_instances()
                .map(|(id, _)| id)
                .collect::<Vec<_>>(),
            vec![InstanceId(1), InstanceId(0)]
        );
    }
}
