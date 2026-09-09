//! Editor facts from one checking run, including incomplete bodies.
//!
//! This is deliberately not a checked program or executable input. The source
//! inventory owns definition IDs; each body's source arena and local inventory
//! own expression/local IDs. None of these coordinates survive another analysis.
use crate::*;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, Default)]
pub struct ExpressionFacts {
    /// An established type, never an inference variable or recovery placeholder.
    pub ty: Option<TypeID>,
    /// Recorded lexical resolution, independent of type availability. An overload
    /// set is an ordered candidate inventory, not a selected call target.
    pub reference: Option<Reference>,
    /// The binding introduced by a let/var/for expression, when visited.
    pub binding: Option<LocalId>,
}

#[derive(Clone, Debug)]
pub struct AnalyzedLocal {
    pub name: Name,
    pub ty: Option<TypeID>,
    /// Existing source precision: parameters use their enclosing function/lambda
    /// location; declarations use the binding statement's location.
    pub loc: Loc,
}

#[derive(Clone, Debug)]
pub struct BodyAnalysis {
    pub(crate) expressions: Vec<ExpressionFacts>,
    pub(crate) locals: Vec<AnalyzedLocal>,
    pub(crate) requirements: HashMap<RequirementId, DefId>,
}

impl BodyAnalysis {
    pub fn expression(&self, id: ExprID) -> Option<&ExpressionFacts> {
        self.expressions.get(id)
    }

    pub fn local(&self, id: LocalId) -> Option<&AnalyzedLocal> {
        self.locals.get(id.index())
    }

    /// The declaration owning this requirement's members. Failed requirements
    /// can leave gaps in the checker's ordinal IDs. This is identity information;
    /// partially solved requirement signatures are not exposed as type facts.
    pub fn requirement_interface(&self, id: RequirementId) -> Option<DefId> {
        self.requirements.get(&id).copied()
    }
}

/// An immutable snapshot produced by `Compiler::analyze`. Source declarations
/// are inventory, not certified signatures. Missing bodies/facts mean unavailable,
/// never permission to reconstruct semantic meaning by looking up a spelling.
#[derive(Clone, Debug)]
pub struct SourceAnalysis {
    declarations: DeclTable,
    pub(crate) bodies: HashMap<DefId, BodyAnalysis>,
    unavailable_signatures: HashSet<DefId>,
    recovered_types: HashSet<Name>,
    recovered_files: HashSet<Name>,
}

impl SourceAnalysis {
    pub(crate) fn new(
        declarations: DeclTable,
        mut recovered: HashSet<DefId>,
        recovered_files: HashSet<Name>,
    ) -> Self {
        let mut recovered_types = HashSet::new();
        for record in declarations.records() {
            if recovered.contains(&record.definition) {
                recovered.extend(record.members);
                if matches!(record.declaration, Decl::Struct(_) | Decl::Enum { .. }) {
                    recovered_types.insert(record.declaration.name());
                }
            }
        }
        let mut analysis = Self {
            declarations,
            bodies: HashMap::new(),
            unavailable_signatures: recovered,
            recovered_types,
            recovered_files,
        };
        for record in analysis.declarations.records() {
            let functions = std::iter::once(record.definition).chain(record.members);
            for id in functions {
                if let Some(function) = analysis.declarations.function(id) {
                    let mut signature_scope = function.clone();
                    if let Decl::Interface(interface) = &record.declaration {
                        signature_scope.typevars.extend(&interface.typevars);
                    }
                    if !function
                        .annotated_ty()
                        .is_some_and(|ty| analysis.type_is_available(ty, &signature_scope))
                    {
                        analysis.unavailable_signatures.insert(id);
                    }
                }
            }
        }
        analysis
    }

    pub fn declarations(&self) -> &DeclTable {
        &self.declarations
    }

    pub fn body(&self, definition: DefId) -> Option<&BodyAnalysis> {
        self.bodies.get(&definition)
    }

    pub(crate) fn source_is_trusted(&self, source: &FuncDecl) -> bool {
        !self.recovered_files.contains(&source.loc.file)
            && source
                .arena
                .locs
                .iter()
                .all(|loc| !self.recovered_files.contains(&loc.file))
    }

    pub(crate) fn reference_is_trusted(&self, reference: &Reference) -> bool {
        match reference {
            Reference::Global(id) | Reference::InterfaceMember { member: id, .. } => {
                !self.unavailable_signatures.contains(id)
            }
            Reference::Functions(ids) => {
                !ids.is_empty()
                    && ids
                        .iter()
                        .all(|id| !self.unavailable_signatures.contains(id))
            }
            Reference::Local(_) | Reference::SizeParameter(_) => true,
            Reference::Instance(_) => false,
        }
    }

    pub(crate) fn type_is_available(&self, ty: TypeID, source: &FuncDecl) -> bool {
        !ty.contains_anon() && self.type_is_valid(ty, source)
    }

    /// Check annotation provenance and named-type validity using the same source
    /// declaration inventory. This is not overload or lexical name resolution.
    /// Anonymous variables are allowed here only to inspect pre-solve types;
    /// type_is_available additionally excludes them from published facts.
    pub(crate) fn type_is_valid(&self, ty: TypeID, source: &FuncDecl) -> bool {
        match &*ty {
            Type::Name(name, args) => {
                !self.recovered_types.contains(name)
                    && match self.declarations.find(*name).first() {
                        Some(Decl::Struct(st)) => st.typevars.len() == args.len(),
                        Some(Decl::Enum { .. }) => args.is_empty(),
                        _ => false,
                    }
                    && args.iter().all(|ty| self.type_is_valid(*ty, source))
            }
            Type::Tuple(args) => args.iter().all(|ty| self.type_is_valid(*ty, source)),
            Type::Func(domain, ret) => {
                self.type_is_valid(*domain, source) && self.type_is_valid(*ret, source)
            }
            // The parser already checks size-symbol scope. A callee's size
            // parameter can remain in a successfully checked call signature
            // until specialization, even though it is not a caller parameter.
            Type::Array(ty, _) | Type::Slice(ty) | Type::Reference(ty) => {
                self.type_is_valid(*ty, source)
            }
            // Named generic parameters are established symbolic types.
            Type::Var(name) => source.typevars.contains(name),
            _ => true,
        }
    }
}
