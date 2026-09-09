use crate::*;
use std::ops::Deref;
use superslice::Ext;

/// Declaration storage sorted for source-name candidate collection. Semantic
/// identities are allocated once before sorting, independently of coordinates.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DeclTable<F: FunctionInfo = FuncDecl> {
    list: DeclarationList<F>,
    definitions: Vec<DefinitionLocation>,
    ids: Vec<DefId>,
    member_ids: Vec<Vec<DefId>>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
enum DefinitionLocation {
    Declaration(usize),
    Member(usize, usize),
    Missing,
}

#[derive(Clone, Debug)]
pub struct DeclRecord<F: FunctionInfo = FuncDecl> {
    pub definition: DefId,
    pub declaration: Decl<F>,
    pub members: Vec<DefId>,
}

impl<F: FunctionInfo> DeclTable<F> {
    pub fn new(decls: Vec<Decl<F>>) -> Self {
        let mut next = decls.len() as u32;
        let records = decls
            .into_iter()
            .enumerate()
            .map(|(index, declaration)| {
                let members = match &declaration {
                    Decl::Interface(interface) => interface
                        .funcs
                        .iter()
                        .map(|_| {
                            let id = DefId(next);
                            next += 1;
                            id
                        })
                        .collect(),
                    _ => Vec::new(),
                };
                DeclRecord {
                    definition: DefId(index as u32),
                    declaration,
                    members,
                }
            })
            .collect();
        Self::from_records(records)
    }
    pub fn from_records(mut records: Vec<DeclRecord<F>>) -> Self {
        records.sort_by_key(|record| record.declaration.name());
        let count = records
            .iter()
            .flat_map(|record| {
                std::iter::once(record.definition).chain(record.members.iter().copied())
            })
            .map(|id| id.index() + 1)
            .max()
            .unwrap_or(0);
        let mut definitions = vec![DefinitionLocation::Missing; count];
        let mut ids = Vec::new();
        let mut member_ids = Vec::new();
        let mut decls = Vec::new();
        for (index, record) in records.into_iter().enumerate() {
            let member_count = match &record.declaration {
                Decl::Interface(interface) => interface.funcs.len(),
                _ => 0,
            };
            assert_eq!(
                record.members.len(),
                member_count,
                "invalid member inventory"
            );
            assert_eq!(
                definitions[record.definition.index()],
                DefinitionLocation::Missing,
                "duplicate declaration identity"
            );
            definitions[record.definition.index()] = DefinitionLocation::Declaration(index);
            for (member, id) in record.members.iter().enumerate() {
                assert_eq!(
                    definitions[id.index()],
                    DefinitionLocation::Missing,
                    "duplicate member identity"
                );
                definitions[id.index()] = DefinitionLocation::Member(index, member);
            }
            ids.push(record.definition);
            member_ids.push(record.members);
            decls.push(record.declaration);
        }
        Self {
            list: DeclarationList::from_sorted(decls),
            definitions,
            ids,
            member_ids,
        }
    }
    pub fn definition_count(&self) -> usize {
        self.definitions.len()
    }
    pub fn id_at(&self, index: usize) -> DefId {
        self.ids[index]
    }
    pub fn named_ids(&self, name: Name) -> Vec<DefId> {
        let range = self.decls.equal_range_by(|decl| decl.name().cmp(&name));
        self.ids[range].to_vec()
    }
    pub fn definition(&self, id: DefId) -> Option<&Decl<F>> {
        match self.definitions.get(id.index())? {
            DefinitionLocation::Declaration(index) => Some(&self.decls[*index]),
            _ => None,
        }
    }
    pub fn get(&self, id: DefId) -> Option<&Decl<F>> {
        self.definition(id)
    }
    pub fn function(&self, id: DefId) -> Option<&F> {
        match *self.definitions.get(id.index())? {
            DefinitionLocation::Declaration(index) => match &self.decls[index] {
                Decl::Func(function) | Decl::Macro(function) => Some(function),
                _ => None,
            },
            DefinitionLocation::Member(index, member) => match &self.decls[index] {
                Decl::Interface(interface) => interface.funcs.get(member),
                _ => None,
            },
            DefinitionLocation::Missing => None,
        }
    }
    pub fn signature(&self, id: DefId) -> Option<TypeID> {
        if let Some(function) = self.function(id) {
            function.try_ty()
        } else {
            self.definition(id).map(Decl::ty)
        }
    }
    pub fn interface_members(&self, id: DefId) -> &[DefId] {
        match self.definitions.get(id.index()) {
            Some(DefinitionLocation::Declaration(index)) => &self.member_ids[*index],
            _ => &[],
        }
    }
    pub fn records(&self) -> impl Iterator<Item = DeclRecord<F>> + '_ {
        self.decls
            .iter()
            .enumerate()
            .map(move |(index, declaration)| DeclRecord {
                definition: self.ids[index],
                declaration: declaration.clone(),
                members: self.member_ids[index].clone(),
            })
    }
    pub fn map_bodies<G: FunctionInfo>(
        &self,
        mut map: impl FnMut(DefId, F) -> G,
        mut map_assume: impl FnMut(F::Arena, ExprID) -> G::Arena,
    ) -> DeclTable<G> {
        DeclTable::from_records(
            self.records()
                .map(|record| {
                    let declaration = match record.declaration {
                        Decl::Func(function) => Decl::Func(map(record.definition, function)),
                        Decl::Macro(function) => Decl::Macro(map(record.definition, function)),
                        Decl::Interface(interface) => Decl::Interface(Interface {
                            name: interface.name,
                            typevars: interface.typevars,
                            loc: interface.loc,
                            funcs: interface
                                .funcs
                                .into_iter()
                                .zip(record.members.iter())
                                .map(|(function, id)| map(*id, function))
                                .collect(),
                        }),
                        Decl::Struct(value) => Decl::Struct(value),
                        Decl::Enum { name, cases } => Decl::Enum { name, cases },
                        Decl::Global { name, typevars, ty } => Decl::Global { name, typevars, ty },
                        Decl::Const { name, value } => Decl::Const { name, value },
                        Decl::Assume { arena, cond } => Decl::Assume {
                            arena: map_assume(arena, cond),
                            cond,
                        },
                    };
                    DeclRecord {
                        definition: record.definition,
                        declaration,
                        members: record.members,
                    }
                })
                .collect(),
        )
    }
    pub fn interface_requirement(
        &self,
        id: RequirementId,
        name: Name,
        type_args: Vec<TypeID>,
    ) -> Option<InterfaceRequirement> {
        let interface_id = self
            .named_ids(name)
            .into_iter()
            .find(|id| matches!(self.definition(*id), Some(Decl::Interface(_))))?;
        let Decl::Interface(interface) = self.definition(interface_id)? else {
            unreachable!()
        };
        let instance: Instance = interface
            .typevars
            .iter()
            .zip(&type_args)
            .map(|(name, ty)| (typevar(name), *ty))
            .collect();
        let members = interface
            .funcs
            .iter()
            .zip(self.interface_members(interface_id))
            .map(|(function, definition)| {
                Some(InterfaceMember {
                    definition: *definition,
                    signature: function.try_ty()?.subst(&instance),
                    candidates: self
                        .named_ids(function.name())
                        .into_iter()
                        .filter(|id| {
                            matches!(
                                self.definition(*id),
                                Some(Decl::Func(_) | Decl::Global { .. })
                            )
                        })
                        .collect(),
                })
            })
            .collect::<Option<Vec<_>>>()?;
        Some(InterfaceRequirement {
            id,
            interface: interface_id,
            type_args,
            members,
        })
    }

    pub fn interface_alternative(&self, name: Name, type_args: Vec<TypeID>) -> AltInterface {
        let requirement = self.interface_requirement(RequirementId(0), name, type_args.clone());
        AltInterface {
            interface: name,
            typevars: type_args,
            members: requirement.map(|requirement| requirement.members),
        }
    }
}

impl DeclTable {
    /// Returns all alternatives for a declaration name.
    pub fn alts(&self, name: Name) -> Vec<Alt> {
        let sl = self.find(name);
        let mut alts = vec![];

        for d in sl {
            match d {
                Decl::Func(function) => {
                    let Some(ty) = function.annotated_ty() else {
                        continue;
                    };
                    let mut interfaces = vec![];
                    for c in &function.constraints {
                        interfaces.push(self.interface_alternative(
                            c.interface_name,
                            c.typevars.iter().map(|name| typevar(name)).collect(),
                        ))
                    }

                    alts.push(Alt { ty, interfaces });
                }
                Decl::Global { .. } => {
                    alts.push(Alt {
                        ty: d.ty(),
                        interfaces: vec![],
                    });
                }
                _ => (),
            }
        }

        alts
    }
}

/// Shared declaration storage and type-layout lookup. This list deliberately
/// provides no source-definition or concrete-instance identity APIs; those live
/// on the owning phase's program/table.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DeclarationList<F: FunctionInfo = FuncDecl> {
    pub decls: Vec<Decl<F>>,
    pub enum_cases: Vec<(Name, usize)>,
}
impl<F: FunctionInfo> DeclarationList<F> {
    pub(crate) fn from_sorted(decls: Vec<Decl<F>>) -> Self {
        let mut enum_cases = Vec::new();
        for (index, declaration) in decls.iter().enumerate() {
            if let Decl::Enum { cases, .. } = declaration {
                enum_cases.extend(cases.iter().map(|name| (*name, index)));
            }
        }
        enum_cases.sort();
        Self { decls, enum_cases }
    }
    /// Returns a slice of all decls which match name.
    pub fn find(&self, name: Name) -> &[Decl<F>] {
        let range = self.decls.equal_range_by(|x| x.name().cmp(&name));
        &self.decls[range]
    }

    /// Looks up an entry point function by name.
    ///
    /// Entry points are optional: a name that doesn't resolve to a function
    /// yields None so backends can skip it. Deciding whether a missing entry
    /// point is an error is up to the client.
    ///
    /// Non-function decls sharing the name (a global, struct, etc.) are skipped
    /// rather than shadowing the function, since decls with equal names are
    /// ordered by source position.
    pub fn find_entry_point(&self, name: Name) -> Option<&F> {
        self.entry_point_overloads(name).next()
    }

    /// Returns every function declared with the given name, ignoring non-function
    /// decls that happen to share it.
    pub fn entry_point_overloads(&self, name: Name) -> impl Iterator<Item = &F> {
        self.find(name).iter().filter_map(|d| match d {
            Decl::Func(d) => Some(d),
            _ => None,
        })
    }

    /// Calls f for every enum containing a case named name.
    /// This is for resolving .enum_case expressions.
    pub fn find_enum(&self, name: Name, f: &mut impl FnMut(Name)) {
        let range = self.enum_cases.equal_range_by(|x| x.0.cmp(&name));
        for i in range {
            let decl_idx = self.enum_cases[i].1;
            if let Decl::Enum { name, .. } = self.decls[decl_idx] {
                f(name)
            }
        }
    }

    /// Returns all types for a declaration name.
    pub fn types(&self, name: Name) -> Vec<TypeID> {
        let sl = self.find(name);
        let mut alts = vec![];

        for d in sl {
            match d {
                Decl::Func(_) => {
                    alts.push(d.ty());
                }
                Decl::Global { .. } => {
                    alts.push(d.ty());
                }
                _ => (),
            }
        }

        alts
    }
}
impl<F: FunctionInfo> Deref for DeclTable<F> {
    type Target = DeclarationList<F>;
    fn deref(&self) -> &Self::Target {
        &self.list
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_sorted_decls() {
        let decls: Vec<Decl> = vec![
            Decl::Global {
                name: Name::new("a".into()),
                typevars: vec![],
                ty: mk_type(Type::Void),
            },
            Decl::Global {
                name: Name::new("b".into()),
                typevars: vec![],
                ty: mk_type(Type::Void),
            },
            Decl::Global {
                name: Name::new("b".into()),
                typevars: vec![],
                ty: mk_type(Type::Void),
            },
            Decl::Global {
                name: Name::new("c".into()),
                typevars: vec![],
                ty: mk_type(Type::Void),
            },
        ];

        let sorted = DeclTable::new(decls);
        let d = sorted.find(Name::new("z".into()));
        assert_eq!(d.len(), 0);

        let d = sorted.find(Name::new("b".into()));
        assert_eq!(d.len(), 2);
        assert_eq!(d[0].name(), Name::new("b".into()));
        assert_eq!(d[1].name(), Name::new("b".into()));
    }
}
