//! Checked interface candidates and exact-signature selection. Name lookup is
//! confined to candidate collection; selecting a concrete member uses IDs only.
use crate::*;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct InterfaceMember {
    pub definition: DefId,
    /// Signature after substituting the enclosing where clause's parameters.
    pub signature: TypeID,
    pub candidates: Vec<DefId>,
}

impl InterfaceMember {
    pub fn subst(&self, instance: &Instance) -> Self {
        Self {
            signature: self.signature.subst(instance),
            ..self.clone()
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct InterfaceRequirement {
    pub id: RequirementId,
    pub interface: DefId,
    pub type_args: Vec<TypeID>,
    pub members: Vec<InterfaceMember>,
}

impl InterfaceRequirement {
    /// Whole-body specialization preserves body-local requirement indices.
    pub fn subst(&self, instance: &Instance) -> Self {
        Self {
            type_args: self.type_args.iter().map(|ty| ty.subst(instance)).collect(),
            members: self
                .members
                .iter()
                .map(|member| member.subst(instance))
                .collect(),
            ..self.clone()
        }
    }

    pub fn select<F: FunctionInfo>(
        &self,
        instance: &Instance,
        decls: &DeclTable<F>,
    ) -> Result<Option<Vec<SelectedInterfaceMember>>, InterfaceSelectionError> {
        select_interface_members(&self.type_args, &self.members, instance, decls)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SelectedInterfaceMember {
    pub member: DefId,
    pub implementation: DefId,
    pub substitution: Instance,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InterfaceSelectionError {
    pub member: DefId,
}

impl InterfaceSelectionError {
    pub fn message<F: FunctionInfo>(&self, interface: DefId, decls: &DeclTable<F>) -> String {
        let member = decls
            .function(self.member)
            .expect("interface member identity")
            .name();
        let interface = decls
            .definition(interface)
            .expect("interface identity")
            .name();
        format!(
            "function {} for interface {} is required",
            member, interface
        )
    }
}

/// The signature rule intentionally differs from overload unification: generic
/// implementations and reference/array-to-slice coercions do not match merely
/// because their signatures can unify.
pub fn select_interface_member<F: FunctionInfo>(
    member: &InterfaceMember,
    instance: &Instance,
    decls: &DeclTable<F>,
) -> Result<SelectedInterfaceMember, InterfaceSelectionError> {
    let signature = member.signature.subst(instance);
    // Existing interface matching chooses the first exact candidate in source
    // order. In particular, programs may redeclare the stdlib's cmp signature.
    // Keep that policy separate from ordinary overload ambiguity diagnostics.
    let implementation = member
        .candidates
        .iter()
        .copied()
        .find(|id| decls.signature(*id) == Some(signature))
        .ok_or(InterfaceSelectionError {
            member: member.definition,
        })?;
    Ok(SelectedInterfaceMember {
        member: member.definition,
        implementation,
        substitution: instance.clone(),
    })
}

pub fn select_interface_members<F: FunctionInfo>(
    type_args: &[TypeID],
    members: &[InterfaceMember],
    instance: &Instance,
    decls: &DeclTable<F>,
) -> Result<Option<Vec<SelectedInterfaceMember>>, InterfaceSelectionError> {
    // Preserve the existing deferral boundary (top-level Var/Anon only).
    if type_args
        .iter()
        .any(|ty| matches!(&*ty.subst(instance), Type::Var(_) | Type::Anon(_)))
    {
        return Ok(None);
    }
    members
        .iter()
        .map(|member| select_interface_member(member, instance, decls))
        .collect::<Result<Vec<_>, _>>()
        .map(Some)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn table(source: &str) -> DeclTable {
        let mut errors = Vec::new();
        let table = DeclTable::new(crate::parser::parse_program_str(source, &mut errors));
        assert!(errors.is_empty(), "{:?}", errors);
        table
    }

    #[test]
    fn selection_keeps_declaration_identity_and_source_candidate_order() {
        let source = table("interface Value<T> { value(x: T) -> i32 }\nvalue(x: i32) -> i32 { 1 }\nvalue(x: i32) -> i32 { 2 }");
        let requirement = source
            .interface_requirement(
                RequirementId(0),
                Name::str("Value"),
                vec![mk_type(Type::Int32)],
            )
            .unwrap();
        let expected = requirement.members[0].candidates[0];
        let mut records: Vec<_> = source.records().collect();
        records.reverse();
        for record in &mut records {
            match &mut record.declaration {
                Decl::Func(function) => function.name = Name::str("renamed"),
                Decl::Interface(interface) => interface.funcs[0].name = Name::str("renamed_member"),
                _ => unreachable!(),
            }
        }
        let renamed = DeclTable::from_records(records);
        let selected = requirement
            .select(&Instance::new(), &renamed)
            .unwrap()
            .unwrap();
        assert_eq!(selected[0].implementation, expected);
        assert_eq!(
            renamed.function(selected[0].member).unwrap().name,
            Name::str("renamed_member")
        );
    }

    #[test]
    fn exact_interface_matching_does_not_adopt_overload_coercions() {
        for source in [
            "interface Value<T> { value(x: T) -> i32 }\nvalue<U>(x: U) -> i32 { 1 }",
            "interface Value<T> { value(x: [T; 3]) -> i32 }\nvalue(x: [i32]) -> i32 { 1 }",
            "interface Value<T> { value(x: &T) -> i32 }\nvalue(x: i32) -> i32 { 1 }",
        ] {
            let table = table(source);
            let requirement = table
                .interface_requirement(
                    RequirementId(0),
                    Name::str("Value"),
                    vec![mk_type(Type::Int32)],
                )
                .unwrap();
            assert!(
                requirement.select(&Instance::new(), &table).is_err(),
                "{}",
                source
            );
        }
    }
}
