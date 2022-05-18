use crate::*;

pub enum Constraint2 {
    /// Equality.
    Equal(TypeID, TypeID, Loc),

    /// Function overloads.
    Or(TypeID, Vec<TypeID>, Loc),

    /// Field access.
    Field(TypeID, Name, TypeID, Loc),
}

pub fn iterate_solver(
    constraints: &mut [Constraint2],
    instance: &mut Instance,
    decls: &[Decl],
) -> Result<(), TypeError> {
    for constraint in constraints {
        match constraint {
            Constraint2::Equal(a, b, loc) => {
                if !unify(*a, *b, instance) {
                    return Err(TypeError {
                        location: *loc,
                        message: "failed equal constraint".into(),
                    });
                }
            }
            Constraint2::Or(t, alts, loc) => {
                // Try to narrow it down.
                alts.retain(|tt| {
                    let mut inst = Instance::new();
                    unify(*t, *tt, &mut inst)
                });

                // Nothing works!
                if alts.is_empty() {
                    return Err(TypeError {
                        location: *loc,
                        message: "failed or constraint".into(),
                    });
                }

                // Just a single option. Better unify!
                if alts.len() == 1 {
                    *constraint = Constraint2::Equal(*t, alts[0], *loc);
                }
            }
            Constraint2::Field(struct_ty, name, ft, loc) => {
                if let Type::Name(struct_name, _) = **struct_ty {
                    // We've narrowed it down. Better unify!
                }
            }
        }
    }

    Ok(())
}
