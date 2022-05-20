use crate::*;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum Constraint2 {
    /// Equality.
    Equal(TypeID, TypeID, Loc),

    /// Function overloads, enum leading dot syntax, and number literals.
    Or(TypeID, Vec<TypeID>, Loc),

    /// Field access.
    Field(TypeID, Name, TypeID, Loc),
}

impl Constraint2 {
    pub fn solved(&self, inst: &Instance) -> bool {
        match self {
            Constraint2::Equal(a, b, _) => solved_inst(*a, inst) && solved_inst(*b, inst),
            Constraint2::Or(_, _, _) => false,
            Constraint2::Field(struct_ty, _, ft, _) => {
                solved_inst(*struct_ty, inst) && solved_inst(*ft, inst)
            }
        }
    }

    pub fn loc(&self) -> Loc {
        match self {
            Constraint2::Equal(_, _, loc) => *loc,
            Constraint2::Or(_, _, loc) => *loc,
            Constraint2::Field(_, _, _, loc) => *loc,
        }
    }

    pub fn print(&self, inst: &Instance) {
        match self {
            Constraint2::Equal(a, b, loc) => println!(
                "Equal({:?}, {:?}, {:?})",
                subst(*a, inst),
                subst(*b, inst),
                loc
            ),
            Constraint2::Or(a, alts, loc) => println!(
                "Or({:?}, {:?}, {:?})",
                subst(*a, inst),
                (*alts)
                    .iter()
                    .map(|t| subst(*t, inst))
                    .collect::<Vec<TypeID>>(),
                loc
            ),
            Constraint2::Field(a, name, b, loc) => println!(
                "Field({:?}, {:?}, {:?}, {:?})",
                subst(*a, inst),
                name,
                subst(*b, inst),
                loc
            ),
        }
    }
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
                    // Start from the instance we know so far.
                    let mut inst = instance.clone();
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
            Constraint2::Field(struct_ty, field_name, ft, loc) => {
                match *find(*struct_ty, instance) {
                    Type::Name(struct_name, _) => {
                        let decl = find_decl(decls, struct_name).unwrap();

                        // We've narrowed it down. Better unify!
                        if let Some(field) = decl.find_field(*field_name) {
                            *constraint = Constraint2::Equal(field.ty, *ft, *loc);
                        } else {
                            return Err(TypeError {
                                location: *loc,
                                message: format!("no such field: {:?}", field_name).into(),
                            });
                        }
                    }
                    Type::Array(_, _) => {
                        if *field_name == Name::new("len".into()) {
                            *constraint = Constraint2::Equal(mk_type(Type::Int32), *ft, *loc);
                        } else {
                            return Err(TypeError {
                                location: *loc,
                                message: format!("array only has len field, not {:?}", field_name).into(),
                            });
                        }
                    }
                    _ => ()
                }
            }
        }
    }

    Ok(())
}

fn constraints_hash(constraints: &[Constraint2]) -> u64 {
    let mut s = DefaultHasher::new();
    constraints.hash(&mut s);
    s.finish()
}

pub fn print_constraints(constraints: &[Constraint2], inst: &Instance) {
    for c in constraints {
        c.print(inst);
    }
}

pub fn solved_constraints(
    constraints: &[Constraint2],
    instance: &Instance,
) -> Result<(), TypeError> {
    for c in constraints {
        if !c.solved(instance) {
            return Err(TypeError {
                location: c.loc(),
                message: format!("ambiguous constraint: {:?}", c).into(),
            });
        }
    }

    Ok(())
}

pub fn solve_constraints(
    constraints: &mut [Constraint2],
    instance: &mut Instance,
    decls: &[Decl],
) -> Result<(), TypeError> {

    println!("NEW constraints before solve: ");
    print_constraints(constraints, &Instance::new());

    // Continue to propagate as long as we
    // can make changes.
    let mut i = 0;
    loop {
        let h = constraints_hash(constraints);
        println!("---- solve iteration {}", i);
        let old_instance = instance.clone();
        iterate_solver(constraints, instance, decls)?;

        if h == constraints_hash(constraints) && *instance == old_instance {
            // No more progress.
            break;
        }
        i += 1;
    }

    println!("NEW constraints after solve: ");
    print_constraints(constraints, instance);

    solved_constraints(constraints, instance)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_solve_1() {
        let t = anon(0);
        let vd = mk_type(Type::Void);
        let mut constraints = [Constraint2::Equal(vd, t, test_loc())];
        let mut instance = Instance::new();

        assert!(iterate_solver(&mut constraints, &mut instance, &[]).is_ok());
        assert_eq!(instance[&t], vd);
    }

    #[test]
    pub fn test_solve_2() {
        let int8 = mk_type(Type::Int8);
        let vd = mk_type(Type::Void);
        let mut constraints = [Constraint2::Equal(vd, int8, test_loc())];
        let mut instance = Instance::new();

        assert!(iterate_solver(&mut constraints, &mut instance, &[]).is_err());
    }

    #[test]
    pub fn test_solve_3() {
        let int8 = mk_type(Type::Int8);
        let t = anon(0);
        let vd = mk_type(Type::Void);
        let mut constraints = [
            Constraint2::Equal(vd, t, test_loc()),
            Constraint2::Equal(int8, t, test_loc()),
        ];
        let mut instance = Instance::new();

        assert!(iterate_solver(&mut constraints, &mut instance, &[]).is_err());
    }

    #[test]
    pub fn test_or_1() {
        let i = mk_type(Type::Int32);
        let f = mk_type(Type::Float32);

        let mut constraints = [Constraint2::Or(i, vec![i, f], test_loc())];

        let mut instance = Instance::new();
        assert!(iterate_solver(&mut constraints, &mut instance, &[]).is_ok());
        assert!(iterate_solver(&mut constraints, &mut instance, &[]).is_ok());
    }

    #[test]
    pub fn test_field_2() {
        let i = mk_type(Type::Int32);
        let xname = Name::new("x".into());
        let s0name = Name::new("S0".into());

        let decls = vec![Decl::Struct {
            name: s0name,
            fields: vec![Field { name: xname, ty: i }],
            typevars: vec![],
        }];

        let struct_ty = mk_type(Type::Name(s0name, vec![]));
        let v = anon(0);

        let mut constraints = [Constraint2::Field(struct_ty, xname, v, test_loc())];

        let mut instance = Instance::new();
        assert!(iterate_solver(&mut constraints, &mut instance, &decls).is_ok());
        assert!(iterate_solver(&mut constraints, &mut instance, &decls).is_ok());
    }
}
