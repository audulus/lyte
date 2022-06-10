use crate::*;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// An satisfies-interface constraint. This is close
/// to InterfaceConstraint except the types will be
/// anonymous type variables.
///
/// Note: we could use the same type for both AltInterface
/// and InterfaceConstraint, but InterfaceConstraint reflects
/// that the typevars must be a list of names.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct AltInterface {
    pub interface: Name,
    pub typevars: Vec<TypeID>,
}

impl AltInterface {
    /// Applies a type substitution to the constraint.
    pub fn subst(&self, inst: &Instance) -> AltInterface {
        AltInterface {
            interface: self.interface,
            typevars: self.typevars.iter().map(|ty| ty.subst(inst)).collect(),
        }
    }

    /// Is the constraint satisfied in the current environment?
    pub fn satisfied(&self, instance: &Instance, decls: &DeclTable, loc: Loc) -> bool {
        if let Some(Decl::Interface(interface)) = decls.find(self.interface).first() {
            let mut types = vec![];
            for ty in &self.typevars {
                types.push(ty.subst(instance));
            }

            let mut tmp_errors = vec![];
            interface.satisfied(&types, decls, &mut tmp_errors, loc)
        } else {
            // Unknown interface!
            false
        }
    }
}

/// An alternative choice for a type. May also need
/// to satisfy interface constraints.
///
/// Note: hopefully we can get away with this instead of
/// having a hierarchy of constraints like the Swift type checker.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Alt {
    pub ty: TypeID,
    pub interfaces: Vec<AltInterface>,
}

impl Alt {
    /// Replaces named type variables with anonymous type variables,
    /// including in constraints.
    pub fn fresh(&self, next_anon: &mut usize) -> Alt {
        let mut inst = Instance::new();

        let ty = self.ty.fresh_aux(next_anon, &mut inst);
        let interfaces = self
            .interfaces
            .iter()
            .map(|alt_interface| alt_interface.subst(&inst))
            .collect();

        Alt { ty, interfaces }
    }
}

/// A type-inference constraint.
///
/// We're currently only using a list of constraints (no sub-constraints).
/// Hopefully this is adequate, since a constraint hierachy
/// would imply branching/backtracking which can lead to potential
/// exponential runtime behavior. This is why the Swift type checker
/// has a timeout (!!).
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum Constraint {
    /// Equality.
    Equal(TypeID, TypeID, Loc),

    /// Function overloads, enum leading dot syntax.
    Or(TypeID, Vec<Alt>, Loc),

    /// Field access.
    Field(TypeID, Name, TypeID, Loc),
}

impl Constraint {
    /// Have we eliminated all type variables and
    /// resolved disjuctions?
    pub fn solved(&self, inst: &Instance) -> bool {
        match self {
            Constraint::Equal(a, b, _) => a.solved_inst(inst) && b.solved_inst(inst),
            Constraint::Or(t, alts, _) => {
                t.solved_inst(inst) && alts.iter().len() == 1 && alts[0].ty.solved_inst(inst)
            }
            Constraint::Field(struct_ty, _, ft, _) => {
                struct_ty.solved_inst(inst) && ft.solved_inst(inst)
            }
        }
    }

    /// Returns source code location of the constraint.
    pub fn loc(&self) -> Loc {
        match self {
            Constraint::Equal(_, _, loc) => *loc,
            Constraint::Or(_, _, loc) => *loc,
            Constraint::Field(_, _, _, loc) => *loc,
        }
    }

    /// Pretty-prints the constraint.
    pub fn print(&self, inst: &Instance) {
        match self {
            Constraint::Equal(a, b, loc) => println!(
                "Equal({:?}, {:?}, {:?})",
                a.subst(inst),
                b.subst(inst),
                loc
            ),
            Constraint::Or(a, alts, loc) => println!(
                "Or({:?}, {:?}, {:?})",
                a.subst(inst),
                (*alts)
                    .iter()
                    .map(|t| t.ty.subst(inst))
                    .collect::<Vec<TypeID>>(),
                loc
            ),
            Constraint::Field(a, name, b, loc) => println!(
                "Field({:?}, {:?}, {:?}, {:?})",
                a.subst(inst),
                name,
                b.subst(inst),
                loc
            ),
        }
    }
}

pub fn iterate_solver(
    constraints: &mut [Constraint],
    instance: &mut Instance,
    decls: &DeclTable,
    errors: &mut Vec<TypeError>,
) {
    for constraint in constraints {
        match constraint {
            Constraint::Equal(a, b, loc) => {
                if !unify(*a, *b, instance) {
                    errors.push(TypeError {
                        location: *loc,
                        message: format!(
                            "no solution for {:?} == {:?}",
                            a.subst(instance),
                            b.subst(instance)
                        ),
                    });
                }
            }
            Constraint::Or(t, alts, loc) => {
                let alts_clone = alts.clone();

                // Try to narrow it down.
                alts.retain(|alt| {
                    // Start from the instance we know so far.
                    let mut inst = instance.clone();
                    unify(*t, alt.ty, &mut inst)
                });

                // Nothing works!
                if alts.is_empty() {
                    errors.push(TypeError {
                        location: *loc,
                        message: format!(
                            "no solution for {:?} is one of {:?}",
                            t.subst(instance),
                            alts_clone
                        ),
                    });
                }

                // Just a single option. Better unify!
                if alts.len() == 1 {
                    if unify(*t, alts[0].ty, instance) {
                        // Check that all the interfaces are satisfied.
                        for interface_constraint in &alts[0].interfaces {
                            if !interface_constraint.satisfied(instance, decls, *loc) {
                                errors.push(TypeError {
                                    location: *loc,
                                    message: format!("interface constraint {:?} not satisfied", interface_constraint.interface),
                                });
                            }
                        }
                    } else {
                        errors.push(TypeError {
                            location: *loc,
                            message: format!(
                                "no solution for {:?} == {:?}",
                                t.subst(instance),
                                alts[0].ty.subst(instance)
                            ),
                        });
                    }
                }
            }
            Constraint::Field(struct_ty, field_name, ft, loc) => {
                // This starts feeling a bit too nested.

                match &*find(*struct_ty, instance) {
                    Type::Name(struct_name, vars) => {
                        let d = decls.find(*struct_name);

                        if let Some(Decl::Struct(st)) = d.first()
                        {
                            // We've narrowed it down. Better unify!
                            if let Some(field) = find_field(&st.fields, *field_name) {
                                let field_ty = if let Type::Var(name) = *field.ty {
                                    let index = st.typevars.iter().position(|&n| n == name).unwrap();
                                    vars[index]
                                } else {
                                    field.ty
                                };

                                *constraint = Constraint::Equal(field_ty, *ft, *loc);
                            } else {
                                errors.push(TypeError {
                                    location: *loc,
                                    message: format!("no such field: {:?}", field_name),
                                });
                            }
                        } else {
                            errors.push(TypeError {
                                location: *loc,
                                message: format!("{:?} does not refer to a struct", struct_name),
                            });
                        }
                    }
                    Type::Array(_, _) => {
                        if *field_name == Name::new("len".into()) {
                            *constraint = Constraint::Equal(mk_type(Type::Int32), *ft, *loc);
                        } else {
                            errors.push(TypeError {
                                location: *loc,
                                message: format!("array only has len field, not {:?}", field_name),
                            });
                        }
                    }
                    _ => (),
                }
            }
        }
    }
}

fn constraints_hash(constraints: &[Constraint]) -> u64 {
    let mut s = DefaultHasher::new();
    constraints.hash(&mut s);
    s.finish()
}

pub fn print_constraints(constraints: &[Constraint], inst: &Instance) {
    for c in constraints {
        c.print(inst);
    }
}

/// Looks for ambigious constraints and outputs errors.
pub fn solved_constraints(
    constraints: &[Constraint],
    instance: &Instance,
    errors: &mut Vec<TypeError>,
) {
    for c in constraints {
        if !c.solved(instance) {
            errors.push(TypeError {
                location: c.loc(),
                message: format!("ambiguous constraint: {:?}", c),
            });
        }
    }
}

/// Solves a set of type constraints.
///
/// Note that this solver doesn't backtrack, so it doesn't have exponential runtime,
/// but may fail to solve some cases (the compiler will ask for more type annotations).
pub fn solve_constraints(
    constraints: &mut [Constraint],
    instance: &mut Instance,
    decls: &DeclTable,
    errors: &mut Vec<TypeError>,
) {
    //println!("constraints before solve: ");
    //print_constraints(constraints, &Instance::new());

    // Continue as long as we can make changes.
    loop {
        let h = constraints_hash(constraints);
        //println!("---- solve iteration {}", i);
        let old_instance = instance.clone();

        // Errors only matter once we can no longer make progress.
        let mut errors = vec![];
        iterate_solver(constraints, instance, decls, &mut errors);

        if h == constraints_hash(constraints) && *instance == old_instance {
            // No more progress.
            break;
        }
    }

    // Iterate once more to get any errors.
    iterate_solver(constraints, instance, decls, errors);

    //println!("constraints after solve: ");
    //print_constraints(constraints, instance);

    //println!("instance after solve: ");
    //print_instance(instance);

    solved_constraints(constraints, instance, errors);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_solve_1() {
        let t = anon(0);
        let vd = mk_type(Type::Void);
        let mut constraints = [Constraint::Equal(vd, t, test_loc())];
        let mut instance = Instance::new();

        let mut errors = vec![];
        let decls = DeclTable::new(vec![]);
        iterate_solver(&mut constraints, &mut instance, &decls, &mut errors);
        assert!(errors.is_empty());
        assert_eq!(instance[&t], vd);
    }

    #[test]
    pub fn test_solve_2() {
        let int8 = mk_type(Type::Int8);
        let vd = mk_type(Type::Void);
        let mut constraints = [Constraint::Equal(vd, int8, test_loc())];
        let mut instance = Instance::new();

        let mut errors = vec![];
        let decls = DeclTable::new(vec![]);
        iterate_solver(&mut constraints, &mut instance, &decls, &mut errors);
        assert!(!errors.is_empty());
    }

    #[test]
    pub fn test_solve_3() {
        let int8 = mk_type(Type::Int8);
        let t = anon(0);
        let vd = mk_type(Type::Void);
        let mut constraints = [
            Constraint::Equal(vd, t, test_loc()),
            Constraint::Equal(int8, t, test_loc()),
        ];
        let mut instance = Instance::new();

        let mut errors = vec![];
        let decls = DeclTable::new(vec![]);
        iterate_solver(&mut constraints, &mut instance, &decls, &mut errors);
        assert!(!errors.is_empty());
    }

    #[test]
    pub fn test_or_1() {
        let i = mk_type(Type::Int32);
        let f = mk_type(Type::Float32);

        let i_alt = Alt {
            ty: i,
            interfaces: vec![],
        };
        let f_alt = Alt {
            ty: f,
            interfaces: vec![],
        };

        let mut constraints = [Constraint::Or(i, vec![i_alt, f_alt], test_loc())];

        let mut instance = Instance::new();
        let mut errors = vec![];
        let decls = DeclTable::new(vec![]);
        iterate_solver(&mut constraints, &mut instance, &decls, &mut errors);
        assert!(errors.is_empty());
        iterate_solver(&mut constraints, &mut instance, &decls, &mut errors);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_field_2() {
        let i = mk_type(Type::Int32);
        let xname = Name::new("x".into());
        let s0name = Name::new("S0".into());

        let decls = DeclTable::new(vec![Decl::Struct( StructDecl{
            name: s0name,
            fields: vec![Field {
                name: xname,
                ty: i,
                loc: test_loc(),
            }],
            typevars: vec![],
        })]);

        let struct_ty = mk_type(Type::Name(s0name, vec![]));
        let v = anon(0);

        let mut constraints = [Constraint::Field(struct_ty, xname, v, test_loc())];

        let mut instance = Instance::new();
        let mut errors = vec![];
        iterate_solver(&mut constraints, &mut instance, &decls, &mut errors);
        assert!(errors.is_empty());
        iterate_solver(&mut constraints, &mut instance, &decls, &mut errors);
        assert!(errors.is_empty());
    }
}
