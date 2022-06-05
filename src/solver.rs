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
    pub typevars: Vec<TypeID>
}

impl AltInterface {

    /// Applies a type substitution to the constraint.
    pub fn subst(&self, inst: &Instance) -> AltInterface {
        AltInterface {
            interface: self.interface,
            typevars: self.typevars.iter().map(|ty| ty.subst(inst)).collect()
        }
    }

    pub fn satisfied(&self, instance: &Instance, decls: &SortedDecls, loc: Loc) -> bool {
        if let Some(Decl::Interface(interface)) = decls.find(self.interface).first() {

            println!("Found interface");

            let mut types = vec![];
            for ty in &self.typevars {
                types.push(subst(*ty, instance));
            }

            println!("types: {:?}", types);

            let mut tmp_errors = vec![];
            return interface.satisfied(&types, decls, &mut tmp_errors, loc);
        } else {
            // Unknown interface!
            return false;
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
    pub interfaces: Vec<AltInterface>
}

impl Alt {

    /// Replaces named type variables with anonymous type variables,
    /// including in constraints.
    pub fn fresh(&self, next_anon: &mut usize) -> Alt {
        let mut inst = Instance::new();

        let ty = fresh_aux(self.ty, next_anon, &mut inst);
        let interfaces = self.interfaces.iter().map(|alt_interface| {
            alt_interface.subst(&inst)
        }).collect();

        Alt{ ty, interfaces }
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
            Constraint::Equal(a, b, _) => solved_inst(*a, inst) && solved_inst(*b, inst),
            Constraint::Or(_, _, _) => false,
            Constraint::Field(struct_ty, _, ft, _) => {
                solved_inst(*struct_ty, inst) && solved_inst(*ft, inst)
            }
        }
    }

    pub fn loc(&self) -> Loc {
        match self {
            Constraint::Equal(_, _, loc) => *loc,
            Constraint::Or(_, _, loc) => *loc,
            Constraint::Field(_, _, _, loc) => *loc,
        }
    }

    pub fn print(&self, inst: &Instance) {
        match self {
            Constraint::Equal(a, b, loc) => println!(
                "Equal({:?}, {:?}, {:?})",
                subst(*a, inst),
                subst(*b, inst),
                loc
            ),
            Constraint::Or(a, alts, loc) => println!(
                "Or({:?}, {:?}, {:?})",
                subst(*a, inst),
                (*alts)
                    .iter()
                    .map(|t| subst(t.ty, inst))
                    .collect::<Vec<TypeID>>(),
                loc
            ),
            Constraint::Field(a, name, b, loc) => println!(
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
    constraints: &mut [Constraint],
    instance: &mut Instance,
    decls: &SortedDecls,
    errors: &mut Vec<TypeError>,
) {
    for constraint in constraints {

        match constraint {
            Constraint::Equal(a, b, loc) => {
                if !unify(*a, *b, instance) {
                    errors.push(TypeError {
                        location: *loc,
                        message: format!("no solution for {:?} == {:?}", subst(*a, instance), subst(*b, instance)).into(),
                    });
                }
            }
            Constraint::Or(t, alts, loc) => {

                let alts_clone = alts.clone();

                // Try to narrow it down.
                alts.retain(|alt| {

                    // println!("Processing alt {:?}", alt);

                    // Throw out alternatives where the interfaces aren't satisfied.
                    let mut tmp_errors = vec![];
                    for interface_constraint in &alt.interfaces {
                        if let Some(Decl::Interface(interface)) = decls.find(interface_constraint.interface).first() {

                            //println!("Found interface");

                            let mut types = vec![];
                            for ty in &interface_constraint.typevars {
                                types.push(subst(*ty, instance));
                            }

                            //println!("types: {:?}", types);

                            if !interface.satisfied(&types, decls, &mut tmp_errors, *loc) {
                                return false;
                            }
                        } else {
                            // Unknown interface!
                            return false;
                        }
                    }

                    // Start from the instance we know so far.
                    let mut inst = instance.clone();
                    unify(*t, alt.ty, &mut inst)
                });

                // Nothing works!
                if alts.is_empty() {
                    errors.push(TypeError {
                        location: *loc,
                        message: format!("no solution for {:?} is one of {:?}", subst(*t, instance), alts_clone).into(),
                    });
                }

                // Just a single option. Better unify!
                if alts.len() == 1 {
                    *constraint = Constraint::Equal(*t, alts[0].ty, *loc);
                }
            }
            Constraint::Field(struct_ty, field_name, ft, loc) => {
                // This starts feeling a bit too nested.

                match &*find(*struct_ty, instance) {
                    Type::Name(struct_name, vars) => {

                        let d = decls.find(*struct_name);

                        if let Some(Decl::Struct {
                            typevars, fields, ..
                        }) = d.first()
                        {
                            // We've narrowed it down. Better unify!
                            if let Some(field) = find_field(fields, *field_name) {
                                let field_ty = if let Type::Var(name) = *field.ty {
                                    let index = typevars.iter().position(|&n| n == name).unwrap();
                                    vars[index]
                                } else {
                                    field.ty
                                };

                                *constraint = Constraint::Equal(field_ty, *ft, *loc);
                            } else {
                                errors.push(TypeError {
                                    location: *loc,
                                    message: format!("no such field: {:?}", field_name).into(),
                                });
                            }
                        } else {
                            errors.push(TypeError {
                                location: *loc,
                                message: format!("{:?} does not refer to a struct", struct_name)
                                    .into(),
                            });
                        }
                        
                    }
                    Type::Array(_, _) => {
                        if *field_name == Name::new("len".into()) {
                            *constraint = Constraint::Equal(mk_type(Type::Int32), *ft, *loc);
                        } else {
                            errors.push(TypeError {
                                location: *loc,
                                message: format!("array only has len field, not {:?}", field_name)
                                    .into(),
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
                message: format!("ambiguous constraint: {:?}", c).into(),
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
    decls: &SortedDecls,
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
        let decls = SortedDecls::new(vec![]);
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
        let decls = SortedDecls::new(vec![]);
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
        let decls = SortedDecls::new(vec![]);
        iterate_solver(&mut constraints, &mut instance, &decls, &mut errors);
        assert!(!errors.is_empty());
    }

    #[test]
    pub fn test_or_1() {
        let i = mk_type(Type::Int32);
        let f = mk_type(Type::Float32);

        let i_alt = Alt{ty: i, interfaces: vec![]};
        let f_alt = Alt{ty: f, interfaces: vec![]};

        let mut constraints = [Constraint::Or(i, vec![i_alt, f_alt], test_loc())];

        let mut instance = Instance::new();
        let mut errors = vec![];
        let decls = SortedDecls::new(vec![]);
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

        let decls = SortedDecls::new(vec![Decl::Struct {
            name: s0name,
            fields: vec![Field {
                name: xname,
                ty: i,
                loc: test_loc(),
            }],
            typevars: vec![],
        }]);

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
