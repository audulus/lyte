use crate::defs::*;
use internment::Intern;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    Bool,
    Int8,
    UInt8,
    Int32,
    Float32,
    Tuple(Vec<TypeID>),
    Var(Name),
    Anon(usize),
    Func(TypeID, TypeID),
    Array(TypeID, i64),
    Name(Name, Vec<TypeID>),
}

/// An interned type.
/// 
/// Newtyped so we can implement more traits.
#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub struct TypeID(Intern<Type>);

impl TypeID {
    pub fn new(ty: Type) -> Self {
        Self(Intern::new(ty))
    }
}

impl Deref for TypeID {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl fmt::Debug for TypeID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", *self.0)
    }
}

/// A substitution from type variables to
/// other types.
/// 
/// "Substitution" might be better terminology.
pub type Instance = HashMap<TypeID, TypeID>;

pub fn print_instance(inst: &Instance) {
    for (k, v) in inst {
        println!("  {:?} ➡️ {:?}", k, v);
    }
}

/// Convenience to create new TypeIDs from types.
/// 
/// Kinda questionable wrapper over TypeID::new.
pub fn mk_type(proto: Type) -> TypeID {
    TypeID::new(proto)
}

/// Create a new named type variable.
pub fn typevar(name: &str) -> TypeID {
    mk_type(Type::Var(Name::new(String::from(name))))
}

/// Create a new anonymous type variable with an index.
/// 
/// Should we also include a location?
pub fn anon(index: usize) -> TypeID {
    mk_type(Type::Anon(index))
}

/// Convenience to create a function type.
pub fn func(dom: TypeID, range: TypeID) -> TypeID {
    mk_type(Type::Func(dom, range))
}

/// Convenience to create a tuple type.
pub fn tuple(types: Vec<TypeID>) -> TypeID {
    mk_type(Type::Tuple(types))
}

/// Convenience to create a tuple type from parameters.
fn params_ty(params: &Vec<Param>) -> TypeID {
    tuple(params.into_iter().map(|p| p.ty.unwrap()).collect())
}

/// Look up a type in an instance, following as far as possible.
/// 
/// There better not be cycles!
pub fn find(id: TypeID, inst: &Instance) -> TypeID {
    let mut id = id;
    while let Some(t) = inst.get(&id) {
        id = *t;
    }
    id
}

/// Apply type variable substitutions to a type.
pub fn subst(t: TypeID, inst: &Instance) -> TypeID {
    //println!("subst {:?}", t);
    let t = find(t, inst);
    //println!("maps to {:?}", t);

    match &*t {
        Type::Tuple(v) => mk_type(Type::Tuple(v.iter().map(|t| subst(*t, inst)).collect())),
        Type::Func(a, b) => mk_type(Type::Func(subst(*a, inst), subst(*b, inst))),
        Type::Array(a, n) => mk_type(Type::Array(subst(*a, inst), *n)),
        Type::Anon(_) => find(t, inst),
        _ => t,
    }
}

/// Does the type contain any anonymous type variables?
pub fn solved(t: TypeID) -> bool {
    match &*t {
        Type::Tuple(v) => v.iter().all(|t| solved(*t)),
        Type::Func(a, b) => solved(*a) && solved(*b),
        Type::Array(a, _) => solved(*a),
        Type::Var(_) => true,
        Type::Anon(_) => false,
        _ => true,
    }
}

/// Is the type solved when substitutions are applied from an instance?
pub fn solved_inst(t: TypeID, inst: &Instance) -> bool {
    let tt = find(t, inst);
    match &*tt {
        Type::Tuple(v) => v.iter().all(|t| solved_inst(*t, inst)),
        Type::Func(a, b) => solved_inst(*a, inst) && solved_inst(*b, inst),
        Type::Array(a, _) => solved_inst(*a, inst),
        Type::Var(_) => true,
        Type::Anon(_) => false,
        _ => true,
    }
}

/// Find a substitution which makes two types equal.
/// 
/// Returns false if such a substituion doesn't exist.
/// I think this is pretty much textbook Hindley-Milner.
/// It's more or less what's in the dragon book.
pub fn unify(lhs: TypeID, rhs: TypeID, inst: &mut Instance) -> bool {
    let lhs = find(lhs, inst);
    let rhs = find(rhs, inst);

    if lhs == rhs {
        true
    } else {
        match (&*lhs, &*rhs) {
            (Type::Anon(_), _) => {
                inst.insert(lhs, rhs);
                true
            }
            (_, Type::Anon(_)) => {
                inst.insert(rhs, lhs);
                true
            }
            (Type::Tuple(v0), Type::Tuple(v1)) => {
                if v0.len() == v1.len() {
                    for i in 0..v0.len() {
                        if !unify(v0[i], v1[i], inst) {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            }
            (Type::Name(a, a_params), Type::Name(b, b_params)) => {
                if a == b && a_params.len() == b_params.len() {
                    for i in 0..a_params.len() {
                        if !unify(a_params[i], b_params[i], inst) {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            }
            (Type::Array(a, _), Type::Array(b, _)) => unify(*a, *b, inst),
            (Type::Func(a, b), Type::Func(c, d)) => unify(*a, *c, inst) && unify(*b, *d, inst),
            _ => false,
        }
    }
}

fn find_decls_unify(decls: &Vec<Decl>, name: Name, f: impl Fn(&Decl), t: TypeID) {
    for d in decls {
        let mut inst = Instance::new();
        if d.name() == name && unify(d.ty(), t, &mut inst) {
            f(d)
        }
    }
}

fn fresh_aux(ty: TypeID, index: &mut usize, inst: &mut Instance) -> TypeID {
    match &*ty {
        Type::Tuple(v) => {
            let vv = v.iter().map(|t| fresh_aux(*t, index, inst)).collect();
            mk_type(Type::Tuple(vv))
        }
        Type::Array(a, sz) => mk_type(Type::Array(fresh_aux(*a, index, inst), *sz)),
        Type::Var(_) => *inst.entry(ty).or_insert_with(|| {
            let t = mk_type(Type::Anon(*index));
            *index += 1;
            t
        }),
        Type::Func(dom, rng) => mk_type(Type::Func(
            fresh_aux(*dom, index, inst),
            fresh_aux(*rng, index, inst),
        )),
        _ => ty,
    }
}

/// Replaces named type variables with anonymous type variables.
pub fn fresh(ty: TypeID, index: &mut usize) -> TypeID {
    let mut inst = Instance::new();
    fresh_aux(ty, index, &mut inst)
}

impl Decl {
    pub fn ty(&self) -> TypeID {
        match self {
            Decl::Interface { .. } => mk_type(Type::Void),
            Decl::Func(FuncDecl { params, ret, .. }) => func(params_ty(params), *ret),
            Decl::Macro(FuncDecl { params, ret, .. }) => func(params_ty(params), *ret),
            Decl::Struct { name, .. } => mk_type(Type::Name(*name, vec![])),
            Decl::Enum { name, .. } => mk_type(Type::Name(*name, vec![])),
            Decl::Global { ty, .. } => *ty,
        }
    }
}

/// Calls a function for each type variable in the type.
pub fn typevars(ty: TypeID, f: &mut impl FnMut(Name)) {
    match &*ty {
        Type::Tuple(v) => {
            v.iter().for_each(|t| typevars(*t, f));
        }
        Type::Array(a, _sz) => typevars(*a, f),
        Type::Func(dom, rng) => {
            typevars(*dom, f);
            typevars(*rng, f);
        }
        Type::Var(name) => (*f)(*name),
        _ => (),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mk_type() {
        let v0 = mk_type(Type::Void);
        let v1 = mk_type(Type::Void);
        assert_eq!(v0, v1);

        let i0 = mk_type(Type::Int8);
        assert_ne!(v0, i0);
    }

    #[test]
    fn test_unify() {
        let vd = mk_type(Type::Void);
        let int8 = mk_type(Type::Int8);
        let mut inst = Instance::new();
        assert!(unify(vd, vd, &mut inst));
        assert!(!unify(vd, int8, &mut inst));
        assert!(solved(vd));

        let var = anon(0);
        assert!(unify(var, int8, &mut inst));
        assert!(!solved(var));

        let var2 = anon(1);
        assert!(unify(var, var2, &mut inst));

        {
            let mut inst = Instance::new();
            assert!(unify(
                mk_type(Type::Array(var, 0)),
                mk_type(Type::Array(var2, 0)),
                &mut inst
            ));
        }

        {
            let mut inst = Instance::new();
            let b = mk_type(Type::Bool);
            let tup = mk_type(Type::Tuple(vec![b]));
            assert!(unify(var, func(tup, vd), &mut inst));
        }
    }

    #[test]
    fn test_typevars() {

        let vd = mk_type(Type::Void);
        typevars(vd, &mut|_name| ());

        let v = typevar("T");
        typevars(v, &mut|name| ( assert_eq!(name, Name::new("T".into()))));

    }
}
