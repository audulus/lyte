use crate::defs::*;
use std::collections::HashMap;
use internment::Intern;
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
    Var(Name, usize),
    Func(TypeID, TypeID),
    Array(TypeID, i64),
    Name(Name, Vec<TypeID>),
}

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

pub type Instance = HashMap<TypeID, TypeID>;

pub fn mk_type(proto: Type) -> TypeID {
    TypeID::new(proto)
}

pub fn typevar(name: &str) -> TypeID {
    mk_type(Type::Var(Name::new(String::from(name)), 0))
}

pub fn func(dom: TypeID, range: TypeID) -> TypeID {
    mk_type(Type::Func(dom, range))
}

pub fn tuple(types: Vec<TypeID>) -> TypeID {
    mk_type(Type::Tuple(types))
}

fn params_ty(params: &Vec<Param>) -> TypeID {
    tuple(params.into_iter().map(|p| p.ty).collect())
}

pub fn find(id: TypeID, inst: &Instance) -> TypeID {
    let mut id = id;
    while let Some(t) = inst.get(&id) {
        id = *t;
    }
    id
}

pub fn subst(t: TypeID, inst: &Instance) -> TypeID {
    let t = find(t, inst);

    match &*t {
        Type::Tuple(v) => mk_type(Type::Tuple(v.iter().map(|t| subst(*t, inst)).collect())),
        Type::Func(a, b) => mk_type(Type::Func(subst(*a, inst), subst(*b, inst))),
        Type::Array(a, n) => mk_type(Type::Array(subst(*a, inst), *n)),
        Type::Var(_, _) => find(t, inst),
        _ => t,
    }
}

pub fn solved(t: TypeID) -> bool {
    match &*t {
        Type::Tuple(v) => v.iter().all(|t| solved(*t)),
        Type::Func(a, b) => solved(*a) && solved(*b),
        Type::Array(a, _) => solved(*a),
        Type::Var(_, _) => false,
        _ => true,
    }
}

pub fn unify(lhs: TypeID, rhs: TypeID, inst: &mut Instance) -> bool {
    let lhs = find(lhs, inst);
    let rhs = find(rhs, inst);

    if lhs == rhs {
        true
    } else {
        match (&*lhs, &*rhs) {
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
            (Type::Array(a, _), Type::Array(b, _)) => unify(*a, *b, inst),
            (Type::Func(a, b), Type::Func(c, d)) => unify(*a, *c, inst) && unify(*b, *d, inst),
            (Type::Var(_, _), _) => {
                inst.insert(lhs, rhs);
                true
            }
            (_, Type::Var(_, _)) => {
                inst.insert(rhs, lhs);
                true
            }
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
        Type::Array(a, sz) => {
            mk_type(Type::Array(fresh_aux(*a, index, inst), *sz))
        }
        Type::Var(name, _) => {
            *inst.entry(ty)
                .or_insert_with(|| {
                    *index += 1;
                    mk_type(Type::Var(*name, *index))
                })
        }
        Type::Func(dom, rng) => {
            mk_type(Type::Func(fresh_aux(*dom, index, inst), fresh_aux(*rng, index, inst)))
        }
        _ => ty
    }
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

        let var = typevar("T");
        assert!(unify(var, int8, &mut inst));
        assert!(!solved(var));

        let var2 = typevar("S");
        assert!(unify(var, var2, &mut inst));

        {
            let var = typevar("T");
            let var2 = typevar("S");
            let mut inst = Instance::new();
            assert!(unify(mk_type(Type::Array(var, 0)), mk_type(Type::Array(var2, 0)), &mut inst));
        }

        {
            let mut inst = Instance::new();
            let b = mk_type(Type::Bool);
            let tup = mk_type(Type::Tuple(vec![b]));
            assert!(unify(var, func(tup, vd), &mut inst));
        }
    }
}
