use crate::defs::*;
use std::collections::HashMap;

pub type Instance = HashMap<TypeID, TypeID>;

use internment::Intern;

pub fn mk_type(proto: Type) -> TypeID {
    Intern::new(proto)
}

pub fn typevar(name: &str) -> TypeID {
    mk_type(Type::Var(Intern::from(&String::from(name)), 0))
}

pub fn func(dom: TypeID, range: TypeID) -> TypeID {
    mk_type(Type::Func(dom, range))
}

pub fn tuple(a: TypeID, b: TypeID) -> TypeID {
    mk_type(Type::Tuple(a,b))
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

    match *t {
        Type::Tuple(a, b) => mk_type(Type::Tuple(subst(a, inst), subst(b, inst))),
        Type::Func(a, b) => mk_type(Type::Func(subst(a, inst), subst(b, inst))),
        Type::Array(a, n) => mk_type(Type::Array(subst(a, inst), n)),
        Type::Var(_, _) => find(t, inst),
        _ => t,
    }
}

pub fn solved(t: TypeID) -> bool {
    match *t {
        Type::Tuple(a, b) => solved(a) && solved(b),
        Type::Func(a, b) => solved(a) && solved(b),
        Type::Array(a, _) => solved(a),
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
            (Type::Tuple(a, b), Type::Tuple(c, d)) => unify(*a, *c, inst) && unify(*b, *d, inst),
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

    }
}
