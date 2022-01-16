use crate::defs::*;
use std::collections::HashMap;

pub type Instance = HashMap<String, TypeID>;

use internment::Intern;

pub fn mk_type(proto: Type) -> TypeID {
    Intern::new(proto)
}

pub fn typevar(name: &str) -> TypeID {
    mk_type(Type::Var(Intern::from(&String::from(name))))
}

pub fn subst(t: TypeID, inst: &Instance) -> TypeID {
    match *t {
        Type::Tuple(a, b) => {
            let nt = Type::Tuple(subst(a, inst), subst(b, inst));
            mk_type(nt)
        }
        Type::Var(i) => match inst.get(i.as_ref()) {
            Some(t0) => *t0,
            None => t,
        },
        _ => t,
    }
}

pub fn solved(t: TypeID) -> bool {
    match *t {
        Type::Tuple(a, b) => solved(a) && solved(b),
        Type::Func(a, b) => solved(a) && solved(b),
        Type::Var(_) => false,
        _ => true,
    }
}

pub fn unify(lhs: TypeID, rhs: TypeID, inst: &mut Instance) -> bool {
    if lhs == rhs {
        true
    } else {
        match (*lhs, *rhs) {
            (Type::Tuple(a, b), Type::Tuple(c, d)) => unify(a, c, inst) && unify(b, d, inst),
            (Type::Func(a, b), Type::Func(c, d)) => unify(a, c, inst) && unify(b, d, inst),
            (Type::Var(i), _) => {
                inst.insert(i.as_ref().clone(), rhs);
                true
            }
            (_, Type::Var(i)) => {
                inst.insert(i.as_ref().clone(), lhs);
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

        let var = typevar("T");
        assert!(unify(var, int8, &mut inst));

        match inst.get("T") {
            Some(t) => assert_eq!(*t, int8),
            None => assert!(false),
        }
    }
}
