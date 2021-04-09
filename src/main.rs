
// use std::rc::Rc;
// use std::collections::HashSet;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum Type {
	Void,
	Int8,
	Int32,
	Tuple(Box<Type>, Box<Type>),
    Var(usize),
    Function(Box<Type>, Box<Type>)
}

pub type Instance = Vec<Type>;

pub fn subst(t: Type, inst: &Instance) -> Type {
    match t {
        Type::Tuple(a, b) => Type::Tuple(Box::new(subst(*a, inst)), Box::new(subst(*b, inst))),
        Type::Var(i) => inst[i].clone(),
        _ => t
    }
}

pub fn unify(lhs: Type, rhs: Type, inst: &mut Instance) -> bool {
    if lhs == rhs {
        true
    } else {
        match (lhs, rhs) {
            (Type::Tuple(a, b), Type::Tuple(c, d)) => unify(*a, *c, inst) && unify(*b, *d, inst),
            (Type::Function(a, b), Type::Function(c, d)) => unify(*a, *c, inst) && unify(*b, *d, inst),
            (Type::Var(i), rhs) => { inst[i] = rhs; true },
            _ => return false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_types() {
        assert_eq!(Type::Void, Type::Void);
        assert_ne!(Type::Void, Type::Int8);
    }

    #[test]
    fn test_tuple() {
        assert_eq!(Type::Tuple(Box::new(Type::Void), Box::new(Type::Void)), Type::Tuple(Box::new(Type::Void), Box::new(Type::Void)));
    }

    #[test]
    fn test_unify() {
        let mut inst = vec![Type::Void];
        assert!(unify(Type::Void, Type::Void, &mut inst));
        assert!(!unify(Type::Void, Type::Int8, &mut inst));
    
        assert!(unify(Type::Var(0), Type::Int8, &mut inst));
        assert_eq!(inst[0], Type::Int8);
    }
}

fn main() {
    println!("yo")
}
