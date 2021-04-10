#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    Int8,
    Int32,
    Tuple(Box<Type>, Box<Type>),
    Var(usize),
    Func(Box<Type>, Box<Type>),
}

pub type Instance = Vec<Type>;

pub fn subst(t: &Type, inst: &Instance) -> Type {
    match t {
        Type::Tuple(a, b) => Type::Tuple(Box::new(subst(a, inst)), Box::new(subst(b, inst))),
        Type::Var(i) => inst[*i].clone(),
        _ => t.clone(),
    }
}

pub fn unify(lhs: &Type, rhs: &Type, inst: &mut Instance) -> bool {
    if lhs == rhs {
        true
    } else {
        match (lhs, rhs) {
            (Type::Tuple(a, b), Type::Tuple(c, d)) => {
				unify(a, c, inst) && unify(b, d, inst)
			}
            (Type::Func(a, b), Type::Func(c, d)) => {
                unify(a, c, inst) && unify(b, d, inst)
            }
            (Type::Var(i), rhs) => {
                inst[*i] = rhs.clone();
                true
            }
            _ => return false,
        }
    }
}

pub fn solved(t: &Type) -> bool {
    match t {
        Type::Tuple(a, b) => solved(a) && solved(b),
        Type::Func(a, b) => solved(a) && solved(b),
        Type::Var(_) => false,
        _ => true,
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
        assert_eq!(
            Type::Tuple(Box::new(Type::Void), Box::new(Type::Void)),
            Type::Tuple(Box::new(Type::Void), Box::new(Type::Void))
        );
    }

    #[test]
    fn test_unify() {
        let mut inst = vec![Type::Void];
        assert!(unify(&Type::Void, &Type::Void, &mut inst));
        assert!(!unify(&Type::Void, &Type::Int8, &mut inst));

        assert!(unify(&Type::Var(0), &Type::Int8, &mut inst));
        assert_eq!(inst[0], Type::Int8);
    }

	#[test]
	fn test_solved() {
		assert!(solved(&Type::Void));
		assert!(!solved(&Type::Var(0)));
	}
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
struct Loc {
    pub file: String,
    pub line: u32,
}

type TypeNodeID = u32;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
struct Constraint {
    pub a: TypeNodeID,
    pub b: TypeNodeID,
    pub field: String,
    pub loc: Loc,
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct TypeNode {
    pub possible: Vec<Type>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct TypeGraph {
    pub nodes: Vec<TypeNode>,
    pub constraints: Vec<Constraint>,
    pub inst: Instance,
}

impl TypeGraph {
    pub fn subst(&mut self) {
        for n in &mut self.nodes {
            for t in &mut n.possible {
                *t = subst(t, &self.inst);
            }
        }
    }

    pub fn solved(&self) -> bool {
        for n in &self.nodes {
            if n.possible.len() != 1 || !solved(&n.possible[0]) {
                return false;
            }
        }
        return true;
    }
}
