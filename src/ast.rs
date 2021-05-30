use std::collections::HashMap;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    Int8,
    Int32,
    Tuple(Box<Type>, Box<Type>),
    Var(i32),
    Func(Box<Type>, Box<Type>),
}

pub type Instance = HashMap<i32, Type>;

pub fn subst(t: &Type, inst: &Instance) -> Type {
    match t {
        Type::Tuple(a, b) => Type::Tuple(Box::new(subst(a, inst)), Box::new(subst(b, inst))),
        Type::Var(i) => match inst.get(i) {
            Some(t0) => t0.clone(),
            None => t.clone(),
        },
        _ => t.clone(),
    }
}

pub fn unify(lhs: &Type, rhs: &Type, inst: &mut Instance) -> bool {
    if lhs == rhs {
        true
    } else {
        match (lhs, rhs) {
            (Type::Tuple(a, b), Type::Tuple(c, d)) => unify(a, c, inst) && unify(b, d, inst),
            (Type::Func(a, b), Type::Func(c, d)) => unify(a, c, inst) && unify(b, d, inst),
            (Type::Var(i), rhs) => {
                inst.insert(*i, rhs.clone());
                true
            }
            (lhs, Type::Var(i)) => {
                inst.insert(*i, lhs.clone());
                true
            }
            _ => false,
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
        let mut inst = Instance::new();
        assert!(unify(&Type::Void, &Type::Void, &mut inst));
        assert!(!unify(&Type::Void, &Type::Int8, &mut inst));

        assert!(unify(&Type::Var(0), &Type::Int8, &mut inst));

        match inst.get(&0) {
            Some(t) => assert_eq!(*t, Type::Int8),
            None => assert!(false),
        }
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

    pub fn propagate_eq(&mut self, a: &mut TypeNode, b: &mut TypeNode) -> Result<(), String> {

        // If each node has one possible type, they better unify.
        if a.possible.len() == 1 && b.possible.len() == 1 {
            if unify(&a.possible[0], &b.possible[0], &mut self.inst) {

                // We've narrowed down overloads and unified
                // so this substituion applies to the whole graph.
                self.subst();
            } else {
                return Err("type error".to_string());
            }
        }

        if a.possible.len() == 1 {
            b.possible = prune(&b.possible, &a.possible[0]);
            if b.possible.len() == 0 {
                return Err("type error".to_string());
            }
        }

        if b.possible.len() == 1 {
            a.possible = prune(&a.possible, &b.possible[0]);
            if a.possible.len() == 0 {
                return Err("type error".to_string());
            }
        }

        return Ok(());

    }
}

fn prune(v: &Vec<Type>, t0: &Type) -> Vec<Type> {
    let mut result = Vec::new();

    for t in v {
        let mut inst = Instance::new();
        if unify(t, t0, &mut inst) {
            result.push(t.clone());
        }
    }

    return result;
}
