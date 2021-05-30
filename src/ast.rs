use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    Int8,
    Int32,
    Tuple(Rc<Type>, Rc<Type>),
    Var(i32),
    Func(Rc<Type>, Rc<Type>),
}

pub struct Compiler {
    types: HashMap<u64, Rc<Type>>,
}

impl Type {
    pub fn solved(&self) -> bool {
        match self {
            Type::Tuple(a, b) => a.solved() && b.solved(),
            Type::Func(a, b) => a.solved() && b.solved(),
            Type::Var(_) => false,
            _ => true,
        }
    }
}

pub type Instance = HashMap<i32, Rc<Type>>;

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            types: HashMap::new(),
        }
    }

    pub fn mk_type(&mut self, proto: &Type) -> Rc<Type> {
        let mut s = DefaultHasher::new();
        proto.hash(&mut s);
        let hash = s.finish();
        match self.types.get(&hash) {
            Some(t) => t.clone(),
            None => {
                let t = Rc::new(proto.clone());
                self.types.insert(hash, t.clone());
                t
            }
        }
    }

    pub fn subst(&mut self, t: Rc<Type>, inst: &Instance) -> Rc<Type> {
        match &*t {
            Type::Tuple(a, b) => {
                let nt = Type::Tuple(self.subst(a.clone(), inst), self.subst(b.clone(), inst));
                self.mk_type(&nt)
            }
            Type::Var(i) => match inst.get(&i) {
                Some(t0) => t0.clone(),
                None => t,
            },
            _ => t,
        }
    }
}

pub fn unify(lhs: &Rc<Type>, rhs: &Rc<Type>, inst: &mut Instance) -> bool {
    if lhs == rhs {
        true
    } else {
        match (&**lhs, &**rhs) {
            (Type::Tuple(a, b), Type::Tuple(c, d)) => unify(&a, &c, inst) && unify(&b, &d, inst),
            (Type::Func(a, b), Type::Func(c, d)) => unify(&a, &c, inst) && unify(&b, &d, inst),
            (Type::Var(i), _) => {
                inst.insert(*i, rhs.clone());
                true
            }
            (_, Type::Var(i)) => {
                inst.insert(*i, lhs.clone());
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
        let mut compiler = Compiler::new();
        let v0 = compiler.mk_type(&Type::Void);
        let v1 = compiler.mk_type(&Type::Void);
        assert_eq!(v0, v1);

        let i0 = compiler.mk_type(&Type::Int8);
        assert_ne!(v0, i0);
    }
}

/*
pub type Instance = HashMap<i32, Type>;

impl Type {
    pub fn subst(&self, inst: &Instance) -> Self {
        match self {
            Type::Tuple(a, b) => Type::Tuple(Box::new(a.subst(inst)), Box::new(b.subst(inst))),
            Type::Var(i) => match inst.get(i) {
                Some(t0) => t0.clone(),
                None => self.clone(),
            },
            _ => self.clone(),
        }
    }

    pub fn solved(&self) -> bool {
        match self {
            Type::Tuple(a, b) => a.solved() && b.solved(),
            Type::Func(a, b) => a.solved() && b.solved(),
            Type::Var(_) => false,
            _ => true,
        }
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
        assert!((&Type::Void).solved());
        assert!(!(&Type::Var(0)).solved());
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
                *t = t.subst(&self.inst);
            }
        }
    }

    pub fn solved(&self) -> bool {
        for n in &self.nodes {
            if n.possible.len() != 1 || !(&n.possible[0]).solved() {
                return false;
            }
        }
        return true;
    }

    pub fn add_constraint(&mut self, c: &Constraint) {
        self.constraints.push(c.clone())
    }

    pub fn eq(&mut self, t0: TypeNodeID, t1: TypeNodeID, loc: &Loc) {
        self.add_constraint(&Constraint {
            a: t0,
            b: t1,
            field: "".to_string(),
            loc: loc.clone(),
        })
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
*/
