use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub struct TypeID {
    index: u32
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    Int8,
    Int32,
    Tuple(TypeID, TypeID),
    Var(u32),
    Func(TypeID, TypeID),
}

const void:TypeID = TypeID{index: 0};
const int8:TypeID = TypeID{index: 1};
const int32:TypeID = TypeID{index: 2};

pub struct Compiler {
    types: Vec<Type>,
}

pub type Instance = HashMap<u32, TypeID>;

impl Compiler {
    pub fn new() -> Compiler {
        Compiler { types: vec!(Type::Void, Type::Int8, Type::Int32) }
    }

    pub fn mk_type(&mut self, proto: Type) -> TypeID {
        // Dumb linear search.
        for i in 0..self.types.len() {
            if self.types[i] == proto {
                return TypeID{index: i as u32};
            }
        }

        let ix = self.types.len();
        self.types.push(proto);
        return TypeID{index: ix as u32};
    }

    pub fn subst(&mut self, t: TypeID, inst: &Instance) -> TypeID {
        match self.types[t.index as usize] {
            Type::Tuple(a, b) => {
                let nt = Type::Tuple(self.subst(a, inst), self.subst(b, inst));
                self.mk_type(nt)
            }
            Type::Var(i) => match inst.get(&i) {
                Some(t0) => *t0,
                None => t,
            },
            _ => t,
        }
    }

    pub fn solved(&self, t: TypeID) -> bool {
        match self.types[t.index as usize] {
            Type::Tuple(a, b) => self.solved(a) && self.solved(b),
            Type::Func(a, b) => self.solved(a) && self.solved(b),
            Type::Var(_) => false,
            _ => true,
        }
    }

    pub fn unify(&self, lhs: TypeID, rhs: TypeID, inst: &mut Instance) -> bool {
        if lhs == rhs {
            true
        } else {
            match (self.types[lhs.index as usize], self.types[rhs.index as usize]) {
                (Type::Tuple(a, b), Type::Tuple(c, d)) => {
                    self.unify(a, c, inst) && self.unify(b, d, inst)
                }
                (Type::Func(a, b), Type::Func(c, d)) => self.unify(a, c, inst) && self.unify(b, d, inst),
                (Type::Var(i), _) => {
                    inst.insert(i, rhs);
                    true
                }
                (_, Type::Var(i)) => {
                    inst.insert(i, lhs);
                    true
                }
                _ => false,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mk_type() {
        let mut compiler = Compiler::new();
        let v0 = compiler.mk_type(Type::Void);
        let v1 = compiler.mk_type(Type::Void);
        assert_eq!(v0, v1);

        let i0 = compiler.mk_type(Type::Int8);
        assert_ne!(v0, i0);
    }

    fn test_unify() {
        let mut inst = Instance::new();
        let mut compiler = Compiler::new();
        assert!(compiler.unify(void, void, &mut inst));
        assert!(!compiler.unify(void, int8, &mut inst));

        let var = compiler.mk_type(Type::Var(0));
        assert!(compiler.unify(var, int8, &mut inst));

        match inst.get(&0) {
            Some(t) => assert_eq!(*t, int8),
            None => assert!(false),
        }
    }
}

/*
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
