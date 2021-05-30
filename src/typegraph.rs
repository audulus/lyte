use crate::types::*;
use crate::ast::*;

type TypeNodeID = u32;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
struct Constraint {
    pub a: TypeNodeID,
    pub b: TypeNodeID,
    pub field: String,
    pub loc: Loc,
}

/*
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