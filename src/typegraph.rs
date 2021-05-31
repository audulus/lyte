use crate::ast::*;
use crate::types::*;

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
    pub possible: Vec<TypeID>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct TypeGraph {
    pub nodes: Vec<TypeNode>,
    pub constraints: Vec<Constraint>,
    pub inst: Instance,
}

impl TypeGraph {
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
}

impl Compiler {
    pub fn subst_graph(&mut self, g: &mut TypeGraph) {
        for n in &mut g.nodes {
            for t in &mut n.possible {
                *t = self.subst(*t, &g.inst);
            }
        }
    }

    pub fn solved_graph(&self, g: &mut TypeGraph) -> bool {
        for n in &g.nodes {
            if n.possible.len() != 1 || !self.solved(n.possible[0]) {
                return false;
            }
        }
        return true;
    }

    fn prune(&mut self, v: &Vec<TypeID>, t0: TypeID) -> Vec<TypeID> {
        let mut result = Vec::new();

        for t in v {
            let mut inst = Instance::new();
            if self.unify(*t, t0, &mut inst) {
                result.push(*t);
            }
        }

        return result;
    }

    pub fn propagate_eq(
        &mut self,
        g: &mut TypeGraph,
        a: &mut TypeNode,
        b: &mut TypeNode,
    ) -> Result<(), String> {
        // If each node has one possible type, they better unify.
        if a.possible.len() == 1 && b.possible.len() == 1 {
            if self.unify(a.possible[0], b.possible[0], &mut g.inst) {
                // We've narrowed down overloads and unified
                // so this substituion applies to the whole graph.
                self.subst_graph(g);
            } else {
                return Err("type error".to_string());
            }
        }

        if a.possible.len() == 1 {
            b.possible = self.prune(&b.possible, a.possible[0]);
            if b.possible.len() == 0 {
                return Err("type error".to_string());
            }
        }

        if b.possible.len() == 1 {
            a.possible = self.prune(&a.possible, b.possible[0]);
            if a.possible.len() == 0 {
                return Err("type error".to_string());
            }
        }

        return Ok(());
    }
}
