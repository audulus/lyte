use crate::defs::*;
use crate::types::*;

type TypeNodeID = usize;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct Constraint {
    pub a: TypeNodeID,
    pub b: TypeNodeID,
    pub field: String,
    pub loc: Loc,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TypeNode {
    pub possible: Vec<TypeID>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TypeGraph {
    pub nodes: Vec<TypeNode>,
    pub constraints: Vec<Constraint>,
    pub inst: Instance,
}

impl TypeGraph {
    pub fn new() -> Self {
        Self {
            nodes: vec![],
            constraints: vec![],
            inst: Instance::new(),
        }
    }

    pub fn add_node(&mut self) -> TypeNodeID {
        let ix = self.nodes.len();
        self.nodes.push(TypeNode {
            possible: Vec::new(),
        });
        ix
    }

    pub fn add_constraint(&mut self, c: &Constraint) {
        self.constraints.push(c.clone())
    }

    pub fn eq_constraint(&mut self, t0: TypeNodeID, t1: TypeNodeID, loc: &Loc) {
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
                *t = subst(*t, &g.inst);
            }
        }
    }

    pub fn solved_graph(&self, g: &TypeGraph) -> bool {
        g.nodes
            .iter()
            .all(|n| n.possible.len() == 1 && solved(n.possible[0]))
    }

    fn prune(&mut self, v: &[TypeID], t0: TypeID) -> Vec<TypeID> {
        let mut result = vec![];

        for t in v {
            let mut inst = Instance::new();
            if unify(*t, t0, &mut inst) {
                result.push(*t);
            }
        }

        result
    }

    pub fn propagate_eq(
        &mut self,
        g: &mut TypeGraph,
        a: TypeNodeID,
        b: TypeNodeID,
        loc: &Loc,
    ) -> Result<(), Loc> {
        // If each node has one possible type, they better unify.
        if g.nodes[a].possible.len() == 1 && g.nodes[b].possible.len() == 1 {
            if unify(g.nodes[a].possible[0], g.nodes[b].possible[0], &mut g.inst) {
                // We've narrowed down overloads and unified
                // so this substituion applies to the whole graph.
                self.subst_graph(g);
            } else {
                return Err(loc.clone());
            }
        }

        if g.nodes[a].possible.len() == 1 {
            g.nodes[b].possible = self.prune(&g.nodes[b].possible, g.nodes[a].possible[0]);
            if g.nodes[b].possible.len() == 0 {
                return Err(loc.clone());
            }
        }

        if g.nodes[b].possible.len() == 1 {
            g.nodes[a].possible = self.prune(&g.nodes[a].possible, g.nodes[b].possible[0]);
            if g.nodes[a].possible.len() == 0 {
                return Err(loc.clone());
            }
        }

        Ok(())
    }

    pub fn propagate(&mut self, g: &mut TypeGraph) -> Result<(), Loc> {
        for c in g.constraints.clone() {
            self.propagate_eq(g, c.a, c.b, &c.loc)?
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_typegraph() {
        let l = Loc {
            file: "".to_string(),
            line: 0,
        };
        let mut c = Compiler::new();
        let mut g = TypeGraph::new();

        let vd = mk_type(Type::Void);

        let a = g.add_node();
        g.nodes[a].possible.push(vd);
        let b = g.add_node();
        g.nodes[b].possible.push(vd);

        //g.eq_constraint(a, b, &Loc{ file: "".to_string(), line: 0});

        assert!(c.solved_graph(&g));

        let v = g.add_node();
        g.nodes[v].possible.push(typevar("T"));

        assert!(!c.solved_graph(&g));

        let result = c.propagate_eq(&mut g, b, v, &l);
        assert_eq!(Ok(()), result);

        assert!(c.solved_graph(&g));
    }

    #[test]
    pub fn test_propagate1() {
        let l = Loc {
            file: "".to_string(),
            line: 0,
        };
        let mut c = Compiler::new();
        let mut g = TypeGraph::new();

        let int32 = mk_type(Type::Int32);

        let v = typevar("T");
        let a = g.add_node();
        g.nodes[a].possible.push(v);
        let b = g.add_node();
        g.nodes[b].possible.push(int32);

        g.eq_constraint(a, b, &l);

        assert!(!c.solved_graph(&g));

        let result = c.propagate(&mut g);
        assert_eq!(Ok(()), result);

        assert!(c.solved_graph(&g));
    }
}
