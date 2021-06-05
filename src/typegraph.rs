use crate::ast::*;
use crate::types::*;
use crate::defs::*;

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

    pub fn new() -> TypeGraph {
        return TypeGraph{ nodes: Vec::new(), constraints: Vec::new(), inst: Instance::new() }
    }

    pub fn add_node(&mut self) -> TypeNodeID {
        let ix = self.nodes.len();
        self.nodes.push( TypeNode{possible: Vec::new() } );
        return ix;
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
                *t = self.subst(*t, &g.inst);
            }
        }
    }

    pub fn solved_graph(&self, g: &TypeGraph) -> bool {
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
        a: TypeNodeID,
        b: TypeNodeID,
        loc: &Loc,
    ) -> Result<(), Loc> {

        // If each node has one possible type, they better unify.
        if g.nodes[a].possible.len() == 1 && g.nodes[b].possible.len() == 1 {
            if self.unify(g.nodes[a].possible[0], g.nodes[b].possible[0], &mut g.inst) {
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

        return Ok(());
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
        let l = Loc{ file: "".to_string(), line: 0};
        let mut c = Compiler::new();
        let mut g = TypeGraph::new();

        let a = g.add_node();
        g.nodes[a].possible.push( VOID );
        let b = g.add_node();
        g.nodes[b].possible.push( VOID );
        
        //g.eq_constraint(a, b, &Loc{ file: "".to_string(), line: 0});

        assert!(c.solved_graph(&g));

        let v = g.add_node();
        g.nodes[v].possible.push(c.mk_type(Type::Var(0)));

        assert!(!c.solved_graph(&g));

        let result = c.propagate_eq(&mut g, b, v, &l);
        assert_eq!(Ok(()), result);

        assert!(c.solved_graph(&g));
    }

    #[test]
    pub fn test_propagate1() {
        let l = Loc{ file: "".to_string(), line: 0};
        let mut c = Compiler::new();
        let mut g = TypeGraph::new();

        let v = c.fresh();
        let a = g.add_node();
        g.nodes[a].possible.push(v);
        let b = g.add_node();
        g.nodes[b].possible.push(INT32);

        g.eq_constraint(a, b, &l);

        assert!(!c.solved_graph(&g));

        let result = c.propagate(&mut g);
        assert_eq!(Ok(()), result);

        assert!(c.solved_graph(&g));
    }
}