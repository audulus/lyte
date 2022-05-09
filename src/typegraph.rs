use crate::defs::*;
use crate::types::*;

pub type TypeNodeID = usize;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct Constraint {
    pub a: TypeNodeID,
    pub b: TypeNodeID,
    pub field: Option<Name>,
    pub loc: Loc,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TypeNode {
    pub possible: Vec<TypeID>,
}

impl TypeNode {

    /// Returns the single possible type if there is only one.
    fn unique(&self) -> Option<TypeID> {
        if self.possible.len() == 1 { Some(self.possible[0]) } else { None }
    }

    /// Are there no possibilities in the type node? This indicates a type error.
    fn is_empty(&self) -> bool {
        self.possible.is_empty()
    }

    /// Remove all types from vec which don't unify with t0.
    fn prune(&mut self, t0: TypeID) {
        self.possible.retain(|t| {
            let mut inst = Instance::new();
            unify(*t, t0, &mut inst)
        });
    }
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

    /// Adds a type node, which is a collection of potential types.
    pub fn add_node(&mut self) -> TypeNodeID {
        let ix = self.nodes.len();
        self.nodes.push(TypeNode {
            possible: Vec::new(),
        });
        ix
    }

    /// Add a possible type to a type node.
    pub fn add_possible(&mut self, node: TypeNodeID, ty: TypeID) {
        self.nodes[node].possible.push(ty)
    }

    /// Adds a node for a single type.
    pub fn add_type_node(&mut self, ty: TypeID) -> TypeNodeID {
        let ix = self.nodes.len();
        self.nodes.push(TypeNode {
            possible: vec![ty],
        });
        ix
    }

    /// Add a constraint (edge) to our type graph.
    pub fn add_constraint(&mut self, c: &Constraint) {
        self.constraints.push(c.clone())
    }

    /// Add an equality constraint between type nodes.
    pub fn eq_constraint(&mut self, t0: TypeNodeID, t1: TypeNodeID, loc: Loc) {
        self.add_constraint(&Constraint {
            a: t0,
            b: t1,
            field: None,
            loc: loc,
        })
    }

    /// Adds a constraint that two types must be equal.
    /// XXX: can't we immediately unify and update self.inst
    /// since there are no overloads?
    pub fn eq_types(&mut self, t0: TypeID, t1: TypeID, loc: Loc) {
        if t0 != t1 {
            let tn0 = self.add_type_node(t0);
            let tn1 = self.add_type_node(t1);
            self.eq_constraint(tn0, tn1, loc)
        }
    }

    /// Add a constraint that one of the struct types must have a feild
    /// that unifies with another type.
    pub fn field_constraint(&mut self, structs: Vec<TypeID>, t: TypeID, field: Name, loc: Loc) {
        let a = self.add_node();
        self.nodes[a].possible = structs;
        let b = self.add_type_node(t);
        self.add_constraint(&Constraint{
            a,
            b,
            field: Some(field),
            loc
        })
    }

    /// Applies the current instance to the entire
    /// type graph.
    fn subst(&mut self) {
        for n in &mut self.nodes {
            for t in &mut n.possible {
                *t = subst(*t, &self.inst);
            }
        }
    }

    /// The graph is solved if all nodes have a single
    /// possible type and those types are all solved.
    pub fn solved(&self) -> bool {
        self.nodes
            .iter()
            .all(|n| n.possible.len() == 1 && solved(n.possible[0]))
    }

    pub fn propagate_eq(
        &mut self,
        a: TypeNodeID,
        b: TypeNodeID,
        loc: Loc,
    ) -> Result<(), Loc> {
        // If each node has one possible type, they better unify.
        if let (Some(t0), Some(t1)) = (self.nodes[a].unique(), self.nodes[b].unique()) {
            if unify(t0, t1, &mut self.inst) {
                // We've narrowed down overloads and unified
                // so this substituion applies to the whole graph.
                self.subst();
            } else {
                return Err(loc);
            }
        }

        if let Some(t) = self.nodes[a].unique() {
            self.nodes[b].prune(t);
            if self.nodes[b].is_empty() {
                return Err(loc);
            }
        }

        if let Some(t) = self.nodes[b].unique() {
            self.nodes[a].prune(t);
            if self.nodes[a].is_empty() {
                return Err(loc);
            }
        }

        Ok(())
    }

    pub fn propagate_field(
        &mut self,
        name: Name,
        a: TypeNodeID,
        b: TypeNodeID,
        decls: &Vec<Decl>,
        loc: Loc,
    ) -> Result<(), Loc> {
        if let Some(t) = self.nodes[b].unique() {
            self.nodes[a].possible.retain(|t| {
                if let Type::Name(struct_name) = **t {
                    let mut found = false;
                    find_decls(decls, struct_name, &mut |decl| {

                    });
                    found
                } else {
                    false
                }
            });

            if self.nodes[a].possible.len() == 0 {
                return Err(loc)
            }
        }

        Ok(())
    }

    pub fn propagate(&mut self) -> Result<(), Loc> {
        for c in self.constraints.clone() {
            self.propagate_eq(c.a, c.b, c.loc)?
        }
        Ok(())
    }

    pub fn validate(&self) -> bool {

        for i in 0..self.nodes.len() {

            let mut found = false;
            for c in &self.constraints {
                if c.a == i || c.b == i {
                    found = true;
                    break;
                }
            }

            if !found {
                println!("⚠️ type node {} is not connected to any constraint\n", i);
                return false;
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_typegraph() {
        let mut g = TypeGraph::new();

        let vd = mk_type(Type::Void);

        let a = g.add_node();
        g.nodes[a].possible.push(vd);
        let b = g.add_node();
        g.nodes[b].possible.push(vd);

        //g.eq_constraint(a, b, &Loc{ file: "".to_string(), line: 0});

        assert!(g.solved());

        let v = g.add_node();
        g.nodes[v].possible.push(typevar("T"));

        assert!(!g.solved());

        let result = g.propagate_eq(b, v, test_loc());
        assert_eq!(Ok(()), result);

        assert!(g.solved());
    }

    #[test]
    pub fn test_propagate1() {
        let mut g = TypeGraph::new();

        let int32 = mk_type(Type::Int32);

        let v = typevar("T");
        let a = g.add_node();
        g.nodes[a].possible.push(v);
        let b = g.add_node();
        g.nodes[b].possible.push(int32);

        g.eq_constraint(a, b, test_loc());

        assert!(!g.solved());

        let result = g.propagate();
        assert_eq!(Ok(()), result);

        assert!(g.solved());
    }

    #[test]
    pub fn test_propagate_bad() {
        let mut g = TypeGraph::new();

        let i = g.add_type_node(mk_type(Type::Int32));
        let f = g.add_type_node(mk_type(Type::Float32));

        assert!(!g.validate());

        g.eq_constraint(i, f, test_loc());

        assert!(g.validate());

        let result = g.propagate();

        assert!(result.is_err());

    }

    #[test]
    pub fn test_propagate_bad2() {
        let mut g = TypeGraph::new();

        let i = g.add_type_node(mk_type(Type::Int32));
        let v = g.add_type_node(typevar("T"));
        let f = g.add_type_node(mk_type(Type::Float32));

        assert!(!g.validate());

        g.eq_constraint(i, v, test_loc());
        g.eq_constraint(v, f, test_loc());

        assert!(g.validate());

        let result = g.propagate();

        assert!(result.is_err());

    }

    #[test]
    pub fn test_overload_1() {

        let mut g = TypeGraph::new();
        let i = mk_type(Type::Int32);
        let f = mk_type(Type::Float32);

        let i_node = g.add_type_node(i);
        let fi_node = g.add_node();
        g.add_possible(fi_node, f);
        g.add_possible(fi_node, i);
        g.eq_constraint(i_node, fi_node, test_loc());

        assert!(g.validate());
        assert!(!g.solved());

        let result = g.propagate();

        assert!(result.is_ok());
        assert!(g.solved());

    }
}
