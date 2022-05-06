use crate::*;

struct Checker {
    type_graph: TypeGraph,
    types: Vec<TypeID>
}

impl Checker {

    fn new() -> Self {
        Self {
            type_graph: TypeGraph::new(),
            types: vec![]
        }
    }

    fn check_expr(&mut self, id: ExprID, arena: &ExprArena) {

        match &arena[id] {
            Expr::True | Expr::False => { 
                self.types[id] = mk_type(Type::Bool);
            }
            Expr::Int(_) => {
                self.types[id] = mk_type(Type::Int32);
            }
            Expr::Real(_) => {
                self.types[id] = mk_type(Type::Float32);
            }
            _ => { assert!(false) }
        }

    }
}
