use crate::*;

struct Checker {
    type_graph: TypeGraph,
    types: Vec<TypeID>,
    inst: Instance,
}

impl Checker {

    fn new() -> Self {
        Self {
            type_graph: TypeGraph::new(),
            types: vec![],
            inst: Instance::new(),
        }
    }

    fn eq<F: FnOnce()>(&mut self, lhs: TypeID, rhs: TypeID, loc: &Loc, errf: F) {
        self.type_graph.eq_types(lhs, rhs, loc);
        if !unify(lhs, rhs, &mut self.inst) {
            (errf)();
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
            Expr::Call(f, args) => {

                self.check_expr(*f, arena);

                for e in args {
                    self.check_expr(*e, arena);
                }

                
            }
            _ => { assert!(false) }
        }

    }
}
