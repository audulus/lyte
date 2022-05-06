use crate::*;
use internment::Intern;

struct Checker {
    type_graph: TypeGraph,
    types: Vec<TypeID>,
    inst: Instance,
    next_anon: usize,
}

impl Checker {

    fn new() -> Self {
        Self {
            type_graph: TypeGraph::new(),
            types: vec![],
            inst: Instance::new(),
            next_anon: 0
        }
    }

    fn eq<F: FnOnce()>(&mut self, lhs: TypeID, rhs: TypeID, loc: Loc, errf: F) {
        self.type_graph.eq_types(lhs, rhs, loc);
        if !unify(lhs, rhs, &mut self.inst) {
            (errf)();
        }
    }

    fn fresh(&mut self) -> TypeID {
        let t = mk_type(Type::Var(Intern::new(String::from("__anon__")), self.next_anon));
        self.next_anon += 1;
        t
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

                let v0 = self.fresh();
                let v1 = self.fresh();
                self.eq(self.types[*f], mk_type(Type::Func(v0, v1)), arena.locs[id], || {
                    println!("attempt to call a non-function");
                });
                
            }
            _ => { assert!(false) }
        }

    }
}
