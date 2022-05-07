use crate::*;
use internment::Intern;

#[derive(Copy, Clone, Debug)]
struct Var {
    name: Name,
    ty: TypeID,
    mutable: bool
}

struct Checker {
    type_graph: TypeGraph,
    types: Vec<TypeID>,
    lvalue: Vec<bool>,
    inst: Instance,
    next_anon: usize,
    vars: Vec<Var>
}

impl Checker {

    fn new() -> Self {
        Self {
            type_graph: TypeGraph::new(),
            types: vec![],
            lvalue: vec![],
            inst: Instance::new(),
            next_anon: 0,
            vars: vec![],
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
            Expr::Id(name) => {
                if let Some(v) = self.find(*name) {
                    self.types[id] = v.ty;
                    self.lvalue[id] = v.mutable;
                }
            }
            Expr::Call(f, args) => {

                self.check_expr(*f, arena);

                let v0 = self.fresh();
                let v1 = self.fresh();
                self.eq(self.types[*f], func(v0, v1), arena.locs[id], || {
                    println!("attempt to call a non-function");
                });

                let mut arg_types = vec![];
                for e in args {
                    self.check_expr(*e, arena);
                    arg_types.push(self.types[*e]);
                }

                let ft = func(v0, tuple(arg_types));

                self.eq(self.types[*f], ft, arena.locs[id], || {
                    println!("arguments don't match function.\n");
                });

                self.types[id] = ft;
                
            }
            _ => { assert!(false) }
        }

    }

    fn find(&self, name: Name) -> Option<Var> {
        for v in &self.vars {
            if v.name == name {
                return Some(*v)
            }
        }
        None
    }
}
