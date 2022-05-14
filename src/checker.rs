use crate::*;
use internment::Intern;

#[derive(Copy, Clone, Debug)]
struct Var {
    name: Name,
    ty: TypeID,
    mutable: bool,
}

struct Checker {
    type_graph: TypeGraph,
    types: Vec<TypeID>,
    lvalue: Vec<bool>,
    inst: Instance,
    next_anon: usize,
    vars: Vec<Var>,
    arith_overloads: Vec<TypeID>,
    rel_overloads: Vec<TypeID>,
    neg_overloads: Vec<TypeID>,
}

impl Checker {
    fn new() -> Self {
        let types = [Type::Int32, Type::Float32];
        let mut arith_overloads = vec![];
        let mut rel_overloads = vec![];
        let mut neg_overloads = vec![];
        let b = mk_type(Type::Bool);

        for ty in types {
            let t = mk_type(ty);
            let tt = tuple(vec![t, t]);
            arith_overloads.push(func(tt, t));
            rel_overloads.push(func(tt, b));
            neg_overloads.push(func(t, t));
        }

        Self {
            type_graph: TypeGraph::new(),
            types: vec![],
            lvalue: vec![],
            inst: Instance::new(),
            next_anon: 0,
            vars: vec![],
            arith_overloads,
            rel_overloads,
            neg_overloads,
        }
    }

    fn eq<F: FnOnce()>(&mut self, lhs: TypeID, rhs: TypeID, loc: Loc, errf: F) {
        self.type_graph.eq_types(lhs, rhs, loc);
        if !unify(lhs, rhs, &mut self.inst) {
            (errf)();
        }
    }

    fn fresh(&mut self) -> TypeID {
        let t = mk_type(Type::Var(
            Intern::new(String::from("__anon__")),
            self.next_anon,
        ));
        self.next_anon += 1;
        t
    }

    fn check_expr(&mut self, id: ExprID, arena: &ExprArena, decls: &[Decl]) {
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
                } else {
                    let t = self.fresh();
                    let g = &mut self.type_graph;
                    let type_node = g.add_node();
                    g.add_possible(type_node, t);
                    let decls_node = g.add_node();
                    g.eq_constraint(type_node, decls_node, arena.locs[id]);

                    let mut found = false;
                    for d in decls {
                        if let Decl::Func(FuncDecl { name: fname, .. }) = d {
                            if fname == name {
                                g.add_possible(decls_node, d.ty());
                                found = true;
                            }
                        }
                    }
                    if !found {
                        println!("undeclared identifier: {:?}", *name);
                    }
                }
            }
            Expr::Binop(op, a, b) => {
                self.check_expr(*a, arena, decls);
                self.check_expr(*b, arena, decls);

                let at = self.types[*a];
                let bt = self.types[*b];

                if op.equality() {
                    self.eq(at, bt, arena.locs[id], || {
                        println!("equality operator requres equal types");
                    });

                    self.types[id] = mk_type(Type::Bool);
                }
            }
            Expr::Call(f, args) => {
                self.check_expr(*f, arena, decls);

                let v0 = self.fresh();
                let v1 = self.fresh();
                self.eq(self.types[*f], func(v0, v1), arena.locs[id], || {
                    println!("attempt to call a non-function");
                });

                let mut arg_types = vec![];
                for e in args {
                    self.check_expr(*e, arena, decls);
                    arg_types.push(self.types[*e]);
                }

                let ft = func(v0, tuple(arg_types));

                self.eq(self.types[*f], ft, arena.locs[id], || {
                    println!("arguments don't match function.\n");
                });

                self.types[id] = ft;
            }
            Expr::Field(lhs, name) => {
                self.check_expr(*lhs, arena, decls);

                self.lvalue[id] = self.lvalue[*lhs];

                let lhs_t = self.types[*lhs];
                let t = self.fresh();

                // Find all the struct declarations with that field.
                let mut structs = vec![];
                for d in decls {
                    if let Decl::Struct {
                        name: struct_name,
                        typevars: _,
                        fields,
                    } = d
                    {
                        for field in fields {
                            if field.name == *name {
                                structs.push(mk_type(Type::Name(*struct_name, vec![])));
                            }
                        }
                    }
                }

                let structs_node =
                    self.type_graph
                        .field_constraint(structs, t, *name, arena.locs[id]);
                let lhs_node = self.type_graph.add_type_node(lhs_t);
                self.type_graph
                    .eq_constraint(lhs_node, structs_node, arena.locs[id]);

                self.types[id] = t;
            }
            Expr::Enum(name) => {
                let t = self.fresh();
                let g = &mut self.type_graph;
                let enums_node = g.add_node();

                // Find all the enum declarations with that name.
                for d in decls {
                    if let Decl::Enum {
                        name: enum_name,
                        cases,
                    } = d
                    {
                        for case in cases {
                            if case == name {
                                g.add_possible(enums_node, mk_type(Type::Name(*enum_name, vec![])));
                                break;
                            }
                        }
                    }
                }

                let t_node = g.add_type_node(t);
                g.eq_constraint(enums_node, t_node, arena.locs[id]);
                self.types[id] = t;
            }
            _ => {
                panic!();
            }
        }
    }

    fn find(&self, name: Name) -> Option<Var> {
        for v in &self.vars {
            if v.name == name {
                return Some(*v);
            }
        }
        None
    }
}
