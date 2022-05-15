use crate::*;

#[derive(Clone, Debug)]
pub struct TypeError {
    pub location: Loc,
    pub message: String,
}

#[derive(Copy, Clone, Debug)]
struct Var {
    name: Name,
    ty: TypeID,
    mutable: bool,
}

pub struct Checker {
    type_graph: TypeGraph,
    pub types: Vec<TypeID>,
    lvalue: Vec<bool>,
    inst: Instance,
    next_anon: usize,
    vars: Vec<Var>,
    arith_overloads: Vec<TypeID>,
    rel_overloads: Vec<TypeID>,
    neg_overloads: Vec<TypeID>,
}

impl Checker {
    pub fn new() -> Self {
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

    fn eq(
        &mut self,
        lhs: TypeID,
        rhs: TypeID,
        loc: Loc,
        error_message: &str,
    ) -> Result<(), TypeError> {
        self.type_graph.eq_types(lhs, rhs, loc);
        if unify(lhs, rhs, &mut self.inst) {
            Ok(())
        } else {
            Err(TypeError {
                location: loc,
                message: error_message.into(),
            })
        }
    }

    fn fresh(&mut self) -> TypeID {
        let t = mk_type(Type::Var(
            Name::new(String::from("__anon__")),
            self.next_anon,
        ));
        self.next_anon += 1;
        t
    }

    fn check_expr(
        &mut self,
        id: ExprID,
        arena: &ExprArena,
        decls: &[Decl],
    ) -> Result<TypeID, TypeError> {
        let ty = match &arena[id] {
            Expr::True | Expr::False => mk_type(Type::Bool),
            Expr::Int(_) => mk_type(Type::Int32),
            Expr::Real(_) => mk_type(Type::Float32),
            Expr::Char(_) => mk_type(Type::Int8),
            Expr::String(s) => {
                let int8 = mk_type(Type::Int8);
                mk_type(Type::Array(int8, s.bytes().len() as i64))
            }
            Expr::Id(name) => {
                if let Some(v) = self.find(*name) {
                    self.lvalue[id] = v.mutable;
                    v.ty
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
                        return Err(TypeError {
                            location: arena.locs[id],
                            message: format!("undeclared identifier: {:?}", *name),
                        });
                    }
                    t
                }
            }
            Expr::Unop(a) => self.check_expr(*a, arena, decls)?,
            Expr::Binop(op, a, b) => {
                let at = self.check_expr(*a, arena, decls)?;
                let bt = self.check_expr(*b, arena, decls)?;

                if op.equality() {
                    self.eq(
                        at,
                        bt,
                        arena.locs[id],
                        "equality operator requres equal types",
                    )?;

                    mk_type(Type::Bool)
                } else {
                    self.fresh()
                }
            }
            Expr::Call(f, args) => {
                let lhs = self.check_expr(*f, arena, decls)?;

                let v0 = self.fresh();
                let v1 = self.fresh();
                self.eq(
                    self.types[*f],
                    func(v0, v1),
                    arena.locs[id],
                    "attempt to call a non-function",
                )?;

                let mut arg_types = vec![];
                for e in args {
                    self.check_expr(*e, arena, decls)?;
                    arg_types.push(self.types[*e]);
                }

                let ft = func(v0, tuple(arg_types));

                self.eq(lhs, ft, arena.locs[id], "arguments don't match function")?;

                v0
            }
            Expr::Macro(name, args) => {

                let mut found = false;
                let mut macro_type = mk_type(Type::Void);
                for d in decls {
                    if let Decl::Macro(fn_decl) = d {
                        if fn_decl.name == *name {
                            // Found our macro.
                            found = true;
                            macro_type = d.ty();
                        }
                    }
                }

                if !found {
                    return Err(TypeError {
                        location: arena.locs[id],
                        message: format!("unknown macro: {:?}", *name),
                    });
                }

                let mut arg_types = vec![];
                for e in args {
                    self.check_expr(*e, arena, decls)?;
                    arg_types.push(self.types[*e]);
                }

                let rt = self.fresh();
                let ft = func(rt, tuple(arg_types));

                self.eq(macro_type, ft, arena.locs[id], "arguments don't match function")?;

                rt
            }
            Expr::Field(lhs, name) => {
                let lhs_t = self.check_expr(*lhs, arena, decls)?;

                self.lvalue[id] = self.lvalue[*lhs];

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

                t
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
                t
            }
            Expr::Block(exprs) => {
                let n = self.vars.len();
                let mut t = mk_type(Type::Void);
                for e in exprs {
                    t = self.check_expr(*e, arena, decls)?;
                }
                while self.vars.len() > n {
                    self.vars.pop();
                }
                t
            }
            Expr::Var(name, init, ty) => {
                if let Some(e) = init {
                    self.check_expr(*e, arena, decls)?;
                }

                let ty = if let Some(ty) = ty { *ty } else { self.fresh() };

                self.vars.push(Var {
                    name: *name,
                    ty,
                    mutable: true,
                });

                ty
            }
            Expr::Arena(block) => self.check_expr(*block, arena, decls)?,
            Expr::Return(expr) => self.check_expr(*expr, arena, decls)?,
            Expr::ArrayLiteral(exprs) => {
                let t = self.fresh();
                let t_node = self.type_graph.add_type_node(t);
                for e in exprs {
                    let elem_t = self.check_expr(*e, arena, decls)?;
                    let elem_node = self.type_graph.add_type_node(elem_t);
                    self.type_graph.eq_constraint(t_node, elem_node, arena.locs[*e]);
                }
                t
            }
            Expr::ArrayIndex(array_expr, index_expr) => {
                let t = self.fresh();
                self.check_expr(*array_expr, arena, decls)?;
                let idx_t = self.check_expr(*index_expr, arena, decls)?;
                self.eq(
                    idx_t,
                    mk_type(Type::Int32),
                    arena.locs[*index_expr],
                    "array index must be an i32",
                )?;
                t
            }
            Expr::While(cond, body) => {
                let cond_t = self.check_expr(*cond, arena, decls)?;
                self.eq(
                    cond_t,
                    mk_type(Type::Bool),
                    arena.locs[*cond],
                    "while loop control must be a bool",
                )?;
                self.check_expr(*body, arena, decls)?;
                mk_type(Type::Void)
            }
            Expr::If(cond, then_expr, else_expr) => {
                let t = self.fresh();
                let cond_t = self.check_expr(*cond, arena, decls)?;
                self.eq(
                    cond_t,
                    mk_type(Type::Bool),
                    arena.locs[*cond],
                    "if expression conditional must be a bool",
                )?;
                self.check_expr(*then_expr, arena, decls)?;
                if let Some(else_expr) = else_expr {
                    self.check_expr(*else_expr, arena, decls)?;
                }
                t
            }
            Expr::Tuple(exprs) => {
                let mut types = vec![];
                for e in exprs {
                    types.push(self.check_expr(*e, arena, decls)?);
                }
                mk_type(Type::Tuple(types))
            }
            Expr::Lambda { params, body } => {
                let n = self.vars.len();
                let mut param_types = vec![];
                for param in params {
                    self.vars.push(Var {
                        name: param.name,
                        ty: param.ty,
                        mutable: false,
                    });
                    param_types.push(param.ty);
                }

                self.check_expr(*body, arena, decls)?;

                while self.vars.len() > n {
                    self.vars.pop();
                }

                let rt = self.fresh();

                func(mk_type(Type::Tuple(param_types)), rt)
            }
            _ => {
                println!("haven't yet implemented {:?}", &arena[id]);
                panic!();
            }
        };
        self.types[id] = ty;
        Ok(ty)
    }

    fn find(&self, name: Name) -> Option<Var> {
        for v in &self.vars {
            if v.name == name {
                return Some(*v);
            }
        }
        None
    }

    fn check_decl(
        &mut self,
        decl: &Decl,
        arena: &ExprArena,
        decls: &[Decl],
    ) -> Result<(), TypeError> {
        match decl {
            Decl::Func(func_decl) => {
                if let Some(body) = func_decl.body {

                    println!("---------- checking function {:?} ------------ ", *func_decl.name);

                    self.type_graph = TypeGraph::new();

                    for param in &func_decl.params {
                        self.vars.push(Var {
                            name: param.name,
                            ty: param.ty,
                            mutable: false,
                        });
                    }

                    let ty = self.check_expr(body, arena, decls)?;

                    self.eq(ty, func_decl.ret, arena.locs[body], "return type must match function return type")?;

                    self.vars.clear();

                    self.type_graph.validate();

                    println!("---- type graph before solving:");
                    self.type_graph.print();
                    println!("---- end type graph");

                    self.type_graph.solve();

                    if self.type_graph.solved() {
                        println!("solved type graph");
                    } else {
                        println!("❌ unable to solve type graph");
                    }

                    println!("instance: {:?}", self.type_graph.inst);
                    println!("---- type graph after solving:");
                    self.type_graph.print();
                    println!("---- end type graph");

                    Ok(())
                } else {
                    Ok(())
                }
            }
            Decl::Interface { name, funcs } => Ok(()),
            _ => Ok(()),
        }
    }

    pub fn check(&mut self, arena: &ExprArena, decls: &[Decl]) -> Result<(), TypeError> {
        self.types.resize(arena.exprs.len(), mk_type(Type::Void));
        self.lvalue.resize(arena.exprs.len(), false);
        for decl in decls {
            self.check_decl(decl, arena, decls)?;
        }
        Ok(())
    }
}
