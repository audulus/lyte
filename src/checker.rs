use crate::*;
use internment::Intern;

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

    fn eq(&mut self, lhs: TypeID, rhs: TypeID, loc: Loc, error_message: &str) -> Result<(), TypeError> {
        self.type_graph.eq_types(lhs, rhs, loc);
        if unify(lhs, rhs, &mut self.inst) {
            Ok(())
        } else {
            Err(TypeError{location: loc, message: error_message.into() })
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

    fn check_expr(&mut self, id: ExprID, arena: &ExprArena, decls: &[Decl]) -> Result<(), TypeError> {
        match &arena[id] {
            Expr::True | Expr::False => {
                self.types[id] = mk_type(Type::Bool);
                Ok(())
            }
            Expr::Int(_) => {
                self.types[id] = mk_type(Type::Int32);
                Ok(())
            }
            Expr::Real(_) => {
                self.types[id] = mk_type(Type::Float32);
                Ok(())
            }
            Expr::Id(name) => {
                if let Some(v) = self.find(*name) {
                    self.types[id] = v.ty;
                    self.lvalue[id] = v.mutable;
                    Ok(())
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
                    if found {
                        Ok(())
                    } else {
                        Err(TypeError{ location: arena.locs[id], message: format!("undeclared identifier: {:?}", *name)})
                    }
                }
            }
            Expr::Binop(op, a, b) => {
                self.check_expr(*a, arena, decls)?;
                self.check_expr(*b, arena, decls)?;

                let at = self.types[*a];
                let bt = self.types[*b];

                if op.equality() {
                    self.eq(at, bt, arena.locs[id], "equality operator requres equal types")?;

                    self.types[id] = mk_type(Type::Bool);
                }
                Ok(())
            }
            Expr::Call(f, args) => {
                self.check_expr(*f, arena, decls)?;

                let v0 = self.fresh();
                let v1 = self.fresh();
                self.eq(self.types[*f], func(v0, v1), arena.locs[id], "attempt to call a non-function")?;

                let mut arg_types = vec![];
                for e in args {
                    self.check_expr(*e, arena, decls)?;
                    arg_types.push(self.types[*e]);
                }

                let ft = func(v0, tuple(arg_types));

                self.eq(self.types[*f], ft, arena.locs[id], "arguments don't match function")?;

                self.types[id] = ft;
                Ok(())
            }
            Expr::Field(lhs, name) => {
                self.check_expr(*lhs, arena, decls)?;

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
                Ok(())
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
                Ok(())
            }
            Expr::Block(exprs) => {
                for e in exprs {
                    self.check_expr(*e, arena, decls)?;
                }
                Ok(())
            }
            Expr::Var(name, init, ty) => {

                if let Some(e) = init {
                    self.check_expr(*e, arena, decls)?;
                }

                Ok(())
            }
            Expr::Arena(block) => {
                self.check_expr(*block, arena, decls)
            }
            Expr::Return(expr) => {
                self.check_expr(*expr, arena, decls)
            }
            Expr::ArrayLiteral(exprs) => {
                for e in exprs {
                    self.check_expr(*e, arena, decls)?;
                }
                Ok(())
            }
            Expr::ArrayIndex(array_expr, index_expr) => {
                self.check_expr(*array_expr, arena, decls)?;
                self.check_expr(*index_expr, arena, decls)?;
                Ok(())
            }
            Expr::While(cond, body) => {
                self.check_expr(*cond, arena, decls)?;
                self.check_expr(*body, arena, decls)?;
                Ok(())
            }
            Expr::If(cond, then_expr, else_expr) => {
                self.check_expr(*cond, arena, decls)?;
                self.check_expr(*then_expr, arena, decls)?;
                if let Some(else_expr) = else_expr {
                    self.check_expr(*else_expr, arena, decls)?;
                }
                Ok(())
            }
            Expr::Tuple(exprs) => {
                for e in exprs {
                    self.check_expr(*e, arena, decls)?;
                }
                Ok(())
            }
            _ => {
                println!("haven't yet implemented {:?}", &arena[id]);
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

    fn check_decl(&mut self, decl: &Decl, arena: &ExprArena, decls: &[Decl]) -> Result<(), TypeError> {
        match decl {
            Decl::Func(func_decl) => {
                self.type_graph = TypeGraph::new();
                if let Some(body) = func_decl.body {
                    self.check_expr(body, arena, decls)
                } else {
                    Ok(())
                }
            }
            Decl::Interface{ name, funcs } => {
                Ok(())
            }
            _ => Ok(())
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
