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
    /// Expression types.
    pub types: Vec<TypeID>,

    /// Is an expression assignable?
    lvalue: Vec<bool>,

    /// Current type variable instance.
    inst: Instance,

    /// Next anonymous type variable index.
    next_anon: usize,

    /// Currently declared vars, as we're checking.
    vars: Vec<Var>,

    /// Overloads for arithmetic with built-in types.
    arith_overloads: Vec<TypeID>,

    /// Overloads for relational operations with built-in types.
    rel_overloads: Vec<TypeID>,

    /// Overloads for negation of built-in types.
    neg_overloads: Vec<TypeID>,

    /// New constraints.
    constraints: Vec<Constraint>,
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
            types: vec![],
            lvalue: vec![],
            inst: Instance::new(),
            next_anon: 0,
            vars: vec![],
            arith_overloads,
            rel_overloads,
            neg_overloads,
            constraints: vec![],
        }
    }

    fn eq(
        &mut self,
        lhs: TypeID,
        rhs: TypeID,
        loc: Loc,
        error_message: &str,
    ) -> Result<(), TypeError> {
        self.constraints.push(Constraint::Equal(lhs, rhs, loc));
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
        let t = mk_type(Type::Anon(self.next_anon));
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
                    let mut alternatives = vec![];
                    let mut found = false;
                    for d in decls {
                        if let Decl::Func(FuncDecl { name: fname, .. }) = d {
                            if fname == name {
                                let dt = fresh(d.ty(), &mut self.next_anon);
                                alternatives.push(dt);
                                found = true;
                            }
                        }
                        if let Decl::Global { name: gname, .. } = d {
                            if gname == name {
                                alternatives.push(d.ty());
                                found = true;
                            }
                        }
                    }

                    self.constraints
                        .push(Constraint::Or(t, alternatives, arena.locs[id]));

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
                        &format!(
                            "equality operator requres equal types, got {:?} and {:?}",
                            at, bt
                        ),
                    )?;

                    mk_type(Type::Bool)
                } else if let Binop::Assign = op {
                    // XXX: lhs should be lvalue.

                    self.eq(
                        at,
                        bt,
                        arena.locs[id],
                        &format!(
                            "assignment operator requres equal types, got {:?} and {:?}",
                            at, bt
                        ),
                    )?;

                    at
                } else if op.arithmetic() {
                    let ft = self.fresh();
                    self.constraints.push(Constraint::Or(
                        ft,
                        self.arith_overloads.clone(),
                        arena.locs[id],
                    ));

                    let r = self.fresh();

                    self.eq(
                        func(tuple(vec![at, bt]), r),
                        ft,
                        arena.locs[id],
                        &format!("no match for arithemtic between {:?} and {:?}", at, bt),
                    )?;

                    r
                } else {
                    self.fresh()
                }
            }
            Expr::Call(f, args) => {
                let lhs = self.check_expr(*f, arena, decls)?;

                let ret = self.fresh();
                let v1 = self.fresh();
                self.eq(
                    self.types[*f],
                    func(v1, ret),
                    arena.locs[id],
                    "attempt to call a non-function",
                )?;

                let mut arg_types = vec![];
                for e in args {
                    self.check_expr(*e, arena, decls)?;
                    arg_types.push(self.types[*e]);
                }

                let ft = func(tuple(arg_types), ret);

                self.eq(
                    lhs,
                    ft,
                    arena.locs[id],
                    &format!(
                        "arguments ({:?}) don't match function ({:?})",
                        find(lhs, &self.inst),
                        find(ft, &self.inst)
                    ),
                )?;

                ret
            }
            Expr::Macro(name, args) => {
                let mut found = false;
                let mut macro_type = mk_type(Type::Void);
                for d in decls {
                    if let Decl::Macro(fn_decl) = d {
                        if fn_decl.name == *name {
                            // Found our macro.
                            found = true;
                            macro_type = fresh(d.ty(), &mut self.next_anon);
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

                let ret = self.fresh();
                let ft = func(tuple(arg_types), ret);

                self.eq(
                    macro_type,
                    ft,
                    arena.locs[id],
                    "arguments don't match macro definition",
                )?;

                ret
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

                // Special case for len on arrays.
                if *name == Name::new("len".into()) {
                    structs.push(lhs_t);
                }

                self.constraints
                    .push(Constraint::Field(lhs_t, *name, t, arena.locs[id]));

                t
            }
            Expr::Enum(name) => {
                let t = self.fresh();
                let mut alternatives = vec![];

                // Find all the enum declarations with that name.
                find_enums_with_case(decls, *name, &mut |enum_name| {
                    let enum_ty = mk_type(Type::Name(enum_name, vec![]));
                    alternatives.push(enum_ty);
                });

                self.constraints
                    .push(Constraint::Or(t, alternatives, arena.locs[id]));

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
                let ty = if let Some(ty) = ty { *ty } else { self.fresh() };

                if let Some(e) = init {
                    let init_ty = self.check_expr(*e, arena, decls)?;

                    self.eq(
                        ty,
                        init_ty,
                        arena.locs[id],
                        "variable initializer type must match",
                    )?;
                }

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
                for e in exprs {
                    let elem_t = self.check_expr(*e, arena, decls)?;
                    self.constraints
                        .push(Constraint::Equal(t, elem_t, arena.locs[*e]));
                }
                mk_type(Type::Array(t, 0))
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
            Expr::Array(value, size) => {
                let value_t = self.check_expr(*value, arena, decls)?;
                let size_t = self.check_expr(*size, arena, decls)?;

                self.eq(
                    size_t,
                    mk_type(Type::Int32),
                    arena.locs[*size],
                    "array size must be an i32",
                )?;

                mk_type(Type::Array(value_t, 0))
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
                    let ty = param.ty.unwrap_or_else(|| self.fresh());
                    self.vars.push(Var {
                        name: param.name,
                        mutable: false,
                        ty: ty,
                    });
                    param_types.push(ty);
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

    fn check_fn_decl(
        &mut self,
        func_decl: &FuncDecl,
        arena: &ExprArena,
        decls: &[Decl],
    ) -> Result<(), TypeError> {
        if let Some(body) = func_decl.body {
            println!("🟧 checking function {:?} 🟧", *func_decl.name);

            self.inst.clear();
            self.constraints.clear();

            for param in &func_decl.params {
                self.vars.push(Var {
                    name: param.name,
                    ty: param.ty.unwrap(),
                    mutable: false,
                });
            }

            let ty = self.check_expr(body, arena, decls)?;

            if func_decl.ret != mk_type(Type::Void) {
                self.eq(
                    ty,
                    func_decl.ret,
                    arena.locs[body],
                    "return type must match function return type",
                )?;
            }

            self.vars.clear();

            solve_constraints(&mut self.constraints, &mut self.inst, decls)?;

            println!("instance:");
            print_instance(&self.inst);

            Ok(())
        } else {
            Ok(())
        }
    }

    fn check_interface(&self, _name: Name, _funcs: &[FuncDecl]) -> Result<(), TypeError> {
        Ok(())
    }

    fn _check_decl(
        &mut self,
        decl: &Decl,
        arena: &ExprArena,
        decls: &[Decl],
    ) -> Result<(), TypeError> {
        match decl {
            Decl::Func(func_decl) => self.check_fn_decl(func_decl, arena, decls),
            Decl::Macro(func_decl) => self.check_fn_decl(func_decl, arena, decls),
            Decl::Interface { name, funcs } => self.check_interface(*name, funcs),
            _ => Ok(()),
        }
    }

    pub fn check(&mut self, arena: &ExprArena, decls: &[Decl]) -> Result<(), TypeError> {
        self.types.resize(arena.exprs.len(), mk_type(Type::Void));
        self.lvalue.resize(arena.exprs.len(), false);
        for decl in decls {
            self._check_decl(decl, arena, decls)?;
        }
        Ok(())
    }

    pub fn check_decl(
        &mut self,
        decl: &Decl,
        arena: &ExprArena,
        decls: &[Decl],
    ) -> Result<(), TypeError> {
        self.types.resize(arena.exprs.len(), mk_type(Type::Void));
        self.lvalue.resize(arena.exprs.len(), false);
        self._check_decl(decl, arena, decls)?;
        Ok(())
    }
}
