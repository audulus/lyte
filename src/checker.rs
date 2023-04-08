use crate::*;
use std::collections::HashSet;

/// Type checking errors generated by
/// both the type checker and constraint
/// solver.
#[derive(Clone, Debug)]
pub struct TypeError {
    pub location: Loc,
    pub message: String,
}

/// Local variable declaration.
#[derive(Copy, Clone, Debug)]
struct Var {
    name: Name,
    ty: TypeID,
    mutable: bool,
}

/// Type-checks ASTs.
pub struct Checker {
    /// Overloads for arithmetic with built-in types.
    arith_overloads: Vec<TypeID>,

    /// Overloads for casting.
    cast_overloads: Vec<TypeID>,

    /// Constraints on types.
    constraints: Vec<Constraint>,

    /// Current type variable instance.
    inst: Instance,

    /// Is an expression assignable?
    lvalue: Vec<bool>,

    /// Overloads for negation of built-in types.
    neg_overloads: Vec<TypeID>,

    /// Next anonymous type variable index.
    next_anon: usize,

    /// Overloads for relational operations with built-in types.
    rel_overloads: Vec<TypeID>,

    /// Expression types.
    pub types: Vec<TypeID>,

    /// Currently declared vars, as we're checking.
    vars: Vec<Var>,

    /// Accumulated type errors while checking.
    pub errors: Vec<TypeError>,
}

impl Checker {
    pub fn new() -> Self {
        let int8 = mk_type(Type::Int8);
        let int32: TypeID = mk_type(Type::Int32);
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

        let cast_overloads = vec![
            func(int8, int32),
            func(int32, int8)
        ];

        Self {
            types: vec![],
            lvalue: vec![],
            inst: Instance::new(),
            next_anon: 0,
            vars: vec![],
            arith_overloads,
            rel_overloads,
            neg_overloads,
            cast_overloads,
            constraints: vec![],
            errors: vec![],
        }
    }

    fn eq(&mut self, lhs: TypeID, rhs: TypeID, loc: Loc, _error_message: &str) {
        self.add_constraint(Constraint::Equal(lhs, rhs, loc));
    }

    fn fresh(&mut self) -> TypeID {
        let t = mk_type(Type::Anon(self.next_anon));
        self.next_anon += 1;
        t
    }

    fn add_constraint(&mut self, c: Constraint) {
        // println!("adding constraint {:?}", &c);
        self.constraints.push(c);
    }

    fn check_binop(
        &mut self,
        id: ExprID,
        op: Binop,
        lhs: ExprID,
        rhs: ExprID,
        arena: &ExprArena,
        decls: &DeclTable,
    ) -> TypeID {
        let at = self.check_expr(lhs, arena, decls);
        let bt = self.check_expr(rhs, arena, decls);

        if op.equality() {
            self.eq(
                at,
                bt,
                arena.locs[id],
                &format!(
                    "equality operator requres equal types, got {:?} and {:?}",
                    at, bt
                ),
            );

            mk_type(Type::Bool)
        } else if let Binop::Assign = op {
            if !self.lvalue[lhs] {
                self.errors.push(TypeError {
                    location: arena.locs[id],
                    message: "left-hand side of assignment isn't assignable".to_string(),
                });
            }

            self.eq(
                at,
                bt,
                arena.locs[id],
                &format!(
                    "assignment operator requres equal types, got {:?} and {:?}",
                    at, bt
                ),
            );

            at
        } else if op.arithmetic() {
            let ft = self.fresh();

            let mut alts = vec![];

            for ty in &self.arith_overloads {
                alts.push(Alt {
                    ty: *ty,
                    interfaces: vec![],
                });
            }

            let overload_name = Name::new(op.overload_name().into());
            for d in decls.find(overload_name) {
                if let Decl::Func(_) = d {
                    let dt = d.ty().fresh(&mut self.next_anon);
                    alts.push(Alt {
                        ty: dt,
                        interfaces: vec![],
                    });
                }
            }

            self.add_constraint(Constraint::Or(ft, alts, arena.locs[id]));

            let r = self.fresh();

            self.eq(
                func(tuple(vec![at, bt]), r),
                ft,
                arena.locs[id],
                &format!("no match for arithemtic between {:?} and {:?}", at, bt),
            );

            r
        } else {
            self.fresh()
        }
    }

    fn check_expr(&mut self, id: ExprID, arena: &ExprArena, decls: &DeclTable) -> TypeID {
        let ty = match &arena[id] {
            Expr::True | Expr::False => mk_type(Type::Bool),
            Expr::Int(_) => mk_type(Type::Int32),
            Expr::Real(_) => mk_type(Type::Float32),
            Expr::Char(_) => mk_type(Type::Int8),
            Expr::String(s) => {
                let int8 = mk_type(Type::Int8);
                mk_type(Type::Array(int8, s.bytes().len() as i32))
            }
            Expr::Id(name) => {
                // Local variables will override all declarations.
                // Is this what we want?
                if let Some(v) = self.find(*name) {
                    self.lvalue[id] = v.mutable;
                    v.ty
                } else if let Some(Decl::Global { ty, .. }) = decls.find(*name).first() {
                    self.lvalue[id] = true;
                    *ty
                } else {
                    let t = self.fresh();
                    let alts: Vec<Alt> = decls
                        .alts(*name)
                        .iter()
                        .map(|alt| alt.fresh(&mut self.next_anon))
                        .collect();

                    if alts.is_empty() {
                        self.errors.push(TypeError {
                            location: arena.locs[id],
                            message: format!("undeclared identifier: {:?}", *name),
                        });
                    }

                    self.add_constraint(Constraint::Or(t, alts, arena.locs[id]));
                    t
                }
            }
            Expr::Unop(a) => self.check_expr(*a, arena, decls),
            Expr::Binop(op, a, b) => self.check_binop(id, *op, *a, *b, arena, decls),
            Expr::AsTy(e, ty) => {
                let et = self.check_expr(*e, arena, decls);

                let ft = self.fresh();

                let mut alts = vec![];
                for ty in &self.cast_overloads {
                    alts.push(Alt {
                        ty: *ty,
                        interfaces: vec![],
                    });
                }

                self.add_constraint(Constraint::Or(ft, alts, arena.locs[id]));

                self.eq(
                    func(et, *ty),
                    ft,
                    arena.locs[id],
                    &format!("no match for cast between {:?} and {:?}", et, ty),
                );

                *ty
            }
            Expr::Call(f, args) => {
                let lhs = self.check_expr(*f, arena, decls);

                let ret = self.fresh();
                let v1 = self.fresh();
                self.eq(
                    self.types[*f],
                    func(v1, ret),
                    arena.locs[id],
                    "attempt to call a non-function",
                );

                let mut arg_types = vec![];
                for e in args {
                    self.check_expr(*e, arena, decls);
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
                );

                ret
            }
            Expr::Macro(name, args) => {
                let mut found = false;
                let mut macro_type = mk_type(Type::Void);

                // XXX: overloaded macros?
                for d in decls.find(*name) {
                    if let Decl::Macro(_) = d {
                        // Found our macro.
                        found = true;
                        macro_type = d.ty().fresh(&mut self.next_anon);
                    }
                }

                if !found {
                    self.errors.push(TypeError {
                        location: arena.locs[id],
                        message: format!("unknown macro: {:?}", *name),
                    });
                }

                let mut arg_types = vec![];
                for e in args {
                    self.check_expr(*e, arena, decls);
                    arg_types.push(self.types[*e]);
                }

                let ret = self.fresh();
                let ft = func(tuple(arg_types), ret);

                self.eq(
                    macro_type,
                    ft,
                    arena.locs[id],
                    "arguments don't match macro definition",
                );

                ret
            }
            Expr::Field(lhs, name) => {
                let lhs_t = self.check_expr(*lhs, arena, decls);

                self.lvalue[id] = self.lvalue[*lhs];

                let t = self.fresh();

                self.constraints
                    .push(Constraint::Field(lhs_t, *name, t, arena.locs[id]));

                t
            }
            Expr::Enum(name) => {
                let t = self.fresh();
                let mut alts = vec![];

                // Find all the enum declarations with that name.
                decls.find_enum(*name, &mut |enum_name| {
                    let enum_ty = mk_type(Type::Name(enum_name, vec![]));
                    alts.push(Alt {
                        ty: enum_ty,
                        interfaces: vec![],
                    });
                });

                self.constraints
                    .push(Constraint::Or(t, alts, arena.locs[id]));

                t
            }
            Expr::Block(exprs) => {
                let n = self.vars.len();
                let mut t = mk_type(Type::Void);
                for e in exprs {
                    t = self.check_expr(*e, arena, decls);
                }
                while self.vars.len() > n {
                    self.vars.pop();
                }
                t
            }
            Expr::Var(name, init, ty) => {
                let ty = if let Some(ty) = ty { *ty } else { self.fresh() };

                if let Some(e) = init {
                    let init_ty = self.check_expr(*e, arena, decls);

                    self.eq(
                        ty,
                        init_ty,
                        arena.locs[id],
                        "variable initializer type must match",
                    );
                }

                self.vars.push(Var {
                    name: *name,
                    ty,
                    mutable: true,
                });

                ty
            }
            Expr::Arena(block) => self.check_expr(*block, arena, decls),
            Expr::Return(expr) => self.check_expr(*expr, arena, decls),
            Expr::ArrayLiteral(exprs) => {
                let t = self.fresh();
                for e in exprs {
                    let elem_t = self.check_expr(*e, arena, decls);
                    self.constraints
                        .push(Constraint::Equal(t, elem_t, arena.locs[*e]));
                }
                mk_type(Type::Array(t, 0))
            }
            Expr::ArrayIndex(array_expr, index_expr) => {
                let t = self.fresh();
                let array_t = self.check_expr(*array_expr, arena, decls);
                let idx_t = self.check_expr(*index_expr, arena, decls);
                self.lvalue[id] = self.lvalue[*array_expr];
                self.eq(
                    idx_t,
                    mk_type(Type::Int32),
                    arena.locs[*index_expr],
                    "array index must be an i32",
                );
                self.eq(
                    array_t,
                    mk_type(Type::Array(t, 0)),
                    arena.locs[*array_expr],
                    "must index into an array",
                );
                t
            }
            Expr::Array(value, size) => {
                let value_t = self.check_expr(*value, arena, decls);
                let size_t = self.check_expr(*size, arena, decls);

                self.eq(
                    size_t,
                    mk_type(Type::Int32),
                    arena.locs[*size],
                    "array size must be an i32",
                );

                mk_type(Type::Array(value_t, 0))
            }
            Expr::While(cond, body) => {
                let cond_t = self.check_expr(*cond, arena, decls);
                self.eq(
                    cond_t,
                    mk_type(Type::Bool),
                    arena.locs[*cond],
                    "while loop control must be a bool",
                );
                self.check_expr(*body, arena, decls);
                mk_type(Type::Void)
            }
            Expr::If(cond, then_expr, else_expr) => {
                let t = self.fresh();
                let cond_t = self.check_expr(*cond, arena, decls);
                self.eq(
                    cond_t,
                    mk_type(Type::Bool),
                    arena.locs[*cond],
                    "if expression conditional must be a bool",
                );
                self.check_expr(*then_expr, arena, decls);
                if let Some(else_expr) = else_expr {
                    self.check_expr(*else_expr, arena, decls);
                }
                t
            }
            Expr::Tuple(exprs) => {
                let mut types = vec![];
                for e in exprs {
                    types.push(self.check_expr(*e, arena, decls));
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
                        ty,
                    });
                    param_types.push(ty);
                }

                let rt = self.check_expr(*body, arena, decls);

                while self.vars.len() > n {
                    self.vars.pop();
                }

                func(tuple(param_types), rt)
            }
            _ => {
                println!("haven't yet implemented {:?}", &arena[id]);
                panic!();
            }
        };
        self.types[id] = ty;
        ty
    }

    fn find(&self, name: Name) -> Option<Var> {
        for v in &self.vars {
            if v.name == name {
                return Some(*v);
            }
        }
        None
    }

    fn check_fn_decl(&mut self, func_decl: &FuncDecl, decls: &DeclTable) {
        let n = func_decl.arena.exprs.len();
        self.types.resize(n, mk_type(Type::Void));
        self.lvalue.resize(n, false);

        if let Some(body) = func_decl.body {
            // println!("🟧 checking function {:?} 🟧", *func_decl.name);

            self.inst.clear();
            self.constraints.clear();

            for param in &func_decl.params {
                self.vars.push(Var {
                    name: param.name,
                    ty: param.ty.unwrap(),
                    mutable: false,
                });
            }

            // Add interface functions to available functions.
            for constraint in &func_decl.constraints {
                if let Some(Decl::Interface(interface)) =
                    decls.find(constraint.interface_name).first()
                {
                    let mut inst = Instance::new();

                    interface
                        .typevars
                        .iter()
                        .zip(&constraint.typevars)
                        .for_each(|pair| {
                            let t0 = mk_type(Type::Var(*pair.0));
                            let t1 = mk_type(Type::Var(*pair.1));
                            inst.insert(t0, t1);
                        });

                    for func in &interface.funcs {
                        self.vars.push(Var {
                            name: func.name,
                            ty: func.ty().subst(&inst),
                            mutable: false,
                        });
                    }
                } else {
                    self.errors.push(TypeError {
                        location: func_decl.loc,
                        message: format!("unknown interface: {}", constraint.interface_name),
                    })
                }
            }

            // Check the body of the function.
            let ty = self.check_expr(body, &func_decl.arena, decls);

            if func_decl.ret != mk_type(Type::Void) {
                self.eq(
                    ty,
                    func_decl.ret,
                    func_decl.arena.locs[body],
                    "return type must match function return type",
                );
            }

            self.vars.clear();

            solve_constraints(
                &mut self.constraints,
                &mut self.inst,
                decls,
                &mut self.errors,
            );

            //println!("instance:");
            //print_instance(&self.inst);
        }
    }

    fn check_struct_decl(&mut self, st: &StructDecl) {
        let mut names = HashSet::new();

        for field in &st.fields {
            if names.contains(&field.name) {
                self.errors.push(TypeError {
                    location: field.loc,
                    message: format!("repeated field: {}", &field.name),
                })
            } else {
                names.insert(field.name);
            }

            field.ty.typevars(&mut |name| {
                if !st.typevars.iter().any(|n| *n == name) {
                    self.errors.push(TypeError {
                        location: field.loc,
                        message: format!("unknown type variable: {}", name),
                    })
                }
            })
        }
    }

    fn check_interface(&mut self, _name: Name, funcs: &[FuncDecl]) {
        for func in funcs {
            if func.body.is_some() {
                self.errors.push(TypeError {
                    location: func.loc,
                    message: format!("interface function can't have body: {}", func.name),
                })
            }
        }
    }

    fn _check_decl(&mut self, decl: &Decl, decls: &DeclTable) {
        match decl {
            Decl::Func(func_decl) => self.check_fn_decl(func_decl, decls),
            Decl::Macro(func_decl) => self.check_fn_decl(func_decl, decls),
            Decl::Interface(Interface { name, funcs, .. }) => self.check_interface(*name, funcs),
            Decl::Struct(struct_decl) => self.check_struct_decl(struct_decl),
            _ => (),
        }
    }

    pub fn check(&mut self, decls: &DeclTable) {
        for decl in &decls.decls {
            self._check_decl(decl, decls);
        }
    }

    pub fn check_decl(&mut self, decl: &Decl, decls: &DeclTable) {
        self._check_decl(decl, decls);
    }

    /// Returns types we've solved for.
    pub fn solved_types(&self) -> Vec<TypeID> {
        self.types.iter().map(|t| t.subst(&self.inst)).collect()
    }
}
