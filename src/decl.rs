use crate::*;

/// Formal parameter.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Param {
    pub name: Name,
    pub ty: Option<TypeID>,
}

/// Struct field.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Field {
    pub name: Name,
    pub ty: TypeID,
    pub loc: Loc,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct InterfaceConstraint {
    pub interface_name: Name,
    pub typevars: Vec<Name>,
}

/// A variable captured by a closure.
///
/// Closures always capture by address: the closure struct stores a pointer to
/// the variable's stack slot.  For `var` bindings the slot already exists; for
/// `let` bindings the JIT allocates a fresh slot and copies the value there.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ClosureVar {
    pub name: Name,
    pub ty: TypeID,
}

/// Function declaration.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FuncDecl {
    /// Function name.
    pub name: Name,

    /// Generic type variables.
    pub typevars: Vec<Name>,

    /// Generic integer size variables (used as array sizes).
    pub size_vars: Vec<Name>,

    /// Function parameters.
    pub params: Vec<Param>,

    /// Function body expression ID in the arena.
    pub body: Option<ExprID>,

    /// Return type.
    pub ret: TypeID,

    /// The interfaces that must be available for this function.
    pub constraints: Vec<InterfaceConstraint>,

    /// Precondition expressions checked by the safety checker:
    /// assumed true inside the body, must be provable at every call site.
    /// Each ExprID indexes into `arena`.
    pub requires: Vec<ExprID>,

    /// Location of the declaration in source code.
    pub loc: Loc,

    /// Expression arena for the function body.
    pub arena: ExprArena,

    /// Solved types from the type checker.
    pub types: Vec<TypeID>,

    /// Variables captured from the enclosing scope (non-empty for closures).
    ///
    /// At runtime the JIT passes a `closure_ptr` pointing to a contiguous
    /// array of `i64` slots, one per entry here.  Each slot holds the address
    /// of the captured variable's storage.
    pub closure_vars: Vec<ClosureVar>,

    /// True if this is an extern function provided by the host.
    /// Extern functions have no body and are called indirectly through
    /// a {fn_ptr, context} pair stored in the globals buffer.
    pub is_extern: bool,
}

impl FuncDecl {
    /// Get the types of the function parameters.
    pub fn param_types(&self) -> Vec<TypeID> {
        self.params
            .iter()
            .map(|p| p.ty.expect("parameter type should be known"))
            .collect()
    }

    pub fn domain(&self) -> TypeID {
        crate::TypeID::new(crate::Type::Tuple(self.param_types()))
    }

    /// Expand all macro invocations in this function's arena.
    pub fn expand_macros(&mut self, macros: &std::collections::HashMap<Name, FuncDecl>) {
        let n = self.arena.exprs.len();
        for i in 0..n {
            if let Expr::Macro(name, ref args) = self.arena.exprs[i] {
                let args = args.clone();
                if let Some(mac) = macros.get(&name) {
                    let subst: Vec<(Name, ExprID)> = mac
                        .params
                        .iter()
                        .zip(args.iter())
                        .map(|(p, a)| (p.name, *a))
                        .collect();

                    let body = mac.body.expect("macro must have a body");
                    let new_body = copy_expr(body, &mac.arena, &mut self.arena, &subst);

                    self.arena.exprs[i] = self.arena.exprs[new_body].clone();
                    self.arena.locs[i] = self.arena.locs[new_body];
                }
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StructDecl {
    pub name: Name,
    pub typevars: Vec<Name>,
    pub fields: Vec<Field>,
}

impl StructDecl {
    pub fn find_field(&self, name: &Name) -> Option<Field> {
        for field in &self.fields {
            if field.name == *name {
                return Some(field.clone());
            }
        }
        None
    }

    pub fn field_offset(&self, name: &Name, decls: &DeclTable, inst: &Instance) -> i32 {
        let mut off = 0;
        for field in &self.fields {
            if field.name == *name {
                return off;
            }
            off += field.ty.subst(inst).size(decls);
        }
        panic!()
    }
}

/// Provides a set of functions that some type variables
/// must satisfy.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Interface {
    pub name: Name,
    pub typevars: Vec<Name>,
    pub funcs: Vec<FuncDecl>,
    pub loc: Loc,
}

/// Top-level declaration.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Decl {
    Func(FuncDecl),
    Macro(FuncDecl),
    Struct(StructDecl),
    Enum {
        name: Name,
        cases: Vec<Name>,
    },
    Global {
        name: Name,
        typevars: Vec<Name>,
        ty: TypeID,
    },
    Interface(Interface),
    Const {
        name: Name,
        value: i64,
    },
    Assume {
        arena: ExprArena,
        cond: ExprID,
    },
}

impl Decl {
    pub fn find_field(&self, name: &Name) -> Option<Field> {
        if let Decl::Struct(st) = self {
            st.find_field(name)
        } else {
            None
        }
    }
}

pub fn find_field(fields: &[Field], name: Name) -> Option<Field> {
    for field in fields {
        if field.name == name {
            return Some(field.clone());
        }
    }
    None
}

impl Decl {
    pub fn name(&self) -> Name {
        match self {
            Decl::Func(FuncDecl { name, .. }) => *name,
            Decl::Macro(FuncDecl { name, .. }) => *name,
            Decl::Struct(StructDecl { name, .. }) => *name,
            Decl::Enum { name, .. } => *name,
            Decl::Global { name, .. } => *name,
            Decl::Interface(Interface { name, .. }) => *name,
            Decl::Const { name, .. } => *name,
            Decl::Assume { .. } => Name::str("__assume"),
        }
    }

    /// Pretty-print a declaration in lyte syntax.
    ///
    /// This method formats a declaration as it would appear in lyte source code,
    /// suitable for displaying to users or generating code.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// // Given a function declaration:
    /// let decl = Decl::Func(func);
    /// let output = decl.pretty_print(&decls);
    /// // Output: "add(a: i32, b: i32) → i32"
    /// ```
    pub fn pretty_print(&self) -> String {
        match self {
            Decl::Func(func) => format_func_decl(func, false),
            Decl::Macro(func) => format_func_decl(func, true),
            Decl::Struct(st) => format_struct_decl(st),
            Decl::Enum { name, cases } => format_enum_decl(*name, cases),
            Decl::Global { name, typevars, ty } => {
                let tvs = format_typevars(typevars);
                format!("var {}{}: {}", name, tvs, ty.pretty_print())
            }
            Decl::Interface(iface) => format_interface(iface),
            Decl::Const { name, value } => format!("const {} = {}", name, value),
            Decl::Assume { arena, cond } => {
                format!("assume {}", arena.exprs[*cond].pretty_print(arena, 0))
            }
        }
    }
}

fn format_typevars(typevars: &[Name]) -> String {
    if typevars.is_empty() {
        String::new()
    } else {
        format!(
            "<{}>",
            typevars
                .iter()
                .map(|tv| tv.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

fn format_params(params: &[Param]) -> String {
    params
        .iter()
        .map(|p| {
            if let Some(ty) = p.ty {
                format!("{}: {}", p.name, ty.pretty_print())
            } else {
                p.name.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_constraints(constraints: &[InterfaceConstraint]) -> String {
    if constraints.is_empty() {
        String::new()
    } else {
        let formatted = constraints
            .iter()
            .map(|c| {
                if c.typevars.is_empty() {
                    c.interface_name.to_string()
                } else {
                    format!(
                        "{}<{}>",
                        c.interface_name,
                        c.typevars
                            .iter()
                            .map(|tv| tv.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!(" where {}", formatted)
    }
}

fn format_func_decl(func: &FuncDecl, is_macro: bool) -> String {
    let keyword = if is_macro { "macro" } else { "" };
    let typevars = format_typevars(&func.typevars);
    let params = format_params(&func.params);
    let ret_type = if matches!(&*func.ret, Type::Void) {
        String::new()
    } else {
        format!(" → {}", func.ret.pretty_print())
    };
    let constraints = format_constraints(&func.constraints);
    let requires = func
        .requires
        .iter()
        .map(|&r| format!(" require {}", func.arena.exprs[r].pretty_print(&func.arena, 0)))
        .collect::<Vec<_>>()
        .join("");

    let signature = if is_macro {
        format!("macro {}{}", func.name, typevars)
    } else if keyword.is_empty() {
        format!("{}{}", func.name, typevars)
    } else {
        format!("{} {}{}", keyword, func.name, typevars)
    };

    if let Some(body_id) = func.body {
        let body_str = func.arena.exprs[body_id].pretty_print(&func.arena, 0);
        format!(
            "{}({}){}{}{} {}",
            signature, params, ret_type, constraints, requires, body_str
        )
    } else {
        format!(
            "{}({}){}{}{}",
            signature, params, ret_type, constraints, requires
        )
    }
}

fn format_struct_decl(st: &StructDecl) -> String {
    let typevars = format_typevars(&st.typevars);
    let fields = st
        .fields
        .iter()
        .map(|f| format!("    {}: {}", f.name, f.ty.pretty_print()))
        .collect::<Vec<_>>()
        .join(",\n");

    if fields.is_empty() {
        format!("struct {}{} {{}}", st.name, typevars)
    } else {
        format!("struct {}{} {{\n{}\n}}", st.name, typevars, fields)
    }
}

fn format_enum_decl(name: Name, cases: &[Name]) -> String {
    if cases.is_empty() {
        format!("enum {} {{}}", name)
    } else {
        let cases_str = cases
            .iter()
            .map(|c| c.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        format!("enum {} {{ {} }}", name, cases_str)
    }
}

fn format_interface(iface: &Interface) -> String {
    let typevars = format_typevars(&iface.typevars);
    let funcs = iface
        .funcs
        .iter()
        .map(|f| format!("    {}", format_func_decl(f, false)))
        .collect::<Vec<_>>()
        .join("\n");

    if funcs.is_empty() {
        format!("interface {}{} {{}}", iface.name, typevars)
    } else {
        format!("interface {}{} {{\n{}\n}}", iface.name, typevars, funcs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pretty_print_func() {
        let func = FuncDecl {
            name: Name::str("add"),
            typevars: vec![],
            size_vars: vec![],
            params: vec![
                Param {
                    name: Name::str("a"),
                    ty: Some(mk_type(Type::Int32)),
                },
                Param {
                    name: Name::str("b"),
                    ty: Some(mk_type(Type::Int32)),
                },
            ],
            body: None,
            ret: mk_type(Type::Int32),
            constraints: vec![],
            requires: vec![],
            loc: test_loc(),
            arena: ExprArena::new(),
            types: vec![],
            closure_vars: vec![],
            is_extern: false,
        };

        let decl = Decl::Func(func);
        let output = decl.pretty_print();
        assert_eq!(output, "add(a: i32, b: i32) → i32");
    }

    #[test]
    fn test_pretty_print_generic_func() {
        let mut arena = ExprArena::new();
        let body_id = arena.add(Expr::Id(Name::str("x")), test_loc());

        let func = FuncDecl {
            name: Name::str("id"),
            typevars: vec![Name::str("T")],
            size_vars: vec![],
            params: vec![Param {
                name: Name::str("x"),
                ty: Some(mk_type(Type::Var(Name::str("T")))),
            }],
            body: Some(body_id),
            ret: mk_type(Type::Var(Name::str("T"))),
            constraints: vec![],
            requires: vec![],
            loc: test_loc(),
            arena,
            types: vec![],
            closure_vars: vec![],
            is_extern: false,
        };

        let decl = Decl::Func(func);
        let output = decl.pretty_print();
        assert_eq!(output, "id<T>(x: T) → T x");
    }

    #[test]
    fn test_pretty_print_struct() {
        let st = StructDecl {
            name: Name::str("Point"),
            typevars: vec![],
            fields: vec![
                Field {
                    name: Name::str("x"),
                    ty: mk_type(Type::Float32),
                    loc: test_loc(),
                },
                Field {
                    name: Name::str("y"),
                    ty: mk_type(Type::Float32),
                    loc: test_loc(),
                },
            ],
        };

        let decl = Decl::Struct(st);
        let output = decl.pretty_print();
        assert_eq!(output, "struct Point {\n    x: f32,\n    y: f32\n}");
    }

    #[test]
    fn test_pretty_print_interface() {
        let iface = Interface {
            name: Name::str("Compare"),
            typevars: vec![Name::str("A")],
            funcs: vec![FuncDecl {
                name: Name::str("cmp"),
                typevars: vec![],
                size_vars: vec![],
                params: vec![
                    Param {
                        name: Name::str("lhs"),
                        ty: Some(mk_type(Type::Var(Name::str("A")))),
                    },
                    Param {
                        name: Name::str("rhs"),
                        ty: Some(mk_type(Type::Var(Name::str("A")))),
                    },
                ],
                body: None,
                ret: mk_type(Type::Int32),
                constraints: vec![],
                requires: vec![],
                loc: test_loc(),
                arena: ExprArena::new(),
                types: vec![],
                closure_vars: vec![],
                is_extern: false,
            }],
            loc: test_loc(),
        };

        let decl = Decl::Interface(iface);
        let output = decl.pretty_print();
        assert_eq!(
            output,
            "interface Compare<A> {\n    cmp(lhs: A, rhs: A) → i32\n}"
        );
    }

    #[test]
    fn test_pretty_print_global() {
        let decl = Decl::Global {
            name: Name::str("counter"),
            typevars: vec![],
            ty: mk_type(Type::Int32),
        };

        let output = decl.pretty_print();
        assert_eq!(output, "var counter: i32");
    }

    #[test]
    fn test_pretty_print_enum() {
        let decl = Decl::Enum {
            name: Name::str("Status"),
            cases: vec![Name::str("Active"), Name::str("Inactive")],
        };

        let output = decl.pretty_print();
        assert_eq!(output, "enum Status { Active, Inactive }");
    }

    #[test]
    fn test_pretty_print_func_with_block_body() {
        let mut arena = ExprArena::new();
        // Create body: { x + 1 }
        let x_id = arena.add(Expr::Id(Name::str("x")), test_loc());
        let one_id = arena.add(Expr::Int(1), test_loc());
        let add_id = arena.add(Expr::Binop(Binop::Plus, x_id, one_id), test_loc());
        let body_id = arena.add(Expr::Block(vec![add_id]), test_loc());

        let func = FuncDecl {
            name: Name::str("increment"),
            typevars: vec![],
            size_vars: vec![],
            params: vec![Param {
                name: Name::str("x"),
                ty: Some(mk_type(Type::Int32)),
            }],
            body: Some(body_id),
            ret: mk_type(Type::Int32),
            constraints: vec![],
            requires: vec![],
            loc: test_loc(),
            arena,
            types: vec![],
            closure_vars: vec![],
            is_extern: false,
        };

        let decl = Decl::Func(func);
        let output = decl.pretty_print();
        assert_eq!(output, "increment(x: i32) → i32 {\n    x + 1\n}");
    }
}
