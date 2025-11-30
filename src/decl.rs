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

/// Function declaration.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FuncDecl {
    pub name: Name,
    pub typevars: Vec<Name>,
    pub params: Vec<Param>,
    pub body: Option<ExprID>,
    pub ret: TypeID,
    pub constraints: Vec<InterfaceConstraint>,
    pub loc: Loc,
    pub arena: ExprArena,
    pub types: Vec<TypeID>,
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
    Enum { name: Name, cases: Vec<Name> },
    Global { name: Name, ty: TypeID },
    Interface(Interface),
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
    pub fn pretty_print(&self, decls: &DeclTable) -> String {
        match self {
            Decl::Func(func) => format_func_decl(func, decls, false),
            Decl::Macro(func) => format_func_decl(func, decls, true),
            Decl::Struct(st) => format_struct_decl(st, decls),
            Decl::Enum { name, cases } => format_enum_decl(*name, cases),
            Decl::Global { name, ty } => format!("var {}: {}", name, format_type(*ty, decls)),
            Decl::Interface(iface) => format_interface(iface, decls),
        }
    }
}

fn format_typevars(typevars: &[Name]) -> String {
    if typevars.is_empty() {
        String::new()
    } else {
        format!("<{}>", typevars.iter().map(|tv| tv.to_string()).collect::<Vec<_>>().join(", "))
    }
}

fn format_params(params: &[Param], decls: &DeclTable) -> String {
    params
        .iter()
        .map(|p| {
            if let Some(ty) = p.ty {
                format!("{}: {}", p.name, format_type(ty, decls))
            } else {
                p.name.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_type(ty: TypeID, decls: &DeclTable) -> String {
    match &*ty {
        Type::Void => "void".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Int8 => "i8".to_string(),
        Type::UInt8 => "u8".to_string(),
        Type::Int32 => "i32".to_string(),
        Type::UInt32 => "u32".to_string(),
        Type::Float32 => "f32".to_string(),
        Type::Float64 => "f64".to_string(),
        Type::Tuple(types) => {
            format!("({})", types.iter().map(|t| format_type(*t, decls)).collect::<Vec<_>>().join(", "))
        }
        Type::Var(name) => name.to_string(),
        Type::Anon(id) => format!("?{}", id),
        Type::Func(dom, rng) => format!("{} → {}", format_type(*dom, decls), format_type(*rng, decls)),
        Type::Array(elem, size) => format!("[{}; {}]", format_type(*elem, decls), size),
        Type::Name(name, params) => {
            if params.is_empty() {
                name.to_string()
            } else {
                format!("{}<{}>", name, params.iter().map(|t| format_type(*t, decls)).collect::<Vec<_>>().join(", "))
            }
        }
    }
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
                    format!("{}<{}>", c.interface_name, c.typevars.iter().map(|tv| tv.to_string()).collect::<Vec<_>>().join(", "))
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!(" where {}", formatted)
    }
}

fn format_func_decl(func: &FuncDecl, decls: &DeclTable, is_macro: bool) -> String {
    let keyword = if is_macro { "macro" } else { "" };
    let typevars = format_typevars(&func.typevars);
    let params = format_params(&func.params, decls);
    let ret_type = if matches!(&*func.ret, Type::Void) {
        String::new()
    } else {
        format!(" → {}", format_type(func.ret, decls))
    };
    let constraints = format_constraints(&func.constraints);

    let signature = if is_macro {
        format!("macro {}{}", func.name, typevars)
    } else if keyword.is_empty() {
        format!("{}{}", func.name, typevars)
    } else {
        format!("{} {}{}", keyword, func.name, typevars)
    };

    if func.body.is_some() {
        format!("{}({}){}{} {{ ... }}", signature, params, ret_type, constraints)
    } else {
        format!("{}({}){}{}", signature, params, ret_type, constraints)
    }
}

fn format_struct_decl(st: &StructDecl, decls: &DeclTable) -> String {
    let typevars = format_typevars(&st.typevars);
    let fields = st
        .fields
        .iter()
        .map(|f| format!("    {}: {}", f.name, format_type(f.ty, decls)))
        .collect::<Vec<_>>()
        .join("\n");

    if fields.is_empty() {
        format!("struct {}{} {{}}", st.name, typevars)
    } else {
        format!("struct {}{} {{\n{}\n}}", st.name, typevars, fields)
    }
}

fn format_enum_decl(name: Name, cases: &[Name]) -> String {
    let cases_str = cases
        .iter()
        .map(|c| format!("    {}", c))
        .collect::<Vec<_>>()
        .join("\n");

    if cases.is_empty() {
        format!("enum {} {{}}", name)
    } else {
        format!("enum {} {{\n{}\n}}", name, cases_str)
    }
}

fn format_interface(iface: &Interface, decls: &DeclTable) -> String {
    let typevars = format_typevars(&iface.typevars);
    let funcs = iface
        .funcs
        .iter()
        .map(|f| format!("    {}", format_func_decl(f, decls, false)))
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
        let decls = DeclTable::new(vec![]);

        let func = FuncDecl {
            name: Name::str("add"),
            typevars: vec![],
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
            loc: test_loc(),
            arena: ExprArena::new(),
            types: vec![],
        };

        let decl = Decl::Func(func);
        let output = decl.pretty_print(&decls);
        assert_eq!(output, "add(a: i32, b: i32) → i32");
    }

    #[test]
    fn test_pretty_print_generic_func() {
        let decls = DeclTable::new(vec![]);

        let func = FuncDecl {
            name: Name::str("id"),
            typevars: vec![Name::str("T")],
            params: vec![Param {
                name: Name::str("x"),
                ty: Some(mk_type(Type::Var(Name::str("T")))),
            }],
            body: Some(0),
            ret: mk_type(Type::Var(Name::str("T"))),
            constraints: vec![],
            loc: test_loc(),
            arena: ExprArena::new(),
            types: vec![],
        };

        let decl = Decl::Func(func);
        let output = decl.pretty_print(&decls);
        assert_eq!(output, "id<T>(x: T) → T { ... }");
    }

    #[test]
    fn test_pretty_print_struct() {
        let decls = DeclTable::new(vec![]);

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
        let output = decl.pretty_print(&decls);
        assert_eq!(output, "struct Point {\n    x: f32\n    y: f32\n}");
    }

    #[test]
    fn test_pretty_print_interface() {
        let decls = DeclTable::new(vec![]);

        let iface = Interface {
            name: Name::str("Compare"),
            typevars: vec![Name::str("A")],
            funcs: vec![FuncDecl {
                name: Name::str("cmp"),
                typevars: vec![],
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
                loc: test_loc(),
                arena: ExprArena::new(),
                types: vec![],
            }],
            loc: test_loc(),
        };

        let decl = Decl::Interface(iface);
        let output = decl.pretty_print(&decls);
        assert_eq!(output, "interface Compare<A> {\n    cmp(lhs: A, rhs: A) → i32\n}");
    }

    #[test]
    fn test_pretty_print_global() {
        let decls = DeclTable::new(vec![]);

        let decl = Decl::Global {
            name: Name::str("counter"),
            ty: mk_type(Type::Int32),
        };

        let output = decl.pretty_print(&decls);
        assert_eq!(output, "var counter: i32");
    }

    #[test]
    fn test_pretty_print_enum() {
        let decls = DeclTable::new(vec![]);

        let decl = Decl::Enum {
            name: Name::str("Status"),
            cases: vec![Name::str("Active"), Name::str("Inactive")],
        };

        let output = decl.pretty_print(&decls);
        assert_eq!(output, "enum Status {\n    Active\n    Inactive\n}");
    }
}
