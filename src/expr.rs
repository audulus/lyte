use crate::*;

/// Expressions.
///
/// We use indexes instead of boxing. This gives
/// each expression a unique ID which can be used
/// for derived data without mutating the expression
/// tree. It's also faster. Most hierarchical data
/// should be represented this way.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Expr {

    /// Identifier expression.
    Id(Name),

    /// Signed integer literal.
    Int(i64),

    /// Unsigned integer literal.
    UInt(u64),

    /// Floating-point literal as string.
    Real(String), // f64 is not hashable so we just use the string representation

    /// Function call expression with function and arguments.
    Call(ExprID, Vec<ExprID>),

    /// Macro invocation expression.
    Macro(Name, Vec<ExprID>),

    /// Binary operation expression.
    Binop(Binop, ExprID, ExprID),

    /// Unary operation expression.
    Unop(Unop, ExprID),

    /// Lambda expression with parameters and body.
    Lambda {
        params: Vec<Param>,
        body: ExprID,
    },

    /// String literal.
    String(String),

    /// Character literal.
    Char(char),

    /// Type cast expression.
    Cast,

    /// Struct field access.
    Field(ExprID, Name),

    /// Array expression with element type and size.
    Array(ExprID, ExprID),

    /// Array literal expression.
    ArrayLiteral(Vec<ExprID>),

    /// Array indexing expression.
    ArrayIndex(ExprID, ExprID),

    /// Boolean true literal.
    True,

    /// Boolean false literal.
    False,

    /// Type ascription.
    AsTy(ExprID, TypeID),

    /// Variable assignment.
    Assign(Name, ExprID),

    /// Immutable variable declaration with initializer and optional type.
    Let(Name, ExprID, Option<TypeID>),

    /// Mutable variable declaration with optional initializer and type.
    Var(Name, Option<ExprID>, Option<TypeID>),

    /// If expression with optional else branch.
    If(ExprID, ExprID, Option<ExprID>),

    /// While loop expression.
    While(ExprID, ExprID),

    /// For loop expression.
    For {
        var: Name,
        start: ExprID,
        end: ExprID,
        body: ExprID,
    },

    /// Block expression containing a list of expressions.
    Block(Vec<ExprID>),

    /// Return expression.
    Return(ExprID),

    /// Enum variant access.
    Enum(Name),

    /// Tuple expression.
    Tuple(Vec<ExprID>),

    /// Arena allocation expression.
    Arena(ExprID),

    /// Error expression used for recovery.
    Error,
}

impl Expr {
    /// Pretty-print an expression in lyte syntax.
    ///
    /// This method formats an expression as it would appear in lyte source code.
    /// Since expressions can reference other expressions via ExprID, this requires
    /// access to the ExprArena and DeclTable.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let mut arena = ExprArena::new();
    /// let expr_id = arena.add(Expr::Int(42), test_loc());
    /// let output = arena.exprs[expr_id].pretty_print(&arena, &decls, 0);
    /// // Output: "42"
    /// ```
    pub fn pretty_print(
        &self,
        arena: &ExprArena,
        decls: &DeclTable,
        indent: usize,
    ) -> String {
        match self {
            Expr::Id(name) => name.to_string(),
            Expr::Int(n) => n.to_string(),
            Expr::UInt(n) => n.to_string(),
            Expr::Real(s) => s.clone(),
            Expr::String(s) => format!("\"{}\"", s),
            Expr::Char(c) => format!("'{}'", c),
            Expr::True => "true".to_string(),
            Expr::False => "false".to_string(),
            Expr::Enum(name) => format!(".{}", name),
            Expr::Error => "<error>".to_string(),
            Expr::Cast => "<cast>".to_string(),

            Expr::Call(func_id, args) => {
                let func_str = arena.exprs[*func_id].pretty_print(arena, decls, indent);
                let args_str = args
                    .iter()
                    .map(|id| arena.exprs[*id].pretty_print(arena, decls, indent))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", func_str, args_str)
            }

            Expr::Macro(name, args) => {
                let args_str = args
                    .iter()
                    .map(|id| arena.exprs[*id].pretty_print(arena, decls, indent))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("@{}({})", name, args_str)
            }

            Expr::Binop(op, lhs, rhs) => {
                let lhs_str = arena.exprs[*lhs].pretty_print(arena, decls, indent);
                let rhs_str = arena.exprs[*rhs].pretty_print(arena, decls, indent);
                let op_str = format_binop(*op);
                format!("{} {} {}", lhs_str, op_str, rhs_str)
            }

            Expr::Unop(op, expr) => {
                let expr_str = arena.exprs[*expr].pretty_print(arena, decls, indent);
                let op_str = format_unop(*op);
                format!("{}{}", op_str, expr_str)
            }

            Expr::Lambda { params, body } => {
                let params_str = format_lambda_params(params, decls);
                let body_str = arena.exprs[*body].pretty_print(arena, decls, indent);
                // Check if body is a block
                if matches!(arena.exprs[*body], Expr::Block(_)) {
                    format!("|{}| {}", params_str, body_str)
                } else {
                    format!("|{}| {}", params_str, body_str)
                }
            }

            Expr::Field(expr, name) => {
                let expr_str = arena.exprs[*expr].pretty_print(arena, decls, indent);
                format!("{}.{}", expr_str, name)
            }

            Expr::Array(ty_expr, size_expr) => {
                let ty_str = arena.exprs[*ty_expr].pretty_print(arena, decls, indent);
                let size_str = arena.exprs[*size_expr].pretty_print(arena, decls, indent);
                format!("[{}; {}]", ty_str, size_str)
            }

            Expr::ArrayLiteral(exprs) => {
                let exprs_str = exprs
                    .iter()
                    .map(|id| arena.exprs[*id].pretty_print(arena, decls, indent))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", exprs_str)
            }

            Expr::ArrayIndex(arr, idx) => {
                let arr_str = arena.exprs[*arr].pretty_print(arena, decls, indent);
                let idx_str = arena.exprs[*idx].pretty_print(arena, decls, indent);
                format!("{}[{}]", arr_str, idx_str)
            }

            Expr::AsTy(expr, ty) => {
                let expr_str = arena.exprs[*expr].pretty_print(arena, decls, indent);
                let ty_str = ty.pretty_print(decls);
                format!("{}:{}", expr_str, ty_str)
            }

            Expr::Assign(name, expr) => {
                let expr_str = arena.exprs[*expr].pretty_print(arena, decls, indent);
                format!("{} = {}", name, expr_str)
            }

            Expr::Let(name, expr, ty) => {
                let expr_str = arena.exprs[*expr].pretty_print(arena, decls, indent);
                if let Some(ty) = ty {
                    format!("let {}: {} = {}", name, ty.pretty_print(decls), expr_str)
                } else {
                    format!("let {} = {}", name, expr_str)
                }
            }

            Expr::Var(name, init, ty) => {
                let mut result = format!("var {}", name);
                if let Some(ty) = ty {
                    result.push_str(&format!(": {}", ty.pretty_print(decls)));
                }
                if let Some(init_id) = init {
                    let init_str = arena.exprs[*init_id].pretty_print(arena, decls, indent);
                    result.push_str(&format!(" = {}", init_str));
                }
                result
            }

            Expr::If(cond, then_branch, else_branch) => {
                let cond_str = arena.exprs[*cond].pretty_print(arena, decls, indent);
                let then_str = arena.exprs[*then_branch].pretty_print(arena, decls, indent + 1);

                let mut result = format!("if {} {}", cond_str, then_str);

                if let Some(else_id) = else_branch {
                    let else_str = arena.exprs[*else_id].pretty_print(arena, decls, indent + 1);
                    result.push_str(&format!(" else {}", else_str));
                }
                result
            }

            Expr::While(cond, body) => {
                let cond_str = arena.exprs[*cond].pretty_print(arena, decls, indent);
                let body_str = arena.exprs[*body].pretty_print(arena, decls, indent + 1);
                format!("while {} {}", cond_str, body_str)
            }

            Expr::For { var, start, end, body } => {
                let start_str = arena.exprs[*start].pretty_print(arena, decls, indent);
                let end_str = arena.exprs[*end].pretty_print(arena, decls, indent);
                let body_str = arena.exprs[*body].pretty_print(arena, decls, indent + 1);
                format!("for {} in {} .. {} {}", var, start_str, end_str, body_str)
            }

            Expr::Block(exprs) => {
                if exprs.is_empty() {
                    return "{}".to_string();
                }

                let indent_str = "    ".repeat(indent);
                let next_indent_str = "    ".repeat(indent + 1);

                let exprs_str = exprs
                    .iter()
                    .map(|id| {
                        let expr_str = arena.exprs[*id].pretty_print(arena, decls, indent + 1);
                        format!("{}{}", next_indent_str, expr_str)
                    })
                    .collect::<Vec<_>>()
                    .join("\n");

                format!("{{\n{}\n{}}}", exprs_str, indent_str)
            }

            Expr::Return(expr) => {
                let expr_str = arena.exprs[*expr].pretty_print(arena, decls, indent);
                format!("return {}", expr_str)
            }

            Expr::Tuple(exprs) => {
                let exprs_str = exprs
                    .iter()
                    .map(|id| arena.exprs[*id].pretty_print(arena, decls, indent))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", exprs_str)
            }

            Expr::Arena(expr) => {
                let expr_str = arena.exprs[*expr].pretty_print(arena, decls, indent);
                format!("arena {}", expr_str)
            }
        }
    }
}

fn format_binop(op: Binop) -> &'static str {
    match op {
        Binop::Assign => "=",
        Binop::Plus => "+",
        Binop::Minus => "-",
        Binop::Mult => "*",
        Binop::Div => "/",
        Binop::Pow => "^",
        Binop::Equal => "==",
        Binop::NotEqual => "!=",
        Binop::Less => "<",
        Binop::Greater => ">",
        Binop::Leq => "<=",
        Binop::Geq => ">=",
        Binop::And => "&&",
        Binop::Or => "||",
    }
}

fn format_unop(op: Unop) -> &'static str {
    match op {
        Unop::Neg => "-",
        Unop::Not => "!",
    }
}

fn format_lambda_params(params: &[Param], decls: &DeclTable) -> String {
    params
        .iter()
        .map(|p| {
            if let Some(ty) = p.ty {
                format!("{}: {}", p.name, ty.pretty_print(decls))
            } else {
                p.name.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pretty_print_literals() {
        let arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        assert_eq!(Expr::Int(42).pretty_print(&arena, &decls, 0), "42");
        assert_eq!(Expr::UInt(100).pretty_print(&arena, &decls, 0), "100");
        assert_eq!(Expr::Real("3.14".to_string()).pretty_print(&arena, &decls, 0), "3.14");
        assert_eq!(Expr::String("hello".to_string()).pretty_print(&arena, &decls, 0), "\"hello\"");
        assert_eq!(Expr::Char('x').pretty_print(&arena, &decls, 0), "'x'");
        assert_eq!(Expr::True.pretty_print(&arena, &decls, 0), "true");
        assert_eq!(Expr::False.pretty_print(&arena, &decls, 0), "false");
    }

    #[test]
    fn test_pretty_print_id() {
        let arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        assert_eq!(Expr::Id(Name::str("foo")).pretty_print(&arena, &decls, 0), "foo");
        assert_eq!(Expr::Enum(Name::str("Active")).pretty_print(&arena, &decls, 0), ".Active");
    }

    #[test]
    fn test_pretty_print_binop() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        let lhs_id = arena.add(Expr::Int(1), test_loc());
        let rhs_id = arena.add(Expr::Int(2), test_loc());

        let add_expr = Expr::Binop(Binop::Plus, lhs_id, rhs_id);
        assert_eq!(add_expr.pretty_print(&arena, &decls, 0), "1 + 2");

        let eq_expr = Expr::Binop(Binop::Equal, lhs_id, rhs_id);
        assert_eq!(eq_expr.pretty_print(&arena, &decls, 0), "1 == 2");

        let mult_expr = Expr::Binop(Binop::Mult, lhs_id, rhs_id);
        assert_eq!(mult_expr.pretty_print(&arena, &decls, 0), "1 * 2");
    }

    #[test]
    fn test_pretty_print_unop() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        let expr_id = arena.add(Expr::Int(42), test_loc());

        let neg_expr = Expr::Unop(Unop::Neg, expr_id);
        assert_eq!(neg_expr.pretty_print(&arena, &decls, 0), "-42");

        let bool_id = arena.add(Expr::True, test_loc());
        let not_expr = Expr::Unop(Unop::Not, bool_id);
        assert_eq!(not_expr.pretty_print(&arena, &decls, 0), "!true");
    }

    #[test]
    fn test_pretty_print_call() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        let func_id = arena.add(Expr::Id(Name::str("foo")), test_loc());
        let arg1_id = arena.add(Expr::Int(1), test_loc());
        let arg2_id = arena.add(Expr::Int(2), test_loc());

        let call_expr = Expr::Call(func_id, vec![arg1_id, arg2_id]);
        assert_eq!(call_expr.pretty_print(&arena, &decls, 0), "foo(1, 2)");

        let no_arg_call = Expr::Call(func_id, vec![]);
        assert_eq!(no_arg_call.pretty_print(&arena, &decls, 0), "foo()");
    }

    #[test]
    fn test_pretty_print_array_literal() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        let elem1 = arena.add(Expr::Int(1), test_loc());
        let elem2 = arena.add(Expr::Int(2), test_loc());
        let elem3 = arena.add(Expr::Int(3), test_loc());

        let arr_expr = Expr::ArrayLiteral(vec![elem1, elem2, elem3]);
        assert_eq!(arr_expr.pretty_print(&arena, &decls, 0), "[1, 2, 3]");
    }

    #[test]
    fn test_pretty_print_array_index() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        let arr_id = arena.add(Expr::Id(Name::str("arr")), test_loc());
        let idx_id = arena.add(Expr::Int(0), test_loc());

        let index_expr = Expr::ArrayIndex(arr_id, idx_id);
        assert_eq!(index_expr.pretty_print(&arena, &decls, 0), "arr[0]");
    }

    #[test]
    fn test_pretty_print_field() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        let obj_id = arena.add(Expr::Id(Name::str("point")), test_loc());
        let field_expr = Expr::Field(obj_id, Name::str("x"));

        assert_eq!(field_expr.pretty_print(&arena, &decls, 0), "point.x");
    }

    #[test]
    fn test_pretty_print_var() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        // var x
        let var1 = Expr::Var(Name::str("x"), None, None);
        assert_eq!(var1.pretty_print(&arena, &decls, 0), "var x");

        // var x: i32
        let var2 = Expr::Var(Name::str("x"), None, Some(mk_type(Type::Int32)));
        assert_eq!(var2.pretty_print(&arena, &decls, 0), "var x: i32");

        // var x = 42
        let init_id = arena.add(Expr::Int(42), test_loc());
        let var3 = Expr::Var(Name::str("x"), Some(init_id), None);
        assert_eq!(var3.pretty_print(&arena, &decls, 0), "var x = 42");

        // var x: i32 = 42
        let var4 = Expr::Var(Name::str("x"), Some(init_id), Some(mk_type(Type::Int32)));
        assert_eq!(var4.pretty_print(&arena, &decls, 0), "var x: i32 = 42");
    }

    #[test]
    fn test_pretty_print_let() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        let value_id = arena.add(Expr::Int(42), test_loc());

        // let x = 42
        let let1 = Expr::Let(Name::str("x"), value_id, None);
        assert_eq!(let1.pretty_print(&arena, &decls, 0), "let x = 42");

        // let x: i32 = 42
        let let2 = Expr::Let(Name::str("x"), value_id, Some(mk_type(Type::Int32)));
        assert_eq!(let2.pretty_print(&arena, &decls, 0), "let x: i32 = 42");
    }

    #[test]
    fn test_pretty_print_assign() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        let value_id = arena.add(Expr::Int(42), test_loc());
        let assign_expr = Expr::Assign(Name::str("x"), value_id);

        assert_eq!(assign_expr.pretty_print(&arena, &decls, 0), "x = 42");
    }

    #[test]
    fn test_pretty_print_lambda() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        // |x| x + 1
        let x_id = arena.add(Expr::Id(Name::str("x")), test_loc());
        let one_id = arena.add(Expr::Int(1), test_loc());
        let body_id = arena.add(Expr::Binop(Binop::Plus, x_id, one_id), test_loc());

        let lambda = Expr::Lambda {
            params: vec![Param { name: Name::str("x"), ty: None }],
            body: body_id,
        };

        assert_eq!(lambda.pretty_print(&arena, &decls, 0), "|x| x + 1");
    }

    #[test]
    fn test_pretty_print_if() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        let cond_id = arena.add(Expr::True, test_loc());
        let then_id = arena.add(Expr::Int(1), test_loc());
        let else_id = arena.add(Expr::Int(2), test_loc());

        // if true 1 else 2 (simplified without blocks)
        let if_expr = Expr::If(cond_id, then_id, Some(else_id));
        assert_eq!(if_expr.pretty_print(&arena, &decls, 0), "if true 1 else 2");

        // if true 1 (no else)
        let if_no_else = Expr::If(cond_id, then_id, None);
        assert_eq!(if_no_else.pretty_print(&arena, &decls, 0), "if true 1");
    }

    #[test]
    fn test_pretty_print_while() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        let cond_id = arena.add(Expr::True, test_loc());
        let body_id = arena.add(Expr::Block(vec![]), test_loc());

        let while_expr = Expr::While(cond_id, body_id);
        assert_eq!(while_expr.pretty_print(&arena, &decls, 0), "while true {}");
    }

    #[test]
    fn test_pretty_print_for() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        let start_id = arena.add(Expr::Int(0), test_loc());
        let end_id = arena.add(Expr::Int(10), test_loc());
        let body_id = arena.add(Expr::Block(vec![]), test_loc());

        let for_expr = Expr::For {
            var: Name::str("i"),
            start: start_id,
            end: end_id,
            body: body_id,
        };

        assert_eq!(for_expr.pretty_print(&arena, &decls, 0), "for i in 0 .. 10 {}");
    }

    #[test]
    fn test_pretty_print_block() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        // Empty block
        let empty_block = Expr::Block(vec![]);
        assert_eq!(empty_block.pretty_print(&arena, &decls, 0), "{}");

        // Block with expressions
        let expr1_id = arena.add(Expr::Int(1), test_loc());
        let expr2_id = arena.add(Expr::Int(2), test_loc());
        let block = Expr::Block(vec![expr1_id, expr2_id]);

        assert_eq!(block.pretty_print(&arena, &decls, 0), "{\n    1\n    2\n}");
    }

    #[test]
    fn test_pretty_print_return() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        let value_id = arena.add(Expr::Int(42), test_loc());
        let return_expr = Expr::Return(value_id);

        assert_eq!(return_expr.pretty_print(&arena, &decls, 0), "return 42");
    }

    #[test]
    fn test_pretty_print_tuple() {
        let mut arena = ExprArena::new();
        let decls = DeclTable::new(vec![]);

        let elem1 = arena.add(Expr::Int(1), test_loc());
        let elem2 = arena.add(Expr::String("hello".to_string()), test_loc());

        let tuple_expr = Expr::Tuple(vec![elem1, elem2]);
        assert_eq!(tuple_expr.pretty_print(&arena, &decls, 0), "(1, \"hello\")");
    }
}
