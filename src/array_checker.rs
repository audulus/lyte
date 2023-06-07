use crate::*;

pub struct ArrayError {
    pub location: Loc,
    pub message: String,
}

struct IndexConstraint {
    pub name: Name,
    pub max: Option<i64>,
    pub min: Option<i64>,
}

/// Local variable declaration.
#[derive(Copy, Clone, Debug)]
struct Var {
    name: Name,
    ty: TypeID,
}

pub struct ArrayChecker {
    /// Currently declared vars, as we're checking.
    vars: Vec<Var>,

    /// Constraints we know about each var.
    constraints: Vec<IndexConstraint>,
}

impl ArrayChecker {
    pub fn new() -> Self {
        Self {
            vars: vec![],
            constraints: vec![],
        }
    }

    fn check_expr(&mut self, expr: ExprID, decl: &FuncDecl, decls: &DeclTable) {
        match &decl.arena[expr] {
            Expr::Let(name, init, _) => {
                self.check_expr(*init, decl, decls);
                let ty = decl.types[expr];
                self.vars.push(Var { name: *name, ty });
            }
            Expr::If(cond, then_expr, else_expr) => {
                let initial_constraint_count = self.constraints.len();

                self.check_expr(*cond, decl, decls);

                // Simplest form: match expressions of the form i < n, where n is an integer literal
                if let Expr::Binop(Binop::Less, lhs, rhs) = &decl.arena[*cond] {
                    if let Expr::Id(name) = &decl.arena[*lhs] {
                        if let Expr::Int(n) = &decl.arena[*rhs] {
                            self.constraints.push(IndexConstraint {
                                name: *name,
                                max: Some(*n),
                                min: None,
                            })
                        }
                    }
                }

                self.check_expr(*then_expr, decl, decls);

                if let Some(else_expr) = else_expr {
                    self.check_expr(*else_expr, decl, decls);
                }

                // Pop off any constraints not alreayd invalidated by changing data.
                while self.constraints.len() > initial_constraint_count {
                    self.constraints.pop();
                }
            }
            _ => {
                todo!()
            }
        }
    }
}
