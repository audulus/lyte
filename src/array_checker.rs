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

#[derive(Copy, Clone, Debug)]
struct IndexInterval {
    pub min: i64,
    pub max: i64,
}

fn enclose(a: IndexInterval, b: IndexInterval) -> IndexInterval {
    IndexInterval {
        min: a.min.min(b.min),
        max: a.max.max(b.max),
    }
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

    fn check_expr(&mut self, expr: ExprID, decl: &FuncDecl, decls: &DeclTable) -> IndexInterval {
        match &decl.arena[expr] {
            Expr::Let(name, init, _) => {
                self.check_expr(*init, decl, decls);
                let ty = decl.types[expr];
                self.vars.push(Var { name: *name, ty });
                IndexInterval { min: 0, max: 0 }
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

                // match expressions of the form i < id, where id is another variable
                // with a constraint
                if let Expr::Binop(Binop::Less, lhs, rhs) = &decl.arena[*cond] {
                    if let Expr::Id(name) = &decl.arena[*lhs] {
                        if let Expr::Id(max_name) = &decl.arena[*rhs] {
                            if let Some(c) = self.constraints.iter().find(|c| c.name == *max_name) {
                                if let Some(max) = c.max {
                                    self.constraints.push(IndexConstraint {
                                        name: *name,
                                        max: Some(max),
                                        min: None,
                                    })
                                }
                            }
                        }
                    }
                }

                let mut r = self.check_expr(*then_expr, decl, decls);

                if let Some(else_expr) = else_expr {
                    let else_r = self.check_expr(*else_expr, decl, decls);
                    r = enclose(r, else_r);
                }

                // Pop off any constraints not alreayd invalidated by changing data.
                while self.constraints.len() > initial_constraint_count {
                    self.constraints.pop();
                }

                r
            }
            _ => {
                todo!()
            }
        }
    }
}
