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

#[derive(Copy, Clone, Debug, Default)]
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

    errors: Vec<ArrayError>,
}

impl ArrayChecker {
    pub fn new() -> Self {
        Self {
            vars: vec![],
            constraints: vec![],
            errors: vec![],
        }
    }

    fn match_expr(&mut self, expr: ExprID, decl: &FuncDecl, decls: &DeclTable) {

        // Simplest form: match expressions of the form i < n, where n is an integer literal
        if let Expr::Binop(Binop::Less, lhs, rhs) = &decl.arena[expr] {
            if let Expr::Id(name) = &decl.arena[*lhs] {
                let ival = self.check_expr(*rhs, decl, decls);
                if ival.max == ival.min {
                    self.constraints.push(IndexConstraint {
                        name: *name,
                        max: Some(ival.max-1),
                        min: None,
                    })
                }
            }
        }

        if let Expr::Binop(Binop::Geq, lhs, rhs) = &decl.arena[expr] {
            if let Expr::Id(name) = &decl.arena[*lhs] {
                if let Expr::Int(n) = &decl.arena[*rhs] {
                    self.constraints.push(IndexConstraint {
                        name: *name,
                        max: None,
                        min: Some(*n),
                    })
                }
            }
        }

        // match expressions of the form i < id, where id is another variable
        // with a constraint
        if let Expr::Binop(Binop::Less, lhs, rhs) = &decl.arena[expr] {
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

        if let Expr::Binop(Binop::And, lhs, rhs) = &decl.arena[expr] {
            self.match_expr(*lhs, decl, decls);
            self.match_expr(*rhs, decl, decls);
        }

    }

    fn check_expr(&mut self, expr: ExprID, decl: &FuncDecl, decls: &DeclTable) -> IndexInterval {
        match &decl.arena[expr] {
            Expr::Int(x) => {
                IndexInterval { min: *x, max: *x }
            }
            Expr::Block(exprs) => {
                let n = self.vars.len();
                for e in exprs {
                    self.check_expr(*e, decl, decls);
                }
                while self.vars.len() > n {
                    self.vars.pop();
                }
                IndexInterval::default()
            }
            Expr::Let(name, init, _) => {
                self.check_expr(*init, decl, decls);
                let ty = decl.types[expr];
                self.vars.push(Var { name: *name, ty });

                if ty == mk_type(Type::UInt32) {
                    self.constraints.push(IndexConstraint {
                        name: *name,
                        max: None,
                        min: Some(0),
                    })
                }

                IndexInterval::default()
            }
            Expr::Var(name, _, _) => {
                let ty = decl.types[expr];

                if ty == mk_type(Type::UInt32) {
                    self.constraints.push(IndexConstraint {
                        name: *name,
                        max: None,
                        min: Some(0),
                    })
                }
                IndexInterval::default()
            }
            Expr::Id(name) => {
                let mut min = i64::min_value();
                let mut max = i64::max_value();
                for c in &self.constraints {
                    if c.name == *name {
                        if let Some(m) = c.min {
                            min = min.max(m)
                        }
                        if let Some(m) = c.max {
                            max = max.min(m)
                        }
                    }
                }
                IndexInterval { min, max }
            }
            Expr::If(cond, then_expr, else_expr) => {
                let initial_constraint_count = self.constraints.len();

                self.check_expr(*cond, decl, decls);
                self.match_expr(*cond, decl, decls);

                let mut r = self.check_expr(*then_expr, decl, decls);

                if let Some(else_expr) = else_expr {
                    let else_r = self.check_expr(*else_expr, decl, decls);
                    r = enclose(r, else_r);
                }

                // Pop off any constraints not already invalidated by changing data.
                while self.constraints.len() > initial_constraint_count {
                    self.constraints.pop();
                }

                r
            }
            Expr::ArrayIndex(array_expr, index_expr) => {

                if *array_expr >= decl.types.len() {
                    panic!("no type found for array index expression");
                }

                let lhs_ty = decl.types[*array_expr];
                let rhs_r = self.check_expr(*index_expr, decl, decls);

                if rhs_r.min < 0 {
                    self.errors.push(ArrayError {
                        location: decl.arena.locs[expr],
                        message: format!("couldn't prove index is >= 0"),
                    });
                }

                if let Type::Array(_, n) = *lhs_ty {
                    if rhs_r.max >= n.into() {
                        self.errors.push(ArrayError {
                            location: decl.arena.locs[expr],
                            message: format!("couldn't prove index is less than array length"),
                        });
                    }
                } else {
                    panic!("lhs of index expression isn't an array. Should be caught by the type checker.")
                }

                IndexInterval::default()
            }
            Expr::While(cond, body) => {
                
                let initial_constraint_count = self.constraints.len();
                self.match_expr(*cond, decl, decls);

                self.check_expr(*body, decl, decls);
                while self.constraints.len() > initial_constraint_count {
                    self.constraints.pop();
                }

                IndexInterval::default()
            }
            _ => {
                IndexInterval::default()
            }
        }
    }

    fn check_fn_decl(&mut self, func_decl: &FuncDecl, decls: &DeclTable) {

        if let Some(body) = func_decl.body {
            self.check_expr(body, &func_decl, decls);
        }
    }

    fn check_decl(&mut self, decl: &Decl, decls: &DeclTable) {
        match decl {
            Decl::Func(func_decl) => self.check_fn_decl(func_decl, decls),
            Decl::Macro(func_decl) => self.check_fn_decl(func_decl, decls),
            _ => (),
        }
    }

    pub fn check(&mut self, decls: &DeclTable) {
        for decl in &decls.decls {
            self.check_decl(decl, decls);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub fn check(s: &str) -> Vec<ArrayError> {
        let mut errors = vec![];
        let decls = parse_program_str(&s, &mut errors);
        assert!(errors.is_empty());
        assert_eq!(decls.len(), 1);
        let mut table = DeclTable::new(decls);
        let mut types = vec![];
        for decl in &table.decls {
            let mut type_checker = Checker::new();
            type_checker.check_decl(decl, &table);
            assert!(type_checker.errors.is_empty());
            types.push(type_checker.solved_types());
        }

        for i in 0..table.decls.len() {
            if let Decl::Func(ref mut fdecl) = &mut table.decls[i] {
                 fdecl.types = types[i].clone();
            }
        }
        
        let mut array_checker = ArrayChecker::new();
        array_checker.check(&table);

        for err in &array_checker.errors {
            println!(
                "❌ {}:{}: {}",
                err.location.file, err.location.line, err.message
            );
        }

        array_checker.errors
    }

    #[test]
    pub fn test_array_if() {
        
        let s = "
        f(i: i32) {
            var a: [i32; 100]
            if i >= 0 && i < 100 {
                a[i]
            }
        }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_array_if_bad() {
        
        let s = "
        f(i: i32) {
            var a: [i32; 100]
            if i < 100 {
                a[i]
            }
        }
        ";

        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_array_if_u32() {
        
        let s = "
        f {
            var i: u32
            var a: [i32; 100]
            if i < (50 as u32) {
                a[i]
            }
        }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_while() {
        
        let s = "
        f {
            var i: u32
            var a: [i32; 100]
            while i < (50 as u32) {
                a[i]
                // i = i + 1
            }
        }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

}
