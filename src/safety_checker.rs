use crate::interval::{enclose, IndexInterval};
use crate::*;

pub struct SafetyError {
    pub location: Loc,
    pub message: String,
}

#[derive(Clone, Debug)]
struct IndexConstraint {
    pub name: Name,
    pub min: Option<i64>,
    pub max: Option<i64>,
    pub non_zero: bool,
}

/// Records that variable `index` has been proven < `array.len`.
#[derive(Clone, Debug)]
struct LenBound {
    pub index: Name,
    pub array: Name,
}

/// Local variable declaration.
#[derive(Copy, Clone, Debug)]
struct Var {
    name: Name,
    ty: TypeID,
}

/// Static safety checker using abstract interpretation.
///
/// `SafetyChecker` performs compile-time verification that operations which could
/// trap or cause undefined behavior at runtime are provably safe. Currently checks:
/// - **Array bounds**: array indices are within `[0, length)`
/// - **Division by zero**: integer divisors are provably non-zero
///
/// It uses interval arithmetic to track the possible range of integer values and propagates
/// constraints from conditionals (e.g., `if i < n`, `if b != 0`) to prove safety.
///
/// # How It Works
///
/// The checker maintains a set of constraints on variable values as it traverses the AST.
/// Constraints are derived from:
/// - **Conditionals**: `if i < 100` adds `i.max = 99`; `if b != 0` marks `b` as non-zero
/// - **Type information**: `u32` variables are known to be `>= 0`
/// - **Assignments**: `i = 5` updates the interval to `[5, 5]`
/// - **Arithmetic**: intervals are propagated through `+`, `-`, and `*` operations
///
/// # Limitations
///
/// - Only tracks `+`, `-`, and `*` for arithmetic
/// - Constraints from while loop conditions don't persist after mutation
/// - Complex expressions may result in unconstrained intervals
pub struct SafetyChecker {
    /// Currently declared vars, as we're checking.
    vars: Vec<Var>,

    /// Constraints we know about each var.
    constraints: Vec<IndexConstraint>,

    /// Symbolic length bounds: records that `index < array.len`.
    len_bounds: Vec<LenBound>,

    pub errors: Vec<SafetyError>,
}

impl SafetyChecker {
    pub fn new() -> Self {
        Self {
            vars: vec![],
            constraints: vec![],
            len_bounds: vec![],
            errors: vec![],
        }
    }

    fn add(&mut self, name: Name, min: Option<i64>, max: Option<i64>) {
        self.constraints.push(IndexConstraint { name, min, max, non_zero: false })
    }

    fn replace(&mut self, name: Name, min: Option<i64>, max: Option<i64>) {
        self.constraints.retain(|c| c.name != name);
        self.add(name, min, max);
    }

    fn add_non_zero(&mut self, name: Name) {
        // If there's already a constraint, mark it non_zero.
        // Otherwise, add an unconstrained entry with non_zero set.
        if let Some(c) = self.constraints.iter_mut().find(|c| c.name == name) {
            c.non_zero = true;
        } else {
            self.constraints.push(IndexConstraint { name, min: None, max: None, non_zero: true });
        }
    }

    fn find(&self, name: Name) -> Option<IndexConstraint> {
        self.constraints.iter().find(|c| c.name == name).cloned()
    }

    /// Given expr evaluates to true, add constraints accordingly.
    fn match_expr(&mut self, expr: ExprID, decl: &FuncDecl, decls: &DeclTable) {
        // Simplest form: match expressions of the form i < n, where n is an integer literal
        if let Expr::Binop(Binop::Less, lhs, rhs) = &decl.arena[expr] {
            if let Expr::Id(name) = &decl.arena[*lhs] {
                let ival = self.check_expr(*rhs, decl, decls);
                if ival.max != i64::max_value() {
                    self.add(*name, None, Some(ival.max - 1));
                }
            }
        }

        if let Expr::Binop(Binop::Leq, lhs, rhs) = &decl.arena[expr] {
            if let Expr::Id(name) = &decl.arena[*lhs] {
                let ival = self.check_expr(*rhs, decl, decls);
                if ival.max != i64::max_value() {
                    self.add(*name, None, Some(ival.max));
                }
            }
        }

        if let Expr::Binop(Binop::Geq, lhs, rhs) = &decl.arena[expr] {
            if let Expr::Id(name) = &decl.arena[*lhs] {
                if let Expr::Int(n) = &decl.arena[*rhs] {
                    self.add(*name, Some(*n), None);
                }
            }
        }

        // match expressions of the form i < id, where id is another variable
        // with a constraint
        if let Expr::Binop(Binop::Less, lhs, rhs) = &decl.arena[expr] {
            if let Expr::Id(name) = &decl.arena[*lhs] {
                if let Expr::Id(max_name) = &decl.arena[*rhs] {
                    if let Some(c) = self.find(*max_name) {
                        if let Some(max) = c.max {
                            self.add(*name, None, Some(max));
                        }
                    }
                }
                // match i < array.len — record symbolic length bound
                if let Expr::Field(arr_expr, field_name) = &decl.arena[*rhs] {
                    if field_name.as_str() == "len" {
                        if let Expr::Id(array_name) = &decl.arena[*arr_expr] {
                            self.len_bounds.push(LenBound {
                                index: *name,
                                array: *array_name,
                            });
                        }
                    }
                }
            }
        }

        // match `x != 0` — mark x as non-zero
        if let Expr::Binop(Binop::NotEqual, lhs, rhs) = &decl.arena[expr] {
            if let Expr::Id(name) = &decl.arena[*lhs] {
                if let Expr::Int(0) | Expr::UInt(0) = &decl.arena[*rhs] {
                    self.add_non_zero(*name);
                }
            }
            if let Expr::Id(name) = &decl.arena[*rhs] {
                if let Expr::Int(0) | Expr::UInt(0) = &decl.arena[*lhs] {
                    self.add_non_zero(*name);
                }
            }
        }

        // match `x > 0` — min is 1, which also implies non-zero
        if let Expr::Binop(Binop::Greater, lhs, rhs) = &decl.arena[expr] {
            if let Expr::Id(name) = &decl.arena[*lhs] {
                if let Expr::Int(0) | Expr::UInt(0) = &decl.arena[*rhs] {
                    self.add(*name, Some(1), None);
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
            Expr::Int(x) => IndexInterval { min: *x, max: *x, non_zero: *x != 0 },
            Expr::UInt(x) => IndexInterval { min: *x as i64, max: *x as i64, non_zero: *x != 0 },
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
                    self.add(*name, Some(0), None);
                }

                IndexInterval::default()
            }
            Expr::Var(name, init, _) => {
                if let Some(init) = init {
                    self.check_expr(*init, decl, decls);
                }
                let ty = decl.types[expr];

                if ty == mk_type(Type::UInt32) {
                    self.add(*name, Some(0), None);
                }
                IndexInterval::default()
            }
            Expr::Id(name) => {
                let mut min = i64::min_value();
                let mut max = i64::max_value();
                let mut non_zero = false;
                for c in &self.constraints {
                    if c.name == *name {
                        if let Some(m) = c.min {
                            min = min.max(m)
                        }
                        if let Some(m) = c.max {
                            max = max.min(m)
                        }
                        if c.non_zero {
                            non_zero = true;
                        }
                    }
                }
                IndexInterval { min, max, non_zero }
            }
            Expr::If(cond, then_expr, else_expr) => {
                let initial_constraint_count = self.constraints.len();
                let initial_len_bound_count = self.len_bounds.len();

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
                self.len_bounds.truncate(initial_len_bound_count);

                r
            }
            Expr::ArrayIndex(array_expr, index_expr) => {
                if *array_expr >= decl.types.len() {
                    panic!("no type found for array index expression");
                }

                self.check_expr(*array_expr, decl, decls);
                let lhs_ty = decl.types[*array_expr];
                let rhs_r = self.check_expr(*index_expr, decl, decls);

                if rhs_r.min < 0 {
                    self.errors.push(SafetyError {
                        location: decl.arena.locs[expr],
                        message: format!("couldn't prove index is >= 0"),
                    });
                }

                if let Type::Array(_, ref n) = *lhs_ty {
                    if let ArraySize::Known(n) = n {
                        if *n > 0 && rhs_r.max >= (*n).into() {
                            self.errors.push(SafetyError {
                                location: decl.arena.locs[expr],
                                message: format!("couldn't prove index is less than array length"),
                            });
                        }
                    }
                    // Size variables can't be checked statically.
                } else if let Type::Slice(_) = *lhs_ty {
                    // For slices, check if the index has been proven < slice.len.
                    let array_name = if let Expr::Id(name) = &decl.arena[*array_expr] {
                        Some(*name)
                    } else {
                        None
                    };
                    let index_name = if let Expr::Id(name) = &decl.arena[*index_expr] {
                        Some(*name)
                    } else {
                        None
                    };
                    let has_len_bound = match (index_name, array_name) {
                        (Some(idx), Some(arr)) => {
                            self.len_bounds.iter().any(|b| b.index == idx && b.array == arr)
                        }
                        _ => false,
                    };
                    if !has_len_bound {
                        self.errors.push(SafetyError {
                            location: decl.arena.locs[expr],
                            message: format!("couldn't prove index is less than slice length"),
                        });
                    }
                }

                IndexInterval::default()
            }
            Expr::While(cond, body) => {
                let saved_constraints = self.constraints.clone();
                let saved_len_bounds = self.len_bounds.clone();
                self.match_expr(*cond, decl, decls);

                self.check_expr(*body, decl, decls);
                self.constraints = saved_constraints;
                self.len_bounds = saved_len_bounds;

                IndexInterval::default()
            }
            Expr::Binop(op, lhs, rhs) => {

                if *op == Binop::Plus {
                    let lhs_range = self.check_expr(*lhs, decl, decls);
                    let rhs_range = self.check_expr(*rhs, decl, decls);
                    return lhs_range + rhs_range
                }

                if *op == Binop::Minus {
                    let lhs_range = self.check_expr(*lhs, decl, decls);
                    let rhs_range = self.check_expr(*rhs, decl, decls);
                    return lhs_range - rhs_range
                }

                if *op == Binop::Mult {
                    let lhs_range = self.check_expr(*lhs, decl, decls);
                    let rhs_range = self.check_expr(*rhs, decl, decls);
                    return lhs_range * rhs_range
                }

                if *op == Binop::Div || *op == Binop::Mod {
                    let lhs_range = self.check_expr(*lhs, decl, decls);
                    let rhs_range = self.check_expr(*rhs, decl, decls);

                    // Only check integer division — float div-by-zero produces Inf/NaN per IEEE 754.
                    if *rhs < decl.types.len() {
                        let ty = decl.types[*rhs];
                        let is_int = matches!(*ty, Type::Int32 | Type::UInt32 | Type::Int8 | Type::UInt8);
                        if is_int && !rhs_range.excludes_zero() {
                            self.errors.push(SafetyError {
                                location: decl.arena.locs[expr],
                                message: format!("couldn't prove divisor is non-zero"),
                            });
                        }
                    }

                    return lhs_range // approximate: ignore division for interval tracking
                }

                if *op == Binop::Assign {
                    self.check_expr(*lhs, decl, decls);
                    let rhs_range = self.check_expr(*rhs, decl, decls);

                    if rhs_range != IndexInterval::default() {
                        if let Expr::Id(name) = &decl.arena[*lhs] {
                            self.replace(*name, Some(rhs_range.min), Some(rhs_range.max));
                        }
                    }

                    return IndexInterval::default()
                }

                // For other binops (==, !=, <, >, etc.), still recurse
                // into sub-expressions to check array accesses.
                self.check_expr(*lhs, decl, decls);
                self.check_expr(*rhs, decl, decls);
                IndexInterval::default()
            }
            Expr::Call(_, args) => {
                for arg in args {
                    self.check_expr(*arg, decl, decls);
                }
                IndexInterval::default()
            }
            Expr::Unop(_, expr) => {
                self.check_expr(*expr, decl, decls);
                IndexInterval::default()
            }
            Expr::Return(expr) => {
                self.check_expr(*expr, decl, decls);
                IndexInterval::default()
            }
            Expr::Field(expr, _) => {
                self.check_expr(*expr, decl, decls);
                IndexInterval::default()
            }
            Expr::For { var, start, end, body } => {
                let start_r = self.check_expr(*start, decl, decls);
                let end_r = self.check_expr(*end, decl, decls);
                self.vars.push(Var { name: *var, ty: mk_type(Type::Int32) });
                self.add(*var, Some(start_r.min), Some(end_r.max.saturating_sub(1)));
                // for i in 0 .. arr.len — record that i < arr.len
                if let Expr::Field(arr_expr, field_name) = &decl.arena[*end] {
                    if field_name.as_str() == "len" {
                        if let Expr::Id(array_name) = &decl.arena[*arr_expr] {
                            self.len_bounds.push(LenBound {
                                index: *var,
                                array: *array_name,
                            });
                        }
                    }
                }
                self.check_expr(*body, decl, decls);
                IndexInterval::default()
            }
            Expr::ArrayLiteral(exprs) => {
                for e in exprs {
                    self.check_expr(*e, decl, decls);
                }
                IndexInterval::default()
            }
            _ => IndexInterval::default(),
        }
    }

    fn check_fn_decl(&mut self, func_decl: &FuncDecl, decls: &DeclTable) {
        if let Some(body) = func_decl.body {

            for param in &func_decl.params {
                if let Some(ty) = param.ty {
                    self.vars.push(Var { name: param.name, ty });
                    if ty == mk_type(Type::UInt32) {
                        self.add(param.name, Some(0), None);
                    } else {
                        self.add(param.name, None, None);
                    }
                }
            }

            self.check_expr(body, &func_decl, decls);

            self.vars.clear();
            self.constraints.clear();
            self.len_bounds.clear();
        }
    }

    fn check_decl(&mut self, decl: &Decl, decls: &DeclTable) {
        match decl {
            Decl::Func(func_decl) => {
                // Skip generic functions with size variables — they'll be
                // checked after monomorphization when sizes are concrete.
                if func_decl.size_vars.is_empty() {
                    self.check_fn_decl(func_decl, decls);
                }
            }
            // Macros are untyped templates; skip them.
            Decl::Macro(_) => (),
            _ => (),
        }
    }

    pub fn check(&mut self, decls: &DeclTable) {
        for decl in &decls.decls {
            self.check_decl(decl, decls);
        }
    }

    pub fn print_errors(&self) {
        for err in &self.errors {
            print_error_with_context(err.location, &err.message);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub fn check(s: &str) -> Vec<SafetyError> {
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

        let mut array_checker = SafetyChecker::new();
        array_checker.check(&table);

        array_checker.print_errors();

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
    pub fn test_array_if_mutate_bad() {
        let s = "
        f(x: i32) {
            var i = x
            var a: [i32; 100]
            if i >= 0 && i < 100 {
                a[i]
                i = i + 1
                a[i]
            }
        }
        ";

        let errors = check(s);
        assert_eq!(errors.len(), 1);
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
        // i could be negative!
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_array_if_leq_bad() {
        let s = "
        f(i : u32) {
            var a: [i32; 100]
            if i <= 100u {
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
            if i < 50u {
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
            while i < 50u {
                a[i]
                i = i + 1u
            }
        }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_while_mutate_bad() {
        let s = "
        f {
            var i: u32
            var a: [i32; 50]
            while i < 50u {
                a[i]
                i = i + 1u
                a[i]
            }
        }
        ";

        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }
 
    #[test]
    pub fn test_add_unconstrained() {
        let s = "
        f(x: i32) → i32 { x + 1 }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_subtraction_bounds() {
        // Test that n - 1 where n is in [1, 100] gives [0, 99]
        let s = "
        f(n: i32) {
            var a: [i32; 100]
            if n >= 1 && n <= 100 {
                a[n - 1]
            }
        }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_multiplication_bounds() {
        // Test that i * 2 where i is in [0, 49] gives [0, 98]
        let s = "
        f(i: i32) {
            var a: [i32; 100]
            if i >= 0 && i < 50 {
                a[i * 2]
            }
        }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_slice_unbounded() {
        let s = "
        f(s: [i32]) {
            s[0]
        }
        ";

        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_slice_if_bounded() {
        let s = "
        f(s: [i32]) {
            var i = 0
            if i >= 0 && i < s.len {
                s[i]
            }
        }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_slice_for_bounded() {
        let s = "
        f(s: [i32]) {
            for i in 0 .. s.len {
                s[i]
            }
        }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_slice_wrong_len_bound() {
        // i is bounded by a.len, not b.len
        let s = "
        f(a: [i32], b: [i32]) {
            for i in 0 .. a.len {
                b[i]
            }
        }
        ";

        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_div_by_zero_unconstrained() {
        let s = "
        f(a: i32, b: i32) → i32 { a / b }
        ";

        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_div_by_literal_nonzero() {
        let s = "
        f(a: i32) → i32 { a / 2 }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_div_guarded_neq_zero() {
        let s = "
        f(a: i32, b: i32) → i32 {
            if b != 0 {
                a / b
            } else {
                0
            }
        }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_div_guarded_greater_zero() {
        let s = "
        f(a: i32, b: i32) → i32 {
            if b > 0 {
                a / b
            } else {
                0
            }
        }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_mod_by_zero_unconstrained() {
        let s = "
        f(a: i32, b: i32) → i32 { a % b }
        ";

        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_float_div_by_zero_ok() {
        let s = "
        f(a: f32, b: f32) → f32 { a / b }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }
}
