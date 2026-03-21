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

/// Records that `array.len >= min_len` (the array has at least `min_len` elements).
#[derive(Clone, Debug)]
struct MinLenBound {
    pub array: Name,
    pub min_len: i64,
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

    /// Minimum length bounds: records that `array.len >= N`.
    min_len_bounds: Vec<MinLenBound>,

    pub errors: Vec<SafetyError>,
}

impl SafetyChecker {
    pub fn new() -> Self {
        Self {
            vars: vec![],
            constraints: vec![],
            len_bounds: vec![],
            min_len_bounds: vec![],
            errors: vec![],
        }
    }

    fn add(&mut self, name: Name, min: Option<i64>, max: Option<i64>) {
        self.constraints.push(IndexConstraint {
            name,
            min,
            max,
            non_zero: false,
        })
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
            self.constraints.push(IndexConstraint {
                name,
                min: None,
                max: None,
                non_zero: true,
            });
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
                let ival = self.check_expr(*rhs, decl, decls);
                if ival.min != i64::MIN {
                    self.add(*name, Some(ival.min), None);
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
                    // Transitive LenBound: if max_name < array.len (has a LenBound),
                    // then name < max_name < array.len, so name also has a LenBound.
                    // This lets `j < hi && hi < a.len` prove `a[j]` is in bounds.
                    let transitive: Vec<_> = self
                        .len_bounds
                        .iter()
                        .filter(|b| b.index == *max_name)
                        .map(|b| b.array)
                        .collect();
                    for array in transitive {
                        self.len_bounds.push(LenBound {
                            index: *name,
                            array,
                        });
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

        // match `x > n` — x.min = n + 1
        if let Expr::Binop(Binop::Greater, lhs, rhs) = &decl.arena[expr] {
            if let Expr::Id(name) = &decl.arena[*lhs] {
                let ival = self.check_expr(*rhs, decl, decls);
                if ival.min != i64::MAX {
                    self.add(*name, Some(ival.min + 1), None);
                }
            }
            // reversed: `n > i` means i < n
            if let Expr::Id(name) = &decl.arena[*rhs] {
                let ival = self.check_expr(*lhs, decl, decls);
                if ival.max != i64::MAX {
                    self.add(*name, None, Some(ival.max - 1));
                }
            }
            // match `array.len > N` — record min length bound
            if let Expr::Field(arr_expr, field_name) = &decl.arena[*lhs] {
                if field_name.as_str() == "len" {
                    if let Expr::Id(array_name) = &decl.arena[*arr_expr] {
                        let ival = self.check_expr(*rhs, decl, decls);
                        if ival.min != i64::MAX {
                            self.min_len_bounds.push(MinLenBound {
                                array: *array_name,
                                min_len: ival.min + 1,
                            });
                        }
                    }
                }
            }
        }

        // match `N < array.len` — record min length bound
        if let Expr::Binop(Binop::Less, lhs, rhs) = &decl.arena[expr] {
            if let Expr::Field(arr_expr, field_name) = &decl.arena[*rhs] {
                if field_name.as_str() == "len" {
                    if let Expr::Id(array_name) = &decl.arena[*arr_expr] {
                        let ival = self.check_expr(*lhs, decl, decls);
                        if ival.min != i64::MAX {
                            self.min_len_bounds.push(MinLenBound {
                                array: *array_name,
                                min_len: ival.min + 1,
                            });
                        }
                    }
                }
            }
        }

        // reversed: `n < i` means i > n
        if let Expr::Binop(Binop::Less, lhs, rhs) = &decl.arena[expr] {
            if let Expr::Id(name) = &decl.arena[*rhs] {
                let ival = self.check_expr(*lhs, decl, decls);
                if ival.min != i64::MIN {
                    self.add(*name, Some(ival.min + 1), None);
                }
            }
        }

        // reversed: `n >= i` means i <= n
        if let Expr::Binop(Binop::Geq, lhs, rhs) = &decl.arena[expr] {
            if let Expr::Id(name) = &decl.arena[*rhs] {
                let ival = self.check_expr(*lhs, decl, decls);
                if ival.max != i64::MAX {
                    self.add(*name, None, Some(ival.max));
                }
            }
        }

        // reversed: `n <= i` means i >= n
        if let Expr::Binop(Binop::Leq, lhs, rhs) = &decl.arena[expr] {
            if let Expr::Id(name) = &decl.arena[*rhs] {
                let ival = self.check_expr(*lhs, decl, decls);
                if ival.min != i64::MIN {
                    self.add(*name, Some(ival.min), None);
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
            Expr::Int(x) => IndexInterval {
                min: *x,
                max: *x,
                non_zero: *x != 0,
            },
            Expr::UInt(x) => IndexInterval {
                min: *x as i64,
                max: *x as i64,
                non_zero: *x != 0,
            },
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
                let init_r = self.check_expr(*init, decl, decls);
                let ty = decl.types[expr];
                self.vars.push(Var { name: *name, ty });

                // Track the interval from the initializer.
                let mut min = if init_r.min != i64::MIN {
                    Some(init_r.min)
                } else {
                    None
                };
                let max = if init_r.max != i64::MAX {
                    Some(init_r.max)
                } else {
                    None
                };
                if ty == mk_type(Type::UInt32) {
                    min = Some(min.unwrap_or(0).max(0));
                }
                self.add(*name, min, max);
                if init_r.non_zero {
                    self.add_non_zero(*name);
                }

                // Propagate LenBounds: let x = y inherits y's LenBounds.
                if let Expr::Id(src_name) = &decl.arena[*init] {
                    let inherited: Vec<_> = self
                        .len_bounds
                        .iter()
                        .filter(|b| b.index == *src_name)
                        .map(|b| b.array)
                        .collect();
                    for array in inherited {
                        self.len_bounds.push(LenBound {
                            index: *name,
                            array,
                        });
                    }
                }

                IndexInterval::default()
            }
            Expr::Var(name, init, _) => {
                let init_r = if let Some(init) = init {
                    self.check_expr(*init, decl, decls)
                } else {
                    IndexInterval::default()
                };
                let ty = decl.types[expr];

                let mut min = if init_r.min != i64::MIN {
                    Some(init_r.min)
                } else {
                    None
                };
                let max = if init_r.max != i64::MAX {
                    Some(init_r.max)
                } else {
                    None
                };
                if ty == mk_type(Type::UInt32) {
                    min = Some(min.unwrap_or(0).max(0));
                }
                self.add(*name, min, max);
                if init_r.non_zero {
                    self.add_non_zero(*name);
                }

                // Propagate LenBounds: var x = y inherits y's LenBounds.
                if let Some(init) = init {
                    if let Expr::Id(src_name) = &decl.arena[*init] {
                        let inherited: Vec<_> = self
                            .len_bounds
                            .iter()
                            .filter(|b| b.index == *src_name)
                            .map(|b| b.array)
                            .collect();
                        for array in inherited {
                            self.len_bounds.push(LenBound {
                                index: *name,
                                array,
                            });
                        }
                    }
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
                let initial_min_len_bound_count = self.min_len_bounds.len();

                self.match_expr(*cond, decl, decls);

                let mut r = self.check_expr(*then_expr, decl, decls);

                // Pop condition constraints before checking else branch —
                // the else branch executes when the condition is false,
                // so it must not inherit the then-branch constraints.
                while self.constraints.len() > initial_constraint_count {
                    self.constraints.pop();
                }
                self.len_bounds.truncate(initial_len_bound_count);
                self.min_len_bounds.truncate(initial_min_len_bound_count);

                if let Some(else_expr) = else_expr {
                    let else_r = self.check_expr(*else_expr, decl, decls);
                    r = enclose(r, else_r);
                }

                r
            }
            Expr::ArrayIndex(array_expr, index_expr) => {
                if *array_expr >= decl.types.len() {
                    print_error_with_context(
                        decl.arena.locs[expr],
                        "internal compiler error: no type found for array index expression",
                    );
                    return IndexInterval::default();
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
                        (Some(idx), Some(arr)) => self
                            .len_bounds
                            .iter()
                            .any(|b| b.index == idx && b.array == arr),
                        _ => false,
                    };
                    // Also check if the index is a constant within a proven min length.
                    let has_min_len_bound = if let Some(arr) = array_name {
                        rhs_r.max != i64::MAX
                            && self
                                .min_len_bounds
                                .iter()
                                .any(|b| b.array == arr && rhs_r.max < b.min_len)
                    } else {
                        false
                    };
                    if !has_len_bound && !has_min_len_bound {
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
                let saved_min_len_bounds = self.min_len_bounds.clone();
                self.match_expr(*cond, decl, decls);

                self.check_expr(*body, decl, decls);
                self.constraints = saved_constraints;
                self.len_bounds = saved_len_bounds;
                self.min_len_bounds = saved_min_len_bounds;

                // Invalidate constraints for variables assigned inside the loop.
                // The restore gives us pre-loop state, but mutations in the body
                // mean those constraints may not hold at loop exit.
                self.invalidate_assigned(*body, &decl.arena);

                IndexInterval::default()
            }
            Expr::Binop(op, lhs, rhs) => {
                if *op == Binop::Plus {
                    let lhs_range = self.check_expr(*lhs, decl, decls);
                    let rhs_range = self.check_expr(*rhs, decl, decls);
                    return lhs_range + rhs_range;
                }

                if *op == Binop::Minus {
                    let lhs_range = self.check_expr(*lhs, decl, decls);
                    let rhs_range = self.check_expr(*rhs, decl, decls);
                    return lhs_range - rhs_range;
                }

                if *op == Binop::Mult {
                    let lhs_range = self.check_expr(*lhs, decl, decls);
                    let rhs_range = self.check_expr(*rhs, decl, decls);
                    return lhs_range * rhs_range;
                }

                if *op == Binop::Div || *op == Binop::Mod {
                    let lhs_range = self.check_expr(*lhs, decl, decls);
                    let rhs_range = self.check_expr(*rhs, decl, decls);

                    // Only check integer division — float div-by-zero produces Inf/NaN per IEEE 754.
                    if *rhs < decl.types.len() {
                        let ty = decl.types[*rhs];
                        let is_int =
                            matches!(*ty, Type::Int32 | Type::UInt32 | Type::Int8 | Type::UInt8);
                        if is_int && !rhs_range.excludes_zero() {
                            self.errors.push(SafetyError {
                                location: decl.arena.locs[expr],
                                message: format!("couldn't prove divisor is non-zero"),
                            });
                        }
                    }

                    // Compute division interval if the divisor doesn't span zero.
                    if rhs_range.excludes_zero() {
                        let quotients = [
                            lhs_range
                                .min
                                .checked_div(rhs_range.min)
                                .unwrap_or(lhs_range.min),
                            lhs_range
                                .min
                                .checked_div(rhs_range.max)
                                .unwrap_or(lhs_range.min),
                            lhs_range
                                .max
                                .checked_div(rhs_range.min)
                                .unwrap_or(lhs_range.max),
                            lhs_range
                                .max
                                .checked_div(rhs_range.max)
                                .unwrap_or(lhs_range.max),
                        ];
                        let min = *quotients.iter().min().unwrap();
                        let max = *quotients.iter().max().unwrap();
                        return IndexInterval {
                            min,
                            max,
                            non_zero: min > 0 || max < 0,
                        };
                    }
                    return lhs_range; // divisor spans zero — can't narrow
                }

                if *op == Binop::Assign {
                    self.check_expr(*lhs, decl, decls);
                    let rhs_range = self.check_expr(*rhs, decl, decls);

                    if let Expr::Id(name) = &decl.arena[*lhs] {
                        if rhs_range != IndexInterval::default() {
                            self.replace(*name, Some(rhs_range.min), Some(rhs_range.max));
                        } else {
                            // RHS is unconstrained — clear old constraints on LHS.
                            self.replace(*name, None, None);
                        }
                    }

                    return IndexInterval::default();
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
            Expr::Unop(op, expr) => {
                let r = self.check_expr(*expr, decl, decls);
                match op {
                    Unop::Neg => {
                        // -[a, b] = [-b, -a]
                        let new_min = r.max.checked_neg().unwrap_or(i64::MIN);
                        let new_max = r.min.checked_neg().unwrap_or(i64::MAX);
                        IndexInterval {
                            min: new_min,
                            max: new_max,
                            non_zero: r.non_zero,
                        }
                    }
                    _ => IndexInterval::default(),
                }
            }
            Expr::Return(expr) => {
                self.check_expr(*expr, decl, decls);
                IndexInterval::default()
            }
            Expr::Field(expr, _) => {
                self.check_expr(*expr, decl, decls);
                IndexInterval::default()
            }
            Expr::For {
                var,
                start,
                end,
                body,
            } => {
                let start_r = self.check_expr(*start, decl, decls);
                let end_r = self.check_expr(*end, decl, decls);
                self.vars.push(Var {
                    name: *var,
                    ty: mk_type(Type::Int32),
                });
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
                // for i in lo .. hi where hi has a LenBound — transitive bound
                if let Expr::Id(end_name) = &decl.arena[*end] {
                    let transitive: Vec<_> = self
                        .len_bounds
                        .iter()
                        .filter(|b| b.index == *end_name)
                        .map(|b| b.array)
                        .collect();
                    for array in transitive {
                        self.len_bounds.push(LenBound {
                            index: *var,
                            array,
                        });
                    }
                }
                // Save/restore constraints around the body so that mutations
                // inside the loop (e.g. `i = i + 1`) don't clobber the
                // constraints of outer variables after the loop exits.
                let saved_constraints = self.constraints.clone();
                let saved_len_bounds = self.len_bounds.clone();
                let saved_min_len_bounds = self.min_len_bounds.clone();
                self.check_expr(*body, decl, decls);
                self.constraints = saved_constraints.clone();
                self.len_bounds = saved_len_bounds.clone();
                self.min_len_bounds = saved_min_len_bounds;

                // Invalidate constraints for variables assigned inside the loop.
                self.invalidate_assigned(*body, &decl.arena);

                // Recover bounds for monotonically incrementing variables.
                // If a variable is only modified by `var = var + 1`, then:
                //   - Its min bound is preserved (incrementing preserves >= 0)
                //   - Its max after the loop is: initial + (end - start).
                //     If initial <= start, this simplifies to end.
                //     If end has a LenBound on an array, so does the variable.
                //
                // To verify initial <= start, find the var's Var declaration
                // in the AST and check if its initializer is the same identifier
                // as the loop start (e.g., `var i = lo` with `for j in lo .. hi`).
                let start_name = if let Expr::Id(n) = &decl.arena[*start] {
                    Some(*n)
                } else {
                    None
                };
                let mut assigned = Vec::new();
                Self::collect_assigned_vars(*body, &decl.arena, &mut assigned);
                for name in assigned {
                    if !Self::is_monotonic_increment(name, *body, &decl.arena) {
                        continue;
                    }
                    // Restore the pre-loop min bound (monotonic increase preserves it).
                    if let Some(c) = saved_constraints.iter().find(|c| c.name == name) {
                        if let Some(min) = c.min {
                            self.add(name, Some(min), None);
                        }
                    }
                    // Check if the variable was initialized from the loop start.
                    // Scan the AST for `Var(name, Some(init), _)` where init
                    // is `Expr::Id(start_name)`.
                    let initialized_from_start = start_name.is_some_and(|sn| {
                        decl.arena.exprs.iter().any(|e| {
                            if let Expr::Var(vn, Some(init), _) = e {
                                *vn == name
                                    && matches!(&decl.arena[*init], Expr::Id(n) if *n == sn)
                            } else {
                                false
                            }
                        })
                    });
                    if initialized_from_start {
                        if let Expr::Id(end_name) = &decl.arena[*end] {
                            for b in &saved_len_bounds {
                                if b.index == *end_name {
                                    self.len_bounds.push(LenBound {
                                        index: name,
                                        array: b.array,
                                    });
                                }
                            }
                        }
                    }
                }

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

    /// Check if every assignment to `var_name` in the expression tree is of the
    /// form `var_name = var_name + 1` (monotonic increment by 1). Returns false
    /// if the variable is assigned in any other way.
    fn is_monotonic_increment(var_name: Name, expr: ExprID, arena: &ExprArena) -> bool {
        match &arena[expr] {
            Expr::Binop(Binop::Assign, lhs, rhs) => {
                if let Expr::Id(name) = &arena[*lhs] {
                    if *name == var_name {
                        // Check rhs is `var_name + 1`
                        if let Expr::Binop(Binop::Plus, plus_lhs, plus_rhs) = &arena[*rhs] {
                            let lhs_is_var = matches!(&arena[*plus_lhs], Expr::Id(n) if *n == var_name);
                            let rhs_is_one = matches!(&arena[*plus_rhs], Expr::Int(1) | Expr::UInt(1));
                            return lhs_is_var && rhs_is_one;
                        }
                        return false; // Some other assignment to var_name
                    }
                }
                // Assignment to a different variable — recurse into RHS
                Self::is_monotonic_increment(var_name, *rhs, arena)
            }
            Expr::Block(exprs) => exprs
                .iter()
                .all(|e| Self::is_monotonic_increment(var_name, *e, arena)),
            Expr::If(cond, then_expr, else_expr) => {
                Self::is_monotonic_increment(var_name, *cond, arena)
                    && Self::is_monotonic_increment(var_name, *then_expr, arena)
                    && else_expr
                        .map_or(true, |e| Self::is_monotonic_increment(var_name, e, arena))
            }
            Expr::While(cond, body) => {
                Self::is_monotonic_increment(var_name, *cond, arena)
                    && Self::is_monotonic_increment(var_name, *body, arena)
            }
            Expr::For { body, .. } => Self::is_monotonic_increment(var_name, *body, arena),
            _ => true, // No assignment here
        }
    }

    /// Collect all variable names that are assigned (via `=`) inside an expression tree.
    fn collect_assigned_vars(expr: ExprID, arena: &ExprArena, out: &mut Vec<Name>) {
        match &arena[expr] {
            Expr::Binop(Binop::Assign, lhs, rhs) => {
                if let Expr::Id(name) = &arena[*lhs] {
                    out.push(*name);
                }
                Self::collect_assigned_vars(*rhs, arena, out);
            }
            Expr::Binop(_, lhs, rhs) => {
                Self::collect_assigned_vars(*lhs, arena, out);
                Self::collect_assigned_vars(*rhs, arena, out);
            }
            Expr::Block(exprs) => {
                for e in exprs {
                    Self::collect_assigned_vars(*e, arena, out);
                }
            }
            Expr::If(cond, then_expr, else_expr) => {
                Self::collect_assigned_vars(*cond, arena, out);
                Self::collect_assigned_vars(*then_expr, arena, out);
                if let Some(e) = else_expr {
                    Self::collect_assigned_vars(*e, arena, out);
                }
            }
            Expr::While(cond, body) => {
                Self::collect_assigned_vars(*cond, arena, out);
                Self::collect_assigned_vars(*body, arena, out);
            }
            Expr::For { body, .. } => {
                Self::collect_assigned_vars(*body, arena, out);
            }
            _ => {}
        }
    }

    /// Invalidate constraints for variables that were assigned inside a loop body.
    /// After save/restore, the restored constraints reflect pre-loop state, but
    /// any variable mutated inside the loop could hold a different value at exit.
    fn invalidate_assigned(
        &mut self,
        body: ExprID,
        arena: &ExprArena,
    ) {
        let mut assigned = Vec::new();
        Self::collect_assigned_vars(body, arena, &mut assigned);
        for name in assigned {
            self.constraints.retain(|c| c.name != name);
            self.len_bounds.retain(|b| b.index != name);
        }
    }

    fn check_fn_decl(&mut self, func_decl: &FuncDecl, decls: &DeclTable) {
        if let Some(body) = func_decl.body {
            for param in &func_decl.params {
                if let Some(ty) = param.ty {
                    self.vars.push(Var {
                        name: param.name,
                        ty,
                    });
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
            self.min_len_bounds.clear();
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

    #[test]
    pub fn test_nested_array_access() {
        // b[i] is unconstrained, so a[b[i]] can't be proven safe
        let s = "
        f(i: i32) {
            var a: [i32; 10]
            var b: [i32; 10]
            if i >= 0 && i < 10 {
                a[b[i]]
            }
        }
        ";

        let errors = check(s);
        // b[i] is safe (i is bounded), but a[b[i]] is unsafe
        // because b[i] could return any i32 value
        assert_eq!(errors.len(), 2); // can't prove >= 0 and can't prove < length
    }

    #[test]
    pub fn test_div_by_array_element() {
        // a[i] is unconstrained, so x / a[i] can't be proven non-zero
        let s = "
        f(x: i32) → i32 {
            var a: [i32; 5]
            if true {
                x / a[0]
            } else {
                0
            }
        }
        ";

        let errors = check(s);
        assert_eq!(errors.len(), 1); // can't prove divisor non-zero
    }

    #[test]
    pub fn test_for_loop_with_offset() {
        // for i in 1 .. a.len, then a[i-1] should be safe
        // i is in [1, len-1], so i-1 is in [0, len-2], which is < len
        let s = "
        f {
            var a: [i32; 100]
            for i in 1 .. 100 {
                a[i - 1]
            }
        }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_negative_literal_division() {
        // x / (-1) should be safe because -1 is non-zero
        let s = "
        f(x: i32) → i32 { x / (-1) }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_multiple_arrays_mixed_safety() {
        // a[i] is safe (i bounded), b[j] is unsafe (j unbounded)
        let s = "
        f(i: i32, j: i32) {
            var a: [i32; 100]
            var b: [i32; 50]
            if i >= 0 && i < 100 {
                a[i]
                b[j]
            }
        }
        ";

        let errors = check(s);
        // b[j] has two errors: can't prove >= 0 and can't prove < length
        assert_eq!(errors.len(), 2);
    }

    #[test]
    pub fn test_conditional_narrowing_lost_after_else() {
        // The constraint from if-guard should NOT persist after the if/else
        let s = "
        f(i: i32) {
            var a: [i32; 100]
            if i >= 0 && i < 100 {
                a[i]
            } else {
                0
            }
            a[i]
        }
        ";

        let errors = check(s);
        // The a[i] after the if/else should fail (constraints popped)
        assert_eq!(errors.len(), 2); // can't prove >= 0 and can't prove < length
    }

    // --- Edge cases for interval arithmetic gaps ---

    #[test]
    pub fn test_let_tracks_init_interval() {
        // let x = 5 should track x in [5, 5], so a[x] in [i32; 10] is safe
        let s = "
        f {
            var a: [i32; 10]
            let x = 5
            a[x]
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_var_tracks_init_interval() {
        // var x = 3 should track x in [3, 3], so a[x] in [i32; 5] is safe
        let s = "
        f {
            var a: [i32; 5]
            var x = 3
            a[x]
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_let_init_out_of_bounds() {
        // let x = 10 should fail for [i32; 10] (max index is 9)
        let s = "
        f {
            var a: [i32; 10]
            let x = 10
            a[x]
        }
        ";
        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_division_narrows_interval() {
        // x in [0, 99], x / 10 should be in [0, 9], safe for [i32; 10]
        let s = "
        f(x: i32) {
            var a: [i32; 10]
            if x >= 0 && x < 100 {
                a[x / 10]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_division_still_catches_unsafe() {
        // x in [0, 199], x / 10 is [0, 19], NOT safe for [i32; 10]
        let s = "
        f(x: i32) {
            var a: [i32; 10]
            if x >= 0 && x < 200 {
                a[x / 10]
            }
        }
        ";
        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_greater_arbitrary_value() {
        // if x > 5, then x >= 6, so x is safe as index for [i32; 100]
        // and x - 6 is safe as index for [i32; 100]
        let s = "
        f(x: i32) {
            var a: [i32; 100]
            if x > 5 && x < 100 {
                a[x]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_reversed_greater() {
        // 100 > i means i < 100
        let s = "
        f(i: i32) {
            var a: [i32; 100]
            if i >= 0 && 100 > i {
                a[i]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_reversed_less() {
        // 0 < i means i > 0, so i >= 1
        let s = "
        f(i: i32) {
            var a: [i32; 100]
            if 0 < i && i < 100 {
                a[i - 1]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_reversed_geq() {
        // 99 >= i means i <= 99
        let s = "
        f(i: i32) {
            var a: [i32; 100]
            if i >= 0 && 99 >= i {
                a[i]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_reversed_leq() {
        // 0 <= i means i >= 0
        let s = "
        f(i: i32) {
            var a: [i32; 100]
            if 0 <= i && i < 100 {
                a[i]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_nonzero_through_addition() {
        // x >= 0 means x + 1 >= 1, which is non-zero
        let s = "
        f(a: i32, x: i32) → i32 {
            if x >= 0 {
                a / (x + 1)
            } else {
                0
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_nonzero_through_multiplication() {
        // x > 0 and y > 0 means x * y > 0
        let s = "
        f(a: i32, x: i32, y: i32) → i32 {
            if x > 0 && y > 0 {
                a / (x * y)
            } else {
                0
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_geq_with_variable() {
        // if i >= start where start is constrained
        let s = "
        f(start: i32, i: i32) {
            var a: [i32; 100]
            if start >= 0 && i >= start && i < 100 {
                a[i]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_computed_let_bounds() {
        // let y = x + 1, where x is in [0, 8] means y is in [1, 9]
        let s = "
        f(x: i32) {
            var a: [i32; 10]
            if x >= 0 && x < 9 {
                let y = x + 1
                a[y]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_div_by_literal_nonzero_via_interval() {
        // 2 is in [2, 2], which excludes zero — should be safe
        let s = "
        f(x: i32) → i32 { x / 5 }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_div_nonzero_via_greater() {
        // if b > 5, then b >= 6, excludes zero
        let s = "
        f(a: i32, b: i32) → i32 {
            if b > 5 {
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
    pub fn test_negative_interval_division() {
        // x in [-10, -1], x / 2 is in [-5, 0] — that's safe for non-zero
        // but NOT safe for array indexing (negative)
        let s = "
        f(x: i32) {
            var a: [i32; 10]
            if x >= -10 && x < 0 {
                a[x / 2]
            }
        }
        ";
        let errors = check(s);
        assert_eq!(errors.len(), 1); // can't prove >= 0
    }

    #[test]
    pub fn test_var_reassignment_updates_interval() {
        // var x = 50; x = 3 should update x to [3, 3]
        let s = "
        f {
            var a: [i32; 5]
            var x = 50
            x = 3
            a[x]
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_let_arithmetic_propagation() {
        // let x = 2 * 3 should give x = [6, 6]
        let s = "
        f {
            var a: [i32; 10]
            let x = 2 * 3
            a[x]
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_subtraction_nonzero() {
        // x in [5, 10], 1 in [1, 1] → x - 1 in [4, 9], which is non-zero
        let s = "
        f(a: i32, x: i32) → i32 {
            if x > 4 {
                a / (x - 1)
            } else {
                0
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    // --- Bug fix tests ---

    #[test]
    pub fn test_else_branch_no_condition_constraints() {
        // The else branch runs when the condition is FALSE,
        // so condition constraints must NOT apply there.
        let s = "
        f(i: i32) {
            var a: [i32; 100]
            if i >= 0 && i < 100 {
                a[i]
            } else {
                a[i]
            }
        }
        ";
        let errors = check(s);
        // a[i] in else should fail: can't prove >= 0 and can't prove < length
        assert_eq!(errors.len(), 2);
    }

    #[test]
    pub fn test_else_branch_safe_literal() {
        // Else branch with a safe literal should still be fine.
        let s = "
        f(i: i32) {
            var a: [i32; 100]
            if i >= 0 && i < 100 {
                a[i]
            } else {
                a[0]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_assign_unconstrained_clears_bounds() {
        // Assigning an unconstrained variable should clear old constraints.
        let s = "
        f(j: i32) {
            var a: [i32; 5]
            var i = 3
            i = j
            a[i]
        }
        ";
        let errors = check(s);
        // i was [3,3] but after i = j it's unconstrained
        assert_eq!(errors.len(), 2); // can't prove >= 0 and can't prove < length
    }

    #[test]
    pub fn test_assign_constrained_updates_bounds() {
        // Assigning a constrained value should update bounds correctly.
        let s = "
        f(j: i32) {
            var a: [i32; 10]
            var i = 50
            if j >= 0 && j < 10 {
                i = j
                a[i]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    // --- Aliased index tests ---

    #[test]
    pub fn test_aliased_index_let() {
        // let j = i should propagate i's constraints to j
        let s = "
        f(i: i32) {
            var a: [i32; 100]
            if i >= 0 && i < 100 {
                let j = i
                a[j]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_aliased_index_computed() {
        // let j = i + 1, where i in [0, 98], j in [1, 99], safe for [i32; 100]
        let s = "
        f(i: i32) {
            var a: [i32; 100]
            if i >= 0 && i < 99 {
                let j = i + 1
                a[j]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_aliased_index_unsafe() {
        // let j = i + 1, where i in [0, 99], j in [1, 100], NOT safe for [i32; 100]
        let s = "
        f(i: i32) {
            var a: [i32; 100]
            if i >= 0 && i < 100 {
                let j = i + 1
                a[j]
            }
        }
        ";
        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    // --- Nested loop tests ---

    #[test]
    pub fn test_nested_for_loops_2d() {
        // Nested for loops indexing a flat array with i * cols + j
        let s = "
        f {
            var a: [i32; 100]
            for i in 0 .. 10 {
                for j in 0 .. 10 {
                    a[i * 10 + j]
                }
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_nested_for_loops_2d_overflow() {
        // i * 10 + j where i in [0,9] and j in [0,9] gives [0,99],
        // but array is only 50 elements
        let s = "
        f {
            var a: [i32; 50]
            for i in 0 .. 10 {
                for j in 0 .. 10 {
                    a[i * 10 + j]
                }
            }
        }
        ";
        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_while_loop_slice_bounded() {
        // while i < s.len should allow s[i]
        let s = "
        f(s: [i32]) {
            var i = 0
            while i < s.len {
                s[i]
                i = i + 1
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    // --- Off-by-one edge cases ---

    #[test]
    pub fn test_exact_last_element() {
        // Accessing index length-1 should be safe
        let s = "
        f {
            var a: [i32; 10]
            a[9]
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_one_past_end() {
        // Accessing index == length should be caught
        let s = "
        f {
            var a: [i32; 10]
            a[10]
        }
        ";
        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_for_loop_last_element() {
        // for i in 0 .. 10: i ranges [0, 9], a[i] in [i32; 10] is safe
        let s = "
        f {
            var a: [i32; 10]
            for i in 0 .. 10 {
                a[i]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_for_loop_one_past_end() {
        // for i in 0 .. 11: i ranges [0, 10], a[i] in [i32; 10] is NOT safe
        let s = "
        f {
            var a: [i32; 10]
            for i in 0 .. 11 {
                a[i]
            }
        }
        ";
        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_single_element_array_bounds() {
        // [i32; 1] — only index 0 is valid
        let s = "
        f(i: i32) {
            var a: [i32; 1]
            if i >= 0 && i < 1 {
                a[i]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    // --- Slice edge cases ---

    #[test]
    pub fn test_slice_two_slices_same_index() {
        // i bounded by a.len should work for a[i] but not b[i]
        let s = "
        f(a: [i32], b: [i32]) {
            for i in 0 .. a.len {
                a[i]
                b[i]
            }
        }
        ";
        let errors = check(s);
        assert_eq!(errors.len(), 1); // b[i] fails
    }

    #[test]
    pub fn test_slice_both_bounded() {
        // Both bounded by their own .len
        let s = "
        f(a: [i32], b: [i32]) {
            for i in 0 .. a.len {
                a[i]
            }
            for j in 0 .. b.len {
                b[j]
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_slice_while_no_bound() {
        // while loop without len comparison — should fail
        let s = "
        f(s: [i32]) {
            var i = 0
            while i < 100 {
                s[i]
                i = i + 1
            }
        }
        ";
        let errors = check(s);
        assert_eq!(errors.len(), 1); // can't prove < slice length
    }

    // --- Division edge cases ---

    #[test]
    pub fn test_div_by_zero_literal() {
        let s = "
        f(x: i32) → i32 { x / 0 }
        ";
        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_mod_guarded_nonzero() {
        let s = "
        f(a: i32, b: i32) → i32 {
            if b != 0 {
                a % b
            } else {
                0
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_div_negative_divisor_safe() {
        // b < 0 means b <= -1, which excludes zero
        let s = "
        f(a: i32, b: i32) → i32 {
            if b < 0 {
                a / b
            } else {
                0
            }
        }
        ";
        let errors = check(s);
        assert!(errors.is_empty());
    }
}
