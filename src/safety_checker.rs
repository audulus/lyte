use crate::checked::{CheckedBody as ExprArena, CheckedFunction as FuncDecl};
use crate::interval::{enclose, IndexInterval};
use crate::*;

/// The safety analysis sees the phase's authoritative call references. Type
/// layout still comes from the phase's declaration inventory.
pub trait SafetyProgram: std::ops::Deref<Target = CheckedDeclarations> {
    fn call_target<'a>(
        &'a self,
        reference: &Reference,
        body: &CheckedBody,
        signature: TypeID,
        arity: usize,
    ) -> Option<&'a CheckedFunction>;
}
impl SafetyProgram for CheckedProgram {
    fn call_target<'a>(
        &'a self,
        reference: &Reference,
        body: &CheckedBody,
        signature: TypeID,
        arity: usize,
    ) -> Option<&'a CheckedFunction> {
        let ids: &[DefId] = match reference {
            Reference::Functions(ids) => ids,
            Reference::InterfaceMember {
                requirement,
                member,
            } => {
                let requirement = body.requirements.get(requirement.index())?;
                let member = requirement
                    .members
                    .iter()
                    .find(|candidate| candidate.definition == *member)?;
                &member.candidates
            }
            _ => return None,
        };
        // Templates retain the existing first exact candidate policy. This is
        // source diagnostic coverage, not concrete overload selection.
        ids.iter()
            .filter_map(|id| self.function(*id))
            .find(|function| function.params.len() == arity && function.ty() == signature)
    }
}
impl SafetyProgram for SpecializedProgram {
    fn call_target<'a>(
        &'a self,
        reference: &Reference,
        _body: &CheckedBody,
        _signature: TypeID,
        arity: usize,
    ) -> Option<&'a CheckedFunction> {
        let Reference::Instance(id) = reference else {
            return None;
        };
        // The selected function is authoritative. Checking and specialization
        // establish coercion-aware compatibility; structural validation checks
        // arity before safety runs. A function-valued global is still indirect.
        self.function_instance(*id)
            .filter(|function| function.params.len() == arity)
    }
}

/// Expression analysis needs its owning body and symbolic size binders only.
/// Function signatures and contracts stay at the function/call boundary.
#[derive(Clone, Copy)]
struct SafetyBody<'a> {
    arena: &'a CheckedBody,
    size_vars: &'a [SizeParameter],
}

impl<'a> From<&'a FuncDecl> for SafetyBody<'a> {
    fn from(function: &'a FuncDecl) -> Self {
        Self {
            arena: &function.arena,
            size_vars: &function.size_vars,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
enum PlaceRoot {
    Local(LocalId),
    Global(DefId),
    Instance(InstanceId),
}

/// Field paths are structural projections of an identified storage root.
/// Diagnostic binding spellings never participate in equality.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
struct Place {
    root: PlaceRoot,
    fields: Option<Name>,
}
impl Place {
    fn local(local: LocalId) -> Self {
        Self {
            root: PlaceRoot::Local(local),
            fields: None,
        }
    }
    fn field(self, field: Name) -> Self {
        let fields = match self.fields {
            Some(path) => Name::new(format!("{}.{}", path, field)),
            None => field,
        };
        Self {
            fields: Some(fields),
            ..self
        }
    }
}
fn reference_place(reference: &Reference) -> Option<Place> {
    let root = match reference {
        Reference::Local(local) | Reference::SizeParameter(local) => PlaceRoot::Local(*local),
        Reference::Global(definition) => PlaceRoot::Global(*definition),
        Reference::Instance(instance) => PlaceRoot::Instance(*instance),
        _ => return None,
    };
    Some(Place { root, fields: None })
}
fn id_place(id: ExprID, arena: &ExprArena) -> Option<Place> {
    match &arena[id] {
        Expr::Id(_) => arena.reference(id).and_then(reference_place),
        _ => None,
    }
}

/// Lanes in an `f32x4`. A lane index has to be provably in `0..4` for the same
/// reason an array index has to be in range: no backend checks it at runtime.
const F32X4_LANES: i64 = 4;

/// Walk a parameter type and the corresponding caller-argument type in
/// parallel, recording bindings of `ArraySize::Var(name)` to the concrete
/// `Known(k)` value found in the argument.
///
/// This lets the call-site prover evaluate require clauses that reference
/// size variables (e.g., `idx < N`) or `[T; N].len`, since N is determined
/// by the actual array size passed in.
fn collect_size_subst(param_ty: TypeID, arg_ty: TypeID, out: &mut Vec<(Name, i64)>) {
    match (&*param_ty, &*arg_ty) {
        (Type::Array(p_elem, ArraySize::Var(n)), Type::Array(a_elem, ArraySize::Known(k))) => {
            if !out.iter().any(|(name, _)| name == n) {
                out.push((*n, *k as i64));
            }
            collect_size_subst(*p_elem, *a_elem, out);
        }
        (Type::Array(p_elem, _), Type::Array(a_elem, _)) => {
            collect_size_subst(*p_elem, *a_elem, out);
        }
        (Type::Tuple(ps), Type::Tuple(as_)) if ps.len() == as_.len() => {
            for (p, a) in ps.iter().zip(as_) {
                collect_size_subst(*p, *a, out);
            }
        }
        _ => {}
    }
}

/// A trackable storage place, including direct field projections.
fn expr_place(id: ExprID, arena: &ExprArena) -> Option<Place> {
    match &arena[id] {
        Expr::Id(_) => arena.reference(id).and_then(reference_place),
        Expr::Field(base, field) => Some(expr_place(*base, arena)?.field(*field)),
        _ => None,
    }
}

/// The fixed-size array type of an expression, ignoring any coercion applied
/// to it as a call argument.
///
/// The checker records one type per expression, and passing a `[T; N]` to a
/// `[T]` parameter can leave that recorded type as a slice — `unify` relates
/// the two by element type alone, so the length is not part of the match.
/// The base of an index or field expression is never itself the argument, so
/// walking down from it recovers the array type the coercion hid.
fn array_type(expr: ExprID, context: SafetyBody<'_>, decls: &impl SafetyProgram) -> Option<TypeID> {
    if expr < context.arena.len() {
        let ty = context.arena.ty(expr);
        if let Type::Array(_, ArraySize::Known(_)) = *ty {
            return Some(ty);
        }
    }
    match &context.arena[expr] {
        // An element of `[[T; N]; M]` is a `[T; N]`; anything else has no
        // length to recover.
        Expr::ArrayIndex(base, _) => match &*array_type(*base, context, decls)? {
            Type::Array(elem, _) if matches!(**elem, Type::Array(_, ArraySize::Known(_))) => {
                Some(*elem)
            }
            _ => None,
        },
        Expr::Field(base, field) => {
            let base_ty = if *base < context.arena.len() {
                context.arena.ty(*base)
            } else {
                return None;
            };
            let Type::Name(struct_name, _) = &*base_ty else {
                return None;
            };
            for d in decls.find(*struct_name) {
                if let Decl::Struct(sd) = d {
                    if let Some(f) = sd.find_field(field) {
                        return Some(f.ty);
                    }
                }
            }
            None
        }
        _ => None,
    }
}

/// Static length of an array-typed expression, when it has one.
fn static_len(expr: ExprID, context: SafetyBody<'_>, decls: &impl SafetyProgram) -> Option<i64> {
    match &*array_type(expr, context, decls)? {
        Type::Array(_, ArraySize::Known(n)) => Some(*n as i64),
        _ => None,
    }
}

#[derive(Clone)]
pub struct SafetyError {
    pub location: Loc,
    pub message: String,
}

#[derive(Clone, Debug)]
struct IndexConstraint {
    pub name: Place,
    pub min: Option<i64>,
    pub max: Option<i64>,
    pub non_zero: bool,
}

/// Records that variable `index` has been proven < `array.len`.
#[derive(Clone, Debug)]
struct LenBound {
    pub index: Place,
    pub array: Place,
}

/// Records that `array.len >= min_len` (the array has at least `min_len` elements).
#[derive(Clone, Debug)]
struct MinLenBound {
    pub array: Place,
    pub min_len: i64,
}

/// Records that variable `lo` is proven < variable `hi`.
#[derive(Clone, Debug)]
struct VarBound {
    pub lo: Place,
    pub hi: Place,
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
/// - A lambda body is checked with its parameters unconstrained, the same way
///   a function body is, unless the lambda is called directly (`(|i| ..)(5)`),
///   in which case the argument intervals are used. A lambda stored in a
///   variable and called later must therefore guard its own parameters.
/// - Captured variables keep their definition-site constraints only if the
///   enclosing function never assigns to them, since the call happens at an
///   unknown later time. A captured variable that is assigned anywhere in the
///   function — including inside the lambda itself — is unconstrained in the
///   body, so the body has to guard it too. A directly-called lambda is
///   exempt: its definition site is its call site.
pub struct SafetyChecker {
    /// Constraints we know about each var.
    constraints: Vec<IndexConstraint>,

    /// Symbolic length bounds: records that `index < array.len`.
    len_bounds: Vec<LenBound>,

    /// Symbolic length bounds (non-strict): records that `index <= array.len`.
    leq_len_bounds: Vec<LenBound>,

    /// Minimum length bounds: records that `array.len >= N`.
    min_len_bounds: Vec<MinLenBound>,

    /// Variable-to-variable less-than bounds: records that `lo < hi`.
    var_bounds: Vec<VarBound>,

    /// Every variable assigned anywhere in the function currently being
    /// checked. A lambda body can run at any point after its definition, so
    /// anything in here is unconstrained inside a lambda that isn't called
    /// immediately.
    fn_assigned: Vec<Place>,

    /// Whole-body specialization preserves source locations. The same source
    /// call/requirement can fail in several instances whose emitted names
    /// differ. Keep distinct concretized clauses (e.g. different array sizes).
    failed_requirements: std::collections::HashMap<(Loc, Loc, String), String>,

    pub errors: Vec<SafetyError>,
}

impl SafetyChecker {
    pub fn new() -> Self {
        Self {
            constraints: vec![],
            len_bounds: vec![],
            leq_len_bounds: vec![],
            min_len_bounds: vec![],
            var_bounds: vec![],
            fn_assigned: vec![],
            failed_requirements: std::collections::HashMap::new(),
            errors: vec![],
        }
    }

    /// Record a safety error, dropping exact duplicates. `match_expr` visits
    /// some subexpressions more than once (e.g. the `Greater` arm looks at its
    /// rhs both as a general expression and as an `array.len > N` pattern), and
    /// since `check_expr` descends into lambda bodies that would otherwise
    /// surface as the same diagnostic reported twice at one location.
    fn push_error(&mut self, err: SafetyError) {
        if self
            .errors
            .iter()
            .any(|e| e.location == err.location && e.message == err.message)
        {
            return;
        }
        self.errors.push(err);
    }

    fn add(&mut self, name: Place, min: Option<i64>, max: Option<i64>) {
        self.constraints.push(IndexConstraint {
            name,
            min,
            max,
            non_zero: false,
        })
    }

    fn replace(&mut self, name: Place, min: Option<i64>, max: Option<i64>) {
        self.constraints.retain(|c| c.name != name);
        self.add(name, min, max);
    }

    fn add_non_zero(&mut self, name: Place) {
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

    /// Drop facts about a storage place before a new dynamic value is bound.
    fn forget(&mut self, name: Place) {
        self.constraints.retain(|c| c.name != name);
        self.len_bounds
            .retain(|b| b.index != name && b.array != name);
        self.leq_len_bounds
            .retain(|b| b.index != name && b.array != name);
        self.min_len_bounds.retain(|b| b.array != name);
        self.var_bounds.retain(|b| b.lo != name && b.hi != name);
    }

    fn find(&self, name: Place) -> Option<IndexConstraint> {
        self.constraints.iter().find(|c| c.name == name).cloned()
    }

    /// Given expr evaluates to true, add constraints accordingly.
    fn match_expr(&mut self, expr: ExprID, context: SafetyBody<'_>, decls: &impl SafetyProgram) {
        // Track bounds from comparisons. Handles both simple variables (Expr::Id)
        // and struct field access (Expr::Field) via expr_place.
        if let Expr::Binop(Binop::Less, lhs, rhs) = &context.arena[expr] {
            if let Some(name) = expr_place(*lhs, context.arena) {
                let ival = self.check_expr(*rhs, context, decls);
                if ival.max != i64::max_value() {
                    self.add(name, None, Some(ival.max - 1));
                }
            }
        }

        if let Expr::Binop(Binop::Leq, lhs, rhs) = &context.arena[expr] {
            if let Some(name) = expr_place(*lhs, context.arena) {
                let ival = self.check_expr(*rhs, context, decls);
                if ival.max != i64::max_value() {
                    self.add(name, None, Some(ival.max));
                }
            }
        }

        if let Expr::Binop(Binop::Geq, lhs, rhs) = &context.arena[expr] {
            if let Some(name) = expr_place(*lhs, context.arena) {
                let ival = self.check_expr(*rhs, context, decls);
                if ival.min != i64::MIN {
                    self.add(name, Some(ival.min), None);
                }
            }
        }

        // match `array.len >= N` — record min length bound
        if let Expr::Binop(Binop::Geq, lhs, rhs) = &context.arena[expr] {
            if let Expr::Field(arr_expr, field_name) = &context.arena[*lhs] {
                if field_name.as_str() == "len" {
                    if let Some(ref array_name) = id_place(*arr_expr, context.arena) {
                        let ival = self.check_expr(*rhs, context, decls);
                        if ival.min != i64::MAX {
                            self.min_len_bounds.push(MinLenBound {
                                array: *array_name,
                                min_len: ival.min,
                            });
                        }
                    }
                }
            }
        }

        // match `n <= array.len` — record leq length bound and min length bound
        if let Expr::Binop(Binop::Leq, lhs, rhs) = &context.arena[expr] {
            if let Expr::Field(arr_expr, field_name) = &context.arena[*rhs] {
                if field_name.as_str() == "len" {
                    if let Some(ref array_name) = id_place(*arr_expr, context.arena) {
                        // Record n <= array.len for transitive propagation
                        if let Some(ref name) = id_place(*lhs, context.arena) {
                            self.leq_len_bounds.push(LenBound {
                                index: *name,
                                array: *array_name,
                            });
                        }
                        let ival = self.check_expr(*lhs, context, decls);
                        if ival.min != i64::MAX {
                            self.min_len_bounds.push(MinLenBound {
                                array: *array_name,
                                min_len: ival.min,
                            });
                        }
                    }
                }
            }
        }

        // match `array.len >= n` — record leq length bound (same as n <= array.len)
        if let Expr::Binop(Binop::Geq, lhs, rhs) = &context.arena[expr] {
            if let Expr::Field(arr_expr, field_name) = &context.arena[*lhs] {
                if field_name.as_str() == "len" {
                    if let Some(ref array_name) = id_place(*arr_expr, context.arena) {
                        if let Some(ref name) = id_place(*rhs, context.arena) {
                            self.leq_len_bounds.push(LenBound {
                                index: *name,
                                array: *array_name,
                            });
                        }
                    }
                }
            }
        }

        // match expressions of the form i < id, where id is another variable
        // with a constraint
        if let Expr::Binop(Binop::Less, lhs, rhs) = &context.arena[expr] {
            if let Some(ref name) = id_place(*lhs, context.arena) {
                if let Some(ref max_name) = id_place(*rhs, context.arena) {
                    if let Some(c) = self.find(*max_name) {
                        if let Some(max) = c.max {
                            self.add(*name, None, Some(max));
                        }
                    }
                    // Record the variable-to-variable relationship for
                    // later transitive propagation.
                    self.var_bounds.push(VarBound {
                        lo: *name,
                        hi: *max_name,
                    });
                }
                // match i < array.len — record symbolic length bound
                if let Expr::Field(arr_expr, field_name) = &context.arena[*rhs] {
                    if field_name.as_str() == "len" {
                        if let Some(ref array_name) = id_place(*arr_expr, context.arena) {
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
        if let Expr::Binop(Binop::NotEqual, lhs, rhs) = &context.arena[expr] {
            if let Some(name) = expr_place(*lhs, context.arena) {
                if let Expr::Int(0, _) = &context.arena[*rhs] {
                    self.add_non_zero(name);
                }
            }
            if let Some(name) = expr_place(*rhs, context.arena) {
                if let Expr::Int(0, _) = &context.arena[*lhs] {
                    self.add_non_zero(name);
                }
            }
        }

        // match `x > n` — x.min = n + 1
        if let Expr::Binop(Binop::Greater, lhs, rhs) = &context.arena[expr] {
            if let Some(name) = expr_place(*lhs, context.arena) {
                let ival = self.check_expr(*rhs, context, decls);
                if ival.min != i64::MAX {
                    self.add(name, Some(ival.min + 1), None);
                }
            }
            // reversed: `n > i` means i < n
            if let Some(name) = expr_place(*rhs, context.arena) {
                let ival = self.check_expr(*lhs, context, decls);
                if ival.max != i64::MAX {
                    self.add(name, None, Some(ival.max - 1));
                }
            }
            // match `array.len > N` — record min length bound
            if let Expr::Field(arr_expr, field_name) = &context.arena[*lhs] {
                if field_name.as_str() == "len" {
                    if let Some(ref array_name) = id_place(*arr_expr, context.arena) {
                        let ival = self.check_expr(*rhs, context, decls);
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
        if let Expr::Binop(Binop::Less, lhs, rhs) = &context.arena[expr] {
            if let Expr::Field(arr_expr, field_name) = &context.arena[*rhs] {
                if field_name.as_str() == "len" {
                    if let Some(ref array_name) = id_place(*arr_expr, context.arena) {
                        let ival = self.check_expr(*lhs, context, decls);
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
        if let Expr::Binop(Binop::Less, lhs, rhs) = &context.arena[expr] {
            if let Some(name) = expr_place(*rhs, context.arena) {
                let ival = self.check_expr(*lhs, context, decls);
                if ival.min != i64::MIN {
                    self.add(name, Some(ival.min + 1), None);
                }
            }
        }

        // reversed: `n >= i` means i <= n
        if let Expr::Binop(Binop::Geq, lhs, rhs) = &context.arena[expr] {
            if let Some(name) = expr_place(*rhs, context.arena) {
                let ival = self.check_expr(*lhs, context, decls);
                if ival.max != i64::MAX {
                    self.add(name, None, Some(ival.max));
                }
            }
        }

        // reversed: `n <= i` means i >= n
        if let Expr::Binop(Binop::Leq, lhs, rhs) = &context.arena[expr] {
            if let Some(name) = expr_place(*rhs, context.arena) {
                let ival = self.check_expr(*lhs, context, decls);
                if ival.min != i64::MIN {
                    self.add(name, Some(ival.min), None);
                }
            }
        }

        // match `a == b` — treat as both `a <= b` and `a >= b`
        if let Expr::Binop(Binop::Equal, lhs, rhs) = &context.arena[expr] {
            // Constrain lhs from rhs value
            if let Some(name) = expr_place(*lhs, context.arena) {
                let ival = self.check_expr(*rhs, context, decls);
                self.add(name, Some(ival.min), Some(ival.max));
            }
            // Constrain rhs from lhs value
            if let Some(name) = expr_place(*rhs, context.arena) {
                let ival = self.check_expr(*lhs, context, decls);
                self.add(name, Some(ival.min), Some(ival.max));
            }
            // array.len == N → min_len_bound
            if let Expr::Field(arr_expr, field_name) = &context.arena[*lhs] {
                if field_name.as_str() == "len" {
                    if let Some(ref array_name) = id_place(*arr_expr, context.arena) {
                        let ival = self.check_expr(*rhs, context, decls);
                        if ival.min != i64::MAX {
                            self.min_len_bounds.push(MinLenBound {
                                array: *array_name,
                                min_len: ival.min,
                            });
                        }
                        // Also record leq bound: n <= array.len
                        if let Some(ref name) = id_place(*rhs, context.arena) {
                            self.leq_len_bounds.push(LenBound {
                                index: *name,
                                array: *array_name,
                            });
                        }
                    }
                }
            }
            // N == array.len → min_len_bound
            if let Expr::Field(arr_expr, field_name) = &context.arena[*rhs] {
                if field_name.as_str() == "len" {
                    if let Some(ref array_name) = id_place(*arr_expr, context.arena) {
                        let ival = self.check_expr(*lhs, context, decls);
                        if ival.min != i64::MAX {
                            self.min_len_bounds.push(MinLenBound {
                                array: *array_name,
                                min_len: ival.min,
                            });
                        }
                        // Also record leq bound: n <= array.len
                        if let Some(ref name) = id_place(*lhs, context.arena) {
                            self.leq_len_bounds.push(LenBound {
                                index: *name,
                                array: *array_name,
                            });
                        }
                    }
                }
            }
        }

        if let Expr::Binop(Binop::And, lhs, rhs) = &context.arena[expr] {
            self.match_expr(*lhs, context, decls);
            self.match_expr(*rhs, context, decls);
            self.propagate_len_bounds();
        }
    }

    /// Propagate LenBounds through VarBounds transitively.
    ///
    /// If we know `lo < hi` (VarBound) and `hi < a.len` (LenBound),
    /// then `lo < a.len`.
    ///
    /// Also: if `lo < hi` (VarBound) and `hi <= a.len` (LeqLenBound),
    /// then `lo < a.len` (since `lo < hi <= a.len` implies `lo < a.len`).
    ///
    /// This is done as a fixpoint so that chains of any length are handled.
    fn propagate_len_bounds(&mut self) {
        loop {
            let mut added = false;
            for vb in 0..self.var_bounds.len() {
                let lo = self.var_bounds[vb].lo;
                let hi = self.var_bounds[vb].hi;
                // lo < hi < array.len → lo < array.len
                for lb in 0..self.len_bounds.len() {
                    let array = self.len_bounds[lb].array;
                    if self.len_bounds[lb].index == hi {
                        if !self
                            .len_bounds
                            .iter()
                            .any(|b| b.index == lo && b.array == array)
                        {
                            self.len_bounds.push(LenBound { index: lo, array });
                            added = true;
                        }
                    }
                }
                // lo < hi <= array.len → lo < array.len
                for lb in 0..self.leq_len_bounds.len() {
                    let array = self.leq_len_bounds[lb].array;
                    if self.leq_len_bounds[lb].index == hi {
                        if !self
                            .len_bounds
                            .iter()
                            .any(|b| b.index == lo && b.array == array)
                        {
                            self.len_bounds.push(LenBound { index: lo, array });
                            added = true;
                        }
                    }
                }
            }
            if !added {
                break;
            }
        }
    }

    fn check_expr(
        &mut self,
        expr: ExprID,
        context: SafetyBody<'_>,
        decls: &impl SafetyProgram,
    ) -> IndexInterval {
        match &context.arena[expr] {
            Expr::Int(x, _) => IndexInterval {
                min: *x,
                max: *x,
                non_zero: *x != 0,
            },
            Expr::Block(exprs) => {
                for e in exprs {
                    self.check_expr(*e, context, decls);
                }
                IndexInterval::default()
            }
            Expr::Let(_, init, _) => {
                let local = context.arena.binder(expr);
                let name = &Place::local(local);
                let init_r = self.check_expr(*init, context, decls);
                let ty = context.arena.local(local).ty;

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
                if let Some(ref src_name) = id_place(*init, context.arena) {
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
            Expr::Var(_, init, _) => {
                let local = context.arena.binder(expr);
                let name = &Place::local(local);
                let init_r = if let Some(init) = init {
                    self.check_expr(*init, context, decls)
                } else {
                    IndexInterval::default()
                };
                let ty = context.arena.local(local).ty;

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
                    if let Some(ref src_name) = id_place(*init, context.arena) {
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
            Expr::Id(_) => {
                let Some(place) = context.arena.reference(expr).and_then(reference_place) else {
                    return IndexInterval::default();
                };
                let name = &place;
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
                let initial_var_bound_count = self.var_bounds.len();

                self.match_expr(*cond, context, decls);
                self.propagate_len_bounds();

                let mut r = self.check_expr(*then_expr, context, decls);

                // Pop condition constraints before checking else branch —
                // the else branch executes when the condition is false,
                // so it must not inherit the then-branch constraints.
                while self.constraints.len() > initial_constraint_count {
                    self.constraints.pop();
                }
                self.len_bounds.truncate(initial_len_bound_count);
                self.min_len_bounds.truncate(initial_min_len_bound_count);
                self.var_bounds.truncate(initial_var_bound_count);

                if let Some(else_expr) = else_expr {
                    let else_r = self.check_expr(*else_expr, context, decls);
                    r = enclose(r, else_r);
                }

                r
            }
            Expr::ArrayIndex(array_expr, index_expr) => {
                if *array_expr >= context.arena.len() {
                    print_error_with_context(
                        context.arena.loc(expr),
                        "internal compiler error: no type found for array index expression",
                    );
                    return IndexInterval::default();
                }

                self.check_expr(*array_expr, context, decls);
                let lhs_ty = context.arena.ty(*array_expr);
                let rhs_r = self.check_expr(*index_expr, context, decls);

                if rhs_r.min < 0 {
                    self.push_error(SafetyError {
                        location: context.arena.loc(expr),
                        message: format!("couldn't prove index is >= 0"),
                    });
                }

                if let Type::Array(_, ref n) = *lhs_ty {
                    if let ArraySize::Known(n) = n {
                        let interval_ok = *n > 0 && rhs_r.max < (*n).into();
                        // Also accept a `len_bound { idx, arr }` from a require
                        // clause or `for`/`while` loop condition: this proves
                        // `idx < arr.len`, and arr.len == n for a Known array.
                        let array_name =
                            if let Some(ref name) = id_place(*array_expr, context.arena) {
                                Some(*name)
                            } else {
                                None
                            };
                        let index_name =
                            if let Some(ref name) = id_place(*index_expr, context.arena) {
                                Some(*name)
                            } else {
                                None
                            };
                        let len_bound_ok = match (index_name, array_name) {
                            (Some(idx), Some(arr)) => self
                                .len_bounds
                                .iter()
                                .any(|b| b.index == idx && b.array == arr),
                            _ => false,
                        };
                        if !interval_ok && !len_bound_ok {
                            self.push_error(SafetyError {
                                location: context.arena.loc(expr),
                                message: format!("couldn't prove index is less than array length"),
                            });
                        }
                    } else if let ArraySize::Var(size_name) = n {
                        // Symbolic size: prove `idx < size_name` via either
                        //   (a) a `len_bound` recorded by `for i in 0 .. arr.len`
                        //       or a require clause `idx < arr.len`, or
                        //   (b) a `var_bound` from `for i in 0 .. N` or a
                        //       require clause `idx < N`.
                        let array_name =
                            if let Some(ref name) = id_place(*array_expr, context.arena) {
                                Some(*name)
                            } else {
                                None
                            };
                        let index_name =
                            if let Some(ref name) = id_place(*index_expr, context.arena) {
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
                        let has_var_bound = if let Some(idx) = index_name {
                            self.var_bounds.iter().any(|b| {
                                b.lo == idx
                                    && context.size_vars.iter().any(|parameter| {
                                        parameter.symbol == *size_name
                                            && b.hi == Place::local(parameter.local)
                                    })
                            })
                        } else {
                            false
                        };
                        if !has_len_bound && !has_var_bound {
                            self.push_error(SafetyError {
                                location: context.arena.loc(expr),
                                message: format!("couldn't prove index is less than array length"),
                            });
                        }
                    }
                } else if let Type::Slice(_) = *lhs_ty {
                    // For slices, check if the index has been proven < slice.len.
                    let array_name = if let Some(ref name) = id_place(*array_expr, context.arena) {
                        Some(*name)
                    } else {
                        None
                    };
                    let index_name = if let Some(ref name) = id_place(*index_expr, context.arena) {
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
                        self.push_error(SafetyError {
                            location: context.arena.loc(expr),
                            message: format!("couldn't prove index is less than slice length"),
                        });
                    }
                } else if let Type::Float32x4 = *lhs_ty {
                    // An f32x4 is four lanes wide and has no `.len` to bound an
                    // index against, so the interval has to prove it on its own.
                    if rhs_r.max >= F32X4_LANES {
                        self.push_error(SafetyError {
                            location: context.arena.loc(expr),
                            message: format!("couldn't prove index is less than 4"),
                        });
                    }
                }

                IndexInterval::default()
            }
            Expr::While(cond, body) => {
                let saved_constraints = self.constraints.clone();
                let saved_len_bounds = self.len_bounds.clone();
                let saved_leq_len_bounds = self.leq_len_bounds.clone();
                let saved_min_len_bounds = self.min_len_bounds.clone();
                let saved_var_bounds = self.var_bounds.clone();
                self.match_expr(*cond, context, decls);

                self.check_expr(*body, context, decls);
                self.constraints = saved_constraints;
                self.len_bounds = saved_len_bounds;
                self.leq_len_bounds = saved_leq_len_bounds;
                self.min_len_bounds = saved_min_len_bounds;
                self.var_bounds = saved_var_bounds;

                // Invalidate constraints for variables assigned inside the loop.
                // The restore gives us pre-loop state, but mutations in the body
                // mean those constraints may not hold at loop exit.
                self.invalidate_assigned(*body, context.arena);

                IndexInterval::default()
            }
            Expr::Binop(op, lhs, rhs) => {
                if *op == Binop::Plus {
                    let lhs_range = self.check_expr(*lhs, context, decls);
                    let rhs_range = self.check_expr(*rhs, context, decls);
                    return lhs_range + rhs_range;
                }

                if *op == Binop::Minus {
                    let lhs_range = self.check_expr(*lhs, context, decls);
                    let rhs_range = self.check_expr(*rhs, context, decls);
                    return lhs_range - rhs_range;
                }

                if *op == Binop::Mult {
                    let lhs_range = self.check_expr(*lhs, context, decls);
                    let rhs_range = self.check_expr(*rhs, context, decls);
                    return lhs_range * rhs_range;
                }

                if *op == Binop::Div || *op == Binop::Mod {
                    let lhs_range = self.check_expr(*lhs, context, decls);
                    let rhs_range = self.check_expr(*rhs, context, decls);

                    // Only check integer division — float div-by-zero produces Inf/NaN per IEEE 754.
                    if *rhs < context.arena.len() {
                        let ty = context.arena.ty(*rhs);
                        let is_int =
                            matches!(*ty, Type::Int32 | Type::UInt32 | Type::Int8 | Type::UInt8);
                        if is_int && !rhs_range.excludes_zero() {
                            self.push_error(SafetyError {
                                location: context.arena.loc(expr),
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
                    self.check_expr(*lhs, context, decls);
                    let rhs_range = self.check_expr(*rhs, context, decls);

                    if let Some(name) = expr_place(*lhs, context.arena) {
                        if rhs_range != IndexInterval::default() {
                            self.replace(name, Some(rhs_range.min), Some(rhs_range.max));
                        } else {
                            self.replace(name, None, None);
                        }
                    }

                    return IndexInterval::default();
                }

                // For other binops (==, !=, <, >, etc.), still recurse
                // into sub-expressions to check array accesses.
                self.check_expr(*lhs, context, decls);
                self.check_expr(*rhs, context, decls);
                IndexInterval::default()
            }
            Expr::Call(callee_expr, args) => {
                let arg_ivals: Vec<_> = args
                    .iter()
                    .map(|arg| self.check_expr(*arg, context, decls))
                    .collect();
                // An immediately-invoked lambda has known arguments, so check
                // its body against them rather than unconstrained.
                if let Expr::Lambda { body, .. } = &context.arena[*callee_expr] {
                    let params = context.arena.binders(*callee_expr);
                    self.check_lambda_body(params, *body, Some((args, &arg_ivals)), context, decls);
                }
                self.check_call_requires(*callee_expr, args, expr, context, decls);
                IndexInterval::default()
            }
            Expr::Unop(op, expr) => {
                let r = self.check_expr(*expr, context, decls);
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
                self.check_expr(*expr, context, decls);
                IndexInterval::default()
            }
            Expr::Assume(cond) => {
                // Inject constraints from the condition without scoping —
                // they persist for the rest of the function.
                self.match_expr(*cond, context, decls);
                self.propagate_len_bounds();
                IndexInterval::default()
            }
            Expr::Field(base, field) => {
                // `[T; N].len` is the constant N. A fixed-size array knows its
                // length statically, so a guard like `i < buf.len` carries the
                // same information as `i < N`. This holds for any expression
                // of sized-array type, including an element of a nested array
                // such as `buffers[outer]`.
                if field.as_str() == "len" {
                    if let Some(n) = static_len(*base, context, decls) {
                        return IndexInterval {
                            min: n,
                            max: n,
                            non_zero: n != 0,
                        };
                    }
                }

                // Look up facts by the field's identified root and projection.
                if let Some(name) = expr_place(expr, context.arena) {
                    let mut min = i64::min_value();
                    let mut max = i64::max_value();
                    let mut non_zero = false;
                    for c in &self.constraints {
                        if c.name == name {
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
                } else {
                    IndexInterval::default()
                }
            }
            Expr::For {
                start, end, body, ..
            } => {
                let var = &Place::local(context.arena.binder(expr));
                let start_r = self.check_expr(*start, context, decls);
                let end_r = self.check_expr(*end, context, decls);

                // Keep the existing loop transfer rule: restore entry facts
                // after visiting the body, then invalidate its assigned roots.
                // The loop binding has its own LocalId throughout.
                let saved_constraints = self.constraints.clone();
                let saved_len_bounds = self.len_bounds.clone();
                let saved_leq_len_bounds = self.leq_len_bounds.clone();
                let saved_min_len_bounds = self.min_len_bounds.clone();
                let saved_var_bounds = self.var_bounds.clone();
                self.add(*var, Some(start_r.min), Some(end_r.max.saturating_sub(1)));
                // for i in 0 .. arr.len — record that i < arr.len
                if let Expr::Field(arr_expr, field_name) = &context.arena[*end] {
                    if field_name.as_str() == "len" {
                        if let Some(ref array_name) = id_place(*arr_expr, context.arena) {
                            self.len_bounds.push(LenBound {
                                index: *var,
                                array: *array_name,
                            });
                        }
                    }
                }
                // for i in lo .. hi where hi has a LenBound — transitive bound
                if let Some(ref end_name) = id_place(*end, context.arena) {
                    self.var_bounds.push(VarBound {
                        lo: *var,
                        hi: *end_name,
                    });
                    self.propagate_len_bounds();
                }
                // Restoring the snapshot after the body also undoes mutations
                // inside the loop (e.g. `i = i + 1`), so they don't clobber the
                // constraints of outer variables after the loop exits.
                self.check_expr(*body, context, decls);
                self.constraints = saved_constraints.clone();
                self.len_bounds = saved_len_bounds.clone();
                self.leq_len_bounds = saved_leq_len_bounds;
                self.min_len_bounds = saved_min_len_bounds;
                self.var_bounds = saved_var_bounds;

                // Invalidate constraints for variables assigned inside the loop.
                self.invalidate_assigned(*body, context.arena);

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
                let start_name = if let Some(ref n) = id_place(*start, context.arena) {
                    Some(*n)
                } else {
                    None
                };
                let mut assigned = Vec::new();
                Self::collect_assigned_vars(*body, context.arena, &mut assigned);
                for name in assigned {
                    if !Self::is_monotonic_increment(name, *body, context.arena) {
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
                        context.arena.ids().any(|id| {
                            if let Expr::Var(_, Some(init), _) = &context.arena[id] {
                                Place::local(context.arena.binder(id)) == name
                                    && id_place(*init, context.arena) == Some(sn)
                            } else {
                                false
                            }
                        })
                    });
                    if initialized_from_start {
                        if let Some(ref end_name) = id_place(*end, context.arena) {
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
                    self.check_expr(*e, context, decls);
                }
                IndexInterval::default()
            }
            Expr::Lambda { body, .. } => {
                // Nothing is known about the arguments at the definition site,
                // so the body is checked with its parameters unconstrained.
                // (A directly-called lambda is handled by the `Call` arm, which
                // knows the arguments.)
                let params = context.arena.binders(expr);
                self.check_lambda_body(params, *body, None, context, decls);
                IndexInterval::default()
            }
            Expr::Tuple(exprs) => {
                for e in exprs {
                    self.check_expr(*e, context, decls);
                }
                IndexInterval::default()
            }
            Expr::StructLit(_, fields) => {
                for (_, e) in fields {
                    self.check_expr(*e, context, decls);
                }
                IndexInterval::default()
            }
            Expr::AsTy(e, _) | Expr::Arena(e) => {
                self.check_expr(*e, context, decls);
                IndexInterval::default()
            }
            _ => IndexInterval::default(),
        }
    }

    /// Check a lambda body with its identified parameters and captures.
    /// Direct calls supply argument intervals and symbolic length bounds;
    /// at the definition site parameter values are unknown.
    fn check_lambda_body(
        &mut self,
        params: &[LocalId],
        body: ExprID,
        call_args: Option<(&[ExprID], &[IndexInterval])>,
        context: SafetyBody<'_>,
        decls: &impl SafetyProgram,
    ) {
        let saved_constraints = self.constraints.clone();
        let saved_len_bounds = self.len_bounds.clone();
        let saved_leq_len_bounds = self.leq_len_bounds.clone();
        let saved_min_len_bounds = self.min_len_bounds.clone();
        let saved_var_bounds = self.var_bounds.clone();

        // A lambda that isn't invoked right here runs at some unknown later
        // point, so any capture the enclosing function assigns to — before or
        // after the definition, including from inside this body — may hold a
        // different value by then. Drop what we know about those. An
        // immediately-invoked lambda is exempt: the definition site *is* the
        // call site, so the current state is exact.
        if call_args.is_none() {
            for name in self.fn_assigned.clone() {
                self.forget(name);
            }
        }

        for (i, &param) in params.iter().enumerate() {
            let ty = context.arena.local(param).ty;
            let is_u32 = ty == mk_type(Type::UInt32);

            // A repeated analysis of this lambda starts with fresh parameter facts.
            self.forget(Place::local(param));

            let arg = call_args.and_then(|(exprs, ivals)| Some((exprs.get(i)?, ivals.get(i)?)));
            match arg {
                Some((arg_expr, ival)) => {
                    let mut min = (ival.min != i64::MIN).then_some(ival.min);
                    let max = (ival.max != i64::MAX).then_some(ival.max);
                    if is_u32 {
                        min = Some(min.unwrap_or(0).max(0));
                    }
                    self.add(Place::local(param), min, max);
                    if ival.non_zero {
                        self.add_non_zero(Place::local(param));
                    }
                    // The param inherits the argument's symbolic length bounds.
                    // Read the live state so bounds from earlier parameters
                    // can propagate. Outer captured roots retain their IDs.
                    if let Some(ref arg_name) = id_place(*arg_expr, context.arena) {
                        let inherited: Vec<_> = self
                            .len_bounds
                            .iter()
                            .filter(|b| b.index == *arg_name)
                            .map(|b| b.array)
                            .collect();
                        for array in inherited {
                            self.len_bounds.push(LenBound {
                                index: Place::local(param),
                                array,
                            });
                        }
                    }
                }
                None if is_u32 => self.add(Place::local(param), Some(0), None),
                None => self.add(Place::local(param), None, None),
            }
        }

        self.check_expr(body, context, decls);

        self.constraints = saved_constraints;
        self.len_bounds = saved_len_bounds;
        self.leq_len_bounds = saved_leq_len_bounds;
        self.min_len_bounds = saved_min_len_bounds;
        self.var_bounds = saved_var_bounds;

        // Assignments in the body take effect whenever the lambda is called,
        // which we can't pin down, so conservatively drop what we knew about
        // the variables it writes to.
        self.invalidate_assigned(body, context.arena);
    }

    /// Check if every assignment to `var_name` in the expression tree is of the
    /// form `var_name = var_name + 1` (monotonic increment by 1). Returns false
    /// if the variable is assigned in any other way.
    ///
    /// Walks the whole tree, like `collect_assigned_vars`: the two have to
    /// agree about where an assignment can hide, or the loop-exit min bound
    /// gets handed back on the strength of an increment that isn't the only
    /// write.
    fn is_monotonic_increment(var_name: Place, expr: ExprID, arena: &ExprArena) -> bool {
        if let Expr::Binop(Binop::Assign, lhs, rhs) = &arena[expr] {
            if let Some(ref name) = id_place(*lhs, arena) {
                if *name == var_name {
                    // Check rhs is `var_name + 1`
                    if let Expr::Binop(Binop::Plus, plus_lhs, plus_rhs) = &arena[*rhs] {
                        let lhs_is_var = id_place(*plus_lhs, arena) == Some(var_name);
                        let rhs_is_one = matches!(&arena[*plus_rhs], Expr::Int(1, _));
                        return lhs_is_var && rhs_is_one;
                    }
                    return false; // Some other assignment to var_name
                }
            }
        }
        arena[expr]
            .subexprs()
            .iter()
            .all(|child| Self::is_monotonic_increment(var_name, *child, arena))
    }

    /// Collect all direct storage roots assigned (via `=`) inside an
    /// expression tree.
    ///
    /// Walks every subexpression, including lambda bodies, initializers and
    /// call arguments — an assignment nested in any of those still happens, and
    /// missing one would leave a stale constraint in place.
    fn collect_assigned_vars(expr: ExprID, arena: &ExprArena, out: &mut Vec<Place>) {
        if let Expr::Binop(Binop::Assign, lhs, _) = &arena[expr] {
            if let Some(ref name) = id_place(*lhs, arena) {
                out.push(*name);
            }
        }
        for child in arena[expr].subexprs() {
            Self::collect_assigned_vars(child, arena, out);
        }
    }

    /// Invalidate constraints for variables that were assigned inside a loop body.
    /// After save/restore, the restored constraints reflect pre-loop state, but
    /// any variable mutated inside the loop could hold a different value at exit.
    fn invalidate_assigned(&mut self, body: ExprID, arena: &ExprArena) {
        let mut assigned = Vec::new();
        Self::collect_assigned_vars(body, arena, &mut assigned);
        for name in assigned {
            self.constraints.retain(|c| c.name != name);
            self.len_bounds.retain(|b| b.index != name);
            self.var_bounds.retain(|b| b.lo != name);
        }
    }

    /// Inject each assumption's facts with its own body-local coordinates.
    fn inject_global_assumes(&mut self, decls: &impl SafetyProgram) {
        for decl in &decls.decls {
            if let Decl::Assume { arena, cond } = decl {
                self.match_expr(
                    *cond,
                    SafetyBody {
                        arena,
                        size_vars: &[],
                    },
                    decls,
                );
                self.propagate_len_bounds();
                // Only nonlocal facts cross this body boundary. A local with the
                // same numeric ID in another assumption or function is unrelated.
                let nonlocal = |place: Place| !matches!(place.root, PlaceRoot::Local(_));
                self.constraints.retain(|c| nonlocal(c.name));
                self.len_bounds
                    .retain(|b| nonlocal(b.index) && nonlocal(b.array));
                self.leq_len_bounds
                    .retain(|b| nonlocal(b.index) && nonlocal(b.array));
                self.min_len_bounds.retain(|b| nonlocal(b.array));
                self.var_bounds.retain(|b| nonlocal(b.lo) && nonlocal(b.hi));
            }
        }
    }

    /// Check a direct call using the phase's target policy. Concrete instances
    /// never undergo another signature-based selection; indirect calls remain
    /// deferred. Explicit type applications become instance reads in specialization.
    fn check_call_requires(
        &mut self,
        callee_expr: ExprID,
        args: &[ExprID],
        call_expr: ExprID,
        caller: SafetyBody<'_>,
        decls: &impl SafetyProgram,
    ) {
        if !matches!(caller.arena[callee_expr], Expr::Id(_)) {
            return;
        }
        let Some(reference) = caller.arena.reference(callee_expr) else {
            return;
        };
        let Some(callee) = decls.call_target(
            reference,
            caller.arena,
            caller.arena.ty(callee_expr),
            args.len(),
        ) else {
            return;
        };
        if callee.requires.is_empty() {
            return;
        }
        let subst: Vec<(LocalId, ExprID)> = callee
            .params
            .iter()
            .zip(args)
            .map(|(parameter, &argument)| (parameter.local, argument))
            .collect();
        let mut size_subst = Vec::new();
        for (parameter, &argument) in callee.params.iter().zip(args) {
            collect_size_subst(
                callee.arena.local(parameter.local).ty,
                caller.arena.ty(argument),
                &mut size_subst,
            );
        }
        for &requirement in &callee.requires {
            if !self.prove_at_call(requirement, callee, caller, &subst, &size_subst, decls) {
                let clause = callee.arena.pretty_print(requirement, 0);
                let location = caller.arena.loc(call_expr);
                let key = (location, callee.arena.loc(requirement), clause.clone());
                if let Some(message) = self.failed_requirements.get(&key) {
                    // The diagnostic list is public. Clearing it must allow a
                    // reused checker to report the same requirement again.
                    if self.errors.iter().any(|error| {
                        error.location == location && error.message == *message
                    }) {
                        continue;
                    }
                }
                let message = format!(
                    "couldn't prove require clause `{}` for call to `{}`",
                    clause, callee.name
                );
                self.failed_requirements.insert(key, message.clone());
                self.push_error(SafetyError {
                    location,
                    message,
                });
            }
        }
    }

    /// Evaluate a callee-only expression with a fresh analysis state. Body-local
    /// IDs have different owners in caller and callee; caller facts cannot be
    /// applied to a coincidentally equal callee LocalId.
    fn callee_interval(
        expr: ExprID,
        callee: &FuncDecl,
        decls: &impl SafetyProgram,
    ) -> IndexInterval {
        Self::new().check_expr(expr, callee.into(), decls)
    }

    /// Preserve the small existing precondition proof grammar, translating
    /// parameter references across the call boundary by LocalId.
    fn prove_at_call(
        &mut self,
        req: ExprID,
        callee: &FuncDecl,
        caller: SafetyBody<'_>,
        subst: &[(LocalId, ExprID)],
        size_subst: &[(Name, i64)],
        decls: &impl SafetyProgram,
    ) -> bool {
        let lookup = |reference: &Reference| -> Option<ExprID> {
            let Reference::Local(local) = reference else {
                return None;
            };
            subst
                .iter()
                .find(|(parameter, _)| parameter == local)
                .map(|(_, argument)| *argument)
        };
        let size_lookup = |reference: &Reference| -> Option<i64> {
            let Reference::SizeParameter(local) = reference else {
                return None;
            };
            let symbol = callee
                .size_vars
                .iter()
                .find(|parameter| parameter.local == *local)?
                .symbol;
            size_subst
                .iter()
                .find(|(parameter, _)| *parameter == symbol)
                .map(|(_, value)| *value)
        };
        match &callee.arena[req] {
            Expr::True => true,
            Expr::Binop(Binop::And, lhs, rhs) => {
                self.prove_at_call(*lhs, callee, caller, subst, size_subst, decls)
                    && self.prove_at_call(*rhs, callee, caller, subst, size_subst, decls)
            }
            Expr::Binop(Binop::Less, lhs, rhs) => {
                if let (Expr::Id(_), Expr::Field(array, field)) =
                    (&callee.arena[*lhs], &callee.arena[*rhs])
                {
                    if field.as_str() == "len" {
                        if let (Expr::Id(_), Some(index), Some(array)) = (
                            &callee.arena[*array],
                            callee.arena.reference(*lhs),
                            callee.arena.reference(*array),
                        ) {
                            if let (Some(index_arg), Some(array_arg)) =
                                (lookup(index), lookup(array))
                            {
                                if let (Some(index), Some(array)) = (
                                    id_place(index_arg, caller.arena),
                                    id_place(array_arg, caller.arena),
                                ) {
                                    if self
                                        .len_bounds
                                        .iter()
                                        .any(|bound| bound.index == index && bound.array == array)
                                    {
                                        return true;
                                    }
                                }
                                if let Some(length) = static_len(array_arg, caller, decls) {
                                    let interval = self.check_expr(index_arg, caller, decls);
                                    if interval.max != i64::MAX && interval.max < length {
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                }
                if let (Expr::Id(_), Expr::Id(_)) = (&callee.arena[*lhs], &callee.arena[*rhs]) {
                    if let (Some(argument), Some(size)) = (
                        callee.arena.reference(*lhs).and_then(lookup),
                        callee.arena.reference(*rhs).and_then(size_lookup),
                    ) {
                        let interval = self.check_expr(argument, caller, decls);
                        if interval.max != i64::MAX && interval.max < size {
                            return true;
                        }
                    }
                }
                let left = if let Expr::Id(_) = &callee.arena[*lhs] {
                    callee
                        .arena
                        .reference(*lhs)
                        .and_then(lookup)
                        .map(|argument| self.check_expr(argument, caller, decls))
                } else {
                    Some(Self::callee_interval(*lhs, callee, decls))
                };
                let right = if let Expr::Id(_) = &callee.arena[*rhs] {
                    callee
                        .arena
                        .reference(*rhs)
                        .and_then(lookup)
                        .map(|argument| self.check_expr(argument, caller, decls))
                } else {
                    Some(Self::callee_interval(*rhs, callee, decls))
                };
                match (left, right) {
                    (Some(left), Some(right)) => {
                        left.max != i64::MAX && right.min != i64::MIN && left.max < right.min
                    }
                    _ => false,
                }
            }
            Expr::Binop(Binop::Geq, lhs, rhs) => {
                if let Expr::Id(_) = &callee.arena[*lhs] {
                    if let Some(argument) = callee.arena.reference(*lhs).and_then(lookup) {
                        let argument = self.check_expr(argument, caller, decls);
                        let rhs = Self::callee_interval(*rhs, callee, decls);
                        return argument.min != i64::MIN
                            && rhs.max != i64::MAX
                            && argument.min >= rhs.max;
                    }
                }
                false
            }
            _ => false,
        }
    }

    fn check_fn_decl(&mut self, func_decl: &FuncDecl, decls: &impl SafetyProgram) {
        if let Some(body) = func_decl.body {
            // Inject top-level assume constraints before checking the function.
            self.inject_global_assumes(decls);

            for param in &func_decl.params {
                if func_decl.arena.local(param.local).ty == mk_type(Type::UInt32) {
                    self.add(Place::local(param.local), Some(0), None);
                } else {
                    self.add(Place::local(param.local), None, None);
                }
            }

            // Size variables are i32-valued symbolic constants in the body.
            // Treat them as non-negative (array sizes are always >= 1) so the
            // checker can reason about `for i in 0 .. N` and `arr[idx]` for
            // `[T; N]` parameters. The body still has to prove `idx < N` via
            // a for-loop, require clause, or local check.
            for parameter in &func_decl.size_vars {
                self.add(Place::local(parameter.local), Some(1), None);
            }

            // Inject require clauses as assumptions inside the function body.
            // The clauses live in this checked body and reference parameter IDs.
            for &req in &func_decl.requires {
                self.match_expr(req, func_decl.into(), decls);
            }
            if !func_decl.requires.is_empty() {
                self.propagate_len_bounds();
            }

            // Lambda bodies are checked against this to decide which captures
            // are still trustworthy, so it has to cover the whole function.
            self.fn_assigned.clear();
            Self::collect_assigned_vars(body, &func_decl.arena, &mut self.fn_assigned);

            self.check_expr(body, func_decl.into(), decls);

            self.constraints.clear();
            self.len_bounds.clear();
            self.leq_len_bounds.clear();
            self.min_len_bounds.clear();
            self.var_bounds.clear();
            self.fn_assigned.clear();
        }
    }

    pub fn check_decl(&mut self, decl: &CheckedDecl, decls: &impl SafetyProgram) {
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

    pub fn check(&mut self, decls: &impl SafetyProgram) {
        for decl in &decls.decls {
            self.check_decl(decl, decls);
        }
    }

    /// Reject cycles in checked direct calls, inline lambda calls and the
    /// conservative graph of indirect calls to address-taken functions.
    /// Lexical shadowing has already been resolved; graph construction never
    /// reconstructs bindings from function or local spellings.
    pub fn check_recursion(&mut self, program: &CheckedProgram) {
        use crate::scc::{scc_is_cycle, strongly_connected_components};
        use std::collections::{HashMap, HashSet};

        #[derive(Clone, Copy)]
        struct Node {
            definition: DefId,
            lambda: Option<ExprID>,
        }
        struct Graph {
            nodes: Vec<Node>,
            top: HashMap<DefId, usize>,
            lambdas: HashMap<(DefId, ExprID), usize>,
            calls: Vec<Vec<ExprID>>,
            address_taken: HashSet<DefId>,
        }
        fn definitions(reference: &Reference, body: &CheckedBody) -> Vec<DefId> {
            match reference {
                Reference::Functions(ids) => ids.clone(),
                Reference::InterfaceMember {
                    requirement,
                    member,
                } => body
                    .requirements
                    .get(requirement.index())
                    .and_then(|requirement| {
                        requirement
                            .members
                            .iter()
                            .find(|candidate| candidate.definition == *member)
                    })
                    .map(|member| member.candidates.clone())
                    .unwrap_or_default(),
                _ => vec![],
            }
        }
        fn walk(
            expression: ExprID,
            body: &CheckedBody,
            definition: DefId,
            current: usize,
            in_callee: bool,
            graph: &mut Graph,
        ) {
            match &body[expression] {
                Expr::Id(_) | Expr::TypeApp(_, _) => {
                    if !in_callee {
                        if let Some(reference) = body.reference(expression) {
                            graph.address_taken.extend(definitions(reference, body));
                        }
                    }
                }
                Expr::Lambda {
                    body: lambda_body, ..
                } => {
                    let node = graph.nodes.len();
                    graph.nodes.push(Node {
                        definition,
                        lambda: Some(expression),
                    });
                    graph.calls.push(Vec::new());
                    graph.lambdas.insert((definition, expression), node);
                    walk(*lambda_body, body, definition, node, false, graph);
                }
                Expr::Call(callee, arguments) => {
                    graph.calls[current].push(expression);
                    walk(*callee, body, definition, current, true, graph);
                    for &argument in arguments {
                        walk(argument, body, definition, current, false, graph);
                    }
                }
                expression => {
                    for child in expression.subexprs() {
                        walk(child, body, definition, current, false, graph);
                    }
                }
            }
        }
        let mut graph = Graph {
            nodes: Vec::new(),
            top: HashMap::new(),
            lambdas: HashMap::new(),
            calls: Vec::new(),
            address_taken: HashSet::new(),
        };
        for (coordinate, declaration) in program.decls.decls.iter().enumerate() {
            if let Decl::Func(function) = declaration {
                if function.body.is_some() {
                    let definition = program.decls.id_at(coordinate);
                    graph.top.insert(definition, graph.nodes.len());
                    graph.nodes.push(Node {
                        definition,
                        lambda: None,
                    });
                    graph.calls.push(Vec::new());
                }
            }
        }
        let roots = graph.nodes.clone();
        for (node, root) in roots.iter().enumerate() {
            let function = program.function(root.definition).unwrap();
            walk(
                function.body.unwrap(),
                &function.arena,
                root.definition,
                node,
                false,
                &mut graph,
            );
        }
        let address_taken: Vec<_> = graph
            .nodes
            .iter()
            .enumerate()
            .filter_map(|(index, node)| {
                (node.lambda.is_some() || graph.address_taken.contains(&node.definition))
                    .then_some(index)
            })
            .collect();
        let mut adjacency = vec![Vec::new(); graph.nodes.len()];
        for (index, node) in graph.nodes.iter().enumerate() {
            let function = program.function(node.definition).unwrap();
            let mut targets = HashSet::new();
            for &call in &graph.calls[index] {
                let Expr::Call(callee, _) = &function.arena[call] else {
                    unreachable!();
                };
                let direct = match &function.arena[*callee] {
                    Expr::Id(_) | Expr::TypeApp(_, _) => {
                        let definitions = function
                            .arena
                            .reference(*callee)
                            .map(|reference| definitions(reference, &function.arena))
                            .unwrap_or_default();
                        let mut found = false;
                        for definition in definitions {
                            if program.function(definition).is_some() {
                                found = true;
                                if let Some(&target) = graph.top.get(&definition) {
                                    targets.insert(target);
                                }
                            }
                        }
                        found
                    }
                    Expr::Lambda { .. } => {
                        if let Some(&target) = graph.lambdas.get(&(node.definition, *callee)) {
                            targets.insert(target);
                            true
                        } else {
                            false
                        }
                    }
                    _ => false,
                };
                if !direct {
                    targets.extend(address_taken.iter().copied());
                }
            }
            adjacency[index] = targets.into_iter().collect();
        }
        let describe = |index: usize| {
            let node = graph.nodes[index];
            let function = program.function(node.definition).unwrap();
            match node.lambda {
                Some(expression) => (
                    function.arena.loc(expression),
                    format!("lambda in `{}`", function.name),
                ),
                None => (function.loc, format!("function `{}`", function.name)),
            }
        };
        for component in strongly_connected_components(&adjacency) {
            if !scc_is_cycle(&component, &adjacency) {
                continue;
            }
            if component.len() == 1 {
                let (location, description) = describe(component[0]);
                self.push_error(SafetyError {
                    location,
                    message: format!("--no-recursion: {} is recursive", description),
                });
            } else {
                let descriptions: Vec<_> = component.iter().map(|&node| describe(node).1).collect();
                let cycle = descriptions.join(", ");
                for node in component {
                    let (location, description) = describe(node);
                    self.push_error(SafetyError {
                        location,
                        message: format!(
                            "--no-recursion: {} participates in a recursive cycle [{}]",
                            description, cycle
                        ),
                    });
                }
            }
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

    fn checked_source(s: &str) -> CheckedProgram {
        let mut errors = vec![];
        let decls = parse_program_str(&s, &mut errors);
        assert!(errors.is_empty());
        let table = DeclTable::new(decls);
        let check_function = |function: crate::FuncDecl| {
            let mut checker = Checker::new();
            checker.check_decl(&Decl::Func(function.clone()), &table);
            assert!(checker.errors.is_empty());
            checker.checked_function(&function)
        };
        CheckedProgram::new(table.map_bodies(
            |_, function| check_function(function),
            |arena, cond| {
                let mut checker = Checker::new();
                checker.check_decl(
                    &Decl::Assume {
                        arena: arena.clone(),
                        cond,
                    },
                    &table,
                );
                assert!(checker.errors.is_empty(), "{:?}", checker.errors);
                checker.checked_body(&arena)
            },
        ))
    }

    pub fn check(s: &str) -> Vec<SafetyError> {
        let checked = checked_source(s);
        let mut array_checker = SafetyChecker::new();
        array_checker.check(&checked);

        array_checker.print_errors();

        array_checker.errors
    }

    #[test]
    fn concrete_requirements_use_the_instance_despite_coercing_node_signatures() {
        for (definition, main, recorded_parameter) in [
            (
                "bounded(x: i32, values: [i32]) require x >= 0 {}",
                "bounded(-1, [1, 2])",
                "array",
            ),
            (
                "bounded(x: &i32) require x >= 0 {}",
                "var x = -1; bounded(x)",
                "value",
            ),
        ] {
            let templates = checked_source(&format!("{} main {{ {} }}", definition, main));
            let mut program = MonomorphPass::new()
                .monomorphize(&templates, Name::str("main"))
                .unwrap();
            let main = program.instance_for_entry(Name::str("main")).unwrap();
            let declaration = program.instances[main.index()].declaration;
            let Decl::Func(caller) = &mut program.decls.decls[declaration] else {
                unreachable!()
            };
            let callee = caller
                .arena
                .exprs()
                .iter()
                .find_map(|expr| {
                    if let Expr::Call(callee, _) = expr {
                        Some(*callee)
                    } else {
                        None
                    }
                })
                .unwrap();
            let Some(&Reference::Instance(target)) = caller.arena.reference(callee) else {
                unreachable!()
            };
            let integer = mk_type(Type::Int32);
            let parameters = if recorded_parameter == "array" {
                vec![integer, mk_type(Type::Array(integer, ArraySize::Known(2)))]
            } else {
                vec![integer]
            };
            let recorded = mk_type(Type::Func(
                mk_type(Type::Tuple(parameters)),
                mk_type(Type::Void),
            ));
            caller.arena.set_ty(callee, recorded);
            let actual = program.function_instance(target).unwrap().ty();
            assert_ne!(actual, recorded);
            assert!(unify(actual, recorded, &mut Instance::new()));
            program.validate().unwrap();

            let mut safety = SafetyChecker::new();
            for _ in 0..2 {
                safety.errors.clear();
                safety.check(&program);
                assert_eq!(safety.errors.len(), 1);
                assert!(safety.errors[0].message.contains("`x >= 0`"));
            }
        }
    }

    #[test]
    fn shadowed_field_constraints_do_not_escape_to_the_outer_binding() {
        let errors = check("struct P { x: i32 } f { var a: [i32; 3]; var p: P; p.x = 100; if true { var p: P; p.x = 0; a[p.x] }; a[p.x] }");
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("less than array length"));
    }

    #[test]
    fn local_callee_does_not_inherit_a_same_named_function_requirement() {
        let errors =
            check("bounded(x: i32) require x >= 0 {} f { let bounded = |x: i32| {}; bounded(-1) }");
        assert!(errors.is_empty());
    }

    #[test]
    fn callee_local_ids_cannot_observe_caller_constraint_slots() {
        let errors = check("bounded(x: i32, y: i32) require x >= (y + 1) {} f { let value = 0; let unrelated = -100; bounded(0, 100) }");
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("couldn't prove require clause"));
    }

    #[test]
    fn recursion_graph_uses_direct_bindings_even_when_a_sibling_scope_shadows_the_name() {
        let checked = checked_source("f { if true { let f = |x: i32| {} }; f() }");
        let mut safety = SafetyChecker::new();
        safety.check_recursion(&checked);
        assert_eq!(safety.errors.len(), 1);
        assert!(safety.errors[0]
            .message
            .contains("function `f` is recursive"));
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

    #[test]
    pub fn test_lambda_index_unconstrained() {
        let s = "
        f() {
            var a: [i32; 100]
            var g = (|i: i32| a[i])
        }
        ";

        let errors = check(s);
        // Nothing is known about i at the definition site.
        assert_eq!(errors.len(), 2);
    }

    #[test]
    pub fn test_lambda_index_guarded() {
        let s = "
        f() {
            var a: [i32; 100]
            var g = (|i| if i >= 0 && i < 100 { a[i] } else { 0 })
        }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_lambda_assign_index_unconstrained() {
        let s = "
        f() {
            var a: [i32; 100]
            var g = (|i: i32| a[i] = 7)
        }
        ";

        let errors = check(s);
        assert_eq!(errors.len(), 2);
    }

    #[test]
    pub fn test_lambda_div_by_param() {
        let s = "
        f() {
            var g = (|d| 100 / d)
        }
        ";

        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_lambda_div_guarded() {
        let s = "
        f() {
            var g = (|d| if d != 0 { 100 / d } else { 0 })
        }
        ";

        let errors = check(s);
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_lambda_captures_constraint() {
        let s = "
        f() {
            var a: [i32; 100]
            let i = 5
            var g = (| | a[i])
        }
        ";

        let errors = check(s);
        // The captured i keeps the interval it has at the definition site.
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_lambda_param_shadows_capture() {
        let s = "
        f() {
            var a: [i32; 100]
            let i = 5
            var g = (|i: i32| a[i])
        }
        ";

        let errors = check(s);
        // The param shadows the captured i, so its bounds don't apply.
        assert_eq!(errors.len(), 2);
    }

    #[test]
    pub fn test_lambda_immediate_call_safe_arg() {
        let s = "
        f() {
            var a: [i32; 100]
            let x = (|i| a[i])(5)
        }
        ";

        let errors = check(s);
        // A direct call gives the param a known interval.
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_lambda_immediate_call_unsafe_arg() {
        let s = "
        f() {
            var a: [i32; 100]
            let x = (|i| a[i])(1000)
        }
        ";

        let errors = check(s);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_lambda_nested_unconstrained() {
        let s = "
        f() {
            var a: [i32; 100]
            var g = (|i: i32| (|j| a[j])(i))
        }
        ";

        let errors = check(s);
        // The inner lambda is called with the outer's unconstrained param.
        assert_eq!(errors.len(), 2);
    }

    #[test]
    pub fn test_lambda_body_assignment_invalidates_capture() {
        let s = "
        f() {
            var a: [i32; 100]
            var i = 5
            var g = (| | i = 1000)
            a[i]
        }
        ";

        let errors = check(s);
        // Calling g could have changed i, so its interval no longer holds.
        assert_eq!(errors.len(), 2);
    }

    #[test]
    pub fn test_lambda_u32_param_non_negative() {
        let s = "
        f() {
            var a: [i32; 100]
            var g = (|i: u32| a[i])
        }
        ";

        let errors = check(s);
        // u32 is known to be >= 0, so only the upper bound is unproven.
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_lambda_capture_assigned_after_definition_div() {
        let s = "
        f() {
            var d = 1
            var g = (| | 100 / d)
            d = 0
        }
        ";

        let errors = check(s);
        // g runs at an unknown time, and d is 0 by then.
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_lambda_capture_assigned_after_definition_index() {
        let s = "
        f() {
            var a: [i32; 100]
            var i = 5
            var g = (| | a[i])
            i = 1000
        }
        ";

        let errors = check(s);
        assert_eq!(errors.len(), 2);
    }

    #[test]
    pub fn test_lambda_capture_unassigned_keeps_constraint() {
        let s = "
        f() {
            var a: [i32; 100]
            var i = 5
            var g = (| | a[i])
            let j = i
        }
        ";

        let errors = check(s);
        // i is never assigned, so the definition-site interval still holds.
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_lambda_assignment_seen_through_enclosing_loop() {
        let s = "
        f() {
            var a: [i32; 100]
            var i = 5
            for k in 0 .. 10 {
                var g = (| | i = 1000)
            }
            let x = a[i]
        }
        ";

        let errors = check(s);
        // The loop's invalidation has to reach the assignment inside the lambda.
        assert_eq!(errors.len(), 2);
    }

    #[test]
    pub fn test_lambda_self_assigned_capture_unconstrained() {
        let s = "
        f() {
            var a: [i32; 100]
            var i = 5
            var g = (| | { let x = a[i]
                           i = 1000 })
        }
        ";

        let errors = check(s);
        // g can be called more than once, so i is not [5, 5] in the body.
        assert_eq!(errors.len(), 2);
    }

    #[test]
    pub fn test_lambda_len_bound_survives_shadowing_param() {
        let s = "
        f() {
            var small: [i32; 4]
            var a: [i32; 100]
            for j in 0 .. a.len {
                let z = (|a: [i32; 4], i: i32| a[i])(small, j)
            }
        }
        ";

        let errors = check(s);
        // `j < a.len` is about the 100-element a, not the 4-element param.
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_lambda_len_bound_inherited_by_param() {
        let s = "
        f() {
            var a: [i32; 100]
            for j in 0 .. a.len {
                let z = (|i| a[i])(j)
            }
        }
        ";

        let errors = check(s);
        // The param inherits `j < a.len` from the argument.
        assert!(errors.is_empty());
    }

    #[test]
    pub fn test_lambda_in_tuple_is_checked() {
        let s = "
        f() {
            var a: [i32; 4]
            let t = (1, (|i: i32| a[i]))
        }
        ";

        let errors = check(s);
        assert_eq!(errors.len(), 2);
    }

    #[test]
    pub fn test_index_under_as_ty_is_checked() {
        let s = "
        f() {
            var a: [i32; 4]
            let x = (a[5] as f32)
        }
        ";

        let errors = check(s);
        // The operand of an `as` cast is still checked.
        assert_eq!(errors.len(), 1);
    }

    #[test]
    pub fn test_lambda_error_reported_once() {
        let s = "
        f() {
            var a: [i32; 4]
            var b: [i32; 4]
            var y = 1
            if a.len > (|i| b[i])(1000) { let z = y }
        }
        ";

        let errors = check(s);
        // match_expr visits the rhs twice; the diagnostic is still reported once.
        assert_eq!(errors.len(), 1);
    }
}
