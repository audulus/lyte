//! Checked, ordered loop values and effects for experimental native selection.
//!
//! Discovery follows binding identities through scalar declarations and updates.
//! It records indexed reads and writes in source order, private loop invariants,
//! and incoming/final scalar state. Bounded kernel contracts consume this shared
//! representation; unsupported contracts keep the original bytecode loop.

use crate::pointwise::{self, RecipeId};
use crate::*;
use std::collections::{HashMap, HashSet};
use std::convert::{TryFrom, TryInto};

mod recipes;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Scalar {
    F32,
    F64,
}

impl Scalar {
    pub(crate) fn of(ty: TypeID) -> Option<Self> {
        match &*ty {
            Type::Float32 => Some(Self::F32),
            Type::Float64 => Some(Self::F64),
            _ => None,
        }
    }
}

/// Immutable value identity within one loop analysis, not a local storage slot.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct ValueId(pub usize);

#[derive(Clone, Debug)]
pub enum Operation {
    Input(LocalId),
    Constant(String),
    Binary(Binop, ValueId, ValueId),
    Negate(ValueId),
    ReadElement { stream: usize },
}

#[derive(Clone, Debug)]
pub struct Value {
    pub ty: Scalar,
    pub operation: Operation,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SpanLength {
    Fixed(u32),
    Slice,
}

#[derive(Clone, Debug)]
pub struct MemoryStream {
    pub binding: Reference,
    /// Original base expression for representation-aware address/span lowering.
    pub expression: ExprID,
}

#[derive(Clone, Debug)]
pub struct ScalarInput {
    pub binding: LocalId,
    pub value: ValueId,
}

#[derive(Clone, Debug)]
pub struct Carry {
    pub binding: LocalId,
    pub incoming: ValueId,
    pub next: ValueId,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LoopStep {
    Value(ValueId),
    StoreElement { stream: usize, value: ValueId },
}

#[derive(Clone, Debug)]
pub struct LoopRegion {
    pub induction: LocalId,
    pub values: Vec<Value>,
    pub streams: Vec<MemoryStream>,
    pub inputs: Vec<ScalarInput>,
    pub carries: Vec<Carry>,
    pub tape: Vec<LoopStep>,
    pub ty: Scalar,
}

#[derive(Clone, Debug)]
pub struct Pointwise {
    pub input: usize,
    pub output: usize,
    pub recipe: RecipeId,
    pub coefficients: Vec<ValueId>,
}

#[derive(Clone, Copy, Debug)]
pub struct OnePole {
    pub input: usize,
    pub output: usize,
    pub carry: usize,
    pub feed: ValueId,
    pub feedback: ValueId,
}

#[derive(Clone, Copy, Debug)]
pub struct Biquad {
    pub input: usize,
    pub output: usize,
    /// Ordered coefficient roles: b0, b1, b2, a1, a2.
    pub coefficients: [ValueId; 5],
    /// Carry roles x1, x2, y1, y2, independent of source declaration order.
    pub carries: [usize; 4],
}

impl LoopRegion {
    fn operation(&self, value: ValueId) -> Option<&Operation> {
        self.values.get(value.0).map(|value| &value.operation)
    }

    fn binary(&self, value: ValueId, expected: Binop) -> Option<(ValueId, ValueId)> {
        match self.operation(value)? {
            Operation::Binary(operation, lhs, rhs) if *operation == expected => Some((*lhs, *rhs)),
            _ => None,
        }
    }

    fn read(&self, value: ValueId) -> Option<usize> {
        match self.operation(value)? {
            Operation::ReadElement { stream } => Some(*stream),
            _ => None,
        }
    }

    /// A coefficient is already private scalar state or an exact literal.
    /// Arbitrary arithmetic remains per-iteration work until a contract covers it.
    fn coefficient(&self, value: ValueId) -> bool {
        match self.operation(value) {
            Some(Operation::Constant(_)) => true,
            Some(Operation::Input(binding)) => self
                .inputs
                .iter()
                .any(|input| input.binding == *binding && input.value == value),
            _ => false,
        }
    }

    /// Input/literal materialization has no loop effect. Every remaining value
    /// and every store must occur exactly once in this source-ordered schedule.
    /// In particular, this never drops an unused read or duplicates a shared one.
    fn covers(&self, expected: &[LoopStep]) -> bool {
        self.values.iter().all(|value| value.ty == self.ty)
            && self
                .tape
                .iter()
                .copied()
                .filter(|step| match step {
                    LoopStep::Value(value) => !matches!(
                        self.operation(*value),
                        Some(Operation::Input(_) | Operation::Constant(_))
                    ),
                    LoopStep::StoreElement { .. } => true,
                })
                .eq(expected.iter().copied())
    }

    /// Match the same ordered recipe that generates the native arithmetic.
    /// Nodes denote values, so a reused read or calculation must retain its
    /// exact identity. The complete effect tape also rejects reordered work,
    /// unused reads/arithmetic, and additional writes.
    pub fn pointwise(&self) -> Option<Pointwise> {
        pointwise::recipes().find_map(|(id, recipe)| {
            let matched = self.match_recipe(recipe)?;
            Some(Pointwise {
                input: matched.input,
                output: matched.output,
                recipe: id,
                coefficients: matched.coefficients,
            })
        })
    }

    /// Exact ordered one-pole state update followed by its output write:
    /// `state = input[i] * feed + state * feedback; output[i] = state`.
    pub fn one_pole(&self) -> Option<OnePole> {
        let matched = self.match_recipe(crate::recurrence::ONE_POLE.recipe())?;
        Some(OnePole {
            input: matched.input,
            output: matched.output,
            carry: matched.carries[0],
            feed: matched.coefficients[0],
            feedback: matched.coefficients[1],
        })
    }

    /// Direct form I with explicit, separately rounded arithmetic:
    /// `b0*x + b1*x1 + b2*x2 - a1*y1 - a2*y2`.
    /// The four final carry values must implement the two history shifts.
    pub fn biquad(&self) -> Option<Biquad> {
        let matched = self.match_recipe(crate::recurrence::BIQUAD.recipe())?;
        Some(Biquad {
            input: matched.input,
            output: matched.output,
            coefficients: matched.coefficients.try_into().ok()?,
            carries: matched.carries.try_into().ok()?,
        })
    }
}
fn local(function: &CheckedFunction, expression: ExprID) -> Option<LocalId> {
    match function.arena.reference(expression) {
        Some(Reference::Local(binding)) => Some(*binding),
        _ => None,
    }
}

fn binder(function: &CheckedFunction, expression: ExprID) -> Option<LocalId> {
    match function.arena[expression] {
        Expr::Let(..) | Expr::Var(..) | Expr::For { .. } => Some(function.arena.binder(expression)),
        _ => None,
    }
}

#[derive(Default)]
struct BodyShape {
    definitions: HashSet<LocalId>,
    assignments: Vec<LocalId>,
    ty: Option<Scalar>,
}

impl BodyShape {
    fn collect(&mut self, function: &CheckedFunction, expression: ExprID) -> Option<()> {
        match &function.arena[expression] {
            Expr::Block(statements) => {
                for &statement in statements {
                    self.collect(function, statement)?;
                }
            }
            Expr::Let(_, initializer, _) | Expr::Var(_, Some(initializer), _) => {
                self.definitions.insert(binder(function, expression)?);
                self.ty = self
                    .ty
                    .or_else(|| Scalar::of(function.arena.ty(*initializer)));
            }
            Expr::Binop(Binop::Assign, lhs, rhs) => {
                match function.arena[*lhs] {
                    Expr::Id(_) => {
                        let binding = local(function, *lhs)?;
                        if !self.assignments.contains(&binding) {
                            self.assignments.push(binding);
                        }
                    }
                    Expr::ArrayIndex(..) => {}
                    _ => return None,
                }
                self.ty = self.ty.or_else(|| Scalar::of(function.arena.ty(*rhs)));
            }
            _ => return None,
        }
        Some(())
    }
}

struct Builder<'a> {
    function: &'a CheckedFunction,
    blocked: &'a HashSet<LocalId>,
    definitions: HashSet<LocalId>,
    region: LoopRegion,
    bindings: HashMap<LocalId, ValueId>,
}

impl Builder<'_> {
    fn add(&mut self, operation: Operation) -> ValueId {
        let value = ValueId(self.region.values.len());
        self.region.values.push(Value {
            ty: self.region.ty,
            operation,
        });
        self.region.tape.push(LoopStep::Value(value));
        value
    }

    fn private_scalar(&self, binding: LocalId) -> bool {
        !self.blocked.contains(&binding)
            && Scalar::of(self.function.arena.local(binding).ty) == Some(self.region.ty)
    }

    fn stream(&mut self, base: ExprID, index: ExprID) -> Option<usize> {
        let function = self.function;
        if !matches!(function.arena[base], Expr::Id(_))
            || !matches!(function.arena[index], Expr::Id(_))
            || local(function, index) != Some(self.region.induction)
        {
            return None;
        }
        let binding = function.arena.reference(base)?.clone();
        match &binding {
            Reference::Local(binding) if !self.definitions.contains(binding) => {}
            Reference::Instance(_) => {}
            _ => return None,
        }
        let element = match &*function.arena.ty(base) {
            Type::Array(element, ArraySize::Known(length)) => {
                u32::try_from(*length).ok()?;
                *element
            }
            Type::Slice(element) => *element,
            _ => return None,
        };
        if Scalar::of(element) != Some(self.region.ty) {
            return None;
        }
        if let Some(stream) = self
            .region
            .streams
            .iter()
            .position(|stream| stream.binding == binding)
        {
            return Some(stream);
        }
        let stream = self.region.streams.len();
        self.region.streams.push(MemoryStream {
            binding,
            expression: base,
        });
        Some(stream)
    }

    fn expression(&mut self, expression: ExprID) -> Option<ValueId> {
        let function = self.function;
        if Scalar::of(function.arena.ty(expression)) != Some(self.region.ty) {
            return None;
        }
        let operation = match &function.arena[expression] {
            Expr::Id(_) => {
                let binding = local(function, expression)?;
                if let Some(value) = self.bindings.get(&binding) {
                    return Some(*value);
                }
                if self.definitions.contains(&binding) || !self.private_scalar(binding) {
                    return None;
                }
                let value = self.add(Operation::Input(binding));
                self.region.inputs.push(ScalarInput { binding, value });
                self.bindings.insert(binding, value);
                return Some(value);
            }
            Expr::Real(value, _) => Operation::Constant(value.clone()),
            Expr::ArrayIndex(base, index) => Operation::ReadElement {
                stream: self.stream(*base, *index)?,
            },
            Expr::Binop(
                operation @ (Binop::Plus | Binop::Minus | Binop::Mult | Binop::Div),
                lhs,
                rhs,
            ) => {
                let lhs = self.expression(*lhs)?;
                let rhs = self.expression(*rhs)?;
                Operation::Binary(*operation, lhs, rhs)
            }
            Expr::Unop(Unop::Neg, operand) => Operation::Negate(self.expression(*operand)?),
            _ => return None,
        };
        Some(self.add(operation))
    }

    fn statement(&mut self, expression: ExprID) -> Option<()> {
        let function = self.function;
        match &function.arena[expression] {
            Expr::Block(statements) => {
                // LocalIds remain unique even after an inner lexical block.
                for &statement in statements {
                    self.statement(statement)?;
                }
            }
            Expr::Let(_, initializer, _) | Expr::Var(_, Some(initializer), _) => {
                let binding = binder(function, expression)?;
                if !self.private_scalar(binding) {
                    return None;
                }
                let value = self.expression(*initializer)?;
                self.bindings.insert(binding, value);
            }
            Expr::Binop(Binop::Assign, lhs, rhs) => {
                // Ordinary assignment evaluates its RHS before the lvalue.
                let value = self.expression(*rhs)?;
                match function.arena[*lhs] {
                    Expr::Id(_) => {
                        let binding = local(function, *lhs)?;
                        if !self.private_scalar(binding) || !self.bindings.contains_key(&binding) {
                            return None;
                        }
                        self.bindings.insert(binding, value);
                        if let Some(carry) = self
                            .region
                            .carries
                            .iter_mut()
                            .find(|carry| carry.binding == binding)
                        {
                            carry.next = value;
                        }
                    }
                    Expr::ArrayIndex(base, index) => {
                        let stream = self.stream(base, index)?;
                        self.region
                            .tape
                            .push(LoopStep::StoreElement { stream, value });
                    }
                    _ => return None,
                }
            }
            _ => return None,
        }
        Some(())
    }
}

/// Observe checked HIR after safety checking. Selection and runtime span guards
/// establish a kernel's narrower executable contract.
fn analyze(
    function: &CheckedFunction,
    loop_expr: ExprID,
    blocked: &HashSet<LocalId>,
) -> Option<LoopRegion> {
    let Expr::For { body, .. } = function.arena[loop_expr] else {
        return None;
    };
    let induction = binder(function, loop_expr)?;
    if blocked.contains(&induction) {
        return None;
    }
    let mut shape = BodyShape::default();
    shape.collect(function, body)?;
    let mut builder = Builder {
        function,
        blocked,
        definitions: shape.definitions,
        region: LoopRegion {
            induction,
            values: vec![],
            streams: vec![],
            inputs: vec![],
            carries: vec![],
            tape: vec![],
            ty: shape.ty?,
        },
        bindings: HashMap::new(),
    };
    for binding in shape.assignments {
        if builder.definitions.contains(&binding) {
            continue;
        }
        if binding == induction || !builder.private_scalar(binding) {
            return None;
        }
        let incoming = builder.add(Operation::Input(binding));
        builder.region.carries.push(Carry {
            binding,
            incoming,
            next: incoming,
        });
        builder.bindings.insert(binding, incoming);
    }
    builder.statement(body)?;
    if builder.region.carries.is_empty()
        && !builder
            .region
            .tape
            .iter()
            .any(|step| matches!(step, LoopStep::StoreElement { .. }))
    {
        return None;
    }
    Some(builder.region)
}

/// Analyze one concrete, post-hoisting body. IDs and reports are body-local:
/// recompute after mutation or lambda extraction instead of transporting facts.
/// Captures and reference/unknown-call exposure remain conservative; identity
/// establishes neither purity nor absence of aliasing.
pub(crate) fn analyze_function(function: &CheckedFunction) -> HashMap<ExprID, LoopRegion> {
    fn expose(function: &CheckedFunction, id: ExprID, blocked: &mut HashSet<LocalId>) {
        if let Some(binding) = local(function, id) {
            blocked.insert(binding);
        }
        if matches!(function.arena[id], Expr::Lambda { .. }) {
            return;
        }
        for child in function.arena[id].subexprs() {
            expose(function, child, blocked);
        }
    }

    fn scan(function: &CheckedFunction, id: ExprID, blocked: &mut HashSet<LocalId>) {
        if matches!(function.arena[id], Expr::Lambda { .. }) {
            return;
        }
        if let Expr::Call(callee, args) = &function.arena[id] {
            let callee_ty = function.arena.ty(*callee);
            let params = match &*callee_ty {
                Type::Func(domain, _) => match &**domain {
                    Type::Tuple(params) => Some(params),
                    _ => None,
                },
                _ => None,
            };
            for (position, arg) in args.iter().enumerate() {
                if params
                    .and_then(|p| p.get(position))
                    .is_none_or(|ty| ty.is_ptr())
                {
                    expose(function, *arg, blocked);
                }
            }
        }
        for child in function.arena[id].subexprs() {
            scan(function, child, blocked);
        }
    }

    fn visit(
        function: &CheckedFunction,
        id: ExprID,
        blocked: &HashSet<LocalId>,
        loops: &mut HashMap<ExprID, LoopRegion>,
    ) {
        // Generated lambdas keep ordinary lowering in this consumer.
        if matches!(function.arena[id], Expr::Lambda { .. }) {
            return;
        }
        if let Some(region) = analyze(function, id, blocked) {
            loops.insert(id, region);
        }
        for child in function.arena[id].subexprs() {
            visit(function, child, blocked, loops);
        }
    }

    let mut loops = HashMap::new();
    if let Some(body) = function.body {
        let mut blocked = function.captured_locals();
        blocked.extend(function.closure_vars.iter().copied());
        scan(function, body, &mut blocked);
        visit(function, body, &blocked, &mut loops);
    }
    loops
}

#[cfg(test)]
mod tests;
