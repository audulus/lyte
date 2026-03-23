use crate::*;
use internment::Intern;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;

/// The size of an array: either a known integer or a generic size parameter.
#[derive(Clone, Hash, Eq, PartialEq)]
pub enum ArraySize {
    /// Concrete size. 0 means unknown/unspecified (unifies with any size).
    Known(i32),
    /// Size variable from a generic parameter (e.g. `N` in `f<N>`).
    Var(Name),
}

impl ArraySize {
    /// Returns the concrete size, panicking if this is a size variable.
    pub fn known(&self) -> i32 {
        match self {
            ArraySize::Known(n) => *n,
            ArraySize::Var(name) => panic!("size variable {} is not concrete", name),
        }
    }
}

impl std::fmt::Display for ArraySize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArraySize::Known(n) => write!(f, "{}", n),
            ArraySize::Var(name) => write!(f, "{}", name),
        }
    }
}

impl std::fmt::Debug for ArraySize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArraySize::Known(n) => write!(f, "{}", n),
            ArraySize::Var(name) => write!(f, "{}", name),
        }
    }
}

/// The kind of type. Usually we're passing
/// around TypeIDs.
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    Bool,
    Int8,
    UInt8,
    Int32,
    UInt32,
    Float32,
    Float64,
    Tuple(Vec<TypeID>),

    /// Named type variable.
    Var(Name),

    /// Anonymous type variable.
    Anon(usize),

    /// Function (dom, rng)
    Func(TypeID, TypeID),

    /// Array of fixed size.
    Array(TypeID, ArraySize),

    /// Slice: a reference to an array, represented as a fat pointer {data_ptr, len}.
    /// Only valid as a function parameter type.
    Slice(TypeID),

    /// A named type which may include type parameters.
    Name(Name, Vec<TypeID>),
}

impl Type {
    /// Is this type represented as a pointer in codegen (struct, tuple, array, slice)?
    pub fn is_ptr(&self) -> bool {
        matches!(
            self,
            Type::Name(_, _) | Type::Tuple(_) | Type::Array(_, _) | Type::Slice(_) | Type::Func(_, _)
        )
    }
}

/// An interned type.
///
/// Newtyped so we can implement more traits.
#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub struct TypeID(Intern<Type>);

impl TypeID {
    pub fn new(ty: Type) -> Self {
        Self(Intern::new(ty))
    }

    /// Replaces named type variables with anonymous type variables, using instance for substitutions.
    pub fn fresh_aux(self, index: &mut usize, inst: &mut Instance) -> TypeID {
        match &*self {
            Type::Tuple(v) => {
                let vv = v.iter().map(|t| t.fresh_aux(index, inst)).collect();
                mk_type(Type::Tuple(vv))
            }
            Type::Array(a, sz) => mk_type(Type::Array(a.fresh_aux(index, inst), sz.clone())),
            Type::Slice(a) => mk_type(Type::Slice(a.fresh_aux(index, inst))),
            Type::Var(_) => *inst.entry(self).or_insert_with(|| {
                let t = mk_type(Type::Anon(*index));
                *index += 1;
                t
            }),
            Type::Func(dom, rng) => mk_type(Type::Func(
                dom.fresh_aux(index, inst),
                rng.fresh_aux(index, inst),
            )),
            Type::Name(name, params) => {
                let fresh_params = params
                    .iter()
                    .map(|param| param.fresh_aux(index, inst))
                    .collect();
                mk_type(Type::Name(*name, fresh_params))
            }
            _ => self,
        }
    }

    /// Replaces named type variables with anonymous type variables.
    pub fn fresh(self, index: &mut usize) -> TypeID {
        let mut inst = Instance::new();
        self.fresh_aux(index, &mut inst)
    }

    /// Apply type variable substitutions to a type.
    pub fn subst(self, inst: &Instance) -> TypeID {
        //println!("subst {:?}", t);
        let t = find(self, inst);
        //println!("maps to {:?}", t);

        match &*t {
            Type::Tuple(v) => mk_type(Type::Tuple(v.iter().map(|t| t.subst(inst)).collect())),
            Type::Func(a, b) => mk_type(Type::Func(a.subst(inst), b.subst(inst))),
            Type::Array(a, n) => mk_type(Type::Array(a.subst(inst), n.clone())),
            Type::Slice(a) => mk_type(Type::Slice(a.subst(inst))),
            Type::Anon(_) => find(t, inst),
            Type::Name(name, vars) => mk_type(Type::Name(
                *name,
                vars.iter().map(|t| t.subst(inst)).collect(),
            )),
            _ => t,
        }
    }

    /// Does the type contain any anonymous type variables?
    pub fn solved(self) -> bool {
        match &*self {
            Type::Tuple(v) => v.iter().all(|t| t.solved()),
            Type::Func(a, b) => a.solved() && b.solved(),
            Type::Array(a, _) | Type::Slice(a) => a.solved(),
            Type::Var(_) => true,
            Type::Anon(_) => false,
            _ => true,
        }
    }

    /// Is the type solved when substitutions are applied from an instance?
    pub fn solved_inst(self, inst: &Instance) -> bool {
        let tt = find(self, inst);
        match &*tt {
            Type::Tuple(v) => v.iter().all(|t| t.solved_inst(inst)),
            Type::Func(a, b) => a.solved_inst(inst) && b.solved_inst(inst),
            Type::Array(a, _) | Type::Slice(a) => a.solved_inst(inst),
            Type::Var(_) => true,
            Type::Anon(_) => false,
            _ => true,
        }
    }

    /// Calls a function for each type variable in the type.
    pub fn typevars(self, f: &mut impl FnMut(Name)) {
        match &*self {
            Type::Tuple(v) => {
                v.iter().for_each(|t| t.typevars(f));
            }
            Type::Array(a, _sz) => a.typevars(f),
            Type::Slice(a) => a.typevars(f),
            Type::Func(dom, rng) => {
                dom.typevars(f);
                rng.typevars(f);
            }
            Type::Var(name) => (*f)(*name),
            _ => (),
        }
    }

    /// Returns the size of a type in bytes.
    pub fn size(self, decls: &DeclTable) -> i32 {
        match &*self {
            Type::Void => 0,
            Type::Bool => 1,
            Type::Int8 => 1,
            Type::UInt8 => 1,
            Type::Int32 => 4,
            Type::UInt32 => 4,
            Type::Float32 => 4,
            Type::Float64 => 8,
            Type::Tuple(v) => v.iter().map(|t| t.size(decls)).sum(),
            Type::Name(name, vars) => {
                let decl = decls.find(*name);
                if decl.len() != 1 {
                    // Unknown or ambiguous type — should have been caught by the checker.
                    eprintln!(
                        "internal error: no unique decl for type '{}' (found {})",
                        name,
                        decl.len()
                    );
                    return 0;
                }
                match &decl[0] {
                    Decl::Struct(sdecl) => {
                        let inst: Instance = sdecl
                            .typevars
                            .iter()
                            .zip(vars.iter())
                            .map(|(tv, ty)| (mk_type(Type::Var(*tv)), *ty))
                            .collect();
                        sdecl
                            .fields
                            .iter()
                            .map(|field| field.ty.subst(&inst).size(decls))
                            .sum()
                    }
                    Decl::Enum { .. } => 4, // i32 discriminant
                    _ => {
                        eprintln!(
                            "internal error: expected struct or enum for type '{}', got {:?}",
                            name, decl[0]
                        );
                        0
                    }
                }
            }
            Type::Array(ty, sz) => ty.size(decls) * sz.known(),
            Type::Slice(_) => 12, // fat pointer: data_ptr (8) + len (4)
            Type::Func(_, _) => 16,
            Type::Anon(i) => {
                eprintln!("internal error: asked for size of anonymous type variable {}", i);
                0
            }
            Type::Var(name) => {
                eprintln!("internal error: asked for size of type variable {}", name);
                0
            }
        }
    }

    /// Pretty-print a type in lyte syntax.
    ///
    /// This method formats a type as it would appear in lyte source code.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let ty = mk_type(Type::Int32);
    /// assert_eq!(ty.pretty_print(), "i32");
    ///
    /// let arr_ty = mk_type(Type::Array(mk_type(Type::Float32), 10));
    /// assert_eq!(arr_ty.pretty_print(), "[f32; 10]");
    /// ```
    pub fn pretty_print(self) -> String {
        match &*self {
            Type::Void => "void".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Int8 => "i8".to_string(),
            Type::UInt8 => "u8".to_string(),
            Type::Int32 => "i32".to_string(),
            Type::UInt32 => "u32".to_string(),
            Type::Float32 => "f32".to_string(),
            Type::Float64 => "f64".to_string(),
            Type::Tuple(types) => {
                format!(
                    "({})",
                    types
                        .iter()
                        .map(|t| t.pretty_print())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Type::Var(name) => name.to_string(),
            Type::Anon(id) => format!("?{}", id),
            Type::Func(dom, rng) => {
                format!("{} → {}", dom.pretty_print(), rng.pretty_print())
            }
            Type::Slice(elem) => format!("[{}]", elem.pretty_print()),
            Type::Array(elem, size) => format!("[{}; {}]", elem.pretty_print(), size),
            Type::Name(name, params) => {
                if params.is_empty() {
                    name.to_string()
                } else {
                    format!(
                        "{}<{}>",
                        name,
                        params
                            .iter()
                            .map(|t| t.pretty_print())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
        }
    }
}

impl Deref for TypeID {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Display for TypeID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.pretty_print())
    }
}

impl fmt::Debug for TypeID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", *self.0)
    }
}

/// A substitution from type variables to
/// other types.
///
/// "Substitution" might be better terminology.
pub type Instance = HashMap<TypeID, TypeID>;

pub fn print_instance(inst: &Instance) {
    for (k, v) in inst {
        println!("  {:?} ➡️ {:?}", k, v);
    }
}

/// Convenience to create new TypeIDs from types.
///
/// Kinda questionable wrapper over TypeID::new.
pub fn mk_type(proto: Type) -> TypeID {
    TypeID::new(proto)
}

/// Create a new named type variable.
pub fn typevar(name: &str) -> TypeID {
    mk_type(Type::Var(Name::new(String::from(name))))
}

/// Create a new anonymous type variable with an index.
///
/// Should we also include a location?
pub fn anon(index: usize) -> TypeID {
    mk_type(Type::Anon(index))
}

/// Convenience to create a function type.
pub fn func(dom: TypeID, range: TypeID) -> TypeID {
    mk_type(Type::Func(dom, range))
}

/// Convenience to create a tuple type.
pub fn tuple(types: Vec<TypeID>) -> TypeID {
    mk_type(Type::Tuple(types))
}

/// Convenience to create a tuple type from parameters.
fn params_ty(params: &[Param]) -> TypeID {
    tuple(params.iter().map(|p| p.ty.unwrap()).collect())
}

/// Look up a type in an instance, following as far as possible.
///
/// There better not be cycles!
pub fn find(id: TypeID, inst: &Instance) -> TypeID {
    let mut id = id;
    while let Some(t) = inst.get(&id) {
        if *t == id {
            break; // Avoid infinite loop on identity mapping.
        }
        id = *t;
    }
    id
}

/// Find a substitution which makes two types equal.
///
/// Returns false if such a substituion doesn't exist.
/// I think this is pretty much textbook Hindley-Milner.
/// It's more or less what's in the dragon book.
pub fn unify(lhs: TypeID, rhs: TypeID, inst: &mut Instance) -> bool {
    let lhs = find(lhs, inst);
    let rhs = find(rhs, inst);

    if lhs == rhs {
        true
    } else {
        match (&*lhs, &*rhs) {
            (Type::Anon(_), _) => {
                inst.insert(lhs, rhs);
                true
            }
            (_, Type::Anon(_)) => {
                inst.insert(rhs, lhs);
                true
            }
            (Type::Tuple(v0), Type::Tuple(v1)) => {
                if v0.len() == v1.len() {
                    for i in 0..v0.len() {
                        if !unify(v0[i], v1[i], inst) {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            }
            (Type::Name(a, a_params), Type::Name(b, b_params)) => {
                if a == b && a_params.len() == b_params.len() {
                    for i in 0..a_params.len() {
                        if !unify(a_params[i], b_params[i], inst) {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            }
            (Type::Array(a, sa), Type::Array(b, sb)) => {
                let sizes_ok = match (sa, sb) {
                    (ArraySize::Var(_), _) | (_, ArraySize::Var(_)) => true,
                    (ArraySize::Known(a), ArraySize::Known(b)) => *a == *b,
                };
                sizes_ok && unify(*a, *b, inst)
            }
            (Type::Slice(a), Type::Slice(b)) => unify(*a, *b, inst),
            (Type::Slice(a), Type::Array(b, _)) | (Type::Array(b, _), Type::Slice(a)) => {
                unify(*a, *b, inst)
            }
            (Type::Func(a, b), Type::Func(c, d)) => unify(*a, *c, inst) && unify(*b, *d, inst),
            _ => false,
        }
    }
}

/// Like `unify`, but also treats `Type::Var` as bindable (like `Type::Anon`).
/// Used by the monomorphizer to infer type arguments for generic globals.
pub fn unify_with_vars(lhs: TypeID, rhs: TypeID, inst: &mut Instance) -> bool {
    let lhs = find(lhs, inst);
    let rhs = find(rhs, inst);

    if lhs == rhs {
        true
    } else {
        match (&*lhs, &*rhs) {
            (Type::Anon(_), _) | (Type::Var(_), _) => {
                inst.insert(lhs, rhs);
                true
            }
            (_, Type::Anon(_)) | (_, Type::Var(_)) => {
                inst.insert(rhs, lhs);
                true
            }
            (Type::Tuple(v0), Type::Tuple(v1)) if v0.len() == v1.len() => {
                v0.iter()
                    .zip(v1.iter())
                    .all(|(a, b)| unify_with_vars(*a, *b, inst))
            }
            (Type::Array(a, sa), Type::Array(b, sb)) => {
                let sizes_ok = match (sa, sb) {
                    (ArraySize::Var(_), _) | (_, ArraySize::Var(_)) => true,
                    (ArraySize::Known(a), ArraySize::Known(b)) => *a == *b,
                };
                sizes_ok && unify_with_vars(*a, *b, inst)
            }
            (Type::Slice(a), Type::Slice(b)) => unify_with_vars(*a, *b, inst),
            (Type::Func(a, b), Type::Func(c, d)) => {
                unify_with_vars(*a, *c, inst) && unify_with_vars(*b, *d, inst)
            }
            (Type::Name(a, ap), Type::Name(b, bp)) if a == b && ap.len() == bp.len() => ap
                .iter()
                .zip(bp.iter())
                .all(|(a, b)| unify_with_vars(*a, *b, inst)),
            _ => false,
        }
    }
}

impl Decl {
    pub fn ty(&self) -> TypeID {
        match self {
            Decl::Interface { .. } => mk_type(Type::Void),
            Decl::Func(FuncDecl { params, ret, .. }) => func(params_ty(params), *ret),
            Decl::Macro(FuncDecl { params, ret, .. }) => func(params_ty(params), *ret),
            Decl::Struct(StructDecl { name, .. }) => mk_type(Type::Name(*name, vec![])),
            Decl::Enum { name, .. } => mk_type(Type::Name(*name, vec![])),
            Decl::Global { ty, .. } => *ty,
        }
    }
}

impl FuncDecl {
    /// Returns the type for this function declaration.
    pub fn ty(&self) -> TypeID {
        func(params_ty(&self.params), self.ret)
    }
}

impl Interface {
    /// Is an interface satisfied?
    pub fn satisfied(
        &self,
        types: &[TypeID],
        decls: &DeclTable,
        errors: &mut Vec<TypeError>,
        loc: Loc,
    ) -> bool {
        let mut inst = Instance::new();
        for (v, t) in self.typevars.iter().zip(types) {
            inst.insert(typevar(v), *t);
        }

        // If any type parameter is still unresolved (type variable or anonymous),
        // defer the check — it will be verified when the generic function is
        // instantiated with concrete types.
        if types.iter().any(|t| matches!(&**t, Type::Var(_) | Type::Anon(_))) {
            return true;
        }

        let mut satisfied = true;

        // Find functions among decls that have the same type.
        for func in &self.funcs {
            let d = decls.find(func.name);

            // Do we want to unify instead?
            let found = d.iter().any(|d| d.ty() == func.ty().subst(&inst));

            if !found {
                satisfied = false;
                errors.push(TypeError {
                    location: loc,
                    message: format!(
                        "function {} for interface {} is required",
                        func.name, self.name
                    ),
                });
            }
        }

        satisfied
    }

    /// Replaces any named types with type variables.
    pub fn subst_typevars(&mut self) {
        let mut inst = Instance::new();

        for tv in &self.typevars {
            inst.insert(mk_type(Type::Name(*tv, vec![])), mk_type(Type::Var(*tv)));
        }

        for func in &mut self.funcs {
            for param in &mut func.params {
                if let Some(ty) = &mut param.ty {
                    *ty = ty.subst(&inst)
                }
            }

            func.ret = func.ret.subst(&inst);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mk_type() {
        let v0 = mk_type(Type::Void);
        let v1 = mk_type(Type::Void);
        assert_eq!(v0, v1);

        let i0 = mk_type(Type::Int8);
        assert_ne!(v0, i0);
    }

    #[test]
    fn test_unify() {
        let vd = mk_type(Type::Void);
        let int8 = mk_type(Type::Int8);
        let mut inst = Instance::new();
        assert!(unify(vd, vd, &mut inst));
        assert!(!unify(vd, int8, &mut inst));
        assert!(vd.solved());

        let var = anon(0);
        assert!(unify(var, int8, &mut inst));
        assert!(!var.solved());

        let var2 = anon(1);
        assert!(unify(var, var2, &mut inst));

        {
            let mut inst = Instance::new();
            assert!(unify(
                mk_type(Type::Slice(var)),
                mk_type(Type::Slice(var2)),
                &mut inst
            ));
        }

        {
            let mut inst = Instance::new();
            let b = mk_type(Type::Bool);
            let tup = mk_type(Type::Tuple(vec![b]));
            assert!(unify(var, func(tup, vd), &mut inst));
        }
    }

    #[test]
    fn test_typevars() {
        let vd = mk_type(Type::Void);
        vd.typevars(&mut |_name| ());

        let v = typevar("T");
        v.typevars(&mut |name| assert_eq!(name, Name::new("T".into())));
    }

    #[test]
    fn test_fresh() {
        let t = mk_type(Type::Name(Name::new("MyType".into()), vec![typevar("T")]));

        let mut i = 0usize;
        let tt = t.fresh(&mut i);

        if let Type::Name(name, params) = &*tt {
            assert_eq!(*name, Name::new("MyType".into()));
            assert_eq!(params[0], anon(0));
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_subst_typevar() {
        let t = mk_type(Type::Name(Name::new("TestType".into()), vec![]));

        let tt = typevar("TestType");

        let mut inst = Instance::new();
        inst.insert(t, tt);

        let tt2 = t.subst(&inst);

        assert_eq!(tt, tt2);
    }

    #[test]
    fn test_pretty_print_primitives() {
        assert_eq!(mk_type(Type::Void).pretty_print(), "void");
        assert_eq!(mk_type(Type::Bool).pretty_print(), "bool");
        assert_eq!(mk_type(Type::Int8).pretty_print(), "i8");
        assert_eq!(mk_type(Type::UInt8).pretty_print(), "u8");
        assert_eq!(mk_type(Type::Int32).pretty_print(), "i32");
        assert_eq!(mk_type(Type::UInt32).pretty_print(), "u32");
        assert_eq!(mk_type(Type::Float32).pretty_print(), "f32");
        assert_eq!(mk_type(Type::Float64).pretty_print(), "f64");
    }

    #[test]
    fn test_pretty_print_array() {
        let arr_ty = mk_type(Type::Array(mk_type(Type::Int32), ArraySize::Known(10)));
        assert_eq!(arr_ty.pretty_print(), "[i32; 10]");

        let nested = mk_type(Type::Array(
            mk_type(Type::Array(mk_type(Type::Float32), ArraySize::Known(5))),
            ArraySize::Known(3),
        ));
        assert_eq!(nested.pretty_print(), "[[f32; 5]; 3]");
    }

    #[test]
    fn test_pretty_print_func() {
        let func_ty = mk_type(Type::Func(mk_type(Type::Int32), mk_type(Type::Bool)));
        assert_eq!(func_ty.pretty_print(), "i32 → bool");

        let higher_order = mk_type(Type::Func(func_ty, mk_type(Type::Float32)));
        assert_eq!(higher_order.pretty_print(), "i32 → bool → f32");
    }

    #[test]
    fn test_pretty_print_tuple() {
        let tuple_ty = mk_type(Type::Tuple(vec![
            mk_type(Type::Int32),
            mk_type(Type::Float32),
            mk_type(Type::Bool),
        ]));
        assert_eq!(tuple_ty.pretty_print(), "(i32, f32, bool)");

        let empty_tuple = mk_type(Type::Tuple(vec![]));
        assert_eq!(empty_tuple.pretty_print(), "()");
    }

    #[test]
    fn test_pretty_print_named_type() {
        let named_ty = mk_type(Type::Name(Name::str("Point"), vec![]));
        assert_eq!(named_ty.pretty_print(), "Point");

        let generic_ty = mk_type(Type::Name(Name::str("Vec"), vec![mk_type(Type::Int32)]));
        assert_eq!(generic_ty.pretty_print(), "Vec<i32>");

        let multi_param = mk_type(Type::Name(
            Name::str("Map"),
            vec![mk_type(Type::Int32), mk_type(Type::Float32)],
        ));
        assert_eq!(multi_param.pretty_print(), "Map<i32, f32>");
    }

    #[test]
    fn test_pretty_print_type_var() {
        let var_ty = mk_type(Type::Var(Name::str("T")));
        assert_eq!(var_ty.pretty_print(), "T");

        let anon_ty = mk_type(Type::Anon(42));
        assert_eq!(anon_ty.pretty_print(), "?42");
    }
}
