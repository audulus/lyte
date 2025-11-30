use crate::*;

/// Key that uniquely identifies a monomorphized function or struct.
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct MonomorphKey {
    pub name: Name,
    pub type_args: Vec<TypeID>,
}

impl MonomorphKey {
    pub fn new(name: Name, type_args: Vec<TypeID>) -> Self {
        Self { name, type_args }
    }

    pub fn mangled_name(&self) -> Name {
        mangle_name(self.name, &self.type_args)
    }
}

/// Generates a unique mangled name for a generic function or struct
/// instantiated with concrete type arguments.
///
/// The mangling scheme uses a readable format that includes type information,
/// making debugging easier while ensuring uniqueness.
///
/// # Examples
/// - `id<i32>` becomes `id$i32`
/// - `map<i32, bool>` becomes `map$i32$bool`
/// - `GenericStruct<f32>` becomes `GenericStruct$f32`
pub fn mangle_name(base: Name, type_args: &[TypeID]) -> Name {
    if type_args.is_empty() {
        return base;
    }

    let mut mangled = String::from(&*base);

    for ty in type_args {
        mangled.push('$');
        mangled.push_str(&type_to_string(*ty));
    }

    Name::new(mangled)
}

/// Converts a TypeID to a string representation for name mangling.
/// Uses a compact, unambiguous format.
fn type_to_string(ty: TypeID) -> String {
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
            let type_strs: Vec<_> = types.iter()
                .map(|t| type_to_string(*t))
                .collect();
            format!("({})", type_strs.join(","))
        }

        Type::Array(elem_ty, size) => {
            format!("[{};{}]", type_to_string(*elem_ty), size)
        }

        Type::Func(from, to) => {
            format!("fn({})_{}", type_to_string(*from), type_to_string(*to))
        }

        Type::Name(name, params) => {
            if params.is_empty() {
                format!("{}", name)
            } else {
                let param_strs: Vec<_> = params.iter()
                    .map(|p| type_to_string(*p))
                    .collect();
                format!("{}<{}>", name, param_strs.join(","))
            }
        }

        Type::Var(name) => {
            format!("T{}", name)
        }

        Type::Anon(idx) => {
            format!("#{}", idx)
        }
    }
}

/// Tracks monomorphization in progress to detect infinite recursion.
///
/// Generic recursion happens when instantiating a generic function requires
/// instantiating it again with different (but related) type arguments, potentially
/// creating an infinite chain.
///
/// # Types of Generic Recursion
///
/// ## 1. Direct Infinite Recursion (ALWAYS INFINITE)
/// ```ignore
/// fn foo<T>() {
///     foo<Vec<T>>()  // Creates foo<Vec<i32>>, foo<Vec<Vec<i32>>>, ...
/// }
/// ```
///
/// ## 2. Mutually Recursive (ALWAYS INFINITE)
/// ```ignore
/// fn f<T>() { g<Vec<T>>() }
/// fn g<U>() { f<Vec<U>>() }
/// // Creates f<i32>, g<Vec<i32>>, f<Vec<Vec<i32>>>, ...
/// ```
///
/// ## 3. Bounded Recursion (FINITE - OK!)
/// ```ignore
/// fn recurse<T>(depth: i32) {
///     if depth > 0 {
///         recurse<T>(depth - 1)  // Same type args!
///     }
/// }
/// ```
///
/// ## 4. Type-decreasing Recursion (FINITE - OK!)
/// ```ignore
/// fn unwrap<T>(x: Vec<Vec<T>>) {
///     unwrap<T>(inner)  // Type gets simpler, not more complex
/// }
/// ```
///
/// # Detection Strategy
///
/// We use a "stack" of currently-being-instantiated generics. If we try to
/// instantiate something already on the stack with MORE COMPLEX type args,
/// we've found infinite recursion.
#[derive(Debug, Default)]
pub struct RecursionDetector {
    /// Stack of monomorphizations currently in progress.
    /// If we see the same (name, type_args) twice, we have recursion.
    in_progress: Vec<MonomorphKey>,
}

impl RecursionDetector {
    pub fn new() -> Self {
        Self {
            in_progress: Vec::new(),
        }
    }

    /// Checks if instantiating this key would cause infinite recursion.
    ///
    /// Returns Ok(()) if safe, Err with error message if recursion detected.
    pub fn check(&self, key: &MonomorphKey) -> Result<(), String> {
        // Simple check: is this exact instantiation already in progress?
        if self.in_progress.contains(key) {
            return Err(format!(
                "Infinite generic recursion detected: {} with type args {:?} is already being instantiated",
                key.name, key.type_args
            ));
        }

        // Advanced check: is the same function being instantiated with
        // increasingly complex type arguments?
        for in_progress_key in &self.in_progress {
            if in_progress_key.name == key.name {
                // Same function name - check if types are getting more complex
                if is_more_complex(&key.type_args, &in_progress_key.type_args) {
                    return Err(format!(
                        "Infinite generic recursion detected: {} is being instantiated with increasingly complex types.\n\
                         Previous: {:?}\n\
                         Current:  {:?}",
                        key.name, in_progress_key.type_args, key.type_args
                    ));
                }
            }
        }

        Ok(())
    }

    /// Marks that we're starting to instantiate this key.
    /// Must be paired with `end_instantiation`.
    pub fn begin_instantiation(&mut self, key: MonomorphKey) {
        self.in_progress.push(key);
    }

    /// Marks that we've finished instantiating (successfully or not).
    pub fn end_instantiation(&mut self) {
        self.in_progress.pop();
    }

    /// Gets the current instantiation depth (useful for debugging).
    pub fn depth(&self) -> usize {
        self.in_progress.len()
    }

    /// Gets the stack of in-progress instantiations (for error reporting).
    pub fn stack(&self) -> &[MonomorphKey] {
        &self.in_progress
    }
}

/// Checks if `current` type arguments are "more complex" than `previous`.
///
/// This is a heuristic to detect infinite generic recursion. If types are
/// getting progressively more nested, we're likely in an infinite loop.
///
/// Examples:
/// - `Vec<i32>` is more complex than `i32`
/// - `Vec<Vec<i32>>` is more complex than `Vec<i32>`
/// - `(i32, bool)` is NOT more complex than `i32` (different structure)
fn is_more_complex(current: &[TypeID], previous: &[TypeID]) -> bool {
    if current.len() != previous.len() {
        return false;
    }

    for (curr_ty, prev_ty) in current.iter().zip(previous) {
        if type_complexity(*curr_ty) > type_complexity(*prev_ty) {
            return true;
        }
    }

    false
}

/// Computes a "complexity score" for a type.
/// Higher scores mean more nested/complex types.
fn type_complexity(ty: TypeID) -> usize {
    match &*ty {
        // Primitives have complexity 0
        Type::Void | Type::Bool | Type::Int8 | Type::UInt8
        | Type::Int32 | Type::UInt32 | Type::Float32 | Type::Float64 => 0,

        // Type variables have complexity 0 (they're placeholders)
        Type::Var(_) | Type::Anon(_) => 0,

        // Arrays add 1 + complexity of element
        Type::Array(elem, _) => 1 + type_complexity(*elem),

        // Tuples: 1 + max complexity of elements
        Type::Tuple(types) => {
            1 + types.iter().map(|t| type_complexity(*t)).max().unwrap_or(0)
        }

        // Functions: 1 + max of domain and range
        Type::Func(dom, rng) => {
            1 + type_complexity(*dom).max(type_complexity(*rng))
        }

        // Named types (including generics): 1 + max complexity of parameters
        Type::Name(_, params) => {
            if params.is_empty() {
                0 // Simple named type like "MyStruct"
            } else {
                1 + params.iter().map(|t| type_complexity(*t)).max().unwrap_or(0)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mangle_no_type_args() {
        let name = Name::new("foo".into());
        let mangled = mangle_name(name, &[]);
        assert_eq!(mangled, Name::new("foo".into()));
    }

    #[test]
    fn test_mangle_single_primitive() {
        let name = Name::new("id".into());
        let type_args = vec![mk_type(Type::Int32)];
        let mangled = mangle_name(name, &type_args);
        assert_eq!(mangled, Name::new("id$i32".into()));
    }

    #[test]
    fn test_mangle_multiple_primitives() {
        let name = Name::new("map".into());
        let type_args = vec![
            mk_type(Type::Int32),
            mk_type(Type::Bool),
        ];
        let mangled = mangle_name(name, &type_args);
        assert_eq!(mangled, Name::new("map$i32$bool".into()));
    }

    #[test]
    fn test_mangle_all_primitive_types() {
        let name = Name::new("fn".into());
        let types = vec![
            (Type::Void, "void"),
            (Type::Bool, "bool"),
            (Type::Int8, "i8"),
            (Type::UInt8, "u8"),
            (Type::Int32, "i32"),
            (Type::UInt32, "u32"),
            (Type::Float32, "f32"),
            (Type::Float64, "f64"),
        ];

        for (ty, expected_suffix) in types {
            let type_args = vec![mk_type(ty)];
            let mangled = mangle_name(name, &type_args);
            let expected = Name::new(format!("fn${}", expected_suffix));
            assert_eq!(mangled, expected);
        }
    }

    #[test]
    fn test_mangle_array_type() {
        let name = Name::new("process".into());
        let type_args = vec![mk_type(Type::Array(mk_type(Type::Float32), 10))];
        let mangled = mangle_name(name, &type_args);
        assert_eq!(mangled, Name::new("process$[f32;10]".into()));
    }

    #[test]
    fn test_mangle_tuple_type() {
        let name = Name::new("swap".into());
        let tuple_ty = mk_type(Type::Tuple(vec![
            mk_type(Type::Int32),
            mk_type(Type::Bool),
        ]));
        let type_args = vec![tuple_ty];
        let mangled = mangle_name(name, &type_args);
        assert_eq!(mangled, Name::new("swap$(i32,bool)".into()));
    }

    #[test]
    fn test_mangle_function_type() {
        let name = Name::new("apply".into());
        let func_ty = mk_type(Type::Func(
            mk_type(Type::Int32),
            mk_type(Type::Bool),
        ));
        let type_args = vec![func_ty];
        let mangled = mangle_name(name, &type_args);
        assert_eq!(mangled, Name::new("apply$fn(i32)_bool".into()));
    }

    #[test]
    fn test_mangle_named_type_no_params() {
        let name = Name::new("convert".into());
        let named_ty = mk_type(Type::Name(Name::new("MyStruct".into()), vec![]));
        let type_args = vec![named_ty];
        let mangled = mangle_name(name, &type_args);
        assert_eq!(mangled, Name::new("convert$MyStruct".into()));
    }

    #[test]
    fn test_mangle_named_type_with_params() {
        let name = Name::new("process".into());
        let generic_struct = mk_type(Type::Name(
            Name::new("GenericStruct".into()),
            vec![mk_type(Type::Int32)],
        ));
        let type_args = vec![generic_struct];
        let mangled = mangle_name(name, &type_args);
        assert_eq!(mangled, Name::new("process$GenericStruct<i32>".into()));
    }

    #[test]
    fn test_mangle_nested_generic() {
        let name = Name::new("fn".into());
        // GenericStruct<GenericStruct<i32>>
        let inner = mk_type(Type::Name(
            Name::new("GenericStruct".into()),
            vec![mk_type(Type::Int32)],
        ));
        let outer = mk_type(Type::Name(
            Name::new("GenericStruct".into()),
            vec![inner],
        ));
        let type_args = vec![outer];
        let mangled = mangle_name(name, &type_args);
        assert_eq!(
            mangled,
            Name::new("fn$GenericStruct<GenericStruct<i32>>".into())
        );
    }

    #[test]
    fn test_mangle_complex_combination() {
        let name = Name::new("complex".into());
        // Array of tuples containing a function
        let func_ty = mk_type(Type::Func(
            mk_type(Type::Int32),
            mk_type(Type::Bool),
        ));
        let tuple_ty = mk_type(Type::Tuple(vec![
            func_ty,
            mk_type(Type::Float32),
        ]));
        let array_ty = mk_type(Type::Array(tuple_ty, 5));
        let type_args = vec![array_ty];
        let mangled = mangle_name(name, &type_args);
        assert_eq!(
            mangled,
            Name::new("complex$[(fn(i32)_bool,f32);5]".into())
        );
    }

    #[test]
    fn test_mangle_type_variable() {
        let name = Name::new("fn".into());
        let type_args = vec![typevar("T")];
        let mangled = mangle_name(name, &type_args);
        assert_eq!(mangled, Name::new("fn$TT".into()));
    }

    #[test]
    fn test_mangle_anonymous_type_var() {
        let name = Name::new("fn".into());
        let type_args = vec![anon(42)];
        let mangled = mangle_name(name, &type_args);
        assert_eq!(mangled, Name::new("fn$#42".into()));
    }

    #[test]
    fn test_mangle_uniqueness() {
        let name = Name::new("id".into());

        // id<i32> and id<u32> should produce different mangled names
        let mangled1 = mangle_name(name, &[mk_type(Type::Int32)]);
        let mangled2 = mangle_name(name, &[mk_type(Type::UInt32)]);
        assert_ne!(mangled1, mangled2);

        // id<i32, bool> and id<bool, i32> should be different
        let mangled3 = mangle_name(
            name,
            &[mk_type(Type::Int32), mk_type(Type::Bool)],
        );
        let mangled4 = mangle_name(
            name,
            &[mk_type(Type::Bool), mk_type(Type::Int32)],
        );
        assert_ne!(mangled3, mangled4);
    }

    #[test]
    fn test_mangle_deterministic() {
        let name = Name::new("map".into());
        let type_args = vec![mk_type(Type::Int32), mk_type(Type::Float32)];

        // Multiple calls with same inputs should produce same output
        let mangled1 = mangle_name(name, &type_args);
        let mangled2 = mangle_name(name, &type_args);
        assert_eq!(mangled1, mangled2);
    }

    // Tests for type complexity calculation

    #[test]
    fn test_complexity_primitives() {
        assert_eq!(type_complexity(mk_type(Type::Int32)), 0);
        assert_eq!(type_complexity(mk_type(Type::Bool)), 0);
        assert_eq!(type_complexity(mk_type(Type::Float32)), 0);
    }

    #[test]
    fn test_complexity_array() {
        let i32_array = mk_type(Type::Array(mk_type(Type::Int32), 10));
        assert_eq!(type_complexity(i32_array), 1);

        let nested_array = mk_type(Type::Array(i32_array, 5));
        assert_eq!(type_complexity(nested_array), 2);
    }

    #[test]
    fn test_complexity_named_types() {
        // Simple named type
        let simple = mk_type(Type::Name(Name::new("MyStruct".into()), vec![]));
        assert_eq!(type_complexity(simple), 0);

        // Generic with one param
        let generic = mk_type(Type::Name(
            Name::new("Vec".into()),
            vec![mk_type(Type::Int32)],
        ));
        assert_eq!(type_complexity(generic), 1);

        // Nested generic Vec<Vec<i32>>
        let nested = mk_type(Type::Name(
            Name::new("Vec".into()),
            vec![generic],
        ));
        assert_eq!(type_complexity(nested), 2);
    }

    #[test]
    fn test_is_more_complex_simple() {
        let i32_ty = mk_type(Type::Int32);
        let vec_i32 = mk_type(Type::Name(
            Name::new("Vec".into()),
            vec![i32_ty],
        ));

        // Vec<i32> is more complex than i32
        assert!(is_more_complex(&[vec_i32], &[i32_ty]));

        // i32 is not more complex than Vec<i32>
        assert!(!is_more_complex(&[i32_ty], &[vec_i32]));

        // Same complexity
        assert!(!is_more_complex(&[i32_ty], &[i32_ty]));
    }

    #[test]
    fn test_is_more_complex_nested() {
        let i32_ty = mk_type(Type::Int32);
        let vec_i32 = mk_type(Type::Name(
            Name::new("Vec".into()),
            vec![i32_ty],
        ));
        let vec_vec_i32 = mk_type(Type::Name(
            Name::new("Vec".into()),
            vec![vec_i32],
        ));

        // Vec<Vec<i32>> is more complex than Vec<i32>
        assert!(is_more_complex(&[vec_vec_i32], &[vec_i32]));

        // Vec<Vec<i32>> is more complex than i32
        assert!(is_more_complex(&[vec_vec_i32], &[i32_ty]));
    }

    // Tests for RecursionDetector

    #[test]
    fn test_recursion_detector_no_recursion() {
        let mut detector = RecursionDetector::new();
        let key1 = MonomorphKey::new(
            Name::new("foo".into()),
            vec![mk_type(Type::Int32)],
        );

        assert!(detector.check(&key1).is_ok());
        detector.begin_instantiation(key1.clone());
        assert_eq!(detector.depth(), 1);

        // Different function is OK
        let key2 = MonomorphKey::new(
            Name::new("bar".into()),
            vec![mk_type(Type::Int32)],
        );
        assert!(detector.check(&key2).is_ok());

        detector.end_instantiation();
        assert_eq!(detector.depth(), 0);
    }

    #[test]
    fn test_recursion_detector_same_instantiation() {
        let mut detector = RecursionDetector::new();
        let key = MonomorphKey::new(
            Name::new("foo".into()),
            vec![mk_type(Type::Int32)],
        );

        detector.begin_instantiation(key.clone());

        // Trying to instantiate the same thing again - recursion!
        let result = detector.check(&key);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("already being instantiated"));
    }

    #[test]
    fn test_recursion_detector_increasing_complexity() {
        let mut detector = RecursionDetector::new();
        let name = Name::new("foo".into());

        let i32_ty = mk_type(Type::Int32);
        let vec_i32 = mk_type(Type::Name(
            Name::new("Vec".into()),
            vec![i32_ty],
        ));
        let vec_vec_i32 = mk_type(Type::Name(
            Name::new("Vec".into()),
            vec![vec_i32],
        ));

        let key1 = MonomorphKey::new(name, vec![i32_ty]);
        detector.begin_instantiation(key1);

        // Trying to instantiate foo<Vec<i32>> while foo<i32> is in progress
        let key2 = MonomorphKey::new(name, vec![vec_i32]);
        let result = detector.check(&key2);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("increasingly complex"));

        // Even more complex should also fail
        let key3 = MonomorphKey::new(name, vec![vec_vec_i32]);
        let result = detector.check(&key3);
        assert!(result.is_err());
    }

    #[test]
    fn test_recursion_detector_decreasing_complexity_ok() {
        let mut detector = RecursionDetector::new();
        let name = Name::new("unwrap".into());

        let i32_ty = mk_type(Type::Int32);
        let vec_i32 = mk_type(Type::Name(
            Name::new("Vec".into()),
            vec![i32_ty],
        ));

        let key1 = MonomorphKey::new(name, vec![vec_i32]);
        detector.begin_instantiation(key1);

        // Trying to instantiate unwrap<i32> while unwrap<Vec<i32>> is in progress
        // This is OK - complexity is decreasing (type-decreasing recursion)
        let key2 = MonomorphKey::new(name, vec![i32_ty]);
        let result = detector.check(&key2);
        assert!(result.is_ok());
    }

    #[test]
    fn test_recursion_detector_multiple_params() {
        let mut detector = RecursionDetector::new();
        let name = Name::new("foo".into());

        let i32_ty = mk_type(Type::Int32);
        let bool_ty = mk_type(Type::Bool);
        let vec_i32 = mk_type(Type::Name(
            Name::new("Vec".into()),
            vec![i32_ty],
        ));

        let key1 = MonomorphKey::new(name, vec![i32_ty, bool_ty]);
        detector.begin_instantiation(key1);

        // Increasing complexity in first param
        let key2 = MonomorphKey::new(name, vec![vec_i32, bool_ty]);
        let result = detector.check(&key2);
        assert!(result.is_err());
    }

    #[test]
    fn test_recursion_detector_stack() {
        let mut detector = RecursionDetector::new();

        let key1 = MonomorphKey::new(
            Name::new("f".into()),
            vec![mk_type(Type::Int32)],
        );
        let key2 = MonomorphKey::new(
            Name::new("g".into()),
            vec![mk_type(Type::Bool)],
        );

        detector.begin_instantiation(key1.clone());
        detector.begin_instantiation(key2.clone());

        assert_eq!(detector.depth(), 2);

        let stack = detector.stack();
        assert_eq!(stack.len(), 2);
        assert_eq!(stack[0], key1);
        assert_eq!(stack[1], key2);

        detector.end_instantiation();
        assert_eq!(detector.depth(), 1);

        detector.end_instantiation();
        assert_eq!(detector.depth(), 0);
    }

    #[test]
    fn test_monomorph_key() {
        let name = Name::new("foo".into());
        let type_args = vec![mk_type(Type::Int32), mk_type(Type::Bool)];
        let key = MonomorphKey::new(name, type_args);

        assert_eq!(key.mangled_name(), Name::new("foo$i32$bool".into()));
    }

    #[test]
    fn test_monomorph_key_equality() {
        let name = Name::new("foo".into());
        let type_args = vec![mk_type(Type::Int32)];

        let key1 = MonomorphKey::new(name, type_args.clone());
        let key2 = MonomorphKey::new(name, type_args);

        assert_eq!(key1, key2);

        // Different name
        let key3 = MonomorphKey::new(
            Name::new("bar".into()),
            vec![mk_type(Type::Int32)],
        );
        assert_ne!(key1, key3);

        // Different type args
        let key4 = MonomorphKey::new(name, vec![mk_type(Type::Bool)]);
        assert_ne!(key1, key4);
    }
}
