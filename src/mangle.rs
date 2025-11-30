use crate::*;

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
}
