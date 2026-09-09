use crate::*;

/// A specialization is a source definition and its concrete arguments. Symbol
/// spelling and overload signatures are deliberately absent from its identity.
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct MonomorphKey {
    pub definition: DefId,
    pub type_args: Vec<TypeID>,
    pub size_args: Vec<i32>,
}

impl MonomorphKey {
    pub fn new(definition: DefId, type_args: Vec<TypeID>, size_args: Vec<i32>) -> Self {
        Self {
            definition,
            type_args,
            size_args,
        }
    }
}

/// Retains the language's existing increasing-type-complexity guard. Ordinary
/// recursion is handled by reserving an instance before walking its body.
#[derive(Debug, Default)]
pub struct RecursionDetector {
    in_progress: Vec<MonomorphKey>,
}

impl RecursionDetector {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn check(&self, key: &MonomorphKey, name: Name) -> Result<(), String> {
        if self.in_progress.contains(key) {
            return Err(format!(
                "Infinite generic recursion detected: {} with type args {:?} is already being instantiated",
                name, key.type_args
            ));
        }
        for previous in &self.in_progress {
            if previous.definition == key.definition
                && is_more_complex(&key.type_args, &previous.type_args)
            {
                return Err(format!(
                    "Infinite generic recursion detected: {} is being instantiated with increasingly complex types.\n\
                     Previous: {:?}\n\
                     Current:  {:?}",
                    name, previous.type_args, key.type_args
                ));
            }
        }
        Ok(())
    }

    pub fn begin_instantiation(&mut self, key: MonomorphKey) {
        self.in_progress.push(key);
    }

    pub fn end_instantiation(&mut self) {
        self.in_progress.pop();
    }
}

fn is_more_complex(current: &[TypeID], previous: &[TypeID]) -> bool {
    current.len() == previous.len()
        && current
            .iter()
            .zip(previous)
            .any(|(current, previous)| type_complexity(*current) > type_complexity(*previous))
}

fn type_complexity(ty: TypeID) -> usize {
    match &*ty {
        Type::Void
        | Type::Bool
        | Type::Int8
        | Type::UInt8
        | Type::Int32
        | Type::UInt32
        | Type::Float32
        | Type::Float64
        | Type::Float32x4
        | Type::Var(_)
        | Type::Anon(_) => 0,
        Type::Array(elem, _) | Type::Slice(elem) | Type::Reference(elem) => {
            1 + type_complexity(*elem)
        }
        Type::Tuple(types) => {
            1 + types
                .iter()
                .map(|ty| type_complexity(*ty))
                .max()
                .unwrap_or(0)
        }
        Type::Func(domain, result) => 1 + type_complexity(*domain).max(type_complexity(*result)),
        Type::Name(_, params) if params.is_empty() => 0,
        Type::Name(_, params) => {
            1 + params
                .iter()
                .map(|ty| type_complexity(*ty))
                .max()
                .unwrap_or(0)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recursion_checks_definitions_rather_than_shared_spelling() {
        let mut detector = RecursionDetector::new();
        let first = MonomorphKey::new(DefId(0), vec![mk_type(Type::Int32)], vec![]);
        detector.begin_instantiation(first.clone());
        let nested = mk_type(Type::Array(mk_type(Type::Int32), ArraySize::Known(3)));
        let same_name_other_overload = MonomorphKey::new(DefId(1), vec![nested], vec![]);
        assert!(detector
            .check(&same_name_other_overload, Name::str("f"))
            .is_ok());
        let growing = MonomorphKey::new(DefId(0), vec![nested], vec![]);
        assert!(detector.check(&growing, Name::str("f")).is_err());
        detector.end_instantiation();
        assert!(detector.check(&growing, Name::str("f")).is_ok());
    }

    #[test]
    fn decreasing_types_remain_accepted() {
        let mut detector = RecursionDetector::new();
        let nested = mk_type(Type::Array(mk_type(Type::Int32), ArraySize::Known(3)));
        detector.begin_instantiation(MonomorphKey::new(DefId(0), vec![nested], vec![]));
        assert!(detector
            .check(
                &MonomorphKey::new(DefId(0), vec![mk_type(Type::Int32)], vec![]),
                Name::str("f")
            )
            .is_ok());
    }
}
