
// use std::rc::Rc;
// use std::collections::HashSet;

pub mod ast;

#[cfg(test)]
mod tests {
    use super::*;
    use ast::{unify, Type};

    #[test]
    fn test_basic_types() {
        assert_eq!(Type::Void, Type::Void);
        assert_ne!(Type::Void, Type::Int8);
    }

    #[test]
    fn test_tuple() {
        assert_eq!(Type::Tuple(Box::new(Type::Void), Box::new(Type::Void)), Type::Tuple(Box::new(Type::Void), Box::new(Type::Void)));
    }

    #[test]
    fn test_unify() {
        let mut inst = vec![Type::Void];
        assert!(unify(Type::Void, Type::Void, &mut inst));
        assert!(!unify(Type::Void, Type::Int8, &mut inst));
    
        assert!(unify(Type::Var(0), Type::Int8, &mut inst));
        assert_eq!(inst[0], Type::Int8);
    }
}

fn main() {
    println!("yo")
}
