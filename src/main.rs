
use std::rc::Rc;
// use std::collections::HashSet;

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum Type {
	Void,
	Int8,
	Int32,
	Tuple(Rc<Type>, Rc<Type>)
}

pub struct TypeFactory {
    types: Vec<Rc<Type>>,
}

impl TypeFactory {
    pub fn new() -> TypeFactory {
        TypeFactory { types: [].to_vec() }
    }

    pub fn unique(&mut self, t: Type) -> Rc<Type> {
        for p in &self.types {
            if **p == t {
                return p.clone();
            }
        }
        let p = Rc::new(t);
        self.types.push(p.clone());
        return p
    }

    pub fn tuple(&mut self, a: Rc<Type>, b: Rc<Type>) -> Rc<Type> {
        self.unique(Type::Tuple(a, b))
    }
}

fn main() {
    let mut f = TypeFactory::new();

    let t0 = f.unique(Type::Void);
    let t1 = f.unique(Type::Void);

    assert_eq!(t0, t1);

    let t2 = f.unique(Type::Int8);

    assert_ne!(t1, t2);

    let t3 = f.tuple(t2.clone(), t2.clone());
    let t4 = f.tuple(t2.clone(), t2.clone());

    assert_eq!(t3, t4);

    let t5 = f.unique(Type::Tuple(t2.clone(), t0.clone()));

    assert_ne!(t3, t5);
}
