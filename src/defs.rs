

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub struct TypeID {
    pub index: u32,
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    Int8,
    Int32,
    Tuple(TypeID, TypeID),
    Var(u32),
    Func(TypeID, TypeID),
}

pub struct Compiler {
    pub types: Vec<Type>,
    pub var_index: u32,
    
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            types: vec![Type::Void, Type::Int8, Type::Int32],
            var_index: 0
        }
    }
}