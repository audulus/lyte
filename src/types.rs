use std::collections::HashMap;
use crate::defs::*;

pub const VOID: TypeID = TypeID { index: 0 };
pub const INT8: TypeID = TypeID { index: 1 };
pub const INT32: TypeID = TypeID { index: 2 };

pub type Instance = HashMap<u32, TypeID>;

impl Compiler {

    pub fn mk_type(&mut self, proto: Type) -> TypeID {
        // Dumb linear search.
        for i in 0..self.types.len() {
            if self.types[i] == proto {
                return TypeID { index: i as u32 };
            }
        }

        let ix = self.types.len();
        self.types.push(proto);
        return TypeID { index: ix as u32 };
    }

    pub fn fresh(&mut self) -> TypeID {
        self.typevar_names.push("anonymous".to_string());
        self.mk_type(Type::Var( (self.typevar_names.len() - 1) as u32))
    }

    pub fn typevar(&mut self, name: &str) -> TypeID {
        self.typevar_names.push(name.to_string());
        self.mk_type(Type::Var( (self.typevar_names.len() - 1) as u32))
    }

    pub fn subst(&mut self, t: TypeID, inst: &Instance) -> TypeID {
        match self.types[t.index as usize] {
            Type::Tuple(a, b) => {
                let nt = Type::Tuple(self.subst(a, inst), self.subst(b, inst));
                self.mk_type(nt)
            }
            Type::Var(i) => match inst.get(&i) {
                Some(t0) => *t0,
                None => t,
            },
            _ => t,
        }
    }

    pub fn solved(&self, t: TypeID) -> bool {
        match self.types[t.index as usize] {
            Type::Tuple(a, b) => self.solved(a) && self.solved(b),
            Type::Func(a, b) => self.solved(a) && self.solved(b),
            Type::Var(_) => false,
            _ => true,
        }
    }

    pub fn unify(&self, lhs: TypeID, rhs: TypeID, inst: &mut Instance) -> bool {
        if lhs == rhs {
            true
        } else {
            match (
                self.types[lhs.index as usize],
                self.types[rhs.index as usize],
            ) {
                (Type::Tuple(a, b), Type::Tuple(c, d)) => {
                    self.unify(a, c, inst) && self.unify(b, d, inst)
                }
                (Type::Func(a, b), Type::Func(c, d)) => {
                    self.unify(a, c, inst) && self.unify(b, d, inst)
                }
                (Type::Var(i), _) => {
                    inst.insert(i, rhs);
                    true
                }
                (_, Type::Var(i)) => {
                    inst.insert(i, lhs);
                    true
                }
                _ => false,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mk_type() {
        let mut compiler = Compiler::new();
        let v0 = compiler.mk_type(Type::Void);
        let v1 = compiler.mk_type(Type::Void);
        assert_eq!(v0, v1);

        let i0 = compiler.mk_type(Type::Int8);
        assert_ne!(v0, i0);
    }

    #[test]
    fn test_unify() {
        let mut inst = Instance::new();
        let mut compiler = Compiler::new();
        assert!(compiler.unify(VOID, VOID, &mut inst));
        assert!(!compiler.unify(VOID, INT8, &mut inst));

        let var = compiler.mk_type(Type::Var(0));
        assert!(compiler.unify(var, INT8, &mut inst));

        match inst.get(&0) {
            Some(t) => assert_eq!(*t, INT8),
            None => assert!(false),
        }
    }
}
