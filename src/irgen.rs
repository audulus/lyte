use crate::*;

pub struct Irgen {
    pub types: Vec<TypeID>,
    next_tmp: usize,
}

impl Irgen {
    fn tmp(&mut self) -> Name {
        self.next_tmp += 1;
        Name::new(format!("tmp_{}", self.next_tmp))
    }

    fn load_constant(&mut self, block: &mut ir::BasicBlock, c: ir::Constant) -> Name {
        let name = self.tmp();
        block
            .stmts
            .push(ir::Stmt::Load(name, ir::Constant::Bool(false)));
        name
    }

    fn gen_expr(
        &mut self,
        block: &mut ir::BasicBlock,
        id: ExprID,
        arena: &ExprArena,
        decls: &DeclTable,
    ) -> Name {
        match &arena[id] {
            Expr::True => self.load_constant(block, ir::Constant::Bool(true)),
            Expr::False => self.load_constant(block, ir::Constant::Bool(false)),
            Expr::Int(x) => self.load_constant(block, ir::Constant::Int(*x)),
            Expr::Real(x) => self.load_constant(block, ir::Constant::Float(x.parse().unwrap())),
            _ => self.tmp(),
        }
    }
}
