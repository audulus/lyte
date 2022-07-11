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

    fn load_constant(
        &mut self,
        block: ir::BlockID,
        block_arena: &mut ir::BlockArena,
        c: ir::Constant,
    ) -> Name {
        let name = self.tmp();
        block_arena.blocks[block]
            .stmts
            .push(ir::Stmt::Load(name, c));
        name
    }

    fn gen_expr(
        &mut self,
        block: ir::BlockID,
        block_arena: &mut ir::BlockArena,
        id: ExprID,
        arena: &ExprArena,
        decls: &DeclTable,
    ) -> Name {
        match &arena[id] {
            Expr::True => self.load_constant(block, block_arena, ir::Constant::Bool(true)),
            Expr::False => self.load_constant(block, block_arena, ir::Constant::Bool(false)),
            Expr::Int(x) => self.load_constant(block, block_arena, ir::Constant::Int(*x)),
            Expr::Real(x) => {
                self.load_constant(block, block_arena, ir::Constant::Float(x.parse().unwrap()))
            }
            Expr::Id(name) => *name,
            _ => self.tmp(),
        }
    }

    fn gen_fn_decl(
        &mut self,
        block_arena: &mut ir::BlockArena,
        func_decl: &FuncDecl,
        arena: &ExprArena,
        decls: &DeclTable,
    ) {
        if let Some(body) = func_decl.body {
            let block_id = block_arena.add_block();

            self.gen_expr(block_id, block_arena, body, arena, decls);
        }
    }
}
