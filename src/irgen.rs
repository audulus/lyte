use crate::*;

#[derive(Debug)]
pub struct Irgen {
    pub types: Vec<TypeID>,
    next_tmp: usize,
}

impl Irgen {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            next_tmp: 0,
        }
    }

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
            Expr::Unop(expr) => {
                let name = self.tmp();
                let a = self.gen_expr(block, block_arena, *expr, arena, decls);
                block_arena.blocks[block]
                    .stmts
                    .push(ir::Stmt::Unop(name, Unop::Neg, a));
                name
            }
            Expr::Binop(op, lhs, rhs) => {
                let name = self.tmp();
                let a = self.gen_expr(block, block_arena, *lhs, arena, decls);
                let b = self.gen_expr(block, block_arena, *rhs, arena, decls);
                block_arena.blocks[block]
                    .stmts
                    .push(ir::Stmt::Binop(name, a, *op, b));
                name
            }
            Expr::Field(lhs, field_name) => {
                let name = self.tmp();
                let a = self.gen_expr(block, block_arena, *lhs, arena, decls);
                block_arena.blocks[block]
                    .stmts
                    .push(ir::Stmt::Field(name, a, *field_name));
                name
            }
            Expr::Call(f, args) => {
                let name = self.tmp();
                let fname = self.gen_expr(block, block_arena, *f, arena, decls);
                let argnames = args
                    .iter()
                    .map(|arg| self.gen_expr(block, block_arena, *arg, arena, decls))
                    .collect();
                block_arena.blocks[block]
                    .stmts
                    .push(ir::Stmt::Call(name, fname, argnames));
                name
            }
            Expr::Lambda { params, body } => {
                let name = self.tmp();

                let block_id = block_arena.add_block();
                self.gen_expr(block_id, block_arena, *body, arena, decls);

                name
            }
            Expr::If(cond, then, els) => {
                let name = self.tmp();
                let c = self.gen_expr(block, block_arena, *cond, arena, decls);

                let then_block = block_arena.add_block();
                let then_result = self.gen_expr(then_block, block_arena, *then, arena, decls);
                block_arena.blocks[block]
                    .stmts
                    .push(ir::Stmt::Copy(name, then_result));

                if let Some(els) = *els {
                    let els_block = block_arena.add_block();
                    let els_result = self.gen_expr(els_block, block_arena, els, arena, decls);
                    block_arena.blocks[els_block]
                        .stmts
                        .push(ir::Stmt::Copy(name, els_result));
                    block_arena.blocks[block].term = ir::Terminator::Cond(c, then_block, els_block);
                } else {
                    block_arena.blocks[block].term = ir::Terminator::If(c, then_block);
                };

                name
            }
            Expr::While(cond, body) => {
                let name = self.tmp();

                let body_block = block_arena.add_block();
                _ = self.gen_expr(body_block, block_arena, *body, arena, decls);

                let exit_block = block_arena.add_block();

                let cond_block = block_arena.add_block();
                let c = self.gen_expr(cond_block, block_arena, *cond, arena, decls);
                block_arena.blocks[cond_block].term =
                    ir::Terminator::Cond(c, body_block, exit_block);
                name
            }
            _ => self.tmp(),
        }
    }

    pub fn gen_fn_decl(
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

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ir::BlockArena;

    #[test]
    fn test_irgen() {
        let mut irgen = Irgen::new();
        let mut ir = BlockArena::new();
        let mut exprs = ExprArena::new();
        let loc = Loc {
            file: Name::new("test_irgen".into()),
            line: 1,
        };
        let e = exprs.add(Expr::True, loc);
        let decls = DeclTable::new(Vec::new());

        let block = ir.add_block();
        irgen.gen_expr(block, &mut ir, e, &exprs, &decls);

        println!("ir: {:?}", ir);
        assert_eq!(ir.blocks[0].stmts.len(), 1);
    }
}
