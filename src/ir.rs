use crate::*;

#[derive(Debug)]
pub enum Constant {
    Float(f32),
    Int(i64),
    Bool(bool),
}

#[derive(Debug)]
pub enum Stmt {
    Var(Name, Type),
    Load(Name, Constant),
    Unop(Name, Name),
    Binop(Name, Name, Binop, Name),
    Field(Name, Name, Name),
}

pub type BlockID = usize;

#[derive(Debug)]
pub enum Terminator {
    GoTo(BlockID),
    Cond(Name, BlockID, BlockID),
    Switch(Name, Vec<BlockID>),
    Call(Name, Name, Vec<Name>, BlockID),
    None,
}

#[derive(Debug)]
pub struct BasicBlock {
    pub stmts: Vec<Stmt>,
    pub term: Terminator,
}

impl BasicBlock {
    pub fn new() -> Self {
        BasicBlock {
            stmts: vec![],
            term: Terminator::None,
        }
    }
}

#[derive(Debug)]
pub struct BlockArena {
    pub blocks: Vec<BasicBlock>,
}

impl BlockArena {
    pub fn add_block(&mut self) -> BlockID {
        let id = self.blocks.len();
        self.blocks.push(BasicBlock::new());
        id
    }
}
