use crate::*;

pub enum Constant {
    Float(f32),
    Int(i64),
    Bool(bool),
}

pub enum Stmt {
    Var(Name, Type),
    Load(Name, Constant),
    Unop(Name, Name),
    Binop(Name, Name, Binop, Name),
    Field(Name, Name, Name),
}

pub type BlockID = usize;

pub enum Terminator {
    GoTo(BlockID),
    Cond(Name, BlockID, BlockID),
    Switch(Name, Vec<BlockID>),
    Call(Name, Name, Vec<Name>, BlockID),
}

pub struct BasicBlock {
    pub stmts: Vec<Stmt>,
    pub term: Terminator,
}

pub struct BlockArena {
    pub blocks: Vec<BasicBlock>,
}
