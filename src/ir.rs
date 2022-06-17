use crate::*;

enum Constant {
    Float(f32),
    Int(i32),
}

enum Stmt {
    Load(Name, Constant),
    Unop(Name, Name),
    Binop(Name, Name, Binop, Name),
    Field(Name, Name, Name),
}

pub type BlockID = usize;

enum Terminator {
    GoTo(BlockID),
    Cond(Name, BlockID, BlockID),
    Switch(Name, Vec<BlockID>),
    Call(Name, Name, Vec<Name>, BlockID),
}

struct BasicBlock {
    stmts: Vec<Stmt>,
    term: Terminator,
}
