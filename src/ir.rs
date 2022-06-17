use crate::*;

enum Constant {
    float(f32),
    int(i32),
}

enum Stmt {
    Load(Name, Constant),
    Unop(Name, Name),
    Binop(Name, Name, Binop, Name),
    Field(Name, Name, Name),
}

pub type BlockID = usize;

enum Terminator {
    Oo(BlockID),
    Cond(Name, BlockID, BlockID),
    Switch(Name, Vec<BlockID>),
    Call(Name, Name, Vec<Name>, BlockID),
}

struct BasicBlock {
    stmts: Vec<Stmt>,
    term: Terminator,
}
