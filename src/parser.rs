use crate::defs::*;
use crate::lexer::*;
use crate::types::*;
use std::hash::Hash;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ParseError {
    pub location: Loc,
    pub message: String,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct ExprArena {
    pub exprs: Vec<Expr>,
    pub locs: Vec<Loc>,
    pub errors: Vec<ParseError>,
}

impl ExprArena {
    pub fn new() -> Self {
        Self {
            exprs: vec![],
            locs: vec![],
            errors: vec![],
        }
    }

    pub fn add(&mut self, expr: Expr, loc: Loc) -> ExprID {
        let id = self.exprs.len();
        self.exprs.push(expr);
        self.locs.push(loc);
        id
    }
}

impl std::ops::Index<ExprID> for ExprArena {
    type Output = Expr;

    fn index(&self, index: ExprID) -> &Self::Output {
        &self.exprs[index]
    }
}

fn expect(lexer: &mut Lexer, tok: Token, errors: &mut Vec<ParseError>) {
    if lexer.tok == tok {
        lexer.next();
    } else {
        let message = format!("expected {:?}, got {:?}", tok, lexer.tok);
        errors.push(ParseError {
            location: lexer.loc,
            message,
        })
    }
}

fn expect_id(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> Name {
    if let Token::Id(string) = &lexer.tok {
        let result = Name::new(string.clone());
        lexer.next();
        result
    } else {
        let message = format!("expected identifier, got {:?}", lexer.tok);
        println!("{:?}", message);
        errors.push(ParseError {
            location: lexer.loc,
            message,
        });
        Name::new("".to_string())
    }
}

fn parse_typelist(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> Vec<TypeID> {
    let mut r = vec![];
    expect(lexer, Token::Less, errors);

    loop {
        r.push(parse_basic_type(lexer, errors));

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();
    }

    expect(lexer, Token::Greater, errors);

    r
}

fn parse_basic_type(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> TypeID {
    let t = mk_type(match &lexer.tok {
        Token::Void => {
            lexer.next();
            Type::Void
        }
        Token::Bool => {
            lexer.next();
            Type::Bool
        }
        Token::Int8 => {
            lexer.next();
            Type::Int8
        }
        Token::Int32 => {
            lexer.next();
            Type::Int32
        }
        Token::Lmath => {
            lexer.next();
            let name = expect_id(lexer, errors);
            expect(lexer, Token::Rmath, errors);
            return typevar(&name);
        }
        Token::Typevar => {
            lexer.next();
            let name = expect_id(lexer, errors);
            return typevar(&name);
        }
        Token::Lbracket => {
            lexer.next();
            let r = parse_type(lexer, errors);
            if lexer.tok == Token::Semi {
                lexer.next();
                if let Token::Integer(n) = lexer.tok.clone() {
                    lexer.next();
                    expect(lexer, Token::Rbracket, errors);
                    Type::Array(r, n)
                } else {
                    errors.push(ParseError {
                        location: lexer.loc,
                        message: String::from("Expected integer array size"),
                    });
                    Type::Array(r, 0)
                }
            } else {
                expect(lexer, Token::Rbracket, errors);
                Type::Array(r, 0)
            }
        }
        Token::Id(name) => {
            let name = Name::new(name.clone());
            lexer.next();
            let mut args = vec![];
            if lexer.tok == Token::Less {
                args = parse_typelist(lexer, errors);
            }
            return mk_type(Type::Name(name, args));
        }
        Token::Lparen => {
            lexer.next();
            let t = parse_type(lexer, errors);
            expect(lexer, Token::Rparen, errors);
            return t;
        }
        _ => {
            lexer.next();
            errors.push(ParseError {
                location: lexer.loc,
                message: String::from("Expected type"),
            });
            Type::Void
        }
    });
    t
}

fn parse_type(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> TypeID {
    let mut lhs = parse_basic_type(lexer, errors);

    while lexer.tok == Token::Arrow {
        lexer.next();
        let rhs = parse_basic_type(lexer, errors);

        // Ensure we're always calling with a tuple.
        if let Type::Tuple(_) = *lhs {
            lhs = mk_type(Type::Func(lhs, rhs));
        } else {
            let args = mk_type(Type::Tuple(vec![lhs]));
            lhs = mk_type(Type::Func(args, rhs));
        }
    }

    lhs
}

fn parse_paramlist(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> Vec<Param> {
    let mut r = vec![];

    loop {
        if let Token::Id(name) = &lexer.tok {
            let name = name.clone();
            lexer.next();

            // In the case of lambdas, we can omit the types.
            let ty = if lexer.tok == Token::Colon {
                lexer.next();
                Some(parse_type(lexer, errors))
            } else {
                None
            };

            r.push(Param {
                name: Name::new(name),
                ty,
            })
        }

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();

        skip_newlines(lexer);
    }

    r
}

fn binop(tok: &Token, lhs: ExprID, rhs: ExprID) -> Expr {
    let op = match tok {
        Token::Assign => Binop::Assign,
        Token::Plus => Binop::Plus,
        Token::Minus => Binop::Minus,
        Token::Mult => Binop::Mult,
        Token::Div => Binop::Div,
        Token::Leq => Binop::Leq,
        Token::Geq => Binop::Geq,
        Token::Less => Binop::Less,
        Token::Greater => Binop::Greater,
        Token::Equal => Binop::Equal,
        Token::NotEqual => Binop::NotEqual,
        Token::Power => Binop::Pow,
        _ => {
            println!("got {:?}", tok);
            unreachable!()
        }
    };

    Expr::Binop(op, lhs, rhs)
}

fn parse_lambda(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    if lexer.tok == Token::Pipe {
        lexer.next();
        let params = parse_paramlist(lexer, &mut arena.errors);
        expect(lexer, Token::Pipe, &mut arena.errors);

        let body = parse_lambda(lexer, arena);

        arena.add(Expr::Lambda { params, body }, lexer.loc)
    } else {
        parse_expr(lexer, arena)
    }
}

fn parse_if(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    let cond = parse_expr(lexer, arena);
    let then = parse_block(lexer, arena);

    let els = if lexer.tok == Token::Else {
        lexer.next();
        if lexer.tok == Token::If {
            lexer.next();
            Some(parse_if(lexer, arena))
        } else {
            Some(parse_block(lexer, arena))
        }
    } else {
        None
    };

    arena.add(Expr::If(cond, then, els), lexer.loc)
}

fn parse_expr(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    if lexer.tok == Token::If {
        lexer.next();
        parse_if(lexer, arena)
    } else {
        parse_assign(lexer, arena)
    }
}

fn parse_assign(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    let mut lhs = parse_eq(lexer, arena);

    while lexer.tok == Token::Assign {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_eq(lexer, arena);

        lhs = arena.add(binop(&t, lhs, rhs), lexer.loc)
    }

    lhs
}

fn parse_eq(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    let mut lhs = parse_rel(lexer, arena);

    while lexer.tok == Token::Equal || lexer.tok == Token::NotEqual {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_rel(lexer, arena);

        lhs = arena.add(binop(&t, lhs, rhs), lexer.loc)
    }

    lhs
}

fn parse_rel(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    let mut lhs = parse_sum(lexer, arena);

    while lexer.tok == Token::Leq
        || lexer.tok == Token::Geq
        || lexer.tok == Token::Less
        || lexer.tok == Token::Greater
    {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_sum(lexer, arena);

        lhs = arena.add(binop(&t, lhs, rhs), lexer.loc)
    }

    lhs
}

fn parse_sum(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    let mut lhs = parse_term(lexer, arena);

    while lexer.tok == Token::Plus || lexer.tok == Token::Minus {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_term(lexer, arena);

        lhs = arena.add(binop(&t, lhs, rhs), lexer.loc)
    }

    lhs
}

fn parse_term(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    let mut lhs = parse_exp(lexer, arena);

    while lexer.tok == Token::Mult || lexer.tok == Token::Div {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_exp(lexer, arena);

        lhs = arena.add(binop(&t, lhs, rhs), lexer.loc)
    }

    lhs
}

fn parse_exp(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    let mut lhs = parse_factor(lexer, arena);

    while lexer.tok == Token::Power {
        lexer.next();

        let rhs = parse_factor(lexer, arena);

        lhs = arena.add(binop(&Token::Power, lhs, rhs), lexer.loc)
    }

    lhs
}

fn parse_factor(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    match &lexer.tok {
        Token::Minus => {
            lexer.next();
            let e = parse_factor(lexer, arena);
            arena.add(Expr::Unop(e), lexer.loc)
        }
        Token::Plus => {
            lexer.next();
            parse_factor(lexer, arena)
        }
        Token::Not => {
            lexer.next();
            let e = parse_factor(lexer, arena);
            arena.add(Expr::Unop(e), lexer.loc)
        }
        _ => parse_postfix(lexer, arena),
    }
}

fn parse_postfix(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    let mut e = parse_atom(lexer, arena);

    loop {
        match lexer.tok {
            Token::Lparen => {
                lexer.next();
                let args = if lexer.tok != Token::Rparen {
                    let args = parse_exprlist(lexer, arena);
                    expect(lexer, Token::Rparen, &mut arena.errors);
                    args
                } else {
                    lexer.next();
                    vec![]
                };
                e = arena.add(Expr::Call(e, args), lexer.loc);
            }
            Token::Colon => {
                lexer.next();
                let t = parse_type(lexer, &mut arena.errors);
                e = arena.add(Expr::AsTy(e, t), lexer.loc);
            }
            Token::Lbracket => {
                lexer.next();
                let idx = parse_expr(lexer, arena);
                expect(lexer, Token::Rbracket, &mut arena.errors);
                e = arena.add(Expr::ArrayIndex(e, idx), lexer.loc);
            }
            Token::Dot => {
                lexer.next();
                let field = expect_id(lexer, &mut arena.errors);
                e = arena.add(Expr::Field(e, field), lexer.loc);
            }
            Token::As => {
                lexer.next();
                let ty = parse_type(lexer, &mut arena.errors);
                e = arena.add(Expr::AsTy(e, ty), lexer.loc);
            }
            _ => {
                return e;
            }
        }
    }
}

fn parse_atom(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    match &lexer.tok {
        Token::Id(id) => {
            let e = Expr::Id(Name::new(id.clone()));
            lexer.next();
            arena.add(e, lexer.loc)
        }
        Token::True => {
            lexer.next();
            arena.add(Expr::True, lexer.loc)
        }
        Token::False => {
            lexer.next();
            arena.add(Expr::False, lexer.loc)
        }
        Token::Dot => {
            lexer.next();
            let name = expect_id(lexer, &mut arena.errors);
            arena.add(Expr::Enum(name), lexer.loc)
        }
        Token::Integer(x) => {
            let e = Expr::Int(*x);
            lexer.next();
            arena.add(e, lexer.loc)
        }
        Token::Real(x) => {
            let e = Expr::Real(x.clone());
            lexer.next();
            arena.add(e, lexer.loc)
        }
        Token::String(s) => {
            let e = Expr::String(s.clone());
            lexer.next();
            arena.add(e, lexer.loc)
        }
        Token::Char(c) => {
            let e = Expr::Char(*c);
            lexer.next();
            arena.add(e, lexer.loc)
        }
        Token::Lparen => {
            lexer.next();

            let mut tup = vec![];
            loop {
                let rr = parse_lambda(lexer, arena);
                tup.push(rr);
                if lexer.tok == Token::Rparen {
                    break;
                }
                expect(lexer, Token::Comma, &mut arena.errors);
            }

            lexer.next();

            if tup.len() > 1 {
                let e = Expr::Tuple(tup);
                arena.add(e, lexer.loc)
            } else {
                tup[0]
            }
        }
        Token::Lbrace => parse_block(lexer, arena),
        Token::Lbracket => parse_array_literal(lexer, arena),
        Token::At => {
            // macro invocations
            lexer.next();

            let name = expect_id(lexer, &mut arena.errors);

            expect(lexer, Token::Lparen, &mut arena.errors);
            let params = parse_exprlist(lexer, arena);
            expect(lexer, Token::Rparen, &mut arena.errors);

            let e = Expr::Macro(name, params);
            arena.add(e, lexer.loc)
        }
        _ => {
            lexer.next();
            arena.errors.push(ParseError {
                location: lexer.loc,
                message: String::from("Expected expression"),
            });
            arena.add(Expr::Tuple(vec![]), lexer.loc)
        }
    }
}

fn parse_exprlist(lexer: &mut Lexer, arena: &mut ExprArena) -> Vec<ExprID> {
    let mut r = vec![];

    loop {
        r.push(parse_lambda(lexer, arena));

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();
    }

    r
}

fn parse_array_literal(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    expect(lexer, Token::Lbracket, &mut arena.errors);
    let mut r = vec![];

    while lexer.tok != Token::Rbracket {
        r.push(parse_lambda(lexer, arena));

        if lexer.tok == Token::Comma {
            lexer.next();
        } else if lexer.tok == Token::Semi {
            lexer.next();
            let count = parse_expr(lexer, arena);
            let e = Expr::Array(r[0], count);
            expect(lexer, Token::Rbracket, &mut arena.errors);
            return arena.add(e, lexer.loc);
        } else if lexer.tok == Token::Rbracket {
            break;
        } else {
            arena.errors.push(ParseError {
                location: lexer.loc,
                message: String::from("Expected comma or semicolon in array literal"),
            });
            lexer.next();
        }
    }

    lexer.next();

    arena.add(Expr::ArrayLiteral(r), lexer.loc)
}

fn parse_stmt(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    match lexer.tok.clone() {
        Token::Var | Token::Let => {
            lexer.next();
            let name = expect_id(lexer, &mut arena.errors);

            if lexer.tok == Token::Assign {
                lexer.next();
                let e = parse_lambda(lexer, arena);
                arena.add(Expr::Var(name, Some(e), None), lexer.loc)
            } else if lexer.tok == Token::Colon {
                lexer.next();
                let t = parse_type(lexer, &mut arena.errors);
                arena.add(Expr::Var(name, None, Some(t)), lexer.loc)
            } else {
                arena.errors.push(ParseError {
                    location: lexer.loc,
                    message: String::from("expected assignment or type"),
                });
                arena.add(Expr::Var(name, None, None), lexer.loc)
            }
        }
        Token::Arena => {
            lexer.next();
            let e = parse_block(lexer, arena);
            arena.add(Expr::Arena(e), lexer.loc)
        }
        Token::While => {
            lexer.next();
            let cond = parse_expr(lexer, arena);
            let body = parse_block(lexer, arena);
            arena.add(Expr::While(cond, body), lexer.loc)
        }
        Token::Return => {
            lexer.next();
            let e = parse_expr(lexer, arena);
            arena.add(Expr::Return(e), lexer.loc)
        }
        _ => parse_expr(lexer, arena),
    }
}

fn parse_block(lexer: &mut Lexer, arena: &mut ExprArena) -> ExprID {
    let mut r = vec![];
    expect(lexer, Token::Lbrace, &mut arena.errors);

    loop {
        skip_newlines(lexer);

        if lexer.tok == Token::Rbrace {
            break;
        }

        r.push(parse_stmt(lexer, arena));

        if lexer.tok != Token::Endl {
            break;
        }

        lexer.next();
    }

    lexer.next();

    arena.add(Expr::Block(r), lexer.loc)
}

fn parse_fieldlist(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> Vec<Field> {
    let mut r = vec![];

    skip_newlines(lexer);

    while lexer.tok != Token::Rbrace {
        skip_newlines(lexer);

        let name = expect_id(lexer, errors);

        expect(lexer, Token::Colon, errors);

        let ty = parse_type(lexer, errors);
        r.push(Field { name, ty, loc: lexer.loc });

        skip_newlines(lexer);

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();
    }

    r
}

fn parse_caselist(lexer: &mut Lexer) -> Vec<Name> {
    let mut r = vec![];

    loop {
        if let Token::Id(name) = &lexer.tok {
            let name = Name::new(name.clone());
            lexer.next();
            r.push(name);
        }

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();

        skip_newlines(lexer);
    }

    r
}

pub fn parse_typevar_list(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> Vec<Name> {
    let mut r = vec![];
    expect(lexer, Token::Less, errors);

    loop {
        let name = expect_id(lexer, errors);
        r.push(name);

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();
    }

    expect(lexer, Token::Greater, errors);

    r
}

fn token_in(tok: &Token, set: &[Token]) -> bool {
    set.iter().position(|x| x == tok) != None
}

fn parse_func_decl(name: Name, lexer: &mut Lexer, arena: &mut ExprArena) -> FuncDecl {
    let mut params = vec![];
    let mut typevars = vec![];
    let mut constraints = vec![];

    if lexer.tok == Token::Less {
        typevars = parse_typevar_list(lexer, &mut arena.errors);
    }

    if lexer.tok == Token::Lparen {
        expect(lexer, Token::Lparen, &mut arena.errors);
        params = parse_paramlist(lexer, &mut arena.errors);
        expect(lexer, Token::Rparen, &mut arena.errors);
    }

    skip_newlines(lexer);

    let mut ret = mk_type(Type::Void);
    if lexer.tok == Token::Arrow {
        lexer.next();
        ret = parse_type(lexer, &mut arena.errors);
    }

    skip_newlines(lexer);

    if lexer.tok == Token::Where {
        lexer.next();

        while !token_in(&lexer.tok, &[Token::Lbrace, Token::Rbrace, Token::Endl]) {
            let interface_name = expect_id(lexer, &mut arena.errors);
            let typevars = parse_typevar_list(lexer, &mut arena.errors);
            constraints.push(InterfaceConstraint {
                interface_name,
                typevars,
            })
        }
    }

    let mut body = None;
    if lexer.tok == Token::Lbrace {
        body = Some(parse_block(lexer, arena));
    }

    skip_newlines(lexer);

    FuncDecl {
        name,
        typevars,
        params,
        body,
        ret,
        constraints,
    }
}

fn parse_interface(lexer: &mut Lexer, arena: &mut ExprArena) -> Decl {
    lexer.next();

    let name = expect_id(lexer, &mut arena.errors);

    expect(lexer, Token::Lbrace, &mut arena.errors);

    let mut funcs = vec![];

    while lexer.tok != Token::Rbrace {
        let name = expect_id(lexer, &mut arena.errors);
        funcs.push(parse_func_decl(name, lexer, arena))
    }

    lexer.next();

    Decl::Interface { name, funcs }
}

fn parse_decl(lexer: &mut Lexer, arena: &mut ExprArena) -> Option<Decl> {
    Some(match lexer.tok.clone() {
        Token::Id(name) => {
            // Function declaration.
            lexer.next();
            Decl::Func(parse_func_decl(Name::new(name), lexer, arena))
        }
        Token::Macro => {
            // Macro declaration.
            lexer.next();

            let name = expect_id(lexer, &mut arena.errors);

            Decl::Macro(parse_func_decl(name, lexer, arena))
        }
        Token::Struct => {
            // Struct delcaration.
            lexer.next();

            let name = expect_id(lexer, &mut arena.errors);

            let mut typevars = vec![];
            if lexer.tok == Token::Less {
                typevars = parse_typevar_list(lexer, &mut arena.errors);
            }

            expect(lexer, Token::Lbrace, &mut arena.errors);
            let fields = parse_fieldlist(lexer, &mut arena.errors);
            expect(lexer, Token::Rbrace, &mut arena.errors);

            Decl::Struct {
                name,
                typevars,
                fields,
            }
        }
        Token::Enum => {
            lexer.next();

            let name = expect_id(lexer, &mut arena.errors);

            expect(lexer, Token::Lbrace, &mut arena.errors);
            let cases = parse_caselist(lexer);
            expect(lexer, Token::Rbrace, &mut arena.errors);

            Decl::Enum { name, cases }
        }
        Token::Var => {
            lexer.next();

            let name = expect_id(lexer, &mut arena.errors);

            expect(lexer, Token::Colon, &mut arena.errors);

            let ty = parse_type(lexer, &mut arena.errors);

            Decl::Global { name, ty }
        }
        Token::Interface => parse_interface(lexer, arena),
        _ => {

            arena.errors.push(ParseError {
                location: lexer.loc,
                message: format!("Expected declaration, got {:?}", lexer.tok).into(),
            });

            lexer.next();
            return None;
        }
    })
}

fn skip_newlines(lexer: &mut Lexer) {
    while lexer.tok == Token::Endl {
        lexer.next();
    }
}

pub fn parse_program(lexer: &mut Lexer, arena: &mut ExprArena) -> Vec<Decl> {
    let mut decls = vec![];

    skip_newlines(lexer);

    while lexer.tok != Token::End {
        if let Some(decl) = parse_decl(lexer, arena) {
            decls.push(decl);
        }
        skip_newlines(lexer);
    }

    decls
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::hash::Hasher;

    fn type_parser(string: &str) -> TypeID {
        let mut lexer = Lexer::new(string, "parser tests");
        lexer.next();
        let mut errors = vec![];
        let t = parse_type(&mut lexer, &mut errors);
        assert!(errors.is_empty());
        t
    }

    fn test_type(string: &str, ty: TypeID) {
        assert_eq!(type_parser(string), ty);
    }

    #[test]
    fn test_parse_type() {
        let int8 = mk_type(Type::Int8);
        let int32 = mk_type(Type::Int32);
        let tup = mk_type(Type::Tuple(vec![int8]));
        let func = mk_type(Type::Func(tup, int8));
        test_type("void", mk_type(Type::Void));
        test_type("i8", int8);
        test_type("i32", int32);
        test_type("⟨T⟩", typevar("T"));
        test_type("typevar T", typevar("T"));
        test_type("[i32]", mk_type(Type::Array(int32, 0)));
        test_type("[i32; 4]", mk_type(Type::Array(int32, 4)));
        test_type("i8 -> i8", func);
        test_type("(i32)", int32);
        test_type("(i8 -> i8)", func);
        test_type("[i8 -> i8]", mk_type(Type::Array(func, 0)));
    }

    fn parse_fn<T: std::fmt::Debug>(
        string: &str,
        arena: &mut ExprArena,
        f: fn(&mut Lexer, arena: &mut ExprArena) -> T,
    ) -> Result<T, ParseError> {
        println!("parsing: {}", string);
        let mut lexer = Lexer::new(&String::from(string), "parser tests");
        lexer.next();

        let r = f(&mut lexer, arena);
        println!("{} ==> {:?}, arena: {:?}", string, r, arena);
        expect(&mut lexer, Token::End, &mut arena.errors);
        assert!(arena.errors.is_empty());
        Ok(r)
    }

    fn test<T: std::fmt::Debug>(string: &str, f: fn(&mut Lexer, &mut ExprArena) -> T) {
        let mut arena = ExprArena::new();
        assert!(parse_fn(string, &mut arena, f).is_ok());
    }

    fn test_strings<T: std::fmt::Debug>(f: fn(&mut Lexer, &mut ExprArena) -> T, strings: &[&str]) {
        for string in strings {
            let mut arena = ExprArena::new();
            assert!(parse_fn(string, &mut arena, f).is_ok());
        }
    }

    fn test_strings_hash<T: std::fmt::Debug + std::hash::Hash>(
        f: fn(&mut Lexer, &mut ExprArena) -> Result<T, ParseError>,
        pairs: &[(&str, u64)],
    ) {
        for (string, h) in pairs {
            let mut arena = ExprArena::new();
            if let Ok(result) = parse_fn(string, &mut arena, f) {
                let mut hasher = rustc_hash::FxHasher::default();
                result.hash(&mut hasher);
                arena.hash(&mut hasher);
                assert_eq!(hasher.finish(), *h);
            } else {
                panic!();
            }
        }
    }

    #[test]
    fn test_parse() {
        test("x*y", parse_term);
        test("x+y", parse_sum);
        test("x-y", parse_sum);
        test("f()", parse_expr);
        test("f(x)", parse_expr);
        test("f(x, y)", parse_expr);
        test("f(x) + g(x)", parse_expr);
        test("| | x", parse_lambda);
        test("|x: i8| x", parse_lambda);
        test("|x| x", parse_lambda);
    }

    #[test]
    fn test_parse_atom() {
        test_strings(
            parse_atom,
            &[
                "x",
                "(x)",
                "42",
                "3.14159",
                ".something",
                "(1,2,3)",
                "'a'",
                "'\\n'",
                "[1,2,3]",
                "@my_macro(a,b)",
                "[0; x]",
            ],
        );
    }

    #[test]
    fn test_parse_stmt() {
        test_strings(
            parse_stmt,
            &[
                "x = y",
                "f(x)",
                "var x = y",
                "var x:i32",
                "let x = y",
                "let x = | | x",
                "let x = if x { a } else { b }",
                "let x = if x { a+b } else { b }",
                "if x { }",
                "if x { } else { }",
                "x",
                "{ x }",
                "return x",
                "assert(outer)",
                "a + 5",
                "f(a + 5)",
                "a == 5",
                "f(a == 5)",
                "assert(outer == 42)",
                "x[0]",
                "x.y",
                "a.array[0] = 'x'",
                "var t = true",
                "!x",
                "@my_macro(a,b)",
                "var b = [0; a.len]",
                "a = -b",
                "-a.y",
                "x as i8",
            ],
        );
    }

    #[test]
    fn test_parse_block() {
        test_strings(
            parse_block,
            &[
                "{}",
                "{ }",
                "{ \n }",
                "{ x }",
                "{ x\n x }",
                "{ x \n x }",
                "{ x \n\n x }",
                "{ x = y }",
                "{ f(x) }",
                "{ x = y\n z = w }",
                "{ f(x)\n g(y) }",
                "{ var x = y\n var z = w }",
            ],
        );
    }

    #[test]
    fn test_parse_decl() {
        test_strings(
            parse_decl,
            &[
                "f(){}",
                "f(x: i8) { g(x) }",
                "f(x: i8) -> i8 { g(x) }",
                "f(x: i8) -> (i8 -> i8) { }",
                "f(x: i8, y: i8) { g(x) }",
                "f(x: i8,\n y: i8) { g(x) }",
                "f(x: i8 -> i8) { }",
                "f<T>() { }",
                "f<T>() where MyInterface<T> { }",
                "f<T>() where MyInterface<T> AnotherInterface<T> { }",
                "f<T0, T1>() where MyInterface<T0, T1> { }",
                "f<T0, T1>(a: T0, b: T1) where MyInterface<T0, T1> { }",
                "test {}",
                "f()",
                "struct x { }",
                "struct x { \n }",
                "struct x { x: i8 }",
                "struct x { x: i8\n }",
                "struct x { x: i8, y: i8 }",
                "struct x<T> { }",
                "struct x { y: typevar T }",
                "enum x { }",
                "enum x { a, b, c }",
                "enum x { a,\nb }",
                "macro m() { }",
                "interface x { }",
                "interface x { f(x: i8) }",
            ],
        );
    }

    #[test]
    fn test_parse_program() {
        test_strings(
            parse_program,
            &["", "\n", "\nf()", "f(){} g(){}", "f(){}\n g(){}", "f()\n{}"],
        );
    }
}
