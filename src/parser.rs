use crate::defs::*;
use crate::lexer::*;
use crate::types::*;
use internment::Intern;
use std::hash::Hash;

#[derive(Clone, Debug)]
pub struct ParseError {
    pub location: Loc,
    pub message: String,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct ExprArena {
    pub exprs: Vec<Expr>,
    pub locs: Vec<Loc>,
}

impl ExprArena {
    pub fn new() -> Self {
        Self {
            exprs: vec![],
            locs: vec![],
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

fn expect(lexer: &Lexer, tok: Token) -> Result<(), ParseError> {
    if lexer.tok == tok {
        Ok(())
    } else {
        let message = format!("expected {:?}, got {:?}", tok, lexer.tok);
        println!("{:?}", message);
        Err(ParseError {
            location: lexer.loc,
            message,
        })
    }
}

fn expect_id(lexer: &Lexer) -> Result<Name, ParseError> {
    if let Token::Id(string) = &lexer.tok {
        Ok(Name::new(string.clone()))
    } else {
        let message = format!("expected identifier, got {:?}", lexer.tok);
        println!("{:?}", message);
        Err(ParseError {
            location: lexer.loc,
            message,
        })
    }
}

fn parse_typelist(lexer: &mut Lexer) -> Result<Vec<TypeID>, ParseError> {
    let mut r = vec![];
    expect(lexer, Token::Less)?;

    lexer.next();

    loop {
        r.push(parse_basic_type(lexer)?);

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();
    }

    expect(lexer, Token::Greater)?;
    lexer.next();

    Ok(r)
}

fn parse_basic_type(lexer: &mut Lexer) -> Result<TypeID, ParseError> {
    let t = Ok(mk_type(match &lexer.tok {
        Token::Void => Type::Void,
        Token::Bool => Type::Bool,
        Token::Int8 => Type::Int8,
        Token::Int32 => Type::Int32,
        Token::Lmath => {
            lexer.next();
            let name = expect_id(lexer)?;
            lexer.next();
            expect(lexer, Token::Rmath)?;
            return Ok(typevar(&name));
        }
        Token::Typevar => {
            lexer.next();
            let name = expect_id(lexer)?;
            lexer.next();
            return Ok(typevar(&name));
        }
        Token::Lbracket => {
            lexer.next();
            let r = parse_type(lexer)?;
            if lexer.tok == Token::Semi {
                lexer.next();
                if let Token::Real(n) = lexer.tok.clone() {
                    lexer.next();
                    expect(lexer, Token::Rbracket)?;
                    Type::Array(r, n.parse::<i64>().map_err( |_| ParseError{location: lexer.loc, message: "Expected integer array size".into()} )?)
                } else {
                    return Err(ParseError {
                        location: lexer.loc,
                        message: String::from("Expected integer array size"),
                    });
                }
            } else {
                expect(lexer, Token::Rbracket)?;
                Type::Array(r, 0)
            }
        }
        Token::Id(name) => {
            let name = Intern::new(name.clone());
            lexer.next();
            let mut args = vec![];
            if lexer.tok == Token::Less {
                args = parse_typelist(lexer)?;
            }
            return Ok(mk_type(Type::Name(name, args)));
        }
        Token::Lparen => {
            lexer.next();
            let t = parse_type(lexer)?;
            expect(lexer, Token::Rparen)?;
            lexer.next();
            return Ok(t);
        }
        _ => {
            return Err(ParseError {
                location: lexer.loc,
                message: String::from("Expected type"),
            })
        }
    }));
    lexer.next();
    t
}

fn parse_type(lexer: &mut Lexer) -> Result<TypeID, ParseError> {
    let mut lhs = parse_basic_type(lexer)?;

    while lexer.tok == Token::Arrow {
        lexer.next();
        let rhs = parse_basic_type(lexer)?;

        lhs = mk_type(Type::Func(lhs, rhs));
    }

    Ok(lhs)
}

fn parse_paramlist(lexer: &mut Lexer) -> Result<Vec<Param>, ParseError> {
    let mut r = vec![];

    loop {
        if let Token::Id(name) = &lexer.tok {
            let name = name.clone();
            lexer.next();

            let ty = if lexer.tok == Token::Colon {
                lexer.next();
                parse_type(lexer)?
            } else {
                mk_type(Type::Var(Intern::new(String::from("__anon__")), 0))
            };

            r.push(Param { name, ty })
        }

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();

        skip_newlines(lexer);
    }

    Ok(r)
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

fn parse_lambda(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    if lexer.tok == Token::Pipe {
        lexer.next();
        let params = parse_paramlist(lexer)?;
        expect(lexer, Token::Pipe)?;
        lexer.next();

        let body = parse_lambda(lexer, arena)?;

        Ok(arena.add(Expr::Lambda { params, body }, lexer.loc))
    } else {
        parse_expr(lexer, arena)
    }
}

fn parse_if(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    let cond = parse_expr(lexer, arena)?;
    let then = parse_block(lexer, arena)?;

    let els = if lexer.tok == Token::Else {
        lexer.next();
        if lexer.tok == Token::If {
            lexer.next();
            Some(parse_if(lexer, arena)?)
        } else {
            Some(parse_block(lexer, arena)?)
        }
    } else {
        None
    };

    Ok(arena.add(Expr::If(cond, then, els), lexer.loc))
}

fn parse_expr(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    if lexer.tok == Token::If {
        lexer.next();
        parse_if(lexer, arena)
    } else {
        parse_assign(lexer, arena)
    }
}

fn parse_assign(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    let mut lhs = parse_eq(lexer, arena)?;

    while lexer.tok == Token::Assign {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_eq(lexer, arena)?;

        lhs = arena.add(binop(&t, lhs, rhs), lexer.loc)
    }

    Ok(lhs)
}

fn parse_eq(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    let mut lhs = parse_rel(lexer, arena)?;

    while lexer.tok == Token::Equal || lexer.tok == Token::NotEqual {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_rel(lexer, arena)?;

        lhs = arena.add(binop(&t, lhs, rhs), lexer.loc)
    }

    Ok(lhs)
}

fn parse_rel(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    let mut lhs = parse_sum(lexer, arena)?;

    while lexer.tok == Token::Leq
        || lexer.tok == Token::Geq
        || lexer.tok == Token::Less
        || lexer.tok == Token::Greater
    {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_sum(lexer, arena)?;

        lhs = arena.add(binop(&t, lhs, rhs), lexer.loc)
    }

    Ok(lhs)
}

fn parse_sum(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    let mut lhs = parse_term(lexer, arena)?;

    while lexer.tok == Token::Plus || lexer.tok == Token::Minus {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_term(lexer, arena)?;

        lhs = arena.add(binop(&t, lhs, rhs), lexer.loc)
    }

    Ok(lhs)
}

fn parse_term(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    let mut lhs = parse_exp(lexer, arena)?;

    while lexer.tok == Token::Mult || lexer.tok == Token::Div {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_exp(lexer, arena)?;

        lhs = arena.add(binop(&t, lhs, rhs), lexer.loc)
    }

    Ok(lhs)
}

fn parse_exp(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    let mut lhs = parse_factor(lexer, arena)?;

    while lexer.tok == Token::Power {
        lexer.next();

        let rhs = parse_factor(lexer, arena)?;

        lhs = arena.add(binop(&Token::Power, lhs, rhs), lexer.loc)
    }

    Ok(lhs)
}

fn parse_factor(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    match &lexer.tok {
        Token::Minus => {
            lexer.next();
            let e = parse_factor(lexer, arena)?;
            Ok(arena.add(Expr::Unop(e), lexer.loc))
        }
        Token::Plus => {
            lexer.next();
            parse_factor(lexer, arena)
        }
        Token::Not => {
            lexer.next();
            let e = parse_factor(lexer, arena)?;
            Ok(arena.add(Expr::Unop(e), lexer.loc))
        }
        _ => parse_postfix(lexer, arena),
    }
}

fn parse_postfix(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    let mut e = parse_atom(lexer, arena)?;

    loop {
        match lexer.tok {
            Token::Lparen => {
                lexer.next();
                let args = if lexer.tok != Token::Rparen {
                    let args = parse_exprlist(lexer, arena)?;
                    expect(lexer, Token::Rparen)?;
                    lexer.next();
                    args
                } else {
                    lexer.next();
                    vec![]
                };
                e = arena.add(Expr::Call(e, args), lexer.loc);
            }
            Token::Colon => {
                lexer.next();
                let t = parse_type(lexer)?;
                e = arena.add(Expr::AsTy(e, t), lexer.loc);
            }
            Token::Lbracket => {
                lexer.next();
                let idx = parse_expr(lexer, arena)?;
                expect(lexer, Token::Rbracket)?;
                lexer.next();
                e = arena.add(Expr::ArrayIndex(e, idx), lexer.loc);
            }
            Token::Dot => {
                lexer.next();
                let field = expect_id(lexer)?;
                lexer.next();
                e = arena.add(Expr::Field(e, field), lexer.loc);
            }
            _ => {
                return Ok(e);
            }
        }
    }
}

fn parse_atom(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    Ok(match &lexer.tok {
        Token::Id(id) => {
            let e = Expr::Id(Intern::new(id.clone()));
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
            let name = expect_id(lexer)?;
            lexer.next();
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
                let rr = parse_lambda(lexer, arena)?;
                tup.push(rr);
                if lexer.tok == Token::Rparen {
                    break;
                }
                expect(lexer, Token::Comma)?;
                lexer.next();
            }

            lexer.next();

            if tup.len() > 1 {
                let e = Expr::Tuple(tup);
                arena.add(e, lexer.loc)
            } else {
                tup[0]
            }
        }
        Token::Lbrace => parse_block(lexer, arena)?,
        Token::Lbracket => parse_array_literal(lexer, arena)?,
        Token::At => {
            // macro invocations
            lexer.next();

            let name = expect_id(lexer)?;

            lexer.next();
            expect(lexer, Token::Lparen)?;
            lexer.next();
            let params = parse_exprlist(lexer, arena)?;
            expect(lexer, Token::Rparen)?;
            lexer.next();

            let e = Expr::Macro(name, params);
            arena.add(e, lexer.loc)
        }
        _ => {
            return Err(ParseError {
                location: lexer.loc,
                message: String::from("Expected expression"),
            })
        }
    })
}

fn parse_exprlist(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<Vec<ExprID>, ParseError> {
    let mut r = vec![];

    loop {
        r.push(parse_lambda(lexer, arena)?);

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();
    }

    Ok(r)
}

fn parse_array_literal(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    expect(lexer, Token::Lbracket)?;
    lexer.next();
    let mut r = vec![];

    while lexer.tok != Token::Rbracket {
        r.push(parse_lambda(lexer, arena)?);

        if lexer.tok == Token::Comma {
            lexer.next();
        } else if lexer.tok == Token::Semi {
            lexer.next();
            let count = parse_expr(lexer, arena)?;
            let e = Expr::Array(r[0], count);
            expect(lexer, Token::Rbracket)?;
            lexer.next();
            return Ok(arena.add(e, lexer.loc));
        } else if lexer.tok == Token::Rbracket {
            break;
        } else {
            return Err(ParseError {
                location: lexer.loc,
                message: String::from("Expected comma or semicolon in array literal"),
            });
        }
    }

    lexer.next();

    Ok(arena.add(Expr::ArrayLiteral(r), lexer.loc))
}

fn parse_stmt(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    match lexer.tok.clone() {
        Token::Var | Token::Let => {
            lexer.next();
            let name = expect_id(lexer)?;
            lexer.next();

            if lexer.tok == Token::Assign {
                lexer.next();
                let e = parse_lambda(lexer, arena)?;
                Ok(arena.add(Expr::Var(name, Some(e), None), lexer.loc))
            } else if lexer.tok == Token::Colon {
                lexer.next();
                let t = parse_type(lexer)?;
                Ok(arena.add(Expr::Var(name, None, Some(t)), lexer.loc))
            } else {
                Err(ParseError {
                    location: lexer.loc,
                    message: String::from("expected assignment or type"),
                })
            }
        }
        Token::Arena => {
            lexer.next();
            let e = parse_block(lexer, arena)?;
            Ok(arena.add(Expr::Arena(e), lexer.loc))
        }
        Token::While => {
            lexer.next();
            let cond = parse_expr(lexer, arena)?;
            let body = parse_block(lexer, arena)?;
            Ok(arena.add(Expr::While(cond, body), lexer.loc))
        }
        Token::Return => {
            lexer.next();
            let e = parse_expr(lexer, arena)?;
            Ok(arena.add(Expr::Return(e), lexer.loc))
        }
        _ => parse_expr(lexer, arena),
    }
}

fn parse_block(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    let mut r = vec![];
    expect(lexer, Token::Lbrace)?;

    lexer.next();

    loop {
        skip_newlines(lexer);

        if lexer.tok == Token::Rbrace {
            break;
        }

        r.push(parse_stmt(lexer, arena)?);

        if lexer.tok != Token::Endl {
            break;
        }

        lexer.next();
    }

    lexer.next();

    Ok(arena.add(Expr::Block(r), lexer.loc))
}

fn parse_fieldlist(lexer: &mut Lexer) -> Result<Vec<Field>, ParseError> {
    let mut r = vec![];

    println!("parse fieldlist: {:?}", lexer.tok);
    skip_newlines(lexer);

    while lexer.tok != Token::Rbrace {
        skip_newlines(lexer);

        let name = expect_id(lexer)?;
        lexer.next();

        expect(lexer, Token::Colon)?;

        lexer.next();
        let ty = parse_type(lexer)?;
        r.push(Field { name, ty });

        skip_newlines(lexer);

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();
    }

    Ok(r)
}

fn parse_caselist(lexer: &mut Lexer) -> Result<Vec<Name>, ParseError> {
    let mut r = vec![];

    loop {
        if let Token::Id(name) = &lexer.tok {
            let name = Intern::new(name.clone());
            lexer.next();
            r.push(name);
        }

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();

        skip_newlines(lexer);
    }

    Ok(r)
}

pub fn parse_typevar_list(lexer: &mut Lexer) -> Result<Vec<Name>, ParseError> {
    let mut r = vec![];
    expect(lexer, Token::Less)?;

    lexer.next();

    loop {
        let name = expect_id(lexer)?;
        r.push(name);
        lexer.next();

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();
    }

    expect(lexer, Token::Greater)?;
    lexer.next();

    Ok(r)
}

fn parse_func_decl(
    name: Name,
    lexer: &mut Lexer,
    arena: &mut ExprArena,
) -> Result<FuncDecl, ParseError> {
    let mut params = vec![];
    let mut typevars = vec![];

    if lexer.tok == Token::Less {
        typevars = parse_typevar_list(lexer)?;
    }

    if lexer.tok == Token::Lparen {
        expect(lexer, Token::Lparen)?;
        lexer.next();
        params = parse_paramlist(lexer)?;
        expect(lexer, Token::Rparen)?;
        lexer.next();
    }

    skip_newlines(lexer);

    let mut ret = mk_type(Type::Void);
    if lexer.tok == Token::Arrow {
        lexer.next();
        ret = parse_type(lexer)?;
    }

    skip_newlines(lexer);

    let mut body = None;
    if lexer.tok == Token::Lbrace {
        body = Some(parse_block(lexer, arena)?);
    }

    skip_newlines(lexer);

    Ok(FuncDecl {
        name,
        typevars,
        params,
        body,
        ret,
    })
}

fn parse_interface(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<Decl, ParseError> {
    lexer.next();

    let name = expect_id(lexer)?;

    lexer.next();
    expect(lexer, Token::Lbrace)?;
    lexer.next();

    let mut funcs = vec![];

    while lexer.tok != Token::Rbrace {
        let name = expect_id(lexer)?;
        funcs.push(parse_func_decl(name, lexer, arena)?)
    }

    Ok(Decl::Interface { name, funcs })
}

fn parse_decl(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<Decl, ParseError> {
    match lexer.tok.clone() {
        Token::Id(name) => {
            // Function declaration.
            lexer.next();
            Ok(Decl::Func(parse_func_decl(name.into(), lexer, arena)?))
        }
        Token::Macro => {
            // Macro declaration.
            lexer.next();

            let name = expect_id(lexer)?;

            lexer.next();
            Ok(Decl::Func(parse_func_decl(name, lexer, arena)?))
        }
        Token::Struct => {
            // Struct delcaration.
            lexer.next();

            let name = expect_id(lexer)?;

            lexer.next();

            let mut typevars = vec![];
            if lexer.tok == Token::Less {
                typevars = parse_typevar_list(lexer)?;
            }

            expect(lexer, Token::Lbrace)?;
            lexer.next();

            let fields = parse_fieldlist(lexer)?;
            expect(lexer, Token::Rbrace)?;
            lexer.next();

            Ok(Decl::Struct {
                name,
                typevars,
                fields,
            })
        }
        Token::Enum => {
            lexer.next();

            let name = expect_id(lexer)?;

            lexer.next();

            expect(lexer, Token::Lbrace)?;
            lexer.next();

            let cases = parse_caselist(lexer)?;
            expect(lexer, Token::Rbrace)?;
            lexer.next();

            Ok(Decl::Enum { name, cases })
        }
        Token::Var => {
            lexer.next();

            let name = expect_id(lexer)?;

            lexer.next();

            expect(lexer, Token::Colon)?;
            lexer.next();

            let ty = parse_type(lexer)?;

            Ok(Decl::Global { name, ty })
        }
        Token::Interface => parse_interface(lexer, arena),
        _ => Err(ParseError {
            location: lexer.loc,
            message: String::from("Expected declaration"),
        }),
    }
}

fn skip_newlines(lexer: &mut Lexer) {
    while lexer.tok == Token::Endl {
        lexer.next();
    }
}

pub fn parse_program(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<Vec<Decl>, ParseError> {
    let mut decls = vec![];

    skip_newlines(lexer);

    while lexer.tok != Token::End {
        decls.push(parse_decl(lexer, arena)?);
        skip_newlines(lexer);
    }

    Ok(decls)
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hasher;

    fn type_parser(string: &str) -> TypeID {
        let mut lexer = Lexer::new(string, "parser tests");
        lexer.next();
        parse_type(&mut lexer).unwrap()
    }

    fn test_type(string: &str, ty: TypeID) {
        assert_eq!(type_parser(string), ty);
    }

    #[test]
    fn test_parse_type() {
        let int8 = mk_type(Type::Int8);
        let int32 = mk_type(Type::Int32);
        let func = mk_type(Type::Func(int8, int8));
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
        f: fn(&mut Lexer, arena: &mut ExprArena) -> Result<T, ParseError>,
    ) -> Result<T, ParseError> {
        println!("parsing: {}", string);
        let mut lexer = Lexer::new(&String::from(string), "parser tests");
        lexer.next();
        
        let r = f(&mut lexer, arena)?;
        println!("{} ==> {:?}, arena: {:?}", string, r, arena);
        expect(&lexer, Token::End)?;
        Ok(r)
    }

    fn test<T: std::fmt::Debug>(
        string: &str,
        f: fn(&mut Lexer, &mut ExprArena) -> Result<T, ParseError>,
    ) {
        let mut arena = ExprArena::new();
        assert!(parse_fn(string, &mut arena, f).is_ok());
    }

    fn test_strings<T: std::fmt::Debug>(
        f: fn(&mut Lexer, &mut ExprArena) -> Result<T, ParseError>,
        strings: &[&str],
    ) {
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
                let mut hasher = DefaultHasher::new();
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
        test("|| x", parse_lambda);
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
                "let x = || x",
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
