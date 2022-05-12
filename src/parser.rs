use crate::defs::*;
use crate::lexer::*;
use crate::types::*;
use internment::Intern;

#[derive(Clone, Debug)]
pub struct ParseError {
    pub location: Loc,
    pub message: String,
}

#[derive(Debug)]
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

fn parse_basic_type(lexer: &mut Lexer) -> Result<TypeID, ParseError> {
    let t = Ok(mk_type(match &lexer.tok {
        Token::Void => Type::Void,
        Token::Bool => Type::Bool,
        Token::Int8 => Type::Int8,
        Token::Int32 => Type::Int32,
        Token::Lmath => {
            lexer.next();
            if let Token::Id(name) = lexer.tok.clone() {
                lexer.next();

                expect(lexer, Token::Rmath)?;

                return Ok(typevar(&name));
            } else {
                return Err(ParseError {
                    location: lexer.loc,
                    message: String::from("Expected identifier"),
                });
            }
        }
        Token::Lbracket => {
            lexer.next();
            let r = parse_type(lexer)?;
            if lexer.tok == Token::Semi {
                lexer.next();
                if let Token::Real(n) = lexer.tok {
                    lexer.next();
                    expect(lexer, Token::Rbracket)?;
                    Type::Array(r, n as i64)
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
        Token::Id(name) => Type::Name(Intern::new(name.clone())),
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
            expect(lexer, Token::Colon)?;
            lexer.next();
            let ty = parse_type(lexer)?;
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

fn parse_expr(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    if lexer.tok == Token::If {
        lexer.next();
        let cond = parse_expr(lexer, arena)?;
        let then = parse_block(lexer, arena)?;

        let els = if lexer.tok == Token::Else {
            lexer.next();
            Some(parse_block(lexer, arena)?)
        } else {
            None
        };

        Ok(arena.add(Expr::If(cond, then, els), lexer.loc))
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
    if lexer.tok == Token::Minus {
        lexer.next();
        let e = parse_atom(lexer, arena)?;
        return Ok(arena.add(Expr::Unop(e), lexer.loc));
    }

    if lexer.tok == Token::Plus {
        lexer.next();
        return parse_atom(lexer, arena);
    }

    parse_postfix(lexer, arena)
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
                if let Token::Id(field) = &lexer.tok {
                    e = arena.add(Expr::Field(e, Name::new(field.clone())), lexer.loc);
                    lexer.next();
                } else {
                    return Err(ParseError {
                        location: lexer.loc,
                        message: String::from("Expected field identifier"),
                    })
                }
            }
            _ => { return Ok(e); }
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
        Token::Dot => {
            lexer.next();
            if let Token::Id(id) = &lexer.tok {
                let e = Expr::Enum(Intern::new(id.clone()));
                let r = arena.add(e, lexer.loc);
                lexer.next();
                r
            } else {
                return Err(ParseError {
                    location: lexer.loc,
                    message: String::from("Expected enum case"),
                });
            }
        }
        Token::Integer(x) => {
            let e = Expr::Int(*x);
            lexer.next();
            arena.add(e, lexer.loc)
        }
        Token::Real(x) => {
            let e = Expr::Real(*x);
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
        Token::Lbracket => {
            lexer.next();
            let l = parse_exprlist(lexer, arena)?;
            expect(lexer, Token::Rbracket)?;
            lexer.next();
            let e = Expr::ArrayLiteral(l);
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

fn parse_stmt(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<ExprID, ParseError> {
    match lexer.tok.clone() {
        Token::Var | Token::Let => {
            lexer.next();
            let name = if let Token::Id(name) = &lexer.tok {
                Intern::new(name.clone())
            } else {
                return Err(ParseError {
                    location: lexer.loc,
                    message: String::from("expected identifier"),
                })
            };

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

    loop {
        skip_newlines(lexer);
        if let Token::Id(name) = &lexer.tok {
            let name = Intern::new(name.clone());
            lexer.next();
            expect(lexer, Token::Colon)?;
            lexer.next();
            let ty = parse_type(lexer)?;
            r.push(Field { name, ty })
        }

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

fn parse_decl(lexer: &mut Lexer, arena: &mut ExprArena) -> Result<Decl, ParseError> {
    match lexer.tok.clone() {
        Token::Id(name) => {
            // Function declaration.
            lexer.next();

            let mut params = vec![];

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

            Ok(Decl::Func {
                name: Intern::new(name),
                params,
                body,
                ret,
            })
        }
        Token::Struct => {
            // Struct delcaration.
            lexer.next();

            let name = if let Token::Id(name) = &lexer.tok {
                Intern::new(name.clone())
            } else {
                return Err(ParseError {
                    location: lexer.loc,
                    message: String::from("expected struct name"),
                });
            };

            lexer.next();

            expect(lexer, Token::Lbrace)?;
            lexer.next();

            let fields = parse_fieldlist(lexer)?;
            expect(lexer, Token::Rbrace)?;
            lexer.next();

            Ok(Decl::Struct { name, fields })
        }
        Token::Enum => {
            lexer.next();

            let name = if let Token::Id(name) = &lexer.tok {
                Intern::new(name.clone())
            } else {
                return Err(ParseError {
                    location: lexer.loc,
                    message: String::from("expected enum name"),
                });
            };

            lexer.next();

            expect(lexer, Token::Lbrace)?;
            lexer.next();

            let cases = parse_caselist(lexer)?;
            expect(lexer, Token::Rbrace)?;
            lexer.next();

            Ok(Decl::Enum { name, cases })
        }
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
        test_type("[i32]", mk_type(Type::Array(int32, 0)));
        test_type("[i32; 4]", mk_type(Type::Array(int32, 4)));
        test_type("i8 -> i8", func);
        test_type("(i32)", int32);
        test_type("(i8 -> i8)", func);
        test_type("[i8 -> i8]", mk_type(Type::Array(func, 0)));
    }

    fn parse_fn<T: std::fmt::Debug>(
        string: &str,
        f: fn(&mut Lexer, arena: &mut ExprArena) -> Result<T, ParseError>,
    ) -> Result<T, ParseError> {
        println!("parsing: {}", string);
        let mut lexer = Lexer::new(&String::from(string), "parser tests");
        lexer.next();
        let mut arena = ExprArena::new();
        let r = f(&mut lexer, &mut arena)?;
        println!("{} ==> {:?}, arena: {:?}", string, r, arena);
        expect(&lexer, Token::End)?;
        Ok(r)
    }

    fn test<T: std::fmt::Debug>(
        string: &str,
        f: fn(&mut Lexer, &mut ExprArena) -> Result<T, ParseError>,
    ) {
        assert!(parse_fn(string, f).is_ok());
    }

    fn test_strings<T: std::fmt::Debug>(
        f: fn(&mut Lexer, &mut ExprArena) -> Result<T, ParseError>,
        strings: &[&str],
    ) {
        for string in strings {
            assert!(parse_fn(string, f).is_ok());
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
                "test {}",
                "f()",
                "struct x { }",
                "struct x { \n }",
                "struct x { x: i8 }",
                "struct x { x: i8\n }",
                "struct x { x: i8, y: i8 }",
                "enum x { }",
                "enum x { a, b, c }",
                "enum x { a,\nb }",
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
