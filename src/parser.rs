use crate::defs::*;
use crate::lexer::*;
use crate::types::*;
use internment::Intern;

#[derive(Clone, Debug)]
struct ParseError {
    location: usize,
    message: String,
}

fn expect(lexer: &Lexer, tok: Token) -> Result<(), ParseError> {
    if lexer.tok == tok {
        Ok(())
    } else {
        let message = format!("expected {:?}, got {:?}", tok, lexer.tok);
        println!("{:?}", message);
        Err(ParseError {
            location: lexer.i,
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
                    location: lexer.i,
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
                        location: lexer.i,
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
                location: lexer.i,
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

fn binop(tok: &Token, lhs: Expr, rhs: Expr) -> Expr {
    let op = match tok {
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

    Expr::Binop(op, Box::new(lhs), Box::new(rhs))
}

fn parse_lambda(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    if lexer.tok == Token::Pipe {
        lexer.next();
        let params = parse_paramlist(lexer)?;
        expect(lexer, Token::Pipe)?;
        lexer.next();

        let body = Box::new(parse_lambda(lexer)?);

        Ok(Expr::Lambda { params, body })
    } else {
        parse_expr(lexer)
    }
}

fn parse_expr(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    if lexer.tok == Token::If {
        lexer.next();
        let cond = parse_expr(lexer)?;
        let then = parse_block(lexer)?;

        let els = if lexer.tok == Token::Else {
            lexer.next();
            Some(parse_block(lexer)?)
        } else {
            None
        };

        Ok(Expr::If(Box::new(cond), then, els))
    } else {
        parse_eq(lexer)
    }
}

fn parse_eq(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_rel(lexer)?;

    while lexer.tok == Token::Equal || lexer.tok == Token::NotEqual {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_rel(lexer)?;

        lhs = binop(&t, lhs, rhs)
    }

    Ok(lhs)
}

fn parse_rel(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_sum(lexer)?;

    while lexer.tok == Token::Leq
        || lexer.tok == Token::Geq
        || lexer.tok == Token::Less
        || lexer.tok == Token::Greater
    {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_sum(lexer)?;

        lhs = binop(&t, lhs, rhs)
    }

    Ok(lhs)
}

fn parse_sum(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_term(lexer)?;

    while lexer.tok == Token::Plus || lexer.tok == Token::Minus {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_term(lexer)?;

        lhs = binop(&t, lhs, rhs)
    }

    Ok(lhs)
}

fn parse_term(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_exp(lexer)?;

    while lexer.tok == Token::Mult || lexer.tok == Token::Div {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_exp(lexer)?;

        lhs = binop(&t, lhs, rhs)
    }

    Ok(lhs)
}

fn parse_exp(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_factor(lexer)?;

    while lexer.tok == Token::Power {
        lexer.next();

        let rhs = parse_factor(lexer)?;

        lhs = binop(&Token::Power, lhs, rhs)
    }

    Ok(lhs)
}

fn parse_factor(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    if lexer.tok == Token::Minus {
        lexer.next();
        return Ok(Expr::Unop(Box::new(parse_atom(lexer)?)));
    }

    if lexer.tok == Token::Plus {
        lexer.next();
        return parse_atom(lexer);
    }

    parse_postfix(lexer)
}

fn parse_postfix(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let lhs = parse_atom(lexer)?;

    if lexer.tok == Token::Lparen {
        lexer.next();
        let args = if lexer.tok != Token::Rparen {
            let args = parse_exprlist(lexer)?;
            expect(lexer, Token::Rparen)?;
            lexer.next();
            args
        } else {
            lexer.next();
            vec![]
        };
        Ok(Expr::Call(Box::new(lhs), args))
    } else if lexer.tok == Token::Colon {
        lexer.next();
        let t = parse_type(lexer)?;
        Ok(Expr::AsTy(Box::new(lhs), t))
    } else {
        Ok(lhs)
    }
}

fn parse_atom(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    Ok(match &lexer.tok {
        Token::Id(id) => {
            let e = Expr::Id(Intern::new(id.clone()));
            lexer.next();
            e
        }
        Token::Integer(x) => {
            let e = Expr::Int(*x);
            lexer.next();
            e
        }
        Token::Real(x) => {
            let e = Expr::Real(*x);
            lexer.next();
            e
        }
        Token::Lparen => {
            lexer.next();
            let rr = parse_lambda(lexer)?;
            expect(lexer, Token::Rparen)?;

            lexer.next();
            rr
        }
        Token::Lbrace => Expr::Block(parse_block(lexer)?),
        _ => {
            return Err(ParseError {
                location: lexer.i,
                message: String::from("Expected expression"),
            })
        }
    })
}

fn parse_exprlist(lexer: &mut Lexer) -> Result<Vec<Expr>, ParseError> {
    let mut r = vec![];

    loop {
        r.push(parse_lambda(lexer)?);

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();
    }

    Ok(r)
}

fn parse_stmt(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    match lexer.tok.clone() {
        Token::Var | Token::Let => {
            lexer.next();
            match &lexer.tok {
                Token::Id(name) => {
                    let n = Intern::new(name.clone());
                    lexer.next();
                    expect(lexer, Token::Equal)?;
                    lexer.next();
                    Ok(Expr::Var(n, Box::new(parse_lambda(lexer)?)))
                }
                _ => Err(ParseError {
                    location: lexer.i,
                    message: String::from("Expected assignment or function call"),
                }),
            }
        }
        Token::Return => {
            lexer.next();
            let e = parse_expr(lexer)?;
            Ok(Expr::Return(Box::new(e)))
        }
        _ => parse_expr(lexer),
    }
}

fn parse_block(lexer: &mut Lexer) -> Result<Block, ParseError> {
    let mut r = vec![];
    expect(lexer, Token::Lbrace)?;

    lexer.next();

    loop {
        skip_newlines(lexer);
        
        if lexer.tok == Token::Rbrace {
            break;
        }

        r.push(parse_stmt(lexer)?);

        if lexer.tok != Token::Endl {
            break;
        }

        lexer.next();
    }

    lexer.next();

    Ok(Block::new(r))
}

fn parse_fieldlist(lexer: &mut Lexer) -> Result<Vec<Field>, ParseError> {

    let mut r = vec![];

    loop {
        if let Token::Id(name) = &lexer.tok {
            let name = Intern::new(name.clone());
            lexer.next();
            expect(lexer, Token::Colon)?;
            lexer.next();
            let ty = parse_type(lexer)?;
            r.push(Field { name, ty })
        }

        if lexer.tok != Token::Comma {
            break;
        }

        lexer.next();

        skip_newlines(lexer);
    }

    Ok(r)

}

fn parse_decl(lexer: &mut Lexer) -> Result<Decl, ParseError> {
    match lexer.tok.clone() {
        Token::Id(name) => {
            // Function declaration.
            lexer.next();
            expect(lexer, Token::Lparen)?;
            lexer.next();
            let params = parse_paramlist(lexer)?;
            expect(lexer, Token::Rparen)?;
            lexer.next();

            skip_newlines(lexer);

            match lexer.tok.clone() {
                Token::Lbrace => Ok(Decl::Func {
                    name: Intern::new(name),
                    params,
                    body: parse_block(lexer)?,
                }),
                Token::Arrow => {
                    lexer.next();
                    let _t = parse_type(lexer)?;
                    Ok(Decl::Func {
                        name: Intern::new(name),
                        params,
                        body: parse_block(lexer)?,
                    })
                }
                _ => Err(ParseError {
                    location: lexer.i,
                    message: String::from("Expected return type or function body"),
                }),
            }
        }
        Token::Struct => {
            // Struct delcaration.
            lexer.next();

            let name = if let Token::Id(name) = &lexer.tok {
                Intern::new(name.clone())
            } else {
                return Err(ParseError {
                    location: lexer.i,
                    message: String::from("expected struct name"),
                });
            };

            expect(lexer, Token::Lbrace)?;
            lexer.next();
            
            let fields = parse_fieldlist(lexer)?;
            expect(lexer, Token::Rbrace)?;
            lexer.next();

            Ok(Decl::Struct{ name, fields })
        }
        _ => Err(ParseError {
            location: lexer.i,
            message: String::from("Expected declaration"),
        }),
    }
}

fn skip_newlines(lexer: &mut Lexer) {
    while lexer.tok == Token::Endl {
        lexer.next();
    }
}

fn parse_program(lexer: &mut Lexer) -> Result<Vec<Decl>, ParseError> {
    let mut decls = vec![];

    skip_newlines(lexer);

    while lexer.tok != Token::End {
        decls.push(parse_decl(lexer)?);
        skip_newlines(lexer);
    }

    Ok(decls)
}

#[cfg(test)]
mod tests {

    use super::*;

    fn type_parser(string: &str) -> TypeID {
        let mut lexer = Lexer::new(&String::from(string));
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
        f: fn(&mut Lexer) -> Result<T, ParseError>,
    ) -> Result<T, ParseError> {
        let mut lexer = Lexer::new(&String::from(string));
        lexer.next();
        let r = f(&mut lexer)?;
        println!("{} ==> {:?}", string, r);
        expect(&lexer, Token::End)?;
        Ok(r)
    }

    fn test<T: std::fmt::Debug>(string: &str, f: fn(&mut Lexer) -> Result<T, ParseError>) {
        assert!(parse_fn(string, f).is_ok());
    }

    fn test_strings<T: std::fmt::Debug>(f: fn(&mut Lexer) -> Result<T, ParseError>, strings: &[&str]) {
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
        test_strings(parse_atom, &[
            "x",
            "(x)",
            "42",
            "3.14159"
        ]);
    }

    #[test]
    fn test_parse_stmt() {
        test_strings(parse_stmt, &[
            "x = y",
            "f(x)",
            "var x = y",
            "let x = y",
            "let x = || x",
            "let x = if x { a } else { b }",
            "let x = if x { a+b } else { b }",
            "if x { }",
            "if x { } else { }",
            "x",
            "{ x }",
            "return x",
        ]);
    }

    #[test]
    fn test_parse_block() {
        test_strings(parse_block, &[
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
        ]);
    }

    #[test]
    fn test_parse_decl() {
        test_strings(parse_decl, &[
            "f(){}",
            "f(x: i8) { g(x) }",
            "f(x: i8) -> i8 { g(x) }",
            "f(x: i8) -> (i8 -> i8) { }",
            "f(x: i8, y: i8) { g(x) }",
            "f(x: i8,\n y: i8) { g(x) }",
            "f(x: i8 -> i8) { }",
        ]);
    }

    #[test]
    fn test_parse_program() {
        test_strings(parse_program, &[
            "",
            "f(){} g(){}",
            "f(){}\n g(){}",
            "f()\n{}",
        ]);
    }
}
