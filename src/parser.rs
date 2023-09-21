use crate::*;

impl std::ops::Index<ExprID> for ExprArena {
    type Output = Expr;

    fn index(&self, index: ExprID) -> &Self::Output {
        &self.exprs[index]
    }
}

struct ParseContext<'a> {
    pub lex: &'a mut Lexer,
    pub errors: &'a mut Vec<ParseError>,
}

impl<'a> ParseContext<'a> {
    pub fn next(&mut self) {
        self.lex.next()
    }

    pub fn err(&mut self, message: String) {
        self.errors.push(ParseError {
            location: self.lex.loc,
            message,
        })
    }
}

fn expect(tok: Token, cx: &mut ParseContext) {
    if cx.lex.tok == tok {
        cx.next();
    } else {
        let message = format!("expected {:?}, got {:?}", tok, cx.lex.tok);
        cx.err(message)
    }
}

fn expect_id(cx: &mut ParseContext) -> Name {
    if let Token::Id(name) = cx.lex.tok {
        cx.next();
        name
    } else {
        let message = format!("expected identifier, got {:?}", cx.lex.tok);
        println!("{:?}", message);
        cx.err(message);
        Name::new("".to_string())
    }
}

fn parse_typelist(typevars: &[Name], cx: &mut ParseContext) -> Vec<TypeID> {
    let mut r = vec![];
    expect(Token::Less, cx);

    loop {
        r.push(parse_basic_type(typevars, cx));

        if cx.lex.tok != Token::Comma {
            break;
        }

        cx.next();
    }

    expect(Token::Greater, cx);

    r
}

fn parse_basic_type(typevars: &[Name], cx: &mut ParseContext) -> TypeID {
    mk_type(match cx.lex.tok {
        Token::Void => {
            cx.next();
            Type::Void
        }
        Token::Bool => {
            cx.next();
            Type::Bool
        }
        Token::Int8 => {
            cx.next();
            Type::Int8
        }
        Token::Int32 => {
            cx.next();
            Type::Int32
        }
        Token::Lmath => {
            cx.next();
            let name = expect_id(cx);
            expect(Token::Rmath, cx);
            return typevar(&name);
        }
        Token::Typevar => {
            cx.next();
            let name = expect_id(cx);
            return typevar(&name);
        }
        Token::Lbracket => {
            cx.next();
            let r = parse_type(typevars, cx);
            if cx.lex.tok == Token::Semi {
                cx.next();
                if let Token::Integer(n) = cx.lex.tok {
                    cx.next();
                    expect(Token::Rbracket, cx);
                    Type::Array(r, n as i32)
                } else {
                    cx.err(String::from("Expected integer array size"));
                    Type::Array(r, 0)
                }
            } else {
                expect(Token::Rbracket, cx);
                Type::Array(r, 0)
            }
        }
        Token::Id(name) => {
            cx.next();

            // Do typevar substitutions here for
            // efficiency.
            if typevars.contains(&name) {
                return mk_type(Type::Var(name));
            }

            let mut args = vec![];
            if cx.lex.tok == Token::Less {
                args = parse_typelist(typevars, cx);
            }
            return mk_type(Type::Name(name, args));
        }
        Token::Lparen => {
            cx.next();
            let t = parse_type(typevars, cx);
            expect(Token::Rparen, cx);
            return t;
        }
        _ => {
            cx.next();
            cx.err(String::from("Expected type"));
            Type::Void
        }
    })
}

fn parse_type(typevars: &[Name], cx: &mut ParseContext) -> TypeID {
    let mut lhs = parse_basic_type(typevars, cx);

    while cx.lex.tok == Token::Arrow {
        cx.next();
        let rhs = parse_basic_type(typevars, cx);

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

fn parse_paramlist(typevars: &[Name], cx: &mut ParseContext) -> Vec<Param> {
    let mut r = vec![];

    loop {
        if let Token::Id(name) = cx.lex.tok {
            cx.next();

            // In the case of lambdas, we can omit the types.
            let ty = if cx.lex.tok == Token::Colon {
                cx.next();
                Some(parse_type(typevars, cx))
            } else {
                None
            };

            r.push(Param { name: name, ty })
        }

        if cx.lex.tok != Token::Comma {
            break;
        }

        cx.next();

        skip_newlines(cx.lex);
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
        Token::Or => Binop::Or,
        Token::And => Binop::And,
        _ => {
            println!("got {:?}", tok);
            unreachable!()
        }
    };

    Expr::Binop(op, lhs, rhs)
}

fn parse_lambda(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    if cx.lex.tok == Token::Pipe {
        cx.next();
        let params = parse_paramlist(typevars, cx);
        expect(Token::Pipe, cx);

        let body = parse_lambda(arena, typevars, cx);

        arena.add(Expr::Lambda { params, body }, cx.lex.loc)
    } else {
        parse_expr(arena, typevars, cx)
    }
}

fn parse_if(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    let cond = parse_expr(arena, typevars, cx);
    let then = parse_block(arena, typevars, cx);

    let els = if cx.lex.tok == Token::Else {
        cx.next();
        if cx.lex.tok == Token::If {
            cx.next();
            Some(parse_if(arena, typevars, cx))
        } else {
            Some(parse_block(arena, typevars, cx))
        }
    } else {
        None
    };

    arena.add(Expr::If(cond, then, els), cx.lex.loc)
}

fn parse_expr(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    if cx.lex.tok == Token::If {
        cx.next();
        parse_if(arena, typevars, cx)
    } else {
        parse_assign(arena, typevars, cx)
    }
}

fn parse_assign(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    let mut lhs = parse_eq(arena, typevars, cx);

    while cx.lex.tok == Token::Assign {
        let t = cx.lex.tok.clone();
        cx.next();
        let rhs = parse_eq(arena, typevars, cx);

        lhs = arena.add(binop(&t, lhs, rhs), cx.lex.loc)
    }

    lhs
}

fn parse_eq(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    let mut lhs = parse_logic(arena, typevars, cx);

    while cx.lex.tok == Token::Equal || cx.lex.tok == Token::NotEqual {
        let t = cx.lex.tok.clone();
        cx.next();
        let rhs = parse_logic(arena, typevars, cx);

        lhs = arena.add(binop(&t, lhs, rhs), cx.lex.loc)
    }

    lhs
}

fn parse_logic(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    let mut lhs = parse_rel(arena, typevars, cx);

    while cx.lex.tok == Token::Or || cx.lex.tok == Token::And {
        let t = cx.lex.tok.clone();
        cx.next();
        let rhs = parse_rel(arena, typevars, cx);

        lhs = arena.add(binop(&t, lhs, rhs), cx.lex.loc)
    }

    lhs
}

fn parse_rel(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    let mut lhs = parse_sum(arena, typevars, cx);

    while cx.lex.tok == Token::Leq
        || cx.lex.tok == Token::Geq
        || cx.lex.tok == Token::Less
        || cx.lex.tok == Token::Greater
    {
        let t = cx.lex.tok.clone();
        cx.next();
        let rhs = parse_sum(arena, typevars, cx);

        lhs = arena.add(binop(&t, lhs, rhs), cx.lex.loc)
    }

    lhs
}

fn parse_sum(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    let mut lhs = parse_term(arena, typevars, cx);

    while cx.lex.tok == Token::Plus || cx.lex.tok == Token::Minus {
        let t = cx.lex.tok.clone();
        cx.next();
        let rhs = parse_term(arena, typevars, cx);

        lhs = arena.add(binop(&t, lhs, rhs), cx.lex.loc)
    }

    lhs
}

fn parse_term(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    let mut lhs = parse_exp(arena, typevars, cx);

    while cx.lex.tok == Token::Mult || cx.lex.tok == Token::Div {
        let t = cx.lex.tok.clone();
        cx.next();
        let rhs = parse_exp(arena, typevars, cx);

        lhs = arena.add(binop(&t, lhs, rhs), cx.lex.loc)
    }

    lhs
}

fn parse_exp(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    let mut lhs = parse_factor(arena, typevars, cx);

    while cx.lex.tok == Token::Power {
        cx.next();

        let rhs = parse_factor(arena, typevars, cx);

        lhs = arena.add(binop(&Token::Power, lhs, rhs), cx.lex.loc)
    }

    lhs
}

fn parse_factor(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    match &cx.lex.tok {
        Token::Minus => {
            cx.next();
            let e = parse_factor(arena, typevars, cx);
            arena.add(Expr::Unop(e), cx.lex.loc)
        }
        Token::Plus => {
            cx.next();
            parse_factor(arena, typevars, cx)
        }
        Token::Not => {
            cx.next();
            let e = parse_factor(arena, typevars, cx);
            arena.add(Expr::Unop(e), cx.lex.loc)
        }
        _ => parse_postfix(arena, typevars, cx),
    }
}

fn parse_postfix(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    let mut e = parse_atom(arena, typevars, cx);

    loop {
        match cx.lex.tok {
            Token::Lparen => {
                cx.next();
                let args = if cx.lex.tok != Token::Rparen {
                    let args = parse_exprlist(arena, typevars, cx);
                    expect(Token::Rparen, cx);
                    args
                } else {
                    cx.next();
                    vec![]
                };
                e = arena.add(Expr::Call(e, args), cx.lex.loc);
            }
            Token::Colon => {
                cx.next();
                let t = parse_type(typevars, cx);
                e = arena.add(Expr::AsTy(e, t), cx.lex.loc);
            }
            Token::Lbracket => {
                cx.next();
                let idx = parse_expr(arena, typevars, cx);
                expect(Token::Rbracket, cx);
                e = arena.add(Expr::ArrayIndex(e, idx), cx.lex.loc);
            }
            Token::Dot => {
                cx.next();
                let field = expect_id(cx);
                e = arena.add(Expr::Field(e, field), cx.lex.loc);
            }
            Token::As => {
                cx.next();
                let ty = parse_type(typevars, cx);
                e = arena.add(Expr::AsTy(e, ty), cx.lex.loc);
            }
            _ => {
                return e;
            }
        }
    }
}

fn parse_atom(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    match cx.lex.tok {
        Token::Id(id) => {
            let e = Expr::Id(id);
            cx.next();
            arena.add(e, cx.lex.loc)
        }
        Token::True => {
            cx.next();
            arena.add(Expr::True, cx.lex.loc)
        }
        Token::False => {
            cx.next();
            arena.add(Expr::False, cx.lex.loc)
        }
        Token::Dot => {
            cx.next();
            let name = expect_id(cx);
            arena.add(Expr::Enum(name), cx.lex.loc)
        }
        Token::Integer(x) => {
            let e = Expr::Int(x);
            cx.next();
            arena.add(e, cx.lex.loc)
        }
        Token::Real(x) => {
            let e = Expr::Real(x.to_string());
            cx.next();
            arena.add(e, cx.lex.loc)
        }
        Token::String(s) => {
            let e = Expr::String(s.to_string());
            cx.next();
            arena.add(e, cx.lex.loc)
        }
        Token::Char(c) => {
            let e = Expr::Char(c);
            cx.next();
            arena.add(e, cx.lex.loc)
        }
        Token::Lparen => {
            cx.next();

            let mut tup = vec![];
            loop {
                let rr = parse_lambda(arena, typevars, cx);
                tup.push(rr);
                if cx.lex.tok == Token::Rparen {
                    break;
                }
                expect(Token::Comma, cx);
            }

            cx.next();

            if tup.len() > 1 {
                let e = Expr::Tuple(tup);
                arena.add(e, cx.lex.loc)
            } else {
                tup[0]
            }
        }
        Token::Lbrace => parse_block(arena, typevars, cx),
        Token::Lbracket => parse_array_literal(arena, typevars, cx),
        Token::At => {
            // macro invocations
            cx.next();

            let name = expect_id(cx);

            expect(Token::Lparen, cx);
            let params = parse_exprlist(arena, typevars, cx);
            expect(Token::Rparen, cx);

            let e = Expr::Macro(name, params);
            arena.add(e, cx.lex.loc)
        }
        _ => {
            cx.next();
            cx.err(String::from("Expected expression"));
            arena.add(Expr::Tuple(vec![]), cx.lex.loc)
        }
    }
}

fn parse_exprlist(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> Vec<ExprID> {
    let mut r = vec![];

    loop {
        r.push(parse_lambda(arena, typevars, cx));

        if cx.lex.tok != Token::Comma {
            break;
        }

        cx.next();
    }

    r
}

fn parse_array_literal(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    expect(Token::Lbracket, cx);
    let mut r = vec![];

    while cx.lex.tok != Token::Rbracket {
        r.push(parse_lambda(arena, typevars, cx));

        if cx.lex.tok == Token::Comma {
            cx.next();
        } else if cx.lex.tok == Token::Semi {
            cx.next();
            let count = parse_expr(arena, typevars, cx);
            let e = Expr::Array(r[0], count);
            expect(Token::Rbracket, cx);
            return arena.add(e, cx.lex.loc);
        } else if cx.lex.tok == Token::Rbracket {
            break;
        } else {
            cx.err(String::from("Expected comma or semicolon in array literal"));
            cx.next();
        }
    }

    cx.next();

    arena.add(Expr::ArrayLiteral(r), cx.lex.loc)
}

fn parse_stmt(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    match &cx.lex.tok {
        Token::Var => {
            cx.next();
            let name = expect_id(cx);

            if cx.lex.tok == Token::Assign {
                cx.next();
                let e = parse_lambda(arena, typevars, cx);
                arena.add(Expr::Var(name, Some(e), None), cx.lex.loc)
            } else if cx.lex.tok == Token::Colon {
                cx.next();
                let t = parse_type(typevars, cx);
                arena.add(Expr::Var(name, None, Some(t)), cx.lex.loc)
            } else {
                cx.err(String::from("expected assignment or type"));
                arena.add(Expr::Var(name, None, None), cx.lex.loc)
            }
        }
        Token::Let => {
            cx.next();
            let name = expect_id(cx);

            if cx.lex.tok == Token::Assign {
                cx.next();
                let e = parse_lambda(arena, typevars, cx);
                arena.add(Expr::Let(name, e, None), cx.lex.loc)
            } else {
                cx.err(String::from("expected assignment or type"));
                arena.add(Expr::Error, cx.lex.loc)
            }
        }
        Token::Arena => {
            cx.next();
            let e = parse_block(arena, typevars, cx);
            arena.add(Expr::Arena(e), cx.lex.loc)
        }
        Token::While => {
            cx.next();
            let cond = parse_expr(arena, typevars, cx);
            let body = parse_block(arena, typevars, cx);
            arena.add(Expr::While(cond, body), cx.lex.loc)
        }
        Token::Return => {
            cx.next();
            let e = parse_expr(arena, typevars, cx);
            arena.add(Expr::Return(e), cx.lex.loc)
        }
        Token::For => {
            cx.next();
            let var = expect_id(cx);
            expect(Token::In, cx);
            let start = parse_expr(arena, typevars, cx);
            expect(Token::Range, cx);
            let end = parse_expr(arena, typevars, cx);
            let body = parse_block(arena, typevars, cx);
            arena.add(
                Expr::For {
                    var,
                    start,
                    end,
                    body,
                },
                cx.lex.loc,
            )
        }
        _ => parse_expr(arena, typevars, cx),
    }
}

fn parse_block(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> ExprID {
    let mut r = vec![];
    expect(Token::Lbrace, cx);

    loop {
        skip_newlines(cx.lex);

        if cx.lex.tok == Token::Rbrace {
            break;
        }

        r.push(parse_stmt(arena, typevars, cx));

        if cx.lex.tok != Token::Endl {
            break;
        }

        cx.next();
    }

    cx.next();

    arena.add(Expr::Block(r), cx.lex.loc)
}

fn parse_fieldlist(typevars: &[Name], cx: &mut ParseContext) -> Vec<Field> {
    let mut r = vec![];

    skip_newlines(cx.lex);

    while cx.lex.tok != Token::Rbrace {
        skip_newlines(cx.lex);

        let name = expect_id(cx);

        expect(Token::Colon, cx);

        let ty = parse_type(typevars, cx);
        r.push(Field {
            name,
            ty,
            loc: cx.lex.loc,
        });

        skip_newlines(cx.lex);

        if cx.lex.tok != Token::Comma {
            break;
        }

        cx.next();
    }

    r
}

fn parse_caselist(cx: &mut ParseContext) -> Vec<Name> {
    let mut r = vec![];

    loop {
        if let Token::Id(name) = cx.lex.tok {
            cx.next();
            r.push(name);
        }

        if cx.lex.tok != Token::Comma {
            break;
        }

        cx.next();

        skip_newlines(cx.lex);
    }

    r
}

fn parse_typevar_list(cx: &mut ParseContext) -> Vec<Name> {
    let mut r = vec![];
    expect(Token::Less, cx);

    loop {
        let name = expect_id(cx);
        r.push(name);

        if cx.lex.tok != Token::Comma {
            break;
        }

        cx.next();
    }

    expect(Token::Greater, cx);

    r
}

fn token_in(tok: &Token, set: &[Token]) -> bool {
    set.iter().any(|x| x == tok)
}

fn parse_func_decl(name: Name, cx: &mut ParseContext) -> FuncDecl {
    let mut params = vec![];
    let mut typevars = vec![];
    let mut constraints = vec![];
    let loc = cx.lex.loc;
    let mut arena = ExprArena::new();

    if cx.lex.tok == Token::Less {
        typevars = parse_typevar_list(cx);
    }

    if cx.lex.tok == Token::Lparen {
        expect(Token::Lparen, cx);
        params = parse_paramlist(&typevars, cx);
        expect(Token::Rparen, cx);
    }

    skip_newlines(cx.lex);

    let mut ret = mk_type(Type::Void);
    if cx.lex.tok == Token::Arrow {
        cx.next();
        ret = parse_type(&typevars, cx);
    }

    skip_newlines(cx.lex);

    if cx.lex.tok == Token::Where {
        cx.next();

        while !token_in(&cx.lex.tok, &[Token::Lbrace, Token::Rbrace, Token::Endl]) {
            let interface_name = expect_id(cx);
            let typevars = parse_typevar_list(cx);
            constraints.push(InterfaceConstraint {
                interface_name,
                typevars,
            })
        }
    }

    let mut body = None;
    if cx.lex.tok == Token::Lbrace {
        body = Some(parse_block(&mut arena, &typevars, cx));
    }

    skip_newlines(cx.lex);

    FuncDecl {
        name,
        typevars,
        params,
        body,
        ret,
        constraints,
        loc,
        arena,
        types: vec![],
    }
}

fn parse_interface(cx: &mut ParseContext) -> Decl {
    let loc = cx.lex.loc;
    cx.next();

    let name = expect_id(cx);

    skip_newlines(cx.lex);

    let mut typevars = vec![];

    if cx.lex.tok == Token::Less {
        typevars = parse_typevar_list(cx);
    }

    expect(Token::Lbrace, cx);

    skip_newlines(cx.lex);

    let mut funcs = vec![];

    while cx.lex.tok != Token::Rbrace {
        let name = expect_id(cx);
        funcs.push(parse_func_decl(name, cx));
        skip_newlines(cx.lex);
    }

    cx.next();

    let mut iface = Interface {
        name,
        typevars,
        funcs,
        loc,
    };

    iface.subst_typevars();

    Decl::Interface(iface)
}

fn parse_decl(cx: &mut ParseContext) -> Option<Decl> {
    Some(match cx.lex.tok.clone() {
        Token::Id(name) => {
            // Function declaration.
            cx.next();
            Decl::Func(parse_func_decl(name, cx))
        }
        Token::Macro => {
            // Macro declaration.
            cx.next();

            let name = expect_id(cx);

            Decl::Macro(parse_func_decl(name, cx))
        }
        Token::Struct => {
            // Struct delcaration.
            cx.next();

            let name = expect_id(cx);

            let mut typevars = vec![];
            if cx.lex.tok == Token::Less {
                typevars = parse_typevar_list(cx);
            }

            expect(Token::Lbrace, cx);
            let fields = parse_fieldlist(&typevars, cx);
            expect(Token::Rbrace, cx);

            Decl::Struct(StructDecl {
                name,
                typevars,
                fields,
            })
        }
        Token::Enum => {
            cx.next();

            let name = expect_id(cx);

            expect(Token::Lbrace, cx);
            let cases = parse_caselist(cx);
            expect(Token::Rbrace, cx);

            Decl::Enum { name, cases }
        }
        Token::Var => {
            cx.next();

            let name = expect_id(cx);

            expect(Token::Colon, cx);

            let ty = parse_type(&[], cx);

            Decl::Global { name, ty }
        }
        Token::Interface => parse_interface(cx),
        _ => {
            cx.err(format!("Expected declaration, got {:?}", cx.lex.tok));
            cx.next();
            return None;
        }
    })
}

fn skip_newlines(lexer: &mut Lexer) {
    while lexer.tok == Token::Endl {
        lexer.next();
    }
}

/// Parser entry point for a single file.
///
/// Note that parsing is only incremental on file-granularity, but salsa is
/// smart enough to avoid invoking type-checking for everything in the file
/// in most cases.
pub fn parse_program(lex: &mut Lexer, errors: &mut Vec<ParseError>) -> Vec<Decl> {
    let mut decls = vec![];

    let mut cx = ParseContext { lex, errors };

    skip_newlines(cx.lex);

    while cx.lex.tok != Token::End {
        if let Some(decl) = parse_decl(&mut cx) {
            decls.push(decl);
        }
        skip_newlines(cx.lex);
    }

    decls
}

#[cfg(test)]
mod tests {

    use super::*;

    fn type_parser(string: &str) -> TypeID {
        let mut lexer = Lexer::new(string, "parser tests");
        let mut errors = vec![];
        let mut cx = ParseContext {
            lex: &mut lexer,
            errors: &mut errors,
        };
        cx.next();

        let t = parse_type(&[], &mut cx);
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
        errors: &mut Vec<ParseError>,
        f: fn(arena: &mut ExprArena, typevars: &[Name], cx: &mut ParseContext) -> T,
    ) -> Result<T, ParseError> {
        println!("parsing: {}", string);
        let mut lexer = Lexer::new(&String::from(string), "parser tests");

        let mut cx = ParseContext {
            lex: &mut lexer,
            errors: errors,
        };
        cx.next();

        let r = f(arena, &[], &mut cx);
        println!("{} ==> {:?}, arena: {:?}", string, r, arena);
        expect(Token::End, &mut cx);
        assert!(errors.is_empty());
        Ok(r)
    }

    fn test<T: std::fmt::Debug>(
        string: &str,
        f: fn(&mut ExprArena, &[Name], &mut ParseContext) -> T,
    ) {
        let mut arena = ExprArena::new();
        let mut errors = vec![];
        assert!(parse_fn(string, &mut arena, &mut errors, f).is_ok());
    }

    fn test_strings<T: std::fmt::Debug>(
        f: fn(&mut ExprArena, &[Name], cx: &mut ParseContext) -> T,
        strings: &[&str],
    ) {
        for string in strings {
            let mut arena = ExprArena::new();
            let mut errors = vec![];
            assert!(parse_fn(string, &mut arena, &mut errors, f).is_ok());
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
        test("x || y", parse_expr);
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
                "for x in 0 .. 5 { }",
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
            |_arena, _, cx| parse_decl(cx),
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
                "f<T>() -> i8 where MyInterface<T> { }",
                "test {}",
                "f()",
                "struct x { }",
                "struct x { \n }",
                "struct x { x: i8 }",
                "struct x { x: i8\n }",
                "struct x { x: i8, y: i8 }",
                "struct x { x: i8\n, y: i8 }",
                "struct x<T> { }",
                "struct x { y: typevar T }",
                "enum x { }",
                "enum x { a, b, c }",
                "enum x { a,\nb }",
                "macro m() { }",
                "interface x { }",
                "interface x { f(x: i8) }",
                "interface Compare {
                    cmp(lhs: typevar A, rhs: typevar A) -> i32
                }",
                "interface Compare<A> {
                    cmp(lhs: typevar A, rhs: typevar A) -> i32
                }",
            ],
        );
    }

    #[test]
    fn test_parse_program() {
        test_strings(
            |_, _, cx| parse_program(cx.lex, cx.errors),
            &["", "\n", "\nf()", "f(){} g(){}", "f(){}\n g(){}", "f()\n{}"],
        );
    }
}
