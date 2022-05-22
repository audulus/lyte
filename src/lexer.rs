use crate::defs::*;

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Id(String),
    Integer(i64),
    Real(String),
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,
    Comma,
    Plus,
    Minus,
    Mult,
    Div,
    Power,
    Less,
    Leq,
    Greater,
    Geq,
    Lshift,
    Rshift,
    Cond,
    Colon,
    Semi,
    Equal,
    NotEqual,
    Assign,
    Dot,
    Arrow,
    TokMod,
    Lmath,
    Rmath,
    If,
    Else,
    While,
    TokAs,
    Var,
    Let,
    Defer,
    Return,
    Len,
    Struct,
    Enum,
    Macro,
    Void,
    Bool,
    Int8,
    Int32,
    Int64,
    Float32,
    Float64,
    Any,
    Arena,
    Ref,
    True,
    False,
    And,
    Or,
    Not,
    Char(char),
    String(String),
    Pipe,
    At,
    Typevar,
    Interface,
    Where,
    Endl,
    End,
    Error,
}

#[derive(Debug)]
pub struct Lexer {
    code: String,
    pub i: usize,
    pub tok: Token,
    pub loc: Loc,
}

fn id_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

impl Lexer {
    pub fn new(code: &str, file: &str) -> Self {
        Lexer {
            code: String::from(code),
            i: 0,
            tok: Token::Error,
            loc: Loc {
                file: Name::new(file.into()),
                line: 1,
            },
        }
    }

    fn _next(&mut self) -> Token {
        let bytes = self.code.as_bytes();
        let n = self.code.len();

        // Skip whitespace and comments.
        let mut has_newline = false;
        while self.i < bytes.len() && bytes[self.i].is_ascii_whitespace() {
            // Whitespace.
            while self.i < bytes.len() && bytes[self.i].is_ascii_whitespace() {
                if bytes[self.i] == b'\n' {
                    self.loc.line += 1;
                    has_newline = true;
                }
                self.i += 1;
            }

            // Comments.
            if self.i + 1 < n && bytes[self.i] == b'/' && bytes[self.i + 1] == b'/' {
                self.i += 2;
                while self.i < n {
                    if bytes[self.i] == b'\n' {
                        self.loc.line += 1;
                        has_newline = true;
                        break;
                    }
                    self.i += 1
                }
            }
        }

        if has_newline {
            return Token::Endl;
        }

        // End of string.
        if self.i == bytes.len() {
            return Token::End;
        }

        // Identifier.
        if bytes[self.i].is_ascii_alphabetic() || bytes[self.i] == b'_' {
            let mut id = String::new();
            while self.i < n && id_byte(bytes[self.i]) {
                id.push(bytes[self.i] as char);
                self.i += 1;
            }

            // Check against keywords.
            return match id.as_str() {
                "if" => Token::If,
                "else" => Token::Else,
                "while" => Token::While,
                "void" => Token::Void,
                "i8" => Token::Int8,
                "i32" => Token::Int32,
                "var" => Token::Var,
                "let" => Token::Let,
                "return" => Token::Return,
                "struct" => Token::Struct,
                "enum" => Token::Enum,
                "arena" => Token::Arena,
                "bool" => Token::Bool,
                "true" => Token::True,
                "false" => Token::False,
                "macro" => Token::Macro,
                "typevar" => Token::Typevar,
                "interface" => Token::Interface,
                "where" => Token::Where,
                _ => Token::Id(id),
            };
        }

        // Numbers.
        if bytes[self.i].is_ascii_digit()
            || (bytes[self.i] == b'.' && self.i + 1 < n && bytes[self.i + 1].is_ascii_digit())
        {
            let start = self.i;
            let mut fraction = false;

            while self.i < n {
                if bytes[self.i] == b'.' {
                    if fraction {
                        break;
                    }
                    fraction = true
                } else if !bytes[self.i].is_ascii_digit() {
                    break;
                }

                self.i += 1;
            }

            return if fraction {
                Token::Real(self.code[start..self.i].into())
            } else {
                Token::Integer(self.code[start..self.i].parse().unwrap())
            };
        }

        // Strings.
        if bytes[self.i] == b'"' {
            self.i += 1;
            let mut s = String::new();
            while self.i < n && bytes[self.i] != b'"' {
                s.push(bytes[self.i] as char);
                self.i += 1;
            }

            if self.i == n {
                return Token::Error;
            }

            self.i += 1;

            return Token::String(s);
        }

        if bytes[self.i] == b'\'' {
            let mut tok = Token::Error;
            self.i += 1;

            if self.i + 1 >= n {
                return Token::Error;
            }

            if bytes[self.i] == b'\\' {
                self.i += 1;
                if bytes[self.i] == b'\\' {
                    tok = Token::Char('\\');
                } else if bytes[self.i] == b'n' {
                    tok = Token::Char('\n');
                }
            } else {
                tok = Token::Char(bytes[self.i] as char);
            }

            self.i += 1;

            if self.i < n && bytes[self.i] == b'\'' {
                self.i += 1;
                return tok;
            } else {
                return Token::Error;
            }
        }

        let c = bytes[self.i] as char;
        self.i += 1;
        match c {
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            '[' => Token::Lbracket,
            ']' => Token::Rbracket,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '-' => {
                if self.i < n && bytes[self.i] == b'>' {
                    self.i += 1;
                    Token::Arrow
                } else {
                    Token::Minus
                }
            }
            '*' => Token::Mult,
            '/' => Token::Div,
            '^' => Token::Power,
            '?' => Token::Cond,
            ':' => Token::Colon,
            ';' => Token::Semi,
            '.' => Token::Dot,
            '|' => Token::Pipe,
            '@' => Token::At,
            '<' => {
                if self.i < n && bytes[self.i] == b'=' {
                    self.i += 1;
                    Token::Leq
                } else {
                    Token::Less
                }
            }
            '>' => {
                if self.i < n && bytes[self.i] == b'=' {
                    self.i += 1;
                    Token::Geq
                } else {
                    Token::Greater
                }
            }
            '=' => {
                if self.i < n && bytes[self.i] == b'=' {
                    self.i += 1;
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            '!' => {
                if self.i < n && bytes[self.i] == b'=' {
                    self.i += 1;
                    Token::NotEqual
                } else {
                    Token::Not
                }
            }
            '\u{e2}' => {
                // Do we have enough?
                if self.i + 1 >= n {
                    return Token::Error;
                }

                if bytes[self.i] == 159 {
                    if bytes[self.i + 1] == 168 {
                        self.i += 2;
                        Token::Lmath
                    } else if bytes[self.i + 1] == 169 {
                        self.i += 2;
                        Token::Rmath
                    } else {
                        Token::Error
                    }
                } else if bytes[self.i] == 134 {
                    if bytes[self.i + 1] == 146 {
                        self.i += 2;
                        Token::Arrow
                    } else {
                        Token::Error
                    }
                } else if bytes[self.i] == 139 {
                    if bytes[self.i + 1] == 133 {
                        self.i += 2;
                        Token::Mult
                    } else {
                        Token::Error
                    }
                } else {
                    Token::Error
                }
            }
            _ => Token::Error,
        }
    }

    pub fn next(&mut self) {
        self.tok = self._next();
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn tokens(expr: &str) -> Vec<Token> {
        let mut lex = Lexer::new(&String::from(expr), "test_lexer");

        let mut toks = vec![];
        lex.next();
        while lex.tok != Token::End {
            toks.push(lex.tok.clone());
            lex.next();
        }
        toks
    }

    fn id(s: &str) -> Token {
        Token::Id(String::from(s))
    }

    #[test]
    fn test_lexer() {
        use Token::*;
        assert_eq!(tokens(""), vec![]);
        assert_eq!(tokens(" "), vec![]);
        assert_eq!(tokens("\n"), vec![Endl]);
        assert_eq!(tokens("\n    \n"), vec![Endl]);
        assert_eq!(tokens("x"), vec![id("x")]);
        assert_eq!(tokens(" x "), vec![id("x")]);
        assert_eq!(tokens("_x"), vec![id("_x")]);
        assert_eq!(tokens("42"), vec![Integer(42)]);
        assert_eq!(tokens("42.0"), vec![Real("42.0".to_string())]);
        assert_eq!(tokens(".5"), vec![Real(".5".to_string())]);
        assert_eq!(tokens("2 + 2"), vec![Integer(2), Plus, Integer(2)]);
        assert_eq!(tokens("foo()"), vec![id("foo"), Lparen, Rparen]);
        assert_eq!(tokens("x <= y"), vec![id("x"), Leq, id("y")]);
        assert_eq!(tokens("x >= y"), vec![id("x"), Geq, id("y")]);
        assert_eq!(tokens("x != y"), vec![id("x"), NotEqual, id("y")]);
        assert_eq!(tokens("."), vec![Dot]);
        assert_eq!(
            tokens("((x))"),
            vec![Lparen, Lparen, id("x"), Rparen, Rparen]
        );
        assert_eq!(tokens("1x"), vec![Integer(1), id("x")]);
        assert_eq!(tokens("void"), vec![Void]);
        assert_eq!(tokens("i8"), vec![Int8]);
        assert_eq!(tokens("i32"), vec![Int32]);
        assert_eq!(tokens("⟨"), vec![Lmath]);
        assert_eq!(tokens("⟩"), vec![Rmath]);
        assert_eq!(tokens("-"), vec![Minus]);
        assert_eq!(tokens("->"), vec![Arrow]);
        assert_eq!(tokens("→"), vec![Arrow]);
        assert_eq!(tokens("|"), vec![Pipe]);
        assert_eq!(tokens("return"), vec![Return]);
        assert_eq!(tokens("struct"), vec![Struct]);
        assert_eq!(tokens("macro"), vec![Macro]);
        assert_eq!(tokens("\"test\""), vec![Token::String("test".into())]);
        assert_eq!(
            tokens("\"test\" \n"),
            vec![Token::String("test".into()), Endl]
        );
        assert_eq!(tokens(".name"), vec![Dot, id("name")]);
        assert_eq!(tokens("snake_case"), vec![id("snake_case")]);
        assert_eq!(tokens("arena"), vec![Arena]);
        assert_eq!(tokens("true"), vec![True]);
        assert_eq!(tokens("false"), vec![False]);
        assert_eq!(tokens("bool"), vec![Bool]);
        assert_eq!(tokens("⋅"), vec![Mult]);
        assert_eq!(tokens("'x'"), vec![Char('x')]);
        assert_eq!(tokens("!"), vec![Not]);
        assert_eq!(tokens("x // comment"), vec![id("x")]);
        tokens("]VV)y<)'");
        tokens("<qVyA]V<");
        tokens("'\\B");
    }
}
