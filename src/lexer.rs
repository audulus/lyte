
#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Id(String),
    Integer(i64),
    Real(f64),
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
    TokVar,
    TokLet,
    TokDefer,
    TokReturn,
    TokLen,
    TokStruct,
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
    Char,
    String,
    End,
    Error,
}

pub struct Lexer {
    code: String,
    pub i: usize,
    pub tok: Token
}

impl Lexer {

    pub fn new(code: &String) -> Self {
        Lexer {
            code: code.clone(),
            i: 0,
            tok: Token::Error
        }
    }

    fn _next(&mut self) -> Token {

        let bytes = self.code.as_bytes();

        // Skip whitespace
        while self.i < bytes.len() && bytes[self.i].is_ascii_whitespace() {
            self.i += 1;
        }

        // End of string.
        if self.i == bytes.len() {
            return Token::End;
        }

        // Identifier.
        if bytes[self.i].is_ascii_alphabetic() {
            let mut id = String::new();
            while self.i < self.code.len() && bytes[self.i].is_ascii_alphanumeric() {
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
                _ => Token::Id(id)
            };
        }

        // Numbers.
        if bytes[self.i].is_ascii_digit()
        || (bytes[self.i] == ('.' as u8) && self.i+1 < self.code.len() && bytes[self.i+1].is_ascii_digit()) {
            let start = self.i;
            let mut fraction = false;
            
            while self.i < self.code.len() {
                if bytes[self.i] == ('.' as u8) {
                    if fraction {
                        break
                    }
                    fraction = true
                } else if !bytes[self.i].is_ascii_digit() {
                    break
                }

                self.i += 1;
            }

            let sl = &self.code[start..self.i];
            let x = sl.parse::<f64>();
            if x.is_err() {
                return Token::Error;
            }
            return Token::Real(x.unwrap());
        }

        let c = bytes[self.i] as char;
        self.i += 1;
        return match c {
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            '[' => Token::Lbracket,
            ']' => Token::Rbracket,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Mult,
            '/' => Token::Div,
            '^' => Token::Power,
            '?' => Token::Cond,
            ':' => Token::Colon,
            '.' => Token::Dot,
            '<' => {
                if bytes[self.i] == ('=' as u8) {
                    self.i += 1;
                    Token::Leq
                } else {
                    Token::Less
                }
            }
            '>' => {
                if bytes[self.i] == ('=' as u8) {
                    self.i += 1;
                    Token::Geq
                } else {
                    Token::Greater
                }
            }
            '=' => {
                if bytes[self.i] == ('=' as u8) {
                    self.i += 1
                }
                Token::Equal
            }
            '!' => {
                if bytes[self.i] == ('=' as u8) {
                    self.i += 1;
                    Token::NotEqual
                } else {
                    Token::Error
                }
            }
            '\u{e2}' => {
                if bytes[self.i] == 159 {
                    if bytes[self.i+1] == 168 {
                        self.i += 2;
                        Token::Lmath
                    } else if bytes[self.i+1] == 169 {
                        self.i += 2;
                        Token::Rmath
                    } else {
                        Token::Error
                    }
                } else {
                    Token::Error
                }
            }
            _ => Token::Error
        }
    }

    pub fn next(&mut self) -> Token {
        self.tok = self._next();
        self.tok.clone()
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn tokens(expr: &str) -> Vec<Token> {
        let mut lex = Lexer::new(&String::from(expr));

        let mut toks = vec![];
        let mut tok = lex.next();
        while tok != Token::End {
            toks.push(tok);
            tok = lex.next();
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
        assert_eq!(tokens("\n"), vec![]);
        assert_eq!(tokens("x"), vec![id("x")]);
        assert_eq!(tokens(" x "), vec![id("x")]);
        assert_eq!(tokens("42"), vec![Real(42.0)]);
        assert_eq!(tokens("3.14159"), vec![Real(3.14159)]);
        assert_eq!(tokens(".5"), vec![Real(0.5)]);
        assert_eq!(tokens("2 + 2"), vec![ Real(2.0), Plus, Real(2.0)]);
        assert_eq!(tokens("foo()"), vec![ id("foo"), Lparen, Rparen]);
        assert_eq!(tokens("x <= y"), vec![ id("x"), Leq, id("y")]);
        assert_eq!(tokens("x >= y"), vec![ id("x"), Geq, id("y")]);
        assert_eq!(tokens("x != y"), vec![ id("x"), NotEqual, id("y")]);
        assert_eq!(tokens("."), vec![Dot]);
        assert_eq!(tokens("((x))"), vec![Lparen, Lparen, id("x"), Rparen, Rparen]);
        assert_eq!(tokens("1x"), vec![ Real(1.0), id("x")]);
        assert_eq!(tokens("void"), vec![Void]);
        assert_eq!(tokens("i8"), vec![Int8]);
        assert_eq!(tokens("i32"), vec![Int32]);
        assert_eq!(tokens("⟨"), vec![Lmath]);
        assert_eq!(tokens("⟩"), vec![Rmath]);
    }

}