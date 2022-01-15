
#[derive(Clone)]
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
    TokInt8,
    TokInt32,
    TokInt64,
    TokFloat32,
    TokFloat64,
    TokAny,
    TokArena,
    TokNew,
    TokRef,
    TokTrue,
    TokFalse,
    TokAnd,
    TokOr,
    TokNot,
    TokOn,
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
            _ => Token::Error
        }
    }

    pub fn next(&mut self) -> Token {
        self.tok = self._next();
        self.tok.clone()
    }
}