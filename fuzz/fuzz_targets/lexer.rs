#![no_main]
use libfuzzer_sys::fuzz_target;
use lyte::lexer::*;

fuzz_target!(|data: String| {
    let mut lex = Lexer::new(&data, "fuzz");
    lex.next();
    while lex.tok != Token::End && lex.tok != Token::Error {
        lex.next();
    }
});
