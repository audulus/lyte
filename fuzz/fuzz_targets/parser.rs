#![no_main]
use libfuzzer_sys::fuzz_target;
use lyte::lexer::*;
use lyte::parser::*;

fuzz_target!(|data: String| {
    let mut lex = Lexer::new(&data, "fuzz");
    let mut arena = ExprArena::new();
    let r = parse_program(&mut lex, &mut arena);
    if r.is_ok() {
        assert!(lex.tok == Token::End);
    }
});
