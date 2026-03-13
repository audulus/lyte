#![no_main]
use libfuzzer_sys::fuzz_target;
use lyte::lexer::*;
use lyte::parser::*;

fuzz_target!(|data: String| {
    let mut lex = Lexer::new(&data, "fuzz");
    lex.next();
    let mut errors = vec![];
    let _ = parse_program(&mut lex, &mut errors);
});
