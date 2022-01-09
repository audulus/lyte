


lalrpop_mod!(pub lyte); // synthesized by LALRPOP

#[test]
fn basic() {
    assert!(lyte::TyParser::new().parse("i8").is_ok());
    assert!(lyte::TyParser::new().parse("foo").is_ok());
}