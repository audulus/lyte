use super::*;

#[test]
fn normalization_remaps_assumption_roots_and_shared_binding_occurrences() {
    let mut errors = vec![];
    let mut lexer = Lexer::new("assume { let value = 1; value >= 0 }", "<prelude>");
    lexer.next();
    let mut declarations = parse_program(&mut lexer, &mut errors);
    assert!(errors.is_empty());
    let Decl::Assume { arena, cond } = &mut declarations[0] else {
        panic!()
    };
    let loc = arena.locs[*cond];
    // The same binding subtree appears twice. Normalization must make each
    // occurrence independently checkable and discard unreachable source nodes.
    *cond = arena.add(Expr::Binop(Binop::And, *cond, *cond), loc);
    arena.add(Expr::Error, test_loc());
    let old_root = *cond;
    let source_locations = arena.locs.clone();
    normalize_source_body(arena, std::iter::once(&mut *cond));
    assert_ne!(*cond, old_root);
    assert_eq!(arena.locs[*cond], loc);
    assert!(arena.locs.iter().all(|loc| source_locations.contains(loc)));
    assert!(!arena.exprs.iter().any(|e| matches!(e, Expr::Error)));

    let table = DeclTable::new(declarations);
    let mut checker = Checker::new();
    checker.check_decl(&table.decls[0], &table);
    assert!(checker.errors.is_empty(), "{:?}", checker.errors);
    let Decl::Assume { arena, cond } = &table.decls[0] else {
        panic!()
    };
    let body = checker.checked_body(arena);
    assert_eq!(body.ty(*cond), mk_type(Type::Bool));
    assert_eq!(body.locals.len(), 2);
    let Expr::Binop(Binop::And, left, right) = body[*cond] else {
        panic!()
    };
    assert_ne!(left, right);
    for (root, expected) in [(left, LocalId(0)), (right, LocalId(1))] {
        let CheckedExpr::Block(statements) = &body[root] else {
            panic!()
        };
        assert!(matches!(body[statements[0]], Expr::Let(local, ..) if local == expected));
        let Expr::Binop(Binop::Geq, value, _) = body[statements[1]] else {
            panic!()
        };
        assert_eq!(body[value], Expr::Id(Reference::Local(expected)));
    }
    CheckedProgram::try_new(DeclTable::new(vec![Decl::Assume {
        arena: body,
        cond: *cond,
    }]))
    .unwrap();
}

#[test]
fn assumptions_check_boolean_values_and_body_declarations_at_source_locations() {
    for (source, message) in [
        (
            "assume 42i32",
            "assume condition must be a boolean expression",
        ),
        ("assume { return 42i32; true }", "return type must match"),
        ("assume { let value = {}; true }", "cannot have type void"),
        (
            "assume { let value = missing; true }",
            "undeclared identifier",
        ),
    ] {
        let mut compiler = Compiler::new();
        compiler.quiet = true;
        assert!(
            compiler.parse(source, "<prelude>"),
            "{}: {:?}",
            source,
            compiler.last_errors
        );
        assert!(!compiler.check(), "{}", source);
        assert!(
            compiler
                .last_type_errors
                .iter()
                .any(|e| e.message.contains(message)),
            "{:?}",
            compiler.last_errors
        );
        assert!(compiler
            .last_type_errors
            .iter()
            .all(|e| e.location.file == Name::str("<prelude>")
                && e.location.line == 1
                && e.location.col > 0));
        assert!(compiler.checked_program().is_none());
        assert!(compiler.specialize().is_err());
    }
}

#[test]
fn assumption_specialization_preserves_local_ownership_and_concrete_targets() {
    let mut compiler = Compiler::new();
    compiler.quiet = true;
    assert!(compiler.parse(
        r#"
        interface Positive<T> { positive(value: T) -> bool }
        positive(value: i32) -> bool { value >= 0 }
        positive(value: bool) -> bool { value }
        nested<T>(value: T) -> bool where Positive<T> { positive(value) }
        check_value<T>(value: T) -> bool where Positive<T> {
            let other = nested(true)
            positive(value) && other
        }
        sized<N>(values: [i32; N]) -> bool { N >= 1 }
        var cache<T>: [T; 2]
        assume {
            let values = [1, 2]
            check_value⟨i32⟩(1) && sized(values) && cache⟨i32⟩.len >= 2
        }
        main {}
    "#,
        "<prelude>"
    ));
    assert!(compiler.check(), "{:?}", compiler.last_errors);
    let templates = compiler.checked_program().unwrap().clone();
    let (source_body, source_root) = templates
        .decls
        .decls
        .iter()
        .find_map(|d| match d {
            Decl::Assume { arena, cond } => Some((arena.clone(), *cond)),
            _ => None,
        })
        .unwrap();
    compiler.specialize().unwrap();
    let output = compiler.specialized_program().unwrap();
    output.validate_origins(&templates).unwrap();
    let (body, root) = output
        .decls
        .decls
        .iter()
        .find_map(|d| match d {
            Decl::Assume { arena, cond } => Some((arena, *cond)),
            _ => None,
        })
        .unwrap();
    assert_eq!(root, source_root);
    assert_eq!(body.loc(root), source_body.loc(source_root));
    assert_eq!(body.ty(root), mk_type(Type::Bool));
    assert_eq!(body.locals, source_body.locals);
    let mut targets = vec![];
    for node in body.nodes() {
        if let Expr::Id(Reference::Instance(target)) = node.kind {
            targets.push(output.instances[target.index()].definition);
        }
    }
    for name in ["check_value", "sized", "cache"] {
        assert!(targets.contains(&templates.decls.named_ids(Name::str(name))[0]));
    }
    let sized = output
        .instances
        .iter()
        .find(|record| record.definition == templates.decls.named_ids(Name::str("sized"))[0])
        .unwrap();
    assert_eq!(sized.size_args, vec![2]);
    let checked = output
        .find_entry_point(Name::str("check_value$i32"))
        .unwrap();
    let positive = templates
        .decls
        .named_ids(Name::str("positive"))
        .into_iter()
        .find(|id| templates.function(*id).unwrap().param_types() == vec![mk_type(Type::Int32)])
        .unwrap();
    assert!(checked.arena.nodes().iter().any(|node| match node.kind {
        Expr::Id(Reference::Instance(target)) =>
            output.instances[target.index()].definition == positive,
        _ => false,
    }));
    assert!(body
        .nodes()
        .iter()
        .any(|node| matches!(node.kind, Expr::Id(Reference::Local(LocalId(0))))));
    let retained = compiler
        .checked_program()
        .unwrap()
        .decls
        .decls
        .iter()
        .find_map(|d| match d {
            Decl::Assume { arena, .. } => Some(arena),
            _ => None,
        })
        .unwrap();
    assert_eq!(retained, &source_body);
}

#[test]
fn assumption_local_facts_do_not_cross_into_other_bodies() {
    for source in [
        "var limit: i32\nassume limit >= { let proof = 1; proof }\nmain(divisor: i32) -> i32 { 10 / divisor }",
        "var limit: i32\nassume limit >= { let proof = 1; proof }\nassume limit >= { let divisor = 0; 10 / divisor }\nmain {}",
    ] {
        let mut compiler = Compiler::new();
        compiler.quiet = true;
        assert!(compiler.parse(source, "<prelude>"));
        assert!(!compiler.check());
        assert!(compiler.last_type_errors.is_empty(), "{:?}", compiler.last_errors);
        assert_eq!(compiler.last_safety_errors.len(), 1, "{:?}", compiler.last_errors);
        assert!(compiler.last_safety_errors[0].message.contains("zero"));
    }

    // Nonlocal facts still compose across assumptions and reach the function.
    let mut compiler = Compiler::new();
    compiler.quiet = true;
    assert!(compiler.parse("var divisor: i32\nvar limit: i32\nassume divisor >= 1\nassume limit >= { let proof = 0; proof }\nmain() -> i32 { 10 / divisor }", "<prelude>"));
    assert!(compiler.check(), "{:?}", compiler.last_errors);
    compiler.specialize().unwrap();
}

#[test]
fn safety_errors_in_assumptions_keep_the_expression_location() {
    let mut compiler = Compiler::new();
    compiler.quiet = true;
    assert!(compiler.parse(
        "var bound: i32\nassume bound >= 10 / 0\nmain {}",
        "<prelude>"
    ));
    let original_location = compiler
        .ast
        .last()
        .unwrap()
        .decls
        .iter()
        .find_map(|decl| match decl {
            Decl::Assume { arena, .. } => arena.exprs.iter().enumerate().find_map(|(id, expr)| {
                matches!(expr, Expr::Binop(Binop::Div, ..)).then_some(arena.locs[id])
            }),
            _ => None,
        })
        .unwrap();
    assert!(!compiler.check());
    assert!(compiler.last_type_errors.is_empty());
    let body = compiler
        .checked_program()
        .unwrap()
        .decls
        .decls
        .iter()
        .find_map(|d| match d {
            Decl::Assume { arena, .. } => Some(arena),
            _ => None,
        })
        .unwrap();
    let division = body
        .nodes()
        .iter()
        .find(|node| matches!(node.kind, Expr::Binop(Binop::Div, ..)))
        .unwrap();
    assert_eq!(compiler.last_safety_errors.len(), 1);
    assert_eq!(compiler.last_safety_errors[0].location, division.loc);
    assert_eq!(division.loc.file, Name::str("<prelude>"));
    assert_eq!(division.loc, original_location);
}
