use crate::{ParseErrorKind, Stmt, Word, WordFrag, parse_source};

#[test]
fn smoke() {
    let src = r#"
        #!shebang
        echo hello$(world) 2>>o <i | cat; or true
        not $true; and "th$$$$is\$"; or that
    "#;
    let ast = parse_source(src).unwrap();
    println!("{ast:?}");
}

#[test]
fn hoist_and_or() {
    let src = r"
        if true
        and false
            echo yes
        end
    ";
    let ast = parse_source(src).unwrap();
    println!("{ast:?}");
    assert!(matches!(
        &ast.stmts[0],
        Stmt::If(_, cond, _, _)
        if matches!(
            &**cond,
            Stmt::Block(_, b)
            if matches!(b[1], Stmt::And(..))
        )
    ));

    let src = r"
        while true
        or false
        end
    ";
    let ast = parse_source(src).unwrap();
    println!("{ast:?}");
    assert!(matches!(
        &ast.stmts[0],
        Stmt::While(_, cond, body)
        if matches!(
            &**cond,
            Stmt::Block(_, b)
            if matches!(b[1], Stmt::Or(..))
        )
        && matches!(&**body, Stmt::Block(_, b) if b.is_empty())
    ));
}

#[test]
fn invalid_and_or() {
    let src = r"
        and true
        function foo
            or true
        end
    ";
    let errs = parse_source(src).unwrap_err();
    assert_eq!(errs[0].kind, ParseErrorKind::Validation("and"));
    assert_eq!(errs[1].kind, ParseErrorKind::Validation("or"));
}

#[test]
fn invalid_break_continue() {
    let src = r"
        if true
            break
        end
        while true
            function foo
                continue
            end
        end
    ";
    let errs = parse_source(src).unwrap_err();
    assert_eq!(errs[0].kind, ParseErrorKind::Validation("break"));
    assert_eq!(errs[1].kind, ParseErrorKind::Validation("continue"));

    let src = r"
        for x in;
            if false
                continue
                function bar
                    while false
                        break
                    end
                end
            end
        end
    ";
    parse_source(src).unwrap();
}

#[test]
fn home() {
    let src = "~ ~/a";
    let ast = parse_source(src).unwrap();
    assert!(matches!(
        &ast.stmts[0],
        Stmt::Command(_, ws)
        if matches!(
            &ws[..],
            [Word::Complex(a), Word::Complex(b)]
            if matches!(&a[..], [WordFrag::Home { slash: false }])
            && matches!(&b[..], [WordFrag::Home { slash: true }, _])
        )
    ));

    parse_source("echo ~a").unwrap_err();
    parse_source("echo a~").unwrap_err();
}

#[test]
fn pipe() {
    let src = "1 | 2 2>| 3 &| 4";
    let ast = parse_source(src).unwrap();
    assert!(matches!(
        &ast.stmts[0],
        Stmt::Pipe(_, pipes, _)
        if pipes.len() == 3
    ));
}

#[test]
fn unknow_escape() {
    let src = r#"'\d'"#;
    let ast = parse_source(src).unwrap();
    assert!(matches!(
        &ast.stmts[0],
        Stmt::Command(_, ws)
        if matches!(
            &ws[..],
            // FIXME: Should simplify this.
            [Word::Complex(frags)]
            if frags[0] == WordFrag::Literal(r"\d".into())
        )
    ));
}
