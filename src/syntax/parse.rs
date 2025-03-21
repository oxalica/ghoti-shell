use chumsky::Parser as _;
use chumsky::combinator::Repeated;
use chumsky::error::Rich;
use chumsky::prelude::*;
use chumsky::text::keyword;

use super::*;

const KEYWORDS: &[&str] = &[
    "begin", "end", "if", "else", "switch", "case", "for", "in", "and", "or", "not",
];

type ParseErr<'i> = extra::Err<Rich<'i, char>>;

trait Parser<'i, T>: chumsky::Parser<'i, &'i str, T, ParseErr<'i>> + Clone {}
impl<'i, T, P> Parser<'i, T> for P where
    P: chumsky::Parser<'i, &'i str, T, extra::Err<Rich<'i, char>>> + Clone
{
}

pub fn source_file<'i>() -> impl chumsky::Parser<'i, &'i str, SourceFile, ParseErr<'i>> {
    let (stmt, list) = stmt_and_list();
    list.then(stmt.or_not())
        .then_ignore(end())
        .map(|(mut stmts, s)| {
            stmts.extend(s);
            SourceFile { stmts }
        })
}

fn sp<'i>() -> Repeated<impl Parser<'i, ()>, (), &'i str, ParseErr<'i>> {
    just(' ')
        .or(just('#').then_ignore(any().and_is(none_of("\n")).repeated()))
        .ignored()
        .labelled("space")
        .repeated()
        .at_least(1)
}

fn eos<'i>() -> impl Parser<'i, ()> {
    choice((
        just("\n").ignored(),
        just("\r\n").ignored(),
        just(';').ignored(),
    ))
    .labelled("end of statement")
}

fn stmt_and_list<'i>() -> (impl Parser<'i, Stmt>, impl Parser<'i, Vec<Stmt>>) {
    let mut stmt = Recursive::declare();

    let stmt_list = empty()
        .to(Vec::new())
        .foldl(
            stmt.clone()
                .or_not()
                .padded_by(sp().at_least(0))
                .then_ignore(eos())
                .repeated(),
            |mut v, s| {
                v.extend(s);
                v
            },
        )
        .labelled("statements")
        .boxed();
    let stmt_block = stmt_list.clone().map(Stmt::Block);

    // Atomic statements.

    let block_stmt = stmt_block
        .clone()
        .delimited_by(keyword("begin").then(eos()), keyword("end"));

    let if_stmt = recursive(|if_stmt| {
        keyword("if")
            .ignore_then(stmt.clone())
            .then_ignore(eos())
            .then(stmt_block.clone())
            .then(choice((
                keyword("end").to(None),
                keyword("else").ignore_then(if_stmt.clone()).map(Some),
            )))
            .map(|((cond, then), else_)| {
                Stmt::If(Box::new(cond), Box::new(then), else_.map(Box::new))
            })
    })
    .labelled("if statement");

    let while_stmt = keyword("while")
        .ignore_then(stmt.clone())
        .then_ignore(eos())
        .then(stmt_block.clone())
        .then_ignore(keyword("end"))
        .map(|(cond, body)| Stmt::While(Box::new(cond), Box::new(body)))
        .labelled("while statement");

    let for_stmt = keyword("for")
        .ignore_then(word())
        .then_ignore(keyword("in"))
        .then(word().repeated().collect::<Vec<_>>())
        .then_ignore(eos())
        .then(stmt_block.clone())
        .then_ignore(keyword("end"))
        .map(|((var, list), body)| Stmt::For(var, list, Box::new(body)))
        .labelled("for statement");

    let function_stmt = keyword("function")
        .ignore_then(word().repeated().at_least(1).collect::<Vec<_>>())
        .then_ignore(eos())
        .then(stmt_block.clone())
        .then_ignore(keyword("end"))
        .map(|(def, body)| Stmt::Function(def, Box::new(body)))
        .labelled("function statement");

    let command_stmt = word()
        .then_ignore(none_of("<>|").ignored().rewind().or(end()))
        .separated_by(sp())
        .at_least(1)
        .collect::<Vec<_>>()
        .map(Stmt::Command)
        .labelled("command");

    let atom_stmt = choice((
        if_stmt,
        block_stmt,
        while_stmt,
        for_stmt,
        function_stmt,
        command_stmt,
    ))
    .boxed();

    // Redirections.

    let redirect_mode = choice((
        just(">>").to(RedirectMode::Append),
        just("<?").to(RedirectMode::ReadOrNull),
        just(">?").to(RedirectMode::WriteNoClobber),
        just("<").to(RedirectMode::Read),
        just(">").to(RedirectMode::Write),
    ));
    let redirect_dest = choice((
        just("&").ignore_then(word()).map(RedirectDest::Fd),
        word().map(RedirectDest::File),
    ));
    let redirects = sp()
        .ignore_then(text::digits(10).to_slice().or_not())
        .then(redirect_mode)
        .then(redirect_dest)
        .try_map(|((port, mode), dest), span| {
            let port = match port {
                None => mode.default_port(),
                Some(port) => port
                    .parse::<RedirectPort>()
                    .map_err(|err| Rich::custom(span, err))?,
            };
            Ok(Redirect { port, mode, dest })
        })
        .labelled("redirection")
        .repeated()
        .at_least(1)
        .collect::<Vec<Redirect>>();

    let pipe_op = choice((
        just("&|").to(RedirectPort::STDOUT_STDERR),
        just("|").to(RedirectPort::STDOUT),
        text::digits(10)
            .to_slice()
            .then_ignore(just(">|"))
            .try_map(|fd: &str, span| fd.parse().map_err(|err| Rich::custom(span, err))),
    ))
    .labelled("pipe operator")
    .padded_by(sp());

    stmt.define({
        use chumsky::pratt::*;

        atom_stmt
            .pratt((
                postfix(5, redirects, |s, redirs, _| {
                    Stmt::Redirect(Box::new(s), redirs)
                }),
                prefix(
                    4,
                    choice((just("!"), keyword("not"))).then(sp()),
                    |_, s, _| Stmt::Not(Box::new(s)),
                ),
                infix(right(3), pipe_op, |l, port, r, _| {
                    Stmt::Pipe(port, Box::new(l), Box::new(r))
                }),
                infix(
                    right(2),
                    choice((just("&&"), keyword("and"))).padded_by(sp()),
                    |l, _, r, _| Stmt::And(Box::new(l), Box::new(r)),
                ),
                infix(
                    right(1),
                    choice((just("||"), keyword("or"))).padded_by(sp()),
                    |l, _, r, _| Stmt::Or(Box::new(l), Box::new(r)),
                ),
            ))
            .labelled("statement pipeline")
    });

    (stmt, stmt_list)
}

fn verbatim_word<'i>() -> impl Parser<'i, &'i str> {
    any()
        .filter(|c: &char| {
            c.is_alphanumeric()
                || matches!(c, '%' | '+' | ',' | '-' | '.' | '/' | ':' | '@' | '_' | '^')
        })
        .repeated()
        .at_least(1)
        .to_slice()
        .filter(|w: &&str| !KEYWORDS.contains(w))
        .labelled("verbatim word")
}

fn word<'i>() -> impl Parser<'i, Word> {
    verbatim_word().map(|w| Word::Simple(w.to_owned()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(src: &str) -> anyhow::Result<SourceFile> {
        let (ast, errs) = super::source_file().parse(src).into_output_errors();
        assert_eq!(errs, []);
        Ok(ast.unwrap())
    }

    #[test]
    fn smoke() {
        let ast = parse(
            r"
#!shebang
echo hello world 2>>o <i | cat || true
not true && this or that
",
        )
        .unwrap();
        println!("{ast:?}");
    }
}
