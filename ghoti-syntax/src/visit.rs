#![expect(clippy::ptr_arg, reason = "Follow original type to use `&mut Vec<_>`")]
use crate::{
    Pos, Redirect, RedirectDest, RedirectPort, SourceFile, Stmt, SwitchCase, Word, WordFrag,
};

macro_rules! define_visitor {
    (
        $i:lifetime;
        $(
            fn $func:ident ($v:ident $(,$arg:ident : $arg_ty:ty)* $(,)?)
            $func_body:block
        )*
    ) => {
        pub trait VisitorMut<$i> {
            $(
                fn $func(&mut self, $($arg : $arg_ty),*) {
                    $func(self, $($arg),*);
                }
            )*
        }

        $(
            pub fn $func<$i, V: VisitorMut<$i> + ?Sized>($v: &mut V, $($arg : $arg_ty),*)
            $func_body
        )*
    };
}

define_visitor! {
    'i;

    fn visit_source_file_mut(v, file: &'i mut SourceFile) {
        file.stmts.iter_mut().for_each(|s| v.visit_stmt_mut(s));
    }

    fn visit_stmt_mut(v, s: &'i mut Stmt) {
        match s {
            Stmt::Command(pos, ws) => v.visit_command_stmt_mut(*pos, ws),
            Stmt::Block(pos, b) => v.visit_block_stmt_mut(*pos, b),
            Stmt::If(pos, cond, then, else_) => v.visit_if_stmt_mut(*pos, cond, then, else_.as_deref_mut()),
            Stmt::For(pos, var, seq, body) => v.visit_for_stmt_mut(*pos, var, seq, body),
            Stmt::While(pos, cond, body) => v.visit_while_stmt_mut(*pos, cond, body),
            Stmt::Break(pos) => v.visit_break_stmt_mut(*pos),
            Stmt::Continue(pos) => v.visit_continue_stmt_mut(*pos),
            Stmt::Function(pos, def, body) => v.visit_function_stmt_mut(*pos, def, body),
            Stmt::Return(pos, w) => v.visit_return_stmt_mut(*pos, w.as_mut()),
            Stmt::Switch(pos, testee, cases) => v.visit_switch_stmt_mut(*pos, testee, cases),
            Stmt::Redirect(pos, s, redirects) => v.visit_redirect_stmt_mut(*pos, s, redirects),
            Stmt::Pipe(pos, pipes, rhs) => v.visit_pipe_stmt_mut(*pos, pipes, rhs),
            Stmt::Not(pos, s) => v.visit_not_stmt_mut(*pos, s),
            Stmt::And(pos, s) => v.visit_and_stmt_mut(*pos, s),
            Stmt::Or(pos, s) => v.visit_or_stmt_mut(*pos, s),
        }
    }

    fn visit_command_stmt_mut(v, _pos: Pos, ws: &'i mut Vec<Word>) {
        ws.iter_mut().for_each(|w| v.visit_word_mut(w));
    }

    fn visit_block_stmt_mut(v, _pos: Pos, stmts: &'i mut Vec<Stmt>) {
        stmts.iter_mut().for_each(|w| v.visit_stmt_mut(w));
    }

    fn visit_if_stmt_mut(v, _pos: Pos, cond: &'i mut Stmt, then: &'i mut Stmt, else_: Option<&'i mut Stmt>) {
        v.visit_stmt_mut(cond);
        v.visit_stmt_mut(then);
        if let Some(else_) = else_ {
            v.visit_stmt_mut(else_);
        }
    }

    fn visit_for_stmt_mut(v, _pos: Pos, var: &'i mut Word, seq: &'i mut Vec<Word>, body: &'i mut Stmt) {
        v.visit_word_mut(var);
        seq.iter_mut().for_each(|w| v.visit_word_mut(w));
        v.visit_stmt_mut(body);
    }

    fn visit_while_stmt_mut(v, _pos: Pos, cond: &'i mut Stmt, body: &'i mut Stmt) {
        v.visit_stmt_mut(cond);
        v.visit_stmt_mut(body);
    }

    fn visit_break_stmt_mut(_v, _pos: Pos) {}

    fn visit_continue_stmt_mut(_v, _pos: Pos) {}

    fn visit_function_stmt_mut(v, _pos: Pos, def: &'i mut Vec<Word>, body: &'i mut Stmt) {
        def.iter_mut().for_each(|w| v.visit_word_mut(w));
        v.visit_stmt_mut(body);
    }

    fn visit_return_stmt_mut(v, _pos: Pos, arg: Option<&'i mut Word>) {
        if let Some(arg) = arg {
            v.visit_word_mut(arg);
        }
    }

    fn visit_switch_stmt_mut(v, _pos: Pos, testee: &'i mut Word, cases: &'i mut Vec<SwitchCase>) {
        v.visit_word_mut(testee);
        cases.iter_mut().for_each(|case| v.visit_switch_case_mut(case));
    }

    fn visit_redirect_stmt_mut(v, _pos: Pos, s: &'i mut Stmt, redirects: &'i mut Vec<Redirect>) {
        v.visit_stmt_mut(s);
        redirects.iter_mut().for_each(|redir| v.visit_redirect_mut(redir));
    }

    fn visit_pipe_stmt_mut(v, _pos: Pos, pipes: &'i mut Vec<(Stmt, RedirectPort)>, rhs: &'i mut Stmt) {
        for (lhs, port) in pipes {
            v.visit_stmt_mut(lhs);
            v.visit_redirect_port_mut(port);
        }
        v.visit_stmt_mut(rhs);
    }

    fn visit_not_stmt_mut(v, _pos: Pos, s: &'i mut Stmt) {
        v.visit_stmt_mut(s);
    }

    fn visit_and_stmt_mut(v, _pos: Pos, s: &'i mut Stmt) {
        v.visit_stmt_mut(s);
    }

    fn visit_or_stmt_mut(v, _pos: Pos, s: &'i mut Stmt) {
        v.visit_stmt_mut(s);
    }

    fn visit_switch_case_mut(v, case: &'i mut SwitchCase) {
        case.globs.iter_mut().for_each(|w| v.visit_word_mut(w));
        v.visit_stmt_mut(&mut case.body);
    }

    fn visit_redirect_mut(v, redirect: &'i mut Redirect) {
        v.visit_redirect_port_mut(&mut redirect.port);
        v.visit_redirect_dest_mut(&mut redirect.dest);
    }

    fn visit_redirect_port_mut(_v, _port: &'i mut RedirectPort) {}

    fn visit_redirect_dest_mut(v, dest: &'i mut RedirectDest) {
        let (RedirectDest::File(w)| RedirectDest::Fd(w)) = dest;
        v.visit_word_mut(w);
    }


    fn visit_word_mut(v, w: &'i mut Word) {
        match w {
            Word::Simple(_) => {}
            Word::Complex(frags) => frags.iter_mut().for_each(|f| v.visit_word_frag_mut(f)),
        }
    }

    fn visit_word_frag_mut(v, frag: &'i mut WordFrag) {
        match frag {
            WordFrag::Literal(_) |
            WordFrag::Variable(_) |
            WordFrag::VariableNoSplit(_) |
            WordFrag::Home { .. } |
            WordFrag::Wildcard |
            WordFrag::WildcardRecursive => {}
            WordFrag::Command(s) |
            WordFrag::CommandNoSplit(s) => v.visit_stmt_mut(s),
            WordFrag::Brace(ws) => ws.iter_mut().for_each(|w| v.visit_word_mut(w)),
        }
    }
}
