#![expect(clippy::ptr_arg, reason = "Follow original type to use `&mut Vec<_>`")]
use crate::{Redirect, RedirectDest, RedirectPort, SourceFile, Stmt, SwitchCase, Word, WordFrag};

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
            Stmt::Command(ws) => v.visit_command_stmt_mut(ws),
            Stmt::Block(b) => v.visit_block_stmt_mut(b),
            Stmt::If(cond, then, else_) => v.visit_if_stmt_mut(cond, then, else_.as_deref_mut()),
            Stmt::For(var, seq, body) => v.visit_for_stmt_mut(var, seq, body),
            Stmt::While(cond, body) => v.visit_while_stmt_mut(cond, body),
            Stmt::Break => v.visit_break_stmt_mut(),
            Stmt::Continue => v.visit_continue_stmt_mut(),
            Stmt::Function(def, body) => v.visit_function_stmt_mut(def, body),
            Stmt::Return(w) => v.visit_return_stmt_mut(w.as_mut()),
            Stmt::Switch(testee, cases) => v.visit_switch_stmt_mut(testee, cases),
            Stmt::Redirect(s, redirects) => v.visit_redirect_stmt_mut(s, redirects),
            Stmt::Pipe(port, lhs, rhs) => v.visit_pipe_stmt_mut(port, lhs, rhs),
            Stmt::Not(s) => v.visit_not_stmt_mut(s),
            Stmt::And(s) => v.visit_and_stmt_mut(s),
            Stmt::Or(s) => v.visit_or_stmt_mut(s),
        }
    }

    fn visit_command_stmt_mut(v, ws: &'i mut Vec<Word>) {
        ws.iter_mut().for_each(|w| v.visit_word_mut(w));
    }

    fn visit_block_stmt_mut(v, stmts: &'i mut Vec<Stmt>) {
        stmts.iter_mut().for_each(|w| v.visit_stmt_mut(w));
    }

    fn visit_if_stmt_mut(v, cond: &'i mut Stmt, then: &'i mut Stmt, else_: Option<&'i mut Stmt>) {
        v.visit_stmt_mut(cond);
        v.visit_stmt_mut(then);
        if let Some(else_) = else_ {
            v.visit_stmt_mut(else_);
        }
    }

    fn visit_for_stmt_mut(v, var: &'i mut Word, seq: &'i mut Vec<Word>, body: &'i mut Stmt) {
        v.visit_word_mut(var);
        seq.iter_mut().for_each(|w| v.visit_word_mut(w));
        v.visit_stmt_mut(body);
    }

    fn visit_while_stmt_mut(v, cond: &'i mut Stmt, body: &'i mut Stmt) {
        v.visit_stmt_mut(cond);
        v.visit_stmt_mut(body);
    }

    fn visit_break_stmt_mut(_v) {}

    fn visit_continue_stmt_mut(_v) {}

    fn visit_function_stmt_mut(v, def: &'i mut Vec<Word>, body: &'i mut Stmt) {
        def.iter_mut().for_each(|w| v.visit_word_mut(w));
        v.visit_stmt_mut(body);
    }

    fn visit_return_stmt_mut(v, arg: Option<&'i mut Word>) {
        if let Some(arg) = arg {
            v.visit_word_mut(arg);
        }
    }

    fn visit_switch_stmt_mut(v, testee: &'i mut Word, cases: &'i mut Vec<SwitchCase>) {
        v.visit_word_mut(testee);
        cases.iter_mut().for_each(|case| v.visit_switch_case_mut(case));
    }

    fn visit_switch_case_mut(v, case: &'i mut SwitchCase) {
        case.globs.iter_mut().for_each(|w| v.visit_word_mut(w));
        v.visit_stmt_mut(&mut case.body);
    }

    fn visit_redirect_stmt_mut(v, s: &'i mut Stmt, redirects: &'i mut Vec<Redirect>) {
        v.visit_stmt_mut(s);
        redirects.iter_mut().for_each(|redir| v.visit_redirect_mut(redir));
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

    fn visit_pipe_stmt_mut(v, port: &'i mut RedirectPort, lhs: &'i mut Stmt, rhs: &'i mut Stmt) {
        v.visit_redirect_port_mut(port);
        v.visit_stmt_mut(lhs);
        v.visit_stmt_mut(rhs);
    }

    fn visit_not_stmt_mut(v, s: &'i mut Stmt) {
        v.visit_stmt_mut(s);
    }

    fn visit_and_stmt_mut(v, s: &'i mut Stmt) {
        v.visit_stmt_mut(s);
    }

    fn visit_or_stmt_mut(v, s: &'i mut Stmt) {
        v.visit_stmt_mut(s);
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
            WordFrag::TildeSegment |
            WordFrag::Wildcard |
            WordFrag::WildcardRecursive => {}
            WordFrag::Command(s) |
            WordFrag::CommandNoSplit(s) => v.visit_stmt_mut(s),
            WordFrag::Brace(ws) => ws.iter_mut().for_each(|w| v.visit_word_mut(w)),
        }
    }
}
