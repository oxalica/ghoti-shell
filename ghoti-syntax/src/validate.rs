use std::mem;

use crate::visit::VisitorMut;
use crate::{ParseError, ParseErrorKind, Pos, SourceFile, Stmt, Word, visit};

pub(crate) fn validate_fixup(file: &mut SourceFile) -> Result<(), Vec<ParseError>> {
    let mut v = ValidateVisitor {
        in_loop: false,
        at_block_start: true,
        error: Vec::new(),
    };
    v.visit_source_file_mut(file);
    if v.error.is_empty() {
        Ok(())
    } else {
        Err(v.error)
    }
}

struct ValidateVisitor {
    in_loop: bool,
    at_block_start: bool,
    error: Vec<ParseError>,
}

impl ValidateVisitor {
    fn emit_err(&mut self, loc: Pos, kw: &'static str) {
        self.error.push(ParseError::new(
            loc as usize,
            loc as usize + kw.len(),
            ParseErrorKind::Validation(kw),
        ));
    }

    fn with_in_loop(&mut self, new: bool, f: impl FnOnce(&mut Self)) {
        let prev = mem::replace(&mut self.in_loop, new);
        f(self);
        self.in_loop = prev;
    }

    fn hoist_and_or(&mut self, cond: &mut Stmt, body: &mut Stmt) {
        let Stmt::Block(_, body) = body else { return };
        let pos = body
            .iter()
            .position(|s| !matches!(s, Stmt::And(..) | Stmt::Or(..)))
            .unwrap_or(body.len());
        if pos == 0 {
            return;
        }
        match cond {
            Stmt::Block(_, conds) => {
                conds.extend(body.drain(..pos));
            }
            cond_stmt => {
                let prev_loc = cond_stmt.pos();
                let prev_cond = mem::replace(cond_stmt, Stmt::Break(!0));
                let mut new_cond = Vec::with_capacity(1 + pos);
                new_cond.push(prev_cond);
                new_cond.extend(body.drain(..pos));
                *cond_stmt = Stmt::Block(prev_loc, new_cond);
            }
        }
    }
}

impl<'i> visit::VisitorMut<'i> for ValidateVisitor {
    fn visit_stmt_mut(&mut self, s: &'i mut Stmt) {
        visit::visit_stmt_mut(self, s);
        self.at_block_start = false;
    }

    fn visit_block_stmt_mut(&mut self, loc: Pos, stmts: &'i mut Vec<Stmt>) {
        self.at_block_start = true;
        visit::visit_block_stmt_mut(self, loc, stmts);
        self.at_block_start = false;
    }

    fn visit_if_stmt_mut(
        &mut self,
        loc: Pos,
        cond: &'i mut Stmt,
        then: &'i mut Stmt,
        else_: Option<&'i mut Stmt>,
    ) {
        self.hoist_and_or(cond, then);
        visit::visit_if_stmt_mut(self, loc, cond, then, else_);
    }

    fn visit_for_stmt_mut(
        &mut self,
        _loc: Pos,
        var: &'i mut Word,
        seq: &'i mut Vec<Word>,
        body: &'i mut Stmt,
    ) {
        visit::visit_word_mut(self, var);
        seq.iter_mut().for_each(|w| visit::visit_word_mut(self, w));
        self.with_in_loop(true, |this| visit::visit_stmt_mut(this, body));
    }

    fn visit_while_stmt_mut(&mut self, _loc: Pos, cond: &'i mut Stmt, body: &'i mut Stmt) {
        self.hoist_and_or(cond, body);
        visit::visit_stmt_mut(self, cond);
        self.with_in_loop(true, |this| visit::visit_stmt_mut(this, body));
    }

    fn visit_function_stmt_mut(&mut self, _loc: Pos, def: &'i mut Vec<Word>, body: &'i mut Stmt) {
        def.iter_mut().for_each(|w| visit::visit_word_mut(self, w));
        self.with_in_loop(false, |this| visit::visit_stmt_mut(this, body));
    }

    fn visit_continue_stmt_mut(&mut self, loc: Pos) {
        if !self.in_loop {
            self.emit_err(loc, "continue");
        }
    }

    fn visit_break_stmt_mut(&mut self, loc: Pos) {
        if !self.in_loop {
            self.emit_err(loc, "break");
        }
    }

    fn visit_and_stmt_mut(&mut self, loc: Pos, s: &'i mut Stmt) {
        if self.at_block_start {
            self.emit_err(loc, "and");
        } else {
            visit::visit_stmt_mut(self, s);
        }
    }

    fn visit_or_stmt_mut(&mut self, loc: Pos, s: &'i mut Stmt) {
        if self.at_block_start {
            self.emit_err(loc, "or");
        } else {
            visit::visit_stmt_mut(self, s);
        }
    }
}
