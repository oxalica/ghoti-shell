use std::cell::RefCell;
use std::mem;
use std::rc::Rc;

use ghoti_syntax::parse_source;

use crate::{ExecContext, Executor, Status, Stdio};

#[tokio::test]
async fn var_scopes() {
    let exec = Executor::default();
    let mut ctx = ExecContext::new(&exec);
    let buf = Rc::new(RefCell::new(String::new()));
    let buf2 = Rc::clone(&buf);
    ctx.io.stdout = Stdio::Collect(Rc::new(move |bytes| {
        buf2.borrow_mut()
            .push_str(std::str::from_utf8(bytes).unwrap());
        Ok(Status::SUCCESS)
    }));

    let mut run = async |src: &str| {
        let src = parse_source(src).unwrap();
        ctx.exec_source(&src).await;
        let ret = mem::take(&mut *buf.borrow_mut());
        ret
    };

    assert_eq!(
        run("
            set -l a 0
            set -f a 1
            set -g a 2
            echo $a
            set -e a
            echo $a
        ")
        .await,
        "1\n2\n",
    );

    assert_eq!(
        run("
            set -g b 0
            set -l a 1
            begin
                set -l b 2
                begin
                    set -l a 3
                    begin
                        echo $a$b
                    end
                end
                echo $a$b
            end
            echo $a$b
        ")
        .await,
        "32\n12\n10\n",
    );
}
