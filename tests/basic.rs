use yurt::{
    vm::{Tape, Stack, State, Runtime},
    compiler::{Expr, compile},
};

#[test]
fn basic() {
    let mut tape = Tape::default();
    compile(
        &Expr::Add(
            Box::new(Expr::Literal(3)),
            Box::new(Expr::Literal(5)),
        ),
        &mut tape,
    );

    let mut stack = Stack::new(1024);
    let mut rt = Runtime::default();
    unsafe {
        tape.ptr().exec_at(0, State::init(&mut rt), stack.ptr());
        assert_eq!(rt.last_stack_ptr().unwrap().pop::<u64>(), 8);
    }
}
