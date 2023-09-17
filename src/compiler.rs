use crate::vm::Tape;

pub enum Expr {
    Literal(i64),
    Add(Box<Expr>, Box<Expr>),
}

pub fn compile(expr: &Expr, tape: &mut Tape) {
    compile_expr(expr, tape);
    tape.push_exit();
}

pub fn compile_expr(expr: &Expr, tape: &mut Tape) {
    match expr {
        Expr::Literal(x) => tape.push_op((*x,), #[inline(always)] |(x,), tape, regs, stack| {
            stack.push::<_, false>(x);
        }),
        Expr::Add(x, y) => {
            compile_expr(x, tape);
            compile_expr(y, tape);
            tape.push_op((), #[inline(always)] |(), tape, regs, stack| unsafe {
                let x = stack.pop::<u64>();
                let y = stack.pop::<u64>();
                stack.push::<_, false>(x + y);
            })
        },
    }
}
