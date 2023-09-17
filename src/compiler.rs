use crate::{
    vm::{Tape, Data, util::AsRepr},
    api::{Module, ModuleBuilder, Func, FuncInfo, Expr, Repr},
};

impl Expr {
    pub fn compile(self, tape: &mut Tape, locals: &mut Vec<(usize, Repr, usize)>, mut stack_depth: usize) -> Result<(), String> {
        fn get_local(local: usize, locals: &[(usize, Repr, usize)]) -> &(usize, Repr, usize) {
                locals
                .iter()
                .rev()
                .find(|(l, _, _)| *l == local)
                .expect("no such local")
        }

        fn compile_binary<A: Data + AsRepr, B: Data + AsRepr, C: Data + AsRepr>(
            a: Expr,
            b: Expr,
            f: impl Fn(A, B) -> C + Copy,
            tape: &mut Tape,
            locals: &mut Vec<(usize, Repr, usize)>,
            mut stack_depth: usize,
        ) -> Result<(), String>
            where
                [(); A::BYTES]:,
                [(); B::BYTES]:,
                [(); C::BYTES]:,
        {
            let a_repr = a.derive_repr(&|l| get_local(l, locals).1.clone());
            let b_repr = b.derive_repr(&|l| get_local(l, locals).1.clone());
            assert_eq!(a_repr, A::repr());
            assert_eq!(b_repr, B::repr());

            a.compile(tape, locals, stack_depth)?;
            stack_depth += a_repr.size();
            b.compile(tape, locals, stack_depth)?;
            stack_depth += b_repr.size();
            tape.push_op((), #[inline(always)] move |(), tape, regs, stack| unsafe {
                let x = stack.pop::<A>();
                let y = stack.pop::<B>();
                stack.push::<_, false>(f(x, y));
            });
            stack_depth -= a_repr.size() + b_repr.size();
            stack_depth += C::repr().size();
            Ok(())
        }

        fn push_copy_to_top(offset: usize, sz: usize, tape: &mut Tape) {
            match (offset, sz) {
                (0, 8) => tape.push_op((), #[inline(always)] |(), tape, regs, stack| unsafe { stack.copy_to_top(0, 8) }),
                (offset, 8) => tape.push_op((offset,), #[inline(always)] |(offset,), tape, regs, stack| unsafe { stack.copy_to_top(offset, 8) }),
                (offset, 16) => tape.push_op((offset,), #[inline(always)] |(offset,), tape, regs, stack| unsafe { stack.copy_to_top(offset, 16) }),
                (offset, sz) => tape.push_op((offset, sz), #[inline(always)] |(offset, sz), tape, regs, stack| unsafe { stack.copy_to_top(offset, sz) }),
            }
        }

        Ok(match self {
            Expr::Local(local) => {
                let (_, repr, local_depth) = get_local(local, locals);
                let offset = stack_depth - local_depth;
                push_copy_to_top(offset, repr.size(), tape);
            },
            Expr::Add(a, b) => compile_binary(*a, *b, |a: u64, b: u64| a + b, tape, locals, stack_depth)?,
            Expr::Mul(a, b) => compile_binary(*a, *b, |a: u64, b: u64| a * b, tape, locals, stack_depth)?,
            Expr::FieldAccess(x, key) => {
                let Repr::Tuple(fields) = x.derive_repr(&|l| get_local(l, locals).1.clone()) else {
                    panic!("Cannot access field of non-tuple");
                };

                x.compile(tape, locals, stack_depth)?;

                let offset = fields[..key].iter().map(|f| f.size()).sum();
                let sz = fields[key].size();
                push_copy_to_top(offset, sz, tape);
            },
            expr => todo!("{expr:?}"),
        })
    }
}

impl Func {
    pub fn compile(self, tape: &mut Tape) -> Result<FuncInfo, String> {
        let addr = tape.next_addr();
        self.body.compile(tape, &mut vec![(0, self.input.clone(), 0)], 0)?;
        tape.push_exit();
        Ok(FuncInfo {
            input: self.input,
            output: self.output,
            addr,
        })
    }
}

impl ModuleBuilder {
    pub fn compile(self) -> Result<Module, String> {
        let mut tape = Tape::default();

        let funcs = self.funcs
            .into_iter()
            .map(|func| func.compile(&mut tape))
            .collect::<Result<_, _>>()?;

        Ok(Module { tape, funcs })
    }
}
