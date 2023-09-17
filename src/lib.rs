#![feature(
    explicit_tail_calls,
    maybe_uninit_uninit_array_transpose,
    maybe_uninit_array_assume_init,
    generic_const_exprs,
    inline_const,
)]
#![allow(incomplete_features, unused_mut, unused_variables)]

pub mod vm;
pub mod compiler;

use core::{
    marker::PhantomData,
    mem::MaybeUninit,
};

// pub unsafe fn cont(mut tape: TapePtr, mut regs: Regs, mut stack: StackPtr) {
//     become tape.read::<TapeFn>()(tape, regs, stack)
// }

// pub unsafe fn add_reg(mut tape: TapePtr, mut regs: Regs, mut stack: StackPtr) {
//     regs.0 += regs.1;

//     become tape.read::<TapeFn>()(tape, regs, stack)
// }

// pub unsafe fn add(mut tape: TapePtr, mut regs: Regs, mut stack: StackPtr) {
//     let a = stack.pop::<u64>();
//     let b = stack.pop::<u64>();
//     stack.push::<_, false>(a + b);

//     become tape.read::<TapeFn>()(tape, regs, stack)
// }

// pub fn ret(tape: TapePtr, regs: Regs, stack: StackPtr) {}

// pub unsafe fn branch(mut tape: TapePtr, mut regs: Regs, mut stack: StackPtr) {
//     let offset = tape.read::<isize>();
//     if stack.read_at::<u64>(0) > 0 {
//         tape.jump_rel(offset as isize);
//     }
//     become tape.read::<TapeFn>()(tape, regs, stack);
// }

// pub unsafe fn skip_if(mut tape: TapePtr, mut regs: Regs, mut stack: StackPtr) {
//     if stack.read_at::<u64>(0) > 0 {
//         tape.read::<TapeFn>();
//     }
//     become tape.read::<TapeFn>()(tape, regs, stack);
// }
