use super::*;

pub type TapeFn = for<'rt, 'tape, 'stack> fn(TapePtr<'tape>, State<'rt, 'tape, 'stack>, StackPtr<'stack>);

#[derive(Copy, Clone)]
pub struct TapeOffset(usize);

#[derive(Default)]
pub struct Tape {
    // `MaybeUninit` is used because it may carry provenance, which is important for function pointers
    // See [https://github.com/rust-lang/miri/issues/2926]
    code: Vec<MaybeUninit<u8>>,
}

impl Tape {
    pub fn push<T: Data>(&mut self, data: T) -> TapeOffset where [(); T::BYTES]: {
        let offset = TapeOffset(self.code.len());
        self.code.extend_from_slice(data.to_bytes().as_ref());
        offset
    }

    pub fn ptr(&self) -> TapePtr {
        TapePtr(self.code.as_ptr(), PhantomData)
    }

    #[track_caller]
    pub fn push_op<A, F>(&mut self, args: A, _f: F)
    where
        A: Args,
        F: for<'rt, 'tape, 'stack> Fn(A, &mut TapePtr<'tape>, &mut State<'rt, 'tape, 'stack>, &mut StackPtr<'stack>) + Copy,
    {
        fn perform<'rt, 'tape, 'stack, A, F>(mut tape: TapePtr<'tape>, mut state: State<'rt, 'tape, 'stack>, mut stack: StackPtr<'stack>)
        where
            A: Args,
            F: for<'rt1, 'tape1, 'stack1> Fn(A, &mut TapePtr<'tape1>, &mut State<'rt1, 'tape1, 'stack1>, &mut StackPtr<'stack1>) + Copy,
        {
            let f = unsafe { core::mem::transmute::<_, &F>(&()) };

            let args = unsafe { A::from_tape(&mut tape) };

            f(args, &mut tape, &mut state, &mut stack);

            become unsafe { tape.read::<TapeFn>()(tape, state, stack) }
        }

        // Doesn't work yet :(
        // const {
        //     if core::mem::size_of::<T>() != 0 {
        //         panic!("Can only perform operations that have no environment");
        //     }
        // }

        trait NoCapture: Sized {
            const ASSERT: () = assert!(core::mem::size_of::<Self>() == 0, "Can only perform operations that have no environment");
        }

        impl<F> NoCapture for F {}

        #[allow(clippy::let_unit_value)]
        let _ = <F as NoCapture>::ASSERT;

        self.push(perform::<A, F> as TapeFn);
        args.push(self);
    }

    pub fn push_exit(&mut self) {
        self.push((|tape, state, stack| {
            state.rt.save_state(tape, state.regs, stack);
        }) as TapeFn);
    }
}

pub trait Args {
    fn push(self, tape: &mut Tape);
    /// Tape data at the pointer head must be valid for these args in sequence.
    unsafe fn from_tape(tape: &mut TapePtr) -> Self;
}

macro_rules! impl_args_for_tuple {
    () => { impl_args_for_tuple!(~); };
    ($head:ident $($X:ident)*) => {
        impl_args_for_tuple!($($X)*);
        impl_args_for_tuple!(~ $head $($X)*);
    };
    (~ $($X:ident)*) => {
        #[allow(unused_variables, non_snake_case)]
        impl<$($X: Data),*> Args for ($($X,)*) where $([(); $X::BYTES]:,)* {
            fn push(self, tape: &mut Tape) {
                let ($($X,)*) = self;
                $(tape.push($X);)*
            }
            unsafe fn from_tape(tape: &mut TapePtr) -> Self {
                ($($X::from_bytes(*tape.read_bytes()),)*)
            }
        }
    };
}

impl_args_for_tuple!(A_ B_ C_ D_ E_ F_);

#[repr(transparent)]
pub struct TapePtr<'tape>(*const MaybeUninit<u8>, PhantomData<&'tape Tape>);

impl<'tape> TapePtr<'tape> {
    /// SAFETY: Offset must be valid for tape.
    #[inline(always)]
    pub unsafe fn jump_rel(&mut self, offset: isize) {
        self.0 = self.0.offset(offset);
    }
    /// SAFETY: `TapePtr` must have at least `N` bytes to read left.
    #[inline(always)]
    pub unsafe fn read_bytes<const N: usize>(&mut self) -> &'tape [MaybeUninit<u8>; N] {
        let r = &*(self.0 as *const [MaybeUninit<u8>; N]);
        self.0 = self.0.offset(N as isize);
        r
    }

    /// SAFETY: `TapePtr` must have data exactly corresponding to this type.
    #[inline(always)]
    pub unsafe fn read<T: Data>(&mut self) -> T
        where [(); T::BYTES]:
    { T::from_bytes(*self.read_bytes()) }

    #[inline(always)]
    pub unsafe fn exec_at<'rt, 'stack>(&mut self, offset: isize, state: State<'rt, 'tape, 'stack>, stack: StackPtr<'stack>) {
        let mut tape = Self(self.0.offset(offset), PhantomData);
        tape.read::<TapeFn>()(tape, state, stack);
    }
}
