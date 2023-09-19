use super::*;

pub type TapeFn = for<'rt, 'tape, 'stack> fn(TapePtr<'tape>, State<'rt, 'tape, 'stack>, StackPtr<'stack>);

// TODO: Rename to `CodeAddr`
#[derive(Debug, Copy, Clone)]
pub struct TapeOffset(usize);

impl TapeOffset {
    pub fn offset_to(&self, other: Self) -> isize { other.0 as isize - self.0 as isize }
}

#[derive(Debug, Copy, Clone)]
pub struct FixupTapeOffset<T>(TapeOffset, PhantomData<T>);

#[derive(Default)]
pub struct Tape {
    // `MaybeUninit` is used because it may carry provenance, which is important for function pointers
    // See [https://github.com/rust-lang/miri/issues/2926]
    code: Vec<MaybeUninit<u8>>,
    pub(crate) symbols: Vec<(usize, usize, String)>,
}

impl Tape {
    pub fn next_addr(&self) -> TapeOffset { TapeOffset(self.code.len()) }

    pub fn push<T: Data>(&mut self, data: T) -> TapeOffset
    where
        [(); T::BYTES]:,
    {
        let offset = self.next_addr();
        self.code.extend_from_slice(data.to_bytes().as_ref());
        offset
    }

    pub fn ptr(&self) -> TapePtr { TapePtr(self.code.as_ptr(), PhantomData) }

    /// Fixup a specific address with a new value.
    // Ensure that types match (TODO: check this somehow?!)
    pub fn fixup<T: Data>(&mut self, addr: FixupTapeOffset<T>, data: T)
    where
        [(); T::BYTES]:,
    {
        let bytes = data.to_bytes();
        self.code[addr.0 .0..][..bytes.len()].clone_from_slice(bytes.as_ref());
    }

    // Returns (instr addr, args addr)
    #[track_caller]
    pub fn push_op<A, F>(&mut self, symbol: String, args: A, _f: F) -> (TapeOffset, A::Addresses)
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

            let next_instr = unsafe { tape.read::<TapeFn>() };
            become next_instr(tape, state, stack);
        }

        // Doesn't work yet :(
        // const {
        //     if core::mem::size_of::<T>() != 0 {
        //         panic!("Can only perform operations that have no environment");
        //     }
        // }

        /// ```compile_fail
        /// use yurt::vm::Tape;
        /// let mut tape = Tape::default();
        /// let i = 0;
        /// tape.push_op((), |(), _, _, _| {
        ///     let _ = &i;
        /// });
        /// ```
        trait NoCapture: Sized {
            const ASSERT: () = assert!(core::mem::size_of::<Self>() == 0, "Can only perform operations that have no environment");
        }

        impl<F> NoCapture for F {}

        #[allow(clippy::let_unit_value)]
        let _ = <F as NoCapture>::ASSERT;

        let pre = self.code.len();

        let instr_addr = self.push(perform::<A, F> as TapeFn);
        let arg_addrs = args.push(self);

        self.symbols.push((pre, self.code.len(), symbol));

        (instr_addr, arg_addrs)
    }

    pub fn push_exit(&mut self) {
        fn exit<'rt, 'tape, 'stack>(tape: TapePtr<'tape>, state: State<'rt, 'tape, 'stack>, stack: StackPtr<'stack>) {
            state.rt.save_state(tape, state.regs, stack);
        }

        let pre = self.code.len();
        self.push(exit as TapeFn);
        self.symbols.push((pre, self.code.len(), "exit".to_string()));
    }
}

pub trait Args {
    type Addresses;

    fn push(self, tape: &mut Tape) -> Self::Addresses;
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
            type Addresses = ($(FixupTapeOffset<$X>,)*);
            fn push(self, tape: &mut Tape) -> Self::Addresses {
                let ($($X,)*) = self;
                ($(FixupTapeOffset(tape.push($X), PhantomData),)*)
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
    pub unsafe fn jump_rel(&mut self, offset: isize) { self.0 = self.0.offset(offset); }
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
    where
        [(); T::BYTES]:,
    {
        T::from_bytes(*self.read_bytes())
    }

    #[inline(always)]
    pub unsafe fn exec_at<'rt, 'stack>(&mut self, offset: TapeOffset, state: State<'rt, 'tape, 'stack>, stack: StackPtr<'stack>) {
        let mut tape = Self(self.0.offset(offset.0 as isize), PhantomData);
        tape.read::<TapeFn>()(tape, state, stack);
    }
}
