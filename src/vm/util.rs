use super::*;
use crate::api::Repr;
use core::ops::Range;

// Take an existing stack offset and return the offset range for the given repr to be placed on the stack.
pub fn incr_offset(offset: usize, repr: &Repr) -> Range<usize> {
    let start = offset.next_multiple_of(repr.align());
    start..(start + repr.size())
}

pub trait AsRepr {
    fn repr() -> Repr;
}

pub trait Data {
    const BYTES: usize;
    const ALIGN: usize;

    fn to_bytes(&self) -> [MaybeUninit<u8>; Self::BYTES]
    where
        [(); Self::BYTES]:;
    /// Bytes must be valid for this type.
    unsafe fn from_bytes(bytes: [MaybeUninit<u8>; Self::BYTES]) -> Self;
}

pub trait StackSafe: Sized {
    const ASSERT: () = assert!(
        core::mem::align_of::<Self>() <= StackPtr::FRAME_ALIGN,
        "Stack-safe aligns must have an alignment <= the minimum stack frame alignment"
    );
}

pub trait ReadWrite {
    unsafe fn write(&self, stack: &mut StackPtr, offset: usize);
    unsafe fn read(stack: &StackPtr, offset: usize) -> Self;
}

macro_rules! tape_data_int {
    ($($T:ty = $N:expr),* $(,)?) => {
        $(
            impl Data for $T {
                const BYTES: usize = $N;
                const ALIGN: usize = core::mem::align_of::<$T>();
                #[inline(always)]
                fn to_bytes(&self) -> [MaybeUninit<u8>; Self::BYTES] { MaybeUninit::new(self.to_ne_bytes()).transpose() }
                #[inline(always)]
                unsafe fn from_bytes(bytes: [MaybeUninit<u8>; Self::BYTES]) -> Self {
                    <$T>::from_ne_bytes(MaybeUninit::array_assume_init(bytes))
                }
            }

            impl StackSafe for $T {}

            impl ReadWrite for $T {
                #[inline(always)]
                unsafe fn write(&self, stack: &mut StackPtr, offset: usize) {
                    stack.ptr_mut().offset(offset as isize).cast::<Self>().write(*self);
                }
                #[inline(always)]
                unsafe fn read(stack: &StackPtr, offset: usize) -> Self {
                    stack.ptr().offset(offset as isize).cast::<Self>().read()
                }
            }
        )*
    };
}

tape_data_int!(
    u8 = 1,
    u16 = 2,
    u32 = 4,
    u64 = 8,
    u128 = 16,
    usize = core::mem::size_of::<usize>(),
    i8 = 1,
    i16 = 2,
    i32 = 4,
    i64 = 8,
    i128 = 16,
    isize = core::mem::size_of::<isize>(),
);

impl AsRepr for u64 {
    fn repr() -> Repr {
        Repr::U64
    }
}

impl Data for TapeFn {
    const BYTES: usize = core::mem::size_of::<TapeFn>();
    const ALIGN: usize = core::mem::align_of::<TapeFn>();

    #[inline(always)]
    fn to_bytes(&self) -> [MaybeUninit<u8>; Self::BYTES] {
        MaybeUninit::new((*self as usize).to_ne_bytes()).transpose()
    }
    #[inline(always)]
    unsafe fn from_bytes(bytes: [MaybeUninit<u8>; Self::BYTES]) -> Self {
        core::mem::transmute(bytes)
    }
}

macro_rules! impl_push_pop_for_tuple {
    () => { impl_push_pop_for_tuple!(~); };
    ($head:ident $($X:ident)*) => {
        impl_push_pop_for_tuple!($($X)*);
        impl_push_pop_for_tuple!(~ $head $($X)*);
    };
    (~ $($X:ident)*) => {
        #[allow(unused_variables, non_snake_case)]
        impl<$($X: ReadWrite + AsRepr),*> ReadWrite for ($($X,)*) {
            #[inline(always)]
            #[allow(unused_assignments)]
            unsafe fn write(&self, stack: &mut StackPtr, mut offset: usize) {
                let ($($X,)*) = self;
                $(
                    // TODO: Faster implementation without invoking $X::repr()
                    let field_range = incr_offset(offset, &$X::repr());
                    let $X = $X.write(stack, field_range.start);
                    offset = field_range.end;
                )*
            }
            #[inline(always)]
            #[allow(unused_assignments)]
            unsafe fn read(stack: &StackPtr, mut offset: usize) -> Self {
                $(
                    // TODO: Faster implementation without invoking $X::repr()
                    let field_range = incr_offset(offset, &$X::repr());
                    let $X = $X::read(stack, field_range.start);
                    offset = field_range.end;
                )*
                ($($X,)*)
            }
        }

        impl<$($X: AsRepr),*> AsRepr for ($($X,)*) {
            fn repr() -> Repr { Repr::Tuple(vec![$($X::repr(),)*]) }
        }
    };
}

impl_push_pop_for_tuple!(A_ B_ C_ D_ E_ F_);
