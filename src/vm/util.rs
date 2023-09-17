use super::*;
use crate::api::Repr;

pub trait AsRepr {
    fn repr() -> Repr;
}

pub trait Data {
    const BYTES: usize;

    fn to_bytes(&self) -> [MaybeUninit<u8>; Self::BYTES] where [(); Self::BYTES]:;
    /// Bytes must be valid for this type.
    unsafe fn from_bytes(bytes: [MaybeUninit<u8>; Self::BYTES]) -> Self;
}

pub trait PushPop {
    unsafe fn push_to(&self, stack: &mut StackPtr);
    unsafe fn pop_from(stack: &mut StackPtr) -> Self;
}

macro_rules! tape_data_int {
    ($($T:ty = $N:expr),* $(,)?) => {
        $(
            impl Data for $T {
                const BYTES: usize = $N;
                #[inline(always)]
                fn to_bytes(&self) -> [MaybeUninit<u8>; Self::BYTES] { MaybeUninit::new(self.to_ne_bytes()).transpose() }
                #[inline(always)]
                unsafe fn from_bytes(bytes: [MaybeUninit<u8>; Self::BYTES]) -> Self {
                    <$T>::from_ne_bytes(MaybeUninit::array_assume_init(bytes))
                }
            }
            impl PushPop for $T {
                unsafe fn push_to(&self, stack: &mut StackPtr) { stack.push::<_, false>(*self) }
                unsafe fn pop_from(stack: &mut StackPtr) -> Self { stack.pop() }
            }
        )*
    };
}

tape_data_int!(
    u8 = 1, u16 = 2, u32 = 4, u64 = 8, u128 = 16, usize = core::mem::size_of::<usize>(),
    i8 = 1, i16 = 2, i32 = 4, i64 = 8, i128 = 16, isize = core::mem::size_of::<isize>(),
);


impl AsRepr for u64 {
    fn repr() -> Repr { Repr::U64 }
}

// impl Data for () {
//     const BYTES: usize = 0;
//     #[inline(always)]
//     fn to_bytes(&self) -> [MaybeUninit<u8>; Self::BYTES] { [] }
//     #[inline(always)]
//     unsafe fn from_bytes(_bytes: [MaybeUninit<u8>; Self::BYTES]) -> Self {}
// }

impl Data for TapeFn {
    const BYTES: usize = core::mem::size_of::<TapeFn>();

    #[inline(always)]
    fn to_bytes(&self) -> [MaybeUninit<u8>; Self::BYTES] { MaybeUninit::new((*self as usize).to_ne_bytes()).transpose() }
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
        impl<$($X: PushPop),*> PushPop for ($($X,)*) {
            unsafe fn push_to(&self, stack: &mut StackPtr) {
                let ($($X,)*) = self;
                impl_push_pop_for_tuple!(@ stack $($X)*);
            }
            unsafe fn pop_from(stack: &mut StackPtr) -> Self {
                $(let $X = $X::pop_from(stack);)*
                ($($X,)*)
            }
        }

        impl<$($X: AsRepr),*> AsRepr for ($($X,)*) {
            fn repr() -> Repr { Repr::Tuple(vec![$($X::repr(),)*]) }
        }
    };
    (@ $stack:ident) => {()};
    (@ $stack:ident $head:ident $($X:ident)*) => { { impl_push_pop_for_tuple!(@ $stack $($X)*); $head.push_to($stack) }; };
}

impl_push_pop_for_tuple!(A_ B_ C_ D_ E_ F_);
