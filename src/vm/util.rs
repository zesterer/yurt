use super::*;

pub trait Data {
    const BYTES: usize;

    fn to_bytes(&self) -> [MaybeUninit<u8>; Self::BYTES];
    /// Bytes must be valid for this type.
    unsafe fn from_bytes(bytes: [MaybeUninit<u8>; Self::BYTES]) -> Self;
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
        )*
    };
}

tape_data_int!(
    u8 = 1, u16 = 2, u32 = 4, u64 = 8, u128 = 16, usize = core::mem::size_of::<usize>(),
    i8 = 1, i16 = 2, i32 = 4, i64 = 8, i128 = 16, isize = core::mem::size_of::<isize>(),
);

impl Data for () {
    const BYTES: usize = 0;
    #[inline(always)]
    fn to_bytes(&self) -> [MaybeUninit<u8>; Self::BYTES] { [] }
    #[inline(always)]
    unsafe fn from_bytes(_bytes: [MaybeUninit<u8>; Self::BYTES]) -> Self {}
}

impl Data for TapeFn {
    const BYTES: usize = core::mem::size_of::<TapeFn>();

    #[inline(always)]
    fn to_bytes(&self) -> [MaybeUninit<u8>; Self::BYTES] { MaybeUninit::new((*self as usize).to_ne_bytes()).transpose() }
    #[inline(always)]
    unsafe fn from_bytes(bytes: [MaybeUninit<u8>; Self::BYTES]) -> Self {
        core::mem::transmute(bytes)
    }
}
