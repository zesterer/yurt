use super::*;

use core::ops::Range;

pub struct Stack {
    bytes: Vec<MaybeUninit<u8>>,
}

impl Stack {
    pub fn new(cap: usize) -> Self {
        Self {
            bytes: vec![MaybeUninit::uninit(); cap],
        }
    }

    pub fn ptr(&mut self) -> StackPtr {
        StackPtr(self.bytes.as_mut_ptr_range(), PhantomData)
    }
}

// Grows down
#[repr(transparent)]
pub struct StackPtr<'stack>(Range<*mut MaybeUninit<u8>>, PhantomData<&'stack mut Stack>);

impl<'stack> StackPtr<'stack> {
    #[inline(always)]
    pub unsafe fn push<T: Data, const CHECK_FOR_OVERFLOW: bool>(&mut self, x: T)
        where [(); T::BYTES]:
    {
        let bytes = x.to_bytes();
        unsafe {
            let new_ptr = self.0.end.offset(-(bytes.len() as isize));
            if new_ptr >= self.0.start || !CHECK_FOR_OVERFLOW {
                self.0.end = new_ptr;
                core::ptr::copy_nonoverlapping(bytes.as_ptr(), self.0.end, bytes.len());
            }
        }
    }
    /// SAFETY: `StackPtr` must have at least `N` bytes to read left.
    #[inline(always)]
    pub unsafe fn read_bytes_at<const N: usize>(&self, offset: usize) -> &'stack [MaybeUninit<u8>; N] {
        &*(self.0.end.offset(offset as isize) as *const [MaybeUninit<u8>; N])
    }
    #[inline(always)]
    pub unsafe fn read_at<T: Data>(&self, offset: usize) -> T
        where [(); T::BYTES]:
    {
        T::from_bytes(*self.read_bytes_at(offset))
    }
    #[inline(always)]
    pub unsafe fn pop<T: Data>(&mut self) -> T
        where [(); T::BYTES]:
    {
        let x = self.read_at(0);
        self.0.end = self.0.end.offset(T::BYTES as isize);
        x
    }
    #[inline(always)]
    pub unsafe fn copy_to_top(&mut self, offset: usize, len: usize) {
        let src = self.0.end.offset(offset as isize);
        self.0.end = self.0.end.offset(-(len as isize));
        core::ptr::copy_nonoverlapping(src, self.0.end, len);
    }
    #[inline(always)]
    pub unsafe fn pop_n(&mut self, n: usize) {
        self.0.end = self.0.end.offset(n as isize);
    }
}
