use super::*;

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
        StackPtr {
            ptr: {
                let ptr = self.bytes.as_mut_ptr();
                let align_offset = ptr.align_offset(StackPtr::FRAME_ALIGN);
                assert!(align_offset < self.bytes.len());
                unsafe { ptr.offset(align_offset as isize) }
            },
            phantom: PhantomData,
        }
    }
}

// Grows down
#[repr(transparent)]
pub struct StackPtr<'stack> {
    ptr: *mut MaybeUninit<u8>,
    phantom: PhantomData<&'stack mut Stack>,
}

impl<'stack> StackPtr<'stack> {
    pub const FRAME_ALIGN: usize = 16;

    #[inline(always)]
    pub(crate) fn ptr(&self) -> *const MaybeUninit<u8> {
        self.ptr
    }
    #[inline(always)]
    pub(crate) fn ptr_mut(&mut self) -> *mut MaybeUninit<u8> {
        self.ptr
    }

    #[inline(always)]
    pub unsafe fn enter_stack_frame<const CHECK_OVERFLOW: bool>(
        &mut self,
        frame_start: usize,
        stack_end: *mut MaybeUninit<u8>,
    ) {
        self.ptr = self.ptr.offset(frame_start as isize);
    }

    #[inline(always)]
    pub unsafe fn read<T: ReadWrite>(&self, offset: usize) -> T {
        T::read(self, offset)
    }
    #[inline(always)]
    pub unsafe fn write<T: ReadWrite>(&mut self, x: T, offset: usize) {
        x.write(self, offset)
    }

    #[inline(always)]
    pub unsafe fn copy(&mut self, src: usize, dst: usize, len: usize) {
        core::ptr::copy_nonoverlapping(
            self.ptr.offset(src as isize),
            self.ptr.offset(dst as isize),
            len,
        );
    }
}
