use super::*;

pub mod tape;
pub mod stack;
pub mod util;

pub use self::{
    tape::{Tape, TapePtr, TapeFn},
    stack::{Stack, StackPtr},
    util::Data,
};

#[derive(Copy, Clone, Default)]
pub struct Regs(u64);

pub struct State<'rt, 'tape, 'stack> {
    regs: Regs,
    rt: &'rt mut Runtime<'tape, 'stack>,
}

impl<'rt, 'tape, 'stack> State<'rt, 'tape, 'stack> {
    pub fn init(rt: &'rt mut Runtime<'tape, 'stack>) -> Self {
        Self {
            regs: Regs::default(),
            rt,
        }
    }
}

#[derive(Default)]
pub struct Runtime<'tape, 'stack> {
    pub(crate) last_state: Option<(TapePtr<'tape>, Regs, StackPtr<'stack>)>,
}

impl<'tape, 'stack> Runtime<'tape, 'stack> {
    fn save_state(&mut self, tape: TapePtr<'tape>, regs: Regs, stack: StackPtr<'stack>) {
        self.last_state = Some((tape, regs, stack));
    }

    pub fn last_stack_ptr(&mut self) -> Option<&mut StackPtr<'stack>> {
        self.last_state.as_mut().map(|(_, _, stack)| stack)
    }
}
