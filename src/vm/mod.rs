use super::*;

pub mod stack;
pub mod tape;
pub mod util;

pub use self::{
    stack::{Stack, StackPtr},
    tape::{FixupTapeOffset, Tape, TapeFn, TapeOffset, TapePtr},
    util::{Data, ReadWrite},
};

#[derive(Copy, Clone, Default)]
pub struct Regs(u64);

pub struct State<'rt, 'tape, 'stack> {
    regs: Regs,
    rt: &'rt mut Runtime<'tape, 'stack>,
}

impl<'rt, 'tape, 'stack> State<'rt, 'tape, 'stack> {
    pub fn init(rt: &'rt mut Runtime<'tape, 'stack>) -> Self { Self { regs: Regs::default(), rt } }
}

#[derive(Default)]
pub struct Runtime<'tape, 'stack> {
    pub(crate) prev_states: Vec<(TapePtr<'tape>, Regs, StackPtr<'stack>)>,
}

impl<'tape, 'stack> Runtime<'tape, 'stack> {
    #[inline(always)]
    fn push_state(&mut self, tape: TapePtr<'tape>, regs: Regs, stack: StackPtr<'stack>) { self.last_state.push((tape, regs, stack)); }

    pub fn last_stack_ptr(&mut self) -> Option<&mut StackPtr<'stack>> { self.last_state.last_mut().map(|(_, _, stack)| stack) }
}
