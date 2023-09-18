use super::*;

use crate::vm::{
    util::{incr_offset, AsRepr},
    Data, ReadWrite, Runtime, Stack, State, Tape,
};

pub struct Module {
    pub(crate) tape: Tape,
    pub(crate) funcs: Vec<FuncInfo>,
}

impl Module {
    pub fn build() -> ModuleBuilder {
        ModuleBuilder::default()
    }

    pub fn call<I: ReadWrite + AsRepr, O: ReadWrite + AsRepr>(
        &self,
        f: FuncId,
        input: I,
    ) -> Result<O, String> {
        let func = &self.funcs[f.0];

        let input_repr = I::repr();
        let output_repr = O::repr();

        if input_repr != func.input {
            Err(format!(
                "Tried to call function with input type of {:?} with input type {:?}",
                func.input, input_repr
            ))
        } else if output_repr != func.output {
            Err(format!(
                "Tried to call function with output type of {:?} with output type {:?}",
                func.output, output_repr
            ))
        } else {
            let mut stack = Stack::new(1024);
            let mut rt = Runtime::default();
            unsafe {
                let mut stack = stack.ptr();

                // ABI is return space, then arg
                let output_range = incr_offset(0, &output_repr);
                let input_range = incr_offset(output_range.end, &input_repr);
                stack.write(input, input_range.start);

                self.tape
                    .ptr()
                    .exec_at(func.addr as isize, State::init(&mut rt), stack);
                Ok(rt.last_stack_ptr().unwrap().read(output_range.start))
            }
        }
    }

    pub fn show_symbols(&self) {
        for (addr, desc) in &self.tape.symbols {
            println!("0x{addr:03X} | {desc}");
        }
    }
}

pub struct FuncInfo {
    pub(crate) input: Repr,
    pub(crate) output: Repr,
    pub(crate) addr: usize,
}

#[derive(Default)]
pub struct ModuleBuilder {
    pub(crate) funcs: Vec<Func>,
}

impl ModuleBuilder {
    pub fn build_func<F: FnOnce(Expr) -> Expr>(
        &mut self,
        input: Repr,
        output: Repr,
        f: F,
    ) -> FuncId {
        let id = FuncId(self.funcs.len());
        self.funcs.push(Func {
            input,
            output,
            body: f(Expr::Local(0)),
        });
        id
    }
}

#[derive(Copy, Clone)]
pub struct FuncId(usize);

pub struct Func {
    pub(crate) input: Repr,
    pub(crate) output: Repr,
    pub(crate) body: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Local(usize),
    FieldAccess(Box<Self>, usize),
    Add(Box<Self>, Box<Self>),
    Mul(Box<Self>, Box<Self>),
    Then(Box<Self>, Box<Self>),
    Scope(Vec<Self>, Box<Self>),
}

impl Expr {
    pub fn field(self, key: usize) -> Self {
        Self::FieldAccess(Box::new(self), key)
    }

    pub fn add(self, other: Self) -> Self {
        Self::Add(Box::new(self), Box::new(other))
    }
    pub fn mul(self, other: Self) -> Self {
        Self::Mul(Box::new(self), Box::new(other))
    }

    pub fn then(self, other: Self) -> Self {
        Self::Then(Box::new(self), Box::new(other))
    }

    pub fn scope<F: FnOnce(Vec<Expr>) -> Expr>(inputs: Vec<Expr>, f: F) -> Self {
        let body = f((0..inputs.len()).map(Self::Local).collect());
        Self::Scope(inputs, Box::new(body))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Repr {
    U64,
    Tuple(Vec<Repr>),
}

impl Repr {
    pub fn size(&self) -> usize {
        match self {
            Self::U64 => u64::BYTES,
            Self::Tuple(fields) => fields.iter().map(|f| f.size()).sum(),
        }
    }

    pub fn align(&self) -> usize {
        match self {
            Self::U64 => u64::ALIGN,
            Self::Tuple(fields) => fields.iter().map(|f| f.align()).max().unwrap_or(1), // Align of an empty tuple is 1
        }
    }
}
