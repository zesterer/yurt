use super::*;

use crate::vm::{Tape, State, Runtime, Stack, Data, util::{AsRepr, PushPop}};

pub struct Module {
    pub(crate) tape: Tape,
    pub(crate) funcs: Vec<FuncInfo>,
}

impl Module {
    pub fn build() -> ModuleBuilder {
        ModuleBuilder::default()
    }

    pub fn call<I: PushPop + AsRepr, O: PushPop + AsRepr>(&self, f: FuncId, input: I) -> Result<O, String> {
        let func = &self.funcs[f.0];

        if I::repr() != func.input {
            Err(format!("Tried to call function with input type of {:?} with input type {:?}", func.input, I::repr()))
        } else if O::repr() != func.output {
            Err(format!("Tried to call function with output type of {:?} with output type {:?}", func.output, O::repr()))
        } else {
            let mut stack = Stack::new(1024);
            let mut rt = Runtime::default();
            unsafe {
                let mut ptr = stack.ptr();

                // Push argument
                input.push_to(&mut ptr);

                self.tape.ptr().exec_at(func.addr as isize, State::init(&mut rt), ptr);
                Ok(O::pop_from(rt.last_stack_ptr().unwrap()))
            }
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
    pub fn build_func<F: FnOnce(Expr) -> Expr>(&mut self, input: Repr, output: Repr, f: F) -> FuncId {
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

    pub fn add(self, other: Self) -> Self { Self::Add(Box::new(self), Box::new(other)) }
    pub fn mul(self, other: Self) -> Self { Self::Mul(Box::new(self), Box::new(other)) }

    pub fn then(self, other: Self) -> Self {
        Self::Then(Box::new(self), Box::new(other))
    }

    pub fn scope<F: FnOnce(Vec<Expr>) -> Expr>(inputs: Vec<Expr>, f: F) -> Self {
        let body = f((0..inputs.len()).map(Self::Local).collect());
        Self::Scope(inputs, Box::new(body))
    }

    pub fn derive_repr(&self, get_local: &impl Fn(usize) -> Repr) -> Repr {
        match self {
            Expr::Local(local) => get_local(*local),
            Expr::FieldAccess(x, key) => if let Repr::Tuple(mut fields) = x.derive_repr(get_local) {
                fields.remove(*key)
            } else {
                panic!("Field access on non-tuple not permitted")
            },
            Expr::Add(a, b) | Expr::Mul(a, b) => {
                let a_repr = a.derive_repr(get_local);
                let b_repr = b.derive_repr(get_local);
                assert_eq!(a_repr, Repr::U64);
                assert_eq!(b_repr, Repr::U64);
                Repr::U64
            },
            expr => todo!("{expr:?}"),
        }
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
            Self::U64 => core::mem::size_of::<u64>(),
            Self::Tuple(fields) => fields
                .iter()
                .map(|f| f.size())
                .sum(),
        }
    }
}
