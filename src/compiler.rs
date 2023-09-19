use crate::{
    api::{Expr, Func, FuncInfo, Module, ModuleBuilder, Repr},
    vm::{
        util::{incr_offset, AsRepr, ReadWrite},
        Data, Tape,
    },
};

enum StackItemKind {
    // local_id
    Local(usize),
    Temporary,
}

struct StackItem {
    kind: StackItemKind,
    repr: Repr,
    offset: usize,
}

// Stack grows upward. Items in the current frame extend *past* stack pointer
// | caller frame | [i32] [i64] ...
//                ^
//                | stack pointer
#[derive(Default)]
struct StackLog {
    // (offset, _)
    items: Vec<StackItem>,
    // In stack frame
    cur_offset: usize,
    // Of stack frame (i.e: after compilation, can be used to determine the overflow check length)
    max_offset: usize,
}

#[derive(Copy, Clone, PartialEq)]
struct StackIdx(usize);

impl StackLog {
    pub fn with_push<F, R>(&mut self, kind: StackItemKind, repr: Repr, f: F) -> R
    where
        F: FnOnce(&mut Self, StackIdx) -> R,
    {
        let item_idx = self.items.len();
        let old_offset = self.cur_offset;
        let offset_range = incr_offset(self.cur_offset, &repr);

        // Apply new item
        self.items.push(StackItem {
            kind,
            repr,
            offset: offset_range.start,
        });
        self.cur_offset = offset_range.end;
        self.max_offset = self.max_offset.max(self.cur_offset);

        // Invoke inner
        let r = f(self, StackIdx(item_idx));

        // Return to old state
        self.cur_offset = old_offset;
        self.items.pop();
        r
    }

    pub fn get_local(&self, local: usize) -> StackIdx {
        self.items
            .iter()
            .enumerate()
            .rev()
            .find(|(_, item)| matches!(&item.kind, StackItemKind::Local(l) if *l == local))
            .map(|(idx, _)| StackIdx(idx))
            .expect("No such local")
    }

    pub fn get(&self, idx: StackIdx) -> &StackItem {
        &self.items[idx.0]
    }

    // Like `with_push`, except the expression is permitted to be derived from some local instead
    pub fn with_expr<F, R>(&mut self, tape: &mut Tape, expr: Expr, f: F) -> Result<R, String>
    where
        F: FnOnce(&mut Tape, &mut Self, (StackIdx, Path)) -> R,
    {
        // Try to extract the expression as a path
        if let Some(path) = expr.try_as_path(self) {
            Ok(f(tape, self, path))
        } else {
            // Path extraction failed, evaluate the expression into a temporary
            self.with_push(
                StackItemKind::Temporary,
                expr.derive_repr(self),
                |stack, expr_out| {
                    expr.compile(tape, stack, expr_out)?;
                    Ok(f(tape, stack, (expr_out, Path::All)))
                },
            )
        }
    }
}

// Represents a path into a repr
#[derive(Debug)]
pub enum Path {
    All,                     // The entire data structure
    Field(usize, Box<Self>), // A field within a tuple
}

impl Expr {
    fn compile(
        self,
        tape: &mut Tape,
        stack: &mut StackLog,
        output: StackIdx,
    ) -> Result<(), String> {
        fn copy(
            tape: &mut Tape,
            stack: &StackLog,
            (src, src_path): (StackIdx, Path),
            (dst, dst_path): (StackIdx, Path),
        ) {
            if src == dst {
                return;
            } // Don't need to copy if src and dst are the same

            let (src, dst) = (stack.get(src), stack.get(dst));
            let src_repr = src.repr.resolve_path(&src_path);
            let dst_repr = dst.repr.resolve_path(&dst_path);
            assert_eq!(
                src_repr, dst_repr,
                "Cannot perform copy between different reprs"
            );

            // println!("src = {:?} [{}], dst = {:?} [{}]", src.repr.resolve_path(&src_path), src.offset + src.repr.resolve_path_offset(&src_path), dst.repr.resolve_path(&dst_path), dst.offset + dst.repr.resolve_path_offset(&dst_path));

            let (src, dst, sz) = (
                src.offset + src.repr.resolve_path_offset(&src_path),
                dst.offset + dst.repr.resolve_path_offset(&dst_path),
                src_repr.size(),
            );

            let symbol = format!("copy {src:3} -[x{sz:2}]-> {dst:3}");

            match (src, dst, sz) {
                (src, dst, sz @ 8) => {
                    tape.push_op(symbol, (src, dst), |(src, dst), tape, regs, stack| unsafe {
                        stack.copy(src, dst, 8)
                    })
                }
                (src, dst, sz) => tape.push_op(
                    symbol,
                    (src, dst, sz),
                    |(src, dst, sz), tape, regs, stack| unsafe { stack.copy(src, dst, sz) },
                ),
            }
        }

        fn binary_op<
            A: Data + ReadWrite + AsRepr,
            B: Data + ReadWrite + AsRepr,
            C: Data + ReadWrite + AsRepr,
            F,
        >(
            tape: &mut Tape,
            stack: &mut StackLog,
            f: F,
            (src0, src0_path): (StackIdx, Path),
            (src1, src1_path): (StackIdx, Path),
            (dst, dst_path): (StackIdx, Path),
        ) where
            F: Fn(A, B) -> C + Copy,
            [(); A::BYTES]:,
            [(); B::BYTES]:,
            [(); C::BYTES]:,
        {
            let (src0, src1, dst) = (stack.get(src0), stack.get(src1), stack.get(dst));
            let src0_repr = src0.repr.resolve_path(&src0_path);
            let src1_repr = src1.repr.resolve_path(&src1_path);
            let dst_repr = dst.repr.resolve_path(&dst_path);
            assert_eq!(
                (src0_repr, src1_repr, dst_repr),
                (&A::repr(), &B::repr(), &C::repr()),
                "Source/destination reprs do not match for binary op"
            );

            let (src0, src1, dst) = (
                src0.offset + src0.repr.resolve_path_offset(&src0_path),
                src1.offset + src1.repr.resolve_path_offset(&src1_path),
                dst.offset + dst.repr.resolve_path_offset(&dst_path),
            );

            let symbol = format!(
                "binary {src0:3}, {src1:3} -> {dst:3} ({})",
                core::any::type_name::<F>()
            );

            match (src0, src1, dst) {
                (src0 @ 8, src1 @ 16, dst @ 32) => {
                    tape.push_op(symbol, (), move |(), tape, regs, stack| unsafe {
                        let a = stack.read(8);
                        let b = stack.read(16);
                        stack.write(f(a, b), 32);
                    })
                }
                (src0, src1, dst) => tape.push_op(
                    symbol,
                    (src0, src1, dst),
                    move |(src0, src1, dst), tape, regs, stack| unsafe {
                        let a = stack.read(src0);
                        let b = stack.read(src1);
                        stack.write(f(a, b), dst);
                    },
                ),
            }
        }

        let output_repr = &stack.get(output).repr;
        let self_repr = self.derive_repr(stack);
        assert_eq!(
            output_repr, &self_repr,
            "Output slot repr {output_repr:?} did not match expression repr {self_repr:?}"
        );

        Ok(match self {
            Expr::Local(local) => copy(
                tape,
                stack,
                (stack.get_local(local), Path::All),
                (output, Path::All),
            ),
            Expr::Add(a, b) => stack.with_expr(tape, *a, move |tape, stack, a_slot| {
                stack.with_expr(tape, *b, move |tape, stack, b_slot| {
                    binary_op(
                        tape,
                        stack,
                        <u64 as core::ops::Add<u64>>::add,
                        a_slot,
                        b_slot,
                        (output, Path::All),
                    );
                })
            })??,
            Expr::FieldAccess(x, key) => {
                stack.with_expr(tape, *x, move |tape, stack, x_slot| {
                    copy(tape, stack, x_slot, (output, Path::All));
                })?
            }
            expr => todo!("{expr:?}"),
        })
    }

    // If possible, try to reuse existing stack data to derive this expression
    fn try_as_path(&self, stack: &StackLog) -> Option<(StackIdx, Path)> {
        match self {
            Expr::Local(local) => Some((stack.get_local(*local), Path::All)),
            Expr::FieldAccess(x, key) => {
                let (x, path) = x.try_as_path(stack)?;
                Some((x, Path::Field(*key, Box::new(path))))
            }
            Expr::Add(..) | Expr::Mul(..) => None,
            expr => todo!("{expr:?}"),
        }
    }

    fn derive_repr(&self, stack: &StackLog) -> Repr {
        match self {
            Expr::Local(local) => stack.get(stack.get_local(*local)).repr.clone(),
            Expr::FieldAccess(x, key) => {
                if let Repr::Tuple(mut fields) = x.derive_repr(stack) {
                    fields.remove(*key)
                } else {
                    panic!("Field access on non-tuple not permitted")
                }
            }
            Expr::Add(a, b) | Expr::Mul(a, b) => {
                let a_repr = a.derive_repr(stack);
                let b_repr = b.derive_repr(stack);
                assert_eq!(a_repr, Repr::U64);
                assert_eq!(b_repr, Repr::U64);
                Repr::U64
            }
            expr => todo!("{expr:?}"),
        }
    }
}

impl Repr {
    pub fn resolve_path(&self, path: &Path) -> &Repr {
        match (self, path) {
            (this, Path::All) => this,
            (Self::Tuple(fields), Path::Field(key, path)) => {
                if *key >= fields.len() {
                    panic!("Tuple field key {key} is invalid for {self:?}")
                } else {
                    fields[*key].resolve_path(path)
                }
            }
            (repr, path) => todo!("resolve path for {repr:?} and {path:?}"),
        }
    }

    pub fn resolve_path_offset(&self, path: &Path) -> usize {
        match (self, path) {
            (_, Path::All) => 0,
            (Self::Tuple(fields), Path::Field(key, path)) => {
                if *key >= fields.len() {
                    panic!("Tuple field key {key} is invalid for {self:?}")
                } else {
                    let mut offset = 0;
                    for field in &fields[..*key] {
                        offset = incr_offset(offset, field).end;
                    }
                    incr_offset(offset, &fields[*key]).start
                        + fields[*key].resolve_path_offset(path)
                }
            }
            (repr, path) => todo!("resolve path offset for {repr:?} and {path:?}"),
        }
    }
}

impl Func {
    pub fn compile(self, tape: &mut Tape) -> Result<FuncInfo, String> {
        let addr = tape.next_addr();
        StackLog::default().with_push(
            StackItemKind::Temporary,
            self.output.clone(),
            |stack, output| {
                stack.with_push(StackItemKind::Local(0), self.input.clone(), |stack, _| {
                    self.body.compile(tape, stack, output)
                })
            },
        )?;
        tape.push_exit();
        Ok(FuncInfo {
            input: self.input,
            output: self.output,
            addr,
        })
    }
}

impl ModuleBuilder {
    pub fn compile(self) -> Result<Module, String> {
        let mut tape = Tape::default();

        let funcs = self
            .funcs
            .into_iter()
            .map(|func| func.compile(&mut tape))
            .collect::<Result<_, _>>()?;

        Ok(Module { tape, funcs })
    }
}
