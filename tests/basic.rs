use yurt::{
    api::{Cond, Expr, Module, Repr},
    vm::{Runtime, Stack, State, Tape},
};

#[test]
fn procedure() {
    let mut module = Module::build();
    let factorial = module.declare_func(Repr::U64, Repr::U64);
    module.define_func(factorial, |arg| {
        Cond::IsTrue.if_then_else(
            arg.clone().eq(Expr::U64(0)),
            Expr::U64(1),
            factorial
                .call(arg.clone().sub(Expr::U64(1)))
                .mul(arg),
        )
    });
    let module = module.compile().unwrap();

    module.show_symbols();

    assert_eq!(module.call::<_, u64>(factorial, 10u64).unwrap(), 16);
}
