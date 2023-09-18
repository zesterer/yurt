use yurt::{
    api::{Module, Repr},
    vm::{Runtime, Stack, State, Tape},
};

#[test]
fn procedure() {
    let mut module = Module::build();
    let add_mul = module.build_func(
        Repr::Tuple(vec![Repr::U64, Repr::U64, Repr::U64]),
        Repr::U64,
        |arg| {
            arg.clone()
                .field(0)
                .add(arg.clone().field(1))
                .add(arg.field(2))
        },
    );
    let module = module.compile().unwrap();

    module.show_symbols();

    assert_eq!(
        module.call::<_, u64>(add_mul, (3u64, 5u64, 1u64)).unwrap(),
        9
    );
}
