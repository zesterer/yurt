use yurt::{
    vm::{Tape, Stack, State, Runtime},
    api::{Module, Repr},
};

#[test]
fn procedure() {
    let mut module = Module::build();
    let add_mul = module.build_func(
        Repr::Tuple(vec![Repr::U64, Repr::U64, Repr::U64]),
        Repr::U64,
        |arg| {
            arg.clone().field(0)
                .mul(arg.clone().field(1))
                .add(arg.field(2))
        },
    );
    let module = module.compile().unwrap();

    assert_eq!(module.call::<_, u64>(add_mul, (3u64, 5u64, 4u64)).unwrap(), 8);
}
