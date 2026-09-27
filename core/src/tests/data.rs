use crate::domain::{Age, Effect, Structure};
use crate::engine::data::{
    AGE_III_STRUCTURES, AGE_II_STRUCTURES, AGE_I_STRUCTURES, GUILD_STRUCTURES, STRUCTURES_BY_NAME,
};

fn all_structures() -> Vec<&'static Structure<'static, Effect>> {
    AGE_I_STRUCTURES
        .iter()
        .chain(AGE_II_STRUCTURES.iter())
        .chain(AGE_III_STRUCTURES.iter())
        .chain(GUILD_STRUCTURES.iter())
        .collect()
}

#[test]
fn chain_links_are_symmetric_and_point_forward_in_time() {
    for structure in all_structures() {
        for dependency in structure.dependencies() {
            let from = STRUCTURES_BY_NAME.get(*dependency).unwrap_or_else(|| {
                panic!("{} chains from unknown {}", structure.name(), dependency)
            });
            assert!(
                from.dependents().contains(&structure.name()),
                "{} is free from {} but {} does not list it as a dependent",
                structure.name(),
                dependency,
                dependency
            );
            assert!(from.age().number() < structure.age().number());
        }
        for dependent in structure.dependents() {
            let to = STRUCTURES_BY_NAME.get(*dependent).unwrap_or_else(|| {
                panic!("{} lists unknown dependent {}", structure.name(), dependent)
            });
            assert!(
                to.dependencies().contains(&structure.name()),
                "{} lists {} as a dependent but {} is not free from it",
                structure.name(),
                dependent,
                dependent
            );
        }
    }
}

#[test]
fn age_numbers_round_trip() {
    for age in [Age::I, Age::II, Age::III] {
        assert_eq!(Age::from_number(age.number()), age);
    }
    assert_eq!(Age::None.number(), 0);
}
