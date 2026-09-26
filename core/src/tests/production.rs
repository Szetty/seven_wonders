use super::helpers::{default_game_state, TEST_WONDER_SIDE};
use crate::domain::{
    Card, GameState, Player, PlayerDecision, ResourceCost, ResourceCount, ResourceType,
    ResourceType::*, ResourceTypes, ResourcesProduced, ALL_RESOURCE_TYPES,
};
use crate::engine::data::{STRUCTURES_BY_NAME, WONDERS_BY_NAME};
use maplit::hashmap;
use std::collections::HashMap;
use std::time::{Duration, Instant};

fn produced(
    single: HashMap<ResourceType, ResourceCount>,
    any: &[ResourceTypes<'static>],
) -> ResourcesProduced {
    ResourcesProduced {
        single_resources: single,
        any_resources: any.to_vec(),
    }
}

#[test]
fn resource_indexes_follow_declaration_order() {
    for (index, resource_type) in ALL_RESOURCE_TYPES.iter().enumerate() {
        assert_eq!(*resource_type as usize, index);
    }
}

#[test]
fn nothing_is_needed_for_an_empty_cost() {
    assert!(produced(hashmap! {}, &[]).can_produce(&[]));
}

#[test]
fn fixed_resources_cover_matching_costs_only() {
    let p = produced(hashmap! {Wood => 2, Loom => 1}, &[]);
    assert!(p.can_produce(&[ResourceCost(Wood, 1)]));
    assert!(p.can_produce(&[ResourceCost(Wood, 2)]));
    assert!(p.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Wood, 3)]));
    assert!(!p.can_produce(&[ResourceCost(Loom, 2)]));
    assert!(!p.can_produce(&[ResourceCost(Clay, 1)]));
}

#[test]
fn each_choice_card_is_used_once() {
    let p = produced(hashmap! {}, &[&[Wood, Loom], &[Wood, Ore]]);
    assert!(p.can_produce(&[ResourceCost(Wood, 1)]));
    assert!(p.can_produce(&[ResourceCost(Wood, 2)]));
    assert!(p.can_produce(&[ResourceCost(Loom, 1), ResourceCost(Ore, 1)]));
    assert!(p.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]));
    assert!(p.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Ore, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Wood, 3)]));
    assert!(!p.can_produce(&[ResourceCost(Wood, 2), ResourceCost(Loom, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Loom, 2)]));
    assert!(!p.can_produce(&[ResourceCost(Clay, 1)]));
}

#[test]
fn identical_choice_cards_combine() {
    let p = produced(hashmap! {}, &[&[Wood, Loom], &[Wood, Loom]]);
    assert!(p.can_produce(&[ResourceCost(Wood, 2)]));
    assert!(p.can_produce(&[ResourceCost(Loom, 2)]));
    assert!(p.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Wood, 3)]));
    assert!(!p.can_produce(&[ResourceCost(Ore, 1)]));
}

#[test]
fn fixed_and_choice_resources_combine() {
    let p = produced(hashmap! {Clay => 1}, &[&[Wood, Loom]]);
    assert!(p.can_produce(&[ResourceCost(Clay, 1), ResourceCost(Wood, 1)]));
    assert!(p.can_produce(&[ResourceCost(Loom, 1), ResourceCost(Clay, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Clay, 2)]));
    assert!(!p.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Stone, 1)]));
    let q = produced(hashmap! {Wood => 1}, &[&[Wood, Loom]]);
    assert!(q.can_produce(&[ResourceCost(Wood, 2)]));
    assert!(q.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]));
    assert!(!q.can_produce(&[ResourceCost(Wood, 2), ResourceCost(Loom, 1)]));
}

#[test]
fn matching_reroutes_earlier_choices() {
    // A greedy assignment would give the Wood/Loom card to Wood and then fail on Loom.
    let p = produced(hashmap! {}, &[&[Wood, Loom], &[Wood]]);
    assert!(p.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]));
}

#[test]
fn many_choice_cards_stay_fast() {
    let mut choices: Vec<ResourceTypes<'static>> =
        vec![&[Wood, Stone, Ore, Clay] as ResourceTypes<'static>; 6];
    choices.extend([&[Glass, Loom, Papyrus] as ResourceTypes<'static>; 4]);
    let p = produced(hashmap! {}, &choices);
    let palace: Vec<ResourceCost> = ALL_RESOURCE_TYPES
        .iter()
        .map(|resource_type| ResourceCost(*resource_type, 1))
        .collect();
    let started = Instant::now();
    assert!(p.can_produce(&palace));
    assert!(!p.can_produce(&[ResourceCost(Wood, 7), ResourceCost(Glass, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Wood, 6), ResourceCost(Glass, 5)]));
    assert!(started.elapsed() < Duration::from_millis(50));
}

fn build(game_state: &mut GameState, player: &str, card: &str) {
    let structure = STRUCTURES_BY_NAME.get(card).unwrap();
    game_state.apply_player_decisions(vec![(
        player.to_string(),
        PlayerDecision::BuildStructure(Card(structure)),
    )]);
}

#[test]
fn brown_and_grey_production_is_tradable_but_yellow_choices_are_not() {
    let mut game_state = default_game_state();
    for card in ["Tree Farm", "Loom", "Forum", "Caravansery"] {
        build(&mut game_state, "a", card);
    }
    let player_state = game_state.get_player_state(&"a".to_string());
    let tradable = &player_state.tradable_resources;
    assert!(tradable.can_produce(&[ResourceCost(Wood, 1)]));
    assert!(tradable.can_produce(&[ResourceCost(Loom, 1)]));
    assert!(!tradable.can_produce(&[ResourceCost(Loom, 2)]));
    assert!(!tradable.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Clay, 1)]));
    assert!(player_state.resources_produced.can_produce(&[
        ResourceCost(Wood, 1),
        ResourceCost(Clay, 1),
        ResourceCost(Loom, 2)
    ]));
}

#[test]
fn wonder_starting_resource_is_tradable_but_stage_choices_are_not() {
    let alexandria = *WONDERS_BY_NAME.get("Alexandria").unwrap();
    let mut game_state = GameState::new(
        Default::default(),
        vec![
            (Player("a".to_string()), &alexandria.1),
            (Player("b".to_string()), &*TEST_WONDER_SIDE),
            (Player("c".to_string()), &*TEST_WONDER_SIDE),
        ],
    );
    game_state.init();
    let tavern = STRUCTURES_BY_NAME.get("Tavern").unwrap();
    for _ in 0..2 {
        game_state.apply_player_decisions(vec![(
            "a".to_string(),
            PlayerDecision::BuildNextWonderStage(Card(tavern)),
        )]);
    }
    let player_state = game_state.get_player_state(&"a".to_string());
    assert!(player_state
        .tradable_resources
        .can_produce(&[ResourceCost(Glass, 1)]));
    assert!(!player_state
        .tradable_resources
        .can_produce(&[ResourceCost(Wood, 1)]));
    assert!(player_state
        .resources_produced
        .can_produce(&[ResourceCost(Glass, 1), ResourceCost(Wood, 1)]));
}
