use super::game_helpers::{build, build_paying, discard, plain_game, stage};
use crate::game::{Action, ActionError, ResourceType::*};

#[test]
fn unknown_player_is_rejected() {
    let mut game = plain_game();
    assert_eq!(
        game.submit("zed", discard("Altar")),
        Err(ActionError::UnknownPlayer)
    );
}

#[test]
fn card_must_be_in_hand_for_every_hand_action() {
    let mut game = plain_game();
    game.set_hand("p1", &["Altar"]);
    for action in [build("Palace"), stage("Palace"), discard("Palace")] {
        assert_eq!(
            game.validate("p1", &action),
            Err(ActionError::CardNotInHand)
        );
    }
}

#[test]
fn discarding_a_card_in_hand_is_legal() {
    let mut game = plain_game();
    game.set_hand("p1", &["Altar"]);
    assert_eq!(game.submit("p1", discard("Altar")), Ok(()));
}

#[test]
fn a_free_card_needs_an_empty_payment() {
    let mut game = plain_game();
    game.set_hand("p1", &["Lumber Yard"]);
    assert_eq!(game.validate("p1", &build("Lumber Yard")), Ok(()));
    assert_eq!(
        game.validate("p1", &build_paying("Lumber Yard", &[], &[(Ore, 1)])),
        Err(ActionError::InvalidPayment)
    );
}

#[test]
fn a_coin_cost_must_be_affordable() {
    let mut game = plain_game();
    game.set_hand("p1", &["Tree Farm"]);
    assert_eq!(game.validate("p1", &build("Tree Farm")), Ok(()));
    game.set_coins("p1", 0);
    assert_eq!(
        game.validate("p1", &build("Tree Farm")),
        Err(ActionError::CannotAfford)
    );
}

#[test]
fn own_production_covers_a_resource_cost() {
    // p1 is Gizah A and produces 1 Stone; Baths costs 1 Stone.
    let mut game = plain_game();
    game.set_hand("p1", &["Baths"]);
    assert_eq!(game.validate("p1", &build("Baths")), Ok(()));
}

#[test]
fn a_resource_nobody_sells_cannot_be_afforded() {
    // Stockade costs Wood; Rhódos (Ore) and Éphesos (Papyrus) do not produce it.
    let mut game = plain_game();
    game.set_hand("p1", &["Stockade"]);
    assert_eq!(
        game.validate("p1", &build("Stockade")),
        Err(ActionError::CannotAfford)
    );
}

#[test]
fn purchases_must_come_from_the_neighbour_that_sells() {
    // p1's east neighbour is p2 (Rhódos A); its starting Ore is tradable.
    let mut game = plain_game();
    game.set_hand("p1", &["Barracks"]);
    assert_eq!(
        game.validate("p1", &build_paying("Barracks", &[], &[(Ore, 1)])),
        Ok(())
    );
    assert_eq!(
        game.validate("p1", &build_paying("Barracks", &[(Ore, 1)], &[])),
        Err(ActionError::InvalidPayment)
    );
    assert_eq!(
        game.validate("p1", &build("Barracks")),
        Err(ActionError::InvalidPayment)
    );
    game.set_coins("p1", 1);
    assert_eq!(
        game.validate("p1", &build_paying("Barracks", &[], &[(Ore, 1)])),
        Err(ActionError::CannotAfford)
    );
}

#[test]
fn chain_builds_are_free() {
    let mut game = plain_game();
    game.give("p1", "Altar");
    game.set_hand("p1", &["Temple"]);
    assert_eq!(game.validate("p1", &build("Temple")), Ok(()));
    assert_eq!(
        game.validate("p1", &build_paying("Temple", &[], &[(Ore, 1)])),
        Err(ActionError::InvalidPayment)
    );
}

#[test]
fn an_identical_structure_cannot_be_built_twice() {
    let mut game = plain_game();
    game.give("p1", "Altar");
    game.set_hand("p1", &["Altar"]);
    assert_eq!(
        game.validate("p1", &build("Altar")),
        Err(ActionError::AlreadyBuilt)
    );
    assert_eq!(game.validate("p1", &discard("Altar")), Ok(()));
}

#[test]
fn wonder_stage_cost_and_limit_are_checked() {
    // Gizah A stage 1 costs 2 Stone; p1 produces 1 and nobody sells Stone.
    let mut game = plain_game();
    game.set_hand("p1", &["Altar"]);
    assert_eq!(
        game.validate("p1", &stage("Altar")),
        Err(ActionError::CannotAfford)
    );
    game.give("p1", "Stone Pit");
    assert_eq!(game.validate("p1", &stage("Altar")), Ok(()));
    game.give_stages("p1", 3);
    assert_eq!(
        game.validate("p1", &stage("Altar")),
        Err(ActionError::NoWonderStageLeft)
    );
}

#[test]
fn free_build_requires_the_olympia_ability() {
    let mut game = plain_game();
    game.set_hand("p1", &["Palace"]);
    assert_eq!(
        game.validate(
            "p1",
            &Action::BuildFree {
                card: "Palace".to_string()
            }
        ),
        Err(ActionError::FreeBuildUnavailable)
    );
}

#[test]
fn build_from_discard_is_not_allowed_outside_its_extra_turn() {
    let game = plain_game();
    assert_eq!(
        game.validate(
            "p1",
            &Action::BuildFromDiscard {
                card: "Palace".to_string()
            }
        ),
        Err(ActionError::ActionNotAllowedNow)
    );
}

#[test]
fn resubmitting_replaces_the_pending_action_but_invalid_ones_do_not() {
    let mut game = plain_game();
    game.set_hand("p1", &["Altar", "Lumber Yard", "Stockade"]);
    game.submit("p1", discard("Altar")).unwrap();
    game.submit("p1", build("Lumber Yard")).unwrap();
    assert_eq!(game.pending_action("p1"), Some(build("Lumber Yard")));
    assert_eq!(
        game.submit("p1", build("Stockade")),
        Err(ActionError::CannotAfford)
    );
    assert_eq!(game.pending_action("p1"), Some(build("Lumber Yard")));
}
