use super::game_helpers::{build, discard, discard_first, discard_turns, game_with, stage};
use crate::domain::PointCategory::CivilianP;
use crate::game::{Action, ActionError, ExtraTurnKind, Game, Phase, Side};

fn babylon_b() -> Game {
    game_with(&[
        ("Babylon", Side::B),
        ("Rhódos", Side::A),
        ("Éphesos", Side::A),
    ])
}

fn halikarnassos() -> Game {
    game_with(&[
        ("Halikarnassós", Side::A),
        ("Rhódos", Side::A),
        ("Éphesos", Side::A),
    ])
}

fn from_discard(card: &str) -> Action {
    Action::BuildFromDiscard {
        card: card.to_string(),
    }
}

fn extra_turn(player: &str, kind: ExtraTurnKind) -> Phase {
    Phase::ExtraTurn {
        player: player.to_string(),
        kind,
    }
}

/// Halikarnassós A: stage 1 done, and the 3 Ore for stage 2 available.
fn ready_for_halikarnassos_stage_two(game: &mut Game) {
    game.give_stages("p1", 1);
    game.give("p1", "Foundry");
    game.give("p1", "Ore Vein");
}

#[test]
fn babylon_b_plays_the_seventh_card() {
    let mut game = babylon_b();
    game.give_stages("p1", 2);
    discard_turns(&mut game, 6);
    assert_eq!(game.phase(), &extra_turn("p1", ExtraTurnKind::PlayLastCard));
    assert_eq!(game.hand_names("p1").len(), 1);
    assert!(game.hand_names("p2").is_empty());
    assert_eq!(game.discard_names().len(), 18 + 2);
    let last = game.hand_names("p1")[0].clone();
    assert_eq!(
        game.submit("p2", discard("Altar")),
        Err(ActionError::NotYourTurn)
    );
    assert_eq!(
        game.validate("p1", &Action::BuildFree { card: last.clone() }),
        Err(ActionError::ActionNotAllowedNow)
    );
    assert_eq!(
        game.validate("p1", &from_discard(&last)),
        Err(ActionError::ActionNotAllowedNow)
    );
    game.submit("p1", discard(&last)).unwrap();
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 2, turn: 1 });
    assert_eq!(game.discard_names().len(), 21);
}

#[test]
fn babylon_b_can_build_its_seventh_card() {
    let mut game = babylon_b();
    game.give_stages("p1", 2);
    discard_turns(&mut game, 6);
    game.set_hand("p1", &["Lumber Yard"]);
    game.submit("p1", build("Lumber Yard")).unwrap();
    assert_eq!(game.built_names("p1"), vec!["Lumber Yard".to_string()]);
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 2, turn: 1 });
}

#[test]
fn babylon_b_stage_built_on_turn_six_grants_the_seventh_card_that_age() {
    // Babylon B stage 2 costs Glass + 2 Wood.
    let mut game = babylon_b();
    game.give_stages("p1", 1);
    game.give("p1", "Glassworks");
    game.give("p1", "Sawmill");
    discard_turns(&mut game, 5);
    game.set_hand("p1", &["Altar", "Theater"]);
    game.submit("p1", stage("Altar")).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    assert_eq!(game.phase(), &extra_turn("p1", ExtraTurnKind::PlayLastCard));
    assert_eq!(game.hand_names("p1"), vec!["Theater".to_string()]);
}

#[test]
fn halikarnassos_builds_from_the_discard_pile_at_the_end_of_the_turn() {
    let mut game = halikarnassos();
    ready_for_halikarnassos_stage_two(&mut game);
    game.set_discard(&["Palace", "Altar"]);
    game.set_hand("p2", &["Pawnshop"]);
    let tucked = game.hand_names("p1")[0].clone();
    game.submit("p1", stage(&tucked)).unwrap();
    game.submit("p2", discard("Pawnshop")).unwrap();
    discard_first(&mut game, "p3");
    assert_eq!(
        game.phase(),
        &extra_turn("p1", ExtraTurnKind::BuildFromDiscard)
    );
    // Cards discarded this very turn are eligible.
    assert_eq!(game.validate("p1", &from_discard("Pawnshop")), Ok(()));
    assert_eq!(
        game.validate("p1", &discard("Altar")),
        Err(ActionError::ActionNotAllowedNow)
    );
    assert_eq!(
        game.validate("p1", &from_discard("Senate")),
        Err(ActionError::CardNotInDiscard)
    );
    game.submit("p1", from_discard("Palace")).unwrap();
    assert!(game.built_names("p1").contains(&"Palace".to_string()));
    assert!(!game.discard_names().contains(&"Palace".to_string()));
    assert_eq!(game.points("p1").get(&CivilianP), Some(&8));
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 2 });
    assert_eq!(game.hand_names("p1").len(), 6);
}

#[test]
fn halikarnassos_skips_the_extra_turn_when_nothing_is_buildable() {
    let mut game = halikarnassos();
    ready_for_halikarnassos_stage_two(&mut game);
    game.give("p1", "Altar");
    game.set_discard(&["Altar"]);
    game.set_hand("p2", &["Lumber Yard"]);
    game.set_hand("p3", &["Stone Pit"]);
    let tucked = game.hand_names("p1")[0].clone();
    game.submit("p1", stage(&tucked)).unwrap();
    game.submit("p2", build("Lumber Yard")).unwrap();
    game.submit("p3", build("Stone Pit")).unwrap();
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 2 });
}

#[test]
fn halikarnassos_at_the_end_of_an_age_can_take_the_discarded_last_cards() {
    let mut game = halikarnassos();
    ready_for_halikarnassos_stage_two(&mut game);
    discard_turns(&mut game, 5);
    game.set_discard(&[]);
    game.set_hand("p2", &["Pawnshop", "Palace"]);
    let tucked = game.hand_names("p1")[0].clone();
    game.submit("p1", stage(&tucked)).unwrap();
    game.submit("p2", discard("Pawnshop")).unwrap();
    discard_first(&mut game, "p3");
    assert_eq!(
        game.phase(),
        &extra_turn("p1", ExtraTurnKind::BuildFromDiscard)
    );
    assert_eq!(game.validate("p1", &from_discard("Palace")), Ok(()));
    game.submit("p1", from_discard("Palace")).unwrap();
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 2, turn: 1 });
}

#[test]
fn babylon_plays_its_last_card_before_halikarnassos_builds_from_discard() {
    let mut game = game_with(&[
        ("Halikarnassós", Side::A),
        ("Babylon", Side::B),
        ("Éphesos", Side::A),
    ]);
    ready_for_halikarnassos_stage_two(&mut game);
    game.give_stages("p2", 2);
    discard_turns(&mut game, 5);
    game.set_discard(&[]);
    let tucked = game.hand_names("p1")[0].clone();
    game.submit("p1", stage(&tucked)).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    assert_eq!(game.phase(), &extra_turn("p2", ExtraTurnKind::PlayLastCard));
    game.set_hand("p2", &["Palace"]);
    game.submit("p2", discard("Palace")).unwrap();
    assert_eq!(
        game.phase(),
        &extra_turn("p1", ExtraTurnKind::BuildFromDiscard)
    );
    assert_eq!(game.validate("p1", &from_discard("Palace")), Ok(()));
}
