use super::game_helpers::{discard_first, discard_turns, game_with};
use crate::game::{Action, ActionError, Game, Phase, Side};

fn olympia(side: Side) -> Game {
    game_with(&[("Olympía", side), ("Rhódos", Side::A), ("Éphesos", Side::A)])
}

fn build_free(card: &str) -> Action {
    Action::BuildFree {
        card: card.to_string(),
    }
}

fn final_guild_and_science(game: &Game, player: &str) -> (i32, i32) {
    let Phase::GameOver { scores } = game.phase() else {
        panic!("expected game over");
    };
    let score = scores.iter().find(|s| s.player == player).unwrap();
    (score.guild, score.scientific)
}

#[test]
fn olympia_a_builds_one_card_free_per_age() {
    let mut game = olympia(Side::A);
    game.set_hand("p1", &["Palace"]);
    assert_eq!(
        game.validate("p1", &build_free("Palace")),
        Err(ActionError::FreeBuildUnavailable)
    );
    game.give_stages("p1", 2);
    game.submit("p1", build_free("Palace")).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    assert_eq!(game.built_names("p1"), vec!["Palace".to_string()]);
    assert_eq!(game.coins("p1"), 3);
    let next = game.hand_names("p1")[0].clone();
    assert_eq!(
        game.validate("p1", &build_free(&next)),
        Err(ActionError::FreeBuildUnavailable)
    );
    discard_turns(&mut game, 5);
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 2, turn: 1 });
    let first = game.hand_names("p1")[0].clone();
    assert_eq!(game.validate("p1", &build_free(&first)), Ok(()));
}

#[test]
fn olympia_a_cannot_free_build_a_duplicate() {
    let mut game = olympia(Side::A);
    game.give_stages("p1", 2);
    game.give("p1", "Altar");
    game.set_hand("p1", &["Altar"]);
    assert_eq!(
        game.validate("p1", &build_free("Altar")),
        Err(ActionError::AlreadyBuilt)
    );
}

#[test]
fn olympia_b_copies_the_best_neighbouring_guild() {
    // p1's neighbours are p3 (west) and p2 (east).
    let mut game = olympia(Side::B);
    game.give_stages("p1", 3);
    game.give("p2", "Workers Guild");
    game.give("p3", "Magistrates Guild");
    for card in ["Lumber Yard", "Stone Pit", "Clay Pool"] {
        game.give("p2", card);
    }
    game.give("p3", "Altar");
    // Workers: 3 brown cards next door → 3; Magistrates: 1 blue card → 1.
    assert_eq!(game.chosen_guild("p1"), Some("Workers Guild".to_string()));
    game.finish_now();
    assert_eq!(final_guild_and_science(&game, "p1").0, 3);
}

#[test]
fn copying_scientists_guild_counts_as_science() {
    let mut game = olympia(Side::B);
    game.give_stages("p1", 3);
    for card in [
        "Scriptorium",
        "School",
        "Workshop",
        "Laboratory",
        "Apothecary",
    ] {
        game.give("p1", card);
    }
    game.give("p2", "Workers Guild");
    for card in ["Lumber Yard", "Stone Pit", "Clay Pool"] {
        game.give("p2", card);
    }
    game.give("p3", "Scientists Guild");
    // 2 tablets, 2 gears, 1 compass = 16; one more compass = 26 (+10 > +3).
    assert_eq!(
        game.chosen_guild("p1"),
        Some("Scientists Guild".to_string())
    );
    game.finish_now();
    assert_eq!(final_guild_and_science(&game, "p1"), (0, 26));
}

#[test]
fn equal_candidates_break_alphabetically() {
    let mut game = olympia(Side::B);
    game.give_stages("p1", 3);
    game.give("p2", "Spies Guild");
    game.give("p3", "Craftsmens Guild");
    assert_eq!(
        game.chosen_guild("p1"),
        Some("Craftsmens Guild".to_string())
    );
}

#[test]
fn no_copy_without_the_ability_or_an_eligible_guild() {
    let mut game = olympia(Side::B);
    game.give("p2", "Workers Guild");
    assert_eq!(game.chosen_guild("p1"), None);
    game.give_stages("p1", 3);
    game.give("p1", "Workers Guild");
    assert_eq!(game.chosen_guild("p1"), None);
}

#[test]
fn evaluating_candidates_leaves_scores_unchanged() {
    let mut game = olympia(Side::B);
    game.give_stages("p1", 3);
    game.give("p2", "Workers Guild");
    game.give("p3", "Scientists Guild");
    let before = game.score_now();
    assert!(game.chosen_guild("p1").is_some());
    assert_eq!(game.score_now(), before);
}
