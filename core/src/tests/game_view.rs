use super::game_helpers::{
    discard, discard_age, discard_first, discard_turns, game_with, plain_game, stage,
};
use crate::game::{
    ActionError, BuildOption, ExtraTurnKind, PassDirection, Payment, PaymentOption, PhaseKind,
    ResourceType::*, Side,
};

#[test]
fn the_initial_view_describes_the_table() {
    let game = plain_game();
    let view = game.view("p1").unwrap();
    assert_eq!(view.me, "p1");
    assert_eq!((view.west.as_str(), view.east.as_str()), ("p3", "p2"));
    assert_eq!(view.phase.kind, PhaseKind::ChoosingCards);
    assert_eq!((view.phase.age, view.phase.turn), (1, 1));
    assert_eq!(view.phase.direction, PassDirection::West);
    assert_eq!(view.phase.extra_turn_player, None);
    assert_eq!(view.hand.len(), 7);
    assert_eq!(view.discard_pile, None);
    assert_eq!(view.discard_count, 0);
    assert_eq!(
        view.submitted,
        vec![
            ("p1".to_string(), false),
            ("p2".to_string(), false),
            ("p3".to_string(), false)
        ]
    );
    assert_eq!(view.my_pending, None);
    assert_eq!(view.scores, None);
    let names: Vec<&str> = view.players.iter().map(|p| p.name.as_str()).collect();
    assert_eq!(names, vec!["p1", "p2", "p3"]);
    let me = &view.players[0];
    assert_eq!((me.wonder.as_str(), me.side), ("Gizah", Side::A));
    assert_eq!((me.stages_built, me.stages_total), (0, 3));
    assert_eq!((me.coins, me.shields), (3, 0));
    assert!(me.built.is_empty() && me.military_tokens.is_empty());
    assert!(!me.free_build_available);
}

#[test]
fn hand_cards_carry_their_build_options() {
    let mut game = plain_game();
    game.give("p1", "Altar");
    game.set_hand(
        "p1",
        &["Lumber Yard", "Tree Farm", "Barracks", "Stockade", "Altar"],
    );
    let hand = game.view("p1").unwrap().hand;
    assert_eq!(hand[0].build, BuildOption::Free);
    assert_eq!(hand[1].build, BuildOption::Coins(1));
    assert_eq!(
        hand[2].build,
        BuildOption::Trade(vec![PaymentOption {
            payment: Payment {
                west: vec![],
                east: vec![(Ore, 1)]
            },
            west_coins: 0,
            east_coins: 2,
            bank_coins: 0,
        }])
    );
    assert_eq!(
        hand[3].build,
        BuildOption::Unavailable {
            reason: ActionError::CannotAfford
        }
    );
    assert_eq!(
        hand[4].build,
        BuildOption::Unavailable {
            reason: ActionError::AlreadyBuilt
        }
    );
    // Gizah A stage 1 needs 2 Stone; p1 has 1 and nobody sells Stone.
    assert!(hand.iter().all(|card| card.wonder_stage
        == BuildOption::Unavailable {
            reason: ActionError::CannotAfford
        }));
    assert!(hand.iter().all(|card| !card.free_build));
    assert_eq!((hand[0].age, hand[0].name.as_str()), (1, "Lumber Yard"));
}

#[test]
fn pending_choices_are_private_but_submission_is_public() {
    let mut game = plain_game();
    let card = game.hand_names("p1")[0].clone();
    game.submit("p1", discard(&card)).unwrap();
    let mine = game.view("p1").unwrap();
    assert_eq!(mine.my_pending, Some(discard(&card)));
    assert_eq!(mine.hand.len(), 7);
    let theirs = game.view("p2").unwrap();
    assert_eq!(theirs.my_pending, None);
    assert_eq!(theirs.submitted[0], ("p1".to_string(), true));
    assert_eq!(theirs.submitted[1], ("p2".to_string(), false));
}

#[test]
fn built_cards_coins_and_tokens_are_public() {
    let mut game = plain_game();
    game.give("p2", "Barracks");
    discard_age(&mut game);
    let view = game.view("p1").unwrap();
    assert_eq!(view.phase.direction, PassDirection::East);
    assert_eq!(view.discard_count, 21);
    let p2 = &view.players[1];
    assert_eq!(p2.built[0].name, "Barracks");
    assert_eq!(p2.built[0].age, 1);
    assert_eq!((p2.shields, p2.military_tokens.clone()), (1, vec![1, 1]));
    assert_eq!(p2.coins, 21);
}

#[test]
fn olympia_free_build_is_flagged() {
    let mut game = game_with(&[
        ("Olympía", Side::A),
        ("Rhódos", Side::A),
        ("Éphesos", Side::A),
    ]);
    game.give_stages("p1", 2);
    game.give("p1", "Altar");
    game.set_hand("p1", &["Altar", "Palace"]);
    let view = game.view("p1").unwrap();
    assert!(view.players[0].free_build_available);
    assert!(!view.hand[0].free_build);
    assert!(view.hand[1].free_build);
}

#[test]
fn others_during_an_extra_turn_can_view_but_not_act() {
    let mut game = game_with(&[
        ("Babylon", Side::B),
        ("Rhódos", Side::A),
        ("Éphesos", Side::A),
    ]);
    game.give_stages("p1", 2);
    discard_turns(&mut game, 6);
    let acting = game.view("p1").unwrap();
    assert_eq!(acting.phase.kind, PhaseKind::ExtraTurn);
    assert_eq!(acting.phase.extra_turn_player, Some("p1".to_string()));
    assert_eq!(
        acting.phase.extra_turn_kind,
        Some(ExtraTurnKind::PlayLastCard)
    );
    assert_eq!(acting.hand.len(), 1);
    assert_eq!(acting.submitted, vec![("p1".to_string(), false)]);
    let waiting = game.view("p2").unwrap();
    assert!(waiting.hand.is_empty());
    assert_eq!(waiting.discard_pile, None);
    assert_eq!(waiting.submitted, vec![("p1".to_string(), false)]);
    assert_eq!(
        game.submit("p2", discard("Altar")),
        Err(ActionError::NotYourTurn)
    );
}

#[test]
fn halikarnassos_sees_only_buildable_discards() {
    let mut game = game_with(&[
        ("Halikarnassós", Side::A),
        ("Rhódos", Side::A),
        ("Éphesos", Side::A),
    ]);
    game.give_stages("p1", 1);
    game.give("p1", "Foundry");
    game.give("p1", "Ore Vein");
    game.give("p1", "Altar");
    game.set_discard(&["Palace", "Altar", "Palace"]);
    game.set_hand("p2", &["Pawnshop"]);
    let tucked = game.hand_names("p1")[0].clone();
    game.submit("p1", stage(&tucked)).unwrap();
    game.submit("p2", discard("Pawnshop")).unwrap();
    game.set_hand("p3", &["Tavern"]);
    game.submit("p3", discard("Tavern")).unwrap();
    let view = game.view("p1").unwrap();
    assert!(view.hand.is_empty());
    assert_eq!(
        view.discard_pile,
        Some(vec![
            "Palace".to_string(),
            "Pawnshop".to_string(),
            "Tavern".to_string()
        ])
    );
    assert_eq!(game.view("p2").unwrap().discard_pile, None);
}

#[test]
fn game_over_view_has_scores_and_no_hand() {
    let mut game = plain_game();
    for _ in 0..3 {
        discard_age(&mut game);
    }
    let view = game.view("p2").unwrap();
    assert_eq!(view.phase.kind, PhaseKind::GameOver);
    assert!(view.hand.is_empty());
    assert!(view.submitted.is_empty());
    assert_eq!(view.scores.map(|scores| scores.len()), Some(3));
}

#[test]
fn unknown_viewer_is_rejected() {
    let mut game = plain_game();
    discard_first(&mut game, "p1");
    assert_eq!(game.view("zed").err(), Some(ActionError::UnknownPlayer));
}
