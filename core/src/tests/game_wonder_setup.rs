use super::game_helpers::{build, claim, discard, names, plain_game, stage, PLAYERS};
use crate::game::{
    Action, ActionError, Game, PassDirection, Phase, PhaseKind, SetupMode, SetupSeat, Side,
    WonderSelection,
};

/// `WONDERS` order.
const ALL_WONDERS: [&str; 7] = [
    "Rhódos",
    "Alexandria",
    "Éphesos",
    "Babylon",
    "Olympía",
    "Halikarnassós",
    "Gizah",
];

fn choose_board() -> Game {
    Game::new(names(3), WonderSelection::ChooseBoard, 42).unwrap()
}

/// Seed 42 draws the same boards as the pinned `Random` deal:
/// p1 Alexandria, p2 Gizah, p3 Babylon.
fn choose_side() -> Game {
    Game::new(names(3), WonderSelection::ChooseSide, 42).unwrap()
}

fn seat(player: &str, wonder: Option<&str>, side: Option<Side>, confirmed: bool) -> SetupSeat {
    SetupSeat {
        player: player.to_string(),
        wonder: wonder.map(str::to_string),
        side,
        confirmed,
    }
}

fn seats(game: &Game) -> Vec<SetupSeat> {
    game.view("p1")
        .unwrap()
        .setup
        .expect("game is in setup")
        .seats
}

fn available(game: &Game) -> Vec<String> {
    game.view("p1")
        .unwrap()
        .setup
        .expect("game is in setup")
        .available
}

#[test]
fn the_choose_board_view_follows_the_contract() {
    let game = choose_board();
    assert_eq!(game.phase(), &Phase::Setup);
    for player in game.players() {
        assert!(
            game.view(player).is_ok(),
            "view for {player} must not panic"
        );
    }
    assert!(game.debug_json().contains("\"Setup\""));
    assert!(game.history().is_empty());

    let view = game.view("p2").unwrap();
    assert_eq!(view.me, "p2");
    assert_eq!(view.phase.kind, PhaseKind::Setup);
    assert_eq!((view.phase.age, view.phase.turn), (1, 1));
    assert_eq!(view.phase.direction, PassDirection::West);
    assert_eq!(view.phase.extra_turn_player, None);
    assert_eq!(view.phase.extra_turn_kind, None);
    assert!(view.players.is_empty());
    assert!(view.hand.is_empty());
    assert_eq!(view.discard_pile, None);
    assert_eq!(view.discard_count, 0);
    assert_eq!(view.my_pending, None);
    assert_eq!(view.scores, None);
    assert_eq!((view.west.as_str(), view.east.as_str()), ("p1", "p3"));
    assert_eq!(
        view.submitted,
        vec![
            ("p1".to_string(), false),
            ("p2".to_string(), false),
            ("p3".to_string(), false)
        ]
    );
    let setup = view.setup.expect("setup view");
    assert_eq!(setup.mode, SetupMode::ChooseBoard);
    assert_eq!(
        setup.seats,
        vec![
            seat("p1", None, None, false),
            seat("p2", None, None, false),
            seat("p3", None, None, false),
        ]
    );
    assert_eq!(setup.available, ALL_WONDERS.map(String::from).to_vec());
}

#[test]
fn choose_side_seats_start_with_their_drawn_board() {
    let game = choose_side();
    let setup = game.view("p3").unwrap().setup.expect("setup view");
    assert_eq!(setup.mode, SetupMode::ChooseSide);
    assert!(setup.available.is_empty());
    assert_eq!(
        setup.seats,
        vec![
            seat("p1", Some("Alexandria"), None, false),
            seat("p2", Some("Gizah"), None, false),
            seat("p3", Some("Babylon"), None, false),
        ]
    );
}

#[test]
fn card_actions_are_not_allowed_during_setup() {
    for mut game in [choose_board(), choose_side()] {
        for action in [
            build("Altar"),
            stage("Altar"),
            discard("Altar"),
            Action::BuildFree {
                card: "Altar".to_string(),
            },
            Action::BuildFromDiscard {
                card: "Altar".to_string(),
            },
        ] {
            assert_eq!(
                game.validate("p1", &action),
                Err(ActionError::ActionNotAllowedNow)
            );
            assert_eq!(
                game.submit("p1", action),
                Err(ActionError::ActionNotAllowedNow)
            );
        }
        assert_eq!(
            game.submit("nobody", claim("Gizah", Side::A)),
            Err(ActionError::UnknownPlayer)
        );
        assert_eq!(game.phase(), &Phase::Setup);
    }
}

#[test]
fn a_claim_reserves_the_board_and_can_be_flipped_or_switched() {
    let mut game = choose_board();
    game.submit("p1", claim("Gizah", Side::A)).unwrap();
    assert_eq!(
        seats(&game)[0],
        seat("p1", Some("Gizah"), Some(Side::A), false)
    );
    assert_eq!(available(&game).len(), 6);
    assert!(!available(&game).contains(&"Gizah".to_string()));

    game.submit("p1", claim("Gizah", Side::B)).unwrap();
    assert_eq!(
        seats(&game)[0],
        seat("p1", Some("Gizah"), Some(Side::B), false)
    );

    game.submit("p1", claim("Babylon", Side::A)).unwrap();
    assert_eq!(
        seats(&game)[0],
        seat("p1", Some("Babylon"), Some(Side::A), false)
    );
    assert!(available(&game).contains(&"Gizah".to_string()));
    assert!(!available(&game).contains(&"Babylon".to_string()));

    game.submit("p2", claim("Gizah", Side::A)).unwrap();
    assert_eq!(
        seats(&game)[1],
        seat("p2", Some("Gizah"), Some(Side::A), false)
    );
    assert_eq!(game.pending_action("p1"), None);
    assert!(game.history().is_empty());
}

#[test]
fn a_held_board_cannot_be_claimed_by_another_seat() {
    let mut game = choose_board();
    game.submit("p1", claim("Gizah", Side::A)).unwrap();
    assert_eq!(
        game.submit("p2", claim("Gizah", Side::B)),
        Err(ActionError::WonderTaken)
    );
    assert_eq!(seats(&game)[1], seat("p2", None, None, false));

    game.submit("p1", Action::ConfirmWonder).unwrap();
    assert_eq!(
        game.submit("p3", claim("Gizah", Side::A)),
        Err(ActionError::WonderTaken)
    );
    assert_eq!(seats(&game)[2], seat("p3", None, None, false));
}

#[test]
fn unknown_boards_are_rejected() {
    for mut game in [choose_board(), choose_side()] {
        for name in ["Atlantis", "Rhodos", ""] {
            assert_eq!(
                game.submit("p1", claim(name, Side::A)),
                Err(ActionError::UnknownWonder)
            );
        }
        assert_eq!(seats(&game)[0].side, None);
    }
}

#[test]
fn choose_side_only_accepts_the_drawn_board() {
    let mut game = choose_side();
    assert_eq!(
        game.submit("p1", claim("Gizah", Side::A)),
        Err(ActionError::NotYourWonder)
    );
    assert_eq!(
        game.submit("p1", claim("Rhódos", Side::A)),
        Err(ActionError::NotYourWonder)
    );
    game.submit("p1", claim("Alexandria", Side::B)).unwrap();
    game.submit("p1", claim("Alexandria", Side::A)).unwrap();
    assert_eq!(
        seats(&game)[0],
        seat("p1", Some("Alexandria"), Some(Side::A), false)
    );
}

#[test]
fn confirming_needs_a_claim_and_happens_once() {
    for mut game in [choose_board(), choose_side()] {
        assert_eq!(
            game.submit("p1", Action::ConfirmWonder),
            Err(ActionError::NoClaim)
        );
    }
    let mut game = choose_board();
    game.submit("p1", claim("Gizah", Side::A)).unwrap();
    game.submit("p1", Action::ConfirmWonder).unwrap();
    assert_eq!(
        game.submit("p1", Action::ConfirmWonder),
        Err(ActionError::AlreadyConfirmed)
    );
    assert_eq!(
        game.submit("p1", claim("Babylon", Side::A)),
        Err(ActionError::AlreadyConfirmed)
    );
    assert_eq!(
        seats(&game)[0],
        seat("p1", Some("Gizah"), Some(Side::A), true)
    );
    assert_eq!(
        game.view("p2").unwrap().submitted,
        vec![
            ("p1".to_string(), true),
            ("p2".to_string(), false),
            ("p3".to_string(), false)
        ]
    );
}

#[test]
fn the_last_confirmation_starts_age_one() {
    let boards = [
        ("Babylon", Side::B),
        ("Gizah", Side::A),
        ("Rhódos", Side::A),
    ];
    let mut game = choose_board();
    for (player, (wonder, side)) in PLAYERS.iter().zip(boards) {
        game.submit(player, claim(wonder, side)).unwrap();
    }
    game.submit("p1", Action::ConfirmWonder).unwrap();
    game.submit("p2", Action::ConfirmWonder).unwrap();
    assert_eq!(game.phase(), &Phase::Setup);
    assert!(game.hand_names("p1").is_empty());

    game.submit("p3", Action::ConfirmWonder).unwrap();
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 1 });
    assert!(game.history().is_empty());
    for player in game.players() {
        assert_eq!(game.hand_names(player).len(), 7);
        assert_eq!(game.coins(player), 3);
    }
    assert_eq!(game.wonder_of("p1"), ("Babylon".to_string(), Side::B));
    assert_eq!(game.wonder_of("p3"), ("Rhódos".to_string(), Side::A));

    // Same seed, same boards, no wonder draws in either mode: the table is
    // identical to an explicit game (starting resources, coins, hands).
    let explicit = Game::new(
        names(3),
        WonderSelection::Explicit(
            boards
                .iter()
                .map(|(wonder, side)| (wonder.to_string(), *side))
                .collect(),
        ),
        42,
    )
    .unwrap();
    for player in game.players() {
        let view = game.view(player).unwrap();
        assert_eq!(view.setup, None);
        assert_eq!(view, explicit.view(player).unwrap());
    }
}

#[test]
fn choose_side_finishes_with_the_chosen_sides() {
    let mut game = choose_side();
    game.submit("p1", claim("Alexandria", Side::B)).unwrap();
    game.submit("p2", claim("Gizah", Side::A)).unwrap();
    game.submit("p3", claim("Babylon", Side::B)).unwrap();
    for player in PLAYERS {
        game.submit(player, Action::ConfirmWonder).unwrap();
    }
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 1 });
    assert_eq!(game.wonder_of("p1"), ("Alexandria".to_string(), Side::B));
    assert_eq!(game.wonder_of("p2"), ("Gizah".to_string(), Side::A));
    assert_eq!(game.wonder_of("p3"), ("Babylon".to_string(), Side::B));
    let view = game.view("p1").unwrap();
    assert_eq!(view.phase.kind, PhaseKind::ChoosingCards);
    assert_eq!(view.players.len(), 3);
    assert_eq!(view.hand.len(), 7);
    assert!(game.history().is_empty());
}

#[test]
fn setup_actions_are_not_allowed_outside_setup() {
    let mut game = plain_game();
    assert_eq!(
        game.submit("p1", claim("Gizah", Side::A)),
        Err(ActionError::ActionNotAllowedNow)
    );
    assert_eq!(
        game.submit("p1", Action::ConfirmWonder),
        Err(ActionError::ActionNotAllowedNow)
    );
    assert_eq!(game.pending_action("p1"), None);
    assert_eq!(game.view("p1").unwrap().setup, None);
}
