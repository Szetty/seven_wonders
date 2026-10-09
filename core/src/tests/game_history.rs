use super::game_helpers::{
    build_paying, discard_age, discard_first, discard_turns, game_with, plain_game, stage,
};
use crate::game::{
    Action, Battle, CoinChange, HistoryEvent, Play, PublicAction, ResourceType::*, Side, TurnKind,
};

fn last_turn(game: &crate::game::Game) -> (u8, u8, TurnKind, Vec<Play>, Vec<CoinChange>) {
    game.history()
        .iter()
        .rev()
        .find_map(|event| match event {
            HistoryEvent::Turn {
                age,
                turn,
                kind,
                plays,
                coins,
            } => Some((*age, *turn, *kind, plays.clone(), coins.clone())),
            HistoryEvent::Battles { .. } => None,
        })
        .expect("a turn was recorded")
}

#[test]
fn a_turn_records_public_actions_trades_and_coin_changes() {
    // p1 Gizah (west p3, east p2); p2 Rhódos produces Ore.
    let mut game = plain_game();
    game.set_hand("p1", &["Barracks"]);
    game.submit("p1", build_paying("Barracks", &[], &[(Ore, 1)]))
        .unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");

    let (age, turn, kind, plays, coins) = last_turn(&game);
    assert_eq!((age, turn, kind), (1, 1, TurnKind::Regular));
    assert_eq!(
        plays,
        vec![
            Play {
                player: "p1".into(),
                action: PublicAction::Built {
                    card: "Barracks".into()
                },
                coins_to_west: 0,
                coins_to_east: 2,
            },
            Play {
                player: "p2".into(),
                action: PublicAction::Discarded,
                coins_to_west: 0,
                coins_to_east: 0
            },
            Play {
                player: "p3".into(),
                action: PublicAction::Discarded,
                coins_to_west: 0,
                coins_to_east: 0
            },
        ]
    );
    assert_eq!(
        coins,
        vec![
            CoinChange {
                player: "p1".into(),
                delta: -2
            },
            CoinChange {
                player: "p2".into(),
                delta: 5
            },
            CoinChange {
                player: "p3".into(),
                delta: 3
            },
        ]
    );
}

#[test]
fn wonder_stages_hide_the_card() {
    let mut game = plain_game();
    game.give("p1", "Stone Pit");
    game.set_hand("p1", &["Altar"]);
    game.submit("p1", stage("Altar")).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    let (_, _, _, plays, _) = last_turn(&game);
    assert_eq!(plays[0].action, PublicAction::WonderStage { stage: 1 });
    let text = format!("{:?}", game.history());
    assert!(!text.contains("Altar"), "the stage card leaked: {text}");
}

#[test]
fn olympia_free_builds_are_recorded() {
    let mut game = game_with(&[
        ("Olympía", Side::A),
        ("Rhódos", Side::A),
        ("Éphesos", Side::A),
    ]);
    game.give_stages("p1", 2);
    game.set_hand("p1", &["Palace"]);
    game.submit(
        "p1",
        Action::BuildFree {
            card: "Palace".into(),
        },
    )
    .unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    let (_, _, _, plays, _) = last_turn(&game);
    assert_eq!(
        plays[0].action,
        PublicAction::BuiltFree {
            card: "Palace".into()
        }
    );
}

#[test]
fn babylon_b_last_card_is_its_own_turn() {
    let mut game = game_with(&[
        ("Babylon", Side::B),
        ("Rhódos", Side::A),
        ("Éphesos", Side::A),
    ]);
    game.give_stages("p1", 2);
    discard_turns(&mut game, 6);
    let last = game.hand_names("p1")[0].clone();
    game.submit("p1", Action::Discard { card: last }).unwrap();
    let turns: Vec<&HistoryEvent> = game
        .history()
        .iter()
        .filter(|e| {
            matches!(
                e,
                HistoryEvent::Turn {
                    kind: TurnKind::PlayLastCard,
                    ..
                }
            )
        })
        .collect();
    assert_eq!(turns.len(), 1);
    let HistoryEvent::Turn {
        plays, coins, turn, ..
    } = turns[0]
    else {
        unreachable!()
    };
    assert_eq!(*turn, 6);
    assert_eq!(plays.len(), 1);
    assert_eq!(plays[0].action, PublicAction::Discarded);
    assert_eq!(
        coins,
        &vec![CoinChange {
            player: "p1".into(),
            delta: 3
        }]
    );
}

#[test]
fn halikarnassos_builds_from_the_discard_pile_in_its_own_turn() {
    let mut game = game_with(&[
        ("Halikarnassós", Side::A),
        ("Rhódos", Side::A),
        ("Éphesos", Side::A),
    ]);
    game.give_stages("p1", 1);
    game.give("p1", "Foundry");
    game.give("p1", "Ore Vein");
    let card = game.hand_names("p1")[0].clone();
    game.submit("p1", stage(&card)).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    let built = game.built_names("p1");
    let pick = game
        .discard_names()
        .into_iter()
        .find(|name| !built.contains(name))
        .expect("a buildable discard");
    game.submit("p1", Action::BuildFromDiscard { card: pick.clone() })
        .unwrap();
    let (_, _, kind, plays, _) = last_turn(&game);
    assert_eq!(kind, TurnKind::BuildFromDiscard);
    assert_eq!(
        plays,
        vec![Play {
            player: "p1".into(),
            action: PublicAction::BuiltFromDiscard { card: pick },
            coins_to_west: 0,
            coins_to_east: 0,
        }]
    );
}

#[test]
fn each_age_ends_with_one_battle_per_neighbouring_pair() {
    let mut game = plain_game();
    game.give("p1", "Stockade");
    discard_age(&mut game);
    assert_eq!(game.history().len(), 7);
    let HistoryEvent::Battles { age, battles } = &game.history()[6] else {
        panic!("expected battles last");
    };
    assert_eq!(*age, 1);
    assert_eq!(
        battles,
        &vec![
            Battle {
                player: "p1".into(),
                rival: "p2".into(),
                player_strength: 1,
                rival_strength: 0,
                player_token: Some(1),
                rival_token: Some(-1)
            },
            Battle {
                player: "p2".into(),
                rival: "p3".into(),
                player_strength: 0,
                rival_strength: 0,
                player_token: None,
                rival_token: None
            },
            Battle {
                player: "p3".into(),
                rival: "p1".into(),
                player_strength: 0,
                rival_strength: 1,
                player_token: Some(-1),
                rival_token: Some(1)
            },
        ]
    );
}
