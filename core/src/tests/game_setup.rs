use super::game_helpers::{game_with, names, plain_game};
use crate::game::{Game, Phase, SetupError, Side, WonderSelection};

fn explicit(wonders: &[(&str, Side)]) -> WonderSelection {
    WonderSelection::Explicit(
        wonders
            .iter()
            .map(|(name, side)| (name.to_string(), *side))
            .collect(),
    )
}

fn deal_fingerprint(game: &Game) -> String {
    game.players()
        .iter()
        .map(|player| {
            let (wonder, side) = game.wonder_of(player);
            format!(
                "{player}:{wonder}/{side:?}:{}",
                game.hand_names(player).join(",")
            )
        })
        .collect::<Vec<_>>()
        .join(";")
}

#[test]
fn player_count_must_be_three_to_seven() {
    assert_eq!(
        Game::new(names(2), WonderSelection::Random, 1).err(),
        Some(SetupError::InvalidPlayersNumber(2))
    );
    assert_eq!(
        Game::new(names(8), WonderSelection::Random, 1).err(),
        Some(SetupError::InvalidPlayersNumber(8))
    );
    for count in 3..=7 {
        assert!(Game::new(names(count), WonderSelection::Random, 1).is_ok());
    }
}

#[test]
fn player_names_must_be_unique() {
    let players = vec!["a".to_string(), "b".to_string(), "a".to_string()];
    assert_eq!(
        Game::new(players, WonderSelection::Random, 1).err(),
        Some(SetupError::DuplicatePlayer("a".to_string()))
    );
}

#[test]
fn explicit_wonders_are_validated() {
    assert_eq!(
        Game::new(names(3), explicit(&[("Gizah", Side::A)]), 1).err(),
        Some(SetupError::WondersLengthMismatch {
            players: 3,
            wonders: 1
        })
    );
    assert_eq!(
        Game::new(
            names(3),
            explicit(&[
                ("Gizah", Side::A),
                ("Rhodos", Side::A),
                ("Babylon", Side::B)
            ]),
            1
        )
        .err(),
        Some(SetupError::InvalidWonder("Rhodos".to_string()))
    );
    assert_eq!(
        Game::new(
            names(3),
            explicit(&[("Gizah", Side::A), ("Gizah", Side::B), ("Babylon", Side::B)]),
            1
        )
        .err(),
        Some(SetupError::DuplicateWonder("Gizah".to_string()))
    );
}

#[test]
fn explicit_wonders_are_assigned_in_seat_order() {
    let game = game_with(&[
        ("Gizah", Side::A),
        ("Alexandria", Side::A),
        ("Babylon", Side::B),
    ]);
    assert_eq!(game.wonder_of("p1"), ("Gizah".to_string(), Side::A));
    assert_eq!(game.wonder_of("p2"), ("Alexandria".to_string(), Side::A));
    assert_eq!(game.wonder_of("p3"), ("Babylon".to_string(), Side::B));
}

#[test]
fn random_wonders_are_distinct() {
    for seed in 0..20 {
        let game = Game::new(names(7), WonderSelection::Random, seed).unwrap();
        let mut wonders: Vec<String> = game.players().iter().map(|p| game.wonder_of(p).0).collect();
        wonders.sort();
        wonders.dedup();
        assert_eq!(wonders.len(), 7);
    }
}

#[test]
fn every_player_starts_with_seven_cards_and_three_coins() {
    let game = plain_game();
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 1 });
    for player in game.players() {
        assert_eq!(game.hand_names(player).len(), 7);
        assert_eq!(game.coins(player), 3);
    }
}

#[test]
fn same_seed_same_deal_and_different_seed_different_deal() {
    let deal =
        |seed| deal_fingerprint(&Game::new(names(5), WonderSelection::Random, seed).unwrap());
    assert_eq!(deal(99), deal(99));
    assert_ne!(deal(99), deal(100));
}

#[test]
fn extreme_seeds_start_games() {
    for seed in [0, u64::MAX] {
        assert!(Game::new(names(3), WonderSelection::Random, seed).is_ok());
    }
}

/// Guards against accidental changes to RNG consumption order.
/// If this changes intentionally, bump `ENGINE_VERSION` and re-pin.
/// Pinned 2026-09-26 (Task 9).
const PINNED_DEAL_SEED_42: &str = "p1:Alexandria/A:Baths,Workshop,Clay Pit,Barracks,Stockade,Scriptorium,Loom;p2:Gizah/A:Marketplace,West trading post,Guard tower,Press,Glassworks,Clay Pool,Theater;p3:Babylon/A:Timber Yard,Altar,Ore Vein,East trading post,Apothecary,Lumber Yard,Stone Pit";

#[test]
fn seed_42_deal_is_pinned() {
    let game = Game::new(names(3), WonderSelection::Random, 42).unwrap();
    assert_eq!(
        deal_fingerprint(&game),
        PINNED_DEAL_SEED_42,
        "RNG consumption order changed; bump ENGINE_VERSION and re-pin if intentional"
    );
}
