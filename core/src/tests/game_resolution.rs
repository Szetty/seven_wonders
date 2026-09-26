use super::game_helpers::{
    build, build_paying, discard, discard_first, discard_turn, plain_game, stage, PLAYERS,
};
use crate::domain::PointCategory::WonderP;
use crate::game::{ActionError, Phase, ResourceType::*};

#[test]
fn a_turn_resolves_only_when_everyone_has_submitted() {
    let mut game = plain_game();
    discard_first(&mut game, "p1");
    discard_first(&mut game, "p2");
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 1 });
    discard_first(&mut game, "p3");
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 2 });
}

#[test]
fn discarding_pays_three_coins_and_fills_the_discard_pile() {
    let mut game = plain_game();
    discard_turn(&mut game);
    for player in PLAYERS {
        assert_eq!(game.coins(player), 6);
        assert_eq!(game.hand_names(player).len(), 6);
    }
    assert_eq!(game.discard_names().len(), 3);
}

#[test]
fn building_pays_the_coin_cost_to_the_bank() {
    let mut game = plain_game();
    game.set_hand("p1", &["Tree Farm"]);
    game.submit("p1", build("Tree Farm")).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    assert_eq!(game.coins("p1"), 2);
    assert_eq!(game.built_names("p1"), vec!["Tree Farm".to_string()]);
}

#[test]
fn trade_coins_go_to_the_neighbour() {
    let mut game = plain_game();
    game.set_hand("p1", &["Barracks"]);
    game.set_hand("p2", &["Lumber Yard"]);
    game.submit("p1", build_paying("Barracks", &[], &[(Ore, 1)]))
        .unwrap();
    game.submit("p2", build("Lumber Yard")).unwrap();
    discard_first(&mut game, "p3");
    assert_eq!(game.coins("p1"), 1);
    assert_eq!(game.coins("p2"), 5);
    assert_eq!(game.coins("p3"), 6);
    assert_eq!(game.shields("p1"), 1);
}

#[test]
fn coins_received_this_turn_cannot_be_spent_this_turn() {
    // p2 must buy Papyrus from p3 (its east neighbour) but has 0 coins before
    // the turn, even though p1 pays p2 two coins in this same turn.
    let mut game = plain_game();
    game.set_coins("p2", 0);
    game.set_hand("p1", &["Barracks"]);
    game.set_hand("p2", &["Scriptorium"]);
    game.submit("p1", build_paying("Barracks", &[], &[(Ore, 1)]))
        .unwrap();
    assert_eq!(
        game.submit("p2", build_paying("Scriptorium", &[], &[(Papyrus, 1)])),
        Err(ActionError::CannotAfford)
    );
    game.submit("p2", discard("Scriptorium")).unwrap();
    discard_first(&mut game, "p3");
    assert_eq!(game.coins("p2"), 5); // 0 + 3 (discard) + 2 (paid by p1)
}

#[test]
fn structure_effects_apply_on_resolution() {
    let mut game = plain_game();
    game.set_hand("p1", &["Tavern"]);
    game.submit("p1", build("Tavern")).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    assert_eq!(game.coins("p1"), 8);
}

#[test]
fn a_wonder_stage_tucks_the_card() {
    let mut game = plain_game();
    game.give("p1", "Stone Pit");
    game.set_hand("p1", &["Altar"]);
    game.submit("p1", stage("Altar")).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    assert_eq!(game.stages_built("p1"), 1);
    assert!(!game.built_names("p1").contains(&"Altar".to_string()));
    assert_eq!(game.points("p1").get(&WonderP), Some(&3));
}

#[test]
fn hands_pass_west_in_age_one() {
    let mut game = plain_game();
    let before: Vec<Vec<String>> = PLAYERS.iter().map(|p| game.hand_names(p)).collect();
    discard_turn(&mut game);
    // Seat i passes to seat i-1, so p1 receives p2's hand, p2 gets p3's, p3 gets p1's.
    assert_eq!(game.hand_names("p1"), before[1][1..].to_vec());
    assert_eq!(game.hand_names("p2"), before[2][1..].to_vec());
    assert_eq!(game.hand_names("p3"), before[0][1..].to_vec());
}
