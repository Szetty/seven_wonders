use crate::api::{self, ErrorType, WonderSideChoice};
use crate::domain::{
    BattleTokens, Cards, Events, GameState, MilitarySymbolCount, Player, PlayerState,
    ResourceType::*, WonderStagesBuilt,
};
use crate::engine::data::WONDER_NAMES;

fn players(names: &[&str]) -> Vec<String> {
    names.iter().map(|name| name.to_string()).collect()
}

fn choice(wonder_name: &str, side_b: bool) -> WonderSideChoice {
    WonderSideChoice {
        wonder_name: wonder_name.to_string(),
        side_b,
    }
}

#[test]
fn test_game_settings() {
    let game_settings = api::game_settings();
    assert_eq!(game_settings.version, env!("CARGO_PKG_VERSION"));
    assert_eq!(game_settings.wonders, *WONDER_NAMES);
}

#[test]
fn test_start_game_with_random_wonders() {
    let safe_game_state = api::start_game(players(&["a", "b", "c"]), vec![]).unwrap();
    let game_state = safe_game_state.lock().unwrap();
    test_game_state(&game_state);

    for name in ["a", "b", "c"] {
        let player_state = game_state.player_states.get(name).unwrap();
        assert_eq!(player_state.player, Player(name.to_string()));
        test_player_state(player_state);
    }
}

#[test]
fn test_start_game_with_seven_random_wonders_uses_each_wonder_once() {
    let safe_game_state =
        api::start_game(players(&["a", "b", "c", "d", "e", "f", "g"]), vec![]).unwrap();
    let game_state = safe_game_state.lock().unwrap();
    let mut wonders: Vec<String> = game_state
        .player_states
        .values()
        .map(|player_state| {
            player_state
                .wonder
                .name()
                .split(" - ")
                .next()
                .unwrap()
                .to_string()
        })
        .collect();
    wonders.sort();
    let mut expected = WONDER_NAMES.clone();
    expected.sort();
    assert_eq!(wonders, expected);
}

#[test]
fn test_start_game_with_specific_wonders() {
    let wonder_sides = vec![
        choice("Gizah", false),
        choice("Alexandria", false),
        choice("Babylon", true),
    ];
    let safe_game_state = api::start_game(players(&["a", "b", "c"]), wonder_sides).unwrap();
    let game_state = safe_game_state.lock().unwrap();
    test_game_state(&game_state);

    {
        let player_state_a = game_state.player_states.get("a").unwrap();
        assert_eq!(player_state_a.player, Player("a".to_string()));
        test_player_state(player_state_a);
        assert_eq!(player_state_a.wonder.name(), "Gizah - A");
        assert_eq!(
            *player_state_a
                .resources_produced
                .single_resources
                .get(&Stone)
                .unwrap(),
            1
        );
    }
    {
        let player_state_b = game_state.player_states.get("b").unwrap();
        assert_eq!(player_state_b.player, Player("b".to_string()));
        test_player_state(player_state_b);
        assert_eq!(player_state_b.wonder.name(), "Alexandria - A");
        assert_eq!(
            *player_state_b
                .resources_produced
                .single_resources
                .get(&Glass)
                .unwrap(),
            1
        );
    }
    {
        let player_state_c = game_state.player_states.get("c").unwrap();
        assert_eq!(player_state_c.player, Player("c".to_string()));
        test_player_state(player_state_c);
        assert_eq!(player_state_c.wonder.name(), "Babylon - B");
        assert_eq!(
            *player_state_c
                .resources_produced
                .single_resources
                .get(&Clay)
                .unwrap(),
            1
        );
    }
}

#[test]
fn test_start_game_accepts_accented_wonder_names() {
    let wonder_sides = vec![
        choice("Rhódos", true),
        choice("Éphesos", false),
        choice("Halikarnassós", true),
    ];
    let safe_game_state = api::start_game(players(&["a", "b", "c"]), wonder_sides).unwrap();
    let game_state = safe_game_state.lock().unwrap();
    assert_eq!(
        game_state.player_states.get("a").unwrap().wonder.name(),
        "Rhódos - B"
    );
    assert_eq!(
        game_state.player_states.get("b").unwrap().wonder.name(),
        "Éphesos - A"
    );
    assert_eq!(
        game_state.player_states.get("c").unwrap().wonder.name(),
        "Halikarnassós - B"
    );
}

#[test]
fn test_start_game_with_invalid_players_number() {
    assert_eq!(
        api::start_game(players(&["a", "b"]), vec![]).unwrap_err(),
        ErrorType::InvalidPlayersNumber(2)
    );
    assert_eq!(
        api::start_game(players(&["a", "b", "c", "d", "e", "f", "g", "h"]), vec![]).unwrap_err(),
        ErrorType::InvalidPlayersNumber(8)
    );
}

#[test]
fn test_start_game_with_invalid_length() {
    let error_type =
        api::start_game(players(&["a", "b", "c"]), vec![choice("Gizah", false)]).unwrap_err();
    assert_eq!(
        error_type,
        ErrorType::InvalidPlayersAndWonderSideLength("3 != 1".to_string())
    );
}

#[test]
fn test_start_game_with_invalid_wonder() {
    let wonder_sides = vec![
        choice("Gizah", false),
        choice("Alexandria", false),
        choice("Rhodos", false),
    ];
    let error_type = api::start_game(players(&["a", "b", "c"]), wonder_sides).unwrap_err();
    assert_eq!(error_type, ErrorType::InvalidWonder("Rhodos".to_string()));
}

fn test_game_state(game_state: &GameState) {
    assert_eq!(game_state.cards_discarded, Cards::default());
    assert_eq!(game_state.current_age, Default::default());
    assert_eq!(game_state.current_age_cards, Default::default());
    assert_eq!(game_state.events, Events::default());
}

fn test_player_state(player_state: &PlayerState) {
    assert_eq!(
        player_state.wonder_stages_built,
        WonderStagesBuilt::default()
    );
    assert_eq!(player_state.coins, 3);
    assert_eq!(
        player_state.military_symbols,
        MilitarySymbolCount::default()
    );
    assert_eq!(player_state.battle_tokens, BattleTokens::default());
    assert_eq!(player_state.scientific_symbols_produced, Default::default());
    assert_ne!(player_state.resources_produced, Default::default());
    assert_eq!(player_state.structure_builder, Default::default());
    assert_eq!(player_state.point_actions.len(), 0);
    assert_eq!(player_state.trade_actions.len(), 1);
    assert!(!player_state.can_play_last_card);
    assert!(!player_state.can_copy_guild);
}
