use crate::api;
use crate::api::start_game;
use crate::core::data::WONDER_NAMES;
use crate::domain::{
    BattleTokens, Cards, Events, GameState, MilitarySymbolCount, Player, PlayerState,
    ResourceType::*, WonderStagesBuilt,
};
use protobuf::RepeatedField;

#[test]
fn test_game_settings() {
    let game_settings = api::game_settings();
    assert_ne!(game_settings.get_version(), "");
    assert_eq!(game_settings.get_supported_wonders(), *WONDER_NAMES);
}

#[test]
fn test_start_game_with_random_wonders() {
    let players = vec!["a".to_string(), "b".to_string(), "c".to_string()];
    let mut start_game_request = start_game::Request::default();
    start_game_request.set_players(RepeatedField::from_vec(players));
    let safe_game_state = api::start_game(start_game_request).unwrap();
    let game_state = safe_game_state.lock().unwrap();
    test_game_state(&game_state);

    {
        let player_state_a = game_state.player_states.get(&"a".to_string()).unwrap();
        assert_eq!(player_state_a.player, Player("a".to_string()));
        test_player_state(player_state_a);
    }
    {
        let player_state_b = game_state.player_states.get(&"b".to_string()).unwrap();
        assert_eq!(player_state_b.player, Player("b".to_string()));
        test_player_state(player_state_b);
    }
    {
        let player_state_c = game_state.player_states.get(&"c".to_string()).unwrap();
        assert_eq!(player_state_c.player, Player("c".to_string()));
        test_player_state(player_state_c);
    }
}

#[test]
fn test_start_game_with_specific_wonders() {
    let players = vec!["a".to_string(), "b".to_string(), "c".to_string()];
    let mut wonder_side_a = start_game::Request_WonderSide::default();
    wonder_side_a.set_wonder_name("Gizah".to_string());
    let mut wonder_side_b = start_game::Request_WonderSide::default();
    wonder_side_b.set_wonder_name("Alexandria".to_string());
    let mut wonder_side_c = start_game::Request_WonderSide::default();
    wonder_side_c.set_wonder_name("Babylon".to_string());
    wonder_side_c.set_is_side_b(true);
    let wonder_sides = vec![wonder_side_a, wonder_side_b, wonder_side_c];
    let mut start_game_request = start_game::Request::default();
    start_game_request.set_players(RepeatedField::from_vec(players));
    start_game_request.set_wonder_sides(RepeatedField::from_vec(wonder_sides));
    let safe_game_state = api::start_game(start_game_request).unwrap();
    let game_state = safe_game_state.lock().unwrap();
    test_game_state(&game_state);

    {
        let player_state_a = game_state.player_states.get(&"a".to_string()).unwrap();
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
        let player_state_b = game_state.player_states.get(&"b".to_string()).unwrap();
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
        let player_state_c = game_state.player_states.get(&"c".to_string()).unwrap();
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
fn test_start_game_with_invalid_length() {
    let players = vec!["a".to_string(), "b".to_string(), "c".to_string()];
    let mut wonder_side_a = start_game::Request_WonderSide::default();
    wonder_side_a.set_wonder_name("Gizah".to_string());
    let wonder_sides = vec![wonder_side_a];
    let mut start_game_request = start_game::Request::default();
    start_game_request.set_players(RepeatedField::from_vec(players));
    start_game_request.set_wonder_sides(RepeatedField::from_vec(wonder_sides));
    let error_type = api::start_game(start_game_request).unwrap_err();
    assert_eq!(
        error_type,
        api::ErrorType::InvalidPlayersAndWonderSideLength("3 != 1".to_string())
    );
}

#[test]
fn test_start_game_with_invalid_wonder() {
    let players = vec!["a".to_string(), "b".to_string(), "c".to_string()];
    let mut wonder_side_a = start_game::Request_WonderSide::default();
    wonder_side_a.set_wonder_name("Gizah".to_string());
    let mut wonder_side_b = start_game::Request_WonderSide::default();
    wonder_side_b.set_wonder_name("Alexandria".to_string());
    let mut wonder_side_c = start_game::Request_WonderSide::default();
    wonder_side_c.set_wonder_name("Rhodos".to_string());
    let wonder_sides = vec![wonder_side_a, wonder_side_b, wonder_side_c];
    let mut start_game_request = start_game::Request::default();
    start_game_request.set_players(RepeatedField::from_vec(players));
    start_game_request.set_wonder_sides(RepeatedField::from_vec(wonder_sides));
    let error_type = api::start_game(start_game_request).unwrap_err();
    assert_eq!(
        error_type,
        api::ErrorType::InvalidWonder("Rhodos".to_string())
    );
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
    assert_eq!(player_state.can_play_last_card, false);
    assert_eq!(player_state.can_copy_guild, false);
}
