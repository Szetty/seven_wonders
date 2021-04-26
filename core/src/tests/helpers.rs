use crate::domain::{Effect, GameState, Neighbours, Player, PlayerState, WonderSide, WonderStage};
use lazy_static::lazy_static;
use maplit::hashmap;

lazy_static! {
    pub static ref TEST_WONDER_SIDE: WonderSide<'static, Effect> = WonderSide(
        "test",
        vec![],
        vec![
            WonderStage(&[], vec![]),
            WonderStage(&[], vec![]),
            WonderStage(&[], vec![]),
        ],
    );
}

pub fn default_game_state() -> GameState {
    let player_names = vec!["a".to_string(), "b".to_string(), "c".to_string()];
    GameState {
        neighbours: Neighbours::new(player_names),
        player_states: hashmap! {
            "a".to_string() => PlayerState {
                player: Player("a".to_string()),
                ..default_player_state()
            },
            "b".to_string() => PlayerState {
                player: Player("b".to_string()),
                ..default_player_state()
            },
            "c".to_string() => PlayerState {
                player: Player("c".to_string()),
                ..default_player_state()
            },
        },
        ..Default::default()
    }
}

pub fn default_player_state() -> PlayerState {
    PlayerState::new(Default::default(), &TEST_WONDER_SIDE)
}
