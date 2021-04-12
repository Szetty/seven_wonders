use crate::domain::{GameState, Neighbours, Player, PlayerState};
use maplit::hashmap;

pub fn default_game_state() -> GameState {
    let player_names = vec!["a".to_string(), "b".to_string(), "c".to_string()];
    GameState {
        neighbours: Neighbours::new(player_names),
        player_states: hashmap! {
            "a".to_string() => PlayerState {
                player: Player("a".to_string()),
                ..Default::default()
            },
            "b".to_string() => PlayerState {
                player: Player("b".to_string()),
                ..Default::default()
            },
            "c".to_string() => PlayerState {
                player: Player("c".to_string()),
                ..Default::default()
            },
        },
        ..Default::default()
    }
}
