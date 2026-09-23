//! Plain-Rust API consumed by the NIF layer in `lib.rs`. No rustler types here.

use crate::domain::{GameState, Player, PlayersWithWonders};
use crate::engine::data::{WONDERS_BY_NAME, WONDER_NAMES};
use crate::engine::game_init;
use std::sync::Mutex;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GameSettings {
    pub version: String,
    pub wonders: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WonderSideChoice {
    pub wonder_name: String,
    pub side_b: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorType {
    InvalidPlayersNumber(usize),
    InvalidPlayersAndWonderSideLength(String),
    InvalidWonder(String),
}

pub type SafeGameState = Mutex<GameState>;

pub fn game_settings() -> GameSettings {
    GameSettings {
        version: VERSION.to_string(),
        wonders: WONDER_NAMES.to_vec(),
    }
}

/// Starts a game for 3..=7 players. An empty `wonder_sides` assigns random
/// wonders and sides; otherwise it must have one entry per player, in seat order.
pub fn start_game(
    players: Vec<String>,
    wonder_sides: Vec<WonderSideChoice>,
) -> Result<SafeGameState, ErrorType> {
    if !(3..=7).contains(&players.len()) {
        return Err(ErrorType::InvalidPlayersNumber(players.len()));
    }
    let game_state = if wonder_sides.is_empty() {
        game_init::init_with_random_wonders(players.into_iter().map(Player).collect())
    } else if players.len() == wonder_sides.len() {
        let mut players_with_wonders: PlayersWithWonders = Vec::with_capacity(players.len());
        for (player_name, choice) in players.into_iter().zip(wonder_sides) {
            let wonder = WONDERS_BY_NAME
                .get(&choice.wonder_name)
                .ok_or_else(|| ErrorType::InvalidWonder(choice.wonder_name.clone()))?;
            let wonder_side = if choice.side_b { &wonder.2 } else { &wonder.1 };
            players_with_wonders.push((Player(player_name), wonder_side));
        }
        game_init::init(players_with_wonders)
    } else {
        return Err(ErrorType::InvalidPlayersAndWonderSideLength(format!(
            "{} != {}",
            players.len(),
            wonder_sides.len()
        )));
    };
    Ok(Mutex::new(game_state))
}
