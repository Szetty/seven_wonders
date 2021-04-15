use crate::core::game_init;
use crate::domain::{GameState, Player, PlayersWithWonders, WONDERS_BY_NAME, WONDER_NAMES};
use crate::VERSION;
use protobuf::RepeatedField;
use std::sync::Mutex;

pub mod game_settings;
pub mod ping;
pub mod start_game;

#[derive(Debug, PartialEq)]
pub enum ErrorType {
    InvalidPlayersNumber(usize),
    InvalidPlayersAndWonderSideLength(String),
    InvalidWonder(String),
}

pub fn ping(ping_request: ping::PingRequest) -> ping::PingReply {
    let mut ping_reply = ping::PingReply::new();
    ping_reply.set_message(format!("Hello, {}", ping_request.get_name()));
    ping_reply
}

pub fn game_settings() -> game_settings::Reply {
    let mut reply = game_settings::Reply::new();
    reply.set_version(VERSION.to_string());
    reply.set_supported_wonders(RepeatedField::from_vec(WONDER_NAMES.to_vec()));
    reply
}

pub type SafeGameState = Mutex<GameState>;

pub fn start_game(request: start_game::Request) -> Result<SafeGameState, ErrorType> {
    let players = request.get_players();
    let wonder_sides = request.get_wonder_sides();
    let game_state = if players.len() < 3 || players.len() > 7 {
        return Err(ErrorType::InvalidPlayersNumber(players.len()));
    } else if wonder_sides.len() == 0 {
        game_init::init_with_random_wonders(
            players
                .iter()
                .map(|player_name| Player(player_name.clone()))
                .collect(),
        )
    } else if players.len() == wonder_sides.len() {
        let mut players_with_wonders: PlayersWithWonders = Default::default();
        for (player_name, wonder_side) in players.into_iter().zip(wonder_sides.into_iter()) {
            let wonder_name = wonder_side.get_wonder_name();
            let is_side_b = wonder_side.get_is_side_b();
            let wonder = match WONDERS_BY_NAME.get(wonder_name) {
                Some(wonder) => wonder,
                None => {
                    return Err(ErrorType::InvalidWonder(wonder_name.to_string()));
                }
            };
            let wonder_side = if is_side_b { &wonder.2 } else { &wonder.1 };
            players_with_wonders.push((Player(player_name.clone()), wonder_side))
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
