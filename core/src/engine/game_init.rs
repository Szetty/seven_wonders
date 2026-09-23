use crate::domain::{Effect, GameState, Player, PlayersWithWonders, Wonder};
use crate::engine::data::WONDERS;
use crate::engine::deck::generate_deck;
use rand::seq::SliceRandom;
use std::iter::zip;

pub fn init_with_random_wonders(players: Vec<Player>) -> GameState {
    let players_count = players.len();
    let mut wonders: Vec<&'static Wonder<'static, Effect>> = WONDERS.iter().collect();
    wonders.shuffle(&mut rand::rng());
    wonders.truncate(players_count);
    let mut players_with_wonders: PlayersWithWonders = vec![];
    for (player, wonder) in zip(players, wonders) {
        if rand::random::<bool>() {
            players_with_wonders.push((player, &wonder.1));
        } else {
            players_with_wonders.push((player, &wonder.2));
        }
    }
    init(players_with_wonders)
}

pub fn init(players_with_wonders: PlayersWithWonders) -> GameState {
    let players_count = players_with_wonders.len();
    let deck = generate_deck(players_count);
    let mut game_state = GameState::new(deck, players_with_wonders);
    game_state.init();
    game_state
}
