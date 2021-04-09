use crate::core::deck::generate_deck;
use crate::domain::{Effect, GameState, Player, PlayersWithWonders, Wonder, WONDERS};
use itertools::zip;
use rand::random;
use rand::seq::SliceRandom;
use rand::thread_rng;

pub fn init_with_random_wonders(players: Vec<Player>) -> GameState {
    let players_count = players.len();
    let mut wonders: Vec<&'static Wonder<'static, Effect>> = WONDERS.iter().collect();
    wonders.shuffle(&mut thread_rng());
    wonders.truncate(players_count);
    let mut players_with_wonders: PlayersWithWonders = vec![];
    for (player, wonder) in zip(players.into_iter(), wonders.into_iter()) {
        if random::<bool>() {
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
    GameState::new(deck, players_with_wonders)
}
