use crate::domain::{Effect, GameState, Player, PlayersWithWonders, Wonder};
use crate::engine::data::WONDERS;
use crate::engine::deck::generate_deck;
use crate::engine::rng::GameRng;

pub fn init_with_random_wonders(players: Vec<Player>, rng: &mut GameRng) -> GameState {
    let players_count = players.len();
    let mut wonders: Vec<&'static Wonder<'static, Effect>> = WONDERS.iter().collect();
    rng.shuffle(&mut wonders);
    wonders.truncate(players_count);
    let mut players_with_wonders: PlayersWithWonders = vec![];
    for (player, wonder) in players.into_iter().zip(wonders) {
        if rng.below(2) == 0 {
            players_with_wonders.push((player, &wonder.1));
        } else {
            players_with_wonders.push((player, &wonder.2));
        }
    }
    init(players_with_wonders, rng)
}

pub fn init(players_with_wonders: PlayersWithWonders, rng: &mut GameRng) -> GameState {
    let players_count = players_with_wonders.len();
    let deck = generate_deck(players_count, rng);
    let mut game_state = GameState::new(deck, players_with_wonders);
    game_state.init();
    game_state
}
