//! `Game::new`: validation, wonder assignment, deck generation and the first deal.
//!
//! RNG consumption order (replay contract; changing it requires bumping
//! `ENGINE_VERSION`):
//! 1. `WonderSelection::Random` only: shuffle the 7 wonders (in `WONDERS`
//!    order), keep the first `n`, then draw one side per seat in seat order.
//! 2. `generate_deck`: Age I shuffle, Age II shuffle, guild shuffle, Age III shuffle.
use super::types::{Phase, SetupError, Side, WonderSelection};
use super::Game;
use crate::domain::{Age, Effect, GameState, Player, PlayersWithWonders, Wonder};
use crate::engine::data::{WONDERS, WONDERS_BY_NAME};
use crate::engine::deck::generate_deck;
use crate::engine::rng::GameRng;
use std::collections::{BTreeSet, VecDeque};

type WonderRef = &'static Wonder<'static, Effect>;

pub(super) fn new_game(
    players: Vec<String>,
    wonders: WonderSelection,
    seed: u64,
) -> Result<Game, SetupError> {
    let count = players.len();
    if !(3..=7).contains(&count) {
        return Err(SetupError::InvalidPlayersNumber(count));
    }
    let mut seen = BTreeSet::new();
    for player in &players {
        if !seen.insert(player.as_str()) {
            return Err(SetupError::DuplicatePlayer(player.clone()));
        }
    }
    let mut rng = GameRng::new(seed);
    let assignment = assign_wonders(count, wonders, &mut rng)?;
    let deck = generate_deck(count, &mut rng);
    let players_with_wonders: PlayersWithWonders = players
        .iter()
        .zip(&assignment)
        .map(|(player, &(wonder, side))| {
            let wonder_side = match side {
                Side::A => &wonder.1,
                Side::B => &wonder.2,
            };
            (Player(player.clone()), wonder_side)
        })
        .collect();
    let mut state = GameState::new(deck, players_with_wonders);
    state.init();
    let mut game = Game {
        state,
        wonders: assignment
            .iter()
            .map(|(wonder, side)| (wonder.0.to_string(), *side))
            .collect(),
        hands: vec![Vec::new(); count],
        built: vec![Vec::new(); count],
        age: 1,
        turn: 1,
        phase: Phase::ChoosingCards { age: 1, turn: 1 },
        pending: vec![None; count],
        extra_turns: VecDeque::new(),
        seats: players,
    };
    game.deal_age(1);
    Ok(game)
}

fn assign_wonders(
    count: usize,
    selection: WonderSelection,
    rng: &mut GameRng,
) -> Result<Vec<(WonderRef, Side)>, SetupError> {
    match selection {
        WonderSelection::Random => {
            let mut pool: Vec<WonderRef> = WONDERS.iter().collect();
            rng.shuffle(&mut pool);
            pool.truncate(count);
            Ok(pool
                .into_iter()
                .map(|wonder| {
                    let side = if rng.below(2) == 0 { Side::A } else { Side::B };
                    (wonder, side)
                })
                .collect())
        }
        WonderSelection::Explicit(choices) => {
            if choices.len() != count {
                return Err(SetupError::WondersLengthMismatch {
                    players: count,
                    wonders: choices.len(),
                });
            }
            let mut used = BTreeSet::new();
            let mut assignment = Vec::with_capacity(count);
            for (name, side) in choices {
                let wonder: WonderRef = WONDERS_BY_NAME
                    .get(&name)
                    .copied()
                    .ok_or_else(|| SetupError::InvalidWonder(name.clone()))?;
                if !used.insert(name.clone()) {
                    return Err(SetupError::DuplicateWonder(name));
                }
                assignment.push((wonder, side));
            }
            Ok(assignment)
        }
    }
}

impl Game {
    /// Deals 7 cards per seat (in seat order) from the age's shuffled deck.
    pub(super) fn deal_age(&mut self, age: u8) {
        self.age = age;
        self.turn = 1;
        self.state.current_age = Age::from_number(age);
        let cards = match age {
            1 => std::mem::take(&mut self.state.deck.0),
            2 => std::mem::take(&mut self.state.deck.1),
            _ => std::mem::take(&mut self.state.deck.2),
        };
        for (hand, chunk) in self.hands.iter_mut().zip(cards.chunks(7)) {
            *hand = chunk.to_vec();
        }
        self.phase = Phase::ChoosingCards { age, turn: 1 };
    }
}
