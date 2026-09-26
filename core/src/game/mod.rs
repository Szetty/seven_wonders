//! Rustler-free game API. A [`Game`] owns the whole table: seats, hands,
//! phase and pending actions. Every public operation is deterministic.
pub(crate) mod payment;
mod setup;
#[cfg(test)]
mod test_support;
mod types;

pub use crate::domain::{Category, ResourceType};
pub use types::{
    Action, ActionError, ExtraTurnKind, FinalScore, Payment, PaymentOption, Phase, SetupError,
    Side, WonderSelection,
};

use crate::domain::{Card, GameState, PlayerState};
use std::collections::VecDeque;
use types::Pending;

/// Bump on any rule change that could alter the replay of a stored game.
pub const ENGINE_VERSION: u32 = 1;

pub struct Game {
    state: GameState,
    /// Player ids in seat order.
    seats: Vec<String>,
    /// Wonder name and side per seat.
    wonders: Vec<(String, Side)>,
    /// Cards in hand per seat.
    hands: Vec<Vec<Card>>,
    /// Structures built per seat, in build order (for the view and Olympía B).
    built: Vec<Vec<Card>>,
    age: u8,
    turn: u8,
    phase: Phase,
    /// The current choice per seat; resubmitting replaces it.
    pending: Vec<Option<Pending>>,
    /// Extra turns still to be played before the game moves on.
    extra_turns: VecDeque<(usize, ExtraTurnKind)>,
}

impl Game {
    pub fn new(
        players: Vec<String>,
        wonders: WonderSelection,
        seed: u64,
    ) -> Result<Game, SetupError> {
        setup::new_game(players, wonders, seed)
    }

    pub fn phase(&self) -> &Phase {
        &self.phase
    }

    /// Player ids in seat order.
    pub fn players(&self) -> &[String] {
        &self.seats
    }

    fn seat_of(&self, player: &str) -> Option<usize> {
        self.seats.iter().position(|seat| seat == player)
    }

    fn west_of(&self, seat: usize) -> usize {
        (seat + self.seats.len() - 1) % self.seats.len()
    }

    fn east_of(&self, seat: usize) -> usize {
        (seat + 1) % self.seats.len()
    }

    fn player(&self, seat: usize) -> &PlayerState {
        self.state.get_player_state(&self.seats[seat])
    }
}
