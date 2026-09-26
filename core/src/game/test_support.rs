//! `#[cfg(test)]` hooks to rig and inspect a [`Game`] from `crate::tests`.
use super::types::Side;
use super::Game;

impl Game {
    fn seat(&self, player: &str) -> usize {
        self.seat_of(player)
            .unwrap_or_else(|| panic!("unknown player {player}"))
    }

    pub fn hand_names(&self, player: &str) -> Vec<String> {
        self.hands[self.seat(player)]
            .iter()
            .map(|card| card.0.name().to_string())
            .collect()
    }

    pub fn wonder_of(&self, player: &str) -> (String, Side) {
        self.wonders[self.seat(player)].clone()
    }

    pub fn coins(&self, player: &str) -> u32 {
        self.player(self.seat(player)).coins
    }
}
