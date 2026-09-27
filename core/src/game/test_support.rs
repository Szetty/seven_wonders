//! `#[cfg(test)]` hooks to rig and inspect a [`Game`] from `crate::tests`.
use super::types::{Action, ActionError, FinalScore, Side};
use super::Game;
use crate::domain::{Card, PlayerDecision, Point, PointCategory};
use crate::engine::data::STRUCTURES_BY_NAME;

fn card(name: &str) -> Card {
    Card(
        STRUCTURES_BY_NAME
            .get(name)
            .copied()
            .unwrap_or_else(|| panic!("unknown card {name}")),
    )
}

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

    /// Replaces the first `cards.len()` cards of the hand, keeping its size.
    pub fn set_hand(&mut self, player: &str, cards: &[&str]) {
        let seat = self.seat(player);
        assert!(
            cards.len() <= self.hands[seat].len(),
            "hand of {player} is too small"
        );
        for (slot, name) in self.hands[seat].iter_mut().zip(cards) {
            *slot = card(name);
        }
    }

    /// Records `name` as built by `player` (no cost) and runs its effects.
    pub fn give(&mut self, player: &str, name: &str) {
        let seat = self.seat(player);
        let built = card(name);
        self.built[seat].push(built);
        self.state.apply_player_decisions(vec![(
            player.to_string(),
            PlayerDecision::BuildStructure(built),
        )]);
        self.state.events.clear();
    }

    /// Builds the next `count` wonder stages for free and runs their effects.
    pub fn give_stages(&mut self, player: &str, count: usize) {
        for _ in 0..count {
            self.state.apply_player_decisions(vec![(
                player.to_string(),
                PlayerDecision::BuildNextWonderStage(card("Altar")),
            )]);
        }
        self.state.events.clear();
    }

    pub fn set_coins(&mut self, player: &str, coins: u32) {
        self.state.get_mut_player_state(&player.to_string()).coins = coins;
    }

    /// The same checks as `submit`, without storing anything.
    pub fn validate(&self, player: &str, action: &Action) -> Result<(), ActionError> {
        let seat = self.precheck(player)?;
        self.check_action(seat, action).map(|_| ())
    }

    pub fn pending_action(&self, player: &str) -> Option<Action> {
        self.pending[self.seat(player)]
            .as_ref()
            .map(|pending| pending.action.clone())
    }

    pub fn discard_names(&self) -> Vec<String> {
        self.state
            .cards_discarded
            .iter()
            .map(|card| card.0.name().to_string())
            .collect()
    }

    pub fn set_discard(&mut self, cards: &[&str]) {
        self.state.cards_discarded = cards.iter().map(|name| card(name)).collect();
    }

    pub fn built_names(&self, player: &str) -> Vec<String> {
        self.built[self.seat(player)]
            .iter()
            .map(|card| card.0.name().to_string())
            .collect()
    }

    pub fn stages_built(&self, player: &str) -> u8 {
        self.player(self.seat(player)).wonder_stages_built
    }

    pub fn shields(&self, player: &str) -> u32 {
        self.player(self.seat(player)).military_symbols
    }

    pub fn tokens(&self, player: &str) -> Vec<i32> {
        self.player(self.seat(player)).battle_tokens.clone()
    }

    pub fn point(&self, player: &str, category: PointCategory) -> Point {
        self.player(self.seat(player))
            .calculate_points(&self.state)
            .get(&category)
            .copied()
            .unwrap_or(0)
    }

    pub fn score_now(&self) -> Vec<FinalScore> {
        self.compute_scores()
    }

    pub fn chosen_guild(&mut self, player: &str) -> Option<String> {
        let seat = self.seat(player);
        self.best_guild_to_copy(seat)
            .map(|guild| guild.name().to_string())
    }

    pub fn finish_now(&mut self) {
        self.finish();
    }
}
