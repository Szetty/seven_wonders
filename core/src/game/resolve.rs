//! Turn resolution: all pending actions are applied at once, then the game
//! moves on (pass hands / extra turns / end of age / game over).
use super::types::{Kind, Phase, Resolved};
use super::Game;
use crate::domain::{Card, EventType, PlayerDecision};
use std::cmp::Ordering;

impl Game {
    pub(super) fn all_required_submitted(&self) -> bool {
        match &self.phase {
            Phase::ChoosingCards { .. } => self.pending.iter().all(Option::is_some),
            Phase::ExtraTurn { player, .. } => self
                .seat_of(player)
                .is_some_and(|seat| self.pending[seat].is_some()),
            Phase::GameOver { .. } => false,
        }
    }

    pub(super) fn resolve(&mut self) {
        let was_choosing = matches!(self.phase, Phase::ChoosingCards { .. });
        let batch = self.take_pending();
        // Seats that must now build from the discard pile; wired up in Task 15.
        let _build_from_discard = self.apply_batch(batch);
        if was_choosing && self.hands.iter().all(|hand| hand.len() == 1) {
            self.discard_last_cards();
        }
        self.advance();
    }

    fn take_pending(&mut self) -> Vec<(usize, Resolved)> {
        self.pending
            .iter_mut()
            .enumerate()
            .filter_map(|(seat, pending)| pending.take().map(|p| (seat, p.resolved)))
            .collect()
    }

    /// Applies a batch "simultaneously": coins were validated against
    /// pre-turn balances; trade coins are credited only after every action
    /// (structures, stages, effects) has been applied. Returns the seats that
    /// triggered Halikarnassós' build-from-discard.
    fn apply_batch(&mut self, batch: Vec<(usize, Resolved)>) -> Vec<usize> {
        let mut decisions = Vec::with_capacity(batch.len());
        let mut credits: Vec<(usize, u32)> = Vec::new();
        for (seat, resolved) in batch {
            let card = self.take_card(seat, &resolved);
            if resolved.west > 0 {
                credits.push((self.west_of(seat), resolved.west));
            }
            if resolved.east > 0 {
                credits.push((self.east_of(seat), resolved.east));
            }
            let name = self.seats[seat].clone();
            self.state.get_mut_player_state(&name).coins -=
                resolved.bank + resolved.west + resolved.east;
            let decision = match resolved.kind {
                Kind::Build | Kind::FromDiscard => {
                    self.built[seat].push(card);
                    PlayerDecision::BuildStructure(card)
                }
                Kind::BuildFree => {
                    self.built[seat].push(card);
                    PlayerDecision::ConstructForFreeOncePerAge(card)
                }
                Kind::Wonder => PlayerDecision::BuildNextWonderStage(card),
                Kind::Discard => PlayerDecision::Discard(card),
            };
            decisions.push((name, decision));
        }
        self.state.apply_player_decisions(decisions);
        for (seat, coins) in credits {
            let name = self.seats[seat].clone();
            self.state.get_mut_player_state(&name).coins += coins;
        }
        let events = std::mem::take(&mut self.state.events);
        events
            .into_iter()
            .filter(|(_, event)| *event == EventType::ConstructFromDiscarded)
            .filter_map(|(player, _)| self.seat_of(&player))
            .collect()
    }

    fn take_card(&mut self, seat: usize, resolved: &Resolved) -> Card {
        let name = resolved.card.0.name();
        let pile = if resolved.kind == Kind::FromDiscard {
            &mut self.state.cards_discarded
        } else {
            &mut self.hands[seat]
        };
        let index = pile
            .iter()
            .position(|card| card.0.name() == name)
            .expect("a validated card is still where it was");
        pile.remove(index)
    }

    /// End of turn 6: every remaining (7th) card goes to the discard pile.
    fn discard_last_cards(&mut self) {
        for hand in &mut self.hands {
            if let Some(card) = hand.pop() {
                self.state.cards_discarded.push(card);
            }
        }
    }

    fn advance(&mut self) {
        if self.hands.iter().all(Vec::is_empty) {
            self.resolve_battles();
            if self.age == 3 {
                self.finish();
            } else {
                self.deal_age(self.age + 1);
            }
        } else {
            self.pass_hands();
            self.turn += 1;
            self.phase = Phase::ChoosingCards {
                age: self.age,
                turn: self.turn,
            };
        }
    }

    /// Each player fights both neighbours: more shields → +1/+3/+5 (Age
    /// I/II/III), fewer → −1, equal → nothing. West battle first.
    fn resolve_battles(&mut self) {
        let victory = match self.age {
            1 => 1,
            2 => 3,
            _ => 5,
        };
        let shields: Vec<u32> = (0..self.seats.len())
            .map(|seat| self.player(seat).military_symbols)
            .collect();
        for seat in 0..self.seats.len() {
            let mut tokens = Vec::new();
            for rival in [self.west_of(seat), self.east_of(seat)] {
                match shields[seat].cmp(&shields[rival]) {
                    Ordering::Greater => tokens.push(victory),
                    Ordering::Less => tokens.push(-1),
                    Ordering::Equal => {}
                }
            }
            let name = self.seats[seat].clone();
            self.state
                .get_mut_player_state(&name)
                .battle_tokens
                .extend(tokens);
        }
    }

    /// Ages I and III pass to the west neighbour, Age II to the east.
    fn pass_hands(&mut self) {
        let mut passed = vec![Vec::new(); self.seats.len()];
        for (seat, hand) in std::mem::take(&mut self.hands).into_iter().enumerate() {
            let to = if self.age == 2 {
                self.east_of(seat)
            } else {
                self.west_of(seat)
            };
            passed[to] = hand;
        }
        self.hands = passed;
    }
}
