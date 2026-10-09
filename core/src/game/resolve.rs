//! Turn resolution: all pending actions are applied at once, then the game
//! moves on (pass hands / extra turns / end of age / game over).
use super::history::{Battle, CoinChange, HistoryEvent, Play, PublicAction, TurnKind};
use super::types::{ExtraTurnKind, Kind, Phase, Resolved};
use super::Game;
use crate::domain::{Card, EventType, PlayerDecision};
use std::cmp::Ordering;
use std::collections::BTreeSet;

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
        let kind = match &self.phase {
            Phase::ChoosingCards { .. } => TurnKind::Regular,
            Phase::ExtraTurn {
                kind: ExtraTurnKind::PlayLastCard,
                ..
            } => TurnKind::PlayLastCard,
            Phase::ExtraTurn {
                kind: ExtraTurnKind::BuildFromDiscard,
                ..
            } => TurnKind::BuildFromDiscard,
            Phase::GameOver { .. } => return,
        };
        let batch = self.take_pending();
        let build_from_discard = self.apply_batch(kind, batch);
        if kind == TurnKind::Regular && self.hands.iter().all(|hand| hand.len() == 1) {
            self.handle_last_cards();
        }
        for seat in build_from_discard {
            self.extra_turns
                .push_back((seat, ExtraTurnKind::BuildFromDiscard));
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
    fn apply_batch(&mut self, kind: TurnKind, batch: Vec<(usize, Resolved)>) -> Vec<usize> {
        let coins_before: Vec<u32> = (0..self.seats.len())
            .map(|seat| self.player(seat).coins)
            .collect();
        let mut decisions = Vec::with_capacity(batch.len());
        let mut credits: Vec<(usize, u32)> = Vec::new();
        let mut plays = Vec::with_capacity(batch.len());
        for (seat, resolved) in batch {
            let card = self.take_card(seat, &resolved);
            let name_of_card = card.0.name().to_string();
            let action = match resolved.kind {
                Kind::Build => PublicAction::Built { card: name_of_card },
                Kind::BuildFree => PublicAction::BuiltFree { card: name_of_card },
                Kind::FromDiscard => PublicAction::BuiltFromDiscard { card: name_of_card },
                // Stages are applied after the loop, so the count is still the old one.
                Kind::Wonder => PublicAction::WonderStage {
                    stage: self.player(seat).wonder_stages_built + 1,
                },
                Kind::Discard => PublicAction::Discarded,
            };
            plays.push(Play {
                player: self.seats[seat].clone(),
                action,
                coins_to_west: resolved.west,
                coins_to_east: resolved.east,
            });
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
        let coins = (0..self.seats.len())
            .filter_map(|seat| {
                let delta = self.player(seat).coins as i32 - coins_before[seat] as i32;
                (delta != 0).then(|| CoinChange {
                    player: self.seats[seat].clone(),
                    delta,
                })
            })
            .collect();
        self.history.push(HistoryEvent::Turn {
            age: self.age,
            turn: self.turn,
            kind,
            plays,
            coins,
        });
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

    /// End of turn 6: Babylon B keeps its last card for an extra turn; all
    /// other last cards are discarded. Queued before any Halikarnassós turn
    /// so these discards are eligible for it.
    fn handle_last_cards(&mut self) {
        for seat in 0..self.seats.len() {
            if self.player(seat).can_play_last_card {
                self.extra_turns
                    .push_back((seat, ExtraTurnKind::PlayLastCard));
            } else if let Some(card) = self.hands[seat].pop() {
                self.state.cards_discarded.push(card);
            }
        }
    }

    /// Distinct names of discarded cards `seat` has not built, sorted.
    pub(super) fn buildable_discard_names(&self, seat: usize) -> Vec<String> {
        let names: BTreeSet<&str> = self
            .state
            .cards_discarded
            .iter()
            .filter(|card| !self.already_built(seat, **card))
            .map(|card| card.0.name())
            .collect();
        names.into_iter().map(str::to_string).collect()
    }

    fn advance(&mut self) {
        while let Some((seat, kind)) = self.extra_turns.pop_front() {
            if kind == ExtraTurnKind::BuildFromDiscard
                && self.buildable_discard_names(seat).is_empty()
            {
                continue;
            }
            self.phase = Phase::ExtraTurn {
                player: self.seats[seat].clone(),
                kind,
            };
            return;
        }
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
        let outcome = |mine: u32, theirs: u32| match mine.cmp(&theirs) {
            Ordering::Greater => Some(victory),
            Ordering::Less => Some(-1),
            Ordering::Equal => None,
        };
        let battles = (0..self.seats.len())
            .map(|seat| {
                let rival = self.east_of(seat);
                Battle {
                    player: self.seats[seat].clone(),
                    rival: self.seats[rival].clone(),
                    player_strength: shields[seat],
                    rival_strength: shields[rival],
                    player_token: outcome(shields[seat], shields[rival]),
                    rival_token: outcome(shields[rival], shields[seat]),
                }
            })
            .collect();
        self.history.push(HistoryEvent::Battles {
            age: self.age,
            battles,
        });
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
