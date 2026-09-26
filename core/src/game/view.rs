//! Per-player view: public table state plus the viewer's private hand and
//! options. Options come from the same legality code `submit` uses.
use super::legality::Requirement;
use super::payment::payment_options;
use super::types::{Action, ActionError, ExtraTurnKind, FinalScore, PaymentOption, Phase, Side};
use super::Game;
use crate::domain::{Card, Category};
use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum PhaseKind {
    ChoosingCards,
    ExtraTurn,
    GameOver,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum PassDirection {
    West,
    East,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PhaseView {
    pub kind: PhaseKind,
    pub age: u8,
    pub turn: u8,
    pub direction: PassDirection,
    pub extra_turn_player: Option<String>,
    pub extra_turn_kind: Option<ExtraTurnKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BuiltCard {
    pub name: String,
    pub category: Category,
    pub age: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PublicPlayer {
    pub name: String,
    pub wonder: String,
    pub side: Side,
    pub stages_built: u8,
    pub stages_total: u8,
    pub built: Vec<BuiltCard>,
    pub coins: u32,
    pub shields: u32,
    pub military_tokens: Vec<i32>,
    pub free_build_available: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum BuildOption {
    Unavailable {
        reason: ActionError,
    },
    /// Chain build or no cost at all: submit with an empty payment.
    Free,
    /// Coin cost only (paid to the bank): submit with an empty payment.
    Coins(u32),
    /// Resources must be bought; submit one of these payments.
    Trade(Vec<PaymentOption>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HandCard {
    pub name: String,
    pub category: Category,
    pub age: u8,
    pub build: BuildOption,
    pub wonder_stage: BuildOption,
    pub free_build: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PlayerView {
    pub me: String,
    pub phase: PhaseView,
    pub players: Vec<PublicPlayer>,
    pub west: String,
    pub east: String,
    pub hand: Vec<HandCard>,
    pub discard_pile: Option<Vec<String>>,
    pub discard_count: usize,
    pub submitted: Vec<(String, bool)>,
    pub my_pending: Option<Action>,
    pub scores: Option<Vec<FinalScore>>,
}

impl Game {
    pub fn view(&self, player: &str) -> Result<PlayerView, ActionError> {
        let seat = self.seat_of(player).ok_or(ActionError::UnknownPlayer)?;
        Ok(self.build_view(seat))
    }

    fn build_view(&self, seat: usize) -> PlayerView {
        let me = &self.seats[seat];
        let acting_with_hand = match &self.phase {
            Phase::ChoosingCards { .. } => true,
            Phase::ExtraTurn {
                player,
                kind: ExtraTurnKind::PlayLastCard,
            } => player == me,
            _ => false,
        };
        let hand = if acting_with_hand {
            let stage = self.option_for(seat, self.wonder_requirement(seat));
            self.hands[seat]
                .iter()
                .map(|card| self.hand_card(seat, *card, &stage))
                .collect()
        } else {
            Vec::new()
        };
        let discard_pile = match &self.phase {
            Phase::ExtraTurn {
                player,
                kind: ExtraTurnKind::BuildFromDiscard,
            } if player == me => Some(self.buildable_discard_names(seat)),
            _ => None,
        };
        let submitted = match &self.phase {
            Phase::ChoosingCards { .. } => self
                .seats
                .iter()
                .zip(&self.pending)
                .map(|(name, pending)| (name.clone(), pending.is_some()))
                .collect(),
            Phase::ExtraTurn { player, .. } => vec![(player.clone(), false)],
            Phase::GameOver { .. } => Vec::new(),
        };
        let scores = match &self.phase {
            Phase::GameOver { scores } => Some(scores.clone()),
            _ => None,
        };
        PlayerView {
            me: me.clone(),
            phase: self.phase_view(),
            players: (0..self.seats.len())
                .map(|s| self.public_player(s))
                .collect(),
            west: self.seats[self.west_of(seat)].clone(),
            east: self.seats[self.east_of(seat)].clone(),
            hand,
            discard_pile,
            discard_count: self.state.cards_discarded.len(),
            submitted,
            my_pending: self.pending[seat].as_ref().map(|p| p.action.clone()),
            scores,
        }
    }

    fn hand_card(&self, seat: usize, card: Card, stage: &BuildOption) -> HandCard {
        HandCard {
            name: card.0.name().to_string(),
            category: card.0.category(),
            age: card.0.age().number(),
            build: self.option_for(seat, self.structure_requirement(seat, card)),
            wonder_stage: stage.clone(),
            free_build: matches!(self.phase, Phase::ChoosingCards { .. })
                && self.free_build_available(seat)
                && !self.already_built(seat, card),
        }
    }

    fn option_for(
        &self,
        seat: usize,
        requirement: Result<Requirement, ActionError>,
    ) -> BuildOption {
        match requirement {
            Err(reason) => BuildOption::Unavailable { reason },
            Ok(Requirement::Free) => BuildOption::Free,
            Ok(Requirement::Coins(coins)) => BuildOption::Coins(coins),
            Ok(Requirement::Trade {
                coin_cost,
                resources,
            }) => {
                let options = payment_options(&self.market(seat), coin_cost, &resources);
                if options.is_empty() {
                    BuildOption::Unavailable {
                        reason: ActionError::CannotAfford,
                    }
                } else {
                    BuildOption::Trade(options)
                }
            }
        }
    }

    fn phase_view(&self) -> PhaseView {
        let direction = if self.age == 2 {
            PassDirection::East
        } else {
            PassDirection::West
        };
        let (kind, extra_turn_player, extra_turn_kind) = match &self.phase {
            Phase::ChoosingCards { .. } => (PhaseKind::ChoosingCards, None, None),
            Phase::ExtraTurn { player, kind } => {
                (PhaseKind::ExtraTurn, Some(player.clone()), Some(*kind))
            }
            Phase::GameOver { .. } => (PhaseKind::GameOver, None, None),
        };
        PhaseView {
            kind,
            age: self.age,
            turn: self.turn,
            direction,
            extra_turn_player,
            extra_turn_kind,
        }
    }

    fn public_player(&self, seat: usize) -> PublicPlayer {
        let player = self.player(seat);
        let (wonder, side) = self.wonders[seat].clone();
        PublicPlayer {
            name: self.seats[seat].clone(),
            wonder,
            side,
            stages_built: player.wonder_stages_built,
            stages_total: u8::try_from(player.wonder.stages_total()).expect("at most 4 stages"),
            built: self.built[seat]
                .iter()
                .map(|card| BuiltCard {
                    name: card.0.name().to_string(),
                    category: card.0.category(),
                    age: card.0.age().number(),
                })
                .collect(),
            coins: player.coins,
            shields: player.military_symbols,
            military_tokens: player.battle_tokens.clone(),
            free_build_available: self.free_build_available(seat),
        }
    }
}
