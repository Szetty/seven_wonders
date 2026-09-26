//! The single source of truth for what a player may do right now. `submit`
//! validates with it; the view builds its options with it (Task 16).
use super::payment::{check_payment, payment_options, Market};
use super::types::{Action, ActionError, ExtraTurnKind, Kind, Payment, Phase, Resolved};
use super::Game;
use crate::domain::{Card, ResourceCost, ALL_RESOURCE_TYPES};

/// What it takes to build something, before looking at a payment.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Requirement {
    Free,
    Coins(u32),
    Trade {
        coin_cost: u32,
        resources: Vec<ResourceCost>,
    },
}

impl Game {
    pub(super) fn check_action(
        &self,
        seat: usize,
        action: &Action,
    ) -> Result<Resolved, ActionError> {
        let extra_turn = match &self.phase {
            Phase::GameOver { .. } => return Err(ActionError::GameOver),
            Phase::ChoosingCards { .. } => None,
            Phase::ExtraTurn { kind, .. } => Some(*kind),
        };
        let hand_actions_allowed = extra_turn != Some(ExtraTurnKind::BuildFromDiscard);
        match action {
            Action::Build { card, payment } => {
                if !hand_actions_allowed {
                    return Err(ActionError::ActionNotAllowedNow);
                }
                let card = self.find_in_hand(seat, card)?;
                let requirement = self.structure_requirement(seat, card)?;
                let (bank, west, east) = self.settle(seat, &requirement, payment)?;
                Ok(Resolved {
                    kind: Kind::Build,
                    card,
                    bank,
                    west,
                    east,
                })
            }
            Action::BuildWonderStage { card, payment } => {
                if !hand_actions_allowed {
                    return Err(ActionError::ActionNotAllowedNow);
                }
                let card = self.find_in_hand(seat, card)?;
                let requirement = self.wonder_requirement(seat)?;
                let (bank, west, east) = self.settle(seat, &requirement, payment)?;
                Ok(Resolved {
                    kind: Kind::Wonder,
                    card,
                    bank,
                    west,
                    east,
                })
            }
            Action::Discard { card } => {
                if !hand_actions_allowed {
                    return Err(ActionError::ActionNotAllowedNow);
                }
                let card = self.find_in_hand(seat, card)?;
                Ok(Resolved::free(Kind::Discard, card))
            }
            Action::BuildFree { card } => {
                if extra_turn.is_some() {
                    return Err(ActionError::ActionNotAllowedNow);
                }
                if !self.free_build_available(seat) {
                    return Err(ActionError::FreeBuildUnavailable);
                }
                let card = self.find_in_hand(seat, card)?;
                if self.already_built(seat, card) {
                    return Err(ActionError::AlreadyBuilt);
                }
                Ok(Resolved::free(Kind::BuildFree, card))
            }
            Action::BuildFromDiscard { card } => {
                if extra_turn != Some(ExtraTurnKind::BuildFromDiscard) {
                    return Err(ActionError::ActionNotAllowedNow);
                }
                let card = self
                    .state
                    .cards_discarded
                    .iter()
                    .find(|discarded| discarded.0.name() == card.as_str())
                    .copied()
                    .ok_or(ActionError::CardNotInDiscard)?;
                if self.already_built(seat, card) {
                    return Err(ActionError::AlreadyBuilt);
                }
                Ok(Resolved::free(Kind::FromDiscard, card))
            }
        }
    }

    fn find_in_hand(&self, seat: usize, name: &str) -> Result<Card, ActionError> {
        self.hands[seat]
            .iter()
            .find(|card| card.0.name() == name)
            .copied()
            .ok_or(ActionError::CardNotInHand)
    }

    pub(super) fn already_built(&self, seat: usize, card: Card) -> bool {
        self.player(seat).structure_builder.already_built(card.0)
    }

    /// Olympía A: the ability is active and unused in the current age.
    pub(super) fn free_build_available(&self, seat: usize) -> bool {
        self.player(seat)
            .structure_builder
            .ages_can_build_free_in
            .contains(&self.state.current_age)
    }

    pub(super) fn structure_requirement(
        &self,
        seat: usize,
        card: Card,
    ) -> Result<Requirement, ActionError> {
        let builder = &self.player(seat).structure_builder;
        if builder.already_built(card.0) {
            return Err(ActionError::AlreadyBuilt);
        }
        if builder.can_build_structure_from_dependencies(card.0) {
            return Ok(Requirement::Free);
        }
        let (coin_cost, resources) = *card.0.cost();
        self.requirement(seat, coin_cost, resources)
    }

    pub(super) fn wonder_requirement(&self, seat: usize) -> Result<Requirement, ActionError> {
        let player = self.player(seat);
        let built = usize::from(player.wonder_stages_built);
        if built >= player.wonder.stages_total() {
            return Err(ActionError::NoWonderStageLeft);
        }
        self.requirement(
            seat,
            0,
            player.wonder.wonder_stage_with_idx(built + 1).cost(),
        )
    }

    fn requirement(
        &self,
        seat: usize,
        coin_cost: u32,
        resources: &[ResourceCost],
    ) -> Result<Requirement, ActionError> {
        let player = self.player(seat);
        if resources.is_empty() || player.resources_produced.can_produce(resources) {
            if coin_cost == 0 {
                Ok(Requirement::Free)
            } else if player.coins >= coin_cost {
                Ok(Requirement::Coins(coin_cost))
            } else {
                Err(ActionError::CannotAfford)
            }
        } else {
            Ok(Requirement::Trade {
                coin_cost,
                resources: resources.to_vec(),
            })
        }
    }

    /// The buyer's view of the market: own production, neighbours' tradable
    /// production, unit prices from the buyer's trade actions, pre-turn coins.
    pub(super) fn market(&self, seat: usize) -> Market<'_> {
        let buyer = self.player(seat);
        let west = self.player(self.west_of(seat));
        let east = self.player(self.east_of(seat));
        Market {
            own: &buyer.resources_produced,
            west: &west.tradable_resources,
            east: &east.tradable_resources,
            west_prices: ALL_RESOURCE_TYPES.map(|r| buyer.apply_trading(west.player.name(), &r)),
            east_prices: ALL_RESOURCE_TYPES.map(|r| buyer.apply_trading(east.player.name(), &r)),
            coins: buyer.coins,
        }
    }

    /// Checks `payment` against `requirement`; returns (bank, west, east) coins.
    fn settle(
        &self,
        seat: usize,
        requirement: &Requirement,
        payment: &Payment,
    ) -> Result<(u32, u32, u32), ActionError> {
        match requirement {
            Requirement::Free if payment.is_empty() => Ok((0, 0, 0)),
            Requirement::Coins(coins) if payment.is_empty() => Ok((*coins, 0, 0)),
            Requirement::Free | Requirement::Coins(_) => Err(ActionError::InvalidPayment),
            Requirement::Trade {
                coin_cost,
                resources,
            } => {
                let market = self.market(seat);
                match check_payment(&market, *coin_cost, resources, payment) {
                    Some((west, east)) => Ok((*coin_cost, west, east)),
                    None if payment_options(&market, *coin_cost, resources).is_empty() => {
                        Err(ActionError::CannotAfford)
                    }
                    None => Err(ActionError::InvalidPayment),
                }
            }
        }
    }
}
