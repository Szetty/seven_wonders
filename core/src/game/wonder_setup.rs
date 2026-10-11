//! `Phase::Setup` for `WonderSelection::ChooseBoard` / `ChooseSide`: each seat
//! claims a board and side (changeable) and then confirms. Everything here is
//! public. The deck generated in `Game::new` waits in [`WonderSetup`] until the
//! last confirmation calls `finish_setup`; until then `Game::state` is an empty
//! placeholder and must not be read.
use super::setup::WonderRef;
use super::types::{Action, ActionError, Side};
use super::view::{SetupMode, SetupSeat, SetupView};
use super::Game;
use crate::domain::Deck;
use crate::engine::data::{WONDERS, WONDERS_BY_NAME};

pub(super) struct WonderSetup {
    pub(super) mode: SetupMode,
    /// One per seat, in seat order.
    pub(super) seats: Vec<SeatClaim>,
    /// Generated in `Game::new`, consumed by `finish_setup`.
    pub(super) deck: Deck,
}

/// A seat's board (claimed, or drawn for `ChooseSide`), side and lock.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(super) struct SeatClaim {
    pub(super) wonder: Option<&'static str>,
    pub(super) side: Option<Side>,
    pub(super) confirmed: bool,
}

/// A validated setup action.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum SetupMove {
    Claim(&'static str, Side),
    Confirm,
}

impl WonderSetup {
    fn check_claim(&self, seat: usize, wonder: &str) -> Result<&'static str, ActionError> {
        if self.seats[seat].confirmed {
            return Err(ActionError::AlreadyConfirmed);
        }
        let name: &'static str = WONDERS_BY_NAME
            .get(wonder)
            .map(|wonder| wonder.0)
            .ok_or(ActionError::UnknownWonder)?;
        match self.mode {
            SetupMode::ChooseSide if self.seats[seat].wonder != Some(name) => {
                Err(ActionError::NotYourWonder)
            }
            SetupMode::ChooseBoard
                if self
                    .seats
                    .iter()
                    .enumerate()
                    .any(|(other, claim)| other != seat && claim.wonder == Some(name)) =>
            {
                Err(ActionError::WonderTaken)
            }
            _ => Ok(name),
        }
    }

    fn check_confirm(&self, seat: usize) -> Result<(), ActionError> {
        let claim = &self.seats[seat];
        if claim.confirmed {
            Err(ActionError::AlreadyConfirmed)
        } else if claim.side.is_none() {
            Err(ActionError::NoClaim)
        } else {
            Ok(())
        }
    }
}

impl Game {
    /// Validates a setup action without applying it (`Phase::Setup` only).
    pub(super) fn check_setup_action(
        &self,
        seat: usize,
        action: &Action,
    ) -> Result<SetupMove, ActionError> {
        let setup = self
            .setup
            .as_ref()
            .expect("Phase::Setup keeps its WonderSetup");
        match action {
            Action::ClaimWonder { wonder, side } => {
                Ok(SetupMove::Claim(setup.check_claim(seat, wonder)?, *side))
            }
            Action::ConfirmWonder => setup.check_confirm(seat).map(|()| SetupMove::Confirm),
            _ => Err(ActionError::ActionNotAllowedNow),
        }
    }

    /// `submit` during `Phase::Setup`. Records no history and leaves `pending`
    /// alone; the last confirmation builds the table and deals Age I.
    pub(super) fn submit_setup(&mut self, seat: usize, action: &Action) -> Result<(), ActionError> {
        let setup_move = self.check_setup_action(seat, action)?;
        let setup = self
            .setup
            .as_mut()
            .expect("Phase::Setup keeps its WonderSetup");
        let claim = &mut setup.seats[seat];
        match setup_move {
            SetupMove::Claim(wonder, side) => {
                claim.wonder = Some(wonder);
                claim.side = Some(side);
            }
            SetupMove::Confirm => claim.confirmed = true,
        }
        if setup.seats.iter().all(|claim| claim.confirmed) {
            self.finish_wonder_setup();
        }
        Ok(())
    }

    fn finish_wonder_setup(&mut self) {
        let setup = self
            .setup
            .take()
            .expect("Phase::Setup keeps its WonderSetup");
        let assignment: Vec<(WonderRef, Side)> = setup
            .seats
            .iter()
            .map(|claim| {
                let name = claim.wonder.expect("a confirmed seat holds a board");
                let wonder = WONDERS_BY_NAME
                    .get(name)
                    .copied()
                    .expect("claims are base wonders");
                (wonder, claim.side.expect("a confirmed seat holds a side"))
            })
            .collect();
        self.finish_setup(setup.deck, &assignment);
    }

    /// Public setup state; `None` outside `Phase::Setup`.
    pub(super) fn setup_view(&self) -> Option<SetupView> {
        let setup = self.setup.as_ref()?;
        let held = |name: &str| {
            setup
                .seats
                .iter()
                .any(|claim| claim.wonder.is_some_and(|held| held == name))
        };
        Some(SetupView {
            mode: setup.mode,
            seats: self
                .seats
                .iter()
                .zip(&setup.seats)
                .map(|(player, claim)| SetupSeat {
                    player: player.clone(),
                    wonder: claim.wonder.map(str::to_string),
                    side: claim.side,
                    confirmed: claim.confirmed,
                })
                .collect(),
            available: match setup.mode {
                SetupMode::ChooseBoard => WONDERS
                    .iter()
                    .map(|wonder| wonder.0)
                    .filter(|name| !held(name))
                    .map(str::to_string)
                    .collect(),
                SetupMode::ChooseSide => Vec::new(),
            },
        })
    }
}
