use crate::domain::{Card, ResourceType};
use serde::Serialize;

/// Resources bought from each neighbour. Entries for the same resource are
/// summed; zero counts are ignored.
#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Payment {
    pub west: Vec<(ResourceType, u8)>,
    pub east: Vec<(ResourceType, u8)>,
}

impl Payment {
    pub fn is_empty(&self) -> bool {
        self.west
            .iter()
            .chain(&self.east)
            .all(|(_, count)| *count == 0)
    }
}

/// One way to pay for a card, with the coins going to each party.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PaymentOption {
    pub payment: Payment,
    pub west_coins: u32,
    pub east_coins: u32,
    pub bank_coins: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum Side {
    A,
    B,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WonderSelection {
    Random,
    Explicit(Vec<(String, Side)>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Action {
    Build {
        card: String,
        payment: Payment,
    },
    BuildWonderStage {
        card: String,
        payment: Payment,
    },
    Discard {
        card: String,
    },
    /// Olympía A: once per age.
    BuildFree {
        card: String,
    },
    /// Halikarnassós extra turn only.
    BuildFromDiscard {
        card: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ExtraTurnKind {
    PlayLastCard,
    BuildFromDiscard,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FinalScore {
    pub player: String,
    pub military: i32,
    pub treasury: i32,
    pub wonder: i32,
    pub civilian: i32,
    pub scientific: i32,
    pub commercial: i32,
    pub guild: i32,
    pub total: i32,
    pub coins: u32,
    pub rank: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Phase {
    /// Every player chooses a card from their hand.
    ChoosingCards {
        age: u8,
        turn: u8,
    },
    /// Only `player` acts.
    ExtraTurn {
        player: String,
        kind: ExtraTurnKind,
    },
    GameOver {
        scores: Vec<FinalScore>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SetupError {
    InvalidPlayersNumber(usize),
    DuplicatePlayer(String),
    InvalidWonder(String),
    WondersLengthMismatch { players: usize, wonders: usize },
    DuplicateWonder(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ActionError {
    UnknownPlayer,
    NotYourTurn,
    GameOver,
    CardNotInHand,
    CardNotInDiscard,
    AlreadyBuilt,
    CannotAfford,
    InvalidPayment,
    NoWonderStageLeft,
    FreeBuildUnavailable,
    ActionNotAllowedNow,
}

/// What a validated action does at resolution time.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Kind {
    Build,
    Wonder,
    Discard,
    BuildFree,
    FromDiscard,
}

/// A validated action with its coin movements (computed from pre-turn state).
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Resolved {
    pub(crate) kind: Kind,
    pub(crate) card: Card,
    pub(crate) bank: u32,
    pub(crate) west: u32,
    pub(crate) east: u32,
}

impl Resolved {
    pub(crate) fn free(kind: Kind, card: Card) -> Self {
        Self {
            kind,
            card,
            bank: 0,
            west: 0,
            east: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Pending {
    pub(crate) action: Action,
    pub(crate) resolved: Resolved,
}
