//! Public record of what happened: one `Turn` per resolved turn (regular or
//! extra) and one `Battles` per age. Derived during resolution, so replaying
//! the action log rebuilds it exactly. Holds no hidden information: discarded
//! cards and the cards under wonder stages are never named.
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum HistoryEvent {
    Turn {
        age: u8,
        turn: u8,
        kind: TurnKind,
        /// One per acting player, in seat order.
        plays: Vec<Play>,
        /// Every seat whose coins changed this turn, in seat order.
        coins: Vec<CoinChange>,
    },
    Battles {
        age: u8,
        /// One per seat against its east neighbour.
        battles: Vec<Battle>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum TurnKind {
    Regular,
    PlayLastCard,
    BuildFromDiscard,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Play {
    pub player: String,
    pub action: PublicAction,
    pub coins_to_west: u32,
    pub coins_to_east: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum PublicAction {
    Built {
        card: String,
    },
    /// Olympía A.
    BuiltFree {
        card: String,
    },
    /// Halikarnassós.
    BuiltFromDiscard {
        card: String,
    },
    WonderStage {
        stage: u8,
    },
    Discarded,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CoinChange {
    pub player: String,
    pub delta: i32,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Battle {
    pub player: String,
    pub rival: String,
    pub player_strength: u32,
    pub rival_strength: u32,
    pub player_token: Option<i32>,
    pub rival_token: Option<i32>,
}
