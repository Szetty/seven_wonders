//! Plain-data description of a card or wonder-stage effect. It travels next to
//! the closure that applies it (`Effect`), so clients can explain an effect
//! without re-implementing it. Never read by the rules.
use super::player::PlayerDirection;
use super::point::{Point, PointCategory};
use super::structure::Category;
use super::supply::{Coin, MilitarySymbolCount, ResourceType, ScientificSymbol};
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum EffectSpec {
    /// `choice`: one of `resources` per turn; `tradable`: neighbours may buy it.
    Produces {
        resources: Vec<ResourceType>,
        choice: bool,
        tradable: bool,
    },
    Points {
        category: PointCategory,
        points: Point,
    },
    Military(MilitarySymbolCount),
    Science {
        symbols: Vec<ScientificSymbol>,
        choice: bool,
    },
    /// Paid once, when built.
    Coins(Coin),
    /// Buy `resources` from the players in `directions` for 1 coin.
    TradeDiscount {
        directions: Vec<PlayerDirection>,
        resources: Vec<ResourceType>,
    },
    /// Paid once, when built.
    CoinsPerCard {
        directions: Vec<PlayerDirection>,
        categories: Vec<Category>,
        coins: Coin,
    },
    /// Scored at game end.
    PointsPerCard {
        category: PointCategory,
        directions: Vec<PlayerDirection>,
        categories: Vec<Category>,
        points: Point,
    },
    CoinsPerWonderStage {
        directions: Vec<PlayerDirection>,
        coins: Coin,
    },
    PointsPerWonderStage {
        category: PointCategory,
        directions: Vec<PlayerDirection>,
        points: Point,
    },
    PointsPerDefeat {
        category: PointCategory,
        directions: Vec<PlayerDirection>,
        points: Point,
    },
    PlayLastCard,
    BuildFreeOncePerAge,
    CopyGuild,
    BuildFromDiscard,
}
