use super::point::Point;

mod scientific;
pub use scientific::*;

mod resources;
pub use resources::*;

pub type Coin = u8;

#[rustfmt::skip]
pub fn calculate_treasury_points(coin: Coin) -> Point { (coin / 3).into() }

pub type MilitarySymbolCount = u8;

pub type BattleTokens = Vec<BattleToken>;
pub fn calculate_military_points(battle_tokens: &BattleTokens) -> Point {
    battle_tokens.iter().map(|t| *t as Point).sum()
}
pub type BattleToken = i8;

pub type TradeValue = u8;
