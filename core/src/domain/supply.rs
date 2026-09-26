use super::point::Point;

mod scientific;
pub use scientific::*;

mod resources;
pub use resources::*;

pub type Coin = u32;

#[rustfmt::skip]
pub fn calculate_treasury_points(coin: Coin) -> Point { (coin / 3) as Point }

pub type MilitarySymbolCount = u32;

pub type BattleTokens = Vec<BattleToken>;
pub fn calculate_military_points(battle_tokens: &BattleTokens) -> Point {
    battle_tokens.iter().sum()
}
pub type BattleToken = i32;

pub type TradeValue = u32;
