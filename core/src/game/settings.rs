//! Static game metadata for clients (asset mapping, wonder lists).
use super::types::Side;
use super::ENGINE_VERSION;
use crate::domain::Category;
use crate::engine::data::{
    AGE_III_STRUCTURES, AGE_II_STRUCTURES, AGE_I_STRUCTURES, GUILD_STRUCTURES, WONDERS,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GameSettings {
    pub engine_version: u32,
    pub wonders: Vec<WonderInfo>,
    pub cards: Vec<CardInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WonderInfo {
    pub name: String,
    pub sides: Vec<Side>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CardInfo {
    pub name: String,
    pub category: Category,
    pub age: u8,
}

pub fn settings() -> GameSettings {
    GameSettings {
        engine_version: ENGINE_VERSION,
        wonders: WONDERS
            .iter()
            .map(|wonder| WonderInfo {
                name: wonder.0.to_string(),
                sides: vec![Side::A, Side::B],
            })
            .collect(),
        cards: AGE_I_STRUCTURES
            .iter()
            .chain(AGE_II_STRUCTURES.iter())
            .chain(AGE_III_STRUCTURES.iter())
            .chain(GUILD_STRUCTURES.iter())
            .map(|structure| CardInfo {
                name: structure.name().to_string(),
                category: structure.category(),
                age: structure.age().number(),
            })
            .collect(),
    }
}
