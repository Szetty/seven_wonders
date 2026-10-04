//! Static game metadata for clients: wonders with both boards, every card with
//! its cost, chains and effect descriptions.
use super::types::Side;
use super::ENGINE_VERSION;
use crate::domain::{
    resource_counts, Category, Coin, Effect, EffectSpec, ResourceCost, ResourceType, Structure,
    WonderSide, ALL_RESOURCE_TYPES,
};
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
    pub boards: Vec<WonderBoardInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WonderBoardInfo {
    pub side: Side,
    /// The starting resource.
    pub effects: Vec<EffectSpec>,
    pub stages: Vec<StageInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StageInfo {
    pub cost: CostInfo,
    pub effects: Vec<EffectSpec>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CardInfo {
    pub name: String,
    pub category: Category,
    pub age: u8,
    pub cost: CostInfo,
    /// Built cards that make this one free (in the card's order).
    pub chains_from: Vec<String>,
    /// Cards this one makes free.
    pub chains_to: Vec<String>,
    pub effects: Vec<EffectSpec>,
}

/// Coins plus resources, summed per resource in `ALL_RESOURCE_TYPES` order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CostInfo {
    pub coins: u32,
    pub resources: Vec<(ResourceType, u8)>,
}

pub fn settings() -> GameSettings {
    GameSettings {
        engine_version: ENGINE_VERSION,
        wonders: WONDERS
            .iter()
            .map(|wonder| WonderInfo {
                name: wonder.0.to_string(),
                sides: vec![Side::A, Side::B],
                boards: vec![board(Side::A, &wonder.1), board(Side::B, &wonder.2)],
            })
            .collect(),
        cards: AGE_I_STRUCTURES
            .iter()
            .chain(AGE_II_STRUCTURES.iter())
            .chain(AGE_III_STRUCTURES.iter())
            .chain(GUILD_STRUCTURES.iter())
            .map(card)
            .collect(),
    }
}

fn card(structure: &'static Structure<'static, Effect>) -> CardInfo {
    let (coins, resources) = *structure.cost();
    CardInfo {
        name: structure.name().to_string(),
        category: structure.category(),
        age: structure.age().number(),
        cost: cost(coins, resources),
        chains_from: structure
            .dependencies()
            .iter()
            .map(|s| s.to_string())
            .collect(),
        chains_to: structure
            .dependents()
            .iter()
            .map(|s| s.to_string())
            .collect(),
        effects: specs(structure.effects()),
    }
}

fn board(side: Side, wonder_side: &'static WonderSide<'static, Effect>) -> WonderBoardInfo {
    WonderBoardInfo {
        side,
        effects: specs(wonder_side.initial_effects()),
        stages: (1..=wonder_side.stages_total())
            .map(|n| {
                let stage = wonder_side.wonder_stage_with_idx(n);
                StageInfo {
                    cost: cost(0, stage.cost()),
                    effects: specs(stage.effects()),
                }
            })
            .collect(),
    }
}

fn specs(effects: &[Effect]) -> Vec<EffectSpec> {
    effects.iter().map(|effect| effect.spec.clone()).collect()
}

fn cost(coins: Coin, resources: &[ResourceCost]) -> CostInfo {
    CostInfo {
        coins,
        resources: ALL_RESOURCE_TYPES
            .iter()
            .zip(resource_counts(resources))
            .filter(|(_, count)| *count > 0)
            .map(|(resource, count)| (*resource, count))
            .collect(),
    }
}
