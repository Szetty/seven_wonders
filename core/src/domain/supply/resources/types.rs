use derive_more::Display;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

pub type ResourceTypes<'a> = &'a [ResourceType];

#[derive(
    Display, Debug, EnumIter, PartialEq, Clone, Copy, Eq, Hash, PartialOrd, Ord, serde::Serialize,
)]
pub enum ResourceType {
    Wood,
    Stone,
    Ore,
    Clay,
    Glass,
    Loom,
    Papyrus,
}

pub const ALL_RESOURCE_TYPES: [ResourceType; 7] = [
    ResourceType::Wood,
    ResourceType::Stone,
    ResourceType::Ore,
    ResourceType::Clay,
    ResourceType::Glass,
    ResourceType::Loom,
    ResourceType::Papyrus,
];

/// Units needed per resource type, indexed by `ResourceType as usize`.
pub fn resource_counts(costs: &[ResourceCost]) -> [u8; 7] {
    let mut counts = [0u8; 7];
    for ResourceCost(resource_type, count) in costs {
        counts[*resource_type as usize] += count;
    }
    counts
}

#[rustfmt::skip]
pub fn all_resource_types() -> impl Iterator<Item = ResourceType> { ResourceType::iter() }

pub type ResourceCosts<'a> = &'a [ResourceCost];

#[derive(Display, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[display("ResourceCost({_0}, {_1})")]
pub struct ResourceCost(pub ResourceType, pub ResourceCount);

pub type ResourceCount = u8;
