use derive_more::Display;
use itertools::Itertools;
use std::collections::HashMap;
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
#[rustfmt::skip]
pub fn all_resource_types() -> impl Iterator<Item = ResourceType> { ResourceType::iter() }

pub type ResourceCosts<'a> = &'a [ResourceCost];

#[derive(Display, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[display(fmt = "ResourceCost({}, {})", _0, _1)]
pub struct ResourceCost(pub ResourceType, pub ResourceCount);

pub fn diff_resource_costs(
    resource_costs1: Vec<ResourceCost>,
    resource_costs2: Vec<ResourceCost>,
) -> Vec<ResourceCost> {
    let mut resource_costs_map = resource_costs_to_map(resource_costs1);
    for (resource_type, count) in resource_costs_to_map(resource_costs2) {
        if let Some(current_count) = resource_costs_map.get_mut(&resource_type) {
            if *current_count > count {
                *current_count -= count;
            } else {
                resource_costs_map.remove(&resource_type);
            }
        }
    }
    resource_cost_map_to_resource_costs(resource_costs_map)
}

pub fn resource_costs_to_map(
    resource_costs: Vec<ResourceCost>,
) -> HashMap<ResourceType, ResourceCount> {
    let mut resource_cost_map: HashMap<ResourceType, ResourceCount> = Default::default();
    for ResourceCost(resource_type, resource_count) in resource_costs {
        if let Some(value) = resource_cost_map.get_mut(&resource_type) {
            *value += resource_count
        } else {
            resource_cost_map.insert(resource_type, resource_count);
        };
    }
    resource_cost_map
}

pub fn resource_cost_map_to_resource_costs(
    resource_cost_map: HashMap<ResourceType, ResourceCount>,
) -> Vec<ResourceCost> {
    resource_cost_map
        .into_iter()
        .map(|(resource_type, resource_count)| ResourceCost(resource_type, resource_count))
        .sorted()
        .collect()
}

pub type ResourceCount = u8;

pub type Combination = (Vec<Vec<ResourceType>>, Vec<ResourceType>);

#[cfg(test)]
use ResourceType::*;

#[test]
fn test_diff_resource_costs() {
    assert_eq!(diff_resource_costs(vec![], vec![]), vec![]);
    assert_eq!(
        diff_resource_costs(vec![ResourceCost(Wood, 1)], vec![ResourceCost(Wood, 1)]),
        vec![]
    );
    assert_eq!(
        diff_resource_costs(vec![ResourceCost(Wood, 1)], vec![ResourceCost(Loom, 1)]),
        vec![ResourceCost(Wood, 1)]
    );
    assert_eq!(
        diff_resource_costs(
            vec![ResourceCost(Wood, 1), ResourceCost(Ore, 1)],
            vec![ResourceCost(Wood, 1)]
        ),
        vec![ResourceCost(Ore, 1)]
    );
    assert_eq!(
        diff_resource_costs(
            vec![ResourceCost(Wood, 1), ResourceCost(Ore, 1)],
            vec![ResourceCost(Ore, 1)]
        ),
        vec![ResourceCost(Wood, 1)]
    );
    assert_eq!(
        diff_resource_costs(
            vec![ResourceCost(Wood, 1), ResourceCost(Ore, 1)],
            vec![ResourceCost(Wood, 1), ResourceCost(Ore, 1)]
        ),
        vec![]
    );
    assert_eq!(
        diff_resource_costs(
            vec![ResourceCost(Wood, 1), ResourceCost(Ore, 1)],
            vec![ResourceCost(Loom, 1), ResourceCost(Glass, 1)]
        ),
        vec![ResourceCost(Wood, 1), ResourceCost(Ore, 1)]
    );
    assert_eq!(
        diff_resource_costs(vec![ResourceCost(Wood, 3)], vec![ResourceCost(Wood, 2)]),
        vec![ResourceCost(Wood, 1)]
    );
    assert_eq!(
        diff_resource_costs(vec![ResourceCost(Wood, 3)], vec![ResourceCost(Wood, 4)]),
        vec![]
    );
}
