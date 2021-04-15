use itertools::Itertools;
use maplit::hashset;
use serde::ser::{Serialize, SerializeStruct, Serializer};
use std::collections::{HashMap, HashSet};
use std::iter;

pub mod types;
use std::fmt;
use types::*;
pub use types::{
    diff_resource_costs, resource_cost_map_to_resource_costs, ResourceCost, ResourceCosts,
    ResourceCount, ResourceType, ResourceTypes,
};

#[derive(PartialEq)]
pub struct ResourcesProduced {
    pub single_resources: HashMap<ResourceType, ResourceCount>,
    pub any_resources: Vec<ResourceTypes<'static>>,
}

impl ResourcesProduced {
    fn new() -> Self {
        let mut single_resources: HashMap<ResourceType, ResourceCount> = Default::default();
        for resource_type in all_resource_types() {
            single_resources.insert(resource_type, 0);
        }
        Self {
            single_resources,
            any_resources: Default::default(),
        }
    }
    pub fn add_all_resources(&mut self, resource_types: ResourceTypes) {
        for resource_type in resource_types {
            (*self
                .single_resources
                .get_mut(resource_type)
                .unwrap_or(&mut 0)) += 1;
        }
    }
    pub fn add_any_resources(&mut self, resource_types: ResourceTypes<'static>) {
        self.any_resources.push(resource_types);
    }
    pub fn cover_resource_costs(&self, resource_costs: Vec<ResourceCost>) -> ResourceCostOptions {
        if resource_costs.len() == 0 {
            return Default::default();
        }
        let costs_remaining = match_single_resources(self.single_resources.clone(), resource_costs);
        if costs_remaining.len() == 0 {
            return Default::default();
        }
        apply_any_resources(
            self.any_resources.iter().map(|r| r.to_vec()).collect(),
            costs_remaining,
        )
    }
}

impl Default for ResourcesProduced {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for ResourcesProduced {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ResourcesProduced")
            .field("single_resources", &self.single_resources)
            .field("any_resources", &self.any_resources)
            .finish()
    }
}

impl Serialize for ResourcesProduced {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = serializer.serialize_struct("ResourcesProduced", 3)?;
        s.serialize_field("type", "ResourcesProduced")?;
        s.serialize_field("single_resources", &self.single_resources)?;
        s.serialize_field("any_resources", &self.any_resources)?;
        s.end()
    }
}

pub type ResourceCostOptions = HashSet<ResourceCostOption>;
pub type ResourceCostOption = Vec<ResourceCost>;

fn match_single_resources(
    mut resources_produced: HashMap<ResourceType, ResourceCount>,
    resource_costs: Vec<ResourceCost>,
) -> Vec<ResourceCost> {
    let mut unmatched_costs = vec![];
    for ResourceCost(resource_type, resource_cost_count) in resource_costs {
        let mut default = 0;
        let resource_produced_count = resources_produced
            .get_mut(&resource_type)
            .unwrap_or(&mut default);
        if *resource_produced_count >= resource_cost_count {
            *resource_produced_count -= resource_cost_count;
        } else {
            let new_resource_cost_count = resource_cost_count - *resource_produced_count;
            unmatched_costs.push(ResourceCost(resource_type, new_resource_cost_count));
            *resource_produced_count = 0;
        }
    }
    unmatched_costs
}

fn apply_any_resources(
    any_resources: Vec<Vec<ResourceType>>,
    resource_costs: Vec<ResourceCost>,
) -> ResourceCostOptions {
    if resource_costs.len() == 0 {
        return Default::default();
    }
    if any_resources.len() == 0 {
        return hashset![resource_costs.clone()];
    }
    let combinations = compute_combinations(any_resources, resource_costs);
    let mut resource_cost_options: ResourceCostOptions = Default::default();
    for (any_resources, resource_costs) in combinations {
        match match_any_resource(any_resources, resource_costs) {
            costs_remaining if costs_remaining.len() == 0 => {
                return Default::default();
            }
            costs_remaining => {
                let a = resource_types_to_resource_costs(costs_remaining);
                resource_cost_options.insert(a);
            }
        }
    }
    resource_cost_options
}

fn compute_combinations(
    any_resources: Vec<Vec<ResourceType>>,
    resource_costs: Vec<ResourceCost>,
) -> impl Iterator<Item = Combination> {
    let any_resources_len = any_resources.len();
    let resources_costs_len = resource_costs
        .iter()
        .map(|ResourceCost(_, c)| *c as usize)
        .sum();
    any_resources
        .into_iter()
        .map(|r| r.to_vec())
        .permutations(any_resources_len)
        .filter(|r| r.len() > 0)
        .unique()
        .cartesian_product(
            resource_costs
                .into_iter()
                .flat_map(|ResourceCost(resource_type, resource_count)| {
                    iter::repeat(resource_type).take(resource_count.into())
                })
                .into_iter()
                .permutations(resources_costs_len)
                .filter(|r| r.len() > 0),
        )
        .unique()
}

fn match_any_resource(
    mut any_resources: Vec<Vec<ResourceType>>,
    resource_costs: Vec<ResourceType>,
) -> Vec<ResourceType> {
    let mut unmatched_costs = vec![];
    for resource_cost_type in resource_costs.into_iter() {
        match any_resources
            .iter()
            .position(|any_resource| any_resource.contains(&resource_cost_type))
        {
            Some(idx) => {
                any_resources.remove(idx);
            }
            None => {
                unmatched_costs.push(resource_cost_type);
            }
        }
    }
    unmatched_costs
}

fn resource_types_to_resource_costs(resource_types: Vec<ResourceType>) -> Vec<ResourceCost> {
    let mut resource_cost_map: HashMap<ResourceType, ResourceCount> = Default::default();
    for resource_type in resource_types.into_iter().sorted() {
        if let Some(value) = resource_cost_map.get_mut(&resource_type) {
            *value += 1
        } else {
            resource_cost_map.insert(resource_type, 1);
        };
    }
    resource_cost_map_to_resource_costs(resource_cost_map)
}

#[cfg(test)]
use ResourceType::*;
#[cfg(test)]
mod test_fixtures;

#[test]
fn test_compute_combinations() {
    let f = |any_resources, resource_costs| {
        compute_combinations(any_resources, resource_costs).collect::<Vec<_>>()
    };
    assert_eq!(f(vec![], vec![]), vec![] as Vec<Combination>);
    assert_eq!(
        f(vec![all_resource_types().collect()], vec![]),
        vec![] as Vec<Combination>
    );
    assert_eq!(
        f(vec![], vec![ResourceCost(Wood, 2)]),
        vec![] as Vec<Combination>
    );
    assert_eq!(
        f(
            vec![all_resource_types().collect()],
            vec![ResourceCost(Wood, 2)]
        ),
        vec![(vec![all_resource_types().collect()], vec![Wood, Wood])]
    );
    test_fixtures::assert_combination_of_three(f);
}

#[test]
fn test_apply_any_resources() {
    assert_eq!(apply_any_resources(vec![], vec![]), hashset![]);
    assert_eq!(
        apply_any_resources(vec![all_resource_types().collect()], vec![]),
        hashset![]
    );
    assert_eq!(
        apply_any_resources(
            vec![],
            vec![ResourceCost(Wood, 1), ResourceCost(Papyrus, 2)]
        ),
        hashset![vec![ResourceCost(Wood, 1), ResourceCost(Papyrus, 2)]]
    );
    assert_eq!(
        apply_any_resources(
            vec![all_resource_types().collect()],
            vec![ResourceCost(Wood, 1), ResourceCost(Papyrus, 2)]
        ),
        hashset![
            vec![ResourceCost(Papyrus, 2)],
            vec![ResourceCost(Wood, 1), ResourceCost(Papyrus, 1)]
        ]
    );
}
