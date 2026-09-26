use serde::ser::{Serialize, SerializeStruct, Serializer};
use std::collections::HashMap;

pub mod types;
use std::fmt;
use types::*;
pub use types::{
    resource_counts, ResourceCost, ResourceCosts, ResourceCount, ResourceType, ResourceTypes,
    ALL_RESOURCE_TYPES,
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

    /// True when this production alone pays `need`: fixed resources first,
    /// then each choice card supplies at most one unit.
    pub fn can_produce(&self, need: &[ResourceCost]) -> bool {
        self.can_produce_counts(&resource_counts(need))
    }

    pub fn can_produce_counts(&self, need: &[u8; 7]) -> bool {
        let mut missing: Vec<ResourceType> = Vec::new();
        for (resource_type, needed) in ALL_RESOURCE_TYPES.iter().zip(need.iter()) {
            let have = self
                .single_resources
                .get(resource_type)
                .copied()
                .unwrap_or(0);
            missing.extend(std::iter::repeat_n(
                *resource_type,
                usize::from(needed.saturating_sub(have)),
            ));
        }
        if missing.len() > self.any_resources.len() {
            return false;
        }
        let mut owner: Vec<Option<usize>> = vec![None; self.any_resources.len()];
        (0..missing.len()).all(|unit| {
            let mut visited = vec![false; self.any_resources.len()];
            augment(
                unit,
                &missing,
                &self.any_resources,
                &mut owner,
                &mut visited,
            )
        })
    }
}

/// Kuhn's augmenting path: give `unit` a choice card, re-routing earlier units
/// to other cards when needed. `owner[card]` is the unit currently using it.
fn augment(
    unit: usize,
    units: &[ResourceType],
    choices: &[ResourceTypes<'static>],
    owner: &mut [Option<usize>],
    visited: &mut [bool],
) -> bool {
    for (card, options) in choices.iter().enumerate() {
        if visited[card] || !options.contains(&units[unit]) {
            continue;
        }
        visited[card] = true;
        let available = match owner[card] {
            None => true,
            Some(other) => augment(other, units, choices, owner, visited),
        };
        if available {
            owner[card] = Some(unit);
            return true;
        }
    }
    false
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
