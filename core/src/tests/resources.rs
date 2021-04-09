use crate::domain::{
    ResourceCost, ResourceCostOptions, ResourceCosts, ResourceCount, ResourceType, ResourceType::*,
    ResourceTypes, ResourcesProduced,
};
use maplit::{hashmap, hashset};
use std::collections::HashMap;

#[test]
fn test_one_single_resource_produced() {
    assert_eq!(csr(hashmap! {}, &[]), hashset![]);
    assert_eq!(csr(hashmap! {Wood => 1}, &[]), hashset![]);
    assert_eq!(
        csr(hashmap! {}, &[ResourceCost(Wood, 1)]),
        hashset![vec![ResourceCost(Wood, 1)]],
    );
    assert_eq!(
        csr(hashmap! {Loom => 1}, &[ResourceCost(Wood, 1)]),
        hashset![vec![ResourceCost(Wood, 1)]]
    );
    assert_eq!(
        csr(hashmap! {Wood => 1}, &[ResourceCost(Wood, 1)]),
        hashset![]
    );
    assert_eq!(
        csr(hashmap! {Wood => 1}, &[ResourceCost(Wood, 2)]),
        hashset![vec![ResourceCost(Wood, 1)]]
    );
    assert_eq!(
        csr(hashmap! {Wood => 2}, &[ResourceCost(Wood, 1)]),
        hashset![]
    );
    assert_eq!(
        csr(hashmap! {Wood => 2}, &[ResourceCost(Wood, 2)]),
        hashset![]
    );
}

#[test]
fn test_multiple_single_resources_produced() {
    assert_eq!(
        csr(
            hashmap! {Wood => 1},
            &[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]
        ),
        hashset![vec![ResourceCost(Loom, 1)]]
    );
    assert_eq!(
        csr(
            hashmap! {Wood => 1, Loom => 1},
            &[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]
        ),
        hashset![]
    );
    assert_eq!(
        csr(
            hashmap! {Wood => 1, Loom => 2},
            &[ResourceCost(Wood, 1), ResourceCost(Loom, 3)]
        ),
        hashset![vec![ResourceCost(Loom, 1)]]
    );
    assert_eq!(
        csr(
            hashmap! {Wood => 2, Loom => 2},
            &[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]
        ),
        hashset![]
    );
    assert_eq!(
        csr(
            hashmap! {Wood => 1, Loom => 1},
            &[ResourceCost(Loom, 1), ResourceCost(Wood, 1)]
        ),
        hashset![]
    );
}

#[test]
fn test_one_any_resource_produced() {
    assert_eq!(car(&[&[Wood, Loom]], &[]), hashset![]);
    assert_eq!(car(&[&[Wood, Loom]], &[ResourceCost(Wood, 1)]), hashset![]);
    assert_eq!(
        car(&[&[Wood, Loom]], &[ResourceCost(Wood, 2)]),
        hashset![vec![ResourceCost(Wood, 1)]]
    );
    assert_eq!(car(&[&[Wood, Loom]], &[ResourceCost(Loom, 1)]), hashset![]);
    assert_eq!(
        car(&[&[Wood, Loom]], &[ResourceCost(Clay, 1)]),
        hashset![vec![ResourceCost(Clay, 1)]]
    );
}

#[test]
fn test_one_any_and_one_single_resource_produced() {
    assert_eq!(
        casr(
            hashmap! {Clay => 1},
            &[&[Wood, Loom]],
            &[ResourceCost(Wood, 2)]
        ),
        hashset![vec![ResourceCost(Wood, 1)]]
    );
    assert_eq!(
        casr(
            hashmap! {Clay => 1},
            &[&[Wood, Loom]],
            &[ResourceCost(Clay, 1)]
        ),
        hashset![]
    );
    assert_eq!(
        casr(
            hashmap! {Clay => 1},
            &[&[Wood, Loom]],
            &[ResourceCost(Wood, 1), ResourceCost(Clay, 1)]
        ),
        hashset![]
    );
    assert_eq!(
        casr(
            hashmap! {Clay => 1},
            &[&[Wood, Loom]],
            &[ResourceCost(Loom, 1), ResourceCost(Clay, 1)]
        ),
        hashset![]
    );
    assert_eq!(
        casr(
            hashmap! {Clay => 1},
            &[&[Wood, Loom]],
            &[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]
        ),
        hashset![vec![ResourceCost(Loom, 1)], vec![ResourceCost(Wood, 1)]]
    );
    assert_eq!(
        casr(
            hashmap! {Clay => 1},
            &[&[Wood, Loom]],
            &[ResourceCost(Stone, 1)]
        ),
        hashset![vec![ResourceCost(Stone, 1)]]
    );
    assert_eq!(
        casr(
            hashmap! {Wood => 1},
            &[&[Wood, Loom]],
            &[ResourceCost(Wood, 2)]
        ),
        hashset![]
    );
    assert_eq!(
        casr(
            hashmap! {Wood => 1},
            &[&[Wood, Loom]],
            &[ResourceCost(Clay, 1)]
        ),
        hashset![vec![ResourceCost(Clay, 1)]]
    );
    assert_eq!(
        casr(
            hashmap! {Wood => 1},
            &[&[Wood, Loom]],
            &[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]
        ),
        hashset![]
    );
    assert_eq!(
        casr(
            hashmap! {Wood => 1},
            &[&[Wood, Loom]],
            &[ResourceCost(Wood, 1), ResourceCost(Stone, 1)]
        ),
        hashset![vec![ResourceCost(Stone, 1)]]
    );
    assert_eq!(
        casr(
            hashmap! {Wood => 1},
            &[&[Wood, Loom]],
            &[ResourceCost(Wood, 2), ResourceCost(Loom, 1)]
        ),
        hashset![vec![ResourceCost(Loom, 1)], vec![ResourceCost(Wood, 1)]]
    );
    assert_eq!(
        casr(
            hashmap! {Wood => 1},
            &[&[Wood, Loom]],
            &[ResourceCost(Loom, 1), ResourceCost(Wood, 1)]
        ),
        hashset![]
    );
}

#[test]
fn test_one_any_with_3_resources_produced() {
    assert_eq!(
        car(&[&[Wood, Loom, Clay]], &[ResourceCost(Wood, 1)]),
        hashset![]
    );
    assert_eq!(
        car(&[&[Wood, Loom, Clay]], &[ResourceCost(Loom, 1)]),
        hashset![]
    );
    assert_eq!(
        car(&[&[Wood, Loom, Clay]], &[ResourceCost(Clay, 1)]),
        hashset![]
    );
    assert_eq!(
        car(&[&[Wood, Loom, Clay]], &[ResourceCost(Ore, 1)]),
        hashset![vec![ResourceCost(Ore, 1)]]
    );
}

#[test]
fn test_multiple_any_resources_produced() {
    assert_eq!(
        car(&[&[Wood, Loom], &[Clay, Ore]], &[ResourceCost(Wood, 1)]),
        hashset![]
    );
    assert_eq!(
        car(&[&[Wood, Loom], &[Clay, Ore]], &[ResourceCost(Wood, 2)]),
        hashset![vec![ResourceCost(Wood, 1)]]
    );
    assert_eq!(
        car(
            &[&[Wood, Loom], &[Clay, Ore]],
            &[ResourceCost(Wood, 1), ResourceCost(Clay, 1)]
        ),
        hashset![]
    );
    assert_eq!(
        car(
            &[&[Wood, Loom], &[Clay, Ore]],
            &[ResourceCost(Ore, 1), ResourceCost(Clay, 1)]
        ),
        hashset![vec![ResourceCost(Clay, 1)], vec![ResourceCost(Ore, 1)]]
    );
    assert_eq!(
        car(&[&[Wood, Loom], &[Clay, Ore]], &[ResourceCost(Stone, 1)]),
        hashset![vec![ResourceCost(Stone, 1)]]
    );
    assert_eq!(
        car(&[&[Wood, Loom], &[Wood, Ore]], &[ResourceCost(Wood, 1)]),
        hashset![]
    );
    assert_eq!(
        car(&[&[Wood, Loom], &[Wood, Ore]], &[ResourceCost(Wood, 2)]),
        hashset![]
    );
    assert_eq!(
        car(
            &[&[Wood, Loom], &[Wood, Ore]],
            &[ResourceCost(Wood, 1), ResourceCost(Ore, 1)]
        ),
        hashset![]
    );
    assert_eq!(
        car(
            &[&[Wood, Loom], &[Wood, Ore]],
            &[ResourceCost(Loom, 1), ResourceCost(Ore, 1)]
        ),
        hashset![]
    );
    assert_eq!(
        car(
            &[&[Wood, Loom], &[Wood, Ore]],
            &[ResourceCost(Wood, 2), ResourceCost(Loom, 1)]
        ),
        hashset![vec![ResourceCost(Loom, 1)], vec![ResourceCost(Wood, 1)]]
    );
    assert_eq!(
        car(
            &[&[Wood, Loom], &[Wood, Ore]],
            &[ResourceCost(Wood, 2), ResourceCost(Ore, 1)]
        ),
        hashset![vec![ResourceCost(Ore, 1)], vec![ResourceCost(Wood, 1)]]
    );
    assert_eq!(
        car(
            &[&[Wood, Loom], &[Wood, Ore]],
            &[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]
        ),
        hashset![]
    );
    assert_eq!(
        car(&[&[Wood, Loom], &[Wood, Ore]], &[ResourceCost(Clay, 1)]),
        hashset![vec![ResourceCost(Clay, 1)]]
    );
    assert_eq!(
        car(&[&[Wood, Loom], &[Wood, Ore]], &[ResourceCost(Wood, 3)]),
        hashset![vec![ResourceCost(Wood, 1)]]
    );
    assert_eq!(
        car(
            &[&[Wood, Loom], &[Wood, Ore]],
            &[ResourceCost(Loom, 1), ResourceCost(Wood, 1)]
        ),
        hashset![]
    );
    assert_eq!(
        car(&[&[Wood, Loom], &[Wood, Loom]], &[ResourceCost(Wood, 1)]),
        hashset![]
    );
    assert_eq!(
        car(&[&[Wood, Loom], &[Wood, Loom]], &[ResourceCost(Loom, 1)]),
        hashset![]
    );
    assert_eq!(
        car(&[&[Wood, Loom], &[Wood, Loom]], &[ResourceCost(Wood, 2)]),
        hashset![]
    );
    assert_eq!(
        car(&[&[Wood, Loom], &[Wood, Loom]], &[ResourceCost(Loom, 2)]),
        hashset![]
    );
    assert_eq!(
        car(
            &[&[Wood, Loom], &[Wood, Loom]],
            &[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]
        ),
        hashset![]
    );
    assert_eq!(
        car(&[&[Wood, Loom], &[Wood, Loom]], &[ResourceCost(Ore, 1)]),
        hashset![vec![ResourceCost(Ore, 1)]]
    );
    assert_eq!(
        car(&[&[Wood, Loom], &[Wood, Loom]], &[ResourceCost(Wood, 3)]),
        hashset![vec![ResourceCost(Wood, 1)]]
    );
}

#[cfg(test)]
fn csr(
    single_resources: HashMap<ResourceType, ResourceCount>,
    resource_costs: ResourceCosts<'static>,
) -> ResourceCostOptions {
    ResourcesProduced {
        single_resources,
        ..Default::default()
    }
    .cover_resource_costs(resource_costs.to_vec())
}

#[cfg(test)]
fn car(
    any_resources: &'static [ResourceTypes<'static>],
    resource_costs: ResourceCosts<'static>,
) -> ResourceCostOptions {
    ResourcesProduced {
        any_resources: any_resources.to_vec(),
        ..Default::default()
    }
    .cover_resource_costs(resource_costs.to_vec())
}

#[cfg(test)]
fn casr(
    single_resources: HashMap<ResourceType, ResourceCount>,
    any_resources: &'static [ResourceTypes<'static>],
    resource_costs: ResourceCosts<'static>,
) -> ResourceCostOptions {
    ResourcesProduced {
        any_resources: any_resources.to_vec(),
        single_resources,
        ..Default::default()
    }
    .cover_resource_costs(resource_costs.to_vec())
}
