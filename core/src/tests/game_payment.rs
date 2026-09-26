use crate::domain::{
    ResourceCost, ResourceCount, ResourceType, ResourceType::*, ResourceTypes, ResourcesProduced,
};
use crate::game::payment::{check_payment, payment_options, Market};
use crate::game::{Payment, PaymentOption};
use maplit::hashmap;
use std::collections::HashMap;

fn prod(
    single: HashMap<ResourceType, ResourceCount>,
    any: &[ResourceTypes<'static>],
) -> ResourcesProduced {
    ResourcesProduced {
        single_resources: single,
        any_resources: any.to_vec(),
    }
}

fn market<'a>(
    own: &'a ResourcesProduced,
    west: &'a ResourcesProduced,
    east: &'a ResourcesProduced,
    coins: u32,
) -> Market<'a> {
    Market {
        own,
        west,
        east,
        west_prices: [2; 7],
        east_prices: [2; 7],
        coins,
    }
}

fn pay(west: &[(ResourceType, u8)], east: &[(ResourceType, u8)]) -> Payment {
    Payment {
        west: west.to_vec(),
        east: east.to_vec(),
    }
}

fn totals(options: &[PaymentOption]) -> Vec<(u32, u32)> {
    options
        .iter()
        .map(|o| (o.west_coins, o.east_coins))
        .collect()
}

#[test]
fn buys_a_missing_resource_from_the_neighbour_that_sells_it() {
    let (own, west, east) = (
        prod(hashmap! {}, &[]),
        prod(hashmap! {Wood => 1}, &[]),
        prod(hashmap! {}, &[]),
    );
    let m = market(&own, &west, &east, 3);
    let cost = [ResourceCost(Wood, 1)];
    assert_eq!(
        payment_options(&m, 0, &cost),
        vec![PaymentOption {
            payment: pay(&[(Wood, 1)], &[]),
            west_coins: 2,
            east_coins: 0,
            bank_coins: 0
        }]
    );
    assert_eq!(
        check_payment(&m, 0, &cost, &pay(&[(Wood, 1)], &[])),
        Some((2, 0))
    );
    assert_eq!(check_payment(&m, 0, &cost, &pay(&[], &[(Wood, 1)])), None);
    assert_eq!(check_payment(&m, 0, &cost, &pay(&[], &[])), None);
}

#[test]
fn nothing_can_be_bought_when_no_neighbour_produces_it() {
    let (own, west, east) = (
        prod(hashmap! {}, &[]),
        prod(hashmap! {Ore => 3}, &[]),
        prod(hashmap! {Clay => 1}, &[]),
    );
    let m = market(&own, &west, &east, 10);
    assert!(payment_options(&m, 0, &[ResourceCost(Wood, 1)]).is_empty());
}

#[test]
fn own_production_covers_part_of_the_cost() {
    let (own, west, east) = (
        prod(hashmap! {Wood => 1}, &[]),
        prod(hashmap! {Wood => 2}, &[]),
        prod(hashmap! {}, &[]),
    );
    let m = market(&own, &west, &east, 3);
    let cost = [ResourceCost(Wood, 2)];
    assert_eq!(totals(&payment_options(&m, 0, &cost)), vec![(2, 0)]);
    // Buying both units is valid but costs 4 > 3 coins.
    assert_eq!(check_payment(&m, 0, &cost, &pay(&[(Wood, 2)], &[])), None);
}

#[test]
fn options_are_sorted_by_total_then_west_and_capped_at_six() {
    let (own, west, east) = (
        prod(hashmap! {}, &[]),
        prod(hashmap! {Wood => 1, Stone => 1, Ore => 1}, &[]),
        prod(hashmap! {Wood => 1, Stone => 1, Ore => 1}, &[]),
    );
    let mut m = market(&own, &west, &east, 20);
    m.east_prices = [1; 7];
    let options = payment_options(
        &m,
        0,
        &[
            ResourceCost(Wood, 1),
            ResourceCost(Stone, 1),
            ResourceCost(Ore, 1),
        ],
    );
    assert_eq!(options.len(), 6);
    assert_eq!(
        options[0].payment,
        pay(&[], &[(Wood, 1), (Stone, 1), (Ore, 1)])
    );
    let keys: Vec<(u32, u32)> = options
        .iter()
        .map(|o| (o.west_coins + o.east_coins, o.west_coins))
        .collect();
    let mut sorted = keys.clone();
    sorted.sort();
    assert_eq!(keys, sorted);
}

#[test]
fn unaffordable_options_are_dropped_including_the_bank_cost() {
    let (own, west, east) = (
        prod(hashmap! {}, &[]),
        prod(hashmap! {Wood => 1}, &[]),
        prod(hashmap! {}, &[]),
    );
    assert!(
        payment_options(&market(&own, &west, &east, 1), 0, &[ResourceCost(Wood, 1)]).is_empty()
    );
    let options = payment_options(&market(&own, &west, &east, 3), 1, &[ResourceCost(Wood, 1)]);
    assert_eq!(options[0].bank_coins, 1);
    assert!(
        payment_options(&market(&own, &west, &east, 2), 1, &[ResourceCost(Wood, 1)]).is_empty()
    );
}

#[test]
fn a_neighbour_choice_card_supplies_one_unit_per_turn() {
    let (own, west, east) = (
        prod(hashmap! {}, &[]),
        prod(hashmap! {}, &[&[Wood, Clay]]),
        prod(hashmap! {Clay => 1}, &[]),
    );
    let m = market(&own, &west, &east, 10);
    let cost = [ResourceCost(Wood, 1), ResourceCost(Clay, 1)];
    assert_eq!(
        check_payment(&m, 0, &cost, &pay(&[(Wood, 1), (Clay, 1)], &[])),
        None
    );
    assert_eq!(
        check_payment(&m, 0, &cost, &pay(&[(Wood, 1)], &[(Clay, 1)])),
        Some((2, 2))
    );
    assert_eq!(payment_options(&m, 0, &cost).len(), 1);
}

#[test]
fn discounted_prices_apply() {
    let (own, west, east) = (
        prod(hashmap! {}, &[]),
        prod(hashmap! {Wood => 1}, &[]),
        prod(hashmap! {Wood => 1}, &[]),
    );
    let mut m = market(&own, &west, &east, 5);
    m.west_prices[Wood as usize] = 1;
    assert_eq!(
        totals(&payment_options(&m, 0, &[ResourceCost(Wood, 1)])),
        vec![(1, 0), (0, 2)]
    );
}

#[test]
fn duplicate_and_zero_entries_are_normalised() {
    let (own, west, east) = (
        prod(hashmap! {}, &[]),
        prod(hashmap! {Wood => 2}, &[]),
        prod(hashmap! {}, &[]),
    );
    let m = market(&own, &west, &east, 4);
    let payment = pay(&[(Wood, 1), (Wood, 1), (Stone, 0)], &[]);
    assert_eq!(
        check_payment(&m, 0, &[ResourceCost(Wood, 2)], &payment),
        Some((4, 0))
    );
}

#[test]
fn resources_outside_the_cost_are_rejected() {
    let (own, west, east) = (
        prod(hashmap! {}, &[]),
        prod(hashmap! {Wood => 1, Glass => 1}, &[]),
        prod(hashmap! {}, &[]),
    );
    let m = market(&own, &west, &east, 10);
    let cost = [ResourceCost(Wood, 1)];
    assert_eq!(
        check_payment(&m, 0, &cost, &pay(&[(Wood, 1), (Glass, 1)], &[])),
        None
    );
    assert_eq!(check_payment(&m, 0, &cost, &pay(&[(Wood, 2)], &[])), None);
}

#[test]
fn palace_sized_costs_enumerate_quickly() {
    let everything =
        hashmap! {Wood => 1, Stone => 1, Ore => 1, Clay => 1, Glass => 1, Loom => 1, Papyrus => 1};
    let (own, west, east) = (
        prod(hashmap! {}, &[]),
        prod(everything.clone(), &[]),
        prod(everything, &[]),
    );
    let m = market(&own, &west, &east, 30);
    let cost: Vec<ResourceCost> = [Wood, Stone, Ore, Clay, Glass, Loom, Papyrus]
        .iter()
        .map(|r| ResourceCost(*r, 1))
        .collect();
    let started = std::time::Instant::now();
    assert_eq!(payment_options(&m, 0, &cost).len(), 6);
    assert!(started.elapsed() < std::time::Duration::from_millis(500));
}
