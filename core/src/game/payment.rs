//! Buying resources from neighbours: validating a submitted [`Payment`] and
//! enumerating the cheapest valid payments for the view.
//!
//! A candidate is a pair of per-resource purchase counts `(west, east)` with
//! `west + east <= cost`. It is valid when each neighbour can produce its part
//! from *tradable* production and the buyer's own production covers the rest.
use super::types::{Payment, PaymentOption};
use crate::domain::{
    resource_counts, ResourceCost, ResourceType, ResourcesProduced, ALL_RESOURCE_TYPES,
};

pub(crate) type Counts = [u8; 7];
pub(crate) const MAX_OPTIONS: usize = 6;

pub(crate) struct Market<'a> {
    /// The buyer's full production (including owner-only choices).
    pub own: &'a ResourcesProduced,
    /// The west neighbour's tradable production.
    pub west: &'a ResourcesProduced,
    /// The east neighbour's tradable production.
    pub east: &'a ResourcesProduced,
    pub west_prices: [u32; 7],
    pub east_prices: [u32; 7],
    /// The buyer's coins before this turn.
    pub coins: u32,
}

pub(crate) fn check_payment(
    market: &Market,
    coin_cost: u32,
    cost: &[ResourceCost],
    payment: &Payment,
) -> Option<(u32, u32)> {
    let need = resource_counts(cost);
    let west = side_counts(&payment.west, &need)?;
    let east = side_counts(&payment.east, &need)?;
    if !is_valid(market, &need, &west, &east) {
        return None;
    }
    let west_coins = coins_for(&west, &market.west_prices);
    let east_coins = coins_for(&east, &market.east_prices);
    (market.coins >= coin_cost + west_coins + east_coins).then_some((west_coins, east_coins))
}

pub(crate) fn payment_options(
    market: &Market,
    coin_cost: u32,
    cost: &[ResourceCost],
) -> Vec<PaymentOption> {
    let need = resource_counts(cost);
    let mut candidates = Vec::new();
    split(0, &need, &mut [0; 7], &mut [0; 7], &mut candidates);
    let mut options: Vec<PaymentOption> = candidates
        .into_iter()
        .filter(|(west, east)| is_valid(market, &need, west, east))
        .filter(|(west, east)| is_minimal(market, &need, west, east))
        .map(|(west, east)| PaymentOption {
            payment: Payment {
                west: to_list(&west),
                east: to_list(&east),
            },
            west_coins: coins_for(&west, &market.west_prices),
            east_coins: coins_for(&east, &market.east_prices),
            bank_coins: coin_cost,
        })
        .filter(|option| market.coins >= option.bank_coins + option.west_coins + option.east_coins)
        .collect();
    options.sort_by(|a, b| {
        (a.west_coins + a.east_coins, a.west_coins, &a.payment).cmp(&(
            b.west_coins + b.east_coins,
            b.west_coins,
            &b.payment,
        ))
    });
    options.dedup();
    options.truncate(MAX_OPTIONS);
    options
}

/// Every `(west, east)` with `west[i] + east[i] <= need[i]`.
fn split(
    index: usize,
    need: &Counts,
    west: &mut Counts,
    east: &mut Counts,
    out: &mut Vec<(Counts, Counts)>,
) {
    if index == need.len() {
        out.push((*west, *east));
        return;
    }
    for from_west in 0..=need[index] {
        for from_east in 0..=(need[index] - from_west) {
            west[index] = from_west;
            east[index] = from_east;
            split(index + 1, need, west, east, out);
        }
    }
    west[index] = 0;
    east[index] = 0;
}

fn is_valid(market: &Market, need: &Counts, west: &Counts, east: &Counts) -> bool {
    let mut own = [0u8; 7];
    for index in 0..need.len() {
        match need[index]
            .checked_sub(west[index])
            .and_then(|rest| rest.checked_sub(east[index]))
        {
            Some(rest) => own[index] = rest,
            None => return false,
        }
    }
    market.west.can_produce_counts(west)
        && market.east.can_produce_counts(east)
        && market.own.can_produce_counts(&own)
}

/// No single purchased unit can be dropped (own production is monotone, so
/// this is equivalent to "no dominated valid purchase exists").
fn is_minimal(market: &Market, need: &Counts, west: &Counts, east: &Counts) -> bool {
    for index in 0..need.len() {
        if west[index] > 0 {
            let mut fewer = *west;
            fewer[index] -= 1;
            if is_valid(market, need, &fewer, east) {
                return false;
            }
        }
        if east[index] > 0 {
            let mut fewer = *east;
            fewer[index] -= 1;
            if is_valid(market, need, west, &fewer) {
                return false;
            }
        }
    }
    true
}

/// Sums a submitted side; `None` if it exceeds the cost for any resource.
fn side_counts(side: &[(ResourceType, u8)], need: &Counts) -> Option<Counts> {
    let mut counts = [0u8; 7];
    for (resource_type, count) in side {
        let index = *resource_type as usize;
        counts[index] = counts[index].checked_add(*count)?;
        if counts[index] > need[index] {
            return None;
        }
    }
    Some(counts)
}

fn coins_for(counts: &Counts, prices: &[u32; 7]) -> u32 {
    counts
        .iter()
        .zip(prices)
        .map(|(count, price)| u32::from(*count) * price)
        .sum()
}

fn to_list(counts: &Counts) -> Vec<(ResourceType, u8)> {
    ALL_RESOURCE_TYPES
        .iter()
        .zip(counts)
        .filter(|(_, count)| **count > 0)
        .map(|(resource_type, count)| (*resource_type, *count))
        .collect()
}
