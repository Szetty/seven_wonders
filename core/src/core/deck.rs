use crate::core::data::{
    AGE_III_STRUCTURES, AGE_II_STRUCTURES, AGE_I_STRUCTURES, GUILD_STRUCTURES,
};
use crate::domain::{Card, Cards, Deck, Effect, Structure};

use rand::seq::SliceRandom;
use rand::thread_rng;

pub fn generate_deck(players_count: usize) -> Deck {
    let mut age1 = generate_age_deck(players_count, AGE_I_STRUCTURES.iter());
    age1.shuffle(&mut thread_rng());
    let mut age2 = generate_age_deck(players_count, AGE_II_STRUCTURES.iter());
    age2.shuffle(&mut thread_rng());
    let mut age3 = generate_age_deck(players_count, AGE_III_STRUCTURES.iter());
    age3.append(&mut generate_guild_cards(players_count));
    age3.shuffle(&mut thread_rng());
    (age1, age2, age3)
}

fn generate_age_deck(
    players_count: usize,
    structures: impl Iterator<Item = &'static Structure<'static, Effect>>,
) -> Cards {
    let mut cards = vec![];
    for structure in structures {
        for _ in structure
            .thresholds()
            .iter()
            .filter(|threshold| **threshold <= players_count as u8)
        {
            cards.push(Card(structure));
        }
    }
    cards
}

fn generate_guild_cards(players_count: usize) -> Cards {
    let mut cards: Cards = GUILD_STRUCTURES.iter().map(|s| Card(s)).collect();
    cards.shuffle(&mut thread_rng());
    cards.truncate(players_count + 2);
    cards
}
