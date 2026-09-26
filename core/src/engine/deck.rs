use crate::domain::{Card, Cards, Deck, Effect, Structure};
use crate::engine::data::{
    AGE_III_STRUCTURES, AGE_II_STRUCTURES, AGE_I_STRUCTURES, GUILD_STRUCTURES,
};
use crate::engine::rng::GameRng;

/// Builds the three age decks for `players_count` players.
///
/// RNG consumption order (part of the replay contract): Age I shuffle,
/// Age II shuffle, guild shuffle, Age III shuffle.
pub fn generate_deck(players_count: usize, rng: &mut GameRng) -> Deck {
    let mut age1 = generate_age_deck(players_count, AGE_I_STRUCTURES.iter());
    rng.shuffle(&mut age1);
    let mut age2 = generate_age_deck(players_count, AGE_II_STRUCTURES.iter());
    rng.shuffle(&mut age2);
    let mut age3 = generate_age_deck(players_count, AGE_III_STRUCTURES.iter());
    age3.append(&mut generate_guild_cards(players_count, rng));
    rng.shuffle(&mut age3);
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

fn generate_guild_cards(players_count: usize, rng: &mut GameRng) -> Cards {
    let mut cards: Cards = GUILD_STRUCTURES.iter().map(Card).collect();
    rng.shuffle(&mut cards);
    cards.truncate(players_count + 2);
    cards
}
