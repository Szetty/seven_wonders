use crate::domain::{Category, Deck};
use crate::engine::deck::generate_deck;
use crate::engine::rng::GameRng;

#[test]
fn test_generate_deck() {
    for players in 3..=7 {
        assert_deck_size(generate_deck(players, &mut GameRng::new(1)), 7 * players);
    }
}

#[test]
fn same_seed_gives_same_deck() {
    let first = card_names(&generate_deck(5, &mut GameRng::new(9)));
    let second = card_names(&generate_deck(5, &mut GameRng::new(9)));
    assert_eq!(first, second);
}

#[test]
fn different_seed_gives_different_deck() {
    let first = card_names(&generate_deck(5, &mut GameRng::new(9)));
    let second = card_names(&generate_deck(5, &mut GameRng::new(10)));
    assert_ne!(first, second);
}

#[test]
fn age_three_holds_players_plus_two_guilds() {
    for players in 3..=7 {
        let (_, _, age3) = generate_deck(players, &mut GameRng::new(players as u64));
        let guilds = age3
            .iter()
            .filter(|card| card.0.category() == Category::Guild)
            .count();
        assert_eq!(guilds, players + 2);
    }
}

fn card_names(deck: &Deck) -> Vec<&'static str> {
    let (age1, age2, age3) = deck;
    age1.iter()
        .chain(age2.iter())
        .chain(age3.iter())
        .map(|card| card.0.name())
        .collect()
}

fn assert_deck_size(deck: Deck, expected_cards_per_age: usize) {
    let (age1, age2, age3) = deck;
    assert_eq!(age1.len(), expected_cards_per_age);
    assert_eq!(age2.len(), expected_cards_per_age);
    assert_eq!(age3.len(), expected_cards_per_age);
}
