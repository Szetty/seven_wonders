use crate::core::deck::generate_deck;
use crate::domain::Deck;

#[test]
fn test_generate_deck() {
    let valid_players_counts = [3, 4, 5, 6, 7];
    for c in valid_players_counts.iter() {
        assert_deck_size(generate_deck(*c), 7 * c);
    }
}

fn assert_deck_size(deck: Deck, expected_cards_per_age: usize) {
    let (age1, age2, age3) = deck;
    assert_eq!(age1.len(), expected_cards_per_age);
    assert_eq!(age2.len(), expected_cards_per_age);
    assert_eq!(age3.len(), expected_cards_per_age);
}
