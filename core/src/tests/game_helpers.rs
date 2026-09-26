use crate::game::{Game, Side, WonderSelection};

pub fn names(count: usize) -> Vec<String> {
    (1..=count).map(|i| format!("p{i}")).collect()
}

pub fn game_with(wonders: &[(&str, Side)]) -> Game {
    let selection = WonderSelection::Explicit(
        wonders
            .iter()
            .map(|(name, side)| (name.to_string(), *side))
            .collect(),
    );
    Game::new(names(wonders.len()), selection, 7).expect("valid setup")
}

/// Gizah A (Stone), Rhódos A (Ore), Éphesos A (Papyrus): no special abilities.
/// Seats: p1 (west p3, east p2), p2 (west p1, east p3), p3 (west p2, east p1).
pub fn plain_game() -> Game {
    game_with(&[
        ("Gizah", Side::A),
        ("Rhódos", Side::A),
        ("Éphesos", Side::A),
    ])
}
