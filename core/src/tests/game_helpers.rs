use crate::game::{Action, Game, Payment, ResourceType, Side, WonderSelection};

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

pub fn build(card: &str) -> Action {
    Action::Build {
        card: card.to_string(),
        payment: Payment::default(),
    }
}

pub fn build_paying(
    card: &str,
    west: &[(ResourceType, u8)],
    east: &[(ResourceType, u8)],
) -> Action {
    Action::Build {
        card: card.to_string(),
        payment: Payment {
            west: west.to_vec(),
            east: east.to_vec(),
        },
    }
}

pub fn stage(card: &str) -> Action {
    Action::BuildWonderStage {
        card: card.to_string(),
        payment: Payment::default(),
    }
}

pub fn discard(card: &str) -> Action {
    Action::Discard {
        card: card.to_string(),
    }
}

pub const PLAYERS: [&str; 3] = ["p1", "p2", "p3"];

pub fn discard_first(game: &mut Game, player: &str) {
    let card = game.hand_names(player)[0].clone();
    game.submit(player, discard(&card))
        .expect("discarding a card in hand is legal");
}

pub fn discard_turn(game: &mut Game) {
    for player in game.players().to_vec() {
        discard_first(game, &player);
    }
}

pub fn discard_turns(game: &mut Game, turns: usize) {
    for _ in 0..turns {
        discard_turn(game);
    }
}

pub fn discard_age(game: &mut Game) {
    discard_turns(game, 6);
}
