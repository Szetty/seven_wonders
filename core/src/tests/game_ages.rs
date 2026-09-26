use super::game_helpers::{discard, discard_age, discard_turn, plain_game, PLAYERS};
use crate::game::{ActionError, Phase};

#[test]
fn the_last_card_is_discarded_and_age_two_is_dealt() {
    let mut game = plain_game();
    discard_age(&mut game);
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 2, turn: 1 });
    assert_eq!(game.discard_names().len(), 21);
    for player in PLAYERS {
        assert_eq!(game.hand_names(player).len(), 7);
        assert_eq!(game.coins(player), 3 + 6 * 3);
    }
}

#[test]
fn hands_pass_east_in_age_two() {
    let mut game = plain_game();
    discard_age(&mut game);
    let before: Vec<Vec<String>> = PLAYERS.iter().map(|p| game.hand_names(p)).collect();
    discard_turn(&mut game);
    // Seat i passes to seat i+1, so p1 receives p3's hand.
    assert_eq!(game.hand_names("p1"), before[2][1..].to_vec());
    assert_eq!(game.hand_names("p2"), before[0][1..].to_vec());
    assert_eq!(game.hand_names("p3"), before[1][1..].to_vec());
}

#[test]
fn battles_award_age_one_tokens() {
    let mut game = plain_game();
    game.give("p1", "Barracks");
    discard_age(&mut game);
    assert_eq!(game.tokens("p1"), vec![1, 1]);
    assert_eq!(game.tokens("p2"), vec![-1]);
    assert_eq!(game.tokens("p3"), vec![-1]);
}

#[test]
fn battles_award_age_two_and_three_tokens_and_nothing_on_ties() {
    let mut game = plain_game();
    game.give("p1", "Barracks"); // p1: 1 shield
    discard_age(&mut game);
    game.give("p2", "Walls"); // p2: 2 shields
    discard_age(&mut game);
    assert_eq!(game.tokens("p1"), vec![1, 1, 3, -1]);
    assert_eq!(game.tokens("p2"), vec![-1, 3, 3]);
    assert_eq!(game.tokens("p3"), vec![-1, -1, -1]);
    game.give("p3", "Fortifications"); // p3: 3 shields
    discard_age(&mut game);
    assert_eq!(game.tokens("p1"), vec![1, 1, 3, -1, -1, -1]);
    assert_eq!(game.tokens("p2"), vec![-1, 3, 3, 5, -1]);
    assert_eq!(game.tokens("p3"), vec![-1, -1, -1, 5, 5]);
}

#[test]
fn the_game_ends_after_age_three_with_shared_rank_on_exact_ties() {
    let mut game = plain_game();
    for _ in 0..3 {
        discard_age(&mut game);
    }
    let Phase::GameOver { scores } = game.phase() else {
        panic!("expected game over, got {:?}", game.phase());
    };
    assert_eq!(scores.len(), 3);
    for (score, player) in scores.iter().zip(PLAYERS) {
        assert_eq!(score.player, player);
        assert_eq!(score.coins, 3 + 18 * 3);
        assert_eq!(score.treasury, 19);
        assert_eq!(score.total, 19);
        assert_eq!(score.rank, 1);
    }
}

#[test]
fn no_action_is_accepted_after_game_over() {
    let mut game = plain_game();
    for _ in 0..3 {
        discard_age(&mut game);
    }
    assert_eq!(
        game.submit("p1", discard("Altar")),
        Err(ActionError::GameOver)
    );
    assert_eq!(
        game.submit("zed", discard("Altar")),
        Err(ActionError::UnknownPlayer)
    );
}
