use super::game_helpers::plain_game;
use crate::game::scoring::rank_scores;
use crate::game::FinalScore;

fn score(player: &str, total: i32, coins: u32) -> FinalScore {
    FinalScore {
        player: player.to_string(),
        military: 0,
        treasury: 0,
        wonder: 0,
        civilian: 0,
        scientific: 0,
        commercial: 0,
        guild: 0,
        total,
        coins,
        rank: 0,
    }
}

#[test]
fn ranks_by_total_then_coins_and_shares_exact_ties() {
    let mut scores = vec![
        score("p1", 40, 5),
        score("p2", 45, 1),
        score("p3", 40, 9),
        score("p4", 40, 5),
    ];
    rank_scores(&mut scores);
    let ranked: Vec<(&str, u8)> = scores.iter().map(|s| (s.player.as_str(), s.rank)).collect();
    assert_eq!(ranked, vec![("p2", 1), ("p3", 2), ("p1", 3), ("p4", 3)]);
}

#[test]
fn final_score_breaks_down_every_category() {
    let mut game = plain_game();
    game.give("p1", "Altar");
    game.give_stages("p1", 1);
    for card in [
        "Apothecary",
        "Workshop",
        "Scriptorium",
        "Lighthouse",
        "Workers Guild",
    ] {
        game.give("p1", card);
    }
    game.give("p2", "Lumber Yard");
    game.give("p2", "Stone Pit");
    game.give("p3", "Clay Pool");
    game.set_coins("p1", 10);
    let scores = game.score_now();
    let p1 = scores.iter().find(|s| s.player == "p1").unwrap();
    assert_eq!(
        *p1,
        FinalScore {
            player: "p1".to_string(),
            military: 0,
            treasury: 3,
            wonder: 3,
            civilian: 2,
            scientific: 10,
            commercial: 1,
            guild: 3,
            total: 22,
            coins: 10,
            rank: 1,
        }
    );
}
