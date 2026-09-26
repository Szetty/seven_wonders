//! Final scoring and ranking.
use super::types::{FinalScore, Phase};
use super::Game;
use crate::domain::PointCategory;

impl Game {
    pub(super) fn finish(&mut self) {
        let scores = self.compute_scores();
        self.phase = Phase::GameOver { scores };
    }

    pub(super) fn compute_scores(&self) -> Vec<FinalScore> {
        let mut scores: Vec<FinalScore> = (0..self.seats.len())
            .map(|seat| {
                let player_state = self.player(seat);
                let points = player_state.calculate_points(&self.state);
                let get = |category: PointCategory| points.get(&category).copied().unwrap_or(0);
                let military = get(PointCategory::MilitaryP);
                let treasury = get(PointCategory::TreasuryP);
                let wonder = get(PointCategory::WonderP);
                let civilian = get(PointCategory::CivilianP);
                let scientific = get(PointCategory::ScientificP);
                let commercial = get(PointCategory::CommercialP);
                let guild = get(PointCategory::GuildP);
                FinalScore {
                    player: self.seats[seat].clone(),
                    military,
                    treasury,
                    wonder,
                    civilian,
                    scientific,
                    commercial,
                    guild,
                    total: military
                        + treasury
                        + wonder
                        + civilian
                        + scientific
                        + commercial
                        + guild,
                    coins: player_state.coins,
                    rank: 0,
                }
            })
            .collect();
        rank_scores(&mut scores);
        scores
    }
}

/// Rank = 1 + number of players with a strictly better `(total, coins)`;
/// equal on both shares the rank. Sorted by rank, ties in seat order.
pub(crate) fn rank_scores(scores: &mut [FinalScore]) {
    let keys: Vec<(i32, u32)> = scores.iter().map(|s| (s.total, s.coins)).collect();
    for (score, key) in scores.iter_mut().zip(&keys) {
        let better = keys.iter().filter(|other| *other > key).count();
        score.rank = u8::try_from(better + 1).expect("at most 7 players");
    }
    scores.sort_by_key(|score| score.rank);
}
