//! Final scoring and ranking.
use super::types::{FinalScore, Phase};
use super::Game;
use crate::domain::{Category, Effect, PointCategory, Structure};
use std::collections::BTreeMap;

type StructureRef = &'static Structure<'static, Effect>;

impl Game {
    pub(super) fn finish(&mut self) {
        self.apply_copy_guild();
        let scores = self.compute_scores();
        self.phase = Phase::GameOver { scores };
    }

    /// Olympía B: each eligible player copies the neighbouring guild that
    /// maximises their total. The copy's effects count; it is not "built".
    fn apply_copy_guild(&mut self) {
        for seat in 0..self.seats.len() {
            if let Some(guild) = self.best_guild_to_copy(seat) {
                let name = self.seats[seat].clone();
                for effect in guild.effects() {
                    (*effect)(&mut self.state, name.clone());
                }
            }
        }
    }

    pub(super) fn best_guild_to_copy(&mut self, seat: usize) -> Option<StructureRef> {
        if !self.player(seat).can_copy_guild {
            return None;
        }
        let mut candidates: BTreeMap<&'static str, StructureRef> = BTreeMap::new();
        for neighbour in [self.west_of(seat), self.east_of(seat)] {
            for card in &self.built[neighbour] {
                if card.0.category() == Category::Guild && !self.already_built(seat, *card) {
                    candidates.insert(card.0.name(), card.0);
                }
            }
        }
        let mut best: Option<(i32, StructureRef)> = None;
        for guild in candidates.into_values() {
            let total = self.total_with(seat, guild);
            match best {
                Some((best_total, _)) if total <= best_total => {}
                _ => best = Some((total, guild)),
            }
        }
        best.map(|(_, guild)| guild)
    }

    /// Total score of `seat` if it also had `guild`'s effects. Guild effects
    /// only push point actions or choice science symbols, so truncating those
    /// two lists restores the state exactly.
    fn total_with(&mut self, seat: usize, guild: StructureRef) -> i32 {
        let name = self.seats[seat].clone();
        let (point_actions, any_symbols) = {
            let player = self.player(seat);
            (
                player.point_actions.len(),
                player.scientific_symbols_produced.any_symbols.len(),
            )
        };
        for effect in guild.effects() {
            (*effect)(&mut self.state, name.clone());
        }
        let total: i32 = self
            .player(seat)
            .calculate_points(&self.state)
            .values()
            .sum();
        let player = self.state.get_mut_player_state(&name);
        player.point_actions.truncate(point_actions);
        player
            .scientific_symbols_produced
            .any_symbols
            .truncate(any_symbols);
        total
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
