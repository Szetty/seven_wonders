//! Deterministic randomness for game setup.
//!
//! Only the raw `next_u64` stream of `ChaCha8Rng` is used; it is value-stable
//! across `rand_chacha` releases. Range sampling and shuffling are implemented
//! here so that upgrading `rand` can never change a replay.
use rand_chacha::rand_core::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;

pub struct GameRng(ChaCha8Rng);

impl GameRng {
    pub fn new(seed: u64) -> Self {
        Self(ChaCha8Rng::seed_from_u64(seed))
    }

    pub fn next_u64(&mut self) -> u64 {
        self.0.next_u64()
    }

    /// Uniform integer in `0..n`, by rejection sampling (no modulo bias).
    pub fn below(&mut self, n: usize) -> usize {
        assert!(n > 0, "GameRng::below called with n = 0");
        let n = n as u64;
        // 2^64 mod n; values above u64::MAX - rejection_zone are rejected so the
        // accepted range (2^64 - rejection_zone values) is a multiple of n.
        let rejection_zone = (u64::MAX % n + 1) % n;
        loop {
            let value = self.0.next_u64();
            if value <= u64::MAX - rejection_zone {
                return (value % n) as usize;
            }
        }
    }

    /// Fisher–Yates shuffle.
    pub fn shuffle<T>(&mut self, items: &mut [T]) {
        for i in (1..items.len()).rev() {
            let j = self.below(i + 1);
            items.swap(i, j);
        }
    }
}
