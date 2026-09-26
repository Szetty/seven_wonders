use crate::engine::rng::GameRng;

#[test]
fn same_seed_gives_same_sequence() {
    let mut a = GameRng::new(7);
    let mut b = GameRng::new(7);
    for _ in 0..100 {
        assert_eq!(a.next_u64(), b.next_u64());
    }
}

#[test]
fn different_seeds_give_different_sequences() {
    let mut a = GameRng::new(1);
    let mut b = GameRng::new(2);
    let first: Vec<u64> = (0..4).map(|_| a.next_u64()).collect();
    let second: Vec<u64> = (0..4).map(|_| b.next_u64()).collect();
    assert_ne!(first, second);
}

#[test]
fn below_stays_in_range_and_reaches_every_value() {
    let mut rng = GameRng::new(1);
    let mut seen = [false; 7];
    for _ in 0..1_000 {
        let value = rng.below(7);
        assert!(value < 7);
        seen[value] = true;
    }
    assert!(seen.iter().all(|hit| *hit));
}

#[test]
fn below_one_is_always_zero() {
    let mut rng = GameRng::new(99);
    for _ in 0..50 {
        assert_eq!(rng.below(1), 0);
    }
}

#[test]
fn shuffle_is_a_deterministic_permutation() {
    let original: Vec<u32> = (0..49).collect();
    let mut first = original.clone();
    GameRng::new(3).shuffle(&mut first);
    let mut second = original.clone();
    GameRng::new(3).shuffle(&mut second);
    assert_eq!(first, second);
    assert_ne!(first, original);
    let mut sorted = first.clone();
    sorted.sort_unstable();
    assert_eq!(sorted, original);
}
