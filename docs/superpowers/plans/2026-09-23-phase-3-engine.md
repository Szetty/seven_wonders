# Phase 3 — Engine Gameplay API Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn `core` (crate `seven_wonders_core`) into a complete, deterministic 7 Wonders base-game engine (3–7 players, every wonder ability) with a per-player view API. Expose it to Elixir through native-term NIFs (`Helios.Core.Native.game_settings/0`, `new_game/3`, `submit/3`, `view/2`, `debug_game/1`) and wrap them in `Helios.Core`.

**Architecture:** The existing `domain` model stays: effects as closures, `GameState`, `PlayerState`. This phase fixes its rules bugs and data errors. A new rustler-free module `src/game/` owns the table: seats, hands, phases, pending actions, legality, payment enumeration, turn resolution, extra turns, battles, scoring and per-player views. A thin `src/nif/` layer holds rustler DTOs that mirror the `game` types, plus `From` conversions between them. All randomness comes from one seeded `ChaCha8Rng`, consumed in a fixed order during setup.

**Tech Stack:** Rust 2021 (stable via `mise`), rustler (the ≥ 0.36 version Phase 0 pinned; latest is 0.38.0), `rand_chacha = "0.10.0"`, serde / serde_json, lazy_static, maplit, itertools, strum, derive_more. Helios side: Elixir 1.19 / OTP 28, ExUnit, Jason.

**Spec:** `docs/superpowers/specs/2026-09-23-phase-3-engine-design.md`. Read it alongside `2026-09-23-migration-overview-design.md`, `2026-09-23-phase-0-foundation-design.md` (its end state is this plan's starting state) and `2026-09-23-phase-4-game-design.md` (Phase 4 consumes this NIF API, so the names must match).

---

## Preconditions and path mapping

Phase 0 must be merged first. Before starting, check that:

```bash
cd core && cargo test            # all legacy tests green
ls src/engine                    # data.rs deck.rs game_init.rs mod.rs trading.rs
grep -n '^name' Cargo.toml       # name = "seven_wonders_core"
ls ../helios/lib/helios/core.ex ../helios/lib/helios/core/native.ex
```

**Path mapping.** The spec and today's tree say `core/src/core/…`. Phase 0 renames that module to `engine`, so this plan uses the post-Phase-0 paths throughout:

| Pre-Phase-0 (today) | Post-Phase-0 (used in this plan) |
|---|---|
| `core/src/core/data.rs` | `core/src/engine/data.rs` |
| `core/src/core/deck.rs` | `core/src/engine/deck.rs` |
| `core/src/core/game_init.rs` | `core/src/engine/game_init.rs` |
| `core/src/core/trading.rs` | `core/src/engine/trading.rs` |
| `core/src/core/mod.rs` | `core/src/engine/mod.rs` |
| `crate::core::…` | `crate::engine::…` |
| crate `core` | crate `seven_wonders_core` |

Unchanged paths: `core/src/domain/**`, `core/src/tests/**` and `core/src/api/mod.rs`. The last one is Phase 0's plain-Rust `game_settings`/`start_game`; Task 18 deletes it. `core/src/lib.rs` holds Phase 0's rustler ≥ 0.36 NIF layer (`GameResource(Mutex<GameState>)`, `game_settings`/`start_game`/`debug_game`); Task 18 replaces it.

## Spec interpretations and deviations (decided here, flagged for the reviewer)

1. **Payment search replaces `cover_resource_costs` + `try_trading`.** The spec says to build payment options from the existing functions, but they have two problems:
   - They are factorial. `compute_combinations` takes permutations of the choice-card list and permutations of the cost units; Palace (7 units) with 6+ choice cards means more than 10⁸ combinations, and the simulation test would never finish.
   - They are incomplete. They only try "neighbour A covers as much as possible, then B covers the rest", so some valid splits are never found.

   Task 7 adds `ResourcesProduced::can_produce` (a bipartite matching, so polynomial time). Task 8 adds `game::payment`, which enumerates every candidate `(west, east)` purchase with `west + east ≤ cost` (at most 3⁷ = 2187 candidates, for Palace) and keeps the valid, minimal, affordable ones. The legacy `engine/trading.rs`, `cover_resource_costs` and their tests are deleted in Task 8, and their scenarios are ported as `can_produce` and payment tests.
2. **The RNG lives only inside `Game::new`.** No randomness is needed after setup, so a `rng` field on `Game` would be dead code. The fixed consumption order is documented in `setup.rs`. Shuffle and range sampling are implemented locally (`engine/rng.rs`) on top of `ChaCha8Rng::next_u64`, so a `rand` upgrade can never change a replay. The `rand` crate is removed in Task 18.
3. **Ordering of two extra turns at the end of an age.** Babylon B's `PlayLastCard` comes first. Halikarnassós's `BuildFromDiscard` comes after it, so the discard pile then holds every end-of-age discard, including Babylon's if Babylon discards.
4. **`discard_pile` in the view** lists the *distinct names of discarded cards the Halikarnassós player may build* (anything already built is excluded), sorted by name. That keeps the invariant "every option the view offers is accepted by `submit`".
5. **Olympía B copy guild.** Candidates are guilds built by either neighbour that the player has not built. The copied guild's effects count toward the player's score (GuildP, or ScientificP for Scientists Guild), but the guild is not added to their built cards.
6. **Simultaneous effects.** In a turn, every structure and wonder stage is recorded first. Effects then run in seat order. So Vineyard and Bazar count neighbour cards built in the same turn, which follows the simultaneous-reveal convention.
7. **When own production covers a cost, the payment must be empty** (`InvalidPayment` otherwise). When a trade is needed, a submitted payment may be non-minimal as long as it is valid and affordable. Duplicate entries are summed and zero counts ignored. Buying a resource the cost does not require is `InvalidPayment`.
8. **NIF shapes.** Each resolves something the spec left open. All match the Phase 4 spec examples:
   - Setup errors:
     - `InvalidPlayersNumber` encodes as the bare atom `:invalid_players_number`, as the spec's NIF table lists it.
     - `WondersLengthMismatch` encodes as `{:wonders_length_mismatch, %{players: 3, wonders: 1}}`.
   - Single-card actions use tuples:
     - `{:discard, "Altar"}`
     - `{:build_free, "Altar"}`
     - `{:build_from_discard, "Altar"}`
   - `BuildOption` encodes as `{:unavailable, :cannot_afford} | :free | {:coins, 1} | {:trade, [option]}`.
   - `phase` in the view is a flat map `%{kind, age, turn, direction, extra_turn_player, extra_turn_kind}`, which is simplest for HEEx.
9. **`FinalScore` order.** The list is sorted by `rank`, with ties kept in seat order.
10. **`debug_game` JSON.** `GameState` is now nested under `"state"`. Phase 0's test looked for a top-level `"player_states"`; that test is replaced in Task 19.

## Data discrepancies found in `engine/data.rs` (each fixed by its own task)

Checked against the base-game rulebook: every card cost, chain, player-count threshold (the total is 7·n for n = 3..7 in every age), guild and wonder stage.

| # | Card / wonder | Current | Rulebook | Task |
|---|---|---|---|---|
| D1 | Baths (Age I, blue) | `allre(&[])` — gives nothing | 3 victory points | 3 |
| D2 | Magistrates Guild | 2 VP per blue card in neighbouring cities | **1** VP per blue card | 4 |
| D3 | Courthouse (Age II) | dependents `["Senate"]` | no dependents (Senate is free only from Library) | 5 |
| D4 | Forum, Caravansery (yellow); Alexandria A stage 2, B stages 1–2 | choice resources are tradable (`anyre`) | owner-only; neighbours may buy only brown/grey production and the wonder's starting resource | 7 |
| D5 | `Neighbours::get_player_name_from_direction` | destructures `(east, west)` from `(west, east)` data, so East trading post discounts the **west** neighbour | East → east neighbour | 6 |
| D6 | Coins `u8` | Bazar, Vineyard and Tavern can overflow past 255 (debug panic) | coins `u32`, points/tokens `i32` | 2 |

Everything else matches the rulebook: all 68 non-guild cards, 10 guilds and 14 wonder sides, including Babylon B (loom + clay / glass + 2 wood / papyrus + 3 clay), Olympía B (2 ore + loom for copy guild), Halikarnassós B (2 ore / 3 clay / glass + papyrus + loom) and Rhódos B.

## Global Constraints

- Players: 3–7; seat order is the `players` order given to `Game::new`. For seat `i` of `n`: `west = (i + n - 1) % n`, `east = (i + 1) % n`.
- Hands pass to the **west** neighbour in Ages I and III and to the **east** neighbour in Age II.
- Randomness: a single `rand_chacha::ChaCha8Rng::seed_from_u64(seed)`, consumed in a fixed order; `pub const ENGINE_VERSION: u32 = 1;` (bump on any rule change that could alter replays).
- Outcome-affecting iteration uses `Vec` or `BTreeMap`/`BTreeSet`; `HashMap` only for lookups whose iteration order never matters.
- Coins are `u32`; points and military tokens are `i32`.
- Tradable resources: only those produced by brown/grey (RM/MG) structures and the wonder's starting resource. Unit price is 2 by default and 1 with a discount. Trade coins are credited after the turn and validated against pre-turn balances.
- Military tokens: +1/+3/+5 (Age I/II/III) for more shields, −1 for fewer, nothing for equal, against each neighbour.
- Treasury = coins / 3. Science = Σ squares + 7 × complete sets, with choice symbols assigned optimally.
- Rank by `total` desc, then `coins` desc. Equal on both → shared rank.
- Payment options: dedupe, sort by total cost ascending then by `west` total ascending, keep at most 6.
- `src/game/` has **no rustler imports**. `src/nif/` holds DTOs with `#[derive(NifMap | NifTaggedEnum | NifUnitEnum)]` and `From` conversions; the resource is `GameResource(Mutex<Game>)`. NIFs use `lock()` (not `try_lock`) and no dirty schedulers.
- Elixir module name `Elixir.Helios.Core.Native`. Resource atoms are `:wood, :stone, :ore, :clay, :glass, :loom, :papyrus`. Category atoms are `:civilian, :commercial, :guild, :manufactured_good, :military, :raw_material, :scientific`. `ActionError` atoms are the snake_case variant names.
- `rand_chacha = "0.10.0"`: latest stable on crates.io (released 2026-02-02; `rand_core ^0.10`). Verify with `cargo search rand_chacha`. Its `rand_core` re-export provides `Rng::next_u64` and `SeedableRng::seed_from_u64`.
- Commit messages end with `Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>`. Always `git add <explicit paths>` (never `-A`/`.`). Never stage `.DS_Store` files or anything under `backend/`.

## Review Focus

The five failure modes most likely to bite a user, in order. Each has a pinning test in the task named.

1. **Malformed action terms from Elixir** (e.g. `{:build, "Altar"}` or an unknown resource atom). These must raise `ArgumentError` in the calling process and never crash the VM or corrupt the game. Pinned in Task 19, "malformed action terms raise".
2. **Hand-written payments.** Duplicate entries (`[{:wood,1},{:wood,1}]`) are summed and zero counts ignored. A resource the card does not need is `:invalid_payment`, never silently accepted. Pinned in Task 8: `duplicate_and_zero_entries_are_normalised` and `resources_outside_the_cost_are_rejected`.
3. **Seed extremes.** `0` and `2^64 − 1` must both start games. Out-of-range integers are rejected at the Elixir boundary. Pinned in Task 9, `extreme_seeds_start_games`, and Task 19, the seed-range test.
4. **Other players during an extra turn** (Babylon B / Halikarnassós). Their submits are `NotYourTurn`, and their `view` still works: empty hand, `submitted` lists only the acting player. Pinned in Task 16, `others_during_an_extra_turn_can_view_but_not_act`.
5. **A rejected resubmission** (the player changes their mind to something illegal) must leave the earlier valid pending choice intact. Pinned in Task 10, `resubmitting_replaces_the_pending_action_but_invalid_ones_do_not`.

---

## File Structure

| Path (post-Phase-0) | Status | Responsibility |
|---|---|---|
| `core/Cargo.toml` | modify | add `rand_chacha = "0.10.0"` (T1), remove `rand` (T18) |
| `core/src/lib.rs` | modify → replace (T18) | module declarations, test module list, `rustler::init!` |
| `core/src/engine/rng.rs` | create (T1) | `GameRng`: seeded ChaCha8, `below`, `shuffle` |
| `core/src/engine/deck.rs` | modify (T1) | seeded deck generation |
| `core/src/engine/game_init.rs` | modify (T1), delete (T18) | legacy init used only by Phase 0's `api` |
| `core/src/engine/trading.rs` | delete (T8) | replaced by `game/payment.rs` |
| `core/src/engine/data.rs` | modify (T3–T5, T7) | data fixes D1–D4 |
| `core/src/api/mod.rs` | modify (T1), delete (T18) | Phase 0 plain-Rust start_game |
| `core/src/common/` | delete (T18) | empty module |
| `core/src/domain/supply.rs`, `point.rs` | modify (T2) | widened numeric types |
| `core/src/domain/structure.rs` | modify (T5, T10) | accessors `age`, `dependents`, `cost`; `Age::number`/`from_number` |
| `core/src/domain/wonder.rs` | modify (T10) | `WonderSide::stages_total`, `WonderStage::cost` |
| `core/src/domain/player.rs` | modify (T6) | East/West fix |
| `core/src/domain/supply/resources.rs`, `resources/types.rs` | modify (T7, T8) | `can_produce`, `ALL_RESOURCE_TYPES`, `resource_counts`; delete legacy coverage |
| `core/src/domain/game_state.rs` | modify (T2, T7, T9, T10) | `tradable_resources`, owner-only effect, `seats`, `Card: Copy`, `apply_player_decisions(IntoIterator)` |
| `core/src/game/mod.rs` | create (T8/T9) | `Game`, `ENGINE_VERSION`, public re-exports |
| `core/src/game/types.rs` | create (T8/T9) | public API types + internal `Kind`/`Resolved`/`Pending` |
| `core/src/game/payment.rs` | create (T8) | payment validation and option enumeration |
| `core/src/game/setup.rs` | create (T9) | `Game::new`, wonder assignment, dealing |
| `core/src/game/legality.rs` | create (T10) | `check_action`, requirements, market |
| `core/src/game/resolve.rs` | create (T11, T13, T15) | resolution, passing, extra turns, battles, age end |
| `core/src/game/scoring.rs` | create (T12, T14) | final scores, ranks, Olympía B copy guild |
| `core/src/game/view.rs` | create (T16) | `PlayerView` and friends |
| `core/src/game/settings.rs` | create (T18) | `settings()` |
| `core/src/game/test_support.rs` | create (T9…T15) | `#[cfg(test)]` rigging/inspection hooks |
| `core/src/nif/mod.rs`, `core/src/nif/dto.rs` | create (T18) | NIF functions and DTOs |
| `core/src/tests/*.rs` | create/modify | see each task |
| `helios/lib/helios/core/native.ex`, `helios/lib/helios/core.ex` | replace (T19) | NIF stubs and public wrapper |
| `helios/test/helios/core_test.exs` | replace (T19) | ExUnit tests incl. full games for n = 3 and 7 |

## Conventions for every task

- Run Rust commands from `core/`. The test filter is a module path substring, e.g. `cargo test tests::game_setup`. Several filters go after `--`: `cargo test -- tests::rng tests::deck`.
- `cargo fmt` then `cargo fmt --check` before every commit.
- **Clippy gate:** `cargo clippy --all-targets -- -D warnings` becomes mandatory from Task 18 on. From Task 8 to Task 17 the new `game` module has items that only later tasks read (e.g. the `built`/`extra_turns` fields), so rustc emits `dead_code` warnings. Those are expected there, and `cargo test` must still pass. Any *other* warning must be fixed right away.
- Test code must stay clippy-clean too (`assert!` for booleans, arrays instead of `vec!` in `for` loops, no unused helpers).
- Commit after every task, with the explicit `git add` shown.

---

### Task 1: Seeded RNG and deterministic deck generation

**Files:**
- Modify: `core/Cargo.toml`
- Create: `core/src/engine/rng.rs`
- Modify: `core/src/engine/mod.rs`, `core/src/engine/deck.rs`, `core/src/engine/game_init.rs`, `core/src/api/mod.rs`, `core/src/lib.rs` (test module list)
- Test: `core/src/tests/rng.rs` (create), `core/src/tests/deck.rs` (modify)

**Interfaces:**
- Consumes: `crate::engine::data::{AGE_I_STRUCTURES, AGE_II_STRUCTURES, AGE_III_STRUCTURES, GUILD_STRUCTURES, WONDERS}`
- Produces:
  - `pub struct GameRng` with `GameRng::new(seed: u64) -> GameRng`, `next_u64(&mut self) -> u64`, `below(&mut self, n: usize) -> usize` (uniform in `0..n`) and `shuffle<T>(&mut self, items: &mut [T])` (Fisher–Yates).
  - `engine::deck::generate_deck(players_count: usize, rng: &mut GameRng) -> Deck`. RNG order: Age I shuffle, Age II shuffle, guild shuffle, Age III shuffle.

- [ ] **Step 1: Add the dependency**

In `core/Cargo.toml` under `[dependencies]` add:

```toml
rand_chacha = "0.10.0"
```

Run `cargo search rand_chacha --limit 1`. Expected: `rand_chacha = "0.10.0"` (or a newer 0.10.x). If a newer **0.10.x** exists, use it. Do not jump to another minor version without re-checking that `rand_chacha::rand_core::Rng` still provides `next_u64`.

- [ ] **Step 2: Write the failing tests**

Create `core/src/tests/rng.rs`:

```rust
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
```

Replace `core/src/tests/deck.rs` with:

```rust
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
```

In `core/src/lib.rs`, add `pub mod rng;` inside the `#[cfg(test)] mod tests { … }` block (keep the list alphabetical).

- [ ] **Step 3: Run the tests to verify they fail**

Run: `cargo test -- tests::rng tests::deck`
Expected: compile error, `could not find rng in engine` / `generate_deck` takes 1 argument.

- [ ] **Step 4: Implement `GameRng`**

Create `core/src/engine/rng.rs`:

```rust
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
```

In `core/src/engine/mod.rs` add `pub mod rng;`.

- [ ] **Step 5: Seed the deck**

Replace `core/src/engine/deck.rs` with:

```rust
use crate::domain::{Card, Cards, Deck, Effect, Structure};
use crate::engine::data::{
    AGE_III_STRUCTURES, AGE_II_STRUCTURES, AGE_I_STRUCTURES, GUILD_STRUCTURES,
};
use crate::engine::rng::GameRng;

/// Builds the three age decks for `players_count` players.
///
/// RNG consumption order (part of the replay contract): Age I shuffle,
/// Age II shuffle, guild shuffle, Age III shuffle.
pub fn generate_deck(players_count: usize, rng: &mut GameRng) -> Deck {
    let mut age1 = generate_age_deck(players_count, AGE_I_STRUCTURES.iter());
    rng.shuffle(&mut age1);
    let mut age2 = generate_age_deck(players_count, AGE_II_STRUCTURES.iter());
    rng.shuffle(&mut age2);
    let mut age3 = generate_age_deck(players_count, AGE_III_STRUCTURES.iter());
    age3.append(&mut generate_guild_cards(players_count, rng));
    rng.shuffle(&mut age3);
    (age1, age2, age3)
}

fn generate_age_deck(
    players_count: usize,
    structures: impl Iterator<Item = &'static Structure<'static, Effect>>,
) -> Cards {
    let mut cards = vec![];
    for structure in structures {
        for _ in structure
            .thresholds()
            .iter()
            .filter(|threshold| **threshold <= players_count as u8)
        {
            cards.push(Card(structure));
        }
    }
    cards
}

fn generate_guild_cards(players_count: usize, rng: &mut GameRng) -> Cards {
    let mut cards: Cards = GUILD_STRUCTURES.iter().map(Card).collect();
    rng.shuffle(&mut cards);
    cards.truncate(players_count + 2);
    cards
}
```

- [ ] **Step 6: Keep the legacy init path compiling**

Phase 0's `api::start_game` uses `game_init`, which Task 18 deletes. Until then, replace `core/src/engine/game_init.rs` with:

```rust
use crate::domain::{Effect, GameState, Player, PlayersWithWonders, Wonder};
use crate::engine::data::WONDERS;
use crate::engine::deck::generate_deck;
use crate::engine::rng::GameRng;

pub fn init_with_random_wonders(players: Vec<Player>, rng: &mut GameRng) -> GameState {
    let players_count = players.len();
    let mut wonders: Vec<&'static Wonder<'static, Effect>> = WONDERS.iter().collect();
    rng.shuffle(&mut wonders);
    wonders.truncate(players_count);
    let mut players_with_wonders: PlayersWithWonders = vec![];
    for (player, wonder) in players.into_iter().zip(wonders) {
        if rng.below(2) == 0 {
            players_with_wonders.push((player, &wonder.1));
        } else {
            players_with_wonders.push((player, &wonder.2));
        }
    }
    init(players_with_wonders, rng)
}

pub fn init(players_with_wonders: PlayersWithWonders, rng: &mut GameRng) -> GameState {
    let players_count = players_with_wonders.len();
    let deck = generate_deck(players_count, rng);
    let mut game_state = GameState::new(deck, players_with_wonders);
    game_state.init();
    game_state
}
```

In `core/src/api/mod.rs`, at the top of `pub fn start_game(…)`, add:

```rust
    let mut rng = crate::engine::rng::GameRng::new(rand::random::<u64>());
```

Then pass `&mut rng` as the last argument to both `game_init::init_with_random_wonders(…)` and `game_init::init(…)`.

- [ ] **Step 7: Run the tests to verify they pass**

Run: `cargo test`
Expected: PASS for `tests::rng::*` and `tests::deck::*`, with every legacy test still green.

- [ ] **Step 8: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/Cargo.toml core/Cargo.lock core/src/engine/rng.rs core/src/engine/mod.rs core/src/engine/deck.rs core/src/engine/game_init.rs core/src/api/mod.rs core/src/lib.rs core/src/tests/rng.rs core/src/tests/deck.rs
git commit -m "feat(core): seeded ChaCha8 GameRng and deterministic decks

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: Widen numeric types (data discrepancy D6)

**Files:**
- Modify: `core/src/domain/supply.rs`, `core/src/domain/point.rs`, `core/src/domain/game_state.rs`
- Test: `core/src/tests/points.rs`, `core/src/tests/game_effects.rs`

**Interfaces:**
- Produces:
  - Money and military: `Coin = u32`, `MilitarySymbolCount = u32`, `BattleToken = i32`, `TradeValue = u32`.
  - Points: `Point = i32`, and `calculate_treasury_points(coin: Coin) -> Point`.

- [ ] **Step 1: Write the failing tests**

Append to `core/src/tests/points.rs`:

```rust
#[test]
fn test_treasury_points_for_large_coin_counts() {
    assert_eq!(crate::domain::calculate_treasury_points(300), 100);
}
```

Append to `core/src/tests/game_effects.rs`:

```rust
#[test]
fn test_coins_do_not_overflow_past_255() {
    let mut game_state = GameState {
        player_states: hashmap! {
            "a".to_string() => PlayerState {
                player: Player("a".to_string()),
                coins: 254,
                ..default_player_state()
            },
            "b".to_string() => PlayerState {
                player: Player("b".to_string()),
                ..default_player_state()
            },
            "c".to_string() => PlayerState {
                player: Player("c".to_string()),
                ..default_player_state()
            },
        },
        ..default_game_state()
    };
    let tavern = STRUCTURES_BY_NAME.get("Tavern").unwrap();
    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildStructure(Card(tavern))},
    );
    assert_eq!(game_state.get_player_state(&"a".to_string()).coins, 259);
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -- tests::points tests::game_effects::test_coins_do_not_overflow_past_255`
Expected: compile error, `literal out of range for u8` (300).

- [ ] **Step 3: Widen the types**

In `core/src/domain/supply.rs`, replace the type aliases and helpers with:

```rust
pub type Coin = u32;

#[rustfmt::skip]
pub fn calculate_treasury_points(coin: Coin) -> Point { (coin / 3) as Point }

pub type MilitarySymbolCount = u32;

pub type BattleTokens = Vec<BattleToken>;
pub fn calculate_military_points(battle_tokens: &BattleTokens) -> Point {
    battle_tokens.iter().sum()
}
pub type BattleToken = i32;

pub type TradeValue = u32;
```

In `core/src/domain/point.rs`: `pub type Point = i32;`

In `core/src/domain/game_state.rs`, in `dynamic_coin_effect`, change `coins * structures_count as u8` to `coins * structures_count as Coin`.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test`
Expected: PASS (all tests).

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/domain/supply.rs core/src/domain/point.rs core/src/domain/game_state.rs core/src/tests/points.rs core/src/tests/game_effects.rs
git commit -m "fix(core): coins u32, points and tokens i32

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---
### Task 3: Data fix D1 — Baths grants 3 civilian points

**Files:**
- Modify: `core/src/engine/data.rs` (the `Baths` line in `AGE_I_STRUCTURES`)
- Test: `core/src/tests/game_effects.rs`

**Interfaces:**
- Consumes: `GameState::apply_player_decisions`, `PlayerState::calculate_points`, `STRUCTURES_BY_NAME`
- Produces: correct data only.

- [ ] **Step 1: Write the failing test**

Append to `core/src/tests/game_effects.rs`:

```rust
#[test]
fn test_baths_gives_three_civilian_points() {
    let mut game_state = default_game_state();
    let baths = STRUCTURES_BY_NAME.get("Baths").unwrap();
    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildStructure(Card(baths))},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(
        player_state.calculate_points(&game_state).get(&CivilianP),
        Some(&3)
    );
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test tests::game_effects::test_baths_gives_three_civilian_points`
Expected: FAIL, `left: Some(0)`, `right: Some(3)`.

- [ ] **Step 3: Fix the data**

In `core/src/engine/data.rs`, change the Baths entry to:

```rust
        Structure("Baths"   , Civilian, I, vec![pe(CivilianP, 3)], &[], &["Aqueduct"], (0, &[RCost(Stone, 1)]), &[3, 7]),
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `cargo test tests::game_effects`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/engine/data.rs core/src/tests/game_effects.rs
git commit -m "fix(core): Baths grants 3 civilian points

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 4: Data fix D2 — Magistrates Guild scores 1 point per neighbouring blue card

**Files:**
- Modify: `core/src/engine/data.rs` (`Magistrates Guild` in `GUILD_STRUCTURES`)
- Test: `core/src/tests/game_effects.rs`

**Interfaces:**
- Consumes: `StructureBuilder { built_structures, .. }`, `Category::Civilian`
- Produces: correct data only.

- [ ] **Step 1: Write the failing test**

In `core/src/tests/game_effects.rs`, add `Civilian` to the `Category::{…}` import list. Then append:

```rust
#[test]
fn test_magistrates_guild_scores_one_point_per_neighbouring_blue_card() {
    let mut game_state = GameState {
        player_states: hashmap! {
            "a".to_string() => PlayerState {
                player: Player("a".to_string()),
                ..default_player_state()
            },
            "b".to_string() => PlayerState {
                player: Player("b".to_string()),
                structure_builder: StructureBuilder {
                    built_structures: hashmap! { Civilian => hashset! {"Altar"} },
                    ..Default::default()
                },
                ..default_player_state()
            },
            "c".to_string() => PlayerState {
                player: Player("c".to_string()),
                structure_builder: StructureBuilder {
                    built_structures: hashmap! { Civilian => hashset! {"Theater", "Baths"} },
                    ..Default::default()
                },
                ..default_player_state()
            },
        },
        ..default_game_state()
    };
    let magistrates = STRUCTURES_BY_NAME.get("Magistrates Guild").unwrap();
    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildStructure(Card(magistrates))},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(
        player_state.calculate_points(&game_state).get(&GuildP),
        Some(&3)
    );
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test tests::game_effects::test_magistrates_guild`
Expected: FAIL, `left: Some(6)`, `right: Some(3)`.

- [ ] **Step 3: Fix the data**

In `GUILD_STRUCTURES`, change the Magistrates line to:

```rust
        Structure("Magistrates Guild" , Guild, III, vec![dpe(GuildP, &[East, West], &[Civilian], 1)]  , &[], &[], (0, &[RCost(Wood, 3), RCost(Stone, 1), RCost(Loom, 1)])               , &[]),
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test tests::game_effects`
Expected: PASS. The legacy `test_guilds` is unaffected: its Magistrates player has no blue neighbours, so it scores `0 from Magistrates` either way.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/engine/data.rs core/src/tests/game_effects.rs
git commit -m "fix(core): Magistrates Guild is 1 VP per neighbouring blue card

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 5: Data fix D3 — chain metadata integrity (Courthouse)

**Files:**
- Modify: `core/src/domain/structure.rs` (accessors), `core/src/engine/data.rs` (Courthouse)
- Test: `core/src/tests/data.rs` (create), `core/src/lib.rs` (test list: `pub mod data;`)

**Interfaces:**
- Produces:
  - `Structure::age(&self) -> Age`
  - `Structure::dependents(&self) -> Dependents<'a>`
  - `Age::number(&self) -> u8` (None→0, I→1, II→2, III→3)
  - `Age::from_number(n: u8) -> Age` (1→I, 2→II, 3→III, else None)

- [ ] **Step 1: Write the failing test**

Create `core/src/tests/data.rs`:

```rust
use crate::domain::{Age, Effect, Structure};
use crate::engine::data::{
    AGE_III_STRUCTURES, AGE_II_STRUCTURES, AGE_I_STRUCTURES, GUILD_STRUCTURES,
    STRUCTURES_BY_NAME,
};

fn all_structures() -> Vec<&'static Structure<'static, Effect>> {
    AGE_I_STRUCTURES
        .iter()
        .chain(AGE_II_STRUCTURES.iter())
        .chain(AGE_III_STRUCTURES.iter())
        .chain(GUILD_STRUCTURES.iter())
        .collect()
}

#[test]
fn chain_links_are_symmetric_and_point_forward_in_time() {
    for structure in all_structures() {
        for dependency in structure.dependencies() {
            let from = STRUCTURES_BY_NAME
                .get(*dependency)
                .unwrap_or_else(|| panic!("{} chains from unknown {}", structure.name(), dependency));
            assert!(
                from.dependents().contains(&structure.name()),
                "{} is free from {} but {} does not list it as a dependent",
                structure.name(),
                dependency,
                dependency
            );
            assert!(from.age().number() < structure.age().number());
        }
        for dependent in structure.dependents() {
            let to = STRUCTURES_BY_NAME
                .get(*dependent)
                .unwrap_or_else(|| panic!("{} lists unknown dependent {}", structure.name(), dependent));
            assert!(
                to.dependencies().contains(&structure.name()),
                "{} lists {} as a dependent but {} is not free from it",
                structure.name(),
                dependent,
                dependent
            );
        }
    }
}

#[test]
fn age_numbers_round_trip() {
    for age in [Age::I, Age::II, Age::III] {
        assert_eq!(Age::from_number(age.number()), age);
    }
    assert_eq!(Age::None.number(), 0);
}
```

Add `pub mod data;` to the test module list in `core/src/lib.rs`.

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test tests::data`
Expected: compile error, no method `age`/`dependents`/`number`.

- [ ] **Step 3: Add the accessors**

In `core/src/domain/structure.rs`, inside `impl<'a, T> Structure<'a, T>` add:

```rust
    pub fn age(&self) -> Age {
        self.2
    }
    pub fn dependents(&self) -> Dependents<'a> {
        self.5
    }
```

and inside `impl Age` add:

```rust
    pub fn number(&self) -> u8 {
        match self {
            Self::None => 0,
            Self::I => 1,
            Self::II => 2,
            Self::III => 3,
        }
    }
    pub fn from_number(number: u8) -> Age {
        match number {
            1 => Self::I,
            2 => Self::II,
            3 => Self::III,
            _ => Self::None,
        }
    }
```

- [ ] **Step 4: Run the test to verify the data bug surfaces**

Run: `cargo test tests::data`
Expected: FAIL, `Courthouse lists Senate as a dependent but Senate is not free from it`.

- [ ] **Step 5: Fix the data**

In `AGE_II_STRUCTURES`, change the Courthouse line to:

```rust
        Structure("Courthouse", Civilian, II, vec![pe(CivilianP, 4)], &["Scriptorium"], &[]          , (0, &[RCost(Clay, 2), RCost(Loom, 1)])                 , &[3, 5]),
```

- [ ] **Step 6: Run the tests to verify they pass**

Run: `cargo test`
Expected: PASS.

- [ ] **Step 7: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/domain/structure.rs core/src/engine/data.rs core/src/tests/data.rs core/src/lib.rs
git commit -m "fix(core): Courthouse has no chain to Senate; add chain integrity test

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 6: Data fix D5 — East/West swap in `Neighbours`

**Files:**
- Modify: `core/src/domain/player.rs` (`get_player_name_from_direction` and its inline test)
- Test: `core/src/tests/game_effects.rs`

**Interfaces:**
- Produces: `Neighbours::get_player_names_from_directions(&p, &[East])` → the seat at `(i + 1) % n`; `&[West]` → the seat at `(i + n - 1) % n`.

- [ ] **Step 1: Write the failing test**

Append to `core/src/tests/game_effects.rs`:

```rust
#[test]
fn test_east_trading_post_discounts_the_east_neighbour() {
    // Seats a, b, c: b's west neighbour is a, b's east neighbour is c.
    let mut game_state = default_game_state();
    let east_trading_post = STRUCTURES_BY_NAME.get("East trading post").unwrap();
    game_state.apply_player_decisions(
        hashmap! {"b".to_string() => PlayerDecision::BuildStructure(Card(east_trading_post))},
    );
    let player_state = game_state.get_player_state(&"b".to_string());
    assert_eq!(player_state.apply_trading(&"c".to_string(), &Wood), 1);
    assert_eq!(player_state.apply_trading(&"a".to_string(), &Wood), 2);
}
```

In `core/src/domain/player.rs`, replace the two direction assertions in `test_get_player_names_from_directions` so they encode the correct behaviour:

```rust
    assert_eq!(
        neighbours.get_player_names_from_directions(&"b".to_string(), &[PlayerDirection::East]),
        HashSet::from_iter(vec!["c".to_string()].into_iter())
    );
    assert_eq!(
        neighbours.get_player_names_from_directions(&"b".to_string(), &[PlayerDirection::West]),
        HashSet::from_iter(vec!["a".to_string()].into_iter())
    );
```

(Phase 0 may have reformatted this test, e.g. `HashSet::from(["c".to_string()])`. Keep its style and change only the expected names: East → `"c"`, West → `"a"`.)

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -- test_east_trading_post test_get_player_names_from_directions`
Expected: both FAIL (the East discount lands on `a`).

- [ ] **Step 3: Fix the destructuring**

In `get_player_name_from_direction`:

```rust
        let (west, east) = self.data.get(player_name).unwrap();
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/domain/player.rs core/src/tests/game_effects.rs
git commit -m "fix(core): East/West neighbour directions were swapped

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 7: Production feasibility and tradable vs owner-only resources (data discrepancy D4)

**Files:**
- Modify: `core/src/domain/supply/resources/types.rs`, `core/src/domain/supply/resources.rs`, `core/src/domain/game_state.rs`, `core/src/engine/data.rs`
- Test: `core/src/tests/production.rs` (create), `core/src/lib.rs` (test list: `pub mod production;`)

**Interfaces:**
- Produces:
  - `pub const ALL_RESOURCE_TYPES: [ResourceType; 7]` (declaration order, so `ALL_RESOURCE_TYPES[i] as usize == i`)
  - `pub fn resource_counts(costs: &[ResourceCost]) -> [u8; 7]`
  - `ResourcesProduced::can_produce(&self, need: &[ResourceCost]) -> bool`
  - `ResourcesProduced::can_produce_counts(&self, need: &[u8; 7]) -> bool`: fixed resources first, then each choice card used at most once (bipartite matching)
  - `PlayerState.tradable_resources: ResourcesProduced`: the subset neighbours may buy
  - `owner_any_resources_effect(resource_types) -> Effect`: adds a choice to `resources_produced` only
  - `all_resources_effect` / `any_resources_effect` add to **both** `resources_produced` and `tradable_resources`

- [ ] **Step 1: Write the failing tests**

Create `core/src/tests/production.rs`:

```rust
use super::helpers::{default_game_state, TEST_WONDER_SIDE};
use crate::domain::{
    Card, GameState, Player, PlayerDecision, ResourceCost, ResourceCount, ResourceType,
    ResourceType::*, ResourceTypes, ResourcesProduced, ALL_RESOURCE_TYPES,
};
use crate::engine::data::{STRUCTURES_BY_NAME, WONDERS_BY_NAME};
use maplit::hashmap;
use std::collections::HashMap;
use std::time::{Duration, Instant};

fn produced(
    single: HashMap<ResourceType, ResourceCount>,
    any: &[ResourceTypes<'static>],
) -> ResourcesProduced {
    ResourcesProduced {
        single_resources: single,
        any_resources: any.to_vec(),
    }
}

#[test]
fn resource_indexes_follow_declaration_order() {
    for (index, resource_type) in ALL_RESOURCE_TYPES.iter().enumerate() {
        assert_eq!(*resource_type as usize, index);
    }
}

#[test]
fn nothing_is_needed_for_an_empty_cost() {
    assert!(produced(hashmap! {}, &[]).can_produce(&[]));
}

#[test]
fn fixed_resources_cover_matching_costs_only() {
    let p = produced(hashmap! {Wood => 2, Loom => 1}, &[]);
    assert!(p.can_produce(&[ResourceCost(Wood, 1)]));
    assert!(p.can_produce(&[ResourceCost(Wood, 2)]));
    assert!(p.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Wood, 3)]));
    assert!(!p.can_produce(&[ResourceCost(Loom, 2)]));
    assert!(!p.can_produce(&[ResourceCost(Clay, 1)]));
}

#[test]
fn each_choice_card_is_used_once() {
    let p = produced(hashmap! {}, &[&[Wood, Loom], &[Wood, Ore]]);
    assert!(p.can_produce(&[ResourceCost(Wood, 1)]));
    assert!(p.can_produce(&[ResourceCost(Wood, 2)]));
    assert!(p.can_produce(&[ResourceCost(Loom, 1), ResourceCost(Ore, 1)]));
    assert!(p.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]));
    assert!(p.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Ore, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Wood, 3)]));
    assert!(!p.can_produce(&[ResourceCost(Wood, 2), ResourceCost(Loom, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Loom, 2)]));
    assert!(!p.can_produce(&[ResourceCost(Clay, 1)]));
}

#[test]
fn identical_choice_cards_combine() {
    let p = produced(hashmap! {}, &[&[Wood, Loom], &[Wood, Loom]]);
    assert!(p.can_produce(&[ResourceCost(Wood, 2)]));
    assert!(p.can_produce(&[ResourceCost(Loom, 2)]));
    assert!(p.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Wood, 3)]));
    assert!(!p.can_produce(&[ResourceCost(Ore, 1)]));
}

#[test]
fn fixed_and_choice_resources_combine() {
    let p = produced(hashmap! {Clay => 1}, &[&[Wood, Loom]]);
    assert!(p.can_produce(&[ResourceCost(Clay, 1), ResourceCost(Wood, 1)]));
    assert!(p.can_produce(&[ResourceCost(Loom, 1), ResourceCost(Clay, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Clay, 2)]));
    assert!(!p.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Stone, 1)]));
    let q = produced(hashmap! {Wood => 1}, &[&[Wood, Loom]]);
    assert!(q.can_produce(&[ResourceCost(Wood, 2)]));
    assert!(q.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]));
    assert!(!q.can_produce(&[ResourceCost(Wood, 2), ResourceCost(Loom, 1)]));
}

#[test]
fn matching_reroutes_earlier_choices() {
    // A greedy assignment would give the Wood/Loom card to Wood and then fail on Loom.
    let p = produced(hashmap! {}, &[&[Wood, Loom], &[Wood]]);
    assert!(p.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]));
}

#[test]
fn many_choice_cards_stay_fast() {
    let mut choices: Vec<ResourceTypes<'static>> =
        vec![&[Wood, Stone, Ore, Clay] as ResourceTypes<'static>; 6];
    choices.extend([&[Glass, Loom, Papyrus] as ResourceTypes<'static>; 4]);
    let p = produced(hashmap! {}, &choices);
    let palace: Vec<ResourceCost> = ALL_RESOURCE_TYPES
        .iter()
        .map(|resource_type| ResourceCost(*resource_type, 1))
        .collect();
    let started = Instant::now();
    assert!(p.can_produce(&palace));
    assert!(!p.can_produce(&[ResourceCost(Wood, 7), ResourceCost(Glass, 1)]));
    assert!(!p.can_produce(&[ResourceCost(Wood, 6), ResourceCost(Glass, 5)]));
    assert!(started.elapsed() < Duration::from_millis(50));
}

fn build(game_state: &mut GameState, player: &str, card: &str) {
    let structure = STRUCTURES_BY_NAME.get(card).unwrap();
    game_state.apply_player_decisions(vec![(
        player.to_string(),
        PlayerDecision::BuildStructure(Card(structure)),
    )]);
}

#[test]
fn brown_and_grey_production_is_tradable_but_yellow_choices_are_not() {
    let mut game_state = default_game_state();
    for card in ["Tree Farm", "Loom", "Forum", "Caravansery"] {
        build(&mut game_state, "a", card);
    }
    let player_state = game_state.get_player_state(&"a".to_string());
    let tradable = &player_state.tradable_resources;
    assert!(tradable.can_produce(&[ResourceCost(Wood, 1)]));
    assert!(tradable.can_produce(&[ResourceCost(Loom, 1)]));
    assert!(!tradable.can_produce(&[ResourceCost(Loom, 2)]));
    assert!(!tradable.can_produce(&[ResourceCost(Wood, 1), ResourceCost(Clay, 1)]));
    assert!(player_state.resources_produced.can_produce(&[
        ResourceCost(Wood, 1),
        ResourceCost(Clay, 1),
        ResourceCost(Loom, 2)
    ]));
}

#[test]
fn wonder_starting_resource_is_tradable_but_stage_choices_are_not() {
    let alexandria = *WONDERS_BY_NAME.get("Alexandria").unwrap();
    let mut game_state = GameState::new(
        Default::default(),
        vec![
            (Player("a".to_string()), &alexandria.1),
            (Player("b".to_string()), &*TEST_WONDER_SIDE),
            (Player("c".to_string()), &*TEST_WONDER_SIDE),
        ],
    );
    game_state.init();
    let tavern = STRUCTURES_BY_NAME.get("Tavern").unwrap();
    for _ in 0..2 {
        game_state.apply_player_decisions(vec![(
            "a".to_string(),
            PlayerDecision::BuildNextWonderStage(Card(tavern)),
        )]);
    }
    let player_state = game_state.get_player_state(&"a".to_string());
    assert!(player_state
        .tradable_resources
        .can_produce(&[ResourceCost(Glass, 1)]));
    assert!(!player_state
        .tradable_resources
        .can_produce(&[ResourceCost(Wood, 1)]));
    assert!(player_state
        .resources_produced
        .can_produce(&[ResourceCost(Glass, 1), ResourceCost(Wood, 1)]));
}
```

Add `pub mod production;` to the test module list in `core/src/lib.rs`.

> `apply_player_decisions` takes a `HashMap` today. The two `vec![…]` calls above compile only after Step 5 changes it to accept any `IntoIterator<Item = (PName, PlayerDecision)>`. That change belongs here because this task is its first user; the legacy `hashmap!{…}` callers keep compiling.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test tests::production`
Expected: compile errors: `ALL_RESOURCE_TYPES`, `can_produce` and `tradable_resources` not found; `apply_player_decisions` expects a `HashMap`.

- [ ] **Step 3: Add counts and constants to `types.rs`**

In `core/src/domain/supply/resources/types.rs`, after the `ResourceType` enum add:

```rust
pub const ALL_RESOURCE_TYPES: [ResourceType; 7] = [
    ResourceType::Wood,
    ResourceType::Stone,
    ResourceType::Ore,
    ResourceType::Clay,
    ResourceType::Glass,
    ResourceType::Loom,
    ResourceType::Papyrus,
];

/// Units needed per resource type, indexed by `ResourceType as usize`.
pub fn resource_counts(costs: &[ResourceCost]) -> [u8; 7] {
    let mut counts = [0u8; 7];
    for ResourceCost(resource_type, count) in costs {
        counts[*resource_type as usize] += count;
    }
    counts
}
```

In `core/src/domain/supply/resources.rs`, add `resource_counts` and `ALL_RESOURCE_TYPES` to the existing `pub use types::{…}` list.

- [ ] **Step 4: Implement `can_produce`**

In `core/src/domain/supply/resources.rs`, inside `impl ResourcesProduced` add:

```rust
    /// True when this production alone pays `need`: fixed resources first,
    /// then each choice card supplies at most one unit.
    pub fn can_produce(&self, need: &[ResourceCost]) -> bool {
        self.can_produce_counts(&resource_counts(need))
    }

    pub fn can_produce_counts(&self, need: &[u8; 7]) -> bool {
        let mut missing: Vec<ResourceType> = Vec::new();
        for (resource_type, needed) in ALL_RESOURCE_TYPES.iter().zip(need.iter()) {
            let have = self
                .single_resources
                .get(resource_type)
                .copied()
                .unwrap_or(0);
            missing.extend(std::iter::repeat_n(
                *resource_type,
                usize::from(needed.saturating_sub(have)),
            ));
        }
        if missing.len() > self.any_resources.len() {
            return false;
        }
        let mut owner: Vec<Option<usize>> = vec![None; self.any_resources.len()];
        (0..missing.len()).all(|unit| {
            let mut visited = vec![false; self.any_resources.len()];
            augment(unit, &missing, &self.any_resources, &mut owner, &mut visited)
        })
    }
```

and at module level (outside the impl):

```rust
/// Kuhn's augmenting path: give `unit` a choice card, re-routing earlier units
/// to other cards when needed. `owner[card]` is the unit currently using it.
fn augment(
    unit: usize,
    units: &[ResourceType],
    choices: &[ResourceTypes<'static>],
    owner: &mut [Option<usize>],
    visited: &mut [bool],
) -> bool {
    for (card, options) in choices.iter().enumerate() {
        if visited[card] || !options.contains(&units[unit]) {
            continue;
        }
        visited[card] = true;
        let available = match owner[card] {
            None => true,
            Some(other) => augment(other, units, choices, owner, visited),
        };
        if available {
            owner[card] = Some(unit);
            return true;
        }
    }
    false
}
```

- [ ] **Step 5: Add tradable production and the owner-only effect**

In `core/src/domain/game_state.rs`:

1. Add the field `pub tradable_resources: ResourcesProduced,` to `PlayerState`, right after `resources_produced`. Initialise it with `tradable_resources: Default::default(),` in `PlayerState::new`. Add `.field("tradable_resources", &self.tradable_resources)` to the `Debug` impl. Add `s.serialize_field("tradable_resources", &self.tradable_resources)?;` to the `Serialize` impl, and raise its field count by one.
2. Replace `all_resources_effect` and `any_resources_effect` and add the owner-only variant:

```rust
pub fn all_resources_effect(resource_types: ResourceTypes<'static>) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state
            .resources_produced
            .add_all_resources(resource_types);
        player_state
            .tradable_resources
            .add_all_resources(resource_types);
    }))
}

pub fn any_resources_effect(resource_types: ResourceTypes<'static>) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state
            .resources_produced
            .add_any_resources(resource_types);
        player_state
            .tradable_resources
            .add_any_resources(resource_types);
    }))
}

/// A choice resource only its owner may use (yellow cards, wonder stages).
pub fn owner_any_resources_effect(resource_types: ResourceTypes<'static>) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state
            .resources_produced
            .add_any_resources(resource_types);
    }))
}
```

3. Change the signature of `apply_player_decisions` (body unchanged):

```rust
    pub fn apply_player_decisions(
        &mut self,
        player_decisions: impl IntoIterator<Item = (PName, PlayerDecision)>,
    ) {
```

- [ ] **Step 6: Mark owner-only choices in the data**

In `core/src/engine/data.rs`, add `owner_any_resources_effect as oanyre,` to the `use crate::domain::{…}` list. Then replace `anyre` with `oanyre` in exactly these places:

- `Forum`: `vec![oanyre(&[Loom, Glass, Papyrus])]`
- `Caravansery`: `vec![oanyre(&[Wood, Stone, Ore, Clay])]`
- `Alexandria - A`, stage 2: `WonderStage(&[RCost(Ore, 2)],   vec![oanyre(&[Wood, Stone, Ore, Clay])])`
- `Alexandria - B`, stage 1: `WonderStage(&[RCost(Clay, 2)], vec![oanyre(&[Wood, Stone, Ore, Clay])])`
- `Alexandria - B`, stage 2: `WonderStage(&[RCost(Wood, 2)], vec![oanyre(&[Loom, Glass, Papyrus])])`

The brown choice cards (Tree Farm, Excavation, Clay Pit, Timber Yard, Forest Cave, Mine) keep `anyre`, so they stay tradable.

- [ ] **Step 7: Run the tests to verify they pass**

Run: `cargo test`
Expected: PASS. That includes the legacy `test_alexandria`, which inspects `resources_produced` only.

- [ ] **Step 8: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/domain/supply/resources/types.rs core/src/domain/supply/resources.rs core/src/domain/game_state.rs core/src/engine/data.rs core/src/tests/production.rs core/src/lib.rs
git commit -m "feat(core): polynomial can_produce and tradable vs owner-only resources

Forum, Caravansery and Alexandria stage choices are owner-only.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 8: Payment module (validation and option enumeration); remove legacy trading

**Files:**
- Create: `core/src/game/mod.rs` (minimal), `core/src/game/types.rs` (payment types), `core/src/game/payment.rs`
- Modify: `core/src/lib.rs` (`pub mod game;`, test list), `core/src/engine/mod.rs`
- Delete: `core/src/engine/trading.rs`, `core/src/tests/trading.rs`, `core/src/tests/resources.rs`, `core/src/domain/supply/resources/test_fixtures.rs`
- Modify (deletions): `core/src/domain/supply/resources.rs`, `core/src/domain/supply/resources/types.rs`, `core/src/domain/game_state.rs`
- Test: `core/src/tests/game_payment.rs` (create)

**Interfaces:**
- Consumes: `ResourcesProduced::can_produce_counts`, `resource_counts`, `ALL_RESOURCE_TYPES`
- Produces:
  - `pub struct Payment { pub west: Vec<(ResourceType, u8)>, pub east: Vec<(ResourceType, u8)> }`, with derives `Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize` and `Payment::is_empty(&self) -> bool`.
  - `pub struct PaymentOption { pub payment: Payment, pub west_coins: u32, pub east_coins: u32, pub bank_coins: u32 }`
  - In `crate::game::payment`:
    - `pub(crate) struct Market<'a> { pub own, pub west, pub east: &'a ResourcesProduced, pub west_prices: [u32; 7], pub east_prices: [u32; 7], pub coins: u32 }`. Here `west`/`east` are the neighbours' **tradable** production and `own` is the buyer's full production.
    - `check_payment(m: &Market, coin_cost: u32, cost: &[ResourceCost], payment: &Payment) -> Option<(u32, u32)>` returns `(west_coins, east_coins)`.
    - `payment_options(m: &Market, coin_cost: u32, cost: &[ResourceCost]) -> Vec<PaymentOption>` returns at most 6 options: minimal, affordable, deduped, sorted by (total, west, payment).

- [ ] **Step 1: Write the failing tests**

Create `core/src/tests/game_payment.rs`:

```rust
use crate::domain::{ResourceCost, ResourceCount, ResourceType, ResourceType::*, ResourceTypes, ResourcesProduced};
use crate::game::payment::{check_payment, payment_options, Market};
use crate::game::{Payment, PaymentOption};
use maplit::hashmap;
use std::collections::HashMap;

fn prod(single: HashMap<ResourceType, ResourceCount>, any: &[ResourceTypes<'static>]) -> ResourcesProduced {
    ResourcesProduced { single_resources: single, any_resources: any.to_vec() }
}

fn market<'a>(own: &'a ResourcesProduced, west: &'a ResourcesProduced, east: &'a ResourcesProduced, coins: u32) -> Market<'a> {
    Market { own, west, east, west_prices: [2; 7], east_prices: [2; 7], coins }
}

fn pay(west: &[(ResourceType, u8)], east: &[(ResourceType, u8)]) -> Payment {
    Payment { west: west.to_vec(), east: east.to_vec() }
}

fn totals(options: &[PaymentOption]) -> Vec<(u32, u32)> {
    options.iter().map(|o| (o.west_coins, o.east_coins)).collect()
}

#[test]
fn buys_a_missing_resource_from_the_neighbour_that_sells_it() {
    let (own, west, east) = (prod(hashmap! {}, &[]), prod(hashmap! {Wood => 1}, &[]), prod(hashmap! {}, &[]));
    let m = market(&own, &west, &east, 3);
    let cost = [ResourceCost(Wood, 1)];
    assert_eq!(
        payment_options(&m, 0, &cost),
        vec![PaymentOption { payment: pay(&[(Wood, 1)], &[]), west_coins: 2, east_coins: 0, bank_coins: 0 }]
    );
    assert_eq!(check_payment(&m, 0, &cost, &pay(&[(Wood, 1)], &[])), Some((2, 0)));
    assert_eq!(check_payment(&m, 0, &cost, &pay(&[], &[(Wood, 1)])), None);
    assert_eq!(check_payment(&m, 0, &cost, &pay(&[], &[])), None);
}

#[test]
fn nothing_can_be_bought_when_no_neighbour_produces_it() {
    let (own, west, east) = (prod(hashmap! {}, &[]), prod(hashmap! {Ore => 3}, &[]), prod(hashmap! {Clay => 1}, &[]));
    let m = market(&own, &west, &east, 10);
    assert!(payment_options(&m, 0, &[ResourceCost(Wood, 1)]).is_empty());
}

#[test]
fn own_production_covers_part_of_the_cost() {
    let (own, west, east) = (prod(hashmap! {Wood => 1}, &[]), prod(hashmap! {Wood => 2}, &[]), prod(hashmap! {}, &[]));
    let m = market(&own, &west, &east, 3);
    let cost = [ResourceCost(Wood, 2)];
    assert_eq!(totals(&payment_options(&m, 0, &cost)), vec![(2, 0)]);
    // Buying both units is valid but costs 4 > 3 coins.
    assert_eq!(check_payment(&m, 0, &cost, &pay(&[(Wood, 2)], &[])), None);
}

#[test]
fn options_are_sorted_by_total_then_west_and_capped_at_six() {
    let (own, west, east) = (
        prod(hashmap! {}, &[]),
        prod(hashmap! {Wood => 1, Stone => 1, Ore => 1}, &[]),
        prod(hashmap! {Wood => 1, Stone => 1, Ore => 1}, &[]),
    );
    let mut m = market(&own, &west, &east, 20);
    m.east_prices = [1; 7];
    let options = payment_options(&m, 0, &[ResourceCost(Wood, 1), ResourceCost(Stone, 1), ResourceCost(Ore, 1)]);
    assert_eq!(options.len(), 6);
    assert_eq!(options[0].payment, pay(&[], &[(Wood, 1), (Stone, 1), (Ore, 1)]));
    let keys: Vec<(u32, u32)> = options.iter().map(|o| (o.west_coins + o.east_coins, o.west_coins)).collect();
    let mut sorted = keys.clone();
    sorted.sort();
    assert_eq!(keys, sorted);
}

#[test]
fn unaffordable_options_are_dropped_including_the_bank_cost() {
    let (own, west, east) = (prod(hashmap! {}, &[]), prod(hashmap! {Wood => 1}, &[]), prod(hashmap! {}, &[]));
    assert!(payment_options(&market(&own, &west, &east, 1), 0, &[ResourceCost(Wood, 1)]).is_empty());
    let options = payment_options(&market(&own, &west, &east, 3), 1, &[ResourceCost(Wood, 1)]);
    assert_eq!(options[0].bank_coins, 1);
    assert!(payment_options(&market(&own, &west, &east, 2), 1, &[ResourceCost(Wood, 1)]).is_empty());
}

#[test]
fn a_neighbour_choice_card_supplies_one_unit_per_turn() {
    let (own, west, east) = (prod(hashmap! {}, &[]), prod(hashmap! {}, &[&[Wood, Clay]]), prod(hashmap! {Clay => 1}, &[]));
    let m = market(&own, &west, &east, 10);
    let cost = [ResourceCost(Wood, 1), ResourceCost(Clay, 1)];
    assert_eq!(check_payment(&m, 0, &cost, &pay(&[(Wood, 1), (Clay, 1)], &[])), None);
    assert_eq!(check_payment(&m, 0, &cost, &pay(&[(Wood, 1)], &[(Clay, 1)])), Some((2, 2)));
    assert_eq!(payment_options(&m, 0, &cost).len(), 1);
}

#[test]
fn discounted_prices_apply() {
    let (own, west, east) = (prod(hashmap! {}, &[]), prod(hashmap! {Wood => 1}, &[]), prod(hashmap! {Wood => 1}, &[]));
    let mut m = market(&own, &west, &east, 5);
    m.west_prices[Wood as usize] = 1;
    assert_eq!(totals(&payment_options(&m, 0, &[ResourceCost(Wood, 1)])), vec![(1, 0), (0, 2)]);
}

#[test]
fn duplicate_and_zero_entries_are_normalised() {
    let (own, west, east) = (prod(hashmap! {}, &[]), prod(hashmap! {Wood => 2}, &[]), prod(hashmap! {}, &[]));
    let m = market(&own, &west, &east, 4);
    let payment = pay(&[(Wood, 1), (Wood, 1), (Stone, 0)], &[]);
    assert_eq!(check_payment(&m, 0, &[ResourceCost(Wood, 2)], &payment), Some((4, 0)));
}

#[test]
fn resources_outside_the_cost_are_rejected() {
    let (own, west, east) = (prod(hashmap! {}, &[]), prod(hashmap! {Wood => 1, Glass => 1}, &[]), prod(hashmap! {}, &[]));
    let m = market(&own, &west, &east, 10);
    let cost = [ResourceCost(Wood, 1)];
    assert_eq!(check_payment(&m, 0, &cost, &pay(&[(Wood, 1), (Glass, 1)], &[])), None);
    assert_eq!(check_payment(&m, 0, &cost, &pay(&[(Wood, 2)], &[])), None);
}

#[test]
fn palace_sized_costs_enumerate_quickly() {
    let everything = hashmap! {Wood => 1, Stone => 1, Ore => 1, Clay => 1, Glass => 1, Loom => 1, Papyrus => 1};
    let (own, west, east) = (prod(hashmap! {}, &[]), prod(everything.clone(), &[]), prod(everything, &[]));
    let m = market(&own, &west, &east, 30);
    let cost: Vec<ResourceCost> = [Wood, Stone, Ore, Clay, Glass, Loom, Papyrus].iter().map(|r| ResourceCost(*r, 1)).collect();
    let started = std::time::Instant::now();
    assert_eq!(payment_options(&m, 0, &cost).len(), 6);
    assert!(started.elapsed() < std::time::Duration::from_millis(500));
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test tests::game_payment`
Expected: compile error, `could not find game in the crate root`.

- [ ] **Step 3: Create the payment types and module skeleton**

Create `core/src/game/types.rs`:

```rust
use crate::domain::ResourceType;
use serde::Serialize;

/// Resources bought from each neighbour. Entries for the same resource are
/// summed; zero counts are ignored.
#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Payment {
    pub west: Vec<(ResourceType, u8)>,
    pub east: Vec<(ResourceType, u8)>,
}

impl Payment {
    pub fn is_empty(&self) -> bool {
        self.west
            .iter()
            .chain(&self.east)
            .all(|(_, count)| *count == 0)
    }
}

/// One way to pay for a card, with the coins going to each party.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PaymentOption {
    pub payment: Payment,
    pub west_coins: u32,
    pub east_coins: u32,
    pub bank_coins: u32,
}
```

Create `core/src/game/mod.rs`:

```rust
//! Rustler-free game API.
pub(crate) mod payment;
mod types;

pub use crate::domain::{Category, ResourceType};
pub use types::{Payment, PaymentOption};
```

In `core/src/lib.rs`, add `pub mod game;` next to the other module declarations, and add `pub mod game_payment;` to the test list.

- [ ] **Step 4: Implement `payment.rs`**

Create `core/src/game/payment.rs`:

```rust
//! Buying resources from neighbours: validating a submitted [`Payment`] and
//! enumerating the cheapest valid payments for the view.
//!
//! A candidate is a pair of per-resource purchase counts `(west, east)` with
//! `west + east <= cost`. It is valid when each neighbour can produce its part
//! from *tradable* production and the buyer's own production covers the rest.
use super::types::{Payment, PaymentOption};
use crate::domain::{resource_counts, ResourceCost, ResourceType, ResourcesProduced, ALL_RESOURCE_TYPES};

pub(crate) type Counts = [u8; 7];
pub(crate) const MAX_OPTIONS: usize = 6;

pub(crate) struct Market<'a> {
    /// The buyer's full production (including owner-only choices).
    pub own: &'a ResourcesProduced,
    /// The west neighbour's tradable production.
    pub west: &'a ResourcesProduced,
    /// The east neighbour's tradable production.
    pub east: &'a ResourcesProduced,
    pub west_prices: [u32; 7],
    pub east_prices: [u32; 7],
    /// The buyer's coins before this turn.
    pub coins: u32,
}

pub(crate) fn check_payment(
    market: &Market,
    coin_cost: u32,
    cost: &[ResourceCost],
    payment: &Payment,
) -> Option<(u32, u32)> {
    let need = resource_counts(cost);
    let west = side_counts(&payment.west, &need)?;
    let east = side_counts(&payment.east, &need)?;
    if !is_valid(market, &need, &west, &east) {
        return None;
    }
    let west_coins = coins_for(&west, &market.west_prices);
    let east_coins = coins_for(&east, &market.east_prices);
    (market.coins >= coin_cost + west_coins + east_coins).then_some((west_coins, east_coins))
}

pub(crate) fn payment_options(
    market: &Market,
    coin_cost: u32,
    cost: &[ResourceCost],
) -> Vec<PaymentOption> {
    let need = resource_counts(cost);
    let mut candidates = Vec::new();
    split(0, &need, &mut [0; 7], &mut [0; 7], &mut candidates);
    let mut options: Vec<PaymentOption> = candidates
        .into_iter()
        .filter(|(west, east)| is_valid(market, &need, west, east))
        .filter(|(west, east)| is_minimal(market, &need, west, east))
        .map(|(west, east)| PaymentOption {
            payment: Payment {
                west: to_list(&west),
                east: to_list(&east),
            },
            west_coins: coins_for(&west, &market.west_prices),
            east_coins: coins_for(&east, &market.east_prices),
            bank_coins: coin_cost,
        })
        .filter(|option| market.coins >= option.bank_coins + option.west_coins + option.east_coins)
        .collect();
    options.sort_by(|a, b| {
        (a.west_coins + a.east_coins, a.west_coins, &a.payment)
            .cmp(&(b.west_coins + b.east_coins, b.west_coins, &b.payment))
    });
    options.dedup();
    options.truncate(MAX_OPTIONS);
    options
}

/// Every `(west, east)` with `west[i] + east[i] <= need[i]`.
fn split(index: usize, need: &Counts, west: &mut Counts, east: &mut Counts, out: &mut Vec<(Counts, Counts)>) {
    if index == need.len() {
        out.push((*west, *east));
        return;
    }
    for from_west in 0..=need[index] {
        for from_east in 0..=(need[index] - from_west) {
            west[index] = from_west;
            east[index] = from_east;
            split(index + 1, need, west, east, out);
        }
    }
    west[index] = 0;
    east[index] = 0;
}

fn is_valid(market: &Market, need: &Counts, west: &Counts, east: &Counts) -> bool {
    let mut own = [0u8; 7];
    for index in 0..need.len() {
        match need[index]
            .checked_sub(west[index])
            .and_then(|rest| rest.checked_sub(east[index]))
        {
            Some(rest) => own[index] = rest,
            None => return false,
        }
    }
    market.west.can_produce_counts(west)
        && market.east.can_produce_counts(east)
        && market.own.can_produce_counts(&own)
}

/// No single purchased unit can be dropped (own production is monotone, so
/// this is equivalent to "no dominated valid purchase exists").
fn is_minimal(market: &Market, need: &Counts, west: &Counts, east: &Counts) -> bool {
    for index in 0..need.len() {
        if west[index] > 0 {
            let mut fewer = *west;
            fewer[index] -= 1;
            if is_valid(market, need, &fewer, east) {
                return false;
            }
        }
        if east[index] > 0 {
            let mut fewer = *east;
            fewer[index] -= 1;
            if is_valid(market, need, west, &fewer) {
                return false;
            }
        }
    }
    true
}

/// Sums a submitted side; `None` if it exceeds the cost for any resource.
fn side_counts(side: &[(ResourceType, u8)], need: &Counts) -> Option<Counts> {
    let mut counts = [0u8; 7];
    for (resource_type, count) in side {
        let index = *resource_type as usize;
        counts[index] = counts[index].checked_add(*count)?;
        if counts[index] > need[index] {
            return None;
        }
    }
    Some(counts)
}

fn coins_for(counts: &Counts, prices: &[u32; 7]) -> u32 {
    counts
        .iter()
        .zip(prices)
        .map(|(count, price)| u32::from(*count) * price)
        .sum()
}

fn to_list(counts: &Counts) -> Vec<(ResourceType, u8)> {
    ALL_RESOURCE_TYPES
        .iter()
        .zip(counts)
        .filter(|(_, count)| **count > 0)
        .map(|(resource_type, count)| (*resource_type, *count))
        .collect()
}
```

- [ ] **Step 5: Run the payment tests to verify they pass**

Run: `cargo test tests::game_payment`
Expected: PASS (10 tests).

- [ ] **Step 6: Delete the legacy coverage/trading code**

1. `git rm core/src/engine/trading.rs core/src/tests/trading.rs core/src/tests/resources.rs core/src/domain/supply/resources/test_fixtures.rs`. Remove `pub mod trading;` from `core/src/engine/mod.rs`, and `pub mod trading;` / `pub mod resources;` from the test list in `core/src/lib.rs`. (The `resources.rs` scenarios are ported as `can_produce` assertions in `tests/production.rs`; the `trading.rs` scenarios are covered by `tests/game_payment.rs`.)
2. In `core/src/domain/supply/resources.rs`, delete all of the following:
   - the functions `cover_resource_costs` (the method), `match_single_resources`, `apply_any_resources`, `compute_combinations`, `match_any_resource` and `resource_types_to_resource_costs`;
   - the type aliases `ResourceCostOptions` and `ResourceCostOption`;
   - the `#[cfg(test)] mod test_fixtures;` line;
   - the tests `test_compute_combinations` and `test_apply_any_resources`, together with the `#[cfg(test)] use ResourceType::*;` they needed.

   Change the `pub use types::{…}` line to:
   ```rust
   pub use types::{
       resource_counts, ResourceCost, ResourceCosts, ResourceCount, ResourceType, ResourceTypes,
       ALL_RESOURCE_TYPES,
   };
   ```
   Then remove imports that became unused: `itertools::Itertools`, `maplit::hashset`, `std::collections::HashSet`, `std::iter`.
3. In `core/src/domain/supply/resources/types.rs`, delete `diff_resource_costs`, `resource_costs_to_map`, `resource_cost_map_to_resource_costs`, the `Combination` type alias and the `test_diff_resource_costs` test (with its `#[cfg(test)] use ResourceType::*;`). Remove the now-unused `itertools::Itertools` and `std::collections::HashMap` imports.
4. In `core/src/domain/game_state.rs`, delete `PlayerState::cover_resource_costs` and `PlayerState::add_trade_action_move` (its only caller was `tests/trading.rs`), then drop `ResourceCostOptions` from the `use super::supply::{…}` list.
5. Run `cargo build --all-targets 2>&1 | grep -E "^(error|warning)"` and fix any remaining unused-import errors or warnings in the files above. Warnings about new `game` items are expected, per the Conventions.

- [ ] **Step 7: Run all tests**

Run: `cargo test`
Expected: PASS.

- [ ] **Step 8: Commit**

```bash
cargo fmt && cargo fmt --check
# (the four deleted files were already staged by `git rm` in Step 6)
git add core/src/game/mod.rs core/src/game/types.rs core/src/game/payment.rs core/src/lib.rs core/src/engine/mod.rs core/src/domain/supply/resources.rs core/src/domain/supply/resources/types.rs core/src/domain/game_state.rs core/src/tests/game_payment.rs
git commit -m "feat(core): payment validation and option enumeration; drop factorial legacy trading

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---
### Task 9: Game API types, `Game::new`, seating and the first deal

**Files:**
- Modify: `core/src/game/types.rs` (append API types), `core/src/game/mod.rs` (replace), `core/src/domain/game_state.rs` (`seats`, `Card: Copy`)
- Create: `core/src/game/setup.rs`, `core/src/game/test_support.rs`
- Test: `core/src/tests/game_helpers.rs` (create), `core/src/tests/game_setup.rs` (create), `core/src/lib.rs` (test list)

**Interfaces:**
- Consumes:
  - `GameRng`, `generate_deck(n, &mut GameRng)`
  - `WONDERS`, `WONDERS_BY_NAME: HashMap<String, &'static Wonder>`
  - `GameState::new(deck, PlayersWithWonders)`, `GameState::init()`
  - `Age::from_number`
- Produces (public, in `crate::game`):
  - `Game::new(players: Vec<String>, wonders: WonderSelection, seed: u64) -> Result<Game, SetupError>`
  - `Game::phase(&self) -> &Phase`
  - `Game::players(&self) -> &[String]`
  - `enum Side { A, B }`
  - `enum WonderSelection { Random, Explicit(Vec<(String, Side)>) }`
  - `enum Action { Build { card: String, payment: Payment }, BuildWonderStage { card: String, payment: Payment }, Discard { card: String }, BuildFree { card: String }, BuildFromDiscard { card: String } }`
  - `enum ExtraTurnKind { PlayLastCard, BuildFromDiscard }`
  - `enum Phase { ChoosingCards { age: u8, turn: u8 }, ExtraTurn { player: String, kind: ExtraTurnKind }, GameOver { scores: Vec<FinalScore> } }`
  - `struct FinalScore { player: String, military: i32, treasury: i32, wonder: i32, civilian: i32, scientific: i32, commercial: i32, guild: i32, total: i32, coins: u32, rank: u8 }`
  - `enum SetupError { InvalidPlayersNumber(usize), DuplicatePlayer(String), InvalidWonder(String), WondersLengthMismatch { players: usize, wonders: usize }, DuplicateWonder(String) }`
  - `enum ActionError { UnknownPlayer, NotYourTurn, GameOver, CardNotInHand, CardNotInDiscard, AlreadyBuilt, CannotAfford, InvalidPayment, NoWonderStageLeft, FreeBuildUnavailable, ActionNotAllowedNow }`
  - `pub const ENGINE_VERSION: u32 = 1`
- Produces (internal to `game`):
  - `Kind { Build, Wonder, Discard, BuildFree, FromDiscard }`
  - `Resolved { kind, card: Card, bank: u32, west: u32, east: u32 }` with `Resolved::free(kind, card)`
  - `Pending { action: Action, resolved: Resolved }`
  - private helpers `Game::seat_of(&self, &str) -> Option<usize>`, `west_of(seat) -> usize`, `east_of(seat) -> usize`, `player(seat) -> &PlayerState`, `deal_age(&mut self, age: u8)`
- Produces (tests only):
  - `Game::hand_names(&self, player) -> Vec<String>`
  - `wonder_of(&self, player) -> (String, Side)`
  - `coins(&self, player) -> u32`
  - `tests::game_helpers::{names, game_with, plain_game}`

- [ ] **Step 1: Write the failing tests**

Create `core/src/tests/game_helpers.rs`:

```rust
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
    game_with(&[("Gizah", Side::A), ("Rhódos", Side::A), ("Éphesos", Side::A)])
}
```

Create `core/src/tests/game_setup.rs`:

```rust
use super::game_helpers::{game_with, names, plain_game};
use crate::game::{Game, Phase, SetupError, Side, WonderSelection};

fn explicit(wonders: &[(&str, Side)]) -> WonderSelection {
    WonderSelection::Explicit(
        wonders
            .iter()
            .map(|(name, side)| (name.to_string(), *side))
            .collect(),
    )
}

fn deal_fingerprint(game: &Game) -> String {
    game.players()
        .iter()
        .map(|player| {
            let (wonder, side) = game.wonder_of(player);
            format!("{player}:{wonder}/{side:?}:{}", game.hand_names(player).join(","))
        })
        .collect::<Vec<_>>()
        .join(";")
}

#[test]
fn player_count_must_be_three_to_seven() {
    assert_eq!(
        Game::new(names(2), WonderSelection::Random, 1).err(),
        Some(SetupError::InvalidPlayersNumber(2))
    );
    assert_eq!(
        Game::new(names(8), WonderSelection::Random, 1).err(),
        Some(SetupError::InvalidPlayersNumber(8))
    );
    for count in 3..=7 {
        assert!(Game::new(names(count), WonderSelection::Random, 1).is_ok());
    }
}

#[test]
fn player_names_must_be_unique() {
    let players = vec!["a".to_string(), "b".to_string(), "a".to_string()];
    assert_eq!(
        Game::new(players, WonderSelection::Random, 1).err(),
        Some(SetupError::DuplicatePlayer("a".to_string()))
    );
}

#[test]
fn explicit_wonders_are_validated() {
    assert_eq!(
        Game::new(names(3), explicit(&[("Gizah", Side::A)]), 1).err(),
        Some(SetupError::WondersLengthMismatch { players: 3, wonders: 1 })
    );
    assert_eq!(
        Game::new(
            names(3),
            explicit(&[("Gizah", Side::A), ("Rhodos", Side::A), ("Babylon", Side::B)]),
            1
        )
        .err(),
        Some(SetupError::InvalidWonder("Rhodos".to_string()))
    );
    assert_eq!(
        Game::new(
            names(3),
            explicit(&[("Gizah", Side::A), ("Gizah", Side::B), ("Babylon", Side::B)]),
            1
        )
        .err(),
        Some(SetupError::DuplicateWonder("Gizah".to_string()))
    );
}

#[test]
fn explicit_wonders_are_assigned_in_seat_order() {
    let game = game_with(&[("Gizah", Side::A), ("Alexandria", Side::A), ("Babylon", Side::B)]);
    assert_eq!(game.wonder_of("p1"), ("Gizah".to_string(), Side::A));
    assert_eq!(game.wonder_of("p2"), ("Alexandria".to_string(), Side::A));
    assert_eq!(game.wonder_of("p3"), ("Babylon".to_string(), Side::B));
}

#[test]
fn random_wonders_are_distinct() {
    for seed in 0..20 {
        let game = Game::new(names(7), WonderSelection::Random, seed).unwrap();
        let mut wonders: Vec<String> = game.players().iter().map(|p| game.wonder_of(p).0).collect();
        wonders.sort();
        wonders.dedup();
        assert_eq!(wonders.len(), 7);
    }
}

#[test]
fn every_player_starts_with_seven_cards_and_three_coins() {
    let game = plain_game();
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 1 });
    for player in game.players() {
        assert_eq!(game.hand_names(player).len(), 7);
        assert_eq!(game.coins(player), 3);
    }
}

#[test]
fn same_seed_same_deal_and_different_seed_different_deal() {
    let deal = |seed| deal_fingerprint(&Game::new(names(5), WonderSelection::Random, seed).unwrap());
    assert_eq!(deal(99), deal(99));
    assert_ne!(deal(99), deal(100));
}

#[test]
fn extreme_seeds_start_games() {
    for seed in [0, u64::MAX] {
        assert!(Game::new(names(3), WonderSelection::Random, seed).is_ok());
    }
}

/// Guards against accidental changes to RNG consumption order.
/// If this changes intentionally, bump `ENGINE_VERSION` and re-pin.
const PINNED_DEAL_SEED_42: &str = "";

#[test]
fn seed_42_deal_is_pinned() {
    let game = Game::new(names(3), WonderSelection::Random, 42).unwrap();
    assert_eq!(
        deal_fingerprint(&game),
        PINNED_DEAL_SEED_42,
        "RNG consumption order changed; bump ENGINE_VERSION and re-pin if intentional"
    );
}
```

Add `pub mod game_helpers;` and `pub mod game_setup;` to the test list in `core/src/lib.rs`.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test tests::game_setup`
Expected: compile error, `Game`, `Side`, `WonderSelection`… not found in `crate::game`.

- [ ] **Step 3: Make `Card` copyable and record seat order in `GameState`**

In `core/src/domain/game_state.rs`:

1. Change the `Card` derive to `#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize)]`.
2. Add `pub seats: Vec<PName>,` as the first field of `GameState`.
3. In `GameState::new`, set it before `neighbours`:
   ```rust
               seats: players_with_wonders
                   .iter()
                   .map(|(player, _)| player.0.clone())
                   .collect(),
   ```
4. Replace `init` so starting effects run in seat order:
   ```rust
       pub fn init(&mut self) {
           let mut effects: Vec<(&Effect, PName)> = Default::default();
           for name in self.seats.clone() {
               let player_state = self.get_mut_player_state(&name);
               effects.extend(player_state.init().iter().map(|e| (e, name.clone())));
           }
           for (effect, player_name) in effects {
               (*effect)(self, player_name);
           }
       }
   ```
5. In `impl Serialize for GameState`, add `s.serialize_field("seats", &self.seats)?;` and raise the field count by one.

- [ ] **Step 4: Add the API types**

Append to `core/src/game/types.rs`, and change its first `use` line to `use crate::domain::{Card, ResourceType};`:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum Side {
    A,
    B,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WonderSelection {
    Random,
    Explicit(Vec<(String, Side)>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Action {
    Build { card: String, payment: Payment },
    BuildWonderStage { card: String, payment: Payment },
    Discard { card: String },
    /// Olympía A: once per age.
    BuildFree { card: String },
    /// Halikarnassós extra turn only.
    BuildFromDiscard { card: String },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ExtraTurnKind {
    PlayLastCard,
    BuildFromDiscard,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FinalScore {
    pub player: String,
    pub military: i32,
    pub treasury: i32,
    pub wonder: i32,
    pub civilian: i32,
    pub scientific: i32,
    pub commercial: i32,
    pub guild: i32,
    pub total: i32,
    pub coins: u32,
    pub rank: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Phase {
    /// Every player chooses a card from their hand.
    ChoosingCards { age: u8, turn: u8 },
    /// Only `player` acts.
    ExtraTurn { player: String, kind: ExtraTurnKind },
    GameOver { scores: Vec<FinalScore> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SetupError {
    InvalidPlayersNumber(usize),
    DuplicatePlayer(String),
    InvalidWonder(String),
    WondersLengthMismatch { players: usize, wonders: usize },
    DuplicateWonder(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ActionError {
    UnknownPlayer,
    NotYourTurn,
    GameOver,
    CardNotInHand,
    CardNotInDiscard,
    AlreadyBuilt,
    CannotAfford,
    InvalidPayment,
    NoWonderStageLeft,
    FreeBuildUnavailable,
    ActionNotAllowedNow,
}

/// What a validated action does at resolution time.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Kind {
    Build,
    Wonder,
    Discard,
    BuildFree,
    FromDiscard,
}

/// A validated action with its coin movements (computed from pre-turn state).
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Resolved {
    pub(crate) kind: Kind,
    pub(crate) card: Card,
    pub(crate) bank: u32,
    pub(crate) west: u32,
    pub(crate) east: u32,
}

impl Resolved {
    pub(crate) fn free(kind: Kind, card: Card) -> Self {
        Self { kind, card, bank: 0, west: 0, east: 0 }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Pending {
    pub(crate) action: Action,
    pub(crate) resolved: Resolved,
}
```

- [ ] **Step 5: Define `Game`**

Replace `core/src/game/mod.rs` with:

```rust
//! Rustler-free game API. A [`Game`] owns the whole table: seats, hands,
//! phase and pending actions. Every public operation is deterministic.
pub(crate) mod payment;
mod setup;
#[cfg(test)]
mod test_support;
mod types;

pub use crate::domain::{Category, ResourceType};
pub use types::{
    Action, ActionError, ExtraTurnKind, FinalScore, Payment, PaymentOption, Phase, SetupError,
    Side, WonderSelection,
};

use crate::domain::{Card, GameState, PlayerState};
use std::collections::VecDeque;
use types::Pending;

/// Bump on any rule change that could alter the replay of a stored game.
pub const ENGINE_VERSION: u32 = 1;

pub struct Game {
    state: GameState,
    /// Player ids in seat order.
    seats: Vec<String>,
    /// Wonder name and side per seat.
    wonders: Vec<(String, Side)>,
    /// Cards in hand per seat.
    hands: Vec<Vec<Card>>,
    /// Structures built per seat, in build order (for the view and Olympía B).
    built: Vec<Vec<Card>>,
    age: u8,
    turn: u8,
    phase: Phase,
    /// The current choice per seat; resubmitting replaces it.
    pending: Vec<Option<Pending>>,
    /// Extra turns still to be played before the game moves on.
    extra_turns: VecDeque<(usize, ExtraTurnKind)>,
}

impl Game {
    pub fn new(players: Vec<String>, wonders: WonderSelection, seed: u64) -> Result<Game, SetupError> {
        setup::new_game(players, wonders, seed)
    }

    pub fn phase(&self) -> &Phase {
        &self.phase
    }

    /// Player ids in seat order.
    pub fn players(&self) -> &[String] {
        &self.seats
    }

    fn seat_of(&self, player: &str) -> Option<usize> {
        self.seats.iter().position(|seat| seat == player)
    }

    fn west_of(&self, seat: usize) -> usize {
        (seat + self.seats.len() - 1) % self.seats.len()
    }

    fn east_of(&self, seat: usize) -> usize {
        (seat + 1) % self.seats.len()
    }

    fn player(&self, seat: usize) -> &PlayerState {
        self.state.get_player_state(&self.seats[seat])
    }
}
```

- [ ] **Step 6: Implement setup and dealing**

Create `core/src/game/setup.rs`:

```rust
//! `Game::new`: validation, wonder assignment, deck generation and the first deal.
//!
//! RNG consumption order (replay contract; changing it requires bumping
//! `ENGINE_VERSION`):
//! 1. `WonderSelection::Random` only: shuffle the 7 wonders (in `WONDERS`
//!    order), keep the first `n`, then draw one side per seat in seat order.
//! 2. `generate_deck`: Age I shuffle, Age II shuffle, guild shuffle, Age III shuffle.
use super::types::{Phase, SetupError, Side, WonderSelection};
use super::Game;
use crate::domain::{Age, Effect, GameState, Player, PlayersWithWonders, Wonder};
use crate::engine::data::{WONDERS, WONDERS_BY_NAME};
use crate::engine::deck::generate_deck;
use crate::engine::rng::GameRng;
use std::collections::{BTreeSet, VecDeque};

type WonderRef = &'static Wonder<'static, Effect>;

pub(super) fn new_game(
    players: Vec<String>,
    wonders: WonderSelection,
    seed: u64,
) -> Result<Game, SetupError> {
    let count = players.len();
    if !(3..=7).contains(&count) {
        return Err(SetupError::InvalidPlayersNumber(count));
    }
    let mut seen = BTreeSet::new();
    for player in &players {
        if !seen.insert(player.as_str()) {
            return Err(SetupError::DuplicatePlayer(player.clone()));
        }
    }
    let mut rng = GameRng::new(seed);
    let assignment = assign_wonders(count, wonders, &mut rng)?;
    let deck = generate_deck(count, &mut rng);
    let players_with_wonders: PlayersWithWonders = players
        .iter()
        .zip(&assignment)
        .map(|(player, &(wonder, side))| {
            let wonder_side = match side {
                Side::A => &wonder.1,
                Side::B => &wonder.2,
            };
            (Player(player.clone()), wonder_side)
        })
        .collect();
    let mut state = GameState::new(deck, players_with_wonders);
    state.init();
    let mut game = Game {
        state,
        wonders: assignment
            .iter()
            .map(|(wonder, side)| (wonder.0.to_string(), *side))
            .collect(),
        hands: vec![Vec::new(); count],
        built: vec![Vec::new(); count],
        age: 1,
        turn: 1,
        phase: Phase::ChoosingCards { age: 1, turn: 1 },
        pending: vec![None; count],
        extra_turns: VecDeque::new(),
        seats: players,
    };
    game.deal_age(1);
    Ok(game)
}

fn assign_wonders(
    count: usize,
    selection: WonderSelection,
    rng: &mut GameRng,
) -> Result<Vec<(WonderRef, Side)>, SetupError> {
    match selection {
        WonderSelection::Random => {
            let mut pool: Vec<WonderRef> = WONDERS.iter().collect();
            rng.shuffle(&mut pool);
            pool.truncate(count);
            Ok(pool
                .into_iter()
                .map(|wonder| {
                    let side = if rng.below(2) == 0 { Side::A } else { Side::B };
                    (wonder, side)
                })
                .collect())
        }
        WonderSelection::Explicit(choices) => {
            if choices.len() != count {
                return Err(SetupError::WondersLengthMismatch {
                    players: count,
                    wonders: choices.len(),
                });
            }
            let mut used = BTreeSet::new();
            let mut assignment = Vec::with_capacity(count);
            for (name, side) in choices {
                let wonder: WonderRef = WONDERS_BY_NAME
                    .get(&name)
                    .copied()
                    .ok_or_else(|| SetupError::InvalidWonder(name.clone()))?;
                if !used.insert(name.clone()) {
                    return Err(SetupError::DuplicateWonder(name));
                }
                assignment.push((wonder, side));
            }
            Ok(assignment)
        }
    }
}

impl Game {
    /// Deals 7 cards per seat (in seat order) from the age's shuffled deck.
    pub(super) fn deal_age(&mut self, age: u8) {
        self.age = age;
        self.turn = 1;
        self.state.current_age = Age::from_number(age);
        let cards = match age {
            1 => std::mem::take(&mut self.state.deck.0),
            2 => std::mem::take(&mut self.state.deck.1),
            _ => std::mem::take(&mut self.state.deck.2),
        };
        for (hand, chunk) in self.hands.iter_mut().zip(cards.chunks(7)) {
            *hand = chunk.to_vec();
        }
        self.phase = Phase::ChoosingCards { age, turn: 1 };
    }
}
```

Create `core/src/game/test_support.rs`:

```rust
//! `#[cfg(test)]` hooks to rig and inspect a [`Game`] from `crate::tests`.
use super::types::Side;
use super::Game;

impl Game {
    fn seat(&self, player: &str) -> usize {
        self.seat_of(player)
            .unwrap_or_else(|| panic!("unknown player {player}"))
    }

    pub fn hand_names(&self, player: &str) -> Vec<String> {
        self.hands[self.seat(player)]
            .iter()
            .map(|card| card.0.name().to_string())
            .collect()
    }

    pub fn wonder_of(&self, player: &str) -> (String, Side) {
        self.wonders[self.seat(player)].clone()
    }

    pub fn coins(&self, player: &str) -> u32 {
        self.player(self.seat(player)).coins
    }
}
```

- [ ] **Step 7: Run the tests; all pass except the pinned deal**

Run: `cargo test tests::game_setup`
Expected: every test PASSES except `seed_42_deal_is_pinned`, which FAILS with `left: "p1:…/…:…;p2:…;p3:…"`, `right: ""`.

- [ ] **Step 8: Pin the deal**

Copy the exact `left` string from the failure output into `PINNED_DEAL_SEED_42`, and add a comment above the constant recording today's date. Then run `cargo test tests::game_setup` again.
Expected: PASS (9 tests).

- [ ] **Step 9: Run everything**

Run: `cargo test`
Expected: PASS. `dead_code` warnings for `built`, `pending`, `extra_turns`, `west_of`, `east_of` are expected until later tasks.

- [ ] **Step 10: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/game/mod.rs core/src/game/types.rs core/src/game/setup.rs core/src/game/test_support.rs core/src/domain/game_state.rs core/src/tests/game_helpers.rs core/src/tests/game_setup.rs core/src/lib.rs
git commit -m "feat(core): Game::new with seeded wonder assignment and first deal

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 10: Legality and `submit` (pending actions, no resolution yet)

**Files:**
- Create: `core/src/game/legality.rs`
- Modify: `core/src/game/mod.rs`, `core/src/game/test_support.rs`, `core/src/domain/structure.rs` (`cost`), `core/src/domain/wonder.rs` (`stages_total`, `WonderStage::cost`), `core/src/domain/game_state.rs` (`get_mut_player_state` visibility)
- Test: `core/src/tests/game_legality.rs` (create), `core/src/tests/game_helpers.rs`, `core/src/lib.rs`

**Interfaces:**
- Consumes: `payment::{Market, check_payment, payment_options}`, `PlayerState.{resources_produced, tradable_resources, coins, wonder, wonder_stages_built, structure_builder}`, `PlayerState::apply_trading`
- Produces (public): `Game::submit(&mut self, player: &str, action: Action) -> Result<(), ActionError>`. It validates the action and stores it as the player's pending action, replacing any earlier one. From Task 11 it also resolves the turn once everyone has submitted.
- Produces (game-internal):
  - `Game::precheck(&self, player) -> Result<usize, ActionError>` (UnknownPlayer → GameOver → NotYourTurn)
  - `Game::check_action(&self, seat, &Action) -> Result<Resolved, ActionError>`
  - `enum Requirement { Free, Coins(u32), Trade { coin_cost: u32, resources: Vec<ResourceCost> } }`
  - `structure_requirement(seat, Card) -> Result<Requirement, ActionError>` (AlreadyBuilt → chain → cost)
  - `wonder_requirement(seat) -> Result<Requirement, ActionError>`
  - `market(seat) -> Market<'_>`
  - `free_build_available(seat) -> bool`
  - `already_built(seat, Card) -> bool`
- Produces (tests only):
  - `set_hand(player, &[&str])`: replaces the first k cards and keeps the hand size
  - `give(player, card)`: records the structure and runs its effects
  - `give_stages(player, count)`
  - `set_coins(player, u32)`
  - `validate(player, &Action) -> Result<(), ActionError>`
  - `pending_action(player) -> Option<Action>`
  - helpers `build`, `build_paying`, `stage`, `discard`
- Produces (domain): `Structure::cost(&self) -> &Cost<'a>`, `WonderSide::stages_total(&self) -> usize`, `WonderStage::cost(&self) -> ResourceCosts<'a>`.

- [ ] **Step 1: Write the failing tests**

Append to `core/src/tests/game_helpers.rs` (and change its `use` line to `use crate::game::{Action, Game, Payment, ResourceType, Side, WonderSelection};`):

```rust
pub fn build(card: &str) -> Action {
    Action::Build { card: card.to_string(), payment: Payment::default() }
}

pub fn build_paying(card: &str, west: &[(ResourceType, u8)], east: &[(ResourceType, u8)]) -> Action {
    Action::Build {
        card: card.to_string(),
        payment: Payment { west: west.to_vec(), east: east.to_vec() },
    }
}

pub fn stage(card: &str) -> Action {
    Action::BuildWonderStage { card: card.to_string(), payment: Payment::default() }
}

pub fn discard(card: &str) -> Action {
    Action::Discard { card: card.to_string() }
}
```

Create `core/src/tests/game_legality.rs`:

```rust
use super::game_helpers::{build, build_paying, discard, plain_game, stage};
use crate::game::{Action, ActionError, ResourceType::*};

#[test]
fn unknown_player_is_rejected() {
    let mut game = plain_game();
    assert_eq!(game.submit("zed", discard("Altar")), Err(ActionError::UnknownPlayer));
}

#[test]
fn card_must_be_in_hand_for_every_hand_action() {
    let mut game = plain_game();
    game.set_hand("p1", &["Altar"]);
    for action in [build("Palace"), stage("Palace"), discard("Palace")] {
        assert_eq!(game.validate("p1", &action), Err(ActionError::CardNotInHand));
    }
}

#[test]
fn discarding_a_card_in_hand_is_legal() {
    let mut game = plain_game();
    game.set_hand("p1", &["Altar"]);
    assert_eq!(game.submit("p1", discard("Altar")), Ok(()));
}

#[test]
fn a_free_card_needs_an_empty_payment() {
    let mut game = plain_game();
    game.set_hand("p1", &["Lumber Yard"]);
    assert_eq!(game.validate("p1", &build("Lumber Yard")), Ok(()));
    assert_eq!(
        game.validate("p1", &build_paying("Lumber Yard", &[], &[(Ore, 1)])),
        Err(ActionError::InvalidPayment)
    );
}

#[test]
fn a_coin_cost_must_be_affordable() {
    let mut game = plain_game();
    game.set_hand("p1", &["Tree Farm"]);
    assert_eq!(game.validate("p1", &build("Tree Farm")), Ok(()));
    game.set_coins("p1", 0);
    assert_eq!(game.validate("p1", &build("Tree Farm")), Err(ActionError::CannotAfford));
}

#[test]
fn own_production_covers_a_resource_cost() {
    // p1 is Gizah A and produces 1 Stone; Baths costs 1 Stone.
    let mut game = plain_game();
    game.set_hand("p1", &["Baths"]);
    assert_eq!(game.validate("p1", &build("Baths")), Ok(()));
}

#[test]
fn a_resource_nobody_sells_cannot_be_afforded() {
    // Stockade costs Wood; Rhódos (Ore) and Éphesos (Papyrus) do not produce it.
    let mut game = plain_game();
    game.set_hand("p1", &["Stockade"]);
    assert_eq!(game.validate("p1", &build("Stockade")), Err(ActionError::CannotAfford));
}

#[test]
fn purchases_must_come_from_the_neighbour_that_sells() {
    // p1's east neighbour is p2 (Rhódos A); its starting Ore is tradable.
    let mut game = plain_game();
    game.set_hand("p1", &["Barracks"]);
    assert_eq!(game.validate("p1", &build_paying("Barracks", &[], &[(Ore, 1)])), Ok(()));
    assert_eq!(
        game.validate("p1", &build_paying("Barracks", &[(Ore, 1)], &[])),
        Err(ActionError::InvalidPayment)
    );
    assert_eq!(game.validate("p1", &build("Barracks")), Err(ActionError::InvalidPayment));
    game.set_coins("p1", 1);
    assert_eq!(
        game.validate("p1", &build_paying("Barracks", &[], &[(Ore, 1)])),
        Err(ActionError::CannotAfford)
    );
}

#[test]
fn chain_builds_are_free() {
    let mut game = plain_game();
    game.give("p1", "Altar");
    game.set_hand("p1", &["Temple"]);
    assert_eq!(game.validate("p1", &build("Temple")), Ok(()));
    assert_eq!(
        game.validate("p1", &build_paying("Temple", &[], &[(Ore, 1)])),
        Err(ActionError::InvalidPayment)
    );
}

#[test]
fn an_identical_structure_cannot_be_built_twice() {
    let mut game = plain_game();
    game.give("p1", "Altar");
    game.set_hand("p1", &["Altar"]);
    assert_eq!(game.validate("p1", &build("Altar")), Err(ActionError::AlreadyBuilt));
    assert_eq!(game.validate("p1", &discard("Altar")), Ok(()));
}

#[test]
fn wonder_stage_cost_and_limit_are_checked() {
    // Gizah A stage 1 costs 2 Stone; p1 produces 1 and nobody sells Stone.
    let mut game = plain_game();
    game.set_hand("p1", &["Altar"]);
    assert_eq!(game.validate("p1", &stage("Altar")), Err(ActionError::CannotAfford));
    game.give("p1", "Stone Pit");
    assert_eq!(game.validate("p1", &stage("Altar")), Ok(()));
    game.give_stages("p1", 3);
    assert_eq!(game.validate("p1", &stage("Altar")), Err(ActionError::NoWonderStageLeft));
}

#[test]
fn free_build_requires_the_olympia_ability() {
    let mut game = plain_game();
    game.set_hand("p1", &["Palace"]);
    assert_eq!(
        game.validate("p1", &Action::BuildFree { card: "Palace".to_string() }),
        Err(ActionError::FreeBuildUnavailable)
    );
}

#[test]
fn build_from_discard_is_not_allowed_outside_its_extra_turn() {
    let game = plain_game();
    assert_eq!(
        game.validate("p1", &Action::BuildFromDiscard { card: "Palace".to_string() }),
        Err(ActionError::ActionNotAllowedNow)
    );
}

#[test]
fn resubmitting_replaces_the_pending_action_but_invalid_ones_do_not() {
    let mut game = plain_game();
    game.set_hand("p1", &["Altar", "Lumber Yard", "Stockade"]);
    game.submit("p1", discard("Altar")).unwrap();
    game.submit("p1", build("Lumber Yard")).unwrap();
    assert_eq!(game.pending_action("p1"), Some(build("Lumber Yard")));
    assert_eq!(game.submit("p1", build("Stockade")), Err(ActionError::CannotAfford));
    assert_eq!(game.pending_action("p1"), Some(build("Lumber Yard")));
}
```

Add `pub mod game_legality;` to the test list.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test tests::game_legality`
Expected: compile error, no method `submit` / `validate` / `set_hand` on `Game`.

- [ ] **Step 3: Domain accessors**

In `core/src/domain/structure.rs`, inside `impl<'a, T> Structure<'a, T>`:

```rust
    pub fn cost(&self) -> &Cost<'a> {
        &self.6
    }
```

In `core/src/domain/wonder.rs`, add to `impl<'a, T> WonderSide<'a, T>`:

```rust
    pub fn stages_total(&self) -> usize {
        self.2.len()
    }
```

and to `impl<'a, T> WonderStage<'a, T>`:

```rust
    pub fn cost(&self) -> ResourceCosts<'a> {
        self.0
    }
```

In `core/src/domain/game_state.rs`, change `fn get_mut_player_state` to `pub(crate) fn get_mut_player_state`.

- [ ] **Step 4: Implement legality**

Create `core/src/game/legality.rs`:

```rust
//! The single source of truth for what a player may do right now. `submit`
//! validates with it; the view builds its options with it (Task 16).
use super::payment::{check_payment, payment_options, Market};
use super::types::{Action, ActionError, ExtraTurnKind, Kind, Payment, Phase, Resolved};
use super::Game;
use crate::domain::{Card, ResourceCost, ALL_RESOURCE_TYPES};

/// What it takes to build something, before looking at a payment.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Requirement {
    Free,
    Coins(u32),
    Trade { coin_cost: u32, resources: Vec<ResourceCost> },
}

impl Game {
    pub(super) fn check_action(&self, seat: usize, action: &Action) -> Result<Resolved, ActionError> {
        let extra_turn = match &self.phase {
            Phase::GameOver { .. } => return Err(ActionError::GameOver),
            Phase::ChoosingCards { .. } => None,
            Phase::ExtraTurn { kind, .. } => Some(*kind),
        };
        let hand_actions_allowed = extra_turn != Some(ExtraTurnKind::BuildFromDiscard);
        match action {
            Action::Build { card, payment } => {
                if !hand_actions_allowed {
                    return Err(ActionError::ActionNotAllowedNow);
                }
                let card = self.find_in_hand(seat, card)?;
                let requirement = self.structure_requirement(seat, card)?;
                let (bank, west, east) = self.settle(seat, &requirement, payment)?;
                Ok(Resolved { kind: Kind::Build, card, bank, west, east })
            }
            Action::BuildWonderStage { card, payment } => {
                if !hand_actions_allowed {
                    return Err(ActionError::ActionNotAllowedNow);
                }
                let card = self.find_in_hand(seat, card)?;
                let requirement = self.wonder_requirement(seat)?;
                let (bank, west, east) = self.settle(seat, &requirement, payment)?;
                Ok(Resolved { kind: Kind::Wonder, card, bank, west, east })
            }
            Action::Discard { card } => {
                if !hand_actions_allowed {
                    return Err(ActionError::ActionNotAllowedNow);
                }
                let card = self.find_in_hand(seat, card)?;
                Ok(Resolved::free(Kind::Discard, card))
            }
            Action::BuildFree { card } => {
                if extra_turn.is_some() {
                    return Err(ActionError::ActionNotAllowedNow);
                }
                if !self.free_build_available(seat) {
                    return Err(ActionError::FreeBuildUnavailable);
                }
                let card = self.find_in_hand(seat, card)?;
                if self.already_built(seat, card) {
                    return Err(ActionError::AlreadyBuilt);
                }
                Ok(Resolved::free(Kind::BuildFree, card))
            }
            Action::BuildFromDiscard { card } => {
                if extra_turn != Some(ExtraTurnKind::BuildFromDiscard) {
                    return Err(ActionError::ActionNotAllowedNow);
                }
                let card = self
                    .state
                    .cards_discarded
                    .iter()
                    .find(|discarded| discarded.0.name() == card.as_str())
                    .copied()
                    .ok_or(ActionError::CardNotInDiscard)?;
                if self.already_built(seat, card) {
                    return Err(ActionError::AlreadyBuilt);
                }
                Ok(Resolved::free(Kind::FromDiscard, card))
            }
        }
    }

    fn find_in_hand(&self, seat: usize, name: &str) -> Result<Card, ActionError> {
        self.hands[seat]
            .iter()
            .find(|card| card.0.name() == name)
            .copied()
            .ok_or(ActionError::CardNotInHand)
    }

    pub(super) fn already_built(&self, seat: usize, card: Card) -> bool {
        self.player(seat).structure_builder.already_built(card.0)
    }

    /// Olympía A: the ability is active and unused in the current age.
    pub(super) fn free_build_available(&self, seat: usize) -> bool {
        self.player(seat)
            .structure_builder
            .ages_can_build_free_in
            .contains(&self.state.current_age)
    }

    pub(super) fn structure_requirement(&self, seat: usize, card: Card) -> Result<Requirement, ActionError> {
        let builder = &self.player(seat).structure_builder;
        if builder.already_built(card.0) {
            return Err(ActionError::AlreadyBuilt);
        }
        if builder.can_build_structure_from_dependencies(card.0) {
            return Ok(Requirement::Free);
        }
        let (coin_cost, resources) = *card.0.cost();
        self.requirement(seat, coin_cost, resources)
    }

    pub(super) fn wonder_requirement(&self, seat: usize) -> Result<Requirement, ActionError> {
        let player = self.player(seat);
        let built = usize::from(player.wonder_stages_built);
        if built >= player.wonder.stages_total() {
            return Err(ActionError::NoWonderStageLeft);
        }
        self.requirement(seat, 0, player.wonder.wonder_stage_with_idx(built + 1).cost())
    }

    fn requirement(&self, seat: usize, coin_cost: u32, resources: &[ResourceCost]) -> Result<Requirement, ActionError> {
        let player = self.player(seat);
        if resources.is_empty() || player.resources_produced.can_produce(resources) {
            if coin_cost == 0 {
                Ok(Requirement::Free)
            } else if player.coins >= coin_cost {
                Ok(Requirement::Coins(coin_cost))
            } else {
                Err(ActionError::CannotAfford)
            }
        } else {
            Ok(Requirement::Trade { coin_cost, resources: resources.to_vec() })
        }
    }

    /// The buyer's view of the market: own production, neighbours' tradable
    /// production, unit prices from the buyer's trade actions, pre-turn coins.
    pub(super) fn market(&self, seat: usize) -> Market<'_> {
        let buyer = self.player(seat);
        let west = self.player(self.west_of(seat));
        let east = self.player(self.east_of(seat));
        Market {
            own: &buyer.resources_produced,
            west: &west.tradable_resources,
            east: &east.tradable_resources,
            west_prices: ALL_RESOURCE_TYPES.map(|r| buyer.apply_trading(west.player.name(), &r)),
            east_prices: ALL_RESOURCE_TYPES.map(|r| buyer.apply_trading(east.player.name(), &r)),
            coins: buyer.coins,
        }
    }

    /// Checks `payment` against `requirement`; returns (bank, west, east) coins.
    fn settle(&self, seat: usize, requirement: &Requirement, payment: &Payment) -> Result<(u32, u32, u32), ActionError> {
        match requirement {
            Requirement::Free if payment.is_empty() => Ok((0, 0, 0)),
            Requirement::Coins(coins) if payment.is_empty() => Ok((*coins, 0, 0)),
            Requirement::Free | Requirement::Coins(_) => Err(ActionError::InvalidPayment),
            Requirement::Trade { coin_cost, resources } => {
                let market = self.market(seat);
                match check_payment(&market, *coin_cost, resources, payment) {
                    Some((west, east)) => Ok((*coin_cost, west, east)),
                    None if payment_options(&market, *coin_cost, resources).is_empty() => {
                        Err(ActionError::CannotAfford)
                    }
                    None => Err(ActionError::InvalidPayment),
                }
            }
        }
    }
}
```

- [ ] **Step 5: Add `submit`**

In `core/src/game/mod.rs`, add `mod legality;` to the module list. `Pending`, `Action`, `ActionError` and `Phase` are already in scope. Then add to `impl Game`:

```rust
    /// Validates `action` and stores it as `player`'s pending choice
    /// (replacing any earlier one).
    pub fn submit(&mut self, player: &str, action: Action) -> Result<(), ActionError> {
        let seat = self.precheck(player)?;
        let resolved = self.check_action(seat, &action)?;
        self.pending[seat] = Some(Pending { action, resolved });
        Ok(())
    }

    /// The seat of `player`, if they may act in the current phase.
    fn precheck(&self, player: &str) -> Result<usize, ActionError> {
        let seat = self.seat_of(player).ok_or(ActionError::UnknownPlayer)?;
        match &self.phase {
            Phase::GameOver { .. } => Err(ActionError::GameOver),
            Phase::ExtraTurn { player: acting, .. } if acting != player => {
                Err(ActionError::NotYourTurn)
            }
            _ => Ok(seat),
        }
    }
```

- [ ] **Step 6: Add the test hooks**

In `core/src/game/test_support.rs`, change the imports to:

```rust
use super::types::{Action, ActionError, Side};
use super::Game;
use crate::domain::{Card, PlayerDecision};
use crate::engine::data::STRUCTURES_BY_NAME;

fn card(name: &str) -> Card {
    Card(
        STRUCTURES_BY_NAME
            .get(name)
            .copied()
            .unwrap_or_else(|| panic!("unknown card {name}")),
    )
}
```

and add to the `impl Game` block:

```rust
    /// Replaces the first `cards.len()` cards of the hand, keeping its size.
    pub fn set_hand(&mut self, player: &str, cards: &[&str]) {
        let seat = self.seat(player);
        assert!(cards.len() <= self.hands[seat].len(), "hand of {player} is too small");
        for (slot, name) in self.hands[seat].iter_mut().zip(cards) {
            *slot = card(name);
        }
    }

    /// Records `name` as built by `player` (no cost) and runs its effects.
    pub fn give(&mut self, player: &str, name: &str) {
        let seat = self.seat(player);
        let built = card(name);
        self.built[seat].push(built);
        self.state.apply_player_decisions(vec![(
            player.to_string(),
            PlayerDecision::BuildStructure(built),
        )]);
        self.state.events.clear();
    }

    /// Builds the next `count` wonder stages for free and runs their effects.
    pub fn give_stages(&mut self, player: &str, count: usize) {
        for _ in 0..count {
            self.state.apply_player_decisions(vec![(
                player.to_string(),
                PlayerDecision::BuildNextWonderStage(card("Altar")),
            )]);
        }
        self.state.events.clear();
    }

    pub fn set_coins(&mut self, player: &str, coins: u32) {
        self.state.get_mut_player_state(&player.to_string()).coins = coins;
    }

    /// The same checks as `submit`, without storing anything.
    pub fn validate(&self, player: &str, action: &Action) -> Result<(), ActionError> {
        let seat = self.precheck(player)?;
        self.check_action(seat, action).map(|_| ())
    }

    pub fn pending_action(&self, player: &str) -> Option<Action> {
        self.pending[self.seat(player)]
            .as_ref()
            .map(|pending| pending.action.clone())
    }
```

- [ ] **Step 7: Run the tests to verify they pass**

Run: `cargo test tests::game_legality`
Expected: PASS (14 tests). Then run `cargo test`. Expected: PASS.

- [ ] **Step 8: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/game/legality.rs core/src/game/mod.rs core/src/game/test_support.rs core/src/domain/structure.rs core/src/domain/wonder.rs core/src/domain/game_state.rs core/src/tests/game_legality.rs core/src/tests/game_helpers.rs core/src/lib.rs
git commit -m "feat(core): action legality and pending submissions

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 11: Turn resolution: payments, trade credit timing, effects, hand passing

**Files:**
- Create: `core/src/game/resolve.rs`
- Modify: `core/src/game/mod.rs` (`mod resolve;`, `submit` resolves), `core/src/game/test_support.rs`
- Test: `core/src/tests/game_resolution.rs` (create), `core/src/tests/game_helpers.rs`, `core/src/lib.rs`

**Interfaces:**
- Consumes: `Pending`, `Resolved`, `Kind`, `GameState::apply_player_decisions`, `EventType::ConstructFromDiscarded`
- Produces (game-internal):
  - `all_required_submitted(&self) -> bool`
  - `resolve(&mut self)`
  - `take_pending(&mut self) -> Vec<(usize, Resolved)>`
  - `apply_batch(&mut self, Vec<(usize, Resolved)>) -> Vec<usize>`, returning the seats whose Halikarnassós stage was just built
  - `advance(&mut self)`
  - `pass_hands(&mut self)`
- Resolution order (fixed):
  1. Take the cards out of hands (or out of the discard pile).
  2. Debit bank and trade coins.
  3. Record structures and stages, then run effects (`apply_player_decisions`, seat order; Discard gives +3 and moves the card to the pile).
  4. Credit trade coins to the neighbours.
  5. Drain events.
- Produces (tests only):
  - `discard_names() -> Vec<String>`
  - `built_names(player) -> Vec<String>`
  - `stages_built(player) -> u8`
  - `shields(player) -> u32`
  - `points(player) -> HashMap<PointCategory, Point>`
  - helpers `PLAYERS`, `discard_first`, `discard_turn`

- [ ] **Step 1: Write the failing tests**

Append to `core/src/tests/game_helpers.rs`:

```rust
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
```

Create `core/src/tests/game_resolution.rs`:

```rust
use super::game_helpers::{
    build, build_paying, discard, discard_first, discard_turn, plain_game, stage, PLAYERS,
};
use crate::domain::PointCategory::WonderP;
use crate::game::{ActionError, Phase, ResourceType::*};

#[test]
fn a_turn_resolves_only_when_everyone_has_submitted() {
    let mut game = plain_game();
    discard_first(&mut game, "p1");
    discard_first(&mut game, "p2");
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 1 });
    discard_first(&mut game, "p3");
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 2 });
}

#[test]
fn discarding_pays_three_coins_and_fills_the_discard_pile() {
    let mut game = plain_game();
    discard_turn(&mut game);
    for player in PLAYERS {
        assert_eq!(game.coins(player), 6);
        assert_eq!(game.hand_names(player).len(), 6);
    }
    assert_eq!(game.discard_names().len(), 3);
}

#[test]
fn building_pays_the_coin_cost_to_the_bank() {
    let mut game = plain_game();
    game.set_hand("p1", &["Tree Farm"]);
    game.submit("p1", build("Tree Farm")).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    assert_eq!(game.coins("p1"), 2);
    assert_eq!(game.built_names("p1"), vec!["Tree Farm".to_string()]);
}

#[test]
fn trade_coins_go_to_the_neighbour() {
    let mut game = plain_game();
    game.set_hand("p1", &["Barracks"]);
    game.set_hand("p2", &["Lumber Yard"]);
    game.submit("p1", build_paying("Barracks", &[], &[(Ore, 1)])).unwrap();
    game.submit("p2", build("Lumber Yard")).unwrap();
    discard_first(&mut game, "p3");
    assert_eq!(game.coins("p1"), 1);
    assert_eq!(game.coins("p2"), 5);
    assert_eq!(game.coins("p3"), 6);
    assert_eq!(game.shields("p1"), 1);
}

#[test]
fn coins_received_this_turn_cannot_be_spent_this_turn() {
    // p2 must buy Papyrus from p3 (its east neighbour) but has 0 coins before
    // the turn, even though p1 pays p2 two coins in this same turn.
    let mut game = plain_game();
    game.set_coins("p2", 0);
    game.set_hand("p1", &["Barracks"]);
    game.set_hand("p2", &["Scriptorium"]);
    game.submit("p1", build_paying("Barracks", &[], &[(Ore, 1)])).unwrap();
    assert_eq!(
        game.submit("p2", build_paying("Scriptorium", &[], &[(Papyrus, 1)])),
        Err(ActionError::CannotAfford)
    );
    game.submit("p2", discard("Scriptorium")).unwrap();
    discard_first(&mut game, "p3");
    assert_eq!(game.coins("p2"), 5); // 0 + 3 (discard) + 2 (paid by p1)
}

#[test]
fn structure_effects_apply_on_resolution() {
    let mut game = plain_game();
    game.set_hand("p1", &["Tavern"]);
    game.submit("p1", build("Tavern")).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    assert_eq!(game.coins("p1"), 8);
}

#[test]
fn a_wonder_stage_tucks_the_card() {
    let mut game = plain_game();
    game.give("p1", "Stone Pit");
    game.set_hand("p1", &["Altar"]);
    game.submit("p1", stage("Altar")).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    assert_eq!(game.stages_built("p1"), 1);
    assert!(!game.built_names("p1").contains(&"Altar".to_string()));
    assert_eq!(game.points("p1").get(&WonderP), Some(&3));
}

#[test]
fn hands_pass_west_in_age_one() {
    let mut game = plain_game();
    let before: Vec<Vec<String>> = PLAYERS.iter().map(|p| game.hand_names(p)).collect();
    discard_turn(&mut game);
    // Seat i passes to seat i-1, so p1 receives p2's hand, p2 gets p3's, p3 gets p1's.
    assert_eq!(game.hand_names("p1"), before[1][1..].to_vec());
    assert_eq!(game.hand_names("p2"), before[2][1..].to_vec());
    assert_eq!(game.hand_names("p3"), before[0][1..].to_vec());
}
```

Add `pub mod game_resolution;` to the test list.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test tests::game_resolution`
Expected: compile error (`discard_names`, `shields`… missing). Once the hooks exist, the tests still FAIL because the phase never leaves turn 1.

- [ ] **Step 3: Implement resolution**

Create `core/src/game/resolve.rs`:

```rust
//! Turn resolution: all pending actions are applied at once, then the game
//! moves on (pass hands / extra turns / end of age / game over).
use super::types::{Kind, Phase, Resolved};
use super::Game;
use crate::domain::{Card, EventType, PlayerDecision};

impl Game {
    pub(super) fn all_required_submitted(&self) -> bool {
        match &self.phase {
            Phase::ChoosingCards { .. } => self.pending.iter().all(Option::is_some),
            Phase::ExtraTurn { player, .. } => self
                .seat_of(player)
                .is_some_and(|seat| self.pending[seat].is_some()),
            Phase::GameOver { .. } => false,
        }
    }

    pub(super) fn resolve(&mut self) {
        let batch = self.take_pending();
        // Seats that must now build from the discard pile; wired up in Task 15.
        let _build_from_discard = self.apply_batch(batch);
        self.advance();
    }

    fn take_pending(&mut self) -> Vec<(usize, Resolved)> {
        self.pending
            .iter_mut()
            .enumerate()
            .filter_map(|(seat, pending)| pending.take().map(|p| (seat, p.resolved)))
            .collect()
    }

    /// Applies a batch "simultaneously": coins were validated against
    /// pre-turn balances; trade coins are credited only after every action
    /// (structures, stages, effects) has been applied. Returns the seats that
    /// triggered Halikarnassós' build-from-discard.
    fn apply_batch(&mut self, batch: Vec<(usize, Resolved)>) -> Vec<usize> {
        let mut decisions = Vec::with_capacity(batch.len());
        let mut credits: Vec<(usize, u32)> = Vec::new();
        for (seat, resolved) in batch {
            let card = self.take_card(seat, &resolved);
            if resolved.west > 0 {
                credits.push((self.west_of(seat), resolved.west));
            }
            if resolved.east > 0 {
                credits.push((self.east_of(seat), resolved.east));
            }
            let name = self.seats[seat].clone();
            self.state.get_mut_player_state(&name).coins -=
                resolved.bank + resolved.west + resolved.east;
            let decision = match resolved.kind {
                Kind::Build | Kind::FromDiscard => {
                    self.built[seat].push(card);
                    PlayerDecision::BuildStructure(card)
                }
                Kind::BuildFree => {
                    self.built[seat].push(card);
                    PlayerDecision::ConstructForFreeOncePerAge(card)
                }
                Kind::Wonder => PlayerDecision::BuildNextWonderStage(card),
                Kind::Discard => PlayerDecision::Discard(card),
            };
            decisions.push((name, decision));
        }
        self.state.apply_player_decisions(decisions);
        for (seat, coins) in credits {
            let name = self.seats[seat].clone();
            self.state.get_mut_player_state(&name).coins += coins;
        }
        let events = std::mem::take(&mut self.state.events);
        events
            .into_iter()
            .filter(|(_, event)| *event == EventType::ConstructFromDiscarded)
            .filter_map(|(player, _)| self.seat_of(&player))
            .collect()
    }

    fn take_card(&mut self, seat: usize, resolved: &Resolved) -> Card {
        let name = resolved.card.0.name();
        let pile = if resolved.kind == Kind::FromDiscard {
            &mut self.state.cards_discarded
        } else {
            &mut self.hands[seat]
        };
        let index = pile
            .iter()
            .position(|card| card.0.name() == name)
            .expect("a validated card is still where it was");
        pile.remove(index)
    }

    fn advance(&mut self) {
        self.pass_hands();
        self.turn += 1;
        self.phase = Phase::ChoosingCards { age: self.age, turn: self.turn };
    }

    /// Ages I and III pass to the west neighbour, Age II to the east.
    fn pass_hands(&mut self) {
        let mut passed = vec![Vec::new(); self.seats.len()];
        for (seat, hand) in std::mem::take(&mut self.hands).into_iter().enumerate() {
            let to = if self.age == 2 { self.east_of(seat) } else { self.west_of(seat) };
            passed[to] = hand;
        }
        self.hands = passed;
    }
}
```

In `core/src/game/mod.rs`, add `mod resolve;`. In `submit`, after storing the pending action, add:

```rust
        if self.all_required_submitted() {
            self.resolve();
        }
```

- [ ] **Step 4: Add the test hooks**

In `core/src/game/test_support.rs`, extend the domain import to `use crate::domain::{Card, PlayerDecision, Point, PointCategory};`, add `use std::collections::HashMap;`, and add:

```rust
    pub fn discard_names(&self) -> Vec<String> {
        self.state
            .cards_discarded
            .iter()
            .map(|card| card.0.name().to_string())
            .collect()
    }

    pub fn built_names(&self, player: &str) -> Vec<String> {
        self.built[self.seat(player)]
            .iter()
            .map(|card| card.0.name().to_string())
            .collect()
    }

    pub fn stages_built(&self, player: &str) -> u8 {
        self.player(self.seat(player)).wonder_stages_built
    }

    pub fn shields(&self, player: &str) -> u32 {
        self.player(self.seat(player)).military_symbols
    }

    pub fn points(&self, player: &str) -> HashMap<PointCategory, Point> {
        self.player(self.seat(player)).calculate_points(&self.state)
    }
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test tests::game_resolution`
Expected: PASS (8 tests). Then run `cargo test`. Expected: PASS. The legality tests never submit for all three players, so no resolution happens there.

- [ ] **Step 6: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/game/resolve.rs core/src/game/mod.rs core/src/game/test_support.rs core/src/tests/game_resolution.rs core/src/tests/game_helpers.rs core/src/lib.rs
git commit -m "feat(core): simultaneous turn resolution, trade credits after the turn, hand passing

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 12: Final scoring and tiebreak

**Files:**
- Create: `core/src/game/scoring.rs`
- Modify: `core/src/game/mod.rs` (`pub(crate) mod scoring;`), `core/src/game/test_support.rs`
- Test: `core/src/tests/game_scoring.rs` (create), `core/src/lib.rs`

**Interfaces:**
- Consumes: `PlayerState::calculate_points(&GameState) -> HashMap<PointCategory, Point>`
- Produces:
  - `Game::compute_scores(&self) -> Vec<FinalScore>` (game-internal)
  - `pub(crate) fn rank_scores(scores: &mut [FinalScore])`: rank = 1 + the number of players with a strictly greater `(total, coins)`; the slice ends up sorted by rank with ties in input (seat) order
  - test hook `score_now() -> Vec<FinalScore>`

- [ ] **Step 1: Write the failing tests**

Create `core/src/tests/game_scoring.rs`:

```rust
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
    for card in ["Apothecary", "Workshop", "Scriptorium", "Lighthouse", "Workers Guild"] {
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
```

Add `pub mod game_scoring;` to the test list.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test tests::game_scoring`
Expected: compile error, `could not find scoring in game`.

- [ ] **Step 3: Implement scoring**

Create `core/src/game/scoring.rs`:

```rust
//! Final scoring and ranking.
use super::types::FinalScore;
use super::Game;
use crate::domain::PointCategory;

impl Game {
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
                    total: military + treasury + wonder + civilian + scientific + commercial + guild,
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
```

In `core/src/game/mod.rs`, add `pub(crate) mod scoring;`. In `core/src/game/test_support.rs`, change the types import to `use super::types::{Action, ActionError, FinalScore, Side};` and add:

```rust
    pub fn score_now(&self) -> Vec<FinalScore> {
        self.compute_scores()
    }
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test tests::game_scoring`
Expected: PASS (2 tests).

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/game/scoring.rs core/src/game/mod.rs core/src/game/test_support.rs core/src/tests/game_scoring.rs core/src/lib.rs
git commit -m "feat(core): final score breakdown with coin tiebreak and shared ranks

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---
### Task 13: End of age: 7th-card discard, military battles, next age, game over

**Files:**
- Modify: `core/src/game/resolve.rs`, `core/src/game/scoring.rs` (`finish`), `core/src/game/test_support.rs`
- Test: `core/src/tests/game_ages.rs` (create), `core/src/tests/game_helpers.rs`, `core/src/lib.rs`

**Interfaces:**
- Consumes: `deal_age`, `compute_scores`, `PlayerState.{military_symbols, battle_tokens}`
- Produces (game-internal):
  - `discard_last_cards(&mut self)`
  - `resolve_battles(&mut self)`: Age I/II/III victory +1/+3/+5, −1 per defeat, tokens pushed west battle first, then east
  - `finish(&mut self)`: sets `Phase::GameOver { scores }`
  - `advance` now either ends the age (when all hands are empty) or passes hands
- Produces (tests only): hook `tokens(player) -> Vec<i32>`; helpers `discard_turns(game, n)` and `discard_age(game)`.

- [ ] **Step 1: Write the failing tests**

Append to `core/src/tests/game_helpers.rs`:

```rust
pub fn discard_turns(game: &mut Game, turns: usize) {
    for _ in 0..turns {
        discard_turn(game);
    }
}

pub fn discard_age(game: &mut Game) {
    discard_turns(game, 6);
}
```

Create `core/src/tests/game_ages.rs`:

```rust
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
    assert_eq!(game.submit("p1", discard("Altar")), Err(ActionError::GameOver));
    assert_eq!(game.submit("zed", discard("Altar")), Err(ActionError::UnknownPlayer));
}
```

Add `pub mod game_ages;` to the test list.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test tests::game_ages`
Expected: compile error (no `tokens` hook). Once the hook exists, the tests FAIL: after turn 6 the engine passes a 1-card hand and stays in Age I.

- [ ] **Step 3: Implement the end of an age**

In `core/src/game/resolve.rs`, add `use std::cmp::Ordering;` and replace `resolve` and `advance`, adding the new helpers:

```rust
    pub(super) fn resolve(&mut self) {
        let was_choosing = matches!(self.phase, Phase::ChoosingCards { .. });
        let batch = self.take_pending();
        // Seats that must now build from the discard pile; wired up in Task 15.
        let _build_from_discard = self.apply_batch(batch);
        if was_choosing && self.hands.iter().all(|hand| hand.len() == 1) {
            self.discard_last_cards();
        }
        self.advance();
    }

    /// End of turn 6: every remaining (7th) card goes to the discard pile.
    fn discard_last_cards(&mut self) {
        for hand in &mut self.hands {
            if let Some(card) = hand.pop() {
                self.state.cards_discarded.push(card);
            }
        }
    }

    fn advance(&mut self) {
        if self.hands.iter().all(Vec::is_empty) {
            self.resolve_battles();
            if self.age == 3 {
                self.finish();
            } else {
                self.deal_age(self.age + 1);
            }
        } else {
            self.pass_hands();
            self.turn += 1;
            self.phase = Phase::ChoosingCards { age: self.age, turn: self.turn };
        }
    }

    /// Each player fights both neighbours: more shields → +1/+3/+5 (Age
    /// I/II/III), fewer → −1, equal → nothing. West battle first.
    fn resolve_battles(&mut self) {
        let victory = match self.age {
            1 => 1,
            2 => 3,
            _ => 5,
        };
        let shields: Vec<u32> = (0..self.seats.len())
            .map(|seat| self.player(seat).military_symbols)
            .collect();
        for seat in 0..self.seats.len() {
            let mut tokens = Vec::new();
            for rival in [self.west_of(seat), self.east_of(seat)] {
                match shields[seat].cmp(&shields[rival]) {
                    Ordering::Greater => tokens.push(victory),
                    Ordering::Less => tokens.push(-1),
                    Ordering::Equal => {}
                }
            }
            let name = self.seats[seat].clone();
            self.state
                .get_mut_player_state(&name)
                .battle_tokens
                .extend(tokens);
        }
    }
```

In `core/src/game/scoring.rs`, change the types import to `use super::types::{FinalScore, Phase};` and add to `impl Game`:

```rust
    pub(super) fn finish(&mut self) {
        let scores = self.compute_scores();
        self.phase = Phase::GameOver { scores };
    }
```

In `core/src/game/test_support.rs` add:

```rust
    pub fn tokens(&self, player: &str) -> Vec<i32> {
        self.player(self.seat(player)).battle_tokens.clone()
    }
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test tests::game_ages`
Expected: PASS (6 tests). Then run `cargo test`. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/game/resolve.rs core/src/game/scoring.rs core/src/game/test_support.rs core/src/tests/game_ages.rs core/src/tests/game_helpers.rs core/src/lib.rs
git commit -m "feat(core): age end with 7th-card discard, battles, next deal and game over

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 14: Olympía abilities: free build once per age (A) and copy a neighbouring guild (B)

**Files:**
- Modify: `core/src/game/scoring.rs`, `core/src/game/test_support.rs`
- Test: `core/src/tests/game_olympia.rs` (create), `core/src/lib.rs`

**Interfaces:**
- Consumes:
  - `free_build_available`, and `Kind::BuildFree`, which is applied through `PlayerDecision::ConstructForFreeOncePerAge`; that marks the current age as used (already implemented in Tasks 10–11)
  - `PlayerState.can_copy_guild`, `point_actions`, `scientific_symbols_produced.any_symbols`, `Structure::effects()`
- Produces (game-internal):
  - `best_guild_to_copy(&mut self, seat) -> Option<&'static Structure<'static, Effect>>`: highest total score, ties → alphabetical, candidates restricted to guilds that neighbours built and the player lacks
  - `apply_copy_guild(&mut self)`, called by `finish` before scoring
- Produces (tests only): hooks `chosen_guild(player) -> Option<String>` and `finish_now()`.

- [ ] **Step 1: Write the failing tests**

Create `core/src/tests/game_olympia.rs`:

```rust
use super::game_helpers::{discard_first, discard_turns, game_with};
use crate::game::{Action, ActionError, Game, Phase, Side};

fn olympia(side: Side) -> Game {
    game_with(&[("Olympía", side), ("Rhódos", Side::A), ("Éphesos", Side::A)])
}

fn build_free(card: &str) -> Action {
    Action::BuildFree { card: card.to_string() }
}

fn final_guild_and_science(game: &Game, player: &str) -> (i32, i32) {
    let Phase::GameOver { scores } = game.phase() else {
        panic!("expected game over");
    };
    let score = scores.iter().find(|s| s.player == player).unwrap();
    (score.guild, score.scientific)
}

#[test]
fn olympia_a_builds_one_card_free_per_age() {
    let mut game = olympia(Side::A);
    game.set_hand("p1", &["Palace"]);
    assert_eq!(game.validate("p1", &build_free("Palace")), Err(ActionError::FreeBuildUnavailable));
    game.give_stages("p1", 2);
    game.submit("p1", build_free("Palace")).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    assert_eq!(game.built_names("p1"), vec!["Palace".to_string()]);
    assert_eq!(game.coins("p1"), 3);
    let next = game.hand_names("p1")[0].clone();
    assert_eq!(game.validate("p1", &build_free(&next)), Err(ActionError::FreeBuildUnavailable));
    discard_turns(&mut game, 5);
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 2, turn: 1 });
    let first = game.hand_names("p1")[0].clone();
    assert_eq!(game.validate("p1", &build_free(&first)), Ok(()));
}

#[test]
fn olympia_a_cannot_free_build_a_duplicate() {
    let mut game = olympia(Side::A);
    game.give_stages("p1", 2);
    game.give("p1", "Altar");
    game.set_hand("p1", &["Altar"]);
    assert_eq!(game.validate("p1", &build_free("Altar")), Err(ActionError::AlreadyBuilt));
}

#[test]
fn olympia_b_copies_the_best_neighbouring_guild() {
    // p1's neighbours are p3 (west) and p2 (east).
    let mut game = olympia(Side::B);
    game.give_stages("p1", 3);
    game.give("p2", "Workers Guild");
    game.give("p3", "Magistrates Guild");
    for card in ["Lumber Yard", "Stone Pit", "Clay Pool"] {
        game.give("p2", card);
    }
    game.give("p3", "Altar");
    // Workers: 3 brown cards next door → 3; Magistrates: 1 blue card → 1.
    assert_eq!(game.chosen_guild("p1"), Some("Workers Guild".to_string()));
    game.finish_now();
    assert_eq!(final_guild_and_science(&game, "p1").0, 3);
}

#[test]
fn copying_scientists_guild_counts_as_science() {
    let mut game = olympia(Side::B);
    game.give_stages("p1", 3);
    for card in ["Scriptorium", "School", "Workshop", "Laboratory", "Apothecary"] {
        game.give("p1", card);
    }
    game.give("p2", "Workers Guild");
    for card in ["Lumber Yard", "Stone Pit", "Clay Pool"] {
        game.give("p2", card);
    }
    game.give("p3", "Scientists Guild");
    // 2 tablets, 2 gears, 1 compass = 16; one more compass = 26 (+10 > +3).
    assert_eq!(game.chosen_guild("p1"), Some("Scientists Guild".to_string()));
    game.finish_now();
    assert_eq!(final_guild_and_science(&game, "p1"), (0, 26));
}

#[test]
fn equal_candidates_break_alphabetically() {
    let mut game = olympia(Side::B);
    game.give_stages("p1", 3);
    game.give("p2", "Spies Guild");
    game.give("p3", "Craftsmens Guild");
    assert_eq!(game.chosen_guild("p1"), Some("Craftsmens Guild".to_string()));
}

#[test]
fn no_copy_without_the_ability_or_an_eligible_guild() {
    let mut game = olympia(Side::B);
    game.give("p2", "Workers Guild");
    assert_eq!(game.chosen_guild("p1"), None);
    game.give_stages("p1", 3);
    game.give("p1", "Workers Guild");
    assert_eq!(game.chosen_guild("p1"), None);
}

#[test]
fn evaluating_candidates_leaves_scores_unchanged() {
    let mut game = olympia(Side::B);
    game.give_stages("p1", 3);
    game.give("p2", "Workers Guild");
    game.give("p3", "Scientists Guild");
    let before = game.score_now();
    assert!(game.chosen_guild("p1").is_some());
    assert_eq!(game.score_now(), before);
}
```

Add `pub mod game_olympia;` to the test list.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test tests::game_olympia`
Expected: compile error, no method `chosen_guild` / `finish_now`.

- [ ] **Step 3: Implement copy guild**

In `core/src/game/scoring.rs`, replace the imports with:

```rust
use super::types::{FinalScore, Phase};
use super::Game;
use crate::domain::{Category, Effect, PointCategory, Structure};
use std::collections::BTreeMap;

type StructureRef = &'static Structure<'static, Effect>;
```

replace `finish` with:

```rust
    pub(super) fn finish(&mut self) {
        self.apply_copy_guild();
        let scores = self.compute_scores();
        self.phase = Phase::GameOver { scores };
    }
```

and add to `impl Game`:

```rust
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
```

In `core/src/game/test_support.rs` add:

```rust
    pub fn chosen_guild(&mut self, player: &str) -> Option<String> {
        let seat = self.seat(player);
        self.best_guild_to_copy(seat)
            .map(|guild| guild.name().to_string())
    }

    pub fn finish_now(&mut self) {
        self.finish();
    }
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test tests::game_olympia`
Expected: PASS (7 tests). Then run `cargo test`. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/game/scoring.rs core/src/game/test_support.rs core/src/tests/game_olympia.rs core/src/lib.rs
git commit -m "feat(core): Olympía A free build per age and Olympía B guild copy at game end

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 15: Extra turns: Babylon B last card and Halikarnassós build-from-discard

**Files:**
- Modify: `core/src/game/resolve.rs`, `core/src/game/test_support.rs`
- Test: `core/src/tests/game_extra_turns.rs` (create), `core/src/lib.rs`

**Interfaces:**
- Consumes:
  - `apply_batch` return value (seats that triggered `EventType::ConstructFromDiscarded`)
  - `PlayerState.can_play_last_card` (set by the Babylon B stage 2 effect)
  - `extra_turns` queue
  - `precheck`, which already answers `NotYourTurn` to non-acting players
  - `check_action`, which already restricts actions per `ExtraTurnKind`
- Produces (game-internal):
  - `handle_last_cards(&mut self)`, which replaces `discard_last_cards`
  - `buildable_discard_names(&self, seat) -> Vec<String>`: distinct, sorted names not already built by `seat`
  - `advance`, which now first pops queued extra turns, skipping a `BuildFromDiscard` with nothing buildable
- Produces (tests only): hook `set_discard(&[&str])`.

- [ ] **Step 1: Write the failing tests**

Create `core/src/tests/game_extra_turns.rs`:

```rust
use super::game_helpers::{build, discard, discard_first, discard_turns, game_with, stage};
use crate::domain::PointCategory::CivilianP;
use crate::game::{Action, ActionError, ExtraTurnKind, Game, Phase, Side};

fn babylon_b() -> Game {
    game_with(&[("Babylon", Side::B), ("Rhódos", Side::A), ("Éphesos", Side::A)])
}

fn halikarnassos() -> Game {
    game_with(&[("Halikarnassós", Side::A), ("Rhódos", Side::A), ("Éphesos", Side::A)])
}

fn from_discard(card: &str) -> Action {
    Action::BuildFromDiscard { card: card.to_string() }
}

fn extra_turn(player: &str, kind: ExtraTurnKind) -> Phase {
    Phase::ExtraTurn { player: player.to_string(), kind }
}

/// Halikarnassós A: stage 1 done, and the 3 Ore for stage 2 available.
fn ready_for_halikarnassos_stage_two(game: &mut Game) {
    game.give_stages("p1", 1);
    game.give("p1", "Foundry");
    game.give("p1", "Ore Vein");
}

#[test]
fn babylon_b_plays_the_seventh_card() {
    let mut game = babylon_b();
    game.give_stages("p1", 2);
    discard_turns(&mut game, 6);
    assert_eq!(game.phase(), &extra_turn("p1", ExtraTurnKind::PlayLastCard));
    assert_eq!(game.hand_names("p1").len(), 1);
    assert!(game.hand_names("p2").is_empty());
    assert_eq!(game.discard_names().len(), 18 + 2);
    let last = game.hand_names("p1")[0].clone();
    assert_eq!(game.submit("p2", discard("Altar")), Err(ActionError::NotYourTurn));
    assert_eq!(
        game.validate("p1", &Action::BuildFree { card: last.clone() }),
        Err(ActionError::ActionNotAllowedNow)
    );
    assert_eq!(game.validate("p1", &from_discard(&last)), Err(ActionError::ActionNotAllowedNow));
    game.submit("p1", discard(&last)).unwrap();
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 2, turn: 1 });
    assert_eq!(game.discard_names().len(), 21);
}

#[test]
fn babylon_b_can_build_its_seventh_card() {
    let mut game = babylon_b();
    game.give_stages("p1", 2);
    discard_turns(&mut game, 6);
    game.set_hand("p1", &["Lumber Yard"]);
    game.submit("p1", build("Lumber Yard")).unwrap();
    assert_eq!(game.built_names("p1"), vec!["Lumber Yard".to_string()]);
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 2, turn: 1 });
}

#[test]
fn babylon_b_stage_built_on_turn_six_grants_the_seventh_card_that_age() {
    // Babylon B stage 2 costs Glass + 2 Wood.
    let mut game = babylon_b();
    game.give_stages("p1", 1);
    game.give("p1", "Glassworks");
    game.give("p1", "Sawmill");
    discard_turns(&mut game, 5);
    game.set_hand("p1", &["Altar", "Theater"]);
    game.submit("p1", stage("Altar")).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    assert_eq!(game.phase(), &extra_turn("p1", ExtraTurnKind::PlayLastCard));
    assert_eq!(game.hand_names("p1"), vec!["Theater".to_string()]);
}

#[test]
fn halikarnassos_builds_from_the_discard_pile_at_the_end_of_the_turn() {
    let mut game = halikarnassos();
    ready_for_halikarnassos_stage_two(&mut game);
    game.set_discard(&["Palace", "Altar"]);
    game.set_hand("p2", &["Pawnshop"]);
    let tucked = game.hand_names("p1")[0].clone();
    game.submit("p1", stage(&tucked)).unwrap();
    game.submit("p2", discard("Pawnshop")).unwrap();
    discard_first(&mut game, "p3");
    assert_eq!(game.phase(), &extra_turn("p1", ExtraTurnKind::BuildFromDiscard));
    // Cards discarded this very turn are eligible.
    assert_eq!(game.validate("p1", &from_discard("Pawnshop")), Ok(()));
    assert_eq!(game.validate("p1", &discard("Altar")), Err(ActionError::ActionNotAllowedNow));
    assert_eq!(game.validate("p1", &from_discard("Senate")), Err(ActionError::CardNotInDiscard));
    game.submit("p1", from_discard("Palace")).unwrap();
    assert!(game.built_names("p1").contains(&"Palace".to_string()));
    assert!(!game.discard_names().contains(&"Palace".to_string()));
    assert_eq!(game.points("p1").get(&CivilianP), Some(&8));
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 2 });
    assert_eq!(game.hand_names("p1").len(), 6);
}

#[test]
fn halikarnassos_skips_the_extra_turn_when_nothing_is_buildable() {
    let mut game = halikarnassos();
    ready_for_halikarnassos_stage_two(&mut game);
    game.give("p1", "Altar");
    game.set_discard(&["Altar"]);
    game.set_hand("p2", &["Lumber Yard"]);
    game.set_hand("p3", &["Stone Pit"]);
    let tucked = game.hand_names("p1")[0].clone();
    game.submit("p1", stage(&tucked)).unwrap();
    game.submit("p2", build("Lumber Yard")).unwrap();
    game.submit("p3", build("Stone Pit")).unwrap();
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 2 });
}

#[test]
fn halikarnassos_at_the_end_of_an_age_can_take_the_discarded_last_cards() {
    let mut game = halikarnassos();
    ready_for_halikarnassos_stage_two(&mut game);
    discard_turns(&mut game, 5);
    game.set_discard(&[]);
    game.set_hand("p2", &["Pawnshop", "Palace"]);
    let tucked = game.hand_names("p1")[0].clone();
    game.submit("p1", stage(&tucked)).unwrap();
    game.submit("p2", discard("Pawnshop")).unwrap();
    discard_first(&mut game, "p3");
    assert_eq!(game.phase(), &extra_turn("p1", ExtraTurnKind::BuildFromDiscard));
    assert_eq!(game.validate("p1", &from_discard("Palace")), Ok(()));
    game.submit("p1", from_discard("Palace")).unwrap();
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 2, turn: 1 });
}

#[test]
fn babylon_plays_its_last_card_before_halikarnassos_builds_from_discard() {
    let mut game = game_with(&[("Halikarnassós", Side::A), ("Babylon", Side::B), ("Éphesos", Side::A)]);
    ready_for_halikarnassos_stage_two(&mut game);
    game.give_stages("p2", 2);
    discard_turns(&mut game, 5);
    game.set_discard(&[]);
    let tucked = game.hand_names("p1")[0].clone();
    game.submit("p1", stage(&tucked)).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    assert_eq!(game.phase(), &extra_turn("p2", ExtraTurnKind::PlayLastCard));
    game.set_hand("p2", &["Palace"]);
    game.submit("p2", discard("Palace")).unwrap();
    assert_eq!(game.phase(), &extra_turn("p1", ExtraTurnKind::BuildFromDiscard));
    assert_eq!(game.validate("p1", &from_discard("Palace")), Ok(()));
}
```

Add `pub mod game_extra_turns;` to the test list.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test tests::game_extra_turns`
Expected: compile error (no `set_discard`). With the hook added, the tests FAIL: no `ExtraTurn` phase is ever entered.

- [ ] **Step 3: Implement the extra-turn queue**

In `core/src/game/resolve.rs`, change the imports to:

```rust
use super::types::{ExtraTurnKind, Kind, Phase, Resolved};
use super::Game;
use crate::domain::{Card, EventType, PlayerDecision};
use std::cmp::Ordering;
use std::collections::BTreeSet;
```

Replace `resolve` and `discard_last_cards` with:

```rust
    pub(super) fn resolve(&mut self) {
        let was_choosing = matches!(self.phase, Phase::ChoosingCards { .. });
        let batch = self.take_pending();
        let build_from_discard = self.apply_batch(batch);
        if was_choosing && self.hands.iter().all(|hand| hand.len() == 1) {
            self.handle_last_cards();
        }
        for seat in build_from_discard {
            self.extra_turns
                .push_back((seat, ExtraTurnKind::BuildFromDiscard));
        }
        self.advance();
    }

    /// End of turn 6: Babylon B keeps its last card for an extra turn; all
    /// other last cards are discarded. Queued before any Halikarnassós turn
    /// so these discards are eligible for it.
    fn handle_last_cards(&mut self) {
        for seat in 0..self.seats.len() {
            if self.player(seat).can_play_last_card {
                self.extra_turns.push_back((seat, ExtraTurnKind::PlayLastCard));
            } else if let Some(card) = self.hands[seat].pop() {
                self.state.cards_discarded.push(card);
            }
        }
    }

    /// Distinct names of discarded cards `seat` has not built, sorted.
    pub(super) fn buildable_discard_names(&self, seat: usize) -> Vec<String> {
        let names: BTreeSet<&str> = self
            .state
            .cards_discarded
            .iter()
            .filter(|card| !self.already_built(seat, **card))
            .map(|card| card.0.name())
            .collect();
        names.into_iter().map(str::to_string).collect()
    }
```

and put this loop at the top of `advance`, before the `if self.hands.iter().all(Vec::is_empty)` branch:

```rust
        while let Some((seat, kind)) = self.extra_turns.pop_front() {
            if kind == ExtraTurnKind::BuildFromDiscard
                && self.buildable_discard_names(seat).is_empty()
            {
                continue;
            }
            self.phase = Phase::ExtraTurn { player: self.seats[seat].clone(), kind };
            return;
        }
```

In `core/src/game/test_support.rs` add:

```rust
    pub fn set_discard(&mut self, cards: &[&str]) {
        self.state.cards_discarded = cards.iter().map(|name| card(name)).collect();
    }
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test tests::game_extra_turns`
Expected: PASS (7 tests). Then run `cargo test`. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/game/resolve.rs core/src/game/test_support.rs core/src/tests/game_extra_turns.rs core/src/lib.rs
git commit -m "feat(core): Babylon B last-card and Halikarnassós build-from-discard extra turns

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---
### Task 16: `PlayerView`

**Files:**
- Create: `core/src/game/view.rs`
- Modify: `core/src/game/mod.rs` (`mod view;` + re-exports)
- Test: `core/src/tests/game_view.rs` (create), `core/src/lib.rs`

**Interfaces:**
- Consumes: `structure_requirement`, `wonder_requirement`, `Requirement`, `market`, `payment_options`, `free_build_available`, `already_built`, `buildable_discard_names`, `pending`, `built`, `wonders`
- Produces (public, re-exported from `crate::game`):
  - `Game::view(&self, player: &str) -> Result<PlayerView, ActionError>` (`UnknownPlayer`)
  - ```rust
    pub struct PlayerView { pub me: String, pub phase: PhaseView, pub players: Vec<PublicPlayer>, pub west: String, pub east: String,
        pub hand: Vec<HandCard>, pub discard_pile: Option<Vec<String>>, pub discard_count: usize,
        pub submitted: Vec<(String, bool)>, pub my_pending: Option<Action>, pub scores: Option<Vec<FinalScore>> }
    pub struct PhaseView { pub kind: PhaseKind, pub age: u8, pub turn: u8, pub direction: PassDirection,
        pub extra_turn_player: Option<String>, pub extra_turn_kind: Option<ExtraTurnKind> }
    pub enum PhaseKind { ChoosingCards, ExtraTurn, GameOver }
    pub enum PassDirection { West, East }
    pub struct PublicPlayer { pub name: String, pub wonder: String, pub side: Side, pub stages_built: u8, pub stages_total: u8,
        pub built: Vec<BuiltCard>, pub coins: u32, pub shields: u32, pub military_tokens: Vec<i32>, pub free_build_available: bool }
    pub struct BuiltCard { pub name: String, pub category: Category, pub age: u8 }
    pub struct HandCard { pub name: String, pub category: Category, pub age: u8,
        pub build: BuildOption, pub wonder_stage: BuildOption, pub free_build: bool }
    pub enum BuildOption { Unavailable { reason: ActionError }, Free, Coins(u32), Trade(Vec<PaymentOption>) }
    ```
- Rules:
  - `hand` is empty unless the viewer is acting with a hand: `ChoosingCards` (even after submitting), or their own `PlayLastCard` turn.
  - `discard_pile` is `Some(buildable names)` only during the viewer's own `BuildFromDiscard` turn.
  - `submitted` lists every seat during `ChoosingCards`, only the acting player during `ExtraTurn`, and is empty at game over.
  - `my_pending` shows only the viewer's own choice.
  - `scores` is `Some` at game over.

- [ ] **Step 1: Write the failing tests**

Create `core/src/tests/game_view.rs`:

```rust
use super::game_helpers::{
    discard, discard_age, discard_first, discard_turns, game_with, plain_game, stage,
};
use crate::game::{
    ActionError, BuildOption, ExtraTurnKind, PassDirection, Payment, PaymentOption, PhaseKind,
    ResourceType::*, Side,
};

#[test]
fn the_initial_view_describes_the_table() {
    let game = plain_game();
    let view = game.view("p1").unwrap();
    assert_eq!(view.me, "p1");
    assert_eq!((view.west.as_str(), view.east.as_str()), ("p3", "p2"));
    assert_eq!(view.phase.kind, PhaseKind::ChoosingCards);
    assert_eq!((view.phase.age, view.phase.turn), (1, 1));
    assert_eq!(view.phase.direction, PassDirection::West);
    assert_eq!(view.phase.extra_turn_player, None);
    assert_eq!(view.hand.len(), 7);
    assert_eq!(view.discard_pile, None);
    assert_eq!(view.discard_count, 0);
    assert_eq!(
        view.submitted,
        vec![("p1".to_string(), false), ("p2".to_string(), false), ("p3".to_string(), false)]
    );
    assert_eq!(view.my_pending, None);
    assert_eq!(view.scores, None);
    let names: Vec<&str> = view.players.iter().map(|p| p.name.as_str()).collect();
    assert_eq!(names, vec!["p1", "p2", "p3"]);
    let me = &view.players[0];
    assert_eq!((me.wonder.as_str(), me.side), ("Gizah", Side::A));
    assert_eq!((me.stages_built, me.stages_total), (0, 3));
    assert_eq!((me.coins, me.shields), (3, 0));
    assert!(me.built.is_empty() && me.military_tokens.is_empty());
    assert!(!me.free_build_available);
}

#[test]
fn hand_cards_carry_their_build_options() {
    let mut game = plain_game();
    game.give("p1", "Altar");
    game.set_hand("p1", &["Lumber Yard", "Tree Farm", "Barracks", "Stockade", "Altar"]);
    let hand = game.view("p1").unwrap().hand;
    assert_eq!(hand[0].build, BuildOption::Free);
    assert_eq!(hand[1].build, BuildOption::Coins(1));
    assert_eq!(
        hand[2].build,
        BuildOption::Trade(vec![PaymentOption {
            payment: Payment { west: vec![], east: vec![(Ore, 1)] },
            west_coins: 0,
            east_coins: 2,
            bank_coins: 0,
        }])
    );
    assert_eq!(hand[3].build, BuildOption::Unavailable { reason: ActionError::CannotAfford });
    assert_eq!(hand[4].build, BuildOption::Unavailable { reason: ActionError::AlreadyBuilt });
    // Gizah A stage 1 needs 2 Stone; p1 has 1 and nobody sells Stone.
    assert!(hand
        .iter()
        .all(|card| card.wonder_stage == BuildOption::Unavailable { reason: ActionError::CannotAfford }));
    assert!(hand.iter().all(|card| !card.free_build));
    assert_eq!((hand[0].age, hand[0].name.as_str()), (1, "Lumber Yard"));
}

#[test]
fn pending_choices_are_private_but_submission_is_public() {
    let mut game = plain_game();
    let card = game.hand_names("p1")[0].clone();
    game.submit("p1", discard(&card)).unwrap();
    let mine = game.view("p1").unwrap();
    assert_eq!(mine.my_pending, Some(discard(&card)));
    assert_eq!(mine.hand.len(), 7);
    let theirs = game.view("p2").unwrap();
    assert_eq!(theirs.my_pending, None);
    assert_eq!(theirs.submitted[0], ("p1".to_string(), true));
    assert_eq!(theirs.submitted[1], ("p2".to_string(), false));
}

#[test]
fn built_cards_coins_and_tokens_are_public() {
    let mut game = plain_game();
    game.give("p2", "Barracks");
    discard_age(&mut game);
    let view = game.view("p1").unwrap();
    assert_eq!(view.phase.direction, PassDirection::East);
    assert_eq!(view.discard_count, 21);
    let p2 = &view.players[1];
    assert_eq!(p2.built[0].name, "Barracks");
    assert_eq!(p2.built[0].age, 1);
    assert_eq!((p2.shields, p2.military_tokens.clone()), (1, vec![1, 1]));
    assert_eq!(p2.coins, 21);
}

#[test]
fn olympia_free_build_is_flagged() {
    let mut game = game_with(&[("Olympía", Side::A), ("Rhódos", Side::A), ("Éphesos", Side::A)]);
    game.give_stages("p1", 2);
    game.give("p1", "Altar");
    game.set_hand("p1", &["Altar", "Palace"]);
    let view = game.view("p1").unwrap();
    assert!(view.players[0].free_build_available);
    assert!(!view.hand[0].free_build);
    assert!(view.hand[1].free_build);
}

#[test]
fn others_during_an_extra_turn_can_view_but_not_act() {
    let mut game = game_with(&[("Babylon", Side::B), ("Rhódos", Side::A), ("Éphesos", Side::A)]);
    game.give_stages("p1", 2);
    discard_turns(&mut game, 6);
    let acting = game.view("p1").unwrap();
    assert_eq!(acting.phase.kind, PhaseKind::ExtraTurn);
    assert_eq!(acting.phase.extra_turn_player, Some("p1".to_string()));
    assert_eq!(acting.phase.extra_turn_kind, Some(ExtraTurnKind::PlayLastCard));
    assert_eq!(acting.hand.len(), 1);
    assert_eq!(acting.submitted, vec![("p1".to_string(), false)]);
    let waiting = game.view("p2").unwrap();
    assert!(waiting.hand.is_empty());
    assert_eq!(waiting.discard_pile, None);
    assert_eq!(waiting.submitted, vec![("p1".to_string(), false)]);
    assert_eq!(game.submit("p2", discard("Altar")), Err(ActionError::NotYourTurn));
}

#[test]
fn halikarnassos_sees_only_buildable_discards() {
    let mut game = game_with(&[("Halikarnassós", Side::A), ("Rhódos", Side::A), ("Éphesos", Side::A)]);
    game.give_stages("p1", 1);
    game.give("p1", "Foundry");
    game.give("p1", "Ore Vein");
    game.give("p1", "Altar");
    game.set_discard(&["Palace", "Altar", "Palace"]);
    game.set_hand("p2", &["Pawnshop"]);
    let tucked = game.hand_names("p1")[0].clone();
    game.submit("p1", stage(&tucked)).unwrap();
    game.submit("p2", discard("Pawnshop")).unwrap();
    game.set_hand("p3", &["Tavern"]);
    game.submit("p3", discard("Tavern")).unwrap();
    let view = game.view("p1").unwrap();
    assert!(view.hand.is_empty());
    assert_eq!(
        view.discard_pile,
        Some(vec!["Palace".to_string(), "Pawnshop".to_string(), "Tavern".to_string()])
    );
    assert_eq!(game.view("p2").unwrap().discard_pile, None);
}

#[test]
fn game_over_view_has_scores_and_no_hand() {
    let mut game = plain_game();
    for _ in 0..3 {
        discard_age(&mut game);
    }
    let view = game.view("p2").unwrap();
    assert_eq!(view.phase.kind, PhaseKind::GameOver);
    assert!(view.hand.is_empty());
    assert!(view.submitted.is_empty());
    assert_eq!(view.scores.map(|scores| scores.len()), Some(3));
}

#[test]
fn unknown_viewer_is_rejected() {
    let mut game = plain_game();
    discard_first(&mut game, "p1");
    assert_eq!(game.view("zed").err(), Some(ActionError::UnknownPlayer));
}
```

Add `pub mod game_view;` to the test list.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test tests::game_view`
Expected: compile error, no method `view`, and `BuildOption`, `PhaseKind`… are not found in `crate::game`.

- [ ] **Step 3: Implement the view**

Create `core/src/game/view.rs`:

```rust
//! Per-player view: public table state plus the viewer's private hand and
//! options. Options come from the same legality code `submit` uses.
use super::legality::Requirement;
use super::payment::payment_options;
use super::types::{Action, ActionError, ExtraTurnKind, FinalScore, PaymentOption, Phase, Side};
use super::Game;
use crate::domain::{Card, Category};
use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum PhaseKind {
    ChoosingCards,
    ExtraTurn,
    GameOver,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum PassDirection {
    West,
    East,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PhaseView {
    pub kind: PhaseKind,
    pub age: u8,
    pub turn: u8,
    pub direction: PassDirection,
    pub extra_turn_player: Option<String>,
    pub extra_turn_kind: Option<ExtraTurnKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BuiltCard {
    pub name: String,
    pub category: Category,
    pub age: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PublicPlayer {
    pub name: String,
    pub wonder: String,
    pub side: Side,
    pub stages_built: u8,
    pub stages_total: u8,
    pub built: Vec<BuiltCard>,
    pub coins: u32,
    pub shields: u32,
    pub military_tokens: Vec<i32>,
    pub free_build_available: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum BuildOption {
    Unavailable { reason: ActionError },
    /// Chain build or no cost at all: submit with an empty payment.
    Free,
    /// Coin cost only (paid to the bank): submit with an empty payment.
    Coins(u32),
    /// Resources must be bought; submit one of these payments.
    Trade(Vec<PaymentOption>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HandCard {
    pub name: String,
    pub category: Category,
    pub age: u8,
    pub build: BuildOption,
    pub wonder_stage: BuildOption,
    pub free_build: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PlayerView {
    pub me: String,
    pub phase: PhaseView,
    pub players: Vec<PublicPlayer>,
    pub west: String,
    pub east: String,
    pub hand: Vec<HandCard>,
    pub discard_pile: Option<Vec<String>>,
    pub discard_count: usize,
    pub submitted: Vec<(String, bool)>,
    pub my_pending: Option<Action>,
    pub scores: Option<Vec<FinalScore>>,
}

impl Game {
    pub fn view(&self, player: &str) -> Result<PlayerView, ActionError> {
        let seat = self.seat_of(player).ok_or(ActionError::UnknownPlayer)?;
        Ok(self.build_view(seat))
    }

    fn build_view(&self, seat: usize) -> PlayerView {
        let me = &self.seats[seat];
        let acting_with_hand = match &self.phase {
            Phase::ChoosingCards { .. } => true,
            Phase::ExtraTurn { player, kind: ExtraTurnKind::PlayLastCard } => player == me,
            _ => false,
        };
        let hand = if acting_with_hand {
            let stage = self.option_for(seat, self.wonder_requirement(seat));
            self.hands[seat]
                .iter()
                .map(|card| self.hand_card(seat, *card, &stage))
                .collect()
        } else {
            Vec::new()
        };
        let discard_pile = match &self.phase {
            Phase::ExtraTurn { player, kind: ExtraTurnKind::BuildFromDiscard } if player == me => {
                Some(self.buildable_discard_names(seat))
            }
            _ => None,
        };
        let submitted = match &self.phase {
            Phase::ChoosingCards { .. } => self
                .seats
                .iter()
                .zip(&self.pending)
                .map(|(name, pending)| (name.clone(), pending.is_some()))
                .collect(),
            Phase::ExtraTurn { player, .. } => vec![(player.clone(), false)],
            Phase::GameOver { .. } => Vec::new(),
        };
        let scores = match &self.phase {
            Phase::GameOver { scores } => Some(scores.clone()),
            _ => None,
        };
        PlayerView {
            me: me.clone(),
            phase: self.phase_view(),
            players: (0..self.seats.len()).map(|s| self.public_player(s)).collect(),
            west: self.seats[self.west_of(seat)].clone(),
            east: self.seats[self.east_of(seat)].clone(),
            hand,
            discard_pile,
            discard_count: self.state.cards_discarded.len(),
            submitted,
            my_pending: self.pending[seat].as_ref().map(|p| p.action.clone()),
            scores,
        }
    }

    fn hand_card(&self, seat: usize, card: Card, stage: &BuildOption) -> HandCard {
        HandCard {
            name: card.0.name().to_string(),
            category: card.0.category(),
            age: card.0.age().number(),
            build: self.option_for(seat, self.structure_requirement(seat, card)),
            wonder_stage: stage.clone(),
            free_build: matches!(self.phase, Phase::ChoosingCards { .. })
                && self.free_build_available(seat)
                && !self.already_built(seat, card),
        }
    }

    fn option_for(&self, seat: usize, requirement: Result<Requirement, ActionError>) -> BuildOption {
        match requirement {
            Err(reason) => BuildOption::Unavailable { reason },
            Ok(Requirement::Free) => BuildOption::Free,
            Ok(Requirement::Coins(coins)) => BuildOption::Coins(coins),
            Ok(Requirement::Trade { coin_cost, resources }) => {
                let options = payment_options(&self.market(seat), coin_cost, &resources);
                if options.is_empty() {
                    BuildOption::Unavailable { reason: ActionError::CannotAfford }
                } else {
                    BuildOption::Trade(options)
                }
            }
        }
    }

    fn phase_view(&self) -> PhaseView {
        let direction = if self.age == 2 { PassDirection::East } else { PassDirection::West };
        let (kind, extra_turn_player, extra_turn_kind) = match &self.phase {
            Phase::ChoosingCards { .. } => (PhaseKind::ChoosingCards, None, None),
            Phase::ExtraTurn { player, kind } => (PhaseKind::ExtraTurn, Some(player.clone()), Some(*kind)),
            Phase::GameOver { .. } => (PhaseKind::GameOver, None, None),
        };
        PhaseView { kind, age: self.age, turn: self.turn, direction, extra_turn_player, extra_turn_kind }
    }

    fn public_player(&self, seat: usize) -> PublicPlayer {
        let player = self.player(seat);
        let (wonder, side) = self.wonders[seat].clone();
        PublicPlayer {
            name: self.seats[seat].clone(),
            wonder,
            side,
            stages_built: player.wonder_stages_built,
            stages_total: u8::try_from(player.wonder.stages_total()).expect("at most 4 stages"),
            built: self.built[seat]
                .iter()
                .map(|card| BuiltCard {
                    name: card.0.name().to_string(),
                    category: card.0.category(),
                    age: card.0.age().number(),
                })
                .collect(),
            coins: player.coins,
            shields: player.military_symbols,
            military_tokens: player.battle_tokens.clone(),
            free_build_available: self.free_build_available(seat),
        }
    }
}
```

In `core/src/game/mod.rs`, add `mod view;` and:

```rust
pub use view::{
    BuildOption, BuiltCard, HandCard, PassDirection, PhaseKind, PhaseView, PlayerView,
    PublicPlayer,
};
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test tests::game_view`
Expected: PASS (9 tests). Then run `cargo test`. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/game/view.rs core/src/game/mod.rs core/src/tests/game_view.rs core/src/lib.rs
git commit -m "feat(core): per-player view with build options from shared legality

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 17: Simulation, determinism and view ⇔ submit invariant

**Files:**
- Test: `core/src/tests/game_simulation.rs` (create), `core/src/lib.rs`

**Interfaces:**
- Consumes: `Game::{new, view, submit, phase, players}`, hooks `validate` and `hand_names`, `GameRng`
- Produces: `tests::game_simulation::{available_actions, play_random_game}` (test-only; Task 18's DTO tests do not depend on them)

- [ ] **Step 1: Write the simulation tests**

Create `core/src/tests/game_simulation.rs`:

```rust
use super::game_helpers::names;
use crate::engine::rng::GameRng;
use crate::game::{Action, BuildOption, Game, Payment, Phase, PlayerView, WonderSelection};

fn push_options(out: &mut Vec<Action>, option: &BuildOption, make: impl Fn(Payment) -> Action) {
    match option {
        BuildOption::Unavailable { .. } => {}
        BuildOption::Free | BuildOption::Coins(_) => out.push(make(Payment::default())),
        BuildOption::Trade(options) => {
            out.extend(options.iter().map(|option| make(option.payment.clone())))
        }
    }
}

/// Every action the view offers.
pub fn available_actions(view: &PlayerView) -> Vec<Action> {
    let mut actions = Vec::new();
    if let Some(pile) = &view.discard_pile {
        actions.extend(pile.iter().map(|card| Action::BuildFromDiscard { card: card.clone() }));
    }
    for card in &view.hand {
        push_options(&mut actions, &card.build, |payment| Action::Build {
            card: card.name.clone(),
            payment,
        });
        push_options(&mut actions, &card.wonder_stage, |payment| Action::BuildWonderStage {
            card: card.name.clone(),
            payment,
        });
        if card.free_build {
            actions.push(Action::BuildFree { card: card.name.clone() });
        }
        actions.push(Action::Discard { card: card.name.clone() });
    }
    actions
}

fn acting_players(game: &Game) -> Vec<String> {
    match game.phase() {
        Phase::ChoosingCards { .. } => game.players().to_vec(),
        Phase::ExtraTurn { player, .. } => vec![player.clone()],
        Phase::GameOver { .. } => Vec::new(),
    }
}

fn cards_on_table(view: &PlayerView) -> usize {
    view.players
        .iter()
        .map(|p| p.built.len() + usize::from(p.stages_built))
        .sum::<usize>()
        + view.discard_count
}

fn assert_invariants(game: &Game) {
    let players = game.players().len();
    let view = game.view(&game.players()[0]).unwrap();
    let in_hands: usize = game.players().iter().map(|p| game.hand_names(p).len()).sum();
    assert_eq!(
        cards_on_table(&view) + in_hands,
        7 * players * usize::from(view.phase.age),
        "card conservation"
    );
    if let Phase::ChoosingCards { turn, .. } = game.phase() {
        for player in game.players() {
            assert_eq!(game.hand_names(player).len(), 8 - usize::from(*turn));
        }
    }
}

fn assert_view_agrees_with_submit(game: &Game, player: &str, view: &PlayerView, actions: &[Action]) {
    for action in actions {
        assert_eq!(game.validate(player, action), Ok(()), "view offered {action:?} to {player}");
    }
    for card in &view.hand {
        if let BuildOption::Unavailable { reason } = &card.build {
            let action = Action::Build { card: card.name.clone(), payment: Payment::default() };
            assert_eq!(game.validate(player, &action), Err(*reason));
        }
        if let BuildOption::Unavailable { reason } = &card.wonder_stage {
            let action = Action::BuildWonderStage { card: card.name.clone(), payment: Payment::default() };
            assert_eq!(game.validate(player, &action), Err(*reason));
        }
    }
}

/// Plays a full game choosing uniformly among the view's options.
/// `on_step` sees the game before every round of submissions.
/// Coins are `u32`: a negative balance would panic (debug overflow check).
pub fn play_random_game(players: usize, seed: u64, mut on_step: impl FnMut(&Game)) -> Game {
    let mut game = Game::new(names(players), WonderSelection::Random, seed).unwrap();
    let mut chooser = GameRng::new(seed.wrapping_mul(31).wrapping_add(17));
    for _ in 0..1_000 {
        on_step(&game);
        let acting = acting_players(&game);
        if acting.is_empty() {
            return game;
        }
        assert_invariants(&game);
        for player in acting {
            let view = game.view(&player).unwrap();
            let actions = available_actions(&view);
            assert!(!actions.is_empty(), "{player} has no legal action");
            assert_view_agrees_with_submit(&game, &player, &view, &actions);
            let choice = actions[chooser.below(actions.len())].clone();
            game.submit(&player, choice).unwrap();
        }
    }
    panic!("game did not finish within 1000 rounds");
}

#[test]
fn random_games_finish_for_every_player_count() {
    for players in 3..=7 {
        for seed in 0..20 {
            let game = play_random_game(players, seed, |_| {});
            let Phase::GameOver { scores } = game.phase() else {
                panic!("game {players}/{seed} did not end");
            };
            assert_eq!(scores.len(), players);
            let mut ranked: Vec<String> = scores.iter().map(|s| s.player.clone()).collect();
            ranked.sort();
            let mut expected = names(players);
            expected.sort();
            assert_eq!(ranked, expected);
            assert!(scores.iter().any(|s| s.rank == 1));
            assert!(scores.iter().all(|s| s.rank >= 1 && usize::from(s.rank) <= players));
            let view = game.view("p1").unwrap();
            assert_eq!(cards_on_table(&view), 21 * players, "card conservation at the end");
        }
    }
}

#[test]
fn identical_inputs_produce_identical_views() {
    let record = |seed: u64| -> Vec<PlayerView> {
        let mut views = Vec::new();
        play_random_game(5, seed, |game| {
            for player in game.players() {
                views.push(game.view(player).unwrap());
            }
        });
        views
    };
    assert_eq!(record(3), record(3));
    assert_ne!(record(3), record(4));
}
```

Add `pub mod game_simulation;` to the test list.

- [ ] **Step 2: Run the simulation**

Run: `cargo test tests::game_simulation`
Expected: PASS (2 tests). Debug builds may need 10–60 s.

If an assertion fails, it names the offending action or card. Fix the engine, not the test. Legality, the view and resolution all live in `game/`. Add a focused regression test for the bug in the matching test file (legality, resolution, extra turns, …) before fixing it.

If the run takes longer than 2 minutes, profile with `cargo test --release tests::game_simulation` first. Do not lower the player counts or seed counts; the spec requires n = 3..=7 with at least 20 seeds each.

- [ ] **Step 3: Commit**

```bash
cargo fmt && cargo fmt --check
git add core/src/tests/game_simulation.rs core/src/lib.rs
git commit -m "test(core): random full-game simulation, determinism and view/submit invariant

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---
### Task 18: Game settings, debug JSON, NIF DTOs and functions (remove the legacy `start_game` path)

**Files:**
- Create: `core/src/game/settings.rs`, `core/src/nif/mod.rs`, `core/src/nif/dto.rs`
- Modify: `core/src/game/mod.rs` (settings, `debug_json`), `core/src/engine/mod.rs`, `core/src/engine/data.rs` (drop `WONDER_NAMES`), `core/Cargo.toml` (drop `rand`)
- Replace: `core/src/lib.rs`
- Delete: `core/src/api/` (whole directory), `core/src/engine/game_init.rs`, `core/src/tests/api.rs`, `core/src/common/` (empty module)
- Test: `core/src/tests/game_settings.rs` (create), `core/src/tests/nif_dto.rs` (create)

**Interfaces:**
- Produces (Rust, `crate::game`):
  - `settings() -> GameSettings { engine_version: u32, wonders: Vec<WonderInfo { name, sides: Vec<Side> }>, cards: Vec<CardInfo { name, category, age: u8 }> }`. It lists 78 card entries: Age I, II, III and guilds, with duplicates across ages (e.g. Loom at age 1 and age 2).
  - `Game::debug_json(&self) -> String`: keys `engine_version, seats, wonders, age, turn, phase, hands, state`.
- Produces (Elixir-visible, `Elixir.Helios.Core.Native`). These are the exact shapes Phase 4 consumes:

  | NIF | Arguments | Returns |
  |---|---|---|
  | `game_settings/0` | — | `%{engine_version: 1, wonders: [%{name: "Rhódos", sides: [:a, :b]}, …], cards: [%{name: "Lumber Yard", category: :raw_material, age: 1}, …]}` |
  | `new_game/3` | `players :: [String.t()]`, `wonders :: :random \| {:explicit, [{name, :a \| :b}]}`, `seed :: 0..2^64-1` | `{:ok, reference}` \| `{:error, :invalid_players_number \| {:duplicate_player, name} \| {:invalid_wonder, name} \| {:wonders_length_mismatch, %{players: n, wonders: m}} \| {:duplicate_wonder, name}}` |
  | `submit/3` | `ref`, `player`, action | `:ok` \| `{:error, atom}` |
  | `view/2` | `ref`, `player` | `{:ok, view_map}` \| `{:error, :unknown_player}` |
  | `debug_game/1` | `ref` | `{:ok, json}` \| `{:error, :lock_fail}` |

  - Action terms:
    - `{:build, %{card: "Altar", payment: %{west: [], east: [{:wood, 1}]}}}`
    - `{:build_wonder_stage, %{card: _, payment: _}}`
    - `{:discard, "Altar"}`
    - `{:build_free, "Altar"}`
    - `{:build_from_discard, "Altar"}`
  - `submit/3` error atoms: `:unknown_player, :not_your_turn, :game_over, :card_not_in_hand, :card_not_in_discard, :already_built, :cannot_afford, :invalid_payment, :no_wonder_stage_left, :free_build_unavailable, :action_not_allowed_now`, plus `:lock_fail` if the mutex is poisoned.
  - View map keys: `me, phase, players, west, east, hand, discard_pile, discard_count, submitted, my_pending, scores`.
    - `phase`: `%{kind: :choosing_cards | :extra_turn | :game_over, age, turn, direction: :west | :east, extra_turn_player: nil | name, extra_turn_kind: nil | :play_last_card | :build_from_discard}`
    - player map: `%{name, wonder, side, stages_built, stages_total, built: [%{name, category, age}], coins, shields, military_tokens, free_build_available}`
    - hand card: `%{name, category, age, build, wonder_stage, free_build}`
    - `build` / `wonder_stage`: `{:unavailable, reason_atom} | :free | {:coins, n} | {:trade, [%{payment, west_coins, east_coins, bank_coins}]}`
    - `submitted`: `[{name, boolean}]`
    - `my_pending`: `nil` or an action term
    - `scores`: `nil` or `[%{player, military, treasury, wonder, civilian, scientific, commercial, guild, total, coins, rank}]`

> **Tasks 18 and 19 are one deployable unit.** After this task, `Helios.Core.Native` in Elixir still declares `start_game/2` and lacks the new stubs, so Helios cannot load the NIF until Task 19 lands. Do not push between them.

- [ ] **Step 1: Write the failing tests**

Create `core/src/tests/game_settings.rs`:

```rust
use super::game_helpers::plain_game;
use crate::game::{settings, CardInfo, Category, Side, ENGINE_VERSION};

#[test]
fn settings_list_every_wonder_and_card() {
    let s = settings();
    assert_eq!(s.engine_version, ENGINE_VERSION);
    assert_eq!(s.wonders.len(), 7);
    assert_eq!(s.wonders[0].name, "Rhódos");
    assert!(s.wonders.iter().all(|w| w.sides == vec![Side::A, Side::B]));
    assert_eq!(s.cards.len(), 78);
    for (name, category, age) in [
        ("Loom", Category::MG, 1),
        ("Loom", Category::MG, 2),
        ("Altar", Category::Civilian, 1),
        ("Builders Guild", Category::Guild, 3),
    ] {
        let card = CardInfo { name: name.to_string(), category, age };
        assert!(s.cards.contains(&card), "{name} age {age} missing");
    }
}

#[test]
fn debug_json_is_valid_json() {
    let game = plain_game();
    let value: serde_json::Value = serde_json::from_str(&game.debug_json()).unwrap();
    assert_eq!(value["engine_version"], 1);
    assert_eq!(value["seats"], serde_json::json!(["p1", "p2", "p3"]));
    assert!(value["state"]["player_states"].is_object());
    assert_eq!(value["hands"][0].as_array().map(Vec::len), Some(7));
    assert!(value["phase"]["ChoosingCards"].is_object());
}
```

Create `core/src/tests/nif_dto.rs`:

```rust
use super::game_helpers::plain_game;
use crate::game::{
    Action, ActionError, BuildOption, Category, Payment, PaymentOption, ResourceType::*,
    SetupError, Side, WonderSelection,
};
use crate::nif::dto::{
    ActionDto, ActionErrorDto, BuildOptionDto, CategoryDto, PaymentDto, PaymentOptionDto,
    PhaseKindDto, PlayerViewDto, ResourceDto, SetupErrorDto, SideDto, WondersDto,
};

#[test]
fn action_dtos_round_trip() {
    let pairs = [
        (
            ActionDto::Build {
                card: "Altar".to_string(),
                payment: PaymentDto { west: vec![], east: vec![(ResourceDto::Wood, 1)] },
            },
            Action::Build {
                card: "Altar".to_string(),
                payment: Payment { west: vec![], east: vec![(Wood, 1)] },
            },
        ),
        (
            ActionDto::BuildWonderStage {
                card: "Altar".to_string(),
                payment: PaymentDto { west: vec![(ResourceDto::Papyrus, 2)], east: vec![] },
            },
            Action::BuildWonderStage {
                card: "Altar".to_string(),
                payment: Payment { west: vec![(Papyrus, 2)], east: vec![] },
            },
        ),
        (ActionDto::Discard("Altar".to_string()), Action::Discard { card: "Altar".to_string() }),
        (ActionDto::BuildFree("Altar".to_string()), Action::BuildFree { card: "Altar".to_string() }),
        (
            ActionDto::BuildFromDiscard("Altar".to_string()),
            Action::BuildFromDiscard { card: "Altar".to_string() },
        ),
    ];
    for (dto, action) in pairs {
        assert_eq!(Action::from(dto.clone()), action);
        assert_eq!(ActionDto::from(action), dto);
    }
}

#[test]
fn setup_inputs_and_errors_convert() {
    assert_eq!(WonderSelection::from(WondersDto::Random), WonderSelection::Random);
    assert_eq!(
        WonderSelection::from(WondersDto::Explicit(vec![("Gizah".to_string(), SideDto::B)])),
        WonderSelection::Explicit(vec![("Gizah".to_string(), Side::B)])
    );
    assert_eq!(SetupErrorDto::from(SetupError::InvalidPlayersNumber(2)), SetupErrorDto::InvalidPlayersNumber);
    assert_eq!(
        SetupErrorDto::from(SetupError::WondersLengthMismatch { players: 3, wonders: 1 }),
        SetupErrorDto::WondersLengthMismatch { players: 3, wonders: 1 }
    );
    assert_eq!(
        SetupErrorDto::from(SetupError::DuplicatePlayer("a".to_string())),
        SetupErrorDto::DuplicatePlayer("a".to_string())
    );
}

#[test]
fn view_parts_convert() {
    assert_eq!(ActionErrorDto::from(ActionError::CannotAfford), ActionErrorDto::CannotAfford);
    assert_eq!(CategoryDto::from(Category::MG), CategoryDto::ManufacturedGood);
    assert_eq!(CategoryDto::from(Category::RM), CategoryDto::RawMaterial);
    let option = PaymentOption {
        payment: Payment { west: vec![(Ore, 1)], east: vec![] },
        west_coins: 2,
        east_coins: 0,
        bank_coins: 0,
    };
    assert_eq!(
        BuildOptionDto::from(BuildOption::Trade(vec![option])),
        BuildOptionDto::Trade(vec![PaymentOptionDto {
            payment: PaymentDto { west: vec![(ResourceDto::Ore, 1)], east: vec![] },
            west_coins: 2,
            east_coins: 0,
            bank_coins: 0,
        }])
    );
    assert_eq!(
        BuildOptionDto::from(BuildOption::Unavailable { reason: ActionError::AlreadyBuilt }),
        BuildOptionDto::Unavailable(ActionErrorDto::AlreadyBuilt)
    );
}

#[test]
fn a_whole_view_converts() {
    let dto = PlayerViewDto::from(plain_game().view("p1").unwrap());
    assert_eq!(dto.me, "p1");
    assert_eq!(dto.phase.kind, PhaseKindDto::ChoosingCards);
    assert_eq!(dto.players.len(), 3);
    assert_eq!(dto.hand.len(), 7);
    assert_eq!(dto.submitted.len(), 3);
    assert_eq!(dto.my_pending, None);
}
```

- [ ] **Step 2: Remove the legacy path and rewrite `lib.rs`**

```bash
git rm -r core/src/api core/src/engine/game_init.rs core/src/tests/api.rs
git rm -r --ignore-unmatch core/src/common
```

In `core/src/engine/mod.rs`, remove `pub mod game_init;`. In `core/src/engine/data.rs`, delete the `WONDER_NAMES` lazy static (its only user was `api`). In `core/Cargo.toml`, delete the `rand = …` dependency line, then confirm nothing else uses it: `grep -rn "rand::" core/src` must print nothing.

Replace `core/src/lib.rs` with:

```rust
//! `seven_wonders_core`: the 7 Wonders base-game engine (`game`) with a
//! Rustler NIF boundary (`nif`) loaded by Helios as `Helios.Core.Native`.
mod domain;
mod engine;
pub mod game;
mod nif;

#[cfg(test)]
mod tests {
    pub mod data;
    pub mod deck;
    pub mod game_ages;
    pub mod game_effects;
    pub mod game_extra_turns;
    pub mod game_helpers;
    pub mod game_legality;
    pub mod game_olympia;
    pub mod game_payment;
    pub mod game_resolution;
    pub mod game_scoring;
    pub mod game_settings;
    pub mod game_setup;
    pub mod game_simulation;
    pub mod game_view;
    pub mod helpers;
    pub mod nif_dto;
    pub mod points;
    pub mod production;
    pub mod rng;
}

rustler::init!("Elixir.Helios.Core.Native");
```

(If Phase 0 kept other test modules that still exist on disk, keep them in the list too. `ls core/src/tests` is the source of truth.)

- [ ] **Step 3: Run the tests to verify they fail**

Run: `cargo test -- tests::game_settings tests::nif_dto`
Expected: compile errors: `settings`, `debug_json` and `crate::nif` don't exist.

- [ ] **Step 4: Implement settings and debug JSON**

Create `core/src/game/settings.rs`:

```rust
//! Static game metadata for clients (asset mapping, wonder lists).
use super::types::Side;
use super::ENGINE_VERSION;
use crate::domain::Category;
use crate::engine::data::{
    AGE_III_STRUCTURES, AGE_II_STRUCTURES, AGE_I_STRUCTURES, GUILD_STRUCTURES, WONDERS,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GameSettings {
    pub engine_version: u32,
    pub wonders: Vec<WonderInfo>,
    pub cards: Vec<CardInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WonderInfo {
    pub name: String,
    pub sides: Vec<Side>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CardInfo {
    pub name: String,
    pub category: Category,
    pub age: u8,
}

pub fn settings() -> GameSettings {
    GameSettings {
        engine_version: ENGINE_VERSION,
        wonders: WONDERS
            .iter()
            .map(|wonder| WonderInfo { name: wonder.0.to_string(), sides: vec![Side::A, Side::B] })
            .collect(),
        cards: AGE_I_STRUCTURES
            .iter()
            .chain(AGE_II_STRUCTURES.iter())
            .chain(AGE_III_STRUCTURES.iter())
            .chain(GUILD_STRUCTURES.iter())
            .map(|structure| CardInfo {
                name: structure.name().to_string(),
                category: structure.category(),
                age: structure.age().number(),
            })
            .collect(),
    }
}
```

In `core/src/game/mod.rs`, add `mod settings;` and `pub use settings::{settings, CardInfo, GameSettings, WonderInfo};`. Then add to `impl Game`:

```rust
    /// Full internal state as JSON, for debugging only (not a stable format).
    pub fn debug_json(&self) -> String {
        let hands: Vec<Vec<&str>> = self
            .hands
            .iter()
            .map(|hand| hand.iter().map(|card| card.0.name()).collect())
            .collect();
        serde_json::json!({
            "engine_version": ENGINE_VERSION,
            "seats": self.seats,
            "wonders": self.wonders,
            "age": self.age,
            "turn": self.turn,
            "phase": self.phase,
            "hands": hands,
            "state": self.state,
        })
        .to_string()
    }
```

- [ ] **Step 5: Implement the DTOs**

Create `core/src/nif/dto.rs`:

```rust
//! Boundary DTOs: rustler encodings of `crate::game` types. Conversions only;
//! no game logic lives here.
use crate::game::{
    Action, ActionError, BuildOption, BuiltCard, CardInfo, Category, ExtraTurnKind, FinalScore,
    GameSettings, HandCard, PassDirection, Payment, PaymentOption, PhaseKind, PhaseView,
    PlayerView, PublicPlayer, ResourceType, SetupError, Side, WonderInfo, WonderSelection,
};
use rustler::{NifMap, NifTaggedEnum, NifUnitEnum};

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum ResourceDto {
    Wood,
    Stone,
    Ore,
    Clay,
    Glass,
    Loom,
    Papyrus,
}

impl From<ResourceType> for ResourceDto {
    fn from(resource: ResourceType) -> Self {
        match resource {
            ResourceType::Wood => Self::Wood,
            ResourceType::Stone => Self::Stone,
            ResourceType::Ore => Self::Ore,
            ResourceType::Clay => Self::Clay,
            ResourceType::Glass => Self::Glass,
            ResourceType::Loom => Self::Loom,
            ResourceType::Papyrus => Self::Papyrus,
        }
    }
}

impl From<ResourceDto> for ResourceType {
    fn from(resource: ResourceDto) -> Self {
        match resource {
            ResourceDto::Wood => Self::Wood,
            ResourceDto::Stone => Self::Stone,
            ResourceDto::Ore => Self::Ore,
            ResourceDto::Clay => Self::Clay,
            ResourceDto::Glass => Self::Glass,
            ResourceDto::Loom => Self::Loom,
            ResourceDto::Papyrus => Self::Papyrus,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum CategoryDto {
    Civilian,
    Commercial,
    Guild,
    ManufacturedGood,
    Military,
    RawMaterial,
    Scientific,
}

impl From<Category> for CategoryDto {
    fn from(category: Category) -> Self {
        match category {
            Category::Civilian => Self::Civilian,
            Category::Commercial => Self::Commercial,
            Category::Guild => Self::Guild,
            Category::MG => Self::ManufacturedGood,
            Category::Military => Self::Military,
            Category::RM => Self::RawMaterial,
            Category::Scientific => Self::Scientific,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum SideDto {
    A,
    B,
}

impl From<Side> for SideDto {
    fn from(side: Side) -> Self {
        match side {
            Side::A => Self::A,
            Side::B => Self::B,
        }
    }
}

impl From<SideDto> for Side {
    fn from(side: SideDto) -> Self {
        match side {
            SideDto::A => Self::A,
            SideDto::B => Self::B,
        }
    }
}

/// `:random` | `{:explicit, [{"Gizah", :a}, …]}`
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum WondersDto {
    Random,
    Explicit(Vec<(String, SideDto)>),
}

impl From<WondersDto> for WonderSelection {
    fn from(wonders: WondersDto) -> Self {
        match wonders {
            WondersDto::Random => Self::Random,
            WondersDto::Explicit(choices) => Self::Explicit(
                choices
                    .into_iter()
                    .map(|(name, side)| (name, side.into()))
                    .collect(),
            ),
        }
    }
}

/// `%{west: [{:wood, 1}], east: []}`
#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct PaymentDto {
    pub west: Vec<(ResourceDto, u8)>,
    pub east: Vec<(ResourceDto, u8)>,
}

impl From<Payment> for PaymentDto {
    fn from(payment: Payment) -> Self {
        let convert = |side: Vec<(ResourceType, u8)>| {
            side.into_iter().map(|(r, n)| (r.into(), n)).collect()
        };
        Self { west: convert(payment.west), east: convert(payment.east) }
    }
}

impl From<PaymentDto> for Payment {
    fn from(payment: PaymentDto) -> Self {
        let convert = |side: Vec<(ResourceDto, u8)>| {
            side.into_iter().map(|(r, n)| (r.into(), n)).collect()
        };
        Self { west: convert(payment.west), east: convert(payment.east) }
    }
}

/// `{:build, %{card, payment}}` | `{:build_wonder_stage, %{card, payment}}` |
/// `{:discard, card}` | `{:build_free, card}` | `{:build_from_discard, card}`
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum ActionDto {
    Build { card: String, payment: PaymentDto },
    BuildWonderStage { card: String, payment: PaymentDto },
    Discard(String),
    BuildFree(String),
    BuildFromDiscard(String),
}

impl From<ActionDto> for Action {
    fn from(action: ActionDto) -> Self {
        match action {
            ActionDto::Build { card, payment } => Self::Build { card, payment: payment.into() },
            ActionDto::BuildWonderStage { card, payment } => {
                Self::BuildWonderStage { card, payment: payment.into() }
            }
            ActionDto::Discard(card) => Self::Discard { card },
            ActionDto::BuildFree(card) => Self::BuildFree { card },
            ActionDto::BuildFromDiscard(card) => Self::BuildFromDiscard { card },
        }
    }
}

impl From<Action> for ActionDto {
    fn from(action: Action) -> Self {
        match action {
            Action::Build { card, payment } => Self::Build { card, payment: payment.into() },
            Action::BuildWonderStage { card, payment } => {
                Self::BuildWonderStage { card, payment: payment.into() }
            }
            Action::Discard { card } => Self::Discard(card),
            Action::BuildFree { card } => Self::BuildFree(card),
            Action::BuildFromDiscard { card } => Self::BuildFromDiscard(card),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum ActionErrorDto {
    UnknownPlayer,
    NotYourTurn,
    GameOver,
    CardNotInHand,
    CardNotInDiscard,
    AlreadyBuilt,
    CannotAfford,
    InvalidPayment,
    NoWonderStageLeft,
    FreeBuildUnavailable,
    ActionNotAllowedNow,
}

impl From<ActionError> for ActionErrorDto {
    fn from(error: ActionError) -> Self {
        match error {
            ActionError::UnknownPlayer => Self::UnknownPlayer,
            ActionError::NotYourTurn => Self::NotYourTurn,
            ActionError::GameOver => Self::GameOver,
            ActionError::CardNotInHand => Self::CardNotInHand,
            ActionError::CardNotInDiscard => Self::CardNotInDiscard,
            ActionError::AlreadyBuilt => Self::AlreadyBuilt,
            ActionError::CannotAfford => Self::CannotAfford,
            ActionError::InvalidPayment => Self::InvalidPayment,
            ActionError::NoWonderStageLeft => Self::NoWonderStageLeft,
            ActionError::FreeBuildUnavailable => Self::FreeBuildUnavailable,
            ActionError::ActionNotAllowedNow => Self::ActionNotAllowedNow,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum SetupErrorDto {
    InvalidPlayersNumber,
    DuplicatePlayer(String),
    InvalidWonder(String),
    WondersLengthMismatch { players: usize, wonders: usize },
    DuplicateWonder(String),
}

impl From<SetupError> for SetupErrorDto {
    fn from(error: SetupError) -> Self {
        match error {
            SetupError::InvalidPlayersNumber(_) => Self::InvalidPlayersNumber,
            SetupError::DuplicatePlayer(name) => Self::DuplicatePlayer(name),
            SetupError::InvalidWonder(name) => Self::InvalidWonder(name),
            SetupError::WondersLengthMismatch { players, wonders } => {
                Self::WondersLengthMismatch { players, wonders }
            }
            SetupError::DuplicateWonder(name) => Self::DuplicateWonder(name),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum ExtraTurnKindDto {
    PlayLastCard,
    BuildFromDiscard,
}

impl From<ExtraTurnKind> for ExtraTurnKindDto {
    fn from(kind: ExtraTurnKind) -> Self {
        match kind {
            ExtraTurnKind::PlayLastCard => Self::PlayLastCard,
            ExtraTurnKind::BuildFromDiscard => Self::BuildFromDiscard,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum PhaseKindDto {
    ChoosingCards,
    ExtraTurn,
    GameOver,
}

impl From<PhaseKind> for PhaseKindDto {
    fn from(kind: PhaseKind) -> Self {
        match kind {
            PhaseKind::ChoosingCards => Self::ChoosingCards,
            PhaseKind::ExtraTurn => Self::ExtraTurn,
            PhaseKind::GameOver => Self::GameOver,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum DirectionDto {
    West,
    East,
}

impl From<PassDirection> for DirectionDto {
    fn from(direction: PassDirection) -> Self {
        match direction {
            PassDirection::West => Self::West,
            PassDirection::East => Self::East,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct PhaseViewDto {
    pub kind: PhaseKindDto,
    pub age: u8,
    pub turn: u8,
    pub direction: DirectionDto,
    pub extra_turn_player: Option<String>,
    pub extra_turn_kind: Option<ExtraTurnKindDto>,
}

impl From<PhaseView> for PhaseViewDto {
    fn from(phase: PhaseView) -> Self {
        Self {
            kind: phase.kind.into(),
            age: phase.age,
            turn: phase.turn,
            direction: phase.direction.into(),
            extra_turn_player: phase.extra_turn_player,
            extra_turn_kind: phase.extra_turn_kind.map(Into::into),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct BuiltCardDto {
    pub name: String,
    pub category: CategoryDto,
    pub age: u8,
}

impl From<BuiltCard> for BuiltCardDto {
    fn from(card: BuiltCard) -> Self {
        Self { name: card.name, category: card.category.into(), age: card.age }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct PublicPlayerDto {
    pub name: String,
    pub wonder: String,
    pub side: SideDto,
    pub stages_built: u8,
    pub stages_total: u8,
    pub built: Vec<BuiltCardDto>,
    pub coins: u32,
    pub shields: u32,
    pub military_tokens: Vec<i32>,
    pub free_build_available: bool,
}

impl From<PublicPlayer> for PublicPlayerDto {
    fn from(player: PublicPlayer) -> Self {
        Self {
            name: player.name,
            wonder: player.wonder,
            side: player.side.into(),
            stages_built: player.stages_built,
            stages_total: player.stages_total,
            built: player.built.into_iter().map(Into::into).collect(),
            coins: player.coins,
            shields: player.shields,
            military_tokens: player.military_tokens,
            free_build_available: player.free_build_available,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct PaymentOptionDto {
    pub payment: PaymentDto,
    pub west_coins: u32,
    pub east_coins: u32,
    pub bank_coins: u32,
}

impl From<PaymentOption> for PaymentOptionDto {
    fn from(option: PaymentOption) -> Self {
        Self {
            payment: option.payment.into(),
            west_coins: option.west_coins,
            east_coins: option.east_coins,
            bank_coins: option.bank_coins,
        }
    }
}

/// `{:unavailable, reason}` | `:free` | `{:coins, n}` | `{:trade, [option]}`
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum BuildOptionDto {
    Unavailable(ActionErrorDto),
    Free,
    Coins(u32),
    Trade(Vec<PaymentOptionDto>),
}

impl From<BuildOption> for BuildOptionDto {
    fn from(option: BuildOption) -> Self {
        match option {
            BuildOption::Unavailable { reason } => Self::Unavailable(reason.into()),
            BuildOption::Free => Self::Free,
            BuildOption::Coins(coins) => Self::Coins(coins),
            BuildOption::Trade(options) => Self::Trade(options.into_iter().map(Into::into).collect()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct HandCardDto {
    pub name: String,
    pub category: CategoryDto,
    pub age: u8,
    pub build: BuildOptionDto,
    pub wonder_stage: BuildOptionDto,
    pub free_build: bool,
}

impl From<HandCard> for HandCardDto {
    fn from(card: HandCard) -> Self {
        Self {
            name: card.name,
            category: card.category.into(),
            age: card.age,
            build: card.build.into(),
            wonder_stage: card.wonder_stage.into(),
            free_build: card.free_build,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct FinalScoreDto {
    pub player: String,
    pub military: i32,
    pub treasury: i32,
    pub wonder: i32,
    pub civilian: i32,
    pub scientific: i32,
    pub commercial: i32,
    pub guild: i32,
    pub total: i32,
    pub coins: u32,
    pub rank: u8,
}

impl From<FinalScore> for FinalScoreDto {
    fn from(score: FinalScore) -> Self {
        Self {
            player: score.player,
            military: score.military,
            treasury: score.treasury,
            wonder: score.wonder,
            civilian: score.civilian,
            scientific: score.scientific,
            commercial: score.commercial,
            guild: score.guild,
            total: score.total,
            coins: score.coins,
            rank: score.rank,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct PlayerViewDto {
    pub me: String,
    pub phase: PhaseViewDto,
    pub players: Vec<PublicPlayerDto>,
    pub west: String,
    pub east: String,
    pub hand: Vec<HandCardDto>,
    pub discard_pile: Option<Vec<String>>,
    pub discard_count: usize,
    pub submitted: Vec<(String, bool)>,
    pub my_pending: Option<ActionDto>,
    pub scores: Option<Vec<FinalScoreDto>>,
}

impl From<PlayerView> for PlayerViewDto {
    fn from(view: PlayerView) -> Self {
        Self {
            me: view.me,
            phase: view.phase.into(),
            players: view.players.into_iter().map(Into::into).collect(),
            west: view.west,
            east: view.east,
            hand: view.hand.into_iter().map(Into::into).collect(),
            discard_pile: view.discard_pile,
            discard_count: view.discard_count,
            submitted: view.submitted,
            my_pending: view.my_pending.map(Into::into),
            scores: view
                .scores
                .map(|scores| scores.into_iter().map(Into::into).collect()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct WonderInfoDto {
    pub name: String,
    pub sides: Vec<SideDto>,
}

impl From<WonderInfo> for WonderInfoDto {
    fn from(wonder: WonderInfo) -> Self {
        Self { name: wonder.name, sides: wonder.sides.into_iter().map(Into::into).collect() }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct CardInfoDto {
    pub name: String,
    pub category: CategoryDto,
    pub age: u8,
}

impl From<CardInfo> for CardInfoDto {
    fn from(card: CardInfo) -> Self {
        Self { name: card.name, category: card.category.into(), age: card.age }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct GameSettingsDto {
    pub engine_version: u32,
    pub wonders: Vec<WonderInfoDto>,
    pub cards: Vec<CardInfoDto>,
}

impl From<GameSettings> for GameSettingsDto {
    fn from(settings: GameSettings) -> Self {
        Self {
            engine_version: settings.engine_version,
            wonders: settings.wonders.into_iter().map(Into::into).collect(),
            cards: settings.cards.into_iter().map(Into::into).collect(),
        }
    }
}
```

- [ ] **Step 6: Implement the NIFs**

Create `core/src/nif/mod.rs`:

```rust
//! Rustler boundary. Decodes Elixir terms into `crate::game` types, calls the
//! engine, encodes the result. Calls are serialized per game by the mutex
//! (and by the GameServer); each takes well under 1 ms, so no dirty scheduler.
pub(crate) mod dto;

use crate::game::{self, Game};
use dto::{ActionDto, ActionErrorDto, GameSettingsDto, PlayerViewDto, SetupErrorDto, WondersDto};
use rustler::{Atom, Encoder, Env, ResourceArc, Term};
use std::sync::Mutex;

mod atoms {
    rustler::atoms! { ok, error, lock_fail }
}

pub struct GameResource(pub Mutex<Game>);

#[rustler::resource_impl]
impl rustler::Resource for GameResource {}

#[rustler::nif]
fn game_settings() -> GameSettingsDto {
    game::settings().into()
}

#[rustler::nif]
fn new_game(
    players: Vec<String>,
    wonders: WondersDto,
    seed: u64,
) -> Result<ResourceArc<GameResource>, SetupErrorDto> {
    Game::new(players, wonders.into(), seed)
        .map(|game| ResourceArc::new(GameResource(Mutex::new(game))))
        .map_err(SetupErrorDto::from)
}

/// `:ok` | `{:error, reason}`
#[rustler::nif]
fn submit<'a>(
    env: Env<'a>,
    resource: ResourceArc<GameResource>,
    player: String,
    action: ActionDto,
) -> Term<'a> {
    let Ok(mut game) = resource.0.lock() else {
        return (atoms::error(), atoms::lock_fail()).encode(env);
    };
    match game.submit(&player, action.into()) {
        Ok(()) => atoms::ok().encode(env),
        Err(error) => (atoms::error(), ActionErrorDto::from(error)).encode(env),
    }
}

/// `{:ok, view}` | `{:error, reason}`
#[rustler::nif]
fn view<'a>(env: Env<'a>, resource: ResourceArc<GameResource>, player: String) -> Term<'a> {
    let Ok(game) = resource.0.lock() else {
        return (atoms::error(), atoms::lock_fail()).encode(env);
    };
    match game.view(&player) {
        Ok(view) => (atoms::ok(), PlayerViewDto::from(view)).encode(env),
        Err(error) => (atoms::error(), ActionErrorDto::from(error)).encode(env),
    }
}

#[rustler::nif]
fn debug_game(resource: ResourceArc<GameResource>) -> Result<String, Atom> {
    let game = resource.0.lock().map_err(|_| atoms::lock_fail())?;
    Ok(game.debug_json())
}
```

(If the rustler version pinned by Phase 0 does not support NIF auto-discovery (it has since 0.34), or needs `#[rustler::resource_impl]` spelled differently, follow the pattern Phase 0 used in its `lib.rs` for `GameResource` and `rustler::init!`.)

- [ ] **Step 7: Run all Rust checks**

```bash
cargo test
cargo fmt && cargo fmt --check
cargo clippy --all-targets -- -D warnings
```

Expected: all tests PASS, including `tests::game_settings` and `tests::nif_dto`. From here on clippy must be clean. Fix each finding in the code this plan introduced:

- Apply clippy's suggested rewrites (e.g. `needless_range_loop` → `enumerate`/`zip`).
- For `dead_code` on an item this plan made unused, delete the item.
- Do not add blanket `#[allow]`s. If a finding is in untouched legacy code that Phase 0 allowed, keep Phase 0's existing allow.

- [ ] **Step 8: Commit**

```bash
# (removed paths were staged by git rm in Step 2)
git add core/Cargo.toml core/Cargo.lock core/src/lib.rs core/src/game/mod.rs core/src/game/settings.rs core/src/nif/mod.rs core/src/nif/dto.rs core/src/engine/mod.rs core/src/engine/data.rs core/src/tests/game_settings.rs core/src/tests/nif_dto.rs
git commit -m "feat(core): native-term NIFs new_game/submit/view/game_settings/debug_game

Removes the legacy start_game path, the api module and the rand dependency.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

If Step 7 required clippy fixes in other files, add those files explicitly as well.

---

### Task 19: `Helios.Core` wrapper and ExUnit tests (including full random games for n = 3 and n = 7)

**Files:**
- Replace: `helios/lib/helios/core/native.ex`, `helios/lib/helios/core.ex`, `helios/test/helios/core_test.exs`

**Interfaces:**
- Consumes: the NIF API from Task 18 (exact shapes in its Interfaces block).
- Produces (Elixir, consumed by Phase 4's `Helios.Games`, `GameServer` and `GameLive`):
  - `Helios.Core.game_settings() :: map()`
  - `Helios.Core.new_game([String.t()], :random | {:explicit, [{String.t(), :a | :b}]}, 0..18_446_744_073_709_551_615) :: {:ok, reference()} | {:error, setup_error}`
  - `Helios.Core.submit(reference(), String.t(), action) :: :ok | {:error, atom()}`
  - `Helios.Core.view(reference(), String.t()) :: {:ok, map()} | {:error, :unknown_player | :lock_fail}`
  - `Helios.Core.debug_game(reference()) :: {:ok, map()} | {:error, :lock_fail}`

- [ ] **Step 1: Write the failing ExUnit tests**

Replace `helios/test/helios/core_test.exs` with:

```elixir
defmodule Helios.CoreTest do
  use ExUnit.Case, async: true

  alias Helios.Core

  @three ["a", "b", "c"]
  @plain_wonders {:explicit, [{"Gizah", :a}, {"Rhódos", :a}, {"Éphesos", :a}]}
  @empty_payment %{west: [], east: []}

  describe "game_settings/0" do
    test "exposes the engine version, every wonder and every card" do
      settings = Core.game_settings()
      assert settings.engine_version == 1
      assert length(settings.wonders) == 7
      assert %{name: "Halikarnassós", sides: [:a, :b]} in settings.wonders
      assert length(settings.cards) == 78
      assert %{name: "Altar", category: :civilian, age: 1} in settings.cards
      assert %{name: "Loom", category: :manufactured_good, age: 2} in settings.cards
    end
  end

  describe "new_game/3" do
    test "starts a game with random wonders" do
      assert {:ok, game} = Core.new_game(@three, :random, 42)
      assert is_reference(game)
    end

    test "starts a game with explicit wonders in seat order" do
      {:ok, game} = Core.new_game(@three, @plain_wonders, 1)
      {:ok, view} = Core.view(game, "a")

      assert Enum.map(view.players, &{&1.name, &1.wonder, &1.side}) ==
               [{"a", "Gizah", :a}, {"b", "Rhódos", :a}, {"c", "Éphesos", :a}]
    end

    test "accepts the full u64 seed range and rejects anything outside it" do
      assert {:ok, _} = Core.new_game(@three, :random, 0)
      assert {:ok, _} = Core.new_game(@three, :random, 18_446_744_073_709_551_615)

      assert_raise FunctionClauseError, fn ->
        Core.new_game(@three, :random, 18_446_744_073_709_551_616)
      end

      assert_raise FunctionClauseError, fn -> Core.new_game(@three, :random, -1) end
    end

    test "reports setup errors" do
      assert Core.new_game(["a", "b"], :random, 1) == {:error, :invalid_players_number}

      assert Core.new_game(Enum.map(1..8, &"p#{&1}"), :random, 1) ==
               {:error, :invalid_players_number}

      assert Core.new_game(["a", "b", "a"], :random, 1) == {:error, {:duplicate_player, "a"}}

      assert Core.new_game(@three, {:explicit, [{"Gizah", :a}]}, 1) ==
               {:error, {:wonders_length_mismatch, %{players: 3, wonders: 1}}}

      assert Core.new_game(@three, {:explicit, [{"Gizah", :a}, {"Rhodos", :a}, {"Éphesos", :a}]}, 1) ==
               {:error, {:invalid_wonder, "Rhodos"}}

      assert Core.new_game(@three, {:explicit, [{"Gizah", :a}, {"Gizah", :b}, {"Éphesos", :a}]}, 1) ==
               {:error, {:duplicate_wonder, "Gizah"}}
    end
  end

  describe "submit/3 and view/2" do
    setup do
      {:ok, game} = Core.new_game(@three, @plain_wonders, 7)
      %{game: game}
    end

    test "the initial view has the documented shape", %{game: game} do
      assert {:ok, view} = Core.view(game, "a")
      assert view.me == "a"
      assert view.west == "c"
      assert view.east == "b"

      assert view.phase == %{
               kind: :choosing_cards,
               age: 1,
               turn: 1,
               direction: :west,
               extra_turn_player: nil,
               extra_turn_kind: nil
             }

      assert length(view.hand) == 7
      assert view.discard_pile == nil
      assert view.discard_count == 0
      assert view.submitted == [{"a", false}, {"b", false}, {"c", false}]
      assert view.my_pending == nil
      assert view.scores == nil

      assert [%{name: _, category: _, age: 1, build: _, wonder_stage: _, free_build: false} | _] =
               view.hand

      assert [
               %{
                 name: "a",
                 wonder: "Gizah",
                 side: :a,
                 stages_built: 0,
                 stages_total: 3,
                 built: [],
                 coins: 3,
                 shields: 0,
                 military_tokens: [],
                 free_build_available: false
               }
               | _
             ] = view.players
    end

    test "everyone discarding advances the turn", %{game: game} do
      for player <- @three do
        {:ok, %{hand: [card | _]}} = Core.view(game, player)
        assert Core.submit(game, player, {:discard, card.name}) == :ok
      end

      {:ok, view} = Core.view(game, "a")
      assert %{kind: :choosing_cards, turn: 2} = view.phase
      assert length(view.hand) == 6
      assert view.discard_count == 3
      assert Enum.all?(view.players, &(&1.coins == 6))
    end

    test "a pending choice is visible to its owner only", %{game: game} do
      {:ok, %{hand: [card | _]}} = Core.view(game, "a")
      :ok = Core.submit(game, "a", {:discard, card.name})

      assert {:ok, %{my_pending: {:discard, name}}} = Core.view(game, "a")
      assert name == card.name
      assert {:ok, %{my_pending: nil, submitted: [{"a", true} | _]}} = Core.view(game, "b")
    end

    test "errors come back as atoms", %{game: game} do
      assert Core.view(game, "zed") == {:error, :unknown_player}
      assert Core.submit(game, "zed", {:discard, "Altar"}) == {:error, :unknown_player}
      assert Core.submit(game, "a", {:discard, "Not A Card"}) == {:error, :card_not_in_hand}

      assert Core.submit(game, "a", {:build_from_discard, "Altar"}) ==
               {:error, :action_not_allowed_now}

      assert Core.submit(game, "a", {:build_free, "Altar"}) == {:error, :free_build_unavailable}
    end

    test "malformed action terms raise instead of crashing the VM", %{game: game} do
      assert_raise ArgumentError, fn -> Core.submit(game, "a", {:build, "Altar"}) end

      assert_raise ArgumentError, fn ->
        Core.submit(game, "a", {:build, %{card: "Altar", payment: %{west: [{:gold, 1}], east: []}}})
      end

      assert_raise ArgumentError, fn -> Core.submit(game, "a", :discard) end
      # The game is still usable afterwards.
      assert {:ok, %{phase: %{turn: 1}}} = Core.view(game, "a")
    end

    test "debug_game/1 returns decoded JSON", %{game: game} do
      assert {:ok, %{"seats" => ["a", "b", "c"], "state" => %{"player_states" => states}}} =
               Core.debug_game(game)

      assert map_size(states) == 3
    end
  end

  describe "full random games through the NIF" do
    for players <- [3, 7] do
      @players players
      test "#{players} players reach game over" do
        names = Enum.map(1..@players, &"p#{&1}")
        {:ok, game} = Core.new_game(names, :random, 1_000 + @players)
        :rand.seed(:exsss, {@players, 2, 3})

        view = play_until_over(game, names, 0)

        assert length(view.scores) == @players
        assert Enum.sort(Enum.map(view.scores, & &1.player)) == Enum.sort(names)
        assert Enum.any?(view.scores, &(&1.rank == 1))
      end
    end
  end

  defp play_until_over(_game, _names, rounds) when rounds > 1_000,
    do: flunk("game did not finish")

  defp play_until_over(game, [first | _] = names, rounds) do
    {:ok, view} = Core.view(game, first)

    if view.phase.kind == :game_over do
      view
    else
      for {player, false} <- view.submitted do
        {:ok, player_view} = Core.view(game, player)
        action = player_view |> available_actions() |> Enum.random()
        assert Core.submit(game, player, action) == :ok
      end

      play_until_over(game, names, rounds + 1)
    end
  end

  defp available_actions(view) do
    from_discard = for name <- view.discard_pile || [], do: {:build_from_discard, name}

    from_hand =
      Enum.flat_map(view.hand, fn card ->
        options(card.build, &{:build, %{card: card.name, payment: &1}}) ++
          options(card.wonder_stage, &{:build_wonder_stage, %{card: card.name, payment: &1}}) ++
          if(card.free_build, do: [{:build_free, card.name}], else: []) ++
          [{:discard, card.name}]
      end)

    from_discard ++ from_hand
  end

  defp options({:unavailable, _reason}, _make), do: []
  defp options(:free, make), do: [make.(@empty_payment)]
  defp options({:coins, _}, make), do: [make.(@empty_payment)]
  defp options({:trade, choices}, make), do: Enum.map(choices, &make.(&1.payment))
end
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cd helios && mix test test/helios/core_test.exs`
Expected: FAIL. `Helios.Core.new_game/3` is undefined (and/or the NIF fails to load because `new_game/3` etc. are not declared in `Helios.Core.Native`).

- [ ] **Step 3: Replace the NIF stubs**

Replace `helios/lib/helios/core/native.ex` with the following, keeping Phase 0's exact `use Rustler …` options if they differ:

```elixir
defmodule Helios.Core.Native do
  @moduledoc false
  use Rustler, otp_app: :helios, crate: "seven_wonders_core", path: "../core"

  def game_settings, do: :erlang.nif_error(:nif_not_loaded)
  def new_game(_players, _wonders, _seed), do: :erlang.nif_error(:nif_not_loaded)
  def submit(_game, _player, _action), do: :erlang.nif_error(:nif_not_loaded)
  def view(_game, _player), do: :erlang.nif_error(:nif_not_loaded)
  def debug_game(_game), do: :erlang.nif_error(:nif_not_loaded)
end
```

- [ ] **Step 4: Replace the public wrapper**

Replace `helios/lib/helios/core.ex` with:

```elixir
defmodule Helios.Core do
  @moduledoc """
  The 7 Wonders rules engine (Rust, loaded as a NIF). All game rules live in
  the engine; callers only orchestrate and persist.

  A game is an opaque reference. Replaying the same `new_game/3` arguments
  and the same accepted `submit/3` calls always rebuilds the same state.
  """

  alias Helios.Core.Native

  @max_seed 18_446_744_073_709_551_615

  @type game :: reference()
  @type resource :: :wood | :stone | :ore | :clay | :glass | :loom | :papyrus
  @type payment :: %{west: [{resource(), pos_integer()}], east: [{resource(), pos_integer()}]}
  @type action ::
          {:build, %{card: String.t(), payment: payment()}}
          | {:build_wonder_stage, %{card: String.t(), payment: payment()}}
          | {:discard, String.t()}
          | {:build_free, String.t()}
          | {:build_from_discard, String.t()}
  @type wonders :: :random | {:explicit, [{String.t(), :a | :b}]}
  @type setup_error ::
          :invalid_players_number
          | {:duplicate_player, String.t()}
          | {:invalid_wonder, String.t()}
          | {:wonders_length_mismatch, %{players: non_neg_integer(), wonders: non_neg_integer()}}
          | {:duplicate_wonder, String.t()}
  @type action_error ::
          :unknown_player
          | :not_your_turn
          | :game_over
          | :card_not_in_hand
          | :card_not_in_discard
          | :already_built
          | :cannot_afford
          | :invalid_payment
          | :no_wonder_stage_left
          | :free_build_unavailable
          | :action_not_allowed_now
          | :lock_fail

  @doc "Engine version, wonders (with sides) and every card (name, category, age)."
  @spec game_settings() :: %{engine_version: pos_integer(), wonders: [map()], cards: [map()]}
  def game_settings, do: Native.game_settings()

  @doc "Starts a game; `players` is the seat order. `seed` must fit in a u64."
  @spec new_game([String.t()], wonders(), non_neg_integer()) ::
          {:ok, game()} | {:error, setup_error()}
  def new_game(players, wonders, seed)
      when is_list(players) and is_integer(seed) and seed >= 0 and seed <= @max_seed do
    Native.new_game(players, wonders, seed)
  end

  @doc """
  Submits (or replaces) `player`'s choice for the current turn. The turn
  resolves inside the call once every required player has submitted.
  Raises `ArgumentError` for malformed action terms.
  """
  @spec submit(game(), String.t(), action()) :: :ok | {:error, action_error()}
  def submit(game, player, action), do: Native.submit(game, player, action)

  @doc "The table as seen by `player` (hidden information excluded)."
  @spec view(game(), String.t()) :: {:ok, map()} | {:error, :unknown_player | :lock_fail}
  def view(game, player), do: Native.view(game, player)

  @doc "Full internal state, decoded from JSON. Debugging only."
  @spec debug_game(game()) :: {:ok, map()} | {:error, :lock_fail}
  def debug_game(game) do
    with {:ok, json} <- Native.debug_game(game), do: {:ok, Jason.decode!(json)}
  end
end
```

- [ ] **Step 5: Run the tests to verify they pass**

```bash
cd helios && mix format && mix test test/helios/core_test.exs
```

Expected: PASS (13 tests, including "3 players reach game over" and "7 players reach game over").

- [ ] **Step 6: Run the Helios gate**

Run: `cd helios && mix precommit`
Expected: PASS. If anything outside these three files still calls `Helios.Core.start_game/2` (check with `grep -rn "start_game" helios/lib helios/test`), switch it to `new_game/3` with the same player list, `:random` or `{:explicit, …}`, and a seed from `:rand.uniform(2 ** 63) - 1`, and add that file to the commit.

- [ ] **Step 7: Commit**

```bash
git add helios/lib/helios/core/native.ex helios/lib/helios/core.ex helios/test/helios/core_test.exs
git commit -m "feat(helios): Helios.Core wraps the new engine NIFs; full-game ExUnit tests

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 20: Final verification gate

**Files:**
- Modify only what the checks below flag.

**Interfaces:**
- Consumes: everything above.
- Produces: a green, clippy-clean tree that meets the spec's acceptance criteria.

- [ ] **Step 1: Run the full Rust suite and linters**

```bash
cd core
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
```

Expected: all three succeed. Every test module listed in `lib.rs` runs, the simulation included.

- [ ] **Step 2: Check for leftovers**

```bash
grep -rn "thread_rng\|rand::" core/src          # expect: nothing
grep -rn "HashMap" core/src/game                  # expect: nothing
grep -rn "rustler" core/src/game                  # expect: nothing (game stays rustler-free)
grep -rn "start_game\|Core.Api\|protobuf" core helios/lib helios/test   # expect: nothing
grep -rn "cover_resource_costs\|try_trading" core/src                  # expect: nothing
```

Any hit is a defect. Fix it in the owning file, re-run Step 1, and commit.

Then confirm the determinism audit the spec asks for (`game_state.rs`, `deck.rs`, `structure_builder.rs`; `game_init.rs` and `trading.rs` no longer exist). The remaining `HashMap`/`HashSet` uses in `domain` are `player_states`, `neighbours`, `built_structures`, `single_resources`, the `affected_players` sets and `PointsMap`. All of them are key lookups, membership tests or order-independent sums (`count_structures_for_categories`, `calculate_points`). Effects run in the order `apply_player_decisions` receives them, which is seat order, from a `Vec`. Only the debug JSON serialises a `HashMap`, and that output is not replay-relevant. If any other `HashMap` iteration feeds game state, replace it with a `BTreeMap` or a seat-ordered `Vec`.

- [ ] **Step 3: Run the Helios suite**

```bash
cd helios && mix precommit
```

Expected: PASS.

- [ ] **Step 4: Confirm acceptance criteria**

Check each spec acceptance criterion against evidence:

| Criterion | Evidence |
|---|---|
| Unit tests per rule | Tasks 2–16 test modules |
| Scenario tests on fixed seeds with explicit wonders | `game_extra_turns`, `game_olympia`, `game_view` (seed 7, explicit wonders) |
| Determinism tests | `game_setup::seed_42_deal_is_pinned`, `game_simulation::identical_inputs_produce_identical_views` |
| Simulation n = 3..=7 × 20 seeds with the invariants | `game_simulation::random_games_finish_for_every_player_count` |
| `cargo fmt --check`, clippy `-D warnings` | Step 1 |
| `Helios.Core` tests incl. full games for n = 3 and 7 | `helios/test/helios/core_test.exs` |

- [ ] **Step 5: Commit any fixes**

If Steps 1–3 required changes:

```bash
git add <each changed file>
git commit -m "chore(core): final lint and cleanup for the engine API

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

## Notes for Phase 4 (consumer contract)

- Engine player ids are the strings passed to `new_game/3`. Phase 4 passes stringified user ids, in seat order.
- `game_actions.action` persistence (`ActionCodec`) must round-trip exactly these terms:
  - `{:build, %{card, payment: %{west, east}}}`
  - `{:build_wonder_stage, %{card, payment}}`
  - `{:discard, card}`
  - `{:build_free, card}`
  - `{:build_from_discard, card}`

  Resources are the atoms `:wood … :papyrus`.
- The "option index" in GameLive's `submit` event resolves as follows:
  - For `{:trade, options}`, use `Enum.at(options, index).payment`.
  - For `:free` and `{:coins, _}`, use `%{west: [], east: []}`.
- Game over is `view.phase.kind == :game_over`. `view.scores` is then the final score list, sorted by rank.
- `Helios.Core.game_settings().engine_version` is `1`. Replays across a version bump may fail with any action error; the GameServer marks the game aborted in that case (Phase 4 spec).
