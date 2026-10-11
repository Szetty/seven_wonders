# Table Polish — Phase D: Wonder Selection Modes (core) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The engine supports two new wonder selection modes — `ChooseBoard` (each player claims a board and side, first claim wins) and `ChooseSide` (boards drawn from the seed, players pick the side) — as a public `Phase::Setup` that ends when every seat has confirmed. The NIF exposes the setup modes, actions, errors and view with exactly the terms fixed in the execution guide. `Random` / `Explicit` games behave and replay exactly as today.

**Architecture:** `Game::new` still validates, assigns wonders and generates the deck first, in every mode, so the RNG stream is fixed before any player choice. Building the table (`GameState::new` + `state.init()` + `deal_age(1)`) moves into one `Game::finish_setup(deck, assignment)` step: `Random` / `Explicit` call it at once; the setup modes park the deck in a new `setup: Option<WonderSetup>` field (mode, one `SeatClaim` per seat, deck) and set `phase = Phase::Setup`. `ClaimWonder` / `ConfirmWonder` are validated and applied by a new `game/wonder_setup.rs` (not via `pending`); the last confirmation calls `finish_setup` with the claims in seat order. During setup the view carries `setup: Some(SetupView)` and empty `players` / `hand`.

**Representation (decided here):** `Game.state` stays a plain `GameState` (not `Option<GameState>`). During setup it is an empty placeholder (`GameState::default()`); the deck lives in `WonderSetup.deck` until `finish_setup` replaces the placeholder. Why: `self.state` / `self.player(seat)` are read in ~30 places across `legality.rs`, `resolve.rs`, `scoring.rs`, `view.rs` and `test_support.rs`, all of them reachable only with dealt hands or built players. An `Option` would put an `expect` in every one of those for a state that cannot occur there. Keeping the placeholder means guarding only the three entry points that can run during setup: `submit` (goes to `submit_setup`), `check_action` (returns `ActionNotAllowedNow`), and `build_view` (skips `players`, gets an empty hand from the existing `acting_with_hand` match). `debug_json` serialises the empty default without reading players. Tests call the view, `validate` and every card action at every setup step (Tasks 2 and 4), so any read we missed shows up as a panic.

**Tech Stack:** Rust 2021 + Rustler 0.38 (`NifMap` / `NifTaggedEnum` / `NifUnitEnum`), `cargo test`.

**Spec:** `docs/superpowers/specs/2026-10-11-table-polish/2026-10-11-wonder-modes-design.md` (sections "Engine (`core`)", "NIF (`core/src/nif/`)", "Testing → Core"). Interface contract: `docs/superpowers/plans/2026-10-11-table-polish/README.md` section "D → E (NIF terms)".

## Global Constraints

- **Determinism / replay contract:** RNG consumption for `Random` and `Explicit` is unchanged. `seed_42_deal_is_pinned` in `core/src/tests/game_setup.rs` must stay green **without editing it** (do not touch `PINNED_DEAL_SEED_42`). `ChooseSide` draws exactly `Random`'s step 1 minus the side draws. `ChooseBoard` draws nothing for wonders. `generate_deck` runs in `Game::new` in every mode.
- **`ENGINE_VERSION` stays `1`.** No stored game can change on replay: `Random` / `Explicit` setup and every resolution rule are byte-for-byte the same (the pinned deal and the unchanged simulation tests prove it); `ChooseBoard` / `ChooseSide` games and the `claim_wonder` / `confirm_wonder` actions don't exist in any stored log yet.
- No `HashMap` iteration in engine code. `WONDERS_BY_NAME` is used only for `get` lookups; `available` iterates the `WONDERS` array (fixed order: Rhódos, Alexandria, Éphesos, Babylon, Olympía, Halikarnassós, Gizah).
- The `game` module stays free of rustler imports; all DTOs live in `core/src/nif/dto.rs`.
- Setup actions produce no `HistoryEvent` and never touch `pending`.
- NIF terms must match the README contract exactly (atoms `:choose_board`, `:choose_side`, `:setup`, `:claim_wonder`, `:confirm_wonder`, `:unknown_wonder`, `:wonder_taken`, `:not_your_wonder`, `:no_claim`, `:already_confirmed`; view field `setup`).
- This phase touches **core only**. `helios/lib/helios/core.ex` `@type` docs (`wonders`, `action`, `action_error`) are left to Phase E: changing them here would make Phase D a helios change too (with `mix precommit`), and Phase E owns every Helios caller of these terms. Phase E must update those three types.
- Stage explicit paths only; never `git add -A` / `git add .`. No push.
- Core checks before each commit: `cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test`.

## Review Focus

- Replay compatibility: `seed_42_deal_is_pinned` is unchanged and green; `ChooseSide` draws the same boards as `Random` for the same seed (3–7 players, several seeds); a `ChooseBoard` game whose claims equal an `Explicit` selection gives identical views for every player once setup ends. (Tests in Task 2: `game_setup.rs`, `the_last_confirmation_starts_age_one`.)
- Board ownership: switching a claim releases the old board (another seat can then claim it), a confirmed board stays taken (`WonderTaken`), and a rejected claim changes nothing. (Tests in Task 2: `game_wonder_setup.rs`.)
- The placeholder `GameState` is never read during setup: the view for every seat, `debug_json`, `validate` and every card action (→ `ActionNotAllowedNow`) return without panicking. (Tests in Task 2 `the_choose_board_view_follows_the_contract` / `card_actions_are_not_allowed_during_setup`, plus the Task 4 simulation, which calls `view` + `validate` at every setup step.)
- NIF view terms: `setup: nil` outside setup; an unclaimed seat has `wonder: nil, side: nil`; `available` keeps `WONDERS` order without held boards; `:choose_side` has `available: []`; `submitted` lists the confirmed flags in seat order. (Tests in Task 3: `a_setup_view_converts`, `a_whole_view_converts`.)
- Determinism of the new modes: a full simulated `ChooseBoard` / `ChooseSide` game replayed from (seed, action log) gives identical final views and history, and its first history event is Age I turn 1 (setup adds none). (Tests in Task 4: `setup_mode_games_finish_and_replay_identically`, `a_choose_board_game_is_deterministic`.)

## Resolved ambiguities (read before implementing)

1. **Spec test "deck identical across `Random` / `ChooseSide` / `ChooseBoard`".** This cannot hold under the spec's own RNG contract. `Random` makes `n` side draws before `generate_deck`, `ChooseSide` makes none, and `ChooseBoard` doesn't shuffle wonders at all, so each mode starts the deck shuffles at a different point in the stream. The plan tests the properties that do hold and that the spec's goal ("player choices never influence the deal") needs:
   - `ChooseBoard` with claims X deals exactly like `Explicit(X)` (neither mode draws for wonders).
   - Within one mode and seed, the hands don't depend on which boards or sides were chosen.
   - `ChooseSide` draws the same boards as `Random`.
2. **Precedence for setup actions outside setup.** The usual `precheck` errors still come first for every action, setup actions included: `UnknownPlayer`, `GameOver`, and `NotYourTurn` for a non-acting player during an extra turn. Otherwise a setup action outside setup returns `ActionNotAllowedNow`, as the README says. During `Setup`, an unknown player gets `UnknownPlayer`, and every card action gets `ActionNotAllowedNow`.
3. **Claiming after confirming** returns `AlreadyConfirmed`. The spec only says claims need "player not confirmed".
4. **Error order for a claim:** `AlreadyConfirmed` → `UnknownWonder` → (`ChooseSide`) `NotYourWonder` / (`ChooseBoard`) `WonderTaken`. Re-claiming your own board, with either side, is allowed.

---

### Task 1: Shared `finish_setup` (refactor, no behaviour change)

**Files:**
- Modify: `core/src/game/setup.rs`

**Interfaces:**
- Produces: `pub(super) type WonderRef = &'static Wonder<'static, Effect>` (in `setup.rs`), `Game::finish_setup(&mut self, deck: Deck, assignment: &[(WonderRef, Side)])` (builds `GameState`, runs `init`, fills `self.wonders`, deals Age I), and `fn draw_boards(count, rng) -> Vec<WonderRef>` (shuffle + truncate, step 1 of `Random`).

This is a pure refactor, so there is no new failing test. The safety net is the existing suite, mainly `seed_42_deal_is_pinned`, `same_seed_same_deal_and_different_seed_different_deal`, `every_player_starts_with_seven_cards_and_three_coins` and the simulation tests.

- [ ] **Step 1: Confirm the baseline is green**

Run: `cd core && cargo test`
Expected: PASS.

- [ ] **Step 2: Rewrite `core/src/game/setup.rs`**

Replace the whole file with:

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
use crate::domain::{Age, Deck, Effect, GameState, Player, PlayersWithWonders, Wonder};
use crate::engine::data::{WONDERS, WONDERS_BY_NAME};
use crate::engine::deck::generate_deck;
use crate::engine::rng::GameRng;
use std::collections::{BTreeSet, VecDeque};

pub(super) type WonderRef = &'static Wonder<'static, Effect>;

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
    let mut game = Game {
        state: GameState::default(),
        seats: players,
        wonders: Vec::new(),
        hands: vec![Vec::new(); count],
        built: vec![Vec::new(); count],
        age: 1,
        turn: 1,
        phase: Phase::ChoosingCards { age: 1, turn: 1 },
        pending: vec![None; count],
        extra_turns: VecDeque::new(),
        history: Vec::new(),
    };
    game.finish_setup(deck, &assignment);
    Ok(game)
}

fn assign_wonders(
    count: usize,
    selection: WonderSelection,
    rng: &mut GameRng,
) -> Result<Vec<(WonderRef, Side)>, SetupError> {
    match selection {
        WonderSelection::Random => Ok(draw_boards(count, rng)
            .into_iter()
            .map(|wonder| {
                let side = if rng.below(2) == 0 { Side::A } else { Side::B };
                (wonder, side)
            })
            .collect()),
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

/// Shuffles the 7 wonders (in `WONDERS` order) and keeps the first `count`.
fn draw_boards(count: usize, rng: &mut GameRng) -> Vec<WonderRef> {
    let mut pool: Vec<WonderRef> = WONDERS.iter().collect();
    rng.shuffle(&mut pool);
    pool.truncate(count);
    pool
}

impl Game {
    /// Builds the table from the final wonder assignment (seat order): player
    /// states with starting coins and resources, then the Age I deal.
    pub(super) fn finish_setup(&mut self, deck: Deck, assignment: &[(WonderRef, Side)]) {
        let players_with_wonders: PlayersWithWonders = self
            .seats
            .iter()
            .zip(assignment)
            .map(|(player, &(wonder, side))| {
                let wonder_side = match side {
                    Side::A => &wonder.1,
                    Side::B => &wonder.2,
                };
                (Player(player.clone()), wonder_side)
            })
            .collect();
        self.state = GameState::new(deck, players_with_wonders);
        self.state.init();
        self.wonders = assignment
            .iter()
            .map(|(wonder, side)| (wonder.0.to_string(), *side))
            .collect();
        self.deal_age(1);
    }

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

The order of operations is the same as before (`assign_wonders` → `generate_deck` → `GameState::new` → `init` → `deal_age(1)`). The only difference is that `GameState` is built after the `Game` literal rather than before it.

- [ ] **Step 3: Run the core checks**

Run: `cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test`
Expected: PASS, with `seed_42_deal_is_pinned` green and no test edited.

- [ ] **Step 4: Commit**

```bash
git add core/src/game/setup.rs
git commit -m "refactor(core): build the table in a shared finish_setup step" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 2: `Phase::Setup` — modes, claims, confirmation, view

**Files:**
- Create: `core/src/game/wonder_setup.rs`, `core/src/tests/game_wonder_setup.rs`
- Modify: `core/src/game/types.rs`, `core/src/game/setup.rs`, `core/src/game/mod.rs`, `core/src/game/legality.rs`, `core/src/game/resolve.rs`, `core/src/game/view.rs`, `core/src/game/test_support.rs`, `core/src/nif/dto.rs` (the match arms the new variants force: `ActionDto`, `ActionErrorDto`, `PhaseKindDto`), `core/src/lib.rs`
- Test: `core/src/tests/game_wonder_setup.rs` (new), `core/src/tests/game_setup.rs`, `core/src/tests/game_helpers.rs`, `core/src/tests/game_simulation.rs` (exhaustive `Phase` match), `core/src/tests/nif_dto.rs`

**Interfaces:**
- Consumes: `Game::finish_setup`, `WonderRef` (Task 1).
- Produces (Rust, re-exported from `crate::game`):

```rust
pub enum WonderSelection { Random, Explicit(Vec<(String, Side)>), ChooseBoard, ChooseSide }
pub enum Phase { Setup, ChoosingCards { age: u8, turn: u8 }, ExtraTurn { .. }, GameOver { .. } }
pub enum Action { /* existing */ ClaimWonder { wonder: String, side: Side }, ConfirmWonder }
pub enum ActionError { /* existing */ UnknownWonder, WonderTaken, NotYourWonder, NoClaim, AlreadyConfirmed }
pub enum PhaseKind { Setup, ChoosingCards, ExtraTurn, GameOver }
pub enum SetupMode { ChooseBoard, ChooseSide }
pub struct SetupSeat { pub player: String, pub wonder: Option<String>, pub side: Option<Side>, pub confirmed: bool }
pub struct SetupView { pub mode: SetupMode, pub seats: Vec<SetupSeat>, pub available: Vec<String> }
// PlayerView gains: pub setup: Option<SetupView>
```

- Behaviour (matching the README contract): during `Setup`, `phase` is `PhaseView { kind: Setup, age: 1, turn: 1, direction: West, extra_turn_player: None, extra_turn_kind: None }`; `players`, `hand` are empty; `discard_pile: None`, `discard_count: 0`, `my_pending: None`, `scores: None`; `west`/`east` are the neighbour seat ids; `submitted` = `(player, confirmed)` in seat order. Outside setup, `setup: None`. Test helpers `claim(wonder, side) -> Action` and `complete_wonder_setup(&mut Game, &[(&str, Side)])` live in `game_helpers.rs`.
- NIF (compile-forced, round-trip tested here): `ActionDto::{ClaimWonder { wonder, side: SideDto }, ConfirmWonder}`, five new `ActionErrorDto` variants, `PhaseKindDto::Setup`.

- [ ] **Step 1: Write the failing tests**

Add to `core/src/tests/game_helpers.rs`:

```rust
pub fn claim(wonder: &str, side: Side) -> Action {
    Action::ClaimWonder {
        wonder: wonder.to_string(),
        side,
    }
}

/// Every seat claims its entry of `wonders` (seat order), then all confirm.
pub fn complete_wonder_setup(game: &mut Game, wonders: &[(&str, Side)]) {
    let players = game.players().to_vec();
    assert_eq!(players.len(), wonders.len());
    for (player, (wonder, side)) in players.iter().zip(wonders) {
        game.submit(player, claim(wonder, *side))
            .expect("the claim is legal");
    }
    for player in &players {
        game.submit(player, Action::ConfirmWonder)
            .expect("the confirmation is legal");
    }
}
```

Create `core/src/tests/game_wonder_setup.rs`:

```rust
use super::game_helpers::{build, claim, discard, names, plain_game, stage, PLAYERS};
use crate::game::{
    Action, ActionError, Game, PassDirection, Phase, PhaseKind, SetupMode, SetupSeat, Side,
    WonderSelection,
};

/// `WONDERS` order.
const ALL_WONDERS: [&str; 7] = [
    "Rhódos",
    "Alexandria",
    "Éphesos",
    "Babylon",
    "Olympía",
    "Halikarnassós",
    "Gizah",
];

fn choose_board() -> Game {
    Game::new(names(3), WonderSelection::ChooseBoard, 42).unwrap()
}

/// Seed 42 draws the same boards as the pinned `Random` deal:
/// p1 Alexandria, p2 Gizah, p3 Babylon.
fn choose_side() -> Game {
    Game::new(names(3), WonderSelection::ChooseSide, 42).unwrap()
}

fn seat(player: &str, wonder: Option<&str>, side: Option<Side>, confirmed: bool) -> SetupSeat {
    SetupSeat {
        player: player.to_string(),
        wonder: wonder.map(str::to_string),
        side,
        confirmed,
    }
}

fn seats(game: &Game) -> Vec<SetupSeat> {
    game.view("p1").unwrap().setup.expect("game is in setup").seats
}

fn available(game: &Game) -> Vec<String> {
    game.view("p1")
        .unwrap()
        .setup
        .expect("game is in setup")
        .available
}

#[test]
fn the_choose_board_view_follows_the_contract() {
    let game = choose_board();
    assert_eq!(game.phase(), &Phase::Setup);
    for player in game.players() {
        assert!(game.view(player).is_ok(), "view for {player} must not panic");
    }
    assert!(game.debug_json().contains("\"Setup\""));
    assert!(game.history().is_empty());

    let view = game.view("p2").unwrap();
    assert_eq!(view.me, "p2");
    assert_eq!(view.phase.kind, PhaseKind::Setup);
    assert_eq!((view.phase.age, view.phase.turn), (1, 1));
    assert_eq!(view.phase.direction, PassDirection::West);
    assert_eq!(view.phase.extra_turn_player, None);
    assert_eq!(view.phase.extra_turn_kind, None);
    assert!(view.players.is_empty());
    assert!(view.hand.is_empty());
    assert_eq!(view.discard_pile, None);
    assert_eq!(view.discard_count, 0);
    assert_eq!(view.my_pending, None);
    assert_eq!(view.scores, None);
    assert_eq!((view.west.as_str(), view.east.as_str()), ("p1", "p3"));
    assert_eq!(
        view.submitted,
        vec![
            ("p1".to_string(), false),
            ("p2".to_string(), false),
            ("p3".to_string(), false)
        ]
    );
    let setup = view.setup.expect("setup view");
    assert_eq!(setup.mode, SetupMode::ChooseBoard);
    assert_eq!(
        setup.seats,
        vec![
            seat("p1", None, None, false),
            seat("p2", None, None, false),
            seat("p3", None, None, false),
        ]
    );
    assert_eq!(setup.available, ALL_WONDERS.map(String::from).to_vec());
}

#[test]
fn choose_side_seats_start_with_their_drawn_board() {
    let game = choose_side();
    let setup = game.view("p3").unwrap().setup.expect("setup view");
    assert_eq!(setup.mode, SetupMode::ChooseSide);
    assert!(setup.available.is_empty());
    assert_eq!(
        setup.seats,
        vec![
            seat("p1", Some("Alexandria"), None, false),
            seat("p2", Some("Gizah"), None, false),
            seat("p3", Some("Babylon"), None, false),
        ]
    );
}

#[test]
fn card_actions_are_not_allowed_during_setup() {
    for mut game in [choose_board(), choose_side()] {
        for action in [
            build("Altar"),
            stage("Altar"),
            discard("Altar"),
            Action::BuildFree {
                card: "Altar".to_string(),
            },
            Action::BuildFromDiscard {
                card: "Altar".to_string(),
            },
        ] {
            assert_eq!(game.validate("p1", &action), Err(ActionError::ActionNotAllowedNow));
            assert_eq!(
                game.submit("p1", action),
                Err(ActionError::ActionNotAllowedNow)
            );
        }
        assert_eq!(
            game.submit("nobody", claim("Gizah", Side::A)),
            Err(ActionError::UnknownPlayer)
        );
        assert_eq!(game.phase(), &Phase::Setup);
    }
}

#[test]
fn a_claim_reserves_the_board_and_can_be_flipped_or_switched() {
    let mut game = choose_board();
    game.submit("p1", claim("Gizah", Side::A)).unwrap();
    assert_eq!(seats(&game)[0], seat("p1", Some("Gizah"), Some(Side::A), false));
    assert_eq!(available(&game).len(), 6);
    assert!(!available(&game).contains(&"Gizah".to_string()));

    game.submit("p1", claim("Gizah", Side::B)).unwrap();
    assert_eq!(seats(&game)[0], seat("p1", Some("Gizah"), Some(Side::B), false));

    game.submit("p1", claim("Babylon", Side::A)).unwrap();
    assert_eq!(seats(&game)[0], seat("p1", Some("Babylon"), Some(Side::A), false));
    assert!(available(&game).contains(&"Gizah".to_string()));
    assert!(!available(&game).contains(&"Babylon".to_string()));

    game.submit("p2", claim("Gizah", Side::A)).unwrap();
    assert_eq!(seats(&game)[1], seat("p2", Some("Gizah"), Some(Side::A), false));
    assert_eq!(game.pending_action("p1"), None);
    assert!(game.history().is_empty());
}

#[test]
fn a_held_board_cannot_be_claimed_by_another_seat() {
    let mut game = choose_board();
    game.submit("p1", claim("Gizah", Side::A)).unwrap();
    assert_eq!(
        game.submit("p2", claim("Gizah", Side::B)),
        Err(ActionError::WonderTaken)
    );
    assert_eq!(seats(&game)[1], seat("p2", None, None, false));

    game.submit("p1", Action::ConfirmWonder).unwrap();
    assert_eq!(
        game.submit("p3", claim("Gizah", Side::A)),
        Err(ActionError::WonderTaken)
    );
    assert_eq!(seats(&game)[2], seat("p3", None, None, false));
}

#[test]
fn unknown_boards_are_rejected() {
    for mut game in [choose_board(), choose_side()] {
        for name in ["Atlantis", "Rhodos", ""] {
            assert_eq!(
                game.submit("p1", claim(name, Side::A)),
                Err(ActionError::UnknownWonder)
            );
        }
        assert_eq!(seats(&game)[0].side, None);
    }
}

#[test]
fn choose_side_only_accepts_the_drawn_board() {
    let mut game = choose_side();
    assert_eq!(
        game.submit("p1", claim("Gizah", Side::A)),
        Err(ActionError::NotYourWonder)
    );
    assert_eq!(
        game.submit("p1", claim("Rhódos", Side::A)),
        Err(ActionError::NotYourWonder)
    );
    game.submit("p1", claim("Alexandria", Side::B)).unwrap();
    game.submit("p1", claim("Alexandria", Side::A)).unwrap();
    assert_eq!(
        seats(&game)[0],
        seat("p1", Some("Alexandria"), Some(Side::A), false)
    );
}

#[test]
fn confirming_needs_a_claim_and_happens_once() {
    for mut game in [choose_board(), choose_side()] {
        assert_eq!(
            game.submit("p1", Action::ConfirmWonder),
            Err(ActionError::NoClaim)
        );
    }
    let mut game = choose_board();
    game.submit("p1", claim("Gizah", Side::A)).unwrap();
    game.submit("p1", Action::ConfirmWonder).unwrap();
    assert_eq!(
        game.submit("p1", Action::ConfirmWonder),
        Err(ActionError::AlreadyConfirmed)
    );
    assert_eq!(
        game.submit("p1", claim("Babylon", Side::A)),
        Err(ActionError::AlreadyConfirmed)
    );
    assert_eq!(seats(&game)[0], seat("p1", Some("Gizah"), Some(Side::A), true));
    assert_eq!(
        game.view("p2").unwrap().submitted,
        vec![
            ("p1".to_string(), true),
            ("p2".to_string(), false),
            ("p3".to_string(), false)
        ]
    );
}

#[test]
fn the_last_confirmation_starts_age_one() {
    let boards = [("Babylon", Side::B), ("Gizah", Side::A), ("Rhódos", Side::A)];
    let mut game = choose_board();
    for (player, (wonder, side)) in PLAYERS.iter().zip(boards) {
        game.submit(player, claim(wonder, side)).unwrap();
    }
    game.submit("p1", Action::ConfirmWonder).unwrap();
    game.submit("p2", Action::ConfirmWonder).unwrap();
    assert_eq!(game.phase(), &Phase::Setup);
    assert!(game.hand_names("p1").is_empty());

    game.submit("p3", Action::ConfirmWonder).unwrap();
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 1 });
    assert!(game.history().is_empty());
    for player in game.players() {
        assert_eq!(game.hand_names(player).len(), 7);
        assert_eq!(game.coins(player), 3);
    }
    assert_eq!(game.wonder_of("p1"), ("Babylon".to_string(), Side::B));
    assert_eq!(game.wonder_of("p3"), ("Rhódos".to_string(), Side::A));

    // Same seed, same boards, no wonder draws in either mode: the table is
    // identical to an explicit game (starting resources, coins, hands).
    let explicit = Game::new(
        names(3),
        WonderSelection::Explicit(
            boards
                .iter()
                .map(|(wonder, side)| (wonder.to_string(), *side))
                .collect(),
        ),
        42,
    )
    .unwrap();
    for player in game.players() {
        let view = game.view(player).unwrap();
        assert_eq!(view.setup, None);
        assert_eq!(view, explicit.view(player).unwrap());
    }
}

#[test]
fn choose_side_finishes_with_the_chosen_sides() {
    let mut game = choose_side();
    game.submit("p1", claim("Alexandria", Side::B)).unwrap();
    game.submit("p2", claim("Gizah", Side::A)).unwrap();
    game.submit("p3", claim("Babylon", Side::B)).unwrap();
    for player in PLAYERS {
        game.submit(player, Action::ConfirmWonder).unwrap();
    }
    assert_eq!(game.phase(), &Phase::ChoosingCards { age: 1, turn: 1 });
    assert_eq!(game.wonder_of("p1"), ("Alexandria".to_string(), Side::B));
    assert_eq!(game.wonder_of("p2"), ("Gizah".to_string(), Side::A));
    assert_eq!(game.wonder_of("p3"), ("Babylon".to_string(), Side::B));
    let view = game.view("p1").unwrap();
    assert_eq!(view.phase.kind, PhaseKind::ChoosingCards);
    assert_eq!(view.players.len(), 3);
    assert_eq!(view.hand.len(), 7);
    assert!(game.history().is_empty());
}

#[test]
fn setup_actions_are_not_allowed_outside_setup() {
    let mut game = plain_game();
    assert_eq!(
        game.submit("p1", claim("Gizah", Side::A)),
        Err(ActionError::ActionNotAllowedNow)
    );
    assert_eq!(
        game.submit("p1", Action::ConfirmWonder),
        Err(ActionError::ActionNotAllowedNow)
    );
    assert_eq!(game.pending_action("p1"), None);
    assert_eq!(game.view("p1").unwrap().setup, None);
}
```

Register it in `core/src/lib.rs` (keep alphabetical order, after `game_view`):

```rust
    pub mod game_view;
    pub mod game_wonder_setup;
```

In `core/src/tests/game_setup.rs`, change the first import line to

```rust
use super::game_helpers::{complete_wonder_setup, game_with, names, plain_game};
```

and append (leave every existing test, `PINNED_DEAL_SEED_42` included, untouched):

```rust
fn drawn_boards(game: &Game) -> Vec<String> {
    game.view("p1")
        .unwrap()
        .setup
        .expect("game is in setup")
        .seats
        .into_iter()
        .map(|seat| seat.wonder.expect("choose_side seats start with a board"))
        .collect()
}

fn hands(game: &Game) -> Vec<Vec<String>> {
    game.players()
        .iter()
        .map(|player| game.hand_names(player))
        .collect()
}

#[test]
fn setup_modes_validate_players_and_open_on_setup() {
    for selection in [WonderSelection::ChooseBoard, WonderSelection::ChooseSide] {
        assert_eq!(
            Game::new(names(2), selection.clone(), 1).err(),
            Some(SetupError::InvalidPlayersNumber(2))
        );
        let players = vec!["a".to_string(), "b".to_string(), "a".to_string()];
        assert_eq!(
            Game::new(players, selection.clone(), 1).err(),
            Some(SetupError::DuplicatePlayer("a".to_string()))
        );
        let game = Game::new(names(7), selection, 1).unwrap();
        assert_eq!(game.phase(), &Phase::Setup);
        assert!(game
            .players()
            .iter()
            .all(|player| game.hand_names(player).is_empty()));
    }
}

#[test]
fn choose_side_draws_the_same_boards_as_random() {
    for players in 3..=7 {
        for seed in [0, 1, 42, 99, u64::MAX] {
            let random = Game::new(names(players), WonderSelection::Random, seed).unwrap();
            let expected: Vec<String> = random
                .players()
                .iter()
                .map(|player| random.wonder_of(player).0)
                .collect();
            let choose_side =
                Game::new(names(players), WonderSelection::ChooseSide, seed).unwrap();
            assert_eq!(drawn_boards(&choose_side), expected, "{players}/{seed}");
        }
    }
}

#[test]
fn choose_board_deals_exactly_like_explicit_boards() {
    let boards = [("Babylon", Side::B), ("Gizah", Side::A), ("Olympía", Side::A)];
    for seed in [0, 7, 42] {
        let fixed = Game::new(names(3), explicit(&boards), seed).unwrap();
        let mut chosen = Game::new(names(3), WonderSelection::ChooseBoard, seed).unwrap();
        complete_wonder_setup(&mut chosen, &boards);
        assert_eq!(deal_fingerprint(&chosen), deal_fingerprint(&fixed), "seed {seed}");
    }
}

#[test]
fn setup_choices_never_change_the_deal() {
    let mut a = Game::new(names(3), WonderSelection::ChooseBoard, 5).unwrap();
    complete_wonder_setup(
        &mut a,
        &[("Gizah", Side::A), ("Babylon", Side::A), ("Rhódos", Side::B)],
    );
    let mut b = Game::new(names(3), WonderSelection::ChooseBoard, 5).unwrap();
    complete_wonder_setup(
        &mut b,
        &[
            ("Éphesos", Side::B),
            ("Alexandria", Side::A),
            ("Halikarnassós", Side::B),
        ],
    );
    assert_eq!(hands(&a), hands(&b));

    let drawn = drawn_boards(&Game::new(names(3), WonderSelection::ChooseSide, 5).unwrap());
    let with_side = |side: Side| {
        let mut game = Game::new(names(3), WonderSelection::ChooseSide, 5).unwrap();
        let boards: Vec<(&str, Side)> = drawn.iter().map(|name| (name.as_str(), side)).collect();
        complete_wonder_setup(&mut game, &boards);
        game
    };
    let all_a = with_side(Side::A);
    let all_b = with_side(Side::B);
    assert_eq!(hands(&all_a), hands(&all_b));
    assert_eq!(all_b.wonder_of("p1"), (drawn[0].clone(), Side::B));
}
```

In `core/src/tests/nif_dto.rs`, add `PhaseKind` to the `crate::game` import, add these two pairs to the `pairs` array in `action_dtos_round_trip`:

```rust
        (
            ActionDto::ClaimWonder {
                wonder: "Gizah".to_string(),
                side: SideDto::B,
            },
            Action::ClaimWonder {
                wonder: "Gizah".to_string(),
                side: Side::B,
            },
        ),
        (ActionDto::ConfirmWonder, Action::ConfirmWonder),
```

and append:

```rust
#[test]
fn setup_errors_and_phase_convert() {
    let pairs = [
        (ActionError::UnknownWonder, ActionErrorDto::UnknownWonder),
        (ActionError::WonderTaken, ActionErrorDto::WonderTaken),
        (ActionError::NotYourWonder, ActionErrorDto::NotYourWonder),
        (ActionError::NoClaim, ActionErrorDto::NoClaim),
        (ActionError::AlreadyConfirmed, ActionErrorDto::AlreadyConfirmed),
    ];
    for (error, dto) in pairs {
        assert_eq!(ActionErrorDto::from(error), dto);
    }
    assert_eq!(PhaseKindDto::from(PhaseKind::Setup), PhaseKindDto::Setup);
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd core && cargo test wonder`
Expected: compile errors (`WonderSelection::ChooseBoard`, `Phase::Setup`, `Action::ClaimWonder`, `SetupMode`, `SetupSeat`, `PlayerView::setup`, `ActionDto::ClaimWonder`, … not found).

- [ ] **Step 3: Types (`core/src/game/types.rs`)**

Replace `WonderSelection`:

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WonderSelection {
    Random,
    Explicit(Vec<(String, Side)>),
    /// Each player claims a board and side during `Phase::Setup`.
    ChooseBoard,
    /// Boards are drawn as for `Random`; each player picks the side during `Phase::Setup`.
    ChooseSide,
}
```

Append two variants to `Action` (after `BuildFromDiscard`):

```rust
    /// `Phase::Setup` only: hold `wonder` on `side`, replacing this seat's
    /// earlier claim (which releases its board).
    ClaimWonder {
        wonder: String,
        side: Side,
    },
    /// `Phase::Setup` only: lock the current claim.
    ConfirmWonder,
```

Add a first variant to `Phase`:

```rust
pub enum Phase {
    /// Players claim boards and sides (`ChooseBoard` / `ChooseSide`); Age I
    /// is dealt when the last seat confirms.
    Setup,
    /// Every player chooses a card from their hand.
    ChoosingCards {
```

Append to `ActionError` (after `ActionNotAllowedNow`):

```rust
    /// `ClaimWonder` names no base-game wonder.
    UnknownWonder,
    /// `ChooseBoard`: another seat holds (claimed or confirmed) that board.
    WonderTaken,
    /// `ChooseSide`: not the board drawn for this seat.
    NotYourWonder,
    /// `ConfirmWonder` before any claim.
    NoClaim,
    /// The seat has already confirmed its board.
    AlreadyConfirmed,
```

- [ ] **Step 4: View types (`core/src/game/view.rs`)**

Add `Setup` as the first `PhaseKind` variant:

```rust
pub enum PhaseKind {
    Setup,
    ChoosingCards,
    ExtraTurn,
    GameOver,
}
```

Add after `PhaseView`:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum SetupMode {
    ChooseBoard,
    ChooseSide,
}

/// One seat during `Phase::Setup` (all public).
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SetupSeat {
    pub player: String,
    /// Claimed board; for `ChooseSide` the drawn board from the start.
    pub wonder: Option<String>,
    pub side: Option<Side>,
    pub confirmed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SetupView {
    pub mode: SetupMode,
    /// Seat order.
    pub seats: Vec<SetupSeat>,
    /// `ChooseBoard`: boards nobody holds, in `WONDERS` order. `ChooseSide`: empty.
    pub available: Vec<String>,
}
```

Add a last field to `PlayerView`:

```rust
    pub scores: Option<Vec<FinalScore>>,
    /// `Some` exactly during `Phase::Setup`.
    pub setup: Option<SetupView>,
}
```

Replace `build_view`:

```rust
    fn build_view(&self, seat: usize) -> PlayerView {
        let me = &self.seats[seat];
        let setup = self.setup_view();
        let acting_with_hand = match &self.phase {
            Phase::ChoosingCards { .. } => true,
            Phase::ExtraTurn {
                player,
                kind: ExtraTurnKind::PlayLastCard,
            } => player == me,
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
            Phase::ExtraTurn {
                player,
                kind: ExtraTurnKind::BuildFromDiscard,
            } if player == me => Some(self.buildable_discard_names(seat)),
            _ => None,
        };
        let submitted = match &self.phase {
            Phase::Setup => setup
                .iter()
                .flat_map(|setup| &setup.seats)
                .map(|claim| (claim.player.clone(), claim.confirmed))
                .collect(),
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
        // During setup `state` is a placeholder without player states.
        let players = if setup.is_some() {
            Vec::new()
        } else {
            (0..self.seats.len())
                .map(|s| self.public_player(s))
                .collect()
        };
        PlayerView {
            me: me.clone(),
            phase: self.phase_view(),
            players,
            west: self.seats[self.west_of(seat)].clone(),
            east: self.seats[self.east_of(seat)].clone(),
            hand,
            discard_pile,
            discard_count: self.state.cards_discarded.len(),
            submitted,
            my_pending: self.pending[seat].as_ref().map(|p| p.action.clone()),
            scores,
            setup,
        }
    }
```

In `phase_view`, add the first match arm:

```rust
            Phase::Setup => (PhaseKind::Setup, None, None),
```

(`self.age` / `self.turn` are `1` / `1` from `new_game`, and the direction for age 1 is `West`, as the contract requires.)

- [ ] **Step 5: Setup state and actions (`core/src/game/wonder_setup.rs`)**

```rust
//! `Phase::Setup` for `WonderSelection::ChooseBoard` / `ChooseSide`: each seat
//! claims a board and side (changeable) and then confirms. Everything here is
//! public. The deck generated in `Game::new` waits in [`WonderSetup`] until the
//! last confirmation calls `finish_setup`; until then `Game::state` is an empty
//! placeholder and must not be read.
use super::setup::WonderRef;
use super::types::{Action, ActionError, Side};
use super::view::{SetupMode, SetupSeat, SetupView};
use super::Game;
use crate::domain::Deck;
use crate::engine::data::{WONDERS, WONDERS_BY_NAME};

pub(super) struct WonderSetup {
    pub(super) mode: SetupMode,
    /// One per seat, in seat order.
    pub(super) seats: Vec<SeatClaim>,
    /// Generated in `Game::new`, consumed by `finish_setup`.
    pub(super) deck: Deck,
}

/// A seat's board (claimed, or drawn for `ChooseSide`), side and lock.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(super) struct SeatClaim {
    pub(super) wonder: Option<&'static str>,
    pub(super) side: Option<Side>,
    pub(super) confirmed: bool,
}

/// A validated setup action.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum SetupMove {
    Claim(&'static str, Side),
    Confirm,
}

impl WonderSetup {
    fn check_claim(&self, seat: usize, wonder: &str) -> Result<&'static str, ActionError> {
        if self.seats[seat].confirmed {
            return Err(ActionError::AlreadyConfirmed);
        }
        let name: &'static str = WONDERS_BY_NAME
            .get(wonder)
            .map(|wonder| wonder.0)
            .ok_or(ActionError::UnknownWonder)?;
        match self.mode {
            SetupMode::ChooseSide if self.seats[seat].wonder != Some(name) => {
                Err(ActionError::NotYourWonder)
            }
            SetupMode::ChooseBoard
                if self
                    .seats
                    .iter()
                    .enumerate()
                    .any(|(other, claim)| other != seat && claim.wonder == Some(name)) =>
            {
                Err(ActionError::WonderTaken)
            }
            _ => Ok(name),
        }
    }

    fn check_confirm(&self, seat: usize) -> Result<(), ActionError> {
        let claim = &self.seats[seat];
        if claim.confirmed {
            Err(ActionError::AlreadyConfirmed)
        } else if claim.side.is_none() {
            Err(ActionError::NoClaim)
        } else {
            Ok(())
        }
    }
}

impl Game {
    /// Validates a setup action without applying it (`Phase::Setup` only).
    pub(super) fn check_setup_action(
        &self,
        seat: usize,
        action: &Action,
    ) -> Result<SetupMove, ActionError> {
        let setup = self
            .setup
            .as_ref()
            .expect("Phase::Setup keeps its WonderSetup");
        match action {
            Action::ClaimWonder { wonder, side } => {
                Ok(SetupMove::Claim(setup.check_claim(seat, wonder)?, *side))
            }
            Action::ConfirmWonder => setup.check_confirm(seat).map(|()| SetupMove::Confirm),
            _ => Err(ActionError::ActionNotAllowedNow),
        }
    }

    /// `submit` during `Phase::Setup`. Records no history and leaves `pending`
    /// alone; the last confirmation builds the table and deals Age I.
    pub(super) fn submit_setup(&mut self, seat: usize, action: &Action) -> Result<(), ActionError> {
        let setup_move = self.check_setup_action(seat, action)?;
        let setup = self
            .setup
            .as_mut()
            .expect("Phase::Setup keeps its WonderSetup");
        let claim = &mut setup.seats[seat];
        match setup_move {
            SetupMove::Claim(wonder, side) => {
                claim.wonder = Some(wonder);
                claim.side = Some(side);
            }
            SetupMove::Confirm => claim.confirmed = true,
        }
        if setup.seats.iter().all(|claim| claim.confirmed) {
            self.finish_wonder_setup();
        }
        Ok(())
    }

    fn finish_wonder_setup(&mut self) {
        let setup = self
            .setup
            .take()
            .expect("Phase::Setup keeps its WonderSetup");
        let assignment: Vec<(WonderRef, Side)> = setup
            .seats
            .iter()
            .map(|claim| {
                let name = claim.wonder.expect("a confirmed seat holds a board");
                let wonder = WONDERS_BY_NAME
                    .get(name)
                    .copied()
                    .expect("claims are base wonders");
                (wonder, claim.side.expect("a confirmed seat holds a side"))
            })
            .collect();
        self.finish_setup(setup.deck, &assignment);
    }

    /// Public setup state; `None` outside `Phase::Setup`.
    pub(super) fn setup_view(&self) -> Option<SetupView> {
        let setup = self.setup.as_ref()?;
        let held = |name: &str| {
            setup
                .seats
                .iter()
                .any(|claim| claim.wonder.is_some_and(|held| held == name))
        };
        Some(SetupView {
            mode: setup.mode,
            seats: self
                .seats
                .iter()
                .zip(&setup.seats)
                .map(|(player, claim)| SetupSeat {
                    player: player.clone(),
                    wonder: claim.wonder.map(str::to_string),
                    side: claim.side,
                    confirmed: claim.confirmed,
                })
                .collect(),
            available: match setup.mode {
                SetupMode::ChooseBoard => WONDERS
                    .iter()
                    .map(|wonder| wonder.0)
                    .filter(|name| !held(name))
                    .map(str::to_string)
                    .collect(),
                SetupMode::ChooseSide => Vec::new(),
            },
        })
    }
}
```

(If the compiler rejects `!held(name)` with `name: &&str`, write `!held(*name)`.)

- [ ] **Step 6: Assignment in `core/src/game/setup.rs`**

Update the header comment's step list:

```rust
//! `Game::new`: validation, wonder assignment, deck generation and the first
//! deal (at once, or after `Phase::Setup` for the choosing modes).
//!
//! RNG consumption order (replay contract; changing it requires bumping
//! `ENGINE_VERSION`):
//! 1. `WonderSelection::Random`: shuffle the 7 wonders (in `WONDERS` order),
//!    keep the first `n`, then draw one side per seat in seat order.
//!    `WonderSelection::ChooseSide`: the same shuffle and truncation, no side
//!    draws. `ChooseBoard` / `Explicit`: no wonder draws.
//! 2. `generate_deck`: Age I shuffle, Age II shuffle, guild shuffle, Age III
//!    shuffle, in every mode (so setup choices never influence the deal).
```

Change the imports to:

```rust
use super::types::{Phase, SetupError, Side, WonderSelection};
use super::view::SetupMode;
use super::wonder_setup::{SeatClaim, WonderSetup};
use super::Game;
```

Add after the `WonderRef` alias:

```rust
/// How `Game::new` hands out wonders.
enum Assignment {
    /// Known now (`Random`, `Explicit`): the table is built at once.
    Fixed(Vec<(WonderRef, Side)>),
    /// Chosen during `Phase::Setup`; `ChooseSide` seats start with their drawn board.
    Open(SetupMode, Vec<SeatClaim>),
}
```

In `new_game`, set the initial phase to `Phase::Setup`, add `setup: None,` to the `Game { … }` literal, and replace `game.finish_setup(deck, &assignment);` with:

```rust
    match assignment {
        Assignment::Fixed(assignment) => game.finish_setup(deck, &assignment),
        Assignment::Open(mode, seats) => {
            game.setup = Some(WonderSetup { mode, seats, deck });
        }
    }
```

(`finish_setup` → `deal_age(1)` sets `Phase::ChoosingCards { age: 1, turn: 1 }` for the fixed modes, so the placeholder `Phase::Setup` never leaks.)

Replace `assign_wonders` with:

```rust
fn assign_wonders(
    count: usize,
    selection: WonderSelection,
    rng: &mut GameRng,
) -> Result<Assignment, SetupError> {
    match selection {
        WonderSelection::Random => Ok(Assignment::Fixed(
            draw_boards(count, rng)
                .into_iter()
                .map(|wonder| {
                    let side = if rng.below(2) == 0 { Side::A } else { Side::B };
                    (wonder, side)
                })
                .collect(),
        )),
        WonderSelection::ChooseSide => Ok(Assignment::Open(
            SetupMode::ChooseSide,
            draw_boards(count, rng)
                .into_iter()
                .map(|wonder| SeatClaim {
                    wonder: Some(wonder.0),
                    ..SeatClaim::default()
                })
                .collect(),
        )),
        WonderSelection::ChooseBoard => Ok(Assignment::Open(
            SetupMode::ChooseBoard,
            vec![SeatClaim::default(); count],
        )),
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
            Ok(Assignment::Fixed(assignment))
        }
    }
}
```

- [ ] **Step 7: Wire it into `Game` (`mod.rs`, `legality.rs`, `resolve.rs`, `test_support.rs`)**

`core/src/game/mod.rs`: add `mod wonder_setup;` (after `mod view;`), and extend the view re-export:

```rust
pub use view::{
    BuildOption, BuiltCard, HandCard, PassDirection, PhaseKind, PhaseView, PlayerView, PublicPlayer,
    SetupMode, SetupSeat, SetupView,
};
```

Document the placeholder on `state` and `wonders`, and add the field last in `Game`:

```rust
pub struct Game {
    /// Empty placeholder (`GameState::default()`) during `Phase::Setup`;
    /// built by `finish_setup`.
    state: GameState,
    /// Player ids in seat order.
    seats: Vec<String>,
    /// Wonder name and side per seat (empty during `Phase::Setup`).
    wonders: Vec<(String, Side)>,
```

```rust
    /// Public events, appended as turns and ages resolve.
    history: Vec<history::HistoryEvent>,
    /// `Some` exactly during `Phase::Setup`: claims and the waiting deck.
    setup: Option<wonder_setup::WonderSetup>,
}
```

In `debug_json`, add `"setup": self.setup_view(),` after `"history": self.history,`.

Route setup in `submit`:

```rust
    pub fn submit(&mut self, player: &str, action: Action) -> Result<(), ActionError> {
        let seat = self.precheck(player)?;
        if matches!(self.phase, Phase::Setup) {
            return self.submit_setup(seat, &action);
        }
        let resolved = self.check_action(seat, &action)?;
        self.pending[seat] = Some(Pending { action, resolved });
        if self.all_required_submitted() {
            self.resolve();
        }
        Ok(())
    }
```

`core/src/game/legality.rs` → `check_action`: add the `Setup` arm to the phase match, and a final arm to the action match:

```rust
        let extra_turn = match &self.phase {
            Phase::GameOver { .. } => return Err(ActionError::GameOver),
            // `submit` routes setup to `submit_setup`; nothing below may read
            // the placeholder state.
            Phase::Setup => return Err(ActionError::ActionNotAllowedNow),
            Phase::ChoosingCards { .. } => None,
            Phase::ExtraTurn { kind, .. } => Some(*kind),
        };
```

```rust
            Action::ClaimWonder { .. } | Action::ConfirmWonder => {
                Err(ActionError::ActionNotAllowedNow)
            }
```

`core/src/game/resolve.rs`: in `all_required_submitted` replace `Phase::GameOver { .. } => false,` with `Phase::Setup | Phase::GameOver { .. } => false,`; in `resolve` replace `Phase::GameOver { .. } => return,` with `Phase::Setup | Phase::GameOver { .. } => return,`.

`core/src/game/test_support.rs`: add `Phase` to the `super::types` import and route `validate`:

```rust
    /// The same checks as `submit`, without storing anything.
    pub fn validate(&self, player: &str, action: &Action) -> Result<(), ActionError> {
        let seat = self.precheck(player)?;
        if matches!(self.phase, Phase::Setup) {
            return self.check_setup_action(seat, action).map(|_| ());
        }
        self.check_action(seat, action).map(|_| ())
    }
```

`core/src/tests/game_simulation.rs`: `acting_players` must cover the new variant (it is an exhaustive match):

```rust
fn acting_players(game: &Game) -> Vec<String> {
    match game.phase() {
        Phase::Setup => game
            .view(&game.players()[0])
            .unwrap()
            .setup
            .expect("setup view during Phase::Setup")
            .seats
            .into_iter()
            .filter(|seat| !seat.confirmed)
            .map(|seat| seat.player)
            .collect(),
        Phase::ChoosingCards { .. } => game.players().to_vec(),
        Phase::ExtraTurn { player, .. } => vec![player.clone()],
        Phase::GameOver { .. } => Vec::new(),
    }
}
```

- [ ] **Step 8: Compile-forced DTO arms (`core/src/nif/dto.rs`)**

`ActionDto` (update the doc comment as well):

```rust
/// `{:build, %{card, payment}}` | `{:build_wonder_stage, %{card, payment}}` |
/// `{:discard, card}` | `{:build_free, card}` | `{:build_from_discard, card}` |
/// `{:claim_wonder, %{wonder, side}}` | `:confirm_wonder`
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum ActionDto {
    Build { card: String, payment: PaymentDto },
    BuildWonderStage { card: String, payment: PaymentDto },
    Discard(String),
    BuildFree(String),
    BuildFromDiscard(String),
    ClaimWonder { wonder: String, side: SideDto },
    ConfirmWonder,
}
```

`impl From<ActionDto> for Action`, new arms:

```rust
            ActionDto::ClaimWonder { wonder, side } => Self::ClaimWonder {
                wonder,
                side: side.into(),
            },
            ActionDto::ConfirmWonder => Self::ConfirmWonder,
```

`impl From<Action> for ActionDto`, new arms:

```rust
            Action::ClaimWonder { wonder, side } => Self::ClaimWonder {
                wonder,
                side: side.into(),
            },
            Action::ConfirmWonder => Self::ConfirmWonder,
```

`ActionErrorDto`: append `UnknownWonder, WonderTaken, NotYourWonder, NoClaim, AlreadyConfirmed,` after `ActionNotAllowedNow`, and in its `From`:

```rust
            ActionError::UnknownWonder => Self::UnknownWonder,
            ActionError::WonderTaken => Self::WonderTaken,
            ActionError::NotYourWonder => Self::NotYourWonder,
            ActionError::NoClaim => Self::NoClaim,
            ActionError::AlreadyConfirmed => Self::AlreadyConfirmed,
```

`PhaseKindDto`: add `Setup` as the first variant, and `PhaseKind::Setup => Self::Setup,` to its `From`.

(`PlayerViewDto` ignores `view.setup` until Task 3; that compiles because the conversion reads fields by name.)

- [ ] **Step 9: Run the tests**

Run: `cd core && cargo test game_wonder_setup && cargo test game_setup && cargo test nif_dto`
Expected: PASS. If `choose_side_seats_start_with_their_drawn_board` fails on the board names, the shuffle diverged from `Random`; fix the code, not the test (the names come from the pinned seed-42 deal).

- [ ] **Step 10: Run the core checks**

Run: `cd core && cargo fmt && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test`
Expected: PASS, with `seed_42_deal_is_pinned` and all simulation tests unchanged and green.

- [ ] **Step 11: Commit**

```bash
git add core/src/game/wonder_setup.rs core/src/game/types.rs core/src/game/setup.rs core/src/game/mod.rs core/src/game/legality.rs core/src/game/resolve.rs core/src/game/view.rs core/src/game/test_support.rs core/src/nif/dto.rs core/src/lib.rs core/src/tests/game_wonder_setup.rs core/src/tests/game_setup.rs core/src/tests/game_helpers.rs core/src/tests/game_simulation.rs core/src/tests/nif_dto.rs
git commit -m "feat(core): choose-board and choose-side wonder setup phase" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 3: NIF — wonder modes and the setup view

**Files:**
- Modify: `core/src/nif/dto.rs`
- Test: `core/src/tests/nif_dto.rs`

**Interfaces:**
- Consumes: `WonderSelection::{ChooseBoard, ChooseSide}`, `SetupMode`, `SetupSeat`, `SetupView`, `PlayerView.setup` (Task 2).
- Produces (Elixir terms, README "D → E" contract):
  - `Core.new_game(players, wonders, seed)` accepts `wonders` = `:random | :choose_board | :choose_side | {:explicit, [{name, :a | :b}]}`.
  - Actions `{:claim_wonder, %{wonder: String.t(), side: :a | :b}}` and `:confirm_wonder` (Task 2).
  - Errors `:unknown_wonder | :wonder_taken | :not_your_wonder | :no_claim | :already_confirmed` (Task 2).
  - View field `setup`: `nil` outside setup; during setup `%{mode: :choose_board | :choose_side, seats: [%{player: String.t(), wonder: String.t() | nil, side: :a | :b | nil, confirmed: boolean}], available: [String.t()]}`, with `phase.kind == :setup`.

- [ ] **Step 1: Write the failing tests**

In `core/src/tests/nif_dto.rs`, change the helper import to `use super::game_helpers::{claim, names, plain_game};`, add `Game` to the `crate::game` import, and add `SetupModeDto, SetupSeatDto` to the `crate::nif::dto` import. Add `assert_eq!(dto.setup, None);` at the end of `a_whole_view_converts`, then append:

```rust
#[test]
fn wonder_modes_convert() {
    assert_eq!(
        WonderSelection::from(WondersDto::ChooseBoard),
        WonderSelection::ChooseBoard
    );
    assert_eq!(
        WonderSelection::from(WondersDto::ChooseSide),
        WonderSelection::ChooseSide
    );
}

#[test]
fn a_setup_view_converts() {
    let mut game = Game::new(names(3), WonderSelection::ChooseBoard, 42).unwrap();
    game.submit("p1", claim("Gizah", Side::B)).unwrap();
    game.submit("p1", Action::ConfirmWonder).unwrap();
    let dto = PlayerViewDto::from(game.view("p2").unwrap());
    assert_eq!(dto.phase.kind, PhaseKindDto::Setup);
    assert_eq!((dto.phase.age, dto.phase.turn), (1, 1));
    assert!(dto.players.is_empty() && dto.hand.is_empty());
    assert_eq!(dto.discard_pile, None);
    assert_eq!(dto.discard_count, 0);
    assert_eq!(dto.my_pending, None);
    assert_eq!(dto.scores, None);
    assert_eq!(
        dto.submitted,
        vec![
            ("p1".to_string(), true),
            ("p2".to_string(), false),
            ("p3".to_string(), false)
        ]
    );
    let setup = dto.setup.expect("setup view");
    assert_eq!(setup.mode, SetupModeDto::ChooseBoard);
    assert_eq!(
        setup.seats,
        vec![
            SetupSeatDto {
                player: "p1".to_string(),
                wonder: Some("Gizah".to_string()),
                side: Some(SideDto::B),
                confirmed: true,
            },
            SetupSeatDto {
                player: "p2".to_string(),
                wonder: None,
                side: None,
                confirmed: false,
            },
            SetupSeatDto {
                player: "p3".to_string(),
                wonder: None,
                side: None,
                confirmed: false,
            },
        ]
    );
    assert_eq!(
        setup.available,
        ["Rhódos", "Alexandria", "Éphesos", "Babylon", "Olympía", "Halikarnassós"]
            .map(String::from)
            .to_vec()
    );

    let game = Game::new(names(3), WonderSelection::ChooseSide, 42).unwrap();
    let setup = PlayerViewDto::from(game.view("p1").unwrap())
        .setup
        .expect("setup view");
    assert_eq!(setup.mode, SetupModeDto::ChooseSide);
    assert!(setup.available.is_empty());
    assert_eq!(setup.seats[0].wonder, Some("Alexandria".to_string()));
    assert_eq!(setup.seats[0].side, None);
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd core && cargo test nif_dto`
Expected: compile errors (`WondersDto::ChooseBoard`, `SetupModeDto`, `SetupSeatDto`, `PlayerViewDto::setup` not found).

- [ ] **Step 3: DTOs (`core/src/nif/dto.rs`)**

Add `SetupMode, SetupSeat, SetupView` to the `crate::game` import list.

Replace `WondersDto` and its `From`:

```rust
/// `:random` | `:choose_board` | `:choose_side` | `{:explicit, [{"Gizah", :a}, …]}`
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum WondersDto {
    Random,
    ChooseBoard,
    ChooseSide,
    Explicit(Vec<(String, SideDto)>),
}

impl From<WondersDto> for WonderSelection {
    fn from(wonders: WondersDto) -> Self {
        match wonders {
            WondersDto::Random => Self::Random,
            WondersDto::ChooseBoard => Self::ChooseBoard,
            WondersDto::ChooseSide => Self::ChooseSide,
            WondersDto::Explicit(choices) => Self::Explicit(
                choices
                    .into_iter()
                    .map(|(name, side)| (name, side.into()))
                    .collect(),
            ),
        }
    }
}
```

Add before `PlayerViewDto`:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum SetupModeDto {
    ChooseBoard,
    ChooseSide,
}

impl From<SetupMode> for SetupModeDto {
    fn from(mode: SetupMode) -> Self {
        match mode {
            SetupMode::ChooseBoard => Self::ChooseBoard,
            SetupMode::ChooseSide => Self::ChooseSide,
        }
    }
}

/// `%{player, wonder: name | nil, side: :a | :b | nil, confirmed: boolean}`
#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct SetupSeatDto {
    pub player: String,
    pub wonder: Option<String>,
    pub side: Option<SideDto>,
    pub confirmed: bool,
}

impl From<SetupSeat> for SetupSeatDto {
    fn from(seat: SetupSeat) -> Self {
        Self {
            player: seat.player,
            wonder: seat.wonder,
            side: seat.side.map(Into::into),
            confirmed: seat.confirmed,
        }
    }
}

/// `%{mode: :choose_board | :choose_side, seats: [seat], available: [name]}`
#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct SetupViewDto {
    pub mode: SetupModeDto,
    pub seats: Vec<SetupSeatDto>,
    pub available: Vec<String>,
}

impl From<SetupView> for SetupViewDto {
    fn from(setup: SetupView) -> Self {
        Self {
            mode: setup.mode.into(),
            seats: convert(setup.seats),
            available: setup.available,
        }
    }
}
```

(`convert` is the generic helper already defined further down in `dto.rs`; module-level functions can be used before their definition.)

Add the field to `PlayerViewDto`:

```rust
    pub scores: Option<Vec<FinalScoreDto>>,
    /// `nil` outside `:setup`.
    pub setup: Option<SetupViewDto>,
}
```

and to its `From`:

```rust
            scores: view
                .scores
                .map(|scores| scores.into_iter().map(Into::into).collect()),
            setup: view.setup.map(Into::into),
```

- [ ] **Step 4: Run the tests**

Run: `cd core && cargo test nif_dto`
Expected: PASS.

- [ ] **Step 5: Run the core checks**

Run: `cd core && cargo fmt && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add core/src/nif/dto.rs core/src/tests/nif_dto.rs
git commit -m "feat(core): expose wonder modes and the setup view over the NIF" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 4: Simulated setup-mode games replay identically (phase gate)

**Files:**
- Modify: `core/src/tests/game_simulation.rs`

**Interfaces:**
- Consumes: everything from Tasks 2–3.
- Produces (test-only): `play_random_game_with(players, selection, seed, on_step) -> (Game, Vec<(String, Action)>)`; `available_actions` offers setup actions when `view.setup` is `Some`. `play_random_game(players, seed, on_step)` keeps its signature and its exact behaviour for `Random` (same chooser draws, so existing simulation tests are unchanged).

- [ ] **Step 1: Write the failing tests**

Append to `core/src/tests/game_simulation.rs`:

```rust
fn replay(players: usize, selection: WonderSelection, seed: u64, log: &[(String, Action)]) -> Game {
    let mut game = Game::new(names(players), selection, seed).unwrap();
    for (player, action) in log {
        game.submit(player, action.clone())
            .expect("a logged action replays");
    }
    game
}

fn final_views(game: &Game) -> Vec<PlayerView> {
    game.players()
        .iter()
        .map(|player| game.view(player).unwrap())
        .collect()
}

#[test]
fn setup_mode_games_finish_and_replay_identically() {
    for selection in [WonderSelection::ChooseBoard, WonderSelection::ChooseSide] {
        for players in 3..=7 {
            for seed in 0..5 {
                let (game, log) = play_random_game_with(players, selection.clone(), seed, |_| {});
                assert!(
                    matches!(game.phase(), Phase::GameOver { .. }),
                    "{selection:?} {players}/{seed} did not end"
                );
                assert!(log.iter().any(|(_, action)| *action == Action::ConfirmWonder));
                assert!(
                    matches!(
                        game.history().first(),
                        Some(HistoryEvent::Turn { age: 1, turn: 1, .. })
                    ),
                    "setup must not add history"
                );
                let mut boards: Vec<String> = game
                    .players()
                    .iter()
                    .map(|player| game.wonder_of(player).0)
                    .collect();
                boards.sort();
                boards.dedup();
                assert_eq!(boards.len(), players, "a board was handed out twice");

                let replayed = replay(players, selection.clone(), seed, &log);
                assert_eq!(final_views(&replayed), final_views(&game));
                assert_eq!(replayed.history(), game.history());
            }
        }
    }
}

#[test]
fn a_choose_board_game_is_deterministic() {
    let record = |seed: u64| -> Vec<PlayerView> {
        let mut views = Vec::new();
        play_random_game_with(5, WonderSelection::ChooseBoard, seed, |game| {
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

- [ ] **Step 2: Run to verify they fail**

Run: `cd core && cargo test game_simulation`
Expected: compile error (`play_random_game_with` not found).

- [ ] **Step 3: Drive setup in the simulation**

Change the imports at the top of `core/src/tests/game_simulation.rs` to:

```rust
use super::game_helpers::names;
use crate::engine::rng::GameRng;
use crate::game::{
    Action, BuildOption, Game, Payment, Phase, PlayerView, SetupMode, SetupView, Side,
    WonderSelection,
};
```

Replace `available_actions` with (the card part is unchanged):

```rust
/// Every action the view offers.
pub fn available_actions(view: &PlayerView) -> Vec<Action> {
    if let Some(setup) = &view.setup {
        return setup_actions(view, setup);
    }
    let mut actions = Vec::new();
    if let Some(pile) = &view.discard_pile {
        actions.extend(
            pile.iter()
                .map(|card| Action::BuildFromDiscard { card: card.clone() }),
        );
    }
    for card in &view.hand {
        push_options(&mut actions, &card.build, |payment| Action::Build {
            card: card.name.clone(),
            payment,
        });
        push_options(&mut actions, &card.wonder_stage, |payment| {
            Action::BuildWonderStage {
                card: card.name.clone(),
                payment,
            }
        });
        if card.free_build {
            actions.push(Action::BuildFree {
                card: card.name.clone(),
            });
        }
        actions.push(Action::Discard {
            card: card.name.clone(),
        });
    }
    actions
}

/// Claims of every board the viewer may take (free boards plus their own
/// claim for `ChooseBoard`; the drawn board for `ChooseSide`) on both sides,
/// plus `ConfirmWonder` once they hold a claim.
fn setup_actions(view: &PlayerView, setup: &SetupView) -> Vec<Action> {
    let mine = setup
        .seats
        .iter()
        .find(|seat| seat.player == view.me)
        .expect("the viewer has a seat");
    if mine.confirmed {
        return Vec::new();
    }
    let boards: Vec<String> = match setup.mode {
        SetupMode::ChooseBoard => setup
            .available
            .iter()
            .cloned()
            .chain(mine.wonder.clone())
            .collect(),
        SetupMode::ChooseSide => mine.wonder.iter().cloned().collect(),
    };
    let mut actions: Vec<Action> = boards
        .into_iter()
        .flat_map(|wonder| {
            [Side::A, Side::B].map(|side| Action::ClaimWonder {
                wonder: wonder.clone(),
                side,
            })
        })
        .collect();
    if mine.side.is_some() {
        actions.push(Action::ConfirmWonder);
    }
    actions
}
```

At the top of `assert_invariants`, replace the first lines up to the `in_hands` computation with:

```rust
fn assert_invariants(game: &Game) {
    let players = game.players().len();
    let view = game.view(&game.players()[0]).unwrap();
    if let Some(setup) = &view.setup {
        let mut held: Vec<&String> = setup
            .seats
            .iter()
            .filter_map(|seat| seat.wonder.as_ref())
            .collect();
        let count = held.len();
        held.sort();
        held.dedup();
        assert_eq!(held.len(), count, "a board is held twice");
        return;
    }
    let in_hands: usize = game
```

(the rest of the function is unchanged).

Replace `play_random_game` with:

```rust
/// Plays a full game choosing uniformly among the view's options.
/// `on_step` sees the game before every round of submissions.
/// Coins are `u32`: a negative balance would panic (debug overflow check).
pub fn play_random_game(players: usize, seed: u64, on_step: impl FnMut(&Game)) -> Game {
    play_random_game_with(players, WonderSelection::Random, seed, on_step).0
}

/// As `play_random_game` for any wonder selection; also returns the accepted
/// submissions in order (the action log Helios persists and replays).
pub fn play_random_game_with(
    players: usize,
    selection: WonderSelection,
    seed: u64,
    mut on_step: impl FnMut(&Game),
) -> (Game, Vec<(String, Action)>) {
    let mut game = Game::new(names(players), selection, seed).unwrap();
    let mut chooser = GameRng::new(seed.wrapping_mul(31).wrapping_add(17));
    let mut log = Vec::new();
    for _ in 0..1_000 {
        on_step(&game);
        let acting = acting_players(&game);
        if acting.is_empty() {
            return (game, log);
        }
        assert_invariants(&game);
        for player in acting {
            let view = game.view(&player).unwrap();
            let actions = available_actions(&view);
            assert!(!actions.is_empty(), "{player} has no legal action");
            assert_view_agrees_with_submit(&game, &player, &view, &actions);
            let choice = actions[chooser.below(actions.len())].clone();
            game.submit(&player, choice.clone()).unwrap();
            log.push((player, choice));
        }
    }
    panic!("game did not finish within 1000 rounds");
}
```

Why the setup round loop is sound: `acting` holds the seats that were unconfirmed when the round started, and only a seat can confirm itself, so the confirmation that finishes setup always comes from the last seat in that list. The phase never changes in the middle of a round. Setup rounds (expected about 15 per seat for 7 boards × 2 sides + confirm) fit well within the 1000-round cap.

`HistoryEvent` (used by the new test) is already imported by the existing mid-file `use crate::game::{HistoryEvent, PublicAction};` line.

- [ ] **Step 4: Run the tests**

Run: `cd core && cargo test game_simulation`
Expected: PASS: the new tests plus the unchanged `random_games_finish_for_every_player_count`, `identical_inputs_produce_identical_views`, `history_agrees_with_the_final_table` and `identical_inputs_produce_identical_history`.

- [ ] **Step 5: Phase gate**

Run: `cd core && cargo fmt && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test`
Expected: PASS.

Then confirm (without committing anything in `helios/`) that the extended view and new variants don't break existing Helios callers, and that the real Elixir terms match the README contract:

```bash
cd helios && mix test
mix run --no-start -e '
{:ok, g} = Helios.Core.new_game(["a", "b", "c"], :choose_board, 42)
:ok = Helios.Core.submit(g, "a", {:claim_wonder, %{wonder: "Gizah", side: :b}})
{:error, :wonder_taken} = Helios.Core.submit(g, "b", {:claim_wonder, %{wonder: "Gizah", side: :a}})
{:error, :action_not_allowed_now} = Helios.Core.submit(g, "b", {:discard, "Altar"})
{:error, :no_claim} = Helios.Core.submit(g, "b", :confirm_wonder)
{:ok, v} = Helios.Core.view(g, "b")
IO.inspect({v.phase, v.setup, v.submitted, v.players, v.hand, v.west, v.east})
{:ok, _} = Helios.Core.new_game(["a", "b", "c"], :choose_side, 42)
'
```

Expected: `mix test` green. The inspect prints the equivalent of (map key order may differ):

```elixir
{%{kind: :setup, age: 1, turn: 1, direction: :west, extra_turn_player: nil, extra_turn_kind: nil},
 %{mode: :choose_board,
   seats: [%{player: "a", wonder: "Gizah", side: :b, confirmed: false},
           %{player: "b", wonder: nil, side: nil, confirmed: false},
           %{player: "c", wonder: nil, side: nil, confirmed: false}],
   available: ["Rhódos", "Alexandria", "Éphesos", "Babylon", "Olympía", "Halikarnassós"]},
 [{"a", false}, {"b", false}, {"c", false}], [], [], "a", "c"}
```

If any term differs from the README "D → E (NIF terms)" section, stop and ask: the contract is fixed and Phase E builds on it.

- [ ] **Step 6: Commit**

```bash
git add core/src/tests/game_simulation.rs
git commit -m "test(core): simulate and replay choose-board and choose-side games" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

Phase D is done when all four commits are in and the phase gate is green. Phase E picks up from the README contract; it must also update the `@type wonders`, `@type action` and `@type action_error` docs in `helios/lib/helios/core.ex`, which this phase leaves unchanged on purpose.
