# Game Table UX — Phase D: History, Motion and Replay Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The engine records the public events of every turn and age; players read them in a history drawer, see each turn happen (cards fly to boards, coins float), see the military conflicts at the end of each age, and can step through a finished game turn by turn.

**Architecture:** `Game.history: Vec<HistoryEvent>` is appended during turn resolution and battles (rebuilt for free by replay; nothing new persisted), exposed by a `history` NIF with no player argument (public data only). GameLive diffs the history on every update to push `turn_resolved` events to a colocated JS hook and to open the end-of-age summary. Finished games are replayed once into ~20–25 frames (public boards + the viewer's own hand and choice) held in LiveView assigns.

**Tech Stack:** Rust 2021 + Rustler 0.38, Phoenix 1.8 / LiveView 1.1 colocated hooks, Web Animations API, Tailwind 4.1, ExUnit + LazyHTML, Playwright.

**Spec:** `docs/superpowers/specs/2026-10-04-game-table-ux/2026-10-04-game-table-ux-design.md` (section "Phase D — History and motion").

## Global Constraints

- Read `helios/AGENTS.md` before touching `helios/` (colocated hooks: `<script :type={Phoenix.LiveView.ColocatedHook} name=".Name">`, names start with `.`, hooks that manage their own DOM need `phx-update="ignore"`; never inline `<script>` otherwise).
- **Determinism:** history must not change any outcome; `ENGINE_VERSION` stays `1`; no `HashMap` iteration in engine code that builds history (seat order only).
- **Hidden information:** history holds only public facts — discards and the card under a wonder stage are never named. The replay sends the viewer only their own hand and choices.
- Animations: every effect is skipped when `prefers-reduced-motion: reduce` matches; nothing animates on mount or reconnect; total turn animation ≤ 1.2 s; ghosts use `pointer-events: none`.
- `game` module stays free of rustler imports; DTOs in `core/src/nif/dto.rs`.
- Stage explicit paths only; never stage `helios/config/config.exs` / `helios/config/dev.exs`.
- Core checks: `cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test`; Helios: `cd helios && mix precommit`.

## Review Focus

- A neighbour who sells a resource during someone else's Babylon B extra turn gains coins without playing: their `CoinChange` must still be in that turn's `coins` (the simulation invariant `3 + Σ delta == final coins` catches it). (Task 1.)
- Several events arriving in one update (turn 6 + battles + new age): the turn animation is pushed, then the age summary opens — and a page that mounts or reconnects mid-game pushes nothing. (Task 4/5 tests.)
- A player whose own submit resolves the turn still gets `my_card` (their just-submitted card, not nil). (Task 4.)
- Replay frame for an extra turn the viewer does not play has an empty hand and no choice; frames never contain another player's hand. (Task 6.)
- `replay_goto` with an out-of-range or non-numeric frame is ignored without crashing. (Task 7.)

---

### Task 1: Engine history

**Files:**
- Create: `core/src/game/history.rs`
- Modify: `core/src/game/mod.rs` (field, accessor, re-exports, `debug_json`), `core/src/game/setup.rs` (initialise the field), `core/src/game/resolve.rs` (record turns and battles), `core/src/lib.rs` (test module)
- Test: `core/src/tests/game_history.rs` (new), `core/src/tests/game_simulation.rs`

**Interfaces:**
- Produces: `crate::game::{HistoryEvent, TurnKind, Play, PublicAction, CoinChange, Battle}` (all `Debug, Clone, PartialEq, Eq, Serialize`) and `Game::history(&self) -> &[HistoryEvent]`.

```rust
pub enum HistoryEvent {
    Turn { age: u8, turn: u8, kind: TurnKind, plays: Vec<Play>, coins: Vec<CoinChange> },
    Battles { age: u8, battles: Vec<Battle> },
}
pub enum TurnKind { Regular, PlayLastCard, BuildFromDiscard }
pub struct Play { pub player: String, pub action: PublicAction, pub coins_to_west: u32, pub coins_to_east: u32 }
pub enum PublicAction { Built { card: String }, BuiltFree { card: String }, BuiltFromDiscard { card: String }, WonderStage { stage: u8 }, Discarded }
pub struct CoinChange { pub player: String, pub delta: i32 }
pub struct Battle { pub player: String, pub rival: String, pub player_strength: u32, pub rival_strength: u32, pub player_token: Option<i32>, pub rival_token: Option<i32> }
```

- [ ] **Step 1: Write the failing tests**

`core/src/tests/game_history.rs`:

```rust
use super::game_helpers::{
    build_paying, discard_age, discard_first, discard_turns, game_with, plain_game, stage,
};
use crate::game::{
    Action, Battle, CoinChange, HistoryEvent, Play, PublicAction, ResourceType::*, Side,
    TurnKind,
};

fn last_turn(game: &crate::game::Game) -> (u8, u8, TurnKind, Vec<Play>, Vec<CoinChange>) {
    game.history()
        .iter()
        .rev()
        .find_map(|event| match event {
            HistoryEvent::Turn { age, turn, kind, plays, coins } => {
                Some((*age, *turn, *kind, plays.clone(), coins.clone()))
            }
            HistoryEvent::Battles { .. } => None,
        })
        .expect("a turn was recorded")
}

#[test]
fn a_turn_records_public_actions_trades_and_coin_changes() {
    // p1 Gizah (west p3, east p2); p2 Rhódos produces Ore.
    let mut game = plain_game();
    game.set_hand("p1", &["Barracks"]);
    game.submit("p1", build_paying("Barracks", &[], &[(Ore, 1)])).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");

    let (age, turn, kind, plays, coins) = last_turn(&game);
    assert_eq!((age, turn, kind), (1, 1, TurnKind::Regular));
    assert_eq!(
        plays,
        vec![
            Play {
                player: "p1".into(),
                action: PublicAction::Built { card: "Barracks".into() },
                coins_to_west: 0,
                coins_to_east: 2,
            },
            Play { player: "p2".into(), action: PublicAction::Discarded, coins_to_west: 0, coins_to_east: 0 },
            Play { player: "p3".into(), action: PublicAction::Discarded, coins_to_west: 0, coins_to_east: 0 },
        ]
    );
    assert_eq!(
        coins,
        vec![
            CoinChange { player: "p1".into(), delta: -2 },
            CoinChange { player: "p2".into(), delta: 5 },
            CoinChange { player: "p3".into(), delta: 3 },
        ]
    );
}

#[test]
fn wonder_stages_hide_the_card() {
    let mut game = plain_game();
    game.give("p1", "Stone Pit");
    game.set_hand("p1", &["Altar"]);
    game.submit("p1", stage("Altar")).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    let (_, _, _, plays, _) = last_turn(&game);
    assert_eq!(plays[0].action, PublicAction::WonderStage { stage: 1 });
    let text = format!("{:?}", game.history());
    assert!(!text.contains("Altar"), "the stage card leaked: {text}");
}

#[test]
fn olympia_free_builds_are_recorded() {
    let mut game = game_with(&[("Olympía", Side::A), ("Rhódos", Side::A), ("Éphesos", Side::A)]);
    game.give_stages("p1", 2);
    game.set_hand("p1", &["Palace"]);
    game.submit("p1", Action::BuildFree { card: "Palace".into() }).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    let (_, _, _, plays, _) = last_turn(&game);
    assert_eq!(plays[0].action, PublicAction::BuiltFree { card: "Palace".into() });
}

#[test]
fn babylon_b_last_card_is_its_own_turn() {
    let mut game = game_with(&[("Babylon", Side::B), ("Rhódos", Side::A), ("Éphesos", Side::A)]);
    game.give_stages("p1", 2);
    discard_turns(&mut game, 6);
    let last = game.hand_names("p1")[0].clone();
    game.submit("p1", Action::Discard { card: last }).unwrap();
    let turns: Vec<&HistoryEvent> = game
        .history()
        .iter()
        .filter(|e| matches!(e, HistoryEvent::Turn { kind: TurnKind::PlayLastCard, .. }))
        .collect();
    assert_eq!(turns.len(), 1);
    let HistoryEvent::Turn { plays, coins, turn, .. } = turns[0] else { unreachable!() };
    assert_eq!(*turn, 6);
    assert_eq!(plays.len(), 1);
    assert_eq!(plays[0].action, PublicAction::Discarded);
    assert_eq!(coins, &vec![CoinChange { player: "p1".into(), delta: 3 }]);
}

#[test]
fn halikarnassos_builds_from_the_discard_pile_in_its_own_turn() {
    let mut game = game_with(&[("Halikarnassós", Side::A), ("Rhódos", Side::A), ("Éphesos", Side::A)]);
    game.give_stages("p1", 1);
    game.give("p1", "Foundry");
    game.give("p1", "Ore Vein");
    let card = game.hand_names("p1")[0].clone();
    game.submit("p1", stage(&card)).unwrap();
    discard_first(&mut game, "p2");
    discard_first(&mut game, "p3");
    let built = game.built_names("p1");
    let pick = game
        .discard_names()
        .into_iter()
        .find(|name| !built.contains(name))
        .expect("a buildable discard");
    game.submit("p1", Action::BuildFromDiscard { card: pick.clone() }).unwrap();
    let (_, _, kind, plays, _) = last_turn(&game);
    assert_eq!(kind, TurnKind::BuildFromDiscard);
    assert_eq!(plays, vec![Play {
        player: "p1".into(),
        action: PublicAction::BuiltFromDiscard { card: pick },
        coins_to_west: 0,
        coins_to_east: 0,
    }]);
}

#[test]
fn each_age_ends_with_one_battle_per_neighbouring_pair() {
    let mut game = plain_game();
    game.give("p1", "Stockade");
    discard_age(&mut game);
    assert_eq!(game.history().len(), 7);
    let HistoryEvent::Battles { age, battles } = &game.history()[6] else {
        panic!("expected battles last");
    };
    assert_eq!(*age, 1);
    assert_eq!(
        battles,
        &vec![
            Battle { player: "p1".into(), rival: "p2".into(), player_strength: 1, rival_strength: 0, player_token: Some(1), rival_token: Some(-1) },
            Battle { player: "p2".into(), rival: "p3".into(), player_strength: 0, rival_strength: 0, player_token: None, rival_token: None },
            Battle { player: "p3".into(), rival: "p1".into(), player_strength: 0, rival_strength: 1, player_token: Some(-1), rival_token: Some(1) },
        ]
    );
}
```

Register `pub mod game_history;` in `core/src/lib.rs`.

Append to `core/src/tests/game_simulation.rs`:

```rust
use crate::game::{HistoryEvent, PublicAction};
use std::collections::HashMap;

#[test]
fn history_agrees_with_the_final_table() {
    for players in [3, 5, 7] {
        for seed in 0..5 {
            let game = play_random_game(players, seed, |_| {});
            let mut coins: HashMap<String, i32> = HashMap::new();
            let mut built: HashMap<String, Vec<String>> = HashMap::new();
            let mut tokens: HashMap<String, Vec<i32>> = HashMap::new();
            for event in game.history() {
                match event {
                    HistoryEvent::Turn { plays, coins: changes, .. } => {
                        for change in changes {
                            *coins.entry(change.player.clone()).or_insert(3) += change.delta;
                        }
                        for play in plays {
                            if let PublicAction::Built { card }
                            | PublicAction::BuiltFree { card }
                            | PublicAction::BuiltFromDiscard { card } = &play.action
                            {
                                built.entry(play.player.clone()).or_default().push(card.clone());
                            }
                        }
                    }
                    HistoryEvent::Battles { battles, .. } => {
                        for battle in battles {
                            if let Some(t) = battle.player_token {
                                tokens.entry(battle.player.clone()).or_default().push(t);
                            }
                            if let Some(t) = battle.rival_token {
                                tokens.entry(battle.rival.clone()).or_default().push(t);
                            }
                        }
                    }
                }
            }
            for player in game.players() {
                assert_eq!(coins.get(player).copied().unwrap_or(3), game.coins(player) as i32, "coins {player} {players}/{seed}");
                assert_eq!(built.get(player).cloned().unwrap_or_default(), game.built_names(player), "built {player}");
                let mut expected = game.tokens(player);
                let mut got = tokens.get(player).cloned().unwrap_or_default();
                expected.sort();
                got.sort();
                assert_eq!(got, expected, "tokens {player}");
            }
        }
    }
}

#[test]
fn identical_inputs_produce_identical_history() {
    let a = play_random_game(5, 3, |_| {});
    let b = play_random_game(5, 3, |_| {});
    assert_eq!(a.history(), b.history());
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd core && cargo test history`
Expected: compile error (`HistoryEvent`, `history()` unknown).

- [ ] **Step 3: Types**

`core/src/game/history.rs`:

```rust
//! Public record of what happened: one `Turn` per resolved turn (regular or
//! extra) and one `Battles` per age. Derived during resolution, so replaying
//! the action log rebuilds it exactly. Holds no hidden information: discarded
//! cards and the cards under wonder stages are never named.
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum HistoryEvent {
    Turn {
        age: u8,
        turn: u8,
        kind: TurnKind,
        /// One per acting player, in seat order.
        plays: Vec<Play>,
        /// Every seat whose coins changed this turn, in seat order.
        coins: Vec<CoinChange>,
    },
    Battles {
        age: u8,
        /// One per seat against its east neighbour.
        battles: Vec<Battle>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum TurnKind {
    Regular,
    PlayLastCard,
    BuildFromDiscard,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Play {
    pub player: String,
    pub action: PublicAction,
    pub coins_to_west: u32,
    pub coins_to_east: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum PublicAction {
    Built { card: String },
    /// Olympía A.
    BuiltFree { card: String },
    /// Halikarnassós.
    BuiltFromDiscard { card: String },
    WonderStage { stage: u8 },
    Discarded,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CoinChange {
    pub player: String,
    pub delta: i32,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Battle {
    pub player: String,
    pub rival: String,
    pub player_strength: u32,
    pub rival_strength: u32,
    pub player_token: Option<i32>,
    pub rival_token: Option<i32>,
}
```

`core/src/game/mod.rs`: `mod history;`, `pub use history::{Battle, CoinChange, HistoryEvent, Play, PublicAction, TurnKind};`, add the field to `Game`:

```rust
    /// Public events, appended as turns and ages resolve.
    history: Vec<history::HistoryEvent>,
```

an accessor:

```rust
    /// Public history of the game (same for every player).
    pub fn history(&self) -> &[HistoryEvent] {
        &self.history
    }
```

and `"history": self.history,` in `debug_json`. `core/src/game/setup.rs`: `history: Vec::new(),` in the `Game { … }` literal.

- [ ] **Step 4: Record turns and battles in `resolve.rs`**

Add `use super::history::{Battle, CoinChange, HistoryEvent, Play, PublicAction, TurnKind};`. Replace `resolve`:

```rust
    pub(super) fn resolve(&mut self) {
        let kind = match &self.phase {
            Phase::ChoosingCards { .. } => TurnKind::Regular,
            Phase::ExtraTurn { kind: ExtraTurnKind::PlayLastCard, .. } => TurnKind::PlayLastCard,
            Phase::ExtraTurn { kind: ExtraTurnKind::BuildFromDiscard, .. } => {
                TurnKind::BuildFromDiscard
            }
            Phase::GameOver { .. } => return,
        };
        let batch = self.take_pending();
        let build_from_discard = self.apply_batch(kind, batch);
        if kind == TurnKind::Regular && self.hands.iter().all(|hand| hand.len() == 1) {
            self.handle_last_cards();
        }
        for seat in build_from_discard {
            self.extra_turns.push_back((seat, ExtraTurnKind::BuildFromDiscard));
        }
        self.advance();
    }
```

In `apply_batch(&mut self, kind: TurnKind, batch: …)`:
- before the loop: `let coins_before: Vec<u32> = (0..self.seats.len()).map(|seat| self.player(seat).coins).collect();` and `let mut plays = Vec::with_capacity(batch.len());`
- inside the loop, right after `let card = self.take_card(seat, &resolved);` and before the decision `match`:

```rust
            let name_of_card = card.0.name().to_string();
            let action = match resolved.kind {
                Kind::Build => PublicAction::Built { card: name_of_card },
                Kind::BuildFree => PublicAction::BuiltFree { card: name_of_card },
                Kind::FromDiscard => PublicAction::BuiltFromDiscard { card: name_of_card },
                // Stages are applied after the loop, so the count is still the old one.
                Kind::Wonder => PublicAction::WonderStage {
                    stage: self.player(seat).wonder_stages_built + 1,
                },
                Kind::Discard => PublicAction::Discarded,
            };
            plays.push(Play {
                player: self.seats[seat].clone(),
                action,
                coins_to_west: resolved.west,
                coins_to_east: resolved.east,
            });
```

- after the credits loop (before taking `events`):

```rust
        let coins = (0..self.seats.len())
            .filter_map(|seat| {
                let delta = self.player(seat).coins as i32 - coins_before[seat] as i32;
                (delta != 0).then(|| CoinChange { player: self.seats[seat].clone(), delta })
            })
            .collect();
        self.history.push(HistoryEvent::Turn {
            age: self.age,
            turn: self.turn,
            kind,
            plays,
            coins,
        });
```

In `resolve_battles`, after awarding tokens:

```rust
        let outcome = |mine: u32, theirs: u32| match mine.cmp(&theirs) {
            Ordering::Greater => Some(victory),
            Ordering::Less => Some(-1),
            Ordering::Equal => None,
        };
        let battles = (0..self.seats.len())
            .map(|seat| {
                let rival = self.east_of(seat);
                Battle {
                    player: self.seats[seat].clone(),
                    rival: self.seats[rival].clone(),
                    player_strength: shields[seat],
                    rival_strength: shields[rival],
                    player_token: outcome(shields[seat], shields[rival]),
                    rival_token: outcome(shields[rival], shields[seat]),
                }
            })
            .collect();
        self.history.push(HistoryEvent::Battles { age: self.age, battles });
```

- [ ] **Step 5: Run the core checks**

Run: `cd core && cargo fmt && cargo clippy --all-targets -- -D warnings && cargo test`
Expected: PASS (all old tests unchanged).

- [ ] **Step 6: Commit**

```bash
git add core/src/game/history.rs core/src/game/mod.rs core/src/game/setup.rs core/src/game/resolve.rs core/src/lib.rs core/src/tests/game_history.rs core/src/tests/game_simulation.rs
git commit -m "feat(core): record a public history of turns and battles"
```

---

### Task 2: `history` NIF and Helios plumbing

**Files:**
- Modify: `core/src/nif/dto.rs`, `core/src/nif/mod.rs`, `helios/lib/helios/core/native.ex`, `helios/lib/helios/core.ex`, `helios/lib/helios/games/game_server.ex`, `helios/lib/helios/games.ex`
- Test: `core/src/tests/nif_dto.rs`, `helios/test/helios/core_shapes_test.exs`, `helios/test/helios/games/game_server_test.exs`

**Interfaces:**
- Produces: `Core.history(game) :: {:ok, [event]} | {:error, :lock_fail}` and `Games.history(game_id) :: {:ok, [event]} | {:error, term()}` where

```elixir
event ::
  {:turn, %{age: 1..3, turn: 1..6, kind: :regular | :play_last_card | :build_from_discard,
            plays: [%{player: String.t(), action: action, coins_to_west: non_neg_integer(), coins_to_east: non_neg_integer()}],
            coins: [%{player: String.t(), delta: integer()}]}}
  | {:battles, %{age: 1..3, battles: [%{player: String.t(), rival: String.t(), player_strength: non_neg_integer(),
                                        rival_strength: non_neg_integer(), player_token: integer() | nil, rival_token: integer() | nil}]}}
action :: {:built, card} | {:built_free, card} | {:built_from_discard, card} | {:wonder_stage, pos_integer()} | :discarded
```

- [ ] **Step 1: Write the failing tests**

`nif_dto.rs`:

```rust
#[test]
fn history_converts() {
    use crate::nif::dto::{HistoryEventDto, PublicActionDto, TurnKindDto};
    let mut game = plain_game();
    super::game_helpers::discard_turn(&mut game);
    let dto: Vec<HistoryEventDto> = game.history().iter().cloned().map(Into::into).collect();
    let HistoryEventDto::Turn { kind, plays, coins, .. } = &dto[0] else { panic!() };
    assert_eq!(*kind, TurnKindDto::Regular);
    assert_eq!(plays[0].action, PublicActionDto::Discarded);
    assert_eq!(coins[0].delta, 3);
}
```

`core_shapes_test.exs`:

```elixir
  test "history/1 lists public turn and battle events" do
    {:ok, ref} = Core.new_game(@players, @wonders, 1)
    assert {:ok, []} = Core.history(ref)

    for _turn <- 1..6, player <- @players do
      {:ok, view} = Core.view(ref, player)
      :ok = Core.submit(ref, player, {:discard, hd(view.hand).name})
    end

    assert {:ok, events} = Core.history(ref)
    assert [{:turn, first} | _] = events
    assert %{age: 1, turn: 1, kind: :regular, plays: [play | _], coins: [%{player: "1", delta: 3} | _]} = first
    assert %{player: "1", action: :discarded, coins_to_west: 0, coins_to_east: 0} = play
    assert {:battles, %{age: 1, battles: [battle, _, _]}} = List.last(events)
    assert %{player: "1", rival: "2", player_strength: 0, rival_strength: 0, player_token: nil, rival_token: nil} = battle
  end
```

`game_server_test.exs`:

```elixir
  test "history/1 goes through the GameServer", %{game: game, users: users} do
    for user <- users do
      [card | _] = hand_names(game, user)
      :ok = Games.submit(game.id, user.id, {:discard, card})
    end

    assert {:ok, [{:turn, %{turn: 1, plays: [_, _, _]}}]} = Games.history(game.id)
  end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd core && cargo test history_converts` — compile error. `cd helios && mix test test/helios/core_shapes_test.exs` — `Core.history/1` undefined.

- [ ] **Step 3: DTOs and NIF**

`dto.rs` (import `Battle, CoinChange, HistoryEvent, Play, PublicAction, TurnKind` from `crate::game`):

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum TurnKindDto {
    Regular,
    PlayLastCard,
    BuildFromDiscard,
}

impl From<TurnKind> for TurnKindDto {
    fn from(kind: TurnKind) -> Self {
        match kind {
            TurnKind::Regular => Self::Regular,
            TurnKind::PlayLastCard => Self::PlayLastCard,
            TurnKind::BuildFromDiscard => Self::BuildFromDiscard,
        }
    }
}

/// `{:built, card}` | `{:built_free, card}` | `{:built_from_discard, card}` | `{:wonder_stage, n}` | `:discarded`
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum PublicActionDto {
    Built(String),
    BuiltFree(String),
    BuiltFromDiscard(String),
    WonderStage(u8),
    Discarded,
}

impl From<PublicAction> for PublicActionDto {
    fn from(action: PublicAction) -> Self {
        match action {
            PublicAction::Built { card } => Self::Built(card),
            PublicAction::BuiltFree { card } => Self::BuiltFree(card),
            PublicAction::BuiltFromDiscard { card } => Self::BuiltFromDiscard(card),
            PublicAction::WonderStage { stage } => Self::WonderStage(stage),
            PublicAction::Discarded => Self::Discarded,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct PlayDto {
    pub player: String,
    pub action: PublicActionDto,
    pub coins_to_west: u32,
    pub coins_to_east: u32,
}

impl From<Play> for PlayDto {
    fn from(play: Play) -> Self {
        Self {
            player: play.player,
            action: play.action.into(),
            coins_to_west: play.coins_to_west,
            coins_to_east: play.coins_to_east,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct CoinChangeDto {
    pub player: String,
    pub delta: i32,
}

impl From<CoinChange> for CoinChangeDto {
    fn from(change: CoinChange) -> Self {
        Self { player: change.player, delta: change.delta }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct BattleDto {
    pub player: String,
    pub rival: String,
    pub player_strength: u32,
    pub rival_strength: u32,
    pub player_token: Option<i32>,
    pub rival_token: Option<i32>,
}

impl From<Battle> for BattleDto {
    fn from(b: Battle) -> Self {
        Self {
            player: b.player,
            rival: b.rival,
            player_strength: b.player_strength,
            rival_strength: b.rival_strength,
            player_token: b.player_token,
            rival_token: b.rival_token,
        }
    }
}

/// `{:turn, %{age, turn, kind, plays, coins}}` | `{:battles, %{age, battles}}`
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum HistoryEventDto {
    Turn { age: u8, turn: u8, kind: TurnKindDto, plays: Vec<PlayDto>, coins: Vec<CoinChangeDto> },
    Battles { age: u8, battles: Vec<BattleDto> },
}

impl From<HistoryEvent> for HistoryEventDto {
    fn from(event: HistoryEvent) -> Self {
        match event {
            HistoryEvent::Turn { age, turn, kind, plays, coins } => Self::Turn {
                age,
                turn,
                kind: kind.into(),
                plays: convert(plays),
                coins: convert(coins),
            },
            HistoryEvent::Battles { age, battles } => Self::Battles { age, battles: convert(battles) },
        }
    }
}
```

`nif/mod.rs` (import `HistoryEventDto`):

```rust
/// `{:ok, [event]}` | `{:error, :lock_fail}` — public, the same for every player.
#[rustler::nif]
fn history(resource: ResourceArc<GameResource>) -> Result<Vec<HistoryEventDto>, Atom> {
    let game = resource.0.lock().map_err(|_| atoms::lock_fail())?;
    Ok(game.history().iter().cloned().map(Into::into).collect())
}
```

- [ ] **Step 4: Elixir side**

`native.ex`: `def history(_game), do: :erlang.nif_error(:nif_not_loaded)`.

`core.ex`:

```elixir
  @doc "Public history: one `{:turn, …}` per resolved turn and one `{:battles, …}` per age."
  @spec history(game()) :: {:ok, [term()]} | {:error, :lock_fail}
  def history(game), do: Native.history(game)
```

`game_server.ex`:

```elixir
  def handle_call(:history, _from, state) do
    {:reply, Core.history(state.ref), state, state.idle_timeout}
  end
```

`games.ex`:

```elixir
  @spec history(term()) :: {:ok, [term()]} | {:error, term()}
  def history(game_id), do: call(game_id, :history)
```

- [ ] **Step 5: Run and commit**

```bash
cd core && cargo fmt && cargo clippy --all-targets -- -D warnings && cargo test
cd ../helios && mix test && mix precommit
git add core/src/nif/dto.rs core/src/nif/mod.rs core/src/tests/nif_dto.rs helios/lib/helios/core/native.ex helios/lib/helios/core.ex helios/lib/helios/games/game_server.ex helios/lib/helios/games.ex helios/test/helios/core_shapes_test.exs helios/test/helios/games/game_server_test.exs
git commit -m "feat(core): history NIF; Games.history/1 through the GameServer"
```

---

### Task 3: History drawer

**Files:**
- Create: `helios/lib/helios_web/history_text.ex`, `helios/lib/helios_web/components/game_components/history_drawer.ex`
- Modify: `helios/lib/helios_web/components/game_components/top_bar.ex` (`#history-button`), `helios/lib/helios_web/live/game_live.ex` (`history` assign, drawer), `helios/lib/helios/games.ex` (`replay/2` also returns `history`)
- Test: `helios/test/helios_web/history_text_test.exs`, `helios/test/helios_web/components/game_components/history_drawer_test.exs`, `top_bar_test.exs`, `helios/test/helios_web/live/game_live_test.exs`, `helios/test/helios/games/replay_test.exs`

**Interfaces:**
- Consumes: `Games.history/1` (Task 2), `GameFormat.neighbours_of/3`, `show_card` event (Phase C).
- Produces: `HistoryText.play(play, ctx)`, `HistoryText.coins(changes, ctx)`, `HistoryText.battle(battle, ctx)`, `HistoryText.turn_title(turn)` where `ctx :: %{me: String.t(), names: map(), view: view}`.
- Produces: `HistoryDrawer.history_drawer/1` — attrs `history`, `ctx`, `frames` (map `event_index => frame_index`, default `%{}`; Task 7 fills it). Ids: `#history-drawer`, `#close-history`, `#history-empty`, `#history-<index>` per event.
- Produces: `Games.replay/2` returns `%{view: view, history: [event]}`; GameLive assign `history`.

- [ ] **Step 1: Write the failing tests**

`history_text_test.exs`:

```elixir
defmodule HeliosWeb.HistoryTextTest do
  use ExUnit.Case, async: true

  alias Helios.SampleViews
  alias HeliosWeb.HistoryText

  # Seats 1..4: 1's west is 4, east is 2.
  defp ctx, do: %{me: "1", names: SampleViews.names(), view: SampleViews.view()}

  test "plays name the actor, the action and coins paid to neighbours" do
    assert HistoryText.play(%{player: "1", action: {:built, "Altar"}, coins_to_west: 0, coins_to_east: 0}, ctx()) ==
             "You built Altar"

    assert HistoryText.play(%{player: "2", action: {:built, "Stockade"}, coins_to_west: 2, coins_to_east: 1}, ctx()) ==
             "Bob built Stockade, paid 2 coins to you and 1 coin to Cid"

    assert HistoryText.play(%{player: "3", action: {:wonder_stage, 2}, coins_to_west: 0, coins_to_east: 0}, ctx()) ==
             "Cid built wonder stage 2"

    assert HistoryText.play(%{player: "4", action: :discarded, coins_to_west: 0, coins_to_east: 0}, ctx()) ==
             "Dee discarded a card"

    assert HistoryText.play(%{player: "1", action: {:built_free, "Palace"}, coins_to_west: 0, coins_to_east: 0}, ctx()) ==
             "You built Palace for free (Olympía)"

    assert HistoryText.play(%{player: "1", action: {:built_from_discard, "Gardens"}, coins_to_west: 0, coins_to_east: 0}, ctx()) ==
             "You built Gardens from the discard pile"
  end

  test "coin changes, battles and turn titles" do
    assert HistoryText.coins([%{player: "2", delta: 5}, %{player: "1", delta: -2}], ctx()) == "Bob +5 · You −2"

    assert HistoryText.battle(
             %{player: "1", rival: "2", player_strength: 2, rival_strength: 1, player_token: 1, rival_token: -1},
             ctx()
           ) == "You 2 vs Bob 1: You +1, Bob −1"

    assert HistoryText.battle(
             %{player: "2", rival: "3", player_strength: 0, rival_strength: 0, player_token: nil, rival_token: nil},
             ctx()
           ) == "Bob 0 vs Cid 0: tie"

    assert HistoryText.turn_title(%{turn: 3, kind: :regular}) == "Turn 3"
    assert HistoryText.turn_title(%{turn: 6, kind: :play_last_card}) == "Turn 6 · last card"
    assert HistoryText.turn_title(%{turn: 6, kind: :build_from_discard}) == "Turn 6 · from the discard pile"
  end
end
```

`history_drawer_test.exs`:

```elixir
defmodule HeliosWeb.GameComponents.HistoryDrawerTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.HistoryDrawer

  @history [
    {:turn, %{age: 1, turn: 1, kind: :regular,
              plays: [%{player: "1", action: {:built, "Altar"}, coins_to_west: 0, coins_to_east: 0}],
              coins: [%{player: "2", delta: 3}]}},
    {:battles, %{age: 1, battles: [%{player: "1", rival: "2", player_strength: 1, rival_strength: 0, player_token: 1, rival_token: -1}]}},
    {:turn, %{age: 2, turn: 1, kind: :regular,
              plays: [%{player: "2", action: :discarded, coins_to_west: 0, coins_to_east: 0}], coins: []}}
  ]

  defp ctx, do: %{me: "1", names: SampleViews.names(), view: SampleViews.view()}

  test "newest first, grouped by age, battles as a block, cards open the card sheet" do
    html = render_component(&HistoryDrawer.history_drawer/1, history: @history, ctx: ctx())
    assert attrs(html, "#history-drawer [data-age]", "data-age") == ["2", "1"]
    assert attrs(html, "#history-drawer [data-history-index]", "data-history-index") == ["2", "1", "0"]
    assert text(html, "#history-1") =~ "You 1 vs Bob 0: You +1, Bob −1"
    assert text(html, "#history-0") =~ "You built Altar"
    assert text(html, "#history-0") =~ "Bob +3"
    assert attrs(html, "#history-0 [phx-click='show_card']", "phx-value-card") == ["Altar"]
    assert attrs(html, "#history-0 [phx-click='show_card']", "phx-value-owner") == ["1"]
    assert [class] = attrs(html, "#history-drawer", "class")
    assert class =~ "hidden"
  end

  test "an empty history says so" do
    html = render_component(&HistoryDrawer.history_drawer/1, history: [], ctx: ctx())
    assert count(html, "#history-empty") == 1
  end
end
```

`top_bar_test.exs`:

```elixir
  test "the history button toggles the drawer" do
    html = render_bar(SampleViews.view())
    assert [js] = attrs(html, "#history-button", "phx-click")
    assert js =~ "#history-drawer"
  end
```

`game_live_test.exs` — in "three players submitting advances the turn", after the turn advanced:

```elixir
    assert has_element?(view_a, "#history-drawer #history-0")
```

`replay_test.exs` — in "replay/2 rebuilds…": `assert {:ok, %{view: view, history: [{:turn, _} | _] = history}} = Games.replay(game, a.id)` and `assert {:battles, %{age: 3}} = List.last(history)`.

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/history_text_test.exs test/helios_web/components/game_components test/helios_web/live/game_live_test.exs test/helios/games/replay_test.exs`
Expected: FAIL.

- [ ] **Step 3: `HistoryText`**

```elixir
defmodule HeliosWeb.HistoryText do
  @moduledoc "English for history events. `ctx` = %{me: id, names: %{id => name}, view: view (seat order)}."

  alias HeliosWeb.GameFormat

  def play(%{player: player, action: action} = play, ctx) do
    "#{actor(player, ctx)} #{did(action)}" <> payments(play, ctx)
  end

  defp did({:built, card}), do: "built #{card}"
  defp did({:built_free, card}), do: "built #{card} for free (Olympía)"
  defp did({:built_from_discard, card}), do: "built #{card} from the discard pile"
  defp did({:wonder_stage, n}), do: "built wonder stage #{n}"
  defp did(:discarded), do: "discarded a card"

  defp payments(%{coins_to_west: 0, coins_to_east: 0}, _ctx), do: ""

  defp payments(%{player: player, coins_to_west: west, coins_to_east: east}, ctx) do
    neighbours = GameFormat.neighbours_of(ctx.view, player, Map.put(ctx.names, ctx.me, "you"))

    paid =
      [{west, neighbours.west}, {east, neighbours.east}]
      |> Enum.reject(fn {coins, _} -> coins == 0 end)
      |> Enum.map_join(" and ", fn {coins, name} -> "#{coins(coins)} to #{name}" end)

    ", paid " <> paid
  end

  def coins(changes, ctx) do
    Enum.map_join(changes, " · ", fn %{player: player, delta: delta} -> "#{actor(player, ctx)} #{signed(delta)}" end)
  end

  def battle(%{player_token: nil, rival_token: nil} = b, ctx), do: score(b, ctx) <> ": tie"

  def battle(b, ctx) do
    score(b, ctx) <> ": #{actor(b.player, ctx)} #{signed(b.player_token)}, #{actor(b.rival, ctx)} #{signed(b.rival_token)}"
  end

  def turn_title(%{turn: turn, kind: :regular}), do: "Turn #{turn}"
  def turn_title(%{turn: turn, kind: :play_last_card}), do: "Turn #{turn} · last card"
  def turn_title(%{turn: turn, kind: :build_from_discard}), do: "Turn #{turn} · from the discard pile"

  def actor(player, %{me: player}), do: "You"
  def actor(player, ctx), do: GameFormat.player_name(ctx.names, player)

  defp score(b, ctx),
    do: "#{actor(b.player, ctx)} #{b.player_strength} vs #{actor(b.rival, ctx)} #{b.rival_strength}"

  defp signed(n) when n > 0, do: "+#{n}"
  defp signed(n), do: "−#{abs(n)}"

  defp coins(1), do: "1 coin"
  defp coins(n), do: "#{n} coins"
end
```

- [ ] **Step 4: `HistoryDrawer`**

```elixir
defmodule HeliosWeb.GameComponents.HistoryDrawer do
  @moduledoc "What happened so far, newest first, grouped by age. Toggled client-side from the top bar."
  use HeliosWeb, :html

  alias HeliosWeb.{GameFormat, HistoryText}

  attr :history, :list, required: true
  attr :ctx, :map, required: true
  attr :frames, :map, default: %{}, doc: "event index => replay frame index (finished games)"

  def history_drawer(assigns) do
    assigns =
      assign(assigns,
        ages:
          assigns.history
          |> Enum.with_index()
          |> Enum.reverse()
          |> Enum.chunk_by(fn {{_tag, event}, _i} -> event.age end)
      )

    ~H"""
    <aside
      id="history-drawer"
      role="dialog"
      aria-labelledby="history-title"
      phx-window-keydown={JS.hide(to: "#history-drawer")}
      phx-key="Escape"
      class="fixed inset-x-0 bottom-0 z-50 hidden max-h-[70dvh] overflow-y-auto rounded-t-2xl bg-white p-4 pb-[max(1rem,env(safe-area-inset-bottom))] text-sm text-zinc-800 shadow-2xl ring-1 ring-black/5 lg:inset-x-auto lg:top-4 lg:right-4 lg:bottom-4 lg:max-h-none lg:w-96 lg:rounded-2xl"
    >
      <header class="mb-2 flex items-center justify-between">
        <h2 id="history-title" class="text-base font-semibold">History</h2>
        <button
          id="close-history"
          type="button"
          aria-label="Close"
          phx-click={JS.hide(to: "#history-drawer")}
          class="inline-flex size-9 items-center justify-center rounded-full text-zinc-500 hover:bg-zinc-100 pointer-coarse:size-11"
        >
          <.icon name="hero-x-mark" class="size-5" />
        </button>
      </header>
      <p :if={@history == []} id="history-empty" class="text-zinc-500">Nothing has happened yet.</p>
      <section :for={[{{_tag, first}, _i} | _] = events <- @ages} data-age={first.age} class="mb-3">
        <h3 class="mb-1 text-xs font-extrabold uppercase tracking-wide text-zinc-500">
          Age {GameFormat.roman(first.age)}
        </h3>
        <%= for {{tag, event}, index} <- events do %>
          <div
            :if={tag == :battles}
            id={"history-#{index}"}
            data-history-index={index}
            class="mb-2 rounded-lg bg-red-50 px-2 py-1.5 ring-1 ring-red-200"
          >
            <p class="font-semibold">Military conflicts</p>
            <p :for={battle <- event.battles}>{HistoryText.battle(battle, @ctx)}</p>
          </div>
          <div
            :if={tag == :turn}
            id={"history-#{index}"}
            data-history-index={index}
            class="mb-2 border-l-4 border-zinc-200 pl-2"
          >
            <%= if Map.has_key?(@frames, index) do %>
              <button
                type="button"
                phx-click="replay_goto"
                phx-value-frame={@frames[index]}
                class="text-xs font-semibold text-sky-700 underline-offset-2 hover:underline"
              >
                {HistoryText.turn_title(event)}
              </button>
            <% else %>
              <p class="text-xs font-semibold text-zinc-500">{HistoryText.turn_title(event)}</p>
            <% end %>
            <p :for={play <- event.plays} class="flex flex-wrap items-center gap-1">
              {HistoryText.play(play, @ctx)}
              <button
                :if={card_of(play)}
                type="button"
                phx-click="show_card"
                phx-value-card={card_of(play)}
                phx-value-owner={play.player}
                aria-label={"About #{card_of(play)}"}
                class="inline-flex text-zinc-400 hover:text-zinc-700"
              >
                <.icon name="hero-information-circle" class="size-4" />
              </button>
            </p>
            <p :if={event.coins != []} class="text-xs text-zinc-600">{HistoryText.coins(event.coins, @ctx)}</p>
          </div>
        <% end %>
      </section>
    </aside>
    """
  end

  defp card_of(%{action: {kind, card}}) when kind in [:built, :built_free, :built_from_discard], do: card
  defp card_of(_play), do: nil
end
```

- [ ] **Step 5: Top bar button and GameLive wiring**

`top_bar.ex` — next to `#age-label`:

```heex
          <button
            id="history-button"
            type="button"
            phx-click={JS.toggle(to: "#history-drawer", display: "block")}
            aria-controls="history-drawer"
            class="inline-flex items-center gap-1 rounded-md bg-white/15 px-2 py-1 text-sm font-semibold transition hover:bg-white/25 pointer-coarse:min-h-11"
          >
            <.icon name="hero-book-open" class="size-4" /><span class="sr-only sm:not-sr-only">History</span>
          </button>
```

`games.ex` `replay/2` — return the history too:

```elixir
    with {:ok, ref, nil} <- Replay.rebuild(game, Replay.actions(game.id), fn _ -> nil end, fn _, _, acc -> acc end),
         {:ok, %{phase: %{kind: :game_over}} = view} <- Core.view(ref, to_string(user_id)),
         {:ok, history} <- Core.history(ref) do
      {:ok, %{view: view, history: history}}
```

(`@spec` → `{:ok, %{view: map(), history: [term()]}}`.)

`game_live.ex`:
- `import HeliosWeb.GameComponents.HistoryDrawer`.
- `mount_active`: after the view, `history = fetch_history(game.id)` and assign it; `mount_finished` success branch assigns `history` from the replay; fallback assigns `history: []`.
- `refresh/1`: assign `history: fetch_history(game.id)` on every update.

```elixir
  defp fetch_history(game_id) do
    case Games.history(game_id) do
      {:ok, history} -> history
      {:error, _reason} -> []
    end
  end
```

- render the drawer once, just inside `#game` (both branches):

```heex
        <.history_drawer
          history={@history}
          ctx={%{me: @view.me, names: @names, view: @view}}
        />
```

- [ ] **Step 6: Run and commit**

Run: `cd helios && mix test && mix precommit`
Expected: PASS.

```bash
git add helios/lib/helios_web/history_text.ex helios/lib/helios_web/components/game_components/history_drawer.ex helios/lib/helios_web/components/game_components/top_bar.ex helios/lib/helios_web/live/game_live.ex helios/lib/helios/games.ex helios/test/helios_web/history_text_test.exs helios/test/helios_web/components/game_components/history_drawer_test.exs helios/test/helios_web/components/game_components/top_bar_test.exs helios/test/helios_web/live/game_live_test.exs helios/test/helios/games/replay_test.exs
git commit -m "feat(web): history drawer of turns and military conflicts"
```

---

### Task 4: Turn animations

**Files:**
- Create: `helios/lib/helios_web/turn_fx.ex` (payload builder), `helios/lib/helios_web/components/game_components/fx.ex` (`#fx-layer` + `.TurnFx` hook)
- Modify: `helios/lib/helios_web/components/game_components/hand.ex` (`.HandRects` hook on `#hand`), `helios/lib/helios_web/live/game_live.ex` (diff + `push_event`, `last_choice`)
- Modify: `e2e/playwright.config.ts` (`reducedMotion: "reduce"`)
- Test: `helios/test/helios_web/turn_fx_test.exs`, `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Produces: `HeliosWeb.TurnFx.payload(turn_event, %{me: id, my_card: name | nil, pass: :west | :east | nil}) :: map()` — `%{plays: [%{player, kind: "built" | "wonder_stage" | "discarded", target: dom_id | nil}], coins: [%{player, delta}], me, my_card, pass: "west" | "east" | nil}`. Task 7 pushes the same payload while replaying.
- Produces: `Fx.fx_layer/1` (renders `#fx-layer`); hook `.TurnFx` handles `turn_resolved` and the DOM event `fx:tokens` (Task 5). `#hand` carries hook `.HandRects`, which stores `window.sevenWondersHandRects = %{card => {left, top, width, height, src}}` before every patch.

- [ ] **Step 1: Write the failing tests**

`turn_fx_test.exs`:

```elixir
defmodule HeliosWeb.TurnFxTest do
  use ExUnit.Case, async: true

  alias HeliosWeb.TurnFx

  test "plays point at the DOM ids they animate to" do
    turn =
      {:turn,
       %{age: 1, turn: 1, kind: :regular,
         plays: [
           %{player: "1", action: {:built, "Lumber Yard"}, coins_to_west: 0, coins_to_east: 0},
           %{player: "2", action: {:wonder_stage, 1}, coins_to_west: 0, coins_to_east: 0},
           %{player: "3", action: :discarded, coins_to_west: 0, coins_to_east: 0}
         ],
         coins: [%{player: "3", delta: 3}]}}

    assert TurnFx.payload(turn, %{me: "1", my_card: "Lumber Yard", pass: :west}) == %{
             plays: [
               %{player: "1", kind: "built", target: "built-1-lumberyard"},
               %{player: "2", kind: "wonder_stage", target: "stage-2-1"},
               %{player: "3", kind: "discarded", target: nil}
             ],
             coins: [%{player: "3", delta: 3}],
             me: "1",
             my_card: "Lumber Yard",
             pass: "west"
           }
  end
end
```

`game_live_test.exs`:

```elixir
  test "a resolved turn is pushed to the animation hook, with my card", %{game: game, players: players} do
    [{a, ta}, {b, _}, {c, _}] = players
    {:ok, view, _html} = open(ta, game.id)
    refute_push_event(view, "turn_resolved", %{})

    {:ok, state} = Games.view(game.id, a.id)
    [first | _] = Enum.map(state.hand, & &1.name)
    view |> element("#hand-card-0") |> render_click()
    view |> element("#discard-button") |> render_click()

    for user <- [b, c] do
      {:ok, other} = Games.view(game.id, user.id)
      :ok = Games.submit(game.id, user.id, {:discard, hd(other.hand).name})
    end

    assert_push_event(view, "turn_resolved", %{my_card: ^first, pass: "west", plays: [_, _, _]})
  end

  test "my own submit that resolves the turn still names my card", %{game: game, players: players} do
    [{a, ta}, {b, _}, {c, _}] = players

    for user <- [b, c] do
      {:ok, other} = Games.view(game.id, user.id)
      :ok = Games.submit(game.id, user.id, {:discard, hd(other.hand).name})
    end

    {:ok, view, _html} = open(ta, game.id)
    {:ok, state} = Games.view(game.id, a.id)
    [first | _] = Enum.map(state.hand, & &1.name)
    view |> element("#hand-card-0") |> render_click()
    view |> element("#discard-button") |> render_click()
    assert_push_event(view, "turn_resolved", %{my_card: ^first})
  end

  test "mounting mid-game animates nothing", %{game: game, players: players} do
    ids = Enum.map(players, fn {user, _} -> to_string(user.id) end)
    {view_fun, submit_fun} = Helios.GameDriver.games_funs(game.id)
    Helios.GameDriver.step(ids, %{}, view_fun, submit_fun)
    [{_a, ta} | _] = players
    {:ok, view, _html} = open(ta, game.id)
    refute_push_event(view, "turn_resolved", %{})
    assert has_element?(view, "#fx-layer[phx-update='ignore']")
    assert has_element?(view, "#hand[phx-hook]")
  end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/turn_fx_test.exs test/helios_web/live/game_live_test.exs`
Expected: FAIL.

- [ ] **Step 3: Payload builder**

```elixir
defmodule HeliosWeb.TurnFx do
  @moduledoc "Builds the `turn_resolved` payload the `.TurnFx` hook animates (DOM ids computed here, not in JS)."

  alias HeliosWeb.GameAssets

  def payload({:turn, turn}, %{me: me, my_card: my_card, pass: pass}) do
    %{
      plays: Enum.map(turn.plays, &play/1),
      coins: Enum.map(turn.coins, &%{player: &1.player, delta: &1.delta}),
      me: me,
      my_card: my_card,
      pass: pass && Atom.to_string(pass)
    }
  end

  defp play(%{player: player, action: {kind, card}})
       when kind in [:built, :built_free, :built_from_discard],
       do: %{player: player, kind: "built", target: "built-#{player}-#{GameAssets.slug(card)}"}

  defp play(%{player: player, action: {:wonder_stage, n}}),
    do: %{player: player, kind: "wonder_stage", target: "stage-#{player}-#{n}"}

  defp play(%{player: player, action: :discarded}),
    do: %{player: player, kind: "discarded", target: nil}
end
```

- [ ] **Step 4: Hooks**

`helios/lib/helios_web/components/game_components/fx.ex`:

```elixir
defmodule HeliosWeb.GameComponents.Fx do
  @moduledoc """
  Full-screen, click-through layer for turn animations. The `.TurnFx` hook
  flies the player's card from the hand to its tile or stage slot, fades in
  other players' new tiles, floats coin changes and slides the next hand in.
  Everything is skipped under `prefers-reduced-motion: reduce`.
  """
  use HeliosWeb, :html

  def fx_layer(assigns) do
    ~H"""
    <div
      id="fx-layer"
      phx-hook=".TurnFx"
      phx-update="ignore"
      class="pointer-events-none fixed inset-0 z-[70] overflow-hidden"
      aria-hidden="true"
    >
    </div>
    <script :type={Phoenix.LiveView.ColocatedHook} name=".TurnFx">
      export default {
        mounted() {
          this.handleEvent("turn_resolved", (payload) => this.play(payload))
          window.addEventListener("fx:tokens", (e) => this.tokens(e.detail))
        },
        reduced() {
          return window.matchMedia("(prefers-reduced-motion: reduce)").matches
        },
        visible(el) {
          if (!el) return null
          const r = el.getBoundingClientRect()
          return r.width > 0 && r.height > 0 ? r : null
        },
        play({plays, coins, me, my_card, pass}) {
          if (this.reduced()) return
          const rects = window.sevenWondersHandRects || {}
          const from = my_card && rects[my_card]
          for (const play of plays) {
            const target = play.target && document.getElementById(play.target)
            if (play.player === me && from) {
              const to = target || document.querySelector(`section[data-player="${me}"] [data-stat="coins"]`)
              this.fly(from, to, !target)
            } else if (target) {
              this.arrive(target)
            }
          }
          for (const {player, delta} of coins) this.float(player, delta)
          if (pass) this.slideHand(pass)
        },
        fly(from, target, fade) {
          const to = this.visible(target)
          if (!to) return
          const ghost = document.createElement("img")
          ghost.src = from.src
          Object.assign(ghost.style, {
            position: "fixed", left: `${from.left}px`, top: `${from.top}px`,
            width: `${from.width}px`, height: `${from.height}px`, borderRadius: "8px",
            transformOrigin: "top left", boxShadow: "0 8px 24px rgba(0,0,0,.35)"
          })
          this.el.appendChild(ghost)
          const move = `translate(${to.left - from.left}px, ${to.top - from.top}px) scale(${to.width / from.width}, ${to.height / from.height})`
          ghost.animate([{transform: "none", opacity: 1}, {transform: move, opacity: fade ? 0 : 0.95}],
                        {duration: 700, easing: "cubic-bezier(.2,.8,.2,1)"})
            .finished.finally(() => ghost.remove())
          if (!fade) target.animate([{opacity: 0}, {opacity: 0, offset: 0.9}, {opacity: 1}], {duration: 750})
        },
        arrive(target) {
          if (!this.visible(target)) return
          target.animate([{transform: "scale(.6)", opacity: 0}, {transform: "scale(1)", opacity: 1}],
                         {duration: 500, delay: 250, easing: "ease-out", fill: "backwards"})
        },
        float(player, delta) {
          const anchor = this.visible(document.querySelector(`section[data-player="${player}"] [data-stat="coins"]`))
          if (!anchor) return
          const label = document.createElement("span")
          label.textContent = delta > 0 ? `+${delta}` : `−${Math.abs(delta)}`
          Object.assign(label.style, {
            position: "fixed", left: `${anchor.left}px`, top: `${anchor.top - 6}px`, fontWeight: "800",
            color: delta > 0 ? "#15803d" : "#b91c1c", textShadow: "0 1px 2px rgba(255,255,255,.9)"
          })
          this.el.appendChild(label)
          label.animate([{transform: "translateY(0)", opacity: 1}, {transform: "translateY(-28px)", opacity: 0}],
                        {duration: 800, delay: 400, easing: "ease-out", fill: "backwards"})
            .finished.finally(() => label.remove())
        },
        slideHand(pass) {
          const hand = document.getElementById("hand") || document.getElementById("replay-hand")
          if (!hand) return
          const offset = pass === "west" ? 48 : -48
          hand.animate([{transform: `translateX(${offset}px)`, opacity: 0}, {transform: "none", opacity: 1}],
                       {duration: 400, delay: 700, easing: "ease-out", fill: "backwards"})
        },
        tokens(list) {
          if (this.reduced()) return
          for (const {player, src, rect} of list || []) {
            const board = `section[data-player="${player}"]`
            const target = document.querySelector(`${board} [data-tokens]`) ||
                           document.querySelector(`${board} [data-stat="coins"]`)
            this.fly({...rect, src}, target, true)
          }
        },
      }
    </script>
    """
  end
end
```

`hand.ex` — on the `#hand` section add `phx-hook=".HandRects"` and, inside the module's `hand/1` template (after the section), the hook:

```heex
    <script :type={Phoenix.LiveView.ColocatedHook} name=".HandRects">
      export default {
        mounted() { this.snapshot() },
        beforeUpdate() { this.snapshot() },
        snapshot() {
          const rects = {}
          this.el.querySelectorAll("[data-card] img").forEach((img) => {
            const r = img.getBoundingClientRect()
            rects[img.closest("[data-card]").dataset.card] =
              {left: r.left, top: r.top, width: r.width, height: r.height, src: img.currentSrc || img.src}
          })
          window.sevenWondersHandRects = rects
        },
      }
    </script>
```

(The hook does not change the DOM, so `#hand` needs no `phx-update="ignore"`.)

- [ ] **Step 5: GameLive diff**

- `import HeliosWeb.GameComponents.Fx`; render `<.fx_layer />` once inside `#game`.
- `setup/3`: `assign(last_choice: nil)`.
- `submit` handler, `:ok` branch: `socket |> assign(:last_choice, choice.card) |> assign_selection(nil)` (bind `choice` from the `with` — restructure so the parsed choice is available, e.g. `with {:ok, choice} <- Choice.parse(params), :ok <- Games.submit_choice(game.id, user.id, choice) do {:ok, choice} end`).
- `refresh/1` becomes:

```elixir
  defp refresh(socket) do
    %{game: game, current_scope: %{user: user}} = socket.assigns

    case Games.view(game.id, user.id) do
      {:ok, view} ->
        history = fetch_history(game.id)

        socket
        |> animate_new_events(history, view)
        |> assign(:history, history)
        |> assign(:last_choice, GameFormat.action_card(view.my_pending))
        |> assign_view(view)

      {:error, reason} ->
        leave(socket, reason, game.lobby_id)
    end
  end

  defp animate_new_events(socket, history, view) do
    new = Enum.drop(history, length(socket.assigns.history))
    pass = if view.phase.kind == :choosing_cards and view.phase.turn > 1, do: view.phase.direction

    Enum.reduce(new, socket, fn
      {:turn, _} = event, socket ->
        push_event(socket, "turn_resolved",
          TurnFx.payload(event, %{me: view.me, my_card: socket.assigns.last_choice, pass: pass}))

      {:battles, _}, socket ->
        socket
    end)
  end
```

(alias `HeliosWeb.TurnFx`). Mount sets `history` without calling `animate_new_events`, so nothing is pushed on mount or reconnect.

`e2e/playwright.config.ts` — in the top-level `use`: `reducedMotion: "reduce",`.

- [ ] **Step 6: Run and commit**

```bash
cd helios && mix test && mix precommit
cd ../e2e && npx playwright test
```

Expected: PASS. Then check by hand once (`cd helios && iex -S mix phx.server`, two browsers + one private window, play a turn) that the card flies and coins float; note the result in the commit message body if anything looked off.

```bash
git add helios/lib/helios_web/turn_fx.ex helios/lib/helios_web/components/game_components/fx.ex helios/lib/helios_web/components/game_components/hand.ex helios/lib/helios_web/live/game_live.ex helios/test/helios_web/turn_fx_test.exs helios/test/helios_web/live/game_live_test.exs e2e/playwright.config.ts
git commit -m "feat(web): animate resolved turns (cards to boards, coin changes, next hand)"
```

---

### Task 5: End-of-age summary

**Files:**
- Create: `helios/lib/helios_web/components/game_components/age_summary.ex`
- Modify: `helios/assets/css/app.css` (`battle-in` keyframes), `helios/lib/helios_web/components/game_components/player_panels.ex` (`data-tokens` on the token row), `helios/lib/helios_web/live/game_live.ex`
- Test: `helios/test/helios_web/components/game_components/age_summary_test.exs`, `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Consumes: `HistoryText.battle/2`, `HistoryText.actor/2` (Task 3), `.TurnFx` `fx:tokens` listener (Task 4).
- Produces: `AgeSummary.age_summary/1` — attrs `event` (`{:battles, %{age, battles}}`), `ctx`. Ids `#age-summary`, `#age-summary-title`, `#battle-N`, `#close-age-summary`. Hook `.AgeSummary` (auto-close after 6 s, dispatches `fx:tokens`, pushes `dismiss_age_summary`). `AgeSummary.ordered(battles, me)`.
- Produces: GameLive assign `age_summary` and event `dismiss_age_summary`. Task 7 sets `age_summary` while replaying.

- [ ] **Step 1: Write the failing tests**

`age_summary_test.exs`:

```elixir
defmodule HeliosWeb.GameComponents.AgeSummaryTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.AgeSummary

  @battles [
    %{player: "2", rival: "3", player_strength: 0, rival_strength: 0, player_token: nil, rival_token: nil},
    %{player: "1", rival: "2", player_strength: 2, rival_strength: 1, player_token: 1, rival_token: -1},
    %{player: "4", rival: "1", player_strength: 3, rival_strength: 2, player_token: 1, rival_token: -1}
  ]

  test "my battles come first; tokens and ties are shown; it closes" do
    html =
      render_component(&AgeSummary.age_summary/1,
        event: {:battles, %{age: 1, battles: @battles}},
        ctx: %{me: "1", names: SampleViews.names(), view: SampleViews.view()}
      )

    # Rows read "<player> <strength> [token] vs [token] <strength> <rival>".
    assert text(html, "#age-summary-title") == "End of Age I — Military conflicts"
    assert text(html, "#battle-0") =~ "You 2"
    assert text(html, "#battle-0") =~ "1 Bob"
    assert text(html, "#battle-1") =~ "Dee 3"
    assert text(html, "#battle-1") =~ "2 You"
    assert text(html, "#battle-2") =~ "tie"
    assert attrs(html, "#battle-0 img[data-token-for]", "src") ==
             ["/images/tokens/victory1.png", "/images/tokens/victoryminus1.png"]
    assert [hook] = attrs(html, "#age-summary", "phx-hook")
    assert hook =~ "AgeSummary"
    assert count(html, "#close-age-summary") == 1
  end
end
```

`game_live_test.exs`:

```elixir
  test "the end of an age opens the military summary, which can be dismissed", %{game: game, players: players} do
    [{_a, ta} | _] = players
    ids = Enum.map(players, fn {user, _} -> to_string(user.id) end)
    {:ok, view, _html} = open(ta, game.id)
    {view_fun, submit_fun} = Helios.GameDriver.games_funs(game.id)
    {:reached, _} = Helios.GameDriver.play_until(ids, %{}, view_fun, submit_fun, &(&1.phase.age == 2))

    assert has_element?(view, "#age-summary #battle-2")
    refute has_element?(view, "#age-summary #battle-3")
    render_click(view, "dismiss_age_summary", %{})
    refute has_element?(view, "#age-summary")
  end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components/age_summary_test.exs test/helios_web/live/game_live_test.exs`
Expected: FAIL.

- [ ] **Step 3: Component and hook**

`app.css` — after the cursor rule:

```css
@keyframes battle-in {
  from { opacity: 0; transform: translateY(8px); }
  to { opacity: 1; transform: none; }
}
```

`player_panels.ex` `player_stats/1`: add `data-tokens` to the military-token span (`<span :if={@player.military_tokens != []} data-tokens class=…>`).

`age_summary.ex`:

```elixir
defmodule HeliosWeb.GameComponents.AgeSummary do
  @moduledoc "End-of-age military conflicts, revealed row by row; closes on click, Escape or after ~6 s."
  use HeliosWeb, :html

  alias HeliosWeb.{GameAssets, GameFormat, HistoryText}

  @close JS.dispatch("seven:close-summary", to: "#age-summary")

  def ordered(battles, me) do
    {mine, others} = Enum.split_with(battles, &(&1.player == me or &1.rival == me))
    mine ++ others
  end

  attr :event, :any, required: true
  attr :ctx, :map, required: true

  def age_summary(assigns) do
    {:battles, %{age: age, battles: battles}} = assigns.event
    assigns = assign(assigns, age: age, rows: ordered(battles, assigns.ctx.me), close: @close)

    ~H"""
    <div
      id="age-summary"
      phx-hook=".AgeSummary"
      phx-click={@close}
      phx-window-keydown={@close}
      phx-key="Escape"
      class="fixed inset-0 z-[55] flex items-center justify-center bg-black/55 p-4"
    >
      <section
        role="dialog"
        aria-modal="true"
        aria-labelledby="age-summary-title"
        class="w-full max-w-lg rounded-2xl bg-antique p-4 shadow-2xl sm:p-6"
      >
        <h2 id="age-summary-title" class="mb-3 text-center text-lg font-bold text-zinc-900">
          End of Age {GameFormat.roman(@age)} — Military conflicts
        </h2>
        <ul class="flex flex-col gap-2">
          <li
            :for={{battle, index} <- Enum.with_index(@rows)}
            id={"battle-#{index}"}
            class="grid grid-cols-[1fr_auto_1fr] items-center gap-2 rounded-lg bg-white/70 px-3 py-2 motion-safe:animate-[battle-in_400ms_ease-out_both]"
            style={"animation-delay: #{index * 350}ms"}
          >
            <span class="flex items-center justify-end gap-1 text-right">
              {HistoryText.actor(battle.player, @ctx)} {battle.player_strength}
              <img
                :if={battle.player_token}
                data-token-for={battle.player}
                src={GameAssets.token_path({:military, battle.player_token})}
                alt={"#{battle.player_token}"}
                class="size-7"
              />
            </span>
            <span class="text-xs font-bold uppercase text-zinc-500">
              {if battle.player_token, do: "vs", else: "tie"}
            </span>
            <span class="flex items-center gap-1">
              <img
                :if={battle.rival_token}
                data-token-for={battle.rival}
                src={GameAssets.token_path({:military, battle.rival_token})}
                alt={"#{battle.rival_token}"}
                class="size-7"
              />
              {battle.rival_strength} {HistoryText.actor(battle.rival, @ctx)}
            </span>
          </li>
        </ul>
        <div class="mt-4 flex justify-center">
          <button
            id="close-age-summary"
            type="button"
            class="rounded-lg bg-zinc-900 px-4 py-2 font-semibold text-white transition hover:bg-zinc-700 pointer-coarse:min-h-11"
          >
            Continue
          </button>
        </div>
      </section>
    </div>
    <script :type={Phoenix.LiveView.ColocatedHook} name=".AgeSummary">
      export default {
        mounted() {
          this.closed = false
          this.el.addEventListener("seven:close-summary", () => this.close())
          this.timer = setTimeout(() => this.close(), 6000)
        },
        destroyed() { clearTimeout(this.timer) },
        close() {
          if (this.closed) return
          this.closed = true
          const tokens = [...this.el.querySelectorAll("img[data-token-for]")].map((img) => {
            const r = img.getBoundingClientRect()
            return {player: img.dataset.tokenFor, src: img.src, rect: {left: r.left, top: r.top, width: r.width, height: r.height}}
          })
          window.dispatchEvent(new CustomEvent("fx:tokens", {detail: tokens}))
          this.pushEvent("dismiss_age_summary", {})
        },
      }
    </script>
    """
  end
end
```

- [ ] **Step 4: GameLive**

- `import HeliosWeb.GameComponents.AgeSummary`; `setup/3` assigns `age_summary: nil`.
- In `animate_new_events/3` the `{:battles, _}` clause becomes `{:battles, _} = event, socket -> assign(socket, :age_summary, event)` (turn events are pushed first because they come first in the list).
- Render inside `#game`, after the drawer:

```heex
        <.age_summary
          :if={@age_summary}
          event={@age_summary}
          ctx={%{me: @view.me, names: @names, view: @view}}
        />
```

- Event: `def handle_event("dismiss_age_summary", _params, socket), do: {:noreply, assign(socket, :age_summary, nil)}`.

- [ ] **Step 5: Run and commit**

Run: `cd helios && mix test && mix precommit`
Expected: PASS.

```bash
git add helios/lib/helios_web/components/game_components/age_summary.ex helios/assets/css/app.css helios/lib/helios_web/components/game_components/player_panels.ex helios/lib/helios_web/live/game_live.ex helios/test/helios_web/components/game_components/age_summary_test.exs helios/test/helios_web/live/game_live_test.exs
git commit -m "feat(web): end-of-age military conflicts summary"
```

---

### Task 6: Replay frames

**Files:**
- Modify: `helios/lib/helios/games.ex` (`replay/2` builds frames)
- Test: `helios/test/helios/games/replay_test.exs`

**Interfaces:**
- Consumes: `Replay.rebuild/4` (Phase C), `Core.history/1`, `ActionCodec.decode/1`.
- Produces: `Games.replay(game, user_id) :: {:ok, %{view: view, history: [event], frames: [frame]}} | {:error, term()}` where `frame :: %{players: [public_player], hand: [String.t()], phase: phase, my_choice: action | nil, event_index: non_neg_integer() | nil}`. Frame *k* is the table at the start of turn *k*; its `event_index` is the index (in `history`) of the `{:turn, _}` event that ended it; the last frame is the final table (`event_index: nil`, phase `:game_over`). `length(frames) == turn events + 1`.

- [ ] **Step 1: Write the failing tests**

Append to `replay_test.exs` (the setup plays a discard-only game: strategy `:discard` always discards the first hand card):

```elixir
  test "frames: one per turn plus the final table, with only my hand and my choice", %{game: game, users: [a | _], ids: [ia | _]} do
    assert {:ok, %{frames: frames, history: history}} = Games.replay(game, a.id)
    turns = Enum.count(history, &match?({:turn, _}, &1))
    assert length(frames) == turns + 1

    [first | _] = frames
    assert %{phase: %{age: 1, turn: 1, kind: :choosing_cards}, event_index: 0} = first
    assert length(first.hand) == 7
    assert first.my_choice == {:discard, hd(first.hand)}

    final = List.last(frames)
    assert %{phase: %{kind: :game_over}, event_index: nil, my_choice: nil, hand: []} = final

    for frame <- frames, frame.phase.kind == :choosing_cards do
      assert length(frame.hand) == 8 - frame.phase.turn
    end

    for frame <- Enum.drop(frames, -1) do
      assert {:turn, %{}} = Enum.at(history, frame.event_index)
    end

    assert Enum.all?(frames, fn frame -> Enum.any?(frame.players, &(&1.name == ia)) end)
  end
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd helios && mix test test/helios/games/replay_test.exs`
Expected: FAIL (no `frames` key).

- [ ] **Step 3: Implement**

In `games.ex` replace `replay/2`:

```elixir
  @doc """
  Rebuilds a finished game from its log in a throwaway engine: the viewer's
  final view, the public history, and one frame per turn (public boards plus
  the viewer's own hand and final choice that turn). `{:error, :not_finished}`
  if the log stops short of game over.
  """
  @spec replay(Game.t(), integer()) ::
          {:ok, %{view: map(), history: [term()], frames: [map()]}} | {:error, term()}
  def replay(%Game{} = game, user_id) do
    me = to_string(user_id)
    init = fn ref -> %{current: snapshot(ref, me), done: [], turns: 0, my_last: nil} end
    step = fn row, ref, acc -> frame_step(row, ref, acc, me) end

    with {:ok, ref, acc} <- Replay.rebuild(game, Replay.actions(game.id), init, step),
         {:ok, %{phase: %{kind: :game_over}} = view} <- Core.view(ref, me),
         {:ok, history} <- Core.history(ref) do
      {:ok, %{view: view, history: history, frames: Enum.reverse(acc.done, [acc.current])}}
    else
      {:ok, _view} -> {:error, :not_finished}
      {:error, reason} -> {:error, reason}
    end
  end

  defp frame_step(row, ref, acc, me) do
    acc =
      if to_string(row.user_id) == me do
        {:ok, action} = ActionCodec.decode(row.action)
        %{acc | my_last: action}
      else
        acc
      end

    {:ok, history} = Core.history(ref)
    turn_indexes = for {{:turn, _}, index} <- Enum.with_index(history), do: index

    if length(turn_indexes) > acc.turns do
      ended = %{acc.current | my_choice: acc.my_last, event_index: List.last(turn_indexes)}
      %{acc | done: [ended | acc.done], current: snapshot(ref, me), turns: length(turn_indexes), my_last: nil}
    else
      acc
    end
  end

  defp snapshot(ref, me) do
    {:ok, view} = Core.view(ref, me)

    %{
      players: view.players,
      hand: Enum.map(view.hand, & &1.name),
      phase: view.phase,
      my_choice: nil,
      event_index: nil
    }
  end
```

(Each accepted submit resolves at most one turn, so a growth of the `:turn` count is exactly one new turn. The viewer's choice is their **last** row before the turn resolved — a changed choice replaces the earlier one.)

- [ ] **Step 4: Run and commit**

Run: `cd helios && mix test test/helios/games/replay_test.exs test/helios_web/live && mix precommit`
Expected: PASS.

```bash
git add helios/lib/helios/games.ex helios/test/helios/games/replay_test.exs
git commit -m "feat(games): replay frames for finished games (public boards + my hand)"
```

---

### Task 7: Replay mode at game over

**Files:**
- Create: `helios/lib/helios_web/components/game_components/replay_bar.ex`
- Modify: `helios/lib/helios_web/components/game_components/hand.ex` (`replay_hand/1`), `helios/lib/helios_web/live/game_live.ex`, `e2e/tests/game.spec.ts`
- Test: `helios/test/helios_web/components/game_components/replay_bar_test.exs`, `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Consumes: `Games.replay/2` frames (Task 6), `TurnFx.payload/2` (Task 4), `age_summary` assign (Task 5), `HistoryDrawer` `frames` attr (Task 3).
- Produces: `ReplayBar.replay_bar/1` — attrs `frame`, `index`, `last`, `playing`. Ids `#replay-bar` (hook `.ReplayAutoplay`, `data-playing`), `#replay-first`, `#replay-prev`, `#replay-position`, `#replay-next`, `#replay-last`, `#replay-play`, `#replay-choice`, `#show-final-scores`. `Hand.replay_hand/1` renders `#replay-hand` with the `.HandRects` hook and the chosen card ringed. GameLive events `replay_first|prev|next|last|toggle`, `replay_goto` (`%{"frame" => index}`), `toggle_final_scores`.

- [ ] **Step 1: Write the failing tests**

`replay_bar_test.exs`:

```elixir
defmodule HeliosWeb.GameComponents.ReplayBarTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias HeliosWeb.GameComponents.ReplayBar

  defp frame(phase, choice \\ nil),
    do: %{phase: phase, hand: ["Altar", "Loom"], my_choice: choice, players: [], event_index: 0}

  test "position, controls, choice" do
    html =
      render_component(&ReplayBar.replay_bar/1,
        frame: frame(%{kind: :choosing_cards, age: 2, turn: 3}, {:discard, "Loom"}),
        index: 3,
        last: 20,
        playing: false
      )

    assert text(html, "#replay-position") == "Age II · Turn 3"
    assert text(html, "#replay-choice") =~ "You chose: Discard Loom"
    assert attrs(html, "#replay-next", "phx-click") == ["replay_next"]
    assert attrs(html, "#replay-bar", "data-playing") == ["false"]
    assert count(html, "#replay-prev[disabled]") == 0
  end

  test "the final frame says Final and disables forward controls" do
    html =
      render_component(&ReplayBar.replay_bar/1,
        frame: frame(%{kind: :game_over, age: 3, turn: 6}),
        index: 20,
        last: 20,
        playing: false
      )

    assert text(html, "#replay-position") == "Final"
    assert count(html, "#replay-next[disabled]") == 1
    assert count(html, "#replay-choice") == 0
  end
end
```

`game_live_test.exs`:

```elixir
  describe "replay of a finished game" do
    setup %{game: game, players: players} do
      ids = Enum.map(players, fn {user, _} -> to_string(user.id) end)
      {view_fun, submit_fun} = Helios.GameDriver.games_funs(game.id)
      {:game_over, _} = Helios.GameDriver.play_until(ids, %{}, view_fun, submit_fun, fn _ -> false end)
      stop_game_servers()
      :ok
    end

    test "starts on the final table and steps through turns", %{game: game, players: [{_a, ta} | _]} do
      {:ok, view, _html} = open(ta, game.id)
      assert has_element?(view, "#replay-position", "Final")
      assert has_element?(view, "#scoreboard")

      view |> element("#replay-first") |> render_click()
      assert has_element?(view, "#replay-position", "Age I · Turn 1")
      assert has_element?(view, "#replay-hand [data-card]")
      assert has_element?(view, "#replay-choice", "You chose: Discard")
      assert has_element?(view, "#my-board [data-stat='coins']", "3")
      refute has_element?(view, "#scoreboard")
      assert has_element?(view, "#show-final-scores")

      view |> element("#replay-next") |> render_click()
      assert_push_event(view, "turn_resolved", %{plays: [_, _, _]})
      assert has_element?(view, "#replay-position", "Age I · Turn 2")

      render_click(view, "replay_goto", %{"frame" => "6"})
      refute has_element?(view, "#age-summary")
      assert has_element?(view, "#replay-position", "Age II · Turn 1")

      render_click(view, "replay_goto", %{"frame" => "999"})
      render_click(view, "replay_goto", %{"frame" => "x"})
      assert has_element?(view, "#replay-position", "Age II · Turn 1")

      view |> element("#replay-last") |> render_click()
      assert has_element?(view, "#replay-position", "Final")
      assert has_element?(view, "#scoreboard")
    end

    test "stepping across the end of an age shows the conflicts", %{game: game, players: [{_a, ta} | _]} do
      {:ok, view, _html} = open(ta, game.id)
      render_click(view, "replay_goto", %{"frame" => "5"})
      assert has_element?(view, "#replay-position", "Age I · Turn 6")
      view |> element("#replay-next") |> render_click()
      assert has_element?(view, "#age-summary")
    end

    test "history entries jump to their frame", %{game: game, players: [{_a, ta} | _]} do
      {:ok, view, _html} = open(ta, game.id)
      assert has_element?(view, "#history-0 [phx-click='replay_goto'][phx-value-frame='0']")
    end
  end
```

(Discard-only 3-player games have no extra turns: frames 0–5 are Age I turns 1–6, frame 6 is Age II turn 1.)

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components/replay_bar_test.exs test/helios_web/live/game_live_test.exs`
Expected: FAIL.

- [ ] **Step 3: `replay_hand/1` in `Hand`**

```elixir
  attr :hand, :list, required: true, doc: "card names"
  attr :chosen, :string, default: nil

  def replay_hand(assigns) do
    ~H"""
    <section id="replay-hand" phx-hook=".HandRects" aria-label="Your hand that turn">
      <div class="-mx-2 flex justify-center-safe gap-2 overflow-x-auto px-2 pt-2 pb-1">
        <div
          :for={card <- @hand}
          data-card={card}
          class={["relative shrink-0 rounded-lg", card == @chosen && "ring-4 ring-amber-500"]}
        >
          <img
            src={GameAssets.card_path(card)}
            alt={card}
            class="h-[98px] w-16 rounded-lg object-cover sm:h-[134px] sm:w-22 short:h-[86px] short:w-14"
          />
          <span
            :if={card == @chosen}
            class="absolute left-1 top-1 rounded bg-amber-500 px-1.5 py-0.5 text-xs font-bold text-white"
          >
            Chosen
          </span>
        </div>
      </div>
    </section>
    """
  end
```

- [ ] **Step 4: `ReplayBar`**

```elixir
defmodule HeliosWeb.GameComponents.ReplayBar do
  @moduledoc "Turn-by-turn navigation through a finished game (frames from `Helios.Games.replay/2`)."
  use HeliosWeb, :html

  alias HeliosWeb.GameFormat

  attr :frame, :map, required: true
  attr :index, :integer, required: true
  attr :last, :integer, required: true
  attr :playing, :boolean, required: true

  def replay_bar(assigns) do
    ~H"""
    <div
      id="replay-bar"
      phx-hook=".ReplayAutoplay"
      data-playing={to_string(@playing)}
      class="flex flex-col gap-2 rounded-xl bg-antique/95 p-2 shadow sm:flex-row sm:items-center sm:justify-between"
    >
      <div class="flex items-center justify-center gap-1">
        <.control id="replay-first" event="replay_first" icon="hero-backward" label="First turn" disabled={@index == 0} />
        <.control id="replay-prev" event="replay_prev" icon="hero-chevron-left" label="Previous turn" disabled={@index == 0} />
        <span id="replay-position" class="min-w-36 text-center font-semibold text-zinc-900">{position(@frame.phase)}</span>
        <.control id="replay-next" event="replay_next" icon="hero-chevron-right" label="Next turn" disabled={@index == @last} />
        <.control id="replay-last" event="replay_last" icon="hero-forward" label="Final table" disabled={@index == @last} />
        <.control
          id="replay-play"
          event="replay_toggle"
          icon={if @playing, do: "hero-pause", else: "hero-play"}
          label={if @playing, do: "Pause", else: "Play"}
          disabled={@index == @last}
        />
      </div>
      <p :if={@frame.my_choice} id="replay-choice" class="text-center text-sm text-zinc-700">
        You chose: <strong>{GameFormat.describe_action(@frame.my_choice)}</strong>
      </p>
    </div>
    <script :type={Phoenix.LiveView.ColocatedHook} name=".ReplayAutoplay">
      export default {
        mounted() { this.sync() },
        updated() { this.sync() },
        destroyed() { clearInterval(this.timer) },
        sync() {
          const playing = this.el.dataset.playing === "true"
          if (playing && !this.timer) this.timer = setInterval(() => this.pushEvent("replay_next", {}), 2500)
          if (!playing && this.timer) { clearInterval(this.timer); this.timer = null }
        },
      }
    </script>
    """
  end

  attr :id, :string, required: true
  attr :event, :string, required: true
  attr :icon, :string, required: true
  attr :label, :string, required: true
  attr :disabled, :boolean, default: false

  defp control(assigns) do
    ~H"""
    <button
      id={@id}
      type="button"
      phx-click={@event}
      disabled={@disabled}
      aria-label={@label}
      class="inline-flex size-9 items-center justify-center rounded-full text-zinc-800 transition hover:bg-white disabled:cursor-not-allowed disabled:opacity-40 pointer-coarse:size-11"
    >
      <.icon name={@icon} class="size-5" />
    </button>
    """
  end

  def position(%{kind: :game_over}), do: "Final"
  def position(%{kind: :extra_turn, age: age, turn: turn}), do: "Age #{GameFormat.roman(age)} · Turn #{turn} · extra turn"
  def position(%{age: age, turn: turn}), do: "Age #{GameFormat.roman(age)} · Turn #{turn}"
end
```

- [ ] **Step 5: GameLive replay mode**

Add `import HeliosWeb.GameComponents.ReplayBar` and these pieces:

1. **Entering replay mode.** `mount_finished` success branch: `{:ok, socket |> assign(:boards_available, true) |> enter_replay(replay)}`. In `refresh/1`, after `assign_view(view)`: if `view.phase.kind == :game_over and is_nil(socket.assigns[:replay])`, call `Games.replay(socket.assigns.game, user.id)` and on `{:ok, replay}` pipe into `enter_replay/2` (on error keep the live final view; no replay bar). `setup/3` assigns `replay: nil, show_scores: false`.

```elixir
  defp enter_replay(socket, %{view: view, history: history, frames: frames}) do
    socket
    |> assign(history: history, show_scores: false)
    |> assign(:replay, %{frames: frames, index: length(frames) - 1, playing: false})
    |> assign_view(view)
    |> assign_frame()
  end

  defp current_frame(%{frames: frames, index: index}), do: Enum.at(frames, index)

  # The table shows the frame's public boards; @view keeps the final view (scores, me, neighbours).
  defp assign_frame(socket) do
    frame = current_frame(socket.assigns.replay)
    table = %{socket.assigns.view | players: frame.players}
    by_name = Map.new(frame.players, &{&1.name, &1})

    assign(socket,
      me: Map.fetch!(by_name, table.me),
      west: Map.fetch!(by_name, table.west),
      east: Map.fetch!(by_name, table.east),
      others: GameFormat.others(table),
      statuses: %{}
    )
  end

  defp frame_links(%{frames: frames}) do
    for {%{event_index: index}, frame} <- Enum.with_index(frames), index != nil, into: %{}, do: {index, frame}
  end
```

2. **Events.**

```elixir
  def handle_event("replay_" <> action, params, %{assigns: %{replay: %{} = replay}} = socket) do
    last = length(replay.frames) - 1

    socket =
      case {action, params} do
        {"first", _} -> goto(socket, 0)
        {"prev", _} -> goto(socket, max(replay.index - 1, 0))
        {"last", _} -> goto(socket, last)
        {"next", _} when replay.index < last -> step_forward(socket)
        {"next", _} -> update(socket, :replay, &%{&1 | playing: false})
        {"toggle", _} -> update(socket, :replay, &%{&1 | playing: not &1.playing and &1.index < last})
        {"goto", %{"frame" => frame}} -> goto_param(socket, frame, last)
        _ -> socket
      end

    {:noreply, socket}
  end

  def handle_event("toggle_final_scores", _params, socket),
    do: {:noreply, update(socket, :show_scores, &(!&1))}

  defp goto_param(socket, frame, last) do
    case Integer.parse(to_string(frame)) do
      {index, ""} when index >= 0 and index <= last -> goto(socket, index)
      _ -> socket
    end
  end

  defp goto(socket, index) do
    socket
    |> update(:replay, &%{&1 | index: index, playing: false})
    |> assign(age_summary: nil)
    |> assign_frame()
  end

  defp step_forward(socket) do
    %{replay: replay, history: history, view: view} = socket.assigns
    frame = current_frame(replay)
    turn = Enum.at(history, frame.event_index)
    next = Enum.at(replay.frames, replay.index + 1)
    pass = if next.phase.kind == :choosing_cards and next.phase.turn > 1, do: next.phase.direction

    socket =
      push_event(socket, "turn_resolved",
        TurnFx.payload(turn, %{me: view.me, my_card: GameFormat.action_card(frame.my_choice), pass: pass}))

    socket =
      case Enum.at(history, frame.event_index + 1) do
        {:battles, _} = battles -> assign(socket, :age_summary, battles)
        _ -> socket
      end

    last = length(replay.frames) - 1

    socket
    |> update(:replay, &%{&1 | index: &1.index + 1, playing: &1.playing and &1.index + 1 < last})
    |> assign_frame()
  end
```

(Put these clauses before the generic catch-alls; when `replay` is nil the `replay_*` events fall through to a clause `def handle_event("replay_" <> _, _params, socket), do: {:noreply, socket}`.)

3. **Rendering the game-over branch** (replacing the Phase C version):

```heex
          <%= if @view.phase.kind == :game_over do %>
            <div class="flex flex-col gap-3 pb-4 sm:gap-4">
              <.replay_bar
                :if={@replay}
                frame={current_frame(@replay)}
                index={@replay.index}
                last={length(@replay.frames) - 1}
                playing={@replay.playing}
              />
              <%= if is_nil(@replay) or @replay.index == length(@replay.frames) - 1 or @show_scores do %>
                <.scoreboard scores={@view.scores} names={@names} me={@view.me} lobby_id={@game.lobby_id} />
              <% else %>
                <button
                  id="show-final-scores"
                  type="button"
                  phx-click="toggle_final_scores"
                  class="mx-auto rounded-lg bg-white/80 px-3 py-1.5 text-sm font-semibold text-zinc-900 ring-1 ring-zinc-300 hover:bg-white pointer-coarse:min-h-11"
                >
                  Final scores
                </button>
              <% end %>
              <p :if={!@boards_available} id="boards-unavailable" class="mx-auto rounded-lg bg-antique/90 px-3 py-2 text-sm text-zinc-700">
                Boards are unavailable for this game
              </p>
              <.table_area
                :if={@boards_available}
                me={@me}
                west={@west}
                east={@east}
                others={@others}
                names={@names}
                statuses={@statuses}
              />
              <div :if={@replay && current_frame(@replay).hand != []} class="sticky bottom-0 z-30 rounded-t-xl bg-antique/95 p-2 shadow">
                <.replay_hand
                  hand={current_frame(@replay).hand}
                  chosen={GameFormat.action_card(current_frame(@replay).my_choice)}
                />
              </div>
              <.card_sheet … as in Phase C … />
            </div>
```

The top bar in replay mode shows the frame's phase with no "waiting for": pass `view={if @replay, do: %{@view | phase: current_frame(@replay).phase, submitted: []}, else: @view}` to `<.top_bar>`.

The drawer gets the frame links: `frames={if @replay, do: frame_links(@replay), else: %{}}`.

4. **e2e.** In `e2e/tests/game.spec.ts` "three players play a full game to the scoreboard", after opening the game from Past games (Phase C step):

```ts
  await expect(owner.page.locator("#replay-position")).toHaveText("Final");
  await owner.page.locator("#replay-first").click();
  await expect(owner.page.locator("#replay-position")).toHaveText("Age I · Turn 1");
  await expect(owner.page.locator("#replay-hand [data-card]")).toHaveCount(7);
  await owner.page.locator("#replay-next").click();
  await expect(owner.page.locator("#replay-position")).toHaveText("Age I · Turn 2");
```

- [ ] **Step 6: Phase gate and commit**

```bash
cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cd ../helios && mix precommit
cd ../e2e && npx playwright test
```

Expected: all green. Then check by hand once in a browser (reduced motion **off**): a finished game opens on "Final"; ⏮ shows turn 1 with your hand; ▶ plays turns every 2.5 s with animations and pauses at the end-of-age summary row reveal; the history drawer's turn titles jump to their frames.

```bash
git add helios/lib/helios_web/components/game_components/replay_bar.ex helios/lib/helios_web/components/game_components/hand.ex helios/lib/helios_web/live/game_live.ex helios/test/helios_web/components/game_components/replay_bar_test.exs helios/test/helios_web/live/game_live_test.exs e2e/tests/game.spec.ts
git commit -m "feat(web): step through a finished game turn by turn"
```
