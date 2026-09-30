# Phase 3 — Engine Gameplay API (Rust)

Status: approved design (2026-09-23). Overview: `2026-09-23-migration-overview-design.md`. Depends on Phase 0 only; can run in parallel with Phases 1–2.

## Goal

Turn `core` (crate `seven_wonders_core`) into a complete, deterministic 7 Wonders base-game engine (3–7 players, all wonder abilities) with a per-player view API, exposed to Elixir through native-term NIFs. All rules live here; Elixir never re-implements them.

## Current state (after Phase 0)

Exists: static card/wonder data (`engine/data.rs`), deck generation (`engine/deck.rs`, `thread_rng`), game init (`engine/game_init.rs`), effects as boxed closures, `GameState::apply_player_decisions` (applies build/wonder/discard/free-build **without any checks**), `cover_resource_costs`, `try_trading` (computes options, no coins moved), scoring (`calculate_points`), 37 tests.

Missing / wrong (all fixed in this phase, each with a failing test first):
- No hands dealt (`current_age_cards` never filled), no card-in-hand check, no passing, no 7th-card discard, no battles, no age progression, no game over, no tiebreak.
- Coin cost never charged; resource costs never checked; chain (free) builds not handled at decision time; duplicate structures not blocked; wonder stage past the last one panics; wonder stage costs unchecked; trade payments never transferred.
- Tradability: every produced resource is tradable today. Rule: neighbours may buy only resources produced by **brown/grey (RM/MG) structures and the wonder's starting resource**; yellow-card and wonder-stage "choice" resources are owner-only.
- Babylon B "play 7th card" only sets a flag; Halikarnassós only pushes an event; Olympía B "copy guild" only sets a flag.
- Data: Baths gives nothing (`allre(&[])`) — must be 3 civilian points.
- `Neighbours::get_player_name_from_direction` swaps East/West labels.
- Coins are `u8` (overflow risk in dynamic coin effects) → use `u32` for coins, `i32` for points/tokens.
- Randomness from `thread_rng`; `HashMap` iteration used in places that may affect outcomes.

## Design

### Determinism
- All randomness (deck shuffles, guild selection, random wonder assignment and sides) flows from a single `rand_chacha::ChaCha8Rng::seed_from_u64(seed)` owned by the `Game`, consumed in a fixed order. `ChaCha8Rng` output is stable across crate versions (unlike `StdRng`).
- Players are held in seat order (`Vec`). Any outcome-affecting iteration uses `Vec` or `BTreeMap`; `HashMap` is allowed only for lookups whose iteration order never matters. Audit `game_state.rs`, `deck.rs`, `game_init.rs`, `trading.rs`, `structure_builder.rs`.
- `pub const ENGINE_VERSION: u32 = 1;` — bump on any rule change that could alter replays. Exposed via `game_settings`.
- Test: same `(players, wonders, seed, actions)` run twice → identical `view` for every player at every step; plus a test that a known seed produces a pinned deal (guards accidental RNG-order changes).

### Seating & directions
- Seat order is the `players` order given to `Game::new`. For seat `i` of `n`: `west = (i + n - 1) % n`, `east = (i + 1) % n`.
- Hands pass to the **west** neighbour in Ages I and III and to the **east** neighbour in Age II. (Only consistency matters; this convention is fixed by tests.)

### Public Rust API (`src/game/`, no rustler imports)
```rust
pub struct Game { /* state, rng, phase, pending actions, seats */ }

pub enum WonderSelection { Random, Explicit(Vec<(String /*wonder name*/, Side)>) }
pub enum Side { A, B }

impl Game {
    pub fn new(players: Vec<String>, wonders: WonderSelection, seed: u64) -> Result<Game, SetupError>;
    pub fn submit(&mut self, player: &str, action: Action) -> Result<(), ActionError>;
    pub fn phase(&self) -> &Phase;
    pub fn view(&self, player: &str) -> Result<PlayerView, ActionError>; // UnknownPlayer
}

pub enum Phase {
    ChoosingCards { age: u8, turn: u8 },               // all players act
    ExtraTurn { player: String, kind: ExtraTurnKind }, // only `player` acts
    GameOver { scores: Vec<FinalScore> },
}
pub enum ExtraTurnKind { PlayLastCard, BuildFromDiscard }

pub enum Action {
    Build { card: String, payment: Payment },
    BuildWonderStage { card: String, payment: Payment },
    Discard { card: String },
    BuildFree { card: String },         // Olympía A, once per age
    BuildFromDiscard { card: String },  // Halikarnassós extra turn only
}
pub struct Payment { pub west: Vec<(ResourceType, u8)>, pub east: Vec<(ResourceType, u8)> }

pub enum SetupError { InvalidPlayersNumber(usize), DuplicatePlayer(String), InvalidWonder(String), WondersLengthMismatch { players: usize, wonders: usize }, DuplicateWonder(String) }

pub enum ActionError {
    UnknownPlayer, NotYourTurn, GameOver,
    CardNotInHand, CardNotInDiscard, AlreadyBuilt,
    CannotAfford, InvalidPayment, NoWonderStageLeft,
    FreeBuildUnavailable, ActionNotAllowedNow,
}
```

### Submission & resolution
- `submit` validates the action fully against the current state (see Legality) and stores it as the player's **pending** action; resubmitting replaces it. Submitting when it's not the player's turn → `NotYourTurn`; during `GameOver` → `GameOver`.
- When every player required by the phase has a pending action, the turn resolves immediately inside the same `submit` call:
  1. Apply all pending actions "simultaneously": remove chosen cards from hands; for Build/Wonder: charge coin cost to the bank, pay trade coins to neighbours (credited after all actions are applied — coins received this turn cannot have been spent this turn because validation used pre-turn balances), record structure / increment wonder stage (card tucked, counted as used), run effects; Discard: +3 coins, card to discard pile; BuildFree: mark the age's free build used.
  2. If a Halikarnassós stage with build-from-discard was built this turn and the discard pile has a card that player hasn't built → phase becomes `ExtraTurn { BuildFromDiscard }` for that player (after step 3's end-of-age discards if this was the age's last turn, so those cards are eligible). If no eligible card, skip.
  3. If hands now hold 1 card each (end of turn 6): if a player has Babylon B's play-last-card ability → `ExtraTurn { PlayLastCard }` for them (they may Build / BuildWonderStage / Discard their last card); all other last cards go to the discard pile. Otherwise all last cards are discarded.
  4. Else pass hands (direction by age) and advance `turn`.
  5. End of age (after all extra turns): military battles — for each player vs each neighbour, more shields → +1/+3/+5 token (Age I/II/III), fewer → −1 token, equal → nothing. Then deal the next age (7 cards each) or finish the game.
- Game over: compute final scores (below); `Phase::GameOver`.

### Legality (single source for both `submit` validation and `view` options)
- **Build**: card in hand; structure with the same name not already built (`AlreadyBuilt`); cost resolved in this order: (a) free via chain (a built structure lists it as a dependent) → cost 0, payment must be empty; (b) coin-only cost → coins ≥ cost; (c) resource cost: own production (including owner-only choice resources) covers the remainder after the payment's purchases; purchases must be producible by that neighbour from **tradable** resources; unit price per resource from the buyer's trade actions (2 default, 1 with discounts); coins ≥ coin cost + trade total. Violations → `CannotAfford` (no payment can work) or `InvalidPayment` (this payment doesn't work).
- **BuildWonderStage**: card in hand; next stage exists (`NoWonderStageLeft`); stage resource cost covered as above.
- **Discard**: card in hand. Always legal otherwise.
- **BuildFree**: player has Olympía A's ability and hasn't used it this age; card in hand; not already built.
- **BuildFromDiscard**: only in `ExtraTurn { BuildFromDiscard }` for that player; card in discard pile; not already built. The card is removed from the discard pile.
- **Payment options** for the view: enumerate valid purchase combinations via the existing `cover_resource_costs` + `try_trading` (restricted to tradable resources), compute total coins per neighbour, drop options the player can't afford, dedupe, sort by total cost ascending then by `west` total ascending, keep at most 6.

### Wonder abilities
- Olympía A stage 2 — `BuildFree` once per age (existing `ages_can_build_free_in`).
- Olympía B stage 3 — at game end, copy one guild built by either neighbour; the engine picks the guild that maximises the player's score (ties → alphabetical name). No player choice needed.
- Halikarnassós (A stage 2, B stages 1–3) — `ExtraTurn { BuildFromDiscard }` as above.
- Babylon B stage 2 — `ExtraTurn { PlayLastCard }` at the end of each age (from the age in which the stage is built onward).
- Existing effects (Rhódos, Alexandria, Éphesos, Gizah, Babylon A science) unchanged except for tradability flags.

### Scoring
`FinalScore { player, military, treasury, wonder, civilian, scientific, commercial, guild, total, coins, rank }`. Treasury = coins / 3. Science = Σ squares + 7 × complete sets, choice symbols assigned optimally (existing). Rank by `total` desc, then `coins` desc; equal on both → shared rank.

### PlayerView
```rust
pub struct PlayerView {
    pub me: String,
    pub phase: PhaseView,                 // age, turn, pass direction, extra-turn player/kind, or game over
    pub players: Vec<PublicPlayer>,       // seat order
    pub west: String, pub east: String,
    pub hand: Vec<HandCard>,              // empty when not my turn to act with a hand
    pub discard_pile: Option<Vec<String>>,// Some only during my BuildFromDiscard extra turn
    pub discard_count: usize,
    pub submitted: Vec<(String, bool)>,   // who has a pending action (never what)
    pub my_pending: Option<Action>,       // my own pending choice, for UI highlighting
    pub scores: Option<Vec<FinalScore>>,  // Some at game over
}
pub struct PublicPlayer { name, wonder: String /*e.g. "Gizah"*/, side: Side, stages_built: u8, stages_total: u8,
    built: Vec<BuiltCard /* name, category, age */>, coins: u32, shields: u32, military_tokens: Vec<i32>, free_build_available: bool }
pub struct HandCard { name: String, category: Category, age: u8,
    build: BuildOption, wonder_stage: BuildOption, free_build: bool }
pub enum BuildOption { Unavailable { reason: ActionError }, Free /* chain or no cost */, Coins(u32), Trade(Vec<PaymentOption>) }
pub struct PaymentOption { payment: Payment, west_coins: u32, east_coins: u32, bank_coins: u32 }
```
Invariant (tested): every option the view marks available is accepted by `submit`, and `submit` rejects cards the view marks `Unavailable` with the stated reason.

### NIF layer (`src/nif/`)
Boundary DTOs mirror the Rust types with `#[derive(NifMap | NifStruct | NifTaggedEnum | NifUnitEnum)]` and `From` conversions; the `game` module stays rustler-free. Resource: `GameResource(Mutex<Game>)`.

| Elixir (`Helios.Core.Native`) | Returns |
|---|---|
| `game_settings()` | `%{engine_version: 1, wonders: [%{name, sides: [:a, :b]}], cards: [%{name, category, age}]}` |
| `new_game(players, wonders, seed)` — `wonders` is `:random` or `{:explicit, [{name, :a \| :b}]}`; `seed` non-negative integer < 2^64 | `{:ok, ref}` \| `{:error, reason}` (`:invalid_players_number`, `{:duplicate_player, name}`, `{:invalid_wonder, name}`, …) |
| `submit(ref, player, action)` — action e.g. `{:build, %{card: "Altar", payment: %{west: [], east: [{:wood, 1}]}}}`, `{:discard, "Altar"}` | `:ok` \| `{:error, atom}` (snake_case of `ActionError`) |
| `view(ref, player)` | `{:ok, map}` \| `{:error, :unknown_player}` |
| `debug_game(ref)` | `{:ok, json_string}` |

Resources and categories are atoms (`:wood, :stone, :ore, :clay, :glass, :loom, :papyrus`; `:civilian, :commercial, :guild, :manufactured_good, :military, :raw_material, :scientific`). `start_game/2` from Phase 0 is removed; `Helios.Core` wrapper is updated to the new functions with specs and ExUnit tests (new_game ok/errors; submit discard for all 3 players advances the turn; view shape).

Locking: NIFs take the mutex with `lock()` (not `try_lock`); calls are serialized per game by the GameServer anyway. Long operations are not expected (< 1 ms), so no dirty schedulers.

## Testing (`cargo test`)
- Unit tests per rule above (costs, chains, duplicates, trading tradability & discounts & coin transfer timing, wonder stage limits, battles per age, passing directions, 7th-card discard, each wonder ability, scoring & tiebreak, Baths).
- Scenario tests on fixed seeds with explicit wonders.
- Determinism tests (see above).
- **Simulation**: for n in 3..=7 and ≥ 20 seeds each, play a full game choosing uniformly among the view's available options (seeded RNG); assert: every view-available action is accepted; hand sizes are 7 at age start and decrease by 1 per turn; card conservation (built + wonder-tucked + discarded + in hands = dealt); coins never negative; the game reaches `GameOver` with scores for every player.
- `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`.

## Acceptance criteria
- All tests above pass; Helios `Helios.Core` tests pass against the new NIF.
- A full random game for 3–7 players completes via the NIF from Elixir (ExUnit test mirroring the simulation for n = 3 and 7).

## Amendments from planning (2026-09-23)
- Payment options are no longer derived from `cover_resource_costs` + `try_trading` (factorial and incomplete). New `ResourcesProduced::can_produce` (bipartite matching) + `game::payment` enumeration; the legacy trading code and its tests are replaced, scenarios ported.
- The RNG lives only in `Game::new`; shuffle/sampling implemented locally over `ChaCha8Rng::next_u64` so dependency upgrades never change replays. `rand` is removed.
- Additional data fixes: Magistrates Guild is 1 VP per neighbouring blue card; Courthouse has no dependents (Senate chains only from Library).
- End of age with two extra turns: Babylon B's play-last-card resolves before Halikarnassós's build-from-discard.
- Elixir shapes: `{:unavailable, reason_atom}`, bare `:invalid_players_number`, flat `phase` map; final scores sorted by rank; `debug_game` JSON nests the state under `"state"`.
