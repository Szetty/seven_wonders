# Game table UX — explanations, everyone's boards, history, replay

Status: approved design (2026-10-04). Builds on the Phase 4 game table (`docs/superpowers/specs/2026-09-23-migration/2026-09-23-phase-4-game-design.md`) and the responsive UI (`docs/superpowers/specs/2026-09-30-responsive-ui/2026-09-30-responsive-ui-design.md`). Mockups from the brainstorming session are in `.superpowers/brainstorm/` (git-ignored; not needed to implement).

## Goal

The table explains itself and shows the whole game: every card and wonder stage can be read, every player's city is visible, each turn visibly happens (cards land, coins move, conflicts resolve), and finished games can be browsed and replayed turn by turn.

Success criteria:
- A player can learn what any card or wonder stage does, and what it would cost them, without leaving the table.
- Every player's built structures, wonder stages, coins, military strength and tokens are visible during and after the game.
- After each turn the player sees what everyone did; after each age, the military conflicts.
- Finished games are listed under "Past games" and can be stepped through turn by turn.
- All existing ExUnit and Playwright suites pass (updated where ids/text change), plus the new tests below.

## Requested changes → where they land

| # | Request | Phase |
|---|---|---|
| 1 | Pointer cursor on interactive game elements | A |
| 2 | Centre the hand cards horizontally | A |
| 3 | "Shields" icon and text don't match the game | A |
| 4 | Click the card art in the sheet to see it bigger | A |
| 5 | Explain a card's effects | B |
| 6 | Show who has chosen and who hasn't | A |
| 7 | Show every player's built structures | C |
| 8 | Show trading options only after choosing the action | B |
| 9 | Click wonder stages of any player for an explanation | B |
| 10 | Animate the turn: cards from hand to board, coins received | D |
| 11 | Animate the end of an age: military tokens | D |
| 12 | Page background extends behind the site header | A |
| 13 | Chain builds read "Free (you have …)" | B |
| 14 | Player names instead of West/East in trades | A |
| 15 | Player name in the pass direction | A |
| 16 | Remove the confusing "Bank" from trade options | A |
| 17 | Built wonder stages on the top edge of the wonder image | A |
| 18 | Cheapest-way badge in the corner of hand cards | B |
| 19 | See the boards while building from the discard pile | C |
| 20 | Golden border on each board's latest built structure | C |
| 21 | See the boards at the end of the game | C |
| 22 | Game history | D |
| 23 | Past games (added during design) | C (list), D (replay) |

## Decisions

- **Four phases, in order A → B → C → D.** Each ends with all checks green (core, helios, e2e) and is shippable on its own. D's animations land on C's boards.
- **Card and wonder explanations come from the engine** (no hand-written rules text in Elixir). Each effect carries a data description (`EffectSpec`) next to its closure; Helios turns specs into English.
- **History is recorded by the engine** in memory while resolving turns (`Game.history`), rebuilt for free by the existing seed + action-log replay. Nothing new is persisted; no migration.
- **Finished games are rebuilt on demand** in a throwaway engine (seed + action log), never through a `GameServer`. If replay fails, the page falls back to today's scoreboard.
- **Replay secrecy = live secrecy:** public information plus the viewer's own hand and choices. Other players' hands, discarded cards and the cards under wonder stages are never sent to the browser.
- **No rule change → `ENGINE_VERSION` stays 1.** The `Effect` refactor and the history must not alter any outcome; the existing determinism/simulation tests prove it.
- **Hidden information:** the history is public-only and identical for every player; it is exposed by a NIF with no player argument.
- **Table layout = "everyone's full board"**: West · You · East on top, the other players' full boards below.
- **Free badge = the green checkmark token** (`tokens/check.png`), not an arrow.
- **Past games** = a header link to a new `/games` page.
- **JS**: LiveView colocated hooks only (LiveView 1.1); every animation is skipped under `prefers-reduced-motion`; Playwright runs with reduced motion.
- **DOM ids** used by tests stay unless listed under "Id changes" below.

## Phase A — Quick fixes (Helios only)

1. **Cursor.** Base-layer rule in `assets/css/app.css` (the snippet Tailwind v4 documents): `button:not(:disabled), [role="button"]:not([aria-disabled="true"]), a[href], label[for], summary { cursor: pointer; }`. Disabled buttons keep `cursor-not-allowed`.
2. **Centring.** The hand row (`#hand`) and the discard picker grid use `justify-center-safe` (Tailwind 4.1): centred when the row fits, left-aligned and scrollable when it overflows (first card never clipped).
3. **Military strength.** New `<.military_icon class=…>` function component (in `GameComponents.Icons`, new module): inline SVG of a red heater shield with a gold rim, matching the card symbol. Replaces every `hero-shield-check`. Tooltips/labels read "Military strength". `data-stat="shields"` → `data-stat="military"`.
4. **Card zoom.** The card art in `#action-panel` becomes a button (`#zoom-card`) that shows `#card-zoom` (fixed, `z-60`, black/80 scrim, art centred). Client-only: `JS.show`/`JS.hide`; closes on click, Esc (`phx-window-keydown` + `phx-key="Escape"` via `JS`), or ✕ (`#close-card-zoom`). The art is 180×275 px, so the zoom is capped at ~1.6× (`max-h-[min(440px,85dvh)]`); sharper art is out of scope.
5. **Chosen / Thinking.** From `view.submitted`: a chip in every player panel header (`data-status="chosen"|"thinking"`, ids `#status-<player>`), and a ✓ next to the name in the top-bar `#connections` list. During an extra turn only the acting player shows "Playing…" (`data-status="playing"`); at game over no chips. The existing `#waiting-for` / `#waiting-count` stay unchanged.
6. **Background.** `Layouts.app` gets `attr :background, :string, default: nil` (a class string) applied to its outermost div, so the site header sits on it. `GameLive` passes the paper background (`bg-[url('/images/paper.jpg')] bg-cover lg:bg-fixed`) and `#game` drops its own.
7. **Names instead of West/East.**
   - Pass direction: "Pass ← to Bob" / "Pass → to Cid" (`#pass-direction`; arrow icon kept, name from `view.west`/`view.east` depending on `phase.direction`).
   - Neighbour panel labels: "Bob · West" (label keeps the direction as secondary text).
   - Trade option buttons: "Bob 2 · Cid 1" (neighbour with 0 coins omitted). Phase B replaces these buttons again (expandable list); A only changes the text.
8. **Bank.** Option buttons no longer show bank coins. When the card has a coin cost, the option group shows it once above the buttons: "+1 coin card cost" (`#build-card-cost`). (Answer to the request: "bank" was the card's printed coin cost, e.g. Tree Farm, paid to the bank; identical across options.)
9. **Stages on top.** `#wonder-stages` moves to `inset-x-0 top-0` with the gradient flipped (`bg-linear-to-b from-black/50`), so the stage information printed along the bottom of the board stays visible.

Phase A tests (ExUnit, LiveView): status chips for submitted/not submitted/extra turn; ✓ in connections; pass-direction text contains the neighbour's name; trade label uses names and no "Bank"; `#build-card-cost` only for coin-cost cards; `#card-zoom` present and toggled by `#zoom-card`; `#wonder-stages` has the top-positioning class; `data-stat="military"`; `Layouts.app` background attr. E2e: update text/`data-stat` matchers that change.

## Phase B — Explanations and costs

### Core

**`EffectSpec`** (`core/src/domain/effect_spec.rs`, plain data, `serde::Serialize`, no rustler). Variants, one per constructor in `game_state.rs`:

| Constructor | Spec |
|---|---|
| `all_resources_effect` | `Produces { resources, choice: false, tradable: true }` |
| `any_resources_effect` | `Produces { resources, choice: true, tradable: true }` |
| `owner_any_resources_effect` | `Produces { resources, choice: true, tradable: false }` |
| `point_effect` | `Points { category, points }` |
| `military_effect` | `Military(n)` |
| `all_scientific_effect` / `any_scientific_effect` | `Science { symbols, choice }` |
| `coin_effect` | `Coins(n)` |
| `trade_effect` | `TradeDiscount { directions, resources }` |
| `dynamic_coin_effect` | `CoinsPerCard { directions, categories, coins }` |
| `dynamic_point_effect` | `PointsPerCard { category, directions, categories, points }` |
| `dynamic_wonder_coin_effect` | `CoinsPerWonderStage { directions, coins }` |
| `dynamic_wonder_point_effect` | `PointsPerWonderStage { category, directions, points }` |
| `dynamic_battle_lost_point_effect` | `PointsPerDefeat { category, directions, points }` |
| `play_last_card_effect` | `PlayLastCard` |
| `construct_free_effect` | `BuildFreeOncePerAge` |
| `copy_guild_effect` | `CopyGuild` |
| `build_free_from_discarded_cards_effect` | `BuildFromDiscard` |

**`Effect`** changes from `Box<dyn Fn(&mut GameState, PName) + Sync>` to `pub struct Effect { pub spec: EffectSpec, apply: Box<dyn Fn(&mut GameState, PName) + Sync> }` with `pub fn apply(&self, state, player)`. Constructors build both; `data.rs` call sites are unchanged. The few appliers (`game_state.rs`, `scoring.rs`) call `effect.apply(…)`. Pure refactor.

**Catalogue in `game_settings`** (additive; existing fields keep their shape):
- `CardInfo` gains `cost: CostInfo { coins, resources: Vec<(ResourceType, u8)> }` (resource costs summed per type), `chains_from: Vec<String>` (dependencies), `chains_to: Vec<String>` (dependents), `effects: Vec<EffectSpec>`. Note `Structure::dependents()` is `#[cfg(test)]` today and must be made public.
- `WonderInfo` gains `boards: Vec<WonderBoardInfo { side, effects (starting resource), stages: Vec<StageInfo { cost: CostInfo, effects }> }>`. `sides` stays.
- Loom, Glassworks and Press appear in Ages I and II with identical cost/effects; consumers may look cards up by name.

**Chain option.** `legality::Requirement` gains `Chain(Card)` (the built prerequisite: the first of `card.dependencies()` that the player has built, in dependency order — deterministic). `structure_requirement` returns it instead of `Free` for chain builds; `settle` treats it like `Free` (empty payment). `BuildOption` gains `Chain(String)`; DTO `{:chain, "Altar"}`. Wonder stages never chain.

**NIF/DTOs** (`core/src/nif/dto.rs`): `EffectSpecDto` (`NifTaggedEnum`), `CostInfoDto`, `WonderBoardInfoDto`, `StageInfoDto`, `PointCategoryDto` (`:civilian`, `:commercial`, `:guild`, `:wonder`, …), `ScienceSymbolDto` (`:compass`, `:gears`, `:tablet`), `DirectionDto` reused/extended with `:own` for effect directions (separate `EffectDirectionDto` if cleaner). `BuildOptionDto` gains `Chain(String)`. `core/src/tests/nif_dto.rs` and `helios/test/helios/core_shapes_test.exs` pin the new shapes.

### Helios

- **`Helios.Catalogue`**: loads `Core.game_settings()` once into `:persistent_term` (lazily on first call); `card(name)`, `wonder_board(wonder, side)`, `stage(wonder, side, n)`.
- **`Helios.Games.Choice`**: `available?({:chain, _})` → true; `payment({:chain, _}, _)` → empty payment.
- **`HeliosWeb.CardText`** (pure): `effects(specs, context)` → list of short English lines (with resource/coin/symbol token icons rendered by a component). `context` optionally names the owner's neighbours (`%{west: "Bob", east: "Cid"}`), else "West neighbour"/"East neighbour"/"you". Examples: "Produces wood or clay", "Produces wood or clay (only for you, not for neighbours)" for `tradable: false`, "+2 military strength", "Worth 3 victory points", "Science: compass", "Any one science symbol of your choice", "Buy wood, stone, ore, clay from Bob for 1 coin", "1 coin per brown card in your city and your neighbours' (once, when built)", "1 victory point per brown card your neighbours have (at game end)", "Play your last card of each age", "Build one structure per age for free", "Copy a guild from a neighbour (at game end)", "Build a card from the discard pile for free". Also `cost(cost_info)` and `chains(card, my_built)`.
- **Hand badge** (`#hand-card-N` gets a child `.cost-badge` with `data-cost`): `GameFormat.cheapest(card.build)` →
  - `:free` / `{:chain, _}` → green badge with `tokens/check.png` (`data-cost="free"`), tooltip "Free" / "Free (you have Altar)";
  - `{:coins, n}` → "n" + coin icon (`data-cost="n"`); for `{:trade, options}` n = min over options of `west_coins + east_coins + bank_coins`;
  - `{:unavailable, reason}` → grey ✕, card dimmed (`data-cost="unavailable"`), tooltip = `Games.error_message(reason)`.
  The badge describes building the card only (not the wonder stage, not Olympía's free build).
- **Action sheet** (`#action-panel`):
  - Explanation box `#card-info`: effects (`CardText`), cost (resource icons + coins), "Free with: Altar ✓/✗" (✓ if in the player's built list), "Leads to: Pantheon".
  - Action rows: `#build-action`, `#wonder-action` ("Build wonder stage N"), `#build-free-button` (Olympía, unchanged), `#discard-button` ("Discard · +3 coins"). Each row shows its cheapest cost on the right ("Free (you have Altar)", "2 coins", "from 2 coins ▾", or the disabled reason).
  - A row whose option is `:free`, `{:chain, _}`, `{:coins, _}` or `{:trade, [single]}` submits on tap (`phx-value-option="0"`).
  - A row with several trade options toggles (client-side `JS.toggle`) a list `#build-payments` / `#wonder-payments` of buttons `#build-option-N` / `#wonder-option-N`, where N is the **engine's option index** (`Choice` unchanged). Displayed sorted by total coins, each reading "Buy [ore] from ← Bob · [wood] from Cid → · 4 coins"; the card's coin cost appears once in the list header ("includes 1 coin card cost").
- **Stage popover.** The stage strip (a shared `stage_strip` component) is added to every wonder image rendered in Phase B (own board and both neighbour panels; Phase C's `player_board` carries it to every player). Every slot is a button `#stage-<player>-<n>` (`aria-haspopup="dialog"`), toggling `#stage-info-<player>-<n>` (client-side `JS.toggle`, closes on outside click/Esc): "Gizah A · Stage 2 · built/not built", cost, effects.

Phase B tests:
- Core: every card and every wonder stage/board has ≥ 1 effect spec; spot checks (Tavern → `[Coins(5)]`, Haven → `[CoinsPerCard, PointsPerCard]`, Rhódos B stage 1 → `[Military(1), Points(3), Coins(3)]`); `chains_from`/`chains_to` for Altar/Temple; `Chain` returned for Temple with Altar built and for Forum via either trading post; chain build submits with empty payment; existing simulation/determinism tests unchanged and green.
- Helios: `CardText` renders every catalogue card and stage to non-empty text without raising; `GameFormat.cheapest/1` for each option shape; LiveView: badges per hand card, `#card-info` content, single-option row submits, multi-option row reveals `#build-option-N` and submitting one uses the right engine index, stage popover content for a neighbour's stage.
- E2e: `e2e/tests/support/game.ts` build helper clicks `#build-action`, then `#build-option-0` if `#build-payments` became visible; `game.spec.ts` likewise.

## Phase C — Everyone's boards

### Table layout (`#table`)

- Row 1: `#west-panel` · `#my-board` · `#east-panel` (`lg:grid-cols-[1fr_1.6fr_1fr]`; `md`: two columns with `#my-board` spanning both, first).
- Row 2: `#other-players`, the remaining players' **full boards** as `article#player-<id>` in seat order continuing from the east neighbour around the table; grid `md:grid-cols-2 lg:grid-cols-4`.
- Phones (< `md`): `#my-board` first, then West, East and others as foldable summaries using the existing toggle pattern (`#<panel>-toggle` / `#<panel>-details`, `JS.toggle_class("is-open")`), extended to every non-own board.

### `player_board` component (`GameComponents.PlayerBoard`, replaces `Board.my_board`, `neighbour_panel` details and `others_strip` cards)

Attrs: `player`, `names`, `role` (`:me | :west | :east | :other`), `status`, `context` (neighbour names for `CardText`). Renders: header (name, role label, status chip), wonder image with the stage strip on top (Phase A/B), stats row (coins `data-stat="coins"`, military `data-stat="military"`, stages, tokens `data-tokens`), built cards grouped by category columns as card-top tiles (today's own-board style) for every player.
- Built tiles are buttons `#built-<player>-<slug>` with `data-card`; tapping opens a read-only card sheet `#card-sheet` (art with zoom, `#card-info` from Phase B, no actions).
- **Latest built**: the last element of `player.built` gets `data-latest` and a gold ring/glow; it stays until that player builds another structure.

### Discard pick (Halikarnassós)

`#discard-picker` gets `#discard-minimise` ("Look at the boards"). Minimised (server assign `discard_minimised`, survives live updates), the sheet and scrim disappear and the dock shows a bar `#discard-restore`: "Pick a card from the discard pile · Show". Picking is unchanged (tap a card).

### End of game

- `phase.kind == :game_over`: the page shows `#scoreboard`, then the full table read-only (no dock, no chips).
- Live (GameServer alive): boards from its final view.
- Loaded later (`status: "finished"`): `Games.replay(game, user_id)` (new) builds a throwaway engine from `seed`, `wonders` and the action log (the same replay code path `GameServer.init` uses, extracted into a shared function) and returns `{:ok, %{view: final_view}}` in Phase C (extended in D). On any error → scoreboard only (from `final_scores`, today's behaviour) plus a note `#boards-unavailable` "Boards are unavailable for this game".

### Past games (`/games`)

- Site header link `#past-games-link` ("Past games") next to "My table".
- `HeliosWeb.PastGamesLive` in the authenticated `live_session`; paper background via `Layouts.app background`.
- `Games.finished_games_for(user, limit \\ 50)`: games with `status == "finished"` where the user has a `game_players` row, ordered by `finished_at desc`, preloading players + users. Portable Ecto query.
- Rows `#past-game-<id>`: finished date, players (trophy on rank 1), the user's rank and total (from `final_scores`), linking to `/game/:id`. Empty state `#past-games-empty`.

Phase C tests: LiveView table with 3, 5 and 7 players (count and order of `#other-players article`), `data-latest` on the right tile per board, read-only card sheet from a neighbour's tile, discard minimise/restore across a live update, finished game rendered via replay (boards present), replay failure fallback (`#boards-unavailable`, scoreboard present), `finished_games_for/1` (only own, only finished, newest first, limit), `PastGamesLive` rows/empty state, header link. E2e: `responsive.spec.ts` stays green (other boards remain `article`s in `#other-players`; neighbour toggles unchanged); new spec: finish a game, open Past games, open the game, boards visible.

## Phase D — History and motion

### Core

```rust
pub enum HistoryEvent {
    Turn { age: u8, turn: u8, kind: TurnKind, plays: Vec<Play> },
    Battles { age: u8, battles: Vec<Battle> },
}
pub enum TurnKind { Regular, PlayLastCard, BuildFromDiscard }
pub struct Play {
    pub player: String,
    pub action: PublicAction,
    pub coins_to_west: u32,
    pub coins_to_east: u32,
    pub coins_delta: i32,
}
pub enum PublicAction {
    Built { card: String },
    BuiltFree { card: String },        // Olympía A
    BuiltFromDiscard { card: String }, // Halikarnassós
    WonderStage { stage: u8 },         // card hidden
    Discarded,                         // card hidden
}
pub struct Battle {
    pub player: String,
    pub rival: String,                 // player's east neighbour
    pub player_strength: u32,
    pub rival_strength: u32,
    pub player_token: Option<i32>,
    pub rival_token: Option<i32>,
}
```

- `Game.history: Vec<HistoryEvent>`; `apply_batch` appends one `Turn` (plays in seat order; `kind` from the phase being resolved; `turn`/`age` of the resolved turn; `coins_delta` = coins after the whole batch including trade credits minus before). `resolve_battles` appends one `Battles` (one entry per seat vs its east neighbour, consistent with the tokens it awards). End-of-turn-6 automatic discards are not player actions and are not recorded.
- `pub fn history(&self) -> &[HistoryEvent]`; NIF `history(game)` → list of `{:turn, %{age, turn, kind, plays}}` | `{:battles, %{age, battles}}` (DTOs in `dto.rs`, pinned in `nif_dto.rs` and `core_shapes_test.exs`). `debug_json` includes it.
- No `ENGINE_VERSION` bump.

Core tests: unit tests for a turn with trades (coins to each side and deltas on both buyer and seller), a discard (+3, no card), a wonder stage (stage number, no card), Olympía free build, Halikarnassós extra turn, Babylon B last card, battles with win/loss/tie. Simulation invariants for random games: `3 + Σ coins_delta == final coins` per player; tokens from `Battles` == `military_tokens`; cards from `Built*` plays == `built` (in order); two replays of the same seed + actions give identical history.

### Helios — live game

- `Core.history/1`, `GameServer` `handle_call(:history)`, `Games.history(game_id)`.
- `GameLive` keeps `history` (fetched on mount and on every refresh) and `last_choice` (the card of the player's latest accepted choice: the choice just submitted in the `submit` handler, otherwise `view.my_pending` before the refresh).
- On refresh, events beyond the previous history length are new. Mount and reconnect set the baseline without animating.
  - New `Turn` events → `push_event("turn_resolved", %{plays: …, my_card: last_choice, pass: :west | :east})` (player ids, public actions, coin deltas).
  - New `Battles` event → assign `age_summary` (renders `#age-summary`).
  - Several at once (turn 6 + battles + next age) → turn animation first, then the summary.
- **`.TurnFx` colocated hook** on `#table`, drawing into `#fx-layer` (`phx-update="ignore"`, `pointer-events-none`, fixed full-screen):
  - The hand hook records hand-card rects in `beforeUpdate` (keyed by `data-card`).
  - My card: a ghost image flies from its old hand rect to `#built-<me>-<slug>` (build), `#stage-<me>-<n>` (wonder stage, flipping to the age back), or fades into my coin counter (discard).
  - Other players' new tiles: fade/scale in with a gold glow (`fx-arrive` class).
  - Coin floats: "+3" / "−2" rising above each board's `[data-stat="coins"]` for every non-zero `coins_delta`.
  - New hand slides in from the side it was passed from.
  - Total ≤ 1.2 s, never blocks input; if `matchMedia("(prefers-reduced-motion: reduce)")` matches, the hook does nothing.
- **End-of-age summary** `#age-summary` (dialog over the table): "End of Age N — Military conflicts", one row per battle (the viewer's two battles first): names, military strengths, tokens (`tokens/victory{1,3,5}.png`, `victoryminus1.png`) or "tie". Rows reveal with CSS stagger (`motion-safe:`). Closes on click, Esc, `#close-age-summary`, or after ~6 s (hook timer → `dismiss_age_summary`); on close, token ghosts fly to each board's `[data-tokens]`. After Age III it appears over the game-over page.
- **History drawer**: `#history-button` in the top bar toggles `#history-drawer` (right-side drawer from `lg`, bottom sheet below; client-side `JS.toggle`, content server-rendered and live). Newest first, grouped by age; each `Battles` event as a block; each turn lists every play: "Bob built [tile] Stockade, paid 2 coins to Cid", "Cid built wonder stage 1", "You discarded a card (+3 coins)", "Ann built Temple for free (Olympía)", "Dana built Gardens from the discard pile". Coins paid to neighbours shown by name; each play also shows its net `coins_delta` ("+5 coins") when non-zero. Card names open `#card-sheet`. Available during the game and at game over.

### Helios — finished games and replay

- `Games.replay(game, user_id)` extended: replays the log in a throwaway engine; after every submit, if `Core.history/1` grew by a `Turn` event, records a frame. Frame k = state at the start of turn k:
  - `players` (public boards, from the viewer's view),
  - `hand` (the viewer's hand at that point, from their own view; empty when not acting),
  - `my_choice` (the viewer's last accepted action row before turn k resolved, decoded with `ActionCodec`; nil if they didn't act),
  - `event_index` (the `Turn` event that ends the frame).
  Plus a final frame (game over). Returns `{:ok, %{view: final_view, history: history, frames: frames}}`. ~20–25 frames; held in LiveView assigns only on this page.
- Once `phase.kind == :game_over`, `GameLive` always uses replay mode (just finished or loaded later), starting on the final frame.
- `#replay-bar`: `#replay-first` ⏮, `#replay-prev` ◀, label "Age I · Turn 3" (`#replay-position`), `#replay-next` ▶, `#replay-last` ⏭, `#replay-play` ▶/⏸ (autoplay every 2.5 s via hook timer pushing `replay_next`; stops at the end).
  - Stepping forward one frame pushes the same `turn_resolved` animation and shows `#age-summary` when crossing a `Battles` event; stepping back or jumping renders without animation.
  - The viewer's hand for the frame is shown read-only in the dock with `my_choice` highlighted and a line "You chose: Build Temple" (`GameFormat.describe_action/1`).
  - History drawer entries become links (`phx-click="replay_goto"`, `phx-value-frame`) that jump to their frame.
  - `#scoreboard` shows on the final frame; on other frames a `#show-final-scores` button opens it.
- Replay failure → Phase C fallback (scoreboard only, `#boards-unavailable`, no replay bar).

Phase D tests (Helios): `Games.history/1`; `assert_push_event(view, "turn_resolved", …)` after all players submit, including `my_card` when the viewer's own submit resolved the turn; no push on mount; `#age-summary` after the last turn of an age, its rows, and dismissal; drawer content for builds, trades, discards, wonder stages; `Games.replay/2` frame count equals `Turn` events + 1, frame hands contain only the viewer's cards, `my_choice` matches the last row; replay bar stepping/jumping and `replay_goto`. E2e: Playwright `use: { reducedMotion: "reduce" }`; a spec that plays a short game to the end and steps the replay bar forward and back.

## Id changes

| Old | New | Phase |
|---|---|---|
| `[data-stat="shields"]` | `[data-stat="military"]` | A |
| `#built-<slug>` (own board only) | `#built-<player>-<slug>` (all boards) | C |
| `#stage-<n>` (own board only) | `#stage-<player>-<n>` (all boards) | B |
| trade buttons directly under `#build-options` | `#build-action` + `#build-payments` › `#build-option-N` (same N) | B |
| `others_strip` count chips | full boards (`article#player-<id>` kept) | C |

## Out of scope

- Sharper card art (the zoom is capped by the 180×275 assets).
- Pagination beyond the 50 most recent past games.
- Spectators or viewing games you weren't seated in.
- Showing other players' hands or hidden cards, even after the game.
- Sound effects.

## Commands

Per phase, for every part touched (from `AGENTS.md`):
- core: `cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test`
- helios: `cd helios && mix precommit`
- e2e: `cd e2e && npm ci && npx playwright install chromium && npx playwright test`
