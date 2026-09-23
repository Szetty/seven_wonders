# Phase 4 — GameServer + GameLive

Status: approved design (2026-09-23). Overview: `2026-09-23-migration-overview-design.md`. Depends on Phases 2 (lobby) and 3 (engine API).

## Goal

Play complete 7 Wonders games in the browser: start from a lobby, one `GameServer` per game wrapping the NIF, every accepted action persisted and replayable, an HTML/Tailwind game table, final scoreboard. Move the card/wonder/token artwork into Helios and delete `frontend/`.

## Design

### Data
Migration `create_games`:
- `games`: `id` (`:binary_id`), `lobby_id` (FK lobbies, delete_all), `seed` (integer, 0..2^63−1), `engine_version` (integer), `status` (string `"active" | "finished" | "aborted"`), `final_scores` (`:map`, nullable), `finished_at` (utc_datetime, nullable), timestamps.
  - `unique_index(:games, [:lobby_id], where: "status = 'active'", name: :games_one_active_per_lobby)` (portable partial index).
- `game_players`: `id`, `game_id` (FK, delete_all), `user_id` (FK users), `seat` (integer). Unique `[:game_id, :seat]` and `[:game_id, :user_id]`.
- `game_actions`: `id`, `game_id` (FK, delete_all), `seq` (integer, from 1), `user_id` (FK users), `action` (`:map`), `inserted_at`. Unique `[:game_id, :seq]`.

Engine player identifiers are **stringified user ids** (seat order = `game_players.seat`); GameLive maps ids to names.

### `Helios.Games` context
- `start_game(scope, lobby) :: {:ok, Game.t()} | {:error, reason}`:
  - Caller must be the lobby owner (`:not_leader`).
  - Players = owner + invitees currently connected to `"lobby:<id>"` (Presence), in members order; 3..7 required (`:not_enough_players` / `:too_many_players`).
  - No active game for the lobby (`:game_in_progress`, enforced by the partial unique index too).
  - Seed from `:rand.uniform(2 ** 63) - 1` unless `Application.get_env(:helios, Helios.Games)[:fixed_seed]`; wonders `:random` unless `[:wonders]` override is configured (used only by the e2e env for determinism).
  - Insert `games` + `game_players` in a transaction, then `ensure_started/1`; if the engine rejects setup, delete the game and return the error.
  - Broadcast `{:game_started, game_id}` on `"lobby:<lobby_id>"`.
- `ensure_started(game_id) :: {:ok, pid} | {:error, :not_found | :not_active | :aborted}` — starts `Helios.Games.GameServer` under `Helios.Games.Supervisor` (DynamicSupervisor) with `name: {:via, Registry, {Helios.Games.Registry, game_id}}`; `{:error, {:already_started, pid}}` → `{:ok, pid}`.
- `submit(game_id, user_id, action)`, `view(game_id, user_id)` — `ensure_started` then `GenServer.call`.
- `active_game_for_lobby(lobby_id)`, `seated?(game, user_id)`, `players(game)`.
- `ActionCodec`: `encode(action) :: map` (JSON-safe: `%{"type" => "build", "card" => "Altar", "payment" => %{"west" => [["wood", 1]], "east" => []}}`) and `decode(map) :: action_term` using an explicit whitelist of type/resource strings → atoms (no `String.to_atom`).
- `error_message/1` for every engine error atom and context error (e.g. `:cannot_afford` → "You can't afford that", `:invalid_payment` → "That payment is no longer valid", `:game_in_progress` → "A game is already running at this table").

### GameServer (GenServer, `restart: :transient`)
- State: `%{game_id, ref, seq, players, idle_timeout}`.
- `init/1`: load game + players + actions ordered by `seq`; `Helios.Core.new_game(player_ids, wonders, seed)`; replay each decoded action with `Helios.Core.submit`. If any replayed action errors (e.g. engine rule change) → mark the game `"aborted"`, broadcast `{:game_aborted}` on `"game:<id>"` and `{:members_changed}` on `"lobby:<lobby_id>"`, return `:ignore`. If `engine_version` differs from `Helios.Core.game_settings().engine_version`, attempt the replay anyway and log a warning.
- `handle_call({:submit, user_id, action})`:
  1. `Helios.Core.submit(ref, to_string(user_id), action)`; on `{:error, reason}` reply with it (nothing persisted).
  2. On `:ok`, insert `game_actions` row with `seq + 1`. If the insert raises, **let the process crash** — the supervisor restarts it and replay rebuilds state from the DB without the unpersisted action (the player resubmits). Memory can never be ahead of the log.
  3. If the engine phase is now `GameOver`: update game `status: "finished"`, `final_scores`, `finished_at`; broadcast `{:game_finished}` on `"game:<id>"` and `{:members_changed}` on the lobby topic.
  4. Broadcast `{:game_updated, seq}` on `"game:<id>"` (no state in the payload — hidden information stays on the server).
- `handle_call({:view, user_id})` → `Helios.Core.view`.
- Every reply returns the idle timeout (default 30 min, configurable); `handle_info(:timeout)` → `{:stop, :normal, state}`. A later call restarts it lazily via `ensure_started`.
- Supervision tree additions: `{Registry, keys: :unique, name: Helios.Games.Registry}`, `{DynamicSupervisor, name: Helios.Games.Supervisor}`.

### Lobby changes
- Owner sees a **Start game** button, enabled when 3–7 members (incl. owner) are connected and no active game exists; shows why it's disabled otherwise ("Need at least 3 connected players").
- On `{:game_started, id}`: seated, connected members `push_navigate` to `/game/<id>`.
- While a game is active: banner "Game in progress" + **Rejoin** link for seated players; Start disabled.

### GameLive (`/game/:game_id`, in the `:authenticated` live_session)
- Mount: `ensure_started`; only seated players (`:unauthorized` → redirect to own lobby with flash); aborted/finished-not-found → redirect with message. Subscribe `"game:<id>"`; track Presence on `"game:<id>"`; fetch `view`.
- Layout (Tailwind, `paper.jpg` background, responsive; card art 180×275, wonder art 800×250):
  - **Top bar**: Age (I/II/III), turn x/6, pass-direction arrow, "Waiting for: …" (names from `submitted`), connection dots from game Presence.
  - **Neighbours**: West (left) and East (right) compact panels — wonder art thumbnail, stages built/total, built cards as small colour chips (hover shows name), coins, shields, military tokens.
  - **Other players**: a horizontal strip of compact summaries (name, wonder, coins, shields, stage count, card counts per colour).
  - **My area**: wonder board art with stage slots (filled with age card-backs when built), built cards grouped into colour columns, coin and military token counters.
  - **Hand**: a row of card images. Selecting a card opens an action panel:
    - **Build** — "Free", "Pay N coins", or a list of payment options ("West 2 · East 1 · Bank 0"); disabled with reason if unavailable.
    - **Build wonder stage** — same option rendering.
    - **Build free (Olympía)** — shown when available.
    - **Discard (+3 coins)**.
  - The submitted choice is highlighted with a "Change" affordance until the turn resolves.
  - **Extra turns**: Halikarnassós gets a discard-pile picker; Babylon B gets a "Play your last card" panel; other players see "Waiting for <name>".
  - **Game over**: scoreboard table (military, treasury, wonder, civilian, scientific, commercial, guild, total, rank), winner highlighted, "Back to lobby" button.
- Events: `"select_card"` (UI state only); `"submit"` with `%{"card", "kind", "option"}` where `option` is an index into the payment options of the **server's current view** for that card — the server resolves it to a `Payment`; clients never construct payments. Errors → flash via `Games.error_message/1`.
- handle_info: `{:game_updated, _}` → refetch view; `{:game_finished}` → refetch; `{:game_aborted}` → redirect to lobby with flash; `presence_diff` → update connection dots.

### Assets
- `git mv frontend/public/cards helios/priv/static/images/cards`, same for `wonders`, `tokens`; `git mv frontend/public/paper.jpg helios/priv/static/images/paper.jpg` (`7_wonders.jpg` already present).
- `HeliosWeb.GameAssets`:
  - `card_path(name)` → `/images/cards/<name lowercased, spaces removed>.png`; `card_back_path(age)` → `/images/cards/age<n>.png`.
  - `wonder_path(wonder, side)` → strip diacritics (`:unicode.characters_to_nfd_binary` + remove combining marks), lowercase, special case `halikarnassos → halikarnassus`, append `A`/`B`: `/images/wonders/<city><Side>.png`.
  - `token_path(:coin | :shield | {:military, n} | resource)` with `loom → linen`, `papyrus → paper`.
- Test: for every card and wonder side in `Helios.Core.game_settings()`, the mapped file exists under `priv/static`.
- Then `git rm -r frontend`.

## Testing

### ExUnit
- `Games.start_game`: owner-only, player count bounds from Presence, one active game per lobby, broadcast.
- `ActionCodec`: round-trip for every action variant; rejects unknown strings.
- `GameServer`: submit persists with increasing `seq`; engine errors persist nothing; **kill-and-replay** (`Process.exit(pid, :kill)`, `ensure_started` again) yields identical views for all players; idle timeout stops the process; aborted replay marks the game aborted; game over marks finished and stores scores (drive a full 3-player game with discard-only/first-available actions).
- `GameLive`: seated-only access; rendering of hand options, submit flow across three LiveView clients advancing the turn; extra-turn panels (use explicit wonders Halikarnassós / Babylon B with a fixed seed); scoreboard at game end.
- `GameAssets` file-existence test.

### Playwright (`e2e/tests/game.spec.ts`)
The e2e env configures `Helios.Games` with `fixed_seed` and explicit wonders without extra-turn abilities (e.g. Gizah A, Rhódos A, Éphesos A) for determinism.
1. **Full game** — A, B, C log in; A invites B and C; both accept; A starts; all land on `/game/<id>`; loop: each player picks the first card with an enabled Build option (else Discard) and submits; repeat until the scoreboard shows for all three with 3 ranked rows.
2. **Rejoin** — mid-game, B reloads the page; B's hand and board are unchanged and play continues.
3. **Change choice** — A submits Discard, then changes to another card before B and C submit; the resolved turn reflects the change.
4. **Start disabled** — with only 2 connected players, Start is disabled with the reason.

## Acceptance criteria
- `mix precommit`, `cargo test`, full Playwright suite green locally and in CI.
- `frontend/` deleted; no references to it remain.
