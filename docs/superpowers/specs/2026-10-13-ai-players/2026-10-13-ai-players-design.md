# AI players — Jev and OpenRouter

Status: approved design (2026-10-13). Builds on the Phase 4 game design (`docs/superpowers/specs/2026-09-23-migration/2026-09-23-phase-4-game-design.md`) and the wonder selection modes (`docs/superpowers/specs/2026-10-11-table-polish/2026-10-11-wonder-modes-design.md`, shipped in #95).

## Goal

The table owner can seat AI players. Each AI seat plays a model chosen from a predefined allowlist, served by one of two providers:

1. **Jev** — TypeSafe's decision model, through its HTTP API directly (`POST https://api.typesafe.ai/v1/systemone`).
2. **OpenRouter** — chat completions with structured output.

Success criteria:
- The owner adds and removes AI seats in the lobby, picking each seat's model from a dropdown of **ready** models only (provider configured and verified).
- AI seats play every decision of a card game (normal picks, Olympia free build, Babylon play-last-card, Halicarnassus build-from-discard) with no human action.
- Every AI decision finishes within its model's fixed deadline (plus a small slack). On timeout, provider error or invalid answer the AI plays a uniformly random legal move, and the UI marks that move **"random"** for everyone, including in replays.
- Calls are token-cheap: one call per decision at most, stateless (no conversation history), roughly 1–1.5k input tokens and a handful of output tokens, no trade payment lists.
- Existing games and replays are unaffected; no engine change, no `ENGINE_VERSION` bump.
- All core, helios and e2e checks pass, plus the new tests below.

## Decisions

- **No engine change.** The player view already lists each hand card's `build` / `wonder_stage` option (`:free | {:chain, c} | {:coins, n} | {:trade, opts} | {:unavailable, r}`) and the existing `Choice` + `Games.submit_choice/3` path resolves a `%{card, kind, option}` server-side. The AI picks a card + kind; the server picks the payment. All legality stays in `core`.
- **Trade payments are never sent to the AI.** A tradable option is labelled with its cheapest cost (option 0 of the engine's list, which is sorted by total coins). If the AI picks it, the server submits a **uniformly random** option from the engine's list, which may cost more than the label.
- **One process per AI seat, acting like a client.** It fetches its own view after each `{:game_updated, _}` broadcast, as a browser does. PubSub stays state-free; hidden information stays on the server.
- **Per-model fixed deadline.** Each allowlisted model has a `deadline_ms`. Retries on retriable errors happen *inside* that budget; the hard stop is the deadline plus ~250 ms slack. No retry after the deadline.
- **Random fallback is not a determinism concern.** The submitted action is written to `game_actions` like any other; replay never calls an AI. The random policy uses `:rand`.
- **API keys are server-side** (env vars), never per-user.
- **AI players only play moves.** No chat, no explanations.
- **Setup modes: the owner chooses for AI seats.** In `choose_board` / `choose_side` games the owner claims and confirms each AI seat's board/side from the setup screen. `random` mode is unchanged.
- **The model allowlist lives in app config**, not in the database. Changing it is a config edit and a deploy.

## Configuration

`config/config.exs`:

```elixir
config :helios, Helios.AI,
  models: [
    %{id: "jev-1.13", label: "Jev 1.13", provider: :jev, model: "jev-1.13.0", deadline_ms: 5_000},
    %{id: "or-<slug>", label: "<label>", provider: :openrouter, model: "<vendor>/<model>",
      deadline_ms: 10_000, reasoning: false}
    # …
  ],
  probe_interval_ms: 600_000
```

- `id` is a stable key stored on seats (`lobby_ai_seats.model_id`, `game_players.ai_model`). Removing a model from config does not break existing games: the seat's moves fall back to random, and the label shown is the stored id.
- `reasoning` (OpenRouter only, default `false`) opts a model into reasoning tokens; off by default for cost and latency.
- `config/runtime.exs` reads `JEV_API_KEY` and `OPENROUTER_API_KEY` (both optional in every env). A provider without a key is never ready.
- `config/test.exs` and `config/e2e.exs` add a `:fake` provider (see Testing) and point the real providers at `Req.Test` stubs.

The initial OpenRouter entries (model slugs and their deadlines) are chosen by the operator when the plan's config task is executed; the spec fixes only the shape.

## Data model

Migrations (portable SQL, no SQLite-only features):

- **`lobby_ai_seats`**: `id`, `lobby_id` (FK, on delete cascade), `model_id` (string), `inserted_at`. The seat order is insertion order.
- **`game_players`**: `user_id` becomes nullable; add `ai_model` (string, nullable). Exactly one of `user_id` / `ai_model` is set (changeset validation).
- **`game_actions`**: `user_id` becomes nullable; add `seat` (integer, nullable) and `source` (string, not null, default `"human"`, one of `human | ai | random`). Human actions keep `user_id`; AI actions set `seat` and leave `user_id` nil. Existing rows backfill to `source = "human"`.

**Engine player ids**: humans keep `to_string(user_id)`; an AI seat is `"ai:<seat>"`. One helper, `Games.player_id/1` (given a `GamePlayer`), builds them, and replay uses it to map log rows to engine ids.

**Display name** of an AI seat: its model label, plus ` #2`, ` #3`… when several seats share a model.

## Lobby (`Helios.Lobbies`)

- `add_ai_seat(scope, lobby, model_id)` — owner only (`:not_leader`). The model must be ready (`:model_unavailable`), and humans plus AI seats must stay ≤ `@max_players` (`:table_full`). Broadcasts `{:members_changed}`.
- `remove_ai_seat(scope, lobby, seat_id)` — owner only; broadcasts `{:members_changed}`.
- `ai_seats(lobby)` lists them in order.
- `Games.eligible_players/1` returns the connected humans (unchanged rule) followed by the AI seats. AI seats skip the Presence check and count towards the minimum of 3 and the maximum of 7.
- `Games.start_game/2` inserts `game_players` rows for AI seats (`ai_model` set) after the humans. It re-checks readiness; if a seated model is no longer ready the game still starts, and the lobby shows a flash: "<label> is unavailable — its moves will be random".
- Lobby AI seats persist across games at the same table until removed.

## Provider readiness (`Helios.AI.Providers`)

A GenServer that caches each provider's status (`:ready` or `{:not_ready, reason}`) and the set of ready model ids.

- Checks all providers at boot and every `probe_interval_ms`. `refresh/0` re-checks on demand (the lobby picker calls it when opened, at most once a minute).
- A provider is ready only if:
  1. its API key is set, and
  2. an authenticated probe succeeds:
     - **OpenRouter**: `GET /api/v1/key` (validates the key, reports remaining credit; free), then `GET /api/v1/models`. Each allowlisted OpenRouter model is ready only if it is listed and its `supported_parameters` include `response_format` / `structured_outputs`.
     - **Jev**: a free models-listing endpoint if the API has one (confirm during planning; the SDK exposes a `Models` type). Otherwise a minimal `systemone` call (one-word state, a 2-option choice), which costs a few input tokens.
- A model is ready when its provider is ready (and, for OpenRouter, the model check passed).
- `ready_models/0` feeds the lobby dropdown; `ready?/1` is checked by `add_ai_seat` and `start_game`.
- Failures are logged with the reason (`:no_key`, `{:http, 401}`, `:unknown_model`, `:no_structured_output`, `:out_of_credit`, transport error).

## The decision payload (`Helios.AI.Prompt`)

`Prompt.build(view, catalogue)` returns `%{state: map, moves: [%{id, label}], primer: string}`. It uses only the AI seat's own view plus the static catalogue.

### State (compact JSON; keys kept short)

```
phase:   age, turn, pass direction, extra-turn kind if any
me:      wonder + side, coins, stages built/total, next stage (cost → effect text),
         built card names, produces (aggregated), science symbols (aggregated),
         shields, military tokens
west/east: same fields as `me` (public info)
others:  one compact entry each: wonder, stages built/total, coins, shields,
         built-card counts by colour
hand:    each card: name, colour, effect text, chains_to
```

- Effect text comes from `HeliosWeb.CardText` (or a shared, terser variant moved to `Helios`) applied to catalogue effect specs.
- Aggregated production, science and shield totals are **presentation only**, summed from catalogue effect specs of built cards and wonder stages. Legality and costs always come from the view's build options.
- Not sent: trade payment lists, history, other players' scores, discard-pile contents (except as move options on a Halicarnassus turn).

### Moves (only legal ones; `n` is the hand or discard index)

| id | meaning | label |
|---|---|---|
| `b<n>` | build hand card `n` | `free` / `chain from <card>` / `<k> coins` / `trade, from <k> coins` |
| `w<n>` | build the next wonder stage with hand card `n` | same cost labels |
| `d<n>` | discard hand card `n` | `+3 coins` |
| `f<n>` | Olympia: build hand card `n` free | `free (once per age)` |
| `x<n>` | Halicarnassus: build discard-pile card `n` free | `<card name>` |

A Halicarnassus turn offers only `x` moves; Babylon's play-last-card turn offers `b`/`w`/`d` for the single card.

`Prompt.to_choice(view, move_id)` maps an id back to a `Choice` (`%{card, kind, option}`). For a `{:trade, opts}` option, `option` is `:rand.uniform(length(opts)) - 1`; otherwise `0`.

### Primer

A short persona that relies on the model's own knowledge of the game instead of restating the rules, e.g.:

> You are an expert player of the 7 Wonders board game (base game). You see your current position and your legal moves. Choose the move that maximises your final score.

The primer is a fixed module attribute, identical on every call (cache-friendly), under 100 tokens.

## Provider adapters (`Helios.AI.Provider` behaviour)

```elixir
@callback decide(model :: map(), prompt :: map(), deadline :: integer()) ::
            {:ok, move_id :: String.t(), usage :: map()} | {:error, term()}
@callback probe(models :: [map()]) :: {:ok, ready_model_ids :: [String.t()]} | {:error, term()}
```

`deadline` is an absolute monotonic time in ms. Both adapters use `Req` (new dependency; Finch pool per provider) through a shared `Helios.AI.HTTP.request/2`. It retries on 429, 529, 5xx and transport errors with short jittered backoff (e.g. 100 ms, 250 ms, 500 ms), sets each attempt's `receive_timeout` to the remaining budget, and stops when less than ~300 ms remain. 4xx other than 429 are not retried.

### Jev (`Helios.AI.Providers.Jev`)

```json
POST https://api.typesafe.ai/v1/systemone
Authorization: Bearer $JEV_API_KEY
{
  "model": "jev-1.13.0",
  "state": { …state… },
  "questions": {
    "move": {
      "type": "choice",
      "instructions": "<primer>",
      "criteria": { "b0": "Lumber Yard — 1 coin", "w0": "wonder stage — trade, from 2 coins", … }
    }
  }
}
```

The answer is `answers.move.choice`. An id outside the offered set is an `{:error, :invalid_move}`. `usage` is taken from the response. Pricing at the time of writing: $0.042 per million input tokens, output free.

### OpenRouter (`Helios.AI.Providers.OpenRouter`)

`POST https://openrouter.ai/api/v1/chat/completions` with:
- `messages`: system = primer; user = state JSON + moves (`id: label` lines).
- `response_format`: `json_schema`, strict, `{"move": {"type": "string", "enum": [ids]}}`.
- `max_tokens` ≈ 16, `temperature` 0.2, reasoning excluded unless the model entry sets `reasoning: true`.
- `provider: %{require_parameters: true}` so OpenRouter routes only to backends honouring structured output.

The answer is the parsed `move`. Unparseable or out-of-set → `{:error, :invalid_move}`. `usage` is taken from the response.

## AI seat process (`Helios.AI.Player`)

- Started under `Helios.AI.Supervisor` (DynamicSupervisor), one per AI seat of an active game, registered in `Helios.Games.Registry` as `{:ai, game_id, seat}`. `Games.ensure_started/1` starts the `GameServer` and then any missing AI players, so they come back after a node restart along with the game.
- Subscribes to `Games.topic(game_id)`. On init and on every `{:game_updated, _}` it fetches its own view (`Games.view/2` with its engine id).
- **Acts when** the phase is `:choosing_cards` (and it has not submitted) or `:extra_turn` with `extra_turn_player == me`. It computes a decision key `{age, turn, phase_kind}` and acts only if the key differs from the last one it acted on. That makes it one provider call per decision at most, even with repeated broadcasts.
- **Does nothing** in `:setup` (the owner acts) and stops on `{:game_finished}`, on abort, or when the `GameServer` stops (monitor).
- **Deciding:**
  1. `Prompt.build/2`. If exactly one legal move exists, submit it directly (source `:ai`, no call).
  2. Look up the model. If it is missing from config or not ready, go to random right away.
  3. Run `provider.decide/3` in `Task.Supervisor.async_nolink` with `deadline = now + deadline_ms`; arm `Process.send_after(self(), {:deadline, key}, deadline_ms + 250)`.
  4. First outcome wins:
     - `{:ok, id, usage}` → `Games.submit_choice` with source `:ai`.
     - `{:error, _}`, task crash, or `{:deadline, key}` → kill the task, pick a uniformly random move from the same list → submit with source `:random`.
  5. A late task reply for an old key is ignored.
  6. A `submit_choice` error (e.g. the phase already moved on) is logged; the player waits for the next update.
- Emits `[:helios, :ai, :decision]` telemetry with model, provider, outcome (`ai | random`), reason, latency and token usage. A log line per decision is enough for cost tracking at first.

## Orchestration changes (`Helios.Games`, `GameServer`)

- `submit_choice` / `submit` accept an optional `source` (default `:human`) and an actor that is either a user id or `{:ai, seat}`. `do_submit` persists `seat`/`user_id` and `source` accordingly.
- **Setup for AI seats:** `Games.claim_wonder_for/4` and `Games.confirm_wonder_for/3` take the owner's scope and an AI seat. They return `:not_leader` unless the caller owns the lobby and the seat is an AI seat of that game, then submit the claim/confirm under `"ai:<seat>"` with source `:human`, since a person chose.
- `Replay.rebuild` maps each `game_actions` row to an engine id via `user_id` or `seat`.
- `Games.history/1` and replay frames join each engine turn event with the `source` of the matching `game_actions` row (by player and turn), so the UI can tag random moves. The engine's history is unchanged.

## UI

### Lobby (`LobbyLive`)
- An AI seat in the member list: bot icon, model label, and a remove button for the owner.
- Owner: an "Add AI player" button opens a dropdown of ready models (the list is refreshed when opened). The button is disabled with an explanatory tooltip when no model is ready or the table is full.
- Non-owners see AI seats read-only.

### Game table (`GameLive`)
- AI players show their display name and a bot icon wherever player names appear.
- While an AI is deciding, its submitted indicator reads "thinking…" in place of the human "waiting".
- A move whose source is `random` gets a **"random"** tag in the history/turn feed entry for that player, and a small "random" badge on that player's board for the turn just resolved. This applies in replays too.

### Setup screen
- The owner sees a claim/confirm control for each AI seat below their own.
- Other players see an AI seat as "owner is choosing…" until it is confirmed.

## Testing

- **`AI.Prompt`** against `SampleViews`:
  - payload fields as specified
  - no trade payment lists, cheapest trade cost labelled
  - only legal ids
  - correct `x`-only moves on a Halicarnassus turn and moves for Babylon's play-last-card turn
  - `to_choice` covers every kind, with a random `option` index within range for trades
- **Random policy**: picks only from the offered ids.
- **Adapters** (`Req.Test`):
  - valid answer
  - out-of-set id → `:invalid_move`
  - 429 then success within the deadline
  - 529/5xx retried
  - slow response stopped at the deadline
  - 401 not retried
  - OpenRouter request has a strict schema with an `enum` of ids and the configured `max_tokens`
- **`AI.Providers`**:
  - missing key → not ready
  - probe 401 → not ready
  - unknown model and no structured outputs → not ready
  - refresh rate limit
  - `ready_models` filters the dropdown
- **`AI.Player`** with a `:fake` provider (configurable answer, delay or error per test):
  - one call per decision key despite repeated broadcasts
  - deadline → random with source `random`
  - single legal move → no call
  - extra turns
  - idle during setup
  - stops at game over
  - restarted by `ensure_started`
- **Lobbies/Games**:
  - add/remove AI seat (owner only, ready model only, ≤ 7 seats)
  - AI seats count towards the 3-player minimum
  - start game with AI seats
  - `source`/`seat` persisted
  - replay of a game with AI and random moves is identical
  - owner claims/confirms for an AI seat in both setup modes; non-owner rejected
- **Full game**: 1 human + 2 AI seats on the fake provider play to `game_over` with `GameDriver` driving the human.
- **LiveView**: bot icon and label in lobby and table, "Add AI player" dropdown, "thinking…" state, "random" tag in history and replay, owner setup controls for AI seats.
- **e2e** (`MIX_ENV=e2e`, fake provider enabled only there): the owner adds an AI seat and plays a turn; the AI's move appears; with the fake set to time out, the move is tagged "random".
- **No real provider calls in tests or CI.** `mix helios.ai.probe` runs the readiness probes and one sample decision against the real configured providers, for manual verification of keys and models.

## Out of scope

- AI difficulty levels, personas or strategies beyond the model choice.
- AIs choosing their own wonder board/side.
- Per-user API keys, spend limits or billing UI (telemetry/log only).
- Choosing among trade payment options.
