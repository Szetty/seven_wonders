# AI players — Execution Guide

Plans implementing `docs/superpowers/specs/2026-10-13-ai-players/2026-10-13-ai-players-design.md`. Read the spec before starting any plan; every plan argues from it.

## Order

| Step | Plan | Touches | Notes |
|---|---|---|---|
| 1 | `2026-10-13-phase-a-ai-foundation.md` | helios | `Req` dependency, model allowlist (`Helios.AI.Models`), provider behaviour, fake provider, `Helios.AI.Readiness`, AI supervisors. |
| 2 | `2026-10-13-phase-b-prompt-and-providers.md` | helios | Decision payload (`Helios.AI.Prompt`), deadline-bounded HTTP, Jev and OpenRouter adapters, API keys, real allowlist, `mix helios.ai.probe`. |
| 3 | `2026-10-13-phase-c-seats-and-players.md` | helios | Migration (nullable `user_id`, `ai_model`, `seat`, `source`), `Helios.Games.Actor`, lobby AI seats, start game with AI seats, sources and random-move keys, owner setup for AI seats, `Helios.AI.Player`. |
| 4 | `2026-10-13-phase-d-ui-and-e2e.md` | helios, e2e | Lobby AI seat controls, table bot icons and "random" tags, setup screen acting for AI seats, past games, e2e spec. |

Phases are sequential: each uses modules introduced by the earlier ones (each task's **Interfaces** block names them). No phase touches `core/`.

Each phase ends with every check green for the parts it touched:

- helios: `cd helios && mix precommit`
- e2e: `cd e2e && npm ci && npx playwright install chromium && npx playwright test`

## Cross-phase interfaces (fixed here so plans agree)

- **Model entry** (`Helios.AI.Models.model()`): `%{id: String.t(), label: String.t(), provider: :jev | :openrouter | :fake, model: String.t(), deadline_ms: pos_integer()}`, plus optional `reasoning: boolean()` and `max_tokens: pos_integer()` (OpenRouter only).
- **Provider behaviour** (`Helios.AI.Provider`):
  - `decide(model, prompt, deadline_ms_monotonic) :: {:ok, move_id, %{input_tokens: n, output_tokens: n}} | {:error, term()}`
  - `probe([model]) :: {:ok, [model_id]} | {:error, term()}`
- **Prompt** (`Helios.AI.Prompt.t()`): `%{state: map(), moves: [move()], primer: String.t()}`, where `move()` is `%{id: String.t(), label: String.t(), card: String.t(), kind: String.t(), trade_options: non_neg_integer()}`. `Prompt.to_choice(move) :: Helios.Games.Choice.t()` picks a random payment index for trades.
- **Readiness** (`Helios.AI.Readiness`): `ready_models/0`, `ready?/1`, `refresh/0`, `probe_now/0` (returns `%{ready: MapSet.t(), reasons: %{provider => reason}}`), PubSub topic `"ai:readiness"` with message `{:ai_readiness_changed}`.
- **Supervisors** (started in `Helios.Application`): `Helios.AI.TaskSupervisor` (Task.Supervisor), `Helios.AI.Supervisor` (DynamicSupervisor for `Helios.AI.Player`), then `Helios.AI.Readiness`.
- **Actors and engine ids** (`Helios.Games.Actor`): an actor is a user id (integer) or `{:ai, seat}`. Engine player ids are `"<user_id>"` and `"ai-<seat>"` (a hyphen, not a colon, so ids stay valid in DOM ids and CSS selectors).
- **Games API** (phase C):
  - `Games.submit(game_id, actor, action, source \\ :human)` and `Games.submit_choice(game_id, actor, choice, source \\ :human)`, where `source` is `:human | :ai | :random`.
  - `Games.view(game_id, actor)`.
  - `Games.snapshot/2` returns `%{view, history, random}` and `Games.replay/2` returns `%{view, history, frames, random}`. `random` is a `MapSet` of `{player_id, age, turn, turn_kind}` (`Helios.Games.RandomMoves`).
  - `Games.submit_for_ai_seat(scope, game_id, seat, action)`.
  - `Games.seat_plan/1`, `Games.display_names/1`, `Games.ai_player_ids/1`, `Games.player_id/1`.
- **GameServer direct calls** (used by `Helios.AI.Player`, phase C): `GameServer.view(pid, actor)` and `GameServer.submit_choice(pid, actor, choice, source)`.
- **Fake provider** (test and e2e config only): the `model` string selects behaviour: `"first"`, `"last"`, `"timeout"`, `"error"`, `"invalid"`, `"down"`. If `config :helios, :fake_ai_listener` holds a pid, it receives `{:fake_ai_decide, model_id, prompt}` and `{:fake_ai_probe, [model_id]}`.

## Refinements made while planning (already folded into the spec)

- AI engine ids use `"ai-<seat>"` instead of `"ai:<seat>"`, because ids are interpolated into DOM ids such as `connection-<id>` and `board-<id>`.
- The readiness GenServer is `Helios.AI.Readiness`, and the adapters are `Helios.AI.Provider.{Jev, OpenRouter, Fake}`.
- A seated model that is no longer ready shows an "unavailable — moves will be random" badge on its lobby row, instead of a flash at start.
- Owner setup actions for AI seats go through a single `Games.submit_for_ai_seat/4`.
- Random moves are tracked as history-shaped keys, recorded by `GameServer` and rebuilt by `Replay`, instead of joining log rows to history events.
- The "thinking…" state for AI seats reuses the existing per-player `:thinking` status, which already appears for anyone who hasn't chosen yet.

## Execution method

Subagent-driven, as for previous initiatives: for each task, a fresh implementer works from the plan task plus the spec. A fresh reviewer then checks the result against the plan, the spec, the Global Constraints and the Review Focus before the next task starts. Finish each phase with a whole-branch review. Use `superpowers:subagent-driven-development` when it is available.

## Human checkpoints (the executor must stop and ask)

- Phase B, Task 5: the initial OpenRouter model slugs, labels and `deadline_ms` values. The user picks them.
- Any push, PR, or CI-dependent acceptance step.
- If a plan step can't be followed as written (a file or function differs from what the plan quotes), stop and ask rather than redesigning.
- If the history `{:turn, event}` fields for an extra turn don't match the phase the extra turn was submitted in (phase C, Task 4 test "random keys match history"), stop and ask.
- Never stage with `git add -A` or `git add .`. Stage the paths each task lists.

## Predicted vs. observed

The plans were written without running the code. Exact test output, timings, token counts and the shape of TypeSafe's `/v1/models` response are predictions. Where a step says "take the value from a real run", do so.
