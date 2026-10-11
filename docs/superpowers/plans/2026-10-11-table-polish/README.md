# Table polish + wonder modes — Execution Guide

Plans implementing:
- `docs/superpowers/specs/2026-10-11-table-polish/2026-10-11-table-polish-design.md` (phases A–C)
- `docs/superpowers/specs/2026-10-11-table-polish/2026-10-11-wonder-modes-design.md` (phases D–E)

Read the spec a plan cites before starting it; every plan argues from it.

## Order

| Step | Plan | Touches | Notes |
|---|---|---|---|
| 1 | `2026-10-11-phase-a-status-bar.md` | helios, e2e | Phone status bar rows, waiting popover, replay waiting fix, military icon. |
| 2 | `2026-10-11-phase-b-replay.md` | helios | Discard-pile marker, discard choices in replay frames, discard fly animation, `?frame=N`. |
| 3 | `2026-10-11-phase-c-tables.md` | helios, e2e | `last_visited_at` migration, Recent tables menu, invite auto-accept on arrival. |
| 4 | `2026-10-11-phase-d-wonder-modes-core.md` | core | `WonderSelection::{ChooseBoard, ChooseSide}`, `Phase::Setup`, claim/confirm actions, NIF DTOs. |
| 5 | `2026-10-11-phase-e-wonder-modes-helios.md` | helios, e2e | Lobby `wonder_mode`, start game, codec, setup screen, replay start, e2e. |

Phases are sequential: later plans use components and functions introduced by earlier ones (each task's **Interfaces** block names them). B builds on A's top bar rows; E builds on A's top bar and D's NIF interface. Phase D can be executed independently of A–C if convenient, but E needs both.

Each phase ends with every check green for the parts it touched:

- core: `cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test`
- helios: `cd helios && mix precommit`
- e2e: `cd e2e && npm ci && npx playwright install chromium && npx playwright test`

## Cross-phase interfaces (fixed here so plans agree)

- **A → B:** `top_bar/1` gains attr `replay` (`:boolean`, default `false`; when true no waiting UI is rendered) and the phone row 2 container `#pass-row` (holds `#pass-direction`; B appends `#discard-pile` to it, and to the desktop row).
- **B:** `#discard-pile` is rendered by `HeliosWeb.GameComponents.TopBar.discard_pile/1` (attr `count`), shown when `count > 0`. Replay frames gain `discard_pile` and `discard_count`.
- **D → E (NIF terms):**
  - `Core.new_game(players, wonders, seed)` accepts `wonders` = `:random | :choose_board | :choose_side | {:explicit, [{name, :a | :b}]}`.
  - Actions: `{:claim_wonder, %{wonder: String.t(), side: :a | :b}}` and `:confirm_wonder`.
  - New errors: `:unknown_wonder`, `:wonder_taken`, `:not_your_wonder`, `:no_claim`, `:already_confirmed` (setup actions outside setup and card actions during setup → existing `:action_not_allowed_now`).
  - Phase: `phase.kind == :setup` with `age: 1, turn: 1`, Age I direction, `extra_turn_player: nil`, `extra_turn_kind: nil`.
  - View during setup: `players: []`, `hand: []`, `discard_pile: nil`, `discard_count: 0`, `my_pending: nil`, `scores: nil`, `west`/`east` = neighbour seat ids, `submitted: [{player_id, confirmed?}]` in seat order, and new field `setup`:
    `%{mode: :choose_board | :choose_side, seats: [%{player: String.t(), wonder: String.t() | nil, side: :a | :b | nil, confirmed: boolean}], available: [String.t()]}` (seats in seat order; `available` empty for `:choose_side`). Outside setup `setup: nil`.
  - Setup actions produce no history events.
  - Phase D changes only `core/`; the `@type` docs in `helios/lib/helios/core.ex` are updated in phase E.
  - Stored actions keep the existing `"type"` key: `%{"type" => "claim_wonder", "wonder" => name, "side" => "a" | "b"}`, `%{"type" => "confirm_wonder"}`.

## Execution method

Subagent-driven, as for previous initiatives: per task a fresh implementer works from the plan task + the spec, then a fresh reviewer checks it against the plan, the spec, Global Constraints and Review Focus before the next task starts. Finish each phase with a whole-branch review. Use `superpowers:subagent-driven-development` when available.

## Human checkpoints (the executor must stop and ask)

- Any push, PR, or CI-dependent acceptance step.
- If a plan step cannot be followed as written (a file or function differs from what the plan quotes), stop and ask rather than redesigning.
- Never stage with `git add -A` / `git add .`; stage the paths each task lists.

## Predicted vs. observed

The plans were written without running the code. Exact test output, seeds, pixel heights and rendered class strings are predictions; where a step says "take the value from a real run", do so.
