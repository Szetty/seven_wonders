# Game table UX — Execution Guide

Plans implementing `docs/superpowers/specs/2026-10-04-game-table-ux/2026-10-04-game-table-ux-design.md` (read it first; every plan argues from it).

## Order

| Step | Plan | Touches | Notes |
|---|---|---|---|
| 1 | `2026-10-04-phase-a-quick-fixes.md` | helios, e2e | No engine change. |
| 2 | `2026-10-04-phase-b-explanations.md` | core, helios, e2e | `Effect` refactor + catalogue + `Chain` option. |
| 3 | `2026-10-04-phase-c-boards.md` | helios, e2e | Everyone's boards, finished-game boards, Past games. |
| 4 | `2026-10-04-phase-d-history.md` | core, helios, e2e | Engine history, animations, end-of-age summary, replay. |

Phases are sequential: later plans use components and functions introduced by earlier ones (each task's **Interfaces** block names them). Each phase ends with every check green:

- core: `cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test`
- helios: `cd helios && mix precommit`
- e2e: `cd e2e && npm ci && npx playwright install chromium && npx playwright test`

## Execution method

Subagent-driven, the same arrangement as the migration and responsive-UI plans: for each task a fresh implementer works from the plan task + the spec, then a fresh reviewer checks the task against the plan, the spec, Global Constraints and Review Focus before the next task starts. Finish each phase with a whole-branch review. If the executing harness has the superpowers skills, use `superpowers:subagent-driven-development`.

## Human checkpoints (the executor must stop and ask)

- Any push, PR, or CI-dependent acceptance step.
- If a plan step cannot be followed as written (a file or function differs from what the plan quotes), stop and ask rather than redesigning.
- Never stage with `git add -A` / `git add .`; stage the paths each task lists. The working tree has unrelated uncommitted changes in `helios/config/config.exs` and `helios/config/dev.exs` — never stage or revert them.

## Predicted vs. observed

The plans were written without running the code. Exact test output, the number of payment options the engine offers for a given card, and seeds found by `GameDriver.find_seed/4` are predictions; where a step says "take the value from a real run", do so.
