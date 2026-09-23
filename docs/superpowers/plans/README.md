# Migration Plans — Execution Guide

Plans implementing `docs/superpowers/specs/2026-09-23-*` (start with `2026-09-23-migration-overview-design.md`).

## Execution method

Subagent-driven: for each task, a fresh implementer agent works from the plan task + its spec, then a fresh reviewer checks the task against the plan, spec and Global Constraints before the next task starts. Finish each phase with a whole-branch review. If the executing harness has the superpowers skills, use `superpowers:subagent-driven-development`.

Each plan's header lists Global Constraints (apply to every task) and Review Focus (edge cases reviewers must check).

## Order

| Step | Plan | Notes |
|---|---|---|
| 1 | `2026-09-23-phase-0-foundation.md` | Must run in the main checkout (not a worktree): `helios/` and `mise.toml` are untracked until Task 1. |
| 2a | `2026-09-23-phase-1-auth.md` → `2026-09-23-phase-2-lobby.md` | Sequential. |
| 2b | `2026-09-23-phase-3-engine.md` | Depends only on Phase 0; may run in parallel with 2a. Tasks 18 and 19 are one deployable unit — don't push between them. |
| 3 | `2026-09-23-phase-4-game.md` | Needs Phases 2 and 3. Task 1 pins the Phase 3 NIF shapes; if it fails, stop and reconcile. |
| 4 | `2026-09-23-phase-5-release.md` | |

## Human checkpoints (the executor must stop and ask)

- Phase 0, Task 7 — before deleting `backend/`: the untracked personal file `backend/assets/static/Ai căutat Gigabyte … eMAG.r.html` and untracked `backend/.env` / `config/prod.secret.exs` are not recoverable from git.
- Phase 4, Task 15 — before `git rm -r -f frontend`: the uncommitted edit to `frontend/src/elm/Pages/Login.elm` will be lost (the plan offers to save it as a patch).
- Any push, PR, or CI-dependent acceptance step.
- Never stage with `git add -A` / `git add .`.

## Open decision

- Phase 4: finished games' scoreboards are viewable only while the GameServer is alive (30 min idle timeout). Recommended addition: render the scoreboard for `status: "finished"` games directly from `games.final_scores` without starting the engine. Decide before executing Phase 4 Task 11.

## Predicted vs. observed

Plans were written without a Rust toolchain; expected command outputs (clippy findings, test counts, the pinned seed-42 deal in Phase 3 Task 9) are predictions. Follow the plans' instructions where they say to take values from a real run.
