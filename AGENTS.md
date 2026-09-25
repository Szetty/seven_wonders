# Seven Wonders — Agent Guide

An online implementation of the 7 Wonders board game (base game, 3–7 players).

## Repository layout

Target layout (see `docs/superpowers/specs/2026-09-23-migration-overview-design.md`):

- `helios/` — Phoenix 1.8 / LiveView app: UI, auth, lobby, game orchestration, SQLite persistence. **Read `helios/AGENTS.md` before touching it**; it takes precedence for files under `helios/`.
- `core/` — Rust 7 Wonders engine (crate `seven_wonders_core`), loaded into Helios as a Rustler NIF via `Helios.Core` / `Helios.Core.Native`.
- `e2e/` — Playwright end-to-end tests. Standalone package that drives a running Helios (`MIX_ENV=e2e`, port 4004). No application code.
- `docs/superpowers/` — design specs (`specs/`) and implementation plans (`plans/`).

Legacy folders — `backend/`, `backend_old/`, `frontend/`, `websocket-client/`, `integration-tests/`, `proto/` — are deprecated. Use them only as behavioural reference; never add features to them. Each is deleted by the migration phase that replaces it.

## Migration in progress

The repo is being migrated in phases 0–5. Before doing migration work, read `docs/superpowers/plans/README.md` (order, human checkpoints, open decisions) and check `git log` to see which phase/tasks are done. Follow the plans; if a plan can't be followed as written, stop and ask rather than redesigning.

## Toolchain

Versions are pinned in `mise.toml` (Erlang, Elixir, Rust, Node). Run `mise install`; for Elixir also `mix local.hex --force && mix local.rebar --force`.

## Commands

| Part | Check |
|---|---|
| core | `cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test` |
| helios | `cd helios && mix precommit` |
| e2e | `cd e2e && npm ci && npx playwright install chromium && npx playwright test` |

Run the checks for every part you changed before claiming work is done.

## Architecture rules

- **All game rules live in `core`.** Elixir orchestrates, persists and broadcasts; it never re-implements rules (legality, costs, trading, scoring).
- **NIF boundary uses Rustler native terms** (`NifMap` / `NifTaggedEnum` / `NifUnitEnum` DTOs in `core/src/nif/`). No protobuf, no JSON (except `debug_game`). The `game` module stays free of rustler imports.
- **The engine must be deterministic** — games are persisted as seed + action log and replayed. All randomness comes from the seeded `ChaCha8Rng` in `Game::new`; never iterate a `HashMap` where order can affect outcomes; bump `ENGINE_VERSION` on any rule change that could alter a replay.
- **Hidden information stays on the server**: PubSub broadcasts carry no game state; each player fetches their own view.
- SQLite via Ecto; keep queries portable (no SQLite-only SQL).

## Git rules

- Stage explicit paths only; never `git add -A` or `git add .`.
- Don't delete untracked files or push without asking the user.
