# Phase 5 — Release & Cleanup

Status: approved design (2026-09-23). Overview: `2026-09-23-migration-overview-design.md`. Depends on Phase 4.

## Goal

A reproducible production image, accurate documentation, and final CI for the three-part repo (`helios/`, `core/`, `e2e/`). No hosting target is chosen in this phase.

## Current state
- Root `Dockerfile` builds a Go binary from `backend/` and the Elm frontend (golang:1.14, node:9, ubuntu:bionic), references non-existent `backend/scripts` — fully stale.
- `bin/build.sh` (`docker build -t seven_wonders:$(cat VERSION)`), `bin/run.sh` (`docker run -p 8080:8080 --env-file $1`).
- `README.md` documents Go/Elm prerequisites, Heroku and Travis badges, protobuf generation.
- `.dockerignore` excludes `.git`, node_modules, elm-stuff, build, `**/.env`.

## Design

### Dockerfile (multi-stage, repo root)
- **Builder**: `hexpm/elixir:1.19.5-erlang-28.<x>-debian-<release>-<date>` (exact tag pinned, matching `mise.toml`); install `build-essential`, `git`, `curl`, and Rust via `rustup` pinned to the `mise.toml` version (minimal profile). `WORKDIR /app`; copy `core/` and `helios/` preserving the relative `../core` path Rustler expects (`/app/core`, `/app/helios`). `MIX_ENV=prod`; `mix local.hex --force && mix local.rebar --force`; `mix deps.get --only prod`; `mix deps.compile`; `mix assets.deploy`; `mix compile` (builds the NIF in release mode — Rustler `mode: :release` in prod); `mix release`.
- **Runtime**: matching `debian:<release>-slim`; install `libstdc++6 openssl libncurses6 locales ca-certificates`; UTF-8 locale; non-root `nobody` user; copy `_build/prod/rel/helios`. `ENV PHX_SERVER=true`. `EXPOSE 4000`. `CMD ["/app/bin/server"]` (generated via `mix phx.gen.release`, which also adds `bin/migrate` and `Helios.Release.migrate/0`). Migrations run at boot via the existing `Ecto.Migrator` child (active when `RELEASE_NAME` is set).
- Required env: `SECRET_KEY_BASE`, `DATABASE_PATH` (e.g. `/data/helios.db` on a mounted volume), `ACCESS_TOKEN`, `PHX_HOST`; optional `PORT`, `POOL_SIZE`.
- `.dockerignore`: add `**/_build`, `**/deps`, `core/target`, `e2e/`, `**/*.db*`, `**/node_modules`, `.git`, `**/.env`, `docs/`.

### Scripts
- `bin/build.sh`: `docker build -t seven_wonders:$(cat VERSION) .` (unchanged behaviour, verified).
- `bin/run.sh`: `docker run -p 4000:4000 -v seven_wonders_data:/data --env-file "$1" seven_wonders:$(cat VERSION)`.
- Add `bin/.env.example` listing the required env vars with placeholder values.

### README.md (rewrite)
- What the project is; architecture diagram (from the overview spec).
- Setup: `mise install`, hex/rebar, `cd helios && mix setup`, `mix phx.server` (dev `ACCESS_TOKEN` defaults to `dev`).
- Tests: `cd core && cargo test`; `cd helios && mix precommit`; `cd e2e && npm ci && npx playwright install chromium && npx playwright test`.
- Docker: build/run scripts and env vars.
- Replace Heroku/Travis badges with the GitHub Actions CI badge.

### CI (final `.github/workflows/ci.yml`)
Jobs: `core`, `helios`, `e2e` (as built in Phases 0–2) plus `docker`: `docker build .` (no push) and a smoke run: start the container with test env vars and a temp volume, poll `GET /login` for HTTP 200 within 60 s.

### Cleanup
- Remove any remaining references to deleted folders (`grep -rE "backend_old|websocket-client|integration-tests|frontend/|proto/"` outside `docs/` returns nothing).
- Root `AGENTS.md`: drop the legacy-folders paragraph and replace "Migration in progress" with a short history note.
- Delete stray root files that are artifacts (`erl_crash.dump` is already ignored; delete if present).

## Acceptance criteria
- `bin/build.sh` succeeds locally; `bin/run.sh bin/.env.example`-style run serves `/login`, a user can log in, and data persists across container restarts (volume).
- CI: `core`, `helios`, `e2e`, `docker` all green.
- README instructions work from a clean clone.

## Amendments from planning (2026-09-23)
- Build order in the Dockerfile is `mix compile` before `mix assets.deploy` (colocated hooks are emitted at compile time).
- `/data` is pre-created and owned by `nobody` in the runtime image so a named volume is writable.
- `bin/build.sh` passes the Rust version from `mise.toml` as a build arg; `bin/smoke.sh` (added) runs the login + persistence smoke test for both CI and manual verification.
- `config/prod.exs` redirects non-localhost hosts to https; README documents running behind a TLS proxy.
