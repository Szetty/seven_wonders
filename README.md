# Seven Wonders Digital version

![CI](https://github.com/Szetty/seven_wonders/actions/workflows/ci.yml/badge.svg)
![Heroku](https://heroku-badge.herokuapp.com/?app=seven-wonders-szetty)

Online implementation of the 7 Wonders board game (base game, 3–7 players).

## Repository layout

- [`helios/`](helios/) — Phoenix 1.8 / LiveView app: UI, auth, lobby, game orchestration, SQLite persistence
- [`core/`](core/) — Rust 7 Wonders engine, loaded into Helios as a Rustler NIF
- [`e2e/`](e2e/) — Playwright end-to-end tests (chromium, mobile and responsive projects)
- [`docs/superpowers/`](docs/superpowers/) — design specs and implementation plans
- [`bin/`](bin/) — deployment scripts

Legacy folders (`backend_old/`, `frontend/`, `websocket-client/`) are deprecated: kept only as behavioural reference and removed phase by phase (see [`AGENTS.md`](AGENTS.md)).

The game UI is responsive: desktop, phones (360 px and up) and landscape phones (custom `short` variant for viewports ≤ 500 px tall).

## Prerequisites

The toolchain is pinned in [`mise.toml`](mise.toml) (Erlang 28, Elixir 1.19, Rust 1.98, Node 24):

```shell
mise install
mise exec -- mix local.hex --force
mise exec -- mix local.rebar --force
```

Prefix `mix`, `cargo` and `npx` commands with `mise exec --` — a bare `mix` may resolve to a broken install.

## Development

### Core (Rust engine)

```shell
cd core
mise exec -- cargo fmt --check
mise exec -- cargo clippy --all-targets -- -D warnings
mise exec -- cargo test
```

Details in [`core/README.md`](core/README.md).

### Web app (Helios)

```shell
cd helios
mise exec -- mix setup
mise exec -- mix phx.server
```

Open http://localhost:4000. Before committing, run the full check (compile with warnings as errors, format, ExUnit):

```shell
cd helios
mise exec -- mix precommit
```

Details in [`helios/README.md`](helios/README.md).

### End-to-end tests (Playwright)

Playwright boots its own Helios instance (`MIX_ENV=e2e`) on port 4004 and reuses it between runs:

```shell
cd e2e
npm ci
npx playwright install chromium
mise exec -- npx playwright test                 # all projects
mise exec -- npx playwright test --project=mobile  # one project
```

## CI

[`.github/workflows/ci.yml`](.github/workflows/ci.yml) runs three jobs on every push and pull request:

- **Core** — `cargo fmt --check`, clippy with `-D warnings`, `cargo test`
- **Helios** — compile with `--warnings-as-errors` (builds the NIF), `mix format --check-formatted`, `mix test`
- **E2E** — full Playwright suite against an `MIX_ENV=e2e` server

## Deployment

```shell
bin/build.sh
bin/run.sh
```
