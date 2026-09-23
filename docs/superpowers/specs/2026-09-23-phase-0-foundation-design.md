# Phase 0 — Foundation

Status: approved design (2026-09-23). Overview: `2026-09-23-migration-overview-design.md`.

## Goal

A committed, building, CI-verified baseline: Helios in git, a modernized `core` crate loaded by Helios through a native-terms Rustler NIF, and the deprecated `backend/` and `proto/` removed.

## Current state (facts this phase acts on)

- `helios/` and `mise.toml` are untracked. `helios/lib/helios_web/endpoint.ex` (~L30-32) hardcodes a Tidewave team token.
- Hex is not installed for the mise Elixir (`mix compile` fails with "Could not find an SCM for dependency :tidewave"). No Rust toolchain is installed.
- `helios/lib/helios_web/live/login_live.ex`: handlers match `%{"login_form" => ...}` but `to_form(map)` without `as:` produces flat params; `phx-hook="LoginEnter"` does not match the colocated hook name `.LoginEnter`. `login_live_test.exs` "handles login attempt" is expected to fail.
- `core/Cargo.toml`: edition 2018, `crate-type = ["dylib"]`, package/lib name `core` (collides with Rust's built-in `core` crate), rustler 0.21.1, protobuf 2.22, derive_more 0.99, strum 0.20, rand 0.8, itertools 0.10.
- `core/src/lib.rs` uses rustler-0.21-only APIs: `#[macro_use] extern crate rustler`, `rustler_atoms!`, `rustler_export_nifs!`, `resource_struct_init!`, `rustler::resource::ResourceArc`, `fn(Env, &[Term]) -> Result<Term, Error>` NIF signatures, `load(env, info) -> bool`.
- `core/src/api/{ping,game_settings,start_game}.rs` are protobuf-generated; `core/src/api/mod.rs` holds `ErrorType`, `ping`, `game_settings`, `start_game`, `SafeGameState = Mutex<GameState>`.
- 37 Rust tests exist (`core/src/tests/*` + inline); never run on a modern toolchain.
- CI (`.github/workflows/ci.yml`) pins Rust 1.51, Elixir 1.11, OTP 22.3; has jobs `core`, `backend`, `old_backend`, `frontend`, `integration-tests`; no helios job.
- An untracked personal file `backend/assets/static/Ai căutat Gigabyte X570 … eMAG.r.html` exists. **It must not be touched by automation**; the user is warned it will disappear with `backend/` deletion (it is untracked, so git cannot recover it).

## Design

### 0.1 Repo hygiene & commit Helios
- Root `.gitignore`: add `.DS_Store`, `erl_crash.dump`.
- Replace the hardcoded Tidewave token with `System.get_env("TIDEWAVE_TOKEN")`; when unset, do not plug Tidewave's team config (plain `plug Tidewave`).
- `mise.toml`: add `rust = "<current stable>"` (pinned exact version).
- Commit `helios/`, `mise.toml`, `MIGRATION.md`. The user's uncommitted `frontend/src/elm/Pages/Login.elm` edit is left untouched (not staged).

### 0.2 Toolchain bootstrap
- Document and run `mise install` and `mise exec -- mix local.hex --force && mix local.rebar --force`.
- `cd helios && mix deps.get && mix compile` must succeed.

### 0.3 Fix the LoginLive placeholder
- Use `to_form(params, as: :login)` and match `%{"login" => params}`; fix the colocated hook reference to `.LoginEnter` (or drop the hook — a form submits on Enter natively; prefer dropping it).
- Update `login_live_test.exs` accordingly. `mix precommit` passes.
- Behaviour stays placeholder (flash only); Phase 1 replaces it.

### 0.4 Modernize `core`
- `Cargo.toml`: `edition = "2021"`, package and lib name `seven_wonders_core`, `crate-type = ["cdylib", "rlib"]` (`rlib` so `cargo test` can link the library normally), latest rustler (≥ 0.36), remove `protobuf`. Bump `derive_more` (1.x, `features = ["display"]`, update `#[display(fmt = ...)]` → `#[display("...")]`), `strum`/`strum_macros` (latest), `rand` (latest; adapt renamed APIs), `itertools` (latest). Keep `lazy_static`, `maplit`, `serde`, `serde_json`.
- Rename internal `mod core` → `mod engine` (path `src/engine/`), updating all `crate::core::` paths.
- Delete `src/api/{ping,game_settings,start_game}.rs` and `Makefile.toml`'s `gen_proto` task (delete `Makefile.toml` if empty). Rewrite `src/api/mod.rs` as plain Rust:
  - `pub fn game_settings() -> GameSettings { version: String, wonders: Vec<String> }`
  - `pub fn start_game(players: Vec<String>, wonder_sides: Vec<WonderSideChoice>) -> Result<Mutex<GameState>, ErrorType>` with `WonderSideChoice { wonder_name: String, side_b: bool }`, same validation as today.
- Rewrite `src/lib.rs` NIF layer with rustler ≥ 0.36:
  - `rustler::atoms! { ok, error, invalid_players_number, invalid_players_and_wonder_side_length, invalid_wonder, lock_fail }`
  - `pub struct GameResource(Mutex<GameState>)` with `#[rustler::resource_impl] impl rustler::Resource for GameResource {}`.
  - `#[derive(NifMap)] struct GameSettings`, `#[derive(NifMap)] struct WonderSideChoice`.
  - `#[rustler::nif] fn game_settings() -> GameSettings`
  - `#[rustler::nif] fn start_game(players: Vec<String>, wonder_sides: Vec<WonderSideChoice>) -> Result<ResourceArc<GameResource>, (Atom, Term)>` — errors encode as `{:error, {reason_atom, detail}}` where detail is the player count (integer) or a string.
  - `#[rustler::nif] fn debug_game(game: ResourceArc<GameResource>) -> Result<String, Atom>` (JSON via `serde_json`; `{:error, :lock_fail}` if the mutex is poisoned/locked).
  - `rustler::init!("Elixir.Helios.Core.Native");`
  - `ping` is removed.
- Tests in `src/tests/api.rs` updated to the new plain-Rust API. All 37 tests pass (the ping test, if any, is removed). `cargo fmt --check` and `cargo clippy -- -D warnings` pass (fix or `#[allow]` with justification for clippy findings in untouched legacy code — prefer fixing).
- **No rule changes** in this phase.

### 0.5 NIF bridge in Helios
- `helios/mix.exs`: add `{:rustler, "~> <same minor as crate>", runtime: false}`.
- `lib/helios/core/native.ex`:
  ```elixir
  defmodule Helios.Core.Native do
    use Rustler, otp_app: :helios, crate: "seven_wonders_core", path: "../core"
    def game_settings, do: :erlang.nif_error(:nif_not_loaded)
    def start_game(_players, _wonder_sides), do: :erlang.nif_error(:nif_not_loaded)
    def debug_game(_game), do: :erlang.nif_error(:nif_not_loaded)
  end
  ```
- `lib/helios/core.ex` — public wrapper with `@spec`s and docs:
  - `game_settings() :: %{version: String.t(), wonders: [String.t()]}`
  - `start_game(players :: [String.t()], wonder_sides :: [{String.t(), boolean()}]) :: {:ok, reference()} | {:error, {atom(), term()}}` — converts tuples to `%{wonder_name: _, side_b: _}` maps.
  - `debug_game(ref) :: {:ok, map()} | {:error, atom()}` — `Jason.decode!`.
- `test/helios/core_test.exs`: settings contain 7 wonders + version; start_game ok with 3 players (random) and with explicit sides; errors for 2 players, 8 players, length mismatch, unknown wonder; debug_game returns a map with `"player_states"` for each player.
- `helios/.gitignore`: add `/priv/native/`.

### 0.6 CI
Replace `.github/workflows/ci.yml` jobs:
- `core`: checkout → `jdx/mise-action` → `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` (working dir `core`). Cache `~/.cargo` + `core/target`.
- `helios`: checkout → `jdx/mise-action` → `mix local.hex --force && mix local.rebar --force` → `mix deps.get` → `mix precommit` equivalent for CI: `mix compile --warnings-as-errors`, `mix format --check-formatted`, `mix test` (working dir `helios`). Cache `helios/deps`, `helios/_build`, `core/target`.
- Keep `old_backend`, `frontend`, `integration-tests` jobs untouched (deleted in Phase 2). Remove the `backend` job.
- Update actions to current major versions (`actions/checkout@v4`, `actions/cache@v4`).

### 0.7 Delete `backend/` and `proto/`
- After 0.5 is green: `git rm -r backend proto`. Update README references to them (minimal: remove the "Backend new" and gen_proto sections).

## Acceptance criteria
- `cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test` passes.
- `cd helios && mix precommit` passes, including `Helios.Core` tests exercising the real NIF.
- CI `core` and `helios` jobs green.
- `backend/`, `proto/` gone; no file references `Core.Api` or protobuf.
- No secrets in committed source (`grep -r "token:" helios/lib` shows no literal tokens).

## Risks
- Rustler/derive_more/rand upgrades may require non-trivial source edits in legacy code; keep them mechanical and covered by the existing tests.
- Crate rename: Rustler's `crate:` option must match the Cargo package name.
