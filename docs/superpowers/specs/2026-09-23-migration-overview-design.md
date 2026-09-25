# Migration Overview — Helios + Core (+ e2e)

Status: approved design (brainstorming 2026-09-23). Supersedes the original `MIGRATION.md` draft (since removed); this spec set is the source of truth.

## Goal

Reduce the repository to:

1. **`helios/`** — Phoenix 1.8 / LiveView 1.1 app: UI, auth, lobby, game orchestration, persistence (SQLite).
2. **`core/`** — Rust 7 Wonders engine, loaded into Helios as a Rustler NIF. Owns **all** game rules.
3. **`e2e/`** — Playwright end-to-end tests, a standalone package that drives a running Helios. Contains no application code.

Everything else (`backend/`, `backend_old/`, `frontend/`, `websocket-client/`, `integration-tests/`, `proto/`) is deleted as soon as its replacement is verified.

## Cross-cutting decisions

| Topic | Decision | Why |
|---|---|---|
| Rules ownership | Rust engine owns dealing, legality, payments, hand passing, battles, ages, scoring. Elixir only orchestrates, persists, broadcasts. | Single source of truth; `cargo test` covers rules; replay = seed + actions. |
| NIF boundary | Rustler native terms (`NifMap`/`NifStruct`/`NifTaggedEnum`) on small boundary DTOs. No protobuf, no JSON (except `debug_game`). | Fastest option (no intermediate encoding), no codegen. Internal `GameState` never crosses the boundary. |
| Game durability | Action log + replay: persist `seed` + every accepted action; rebuild engine by replaying. | Games survive restarts/deploys; free history. Requires a deterministic engine. |
| Determinism | `rand_chacha::ChaCha8Rng` seeded with `u64`; no `HashMap` iteration in outcome-affecting code; `engine_version` stored per game. | Replays must produce identical state across processes and releases. |
| Database | SQLite via `ecto_sqlite3`; keep Ecto code portable (no SQLite-only SQL). | Already configured; simple deploys. |
| Identity | Shared `ACCESS_TOKEN` + permanent name, no password. A name is "held" while its user is online (Presence); logging in with a known, un-held name re-enters that account. | User choice; avoids lock-out from stale cookies. |
| Sessions | Random token in Phoenix session cookie; SHA-256 hash stored in `user_tokens`. No JWT. | Idiomatic Phoenix 1.8; `JWT_SECRET` goes away. |
| Lobby model | Port the Go model: each user owns one lobby; owner invites; invitee accepts (navigates) or declines; persisted invites. | Proven semantics encoded by the legacy integration tests. |
| Real-time | Phoenix PubSub + Presence inside LiveView. No custom WS protocol. | Replaces the envelope protocol entirely. |
| Game UI | HEEx + Tailwind v4 HTML components (no SVG canvas, no pan/zoom). | Elm game page was only a static mock; HTML is responsive, clickable, testable. |
| Styling | Tailwind v4, no daisyUI, no `@apply` (per `helios/AGENTS.md`). Visual identity carried over from Elm: `7_wonders.jpg` login backdrop, antique-white cards, teal→blue header gradient, `paper.jpg` parchment, dark buttons. | |
| Rules scope | Full 7 Wonders base game, 3–7 players, all wonder specials. No expansions. | |
| Toolchain | `mise.toml` pins Erlang 28, Elixir 1.19.5-otp-28, Rust stable. CI uses `jdx/mise-action`. | One source of truth for versions. |
| Testing | Playwright (in `e2e/`) for user flows; ExUnit for domain/LiveView/GenServer logic; `cargo test` for engine rules. | |

## Phases

| # | Phase | Spec | Depends on | Deletes at end |
|---|---|---|---|---|
| 0 | Foundation | `2026-09-23-phase-0-foundation-design.md` | — | `backend/`, `proto/` |
| 1 | Auth & Users | `2026-09-23-phase-1-auth-design.md` | 0 | — |
| 2 | Lobby | `2026-09-23-phase-2-lobby-design.md` | 1 | `backend_old/`, `websocket-client/`, `integration-tests/` (+ `frontend` CI job) |
| 3 | Engine gameplay API | `2026-09-23-phase-3-engine-design.md` | 0 (independent of 1–2; may run in parallel) | — |
| 4 | GameServer + GameLive | `2026-09-23-phase-4-game-design.md` | 2, 3 | `frontend/` |
| 5 | Release & cleanup | `2026-09-23-phase-5-release-design.md` | 4 | `bin/` leftovers, stale docs |

Each phase has its own implementation plan in `docs/superpowers/plans/`.

## Target architecture

```
e2e/ (Playwright) ──HTTP/WS──▶ helios (Phoenix 1.8)
                                ├── LiveViews: LoginLive, LobbyLive, GameLive
                                ├── Accounts (users, user_tokens)  ─┐
                                ├── Lobbies (lobbies, lobby_invites) ├─ Ecto / SQLite
                                ├── Games (games, game_players,     ─┘
                                │          game_actions)
                                ├── Presence: users:online, lobby:<id>, game:<id>
                                ├── PubSub: user:<id>, lobby:<id>, game:<id>
                                ├── GameServer (DynamicSupervisor + Registry), one per game
                                └── Helios.Core ─▶ Helios.Core.Native (Rustler NIF)
                                                     └── core/ (seven_wonders_core crate)
```

## Out of scope

Expansions (Leaders, Cities, …), AI players, turn timers, spectators, choosing wonders manually, passwords/accounts recovery, a specific hosting target, Postgres.
