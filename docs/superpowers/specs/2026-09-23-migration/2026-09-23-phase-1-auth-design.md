# Phase 1 — Auth & Users

Status: approved design (2026-09-23). Overview: `2026-09-23-migration-overview-design.md`. Depends on Phase 0.

## Goal

Real login/logout backed by SQLite, authenticated LiveView sessions with `current_scope`, global online presence, the Helios design base (no daisyUI, Elm visual identity), and the standalone `e2e/` Playwright harness with login flows covered.

## Legacy behaviour being replaced

Go `POST /api/login` / Phoenix `backend` `LoginController`:
- Check order: access token first (`INVALID_ACCESS_TOKEN`, 401), then name (`INVALID_NAME`, 400: empty or taken).
- Only name rules: non-empty and not currently taken. No expiry on tokens. Logout frees the name.
- Elm routes: `/` → guest to `/login`, logged-in to own lobby; `/login` → logged-in to own lobby; protected pages → guest to `/login`.
- Elm messages: "Access token can't be empty!", "Name can't be empty!"; server message shown for known 4xx errors.

## Design

### Data
Migration `create_users`:
- `users`: `id` (integer PK), `name` (string, not null), timestamps. `unique_index(:users, [:name])` (exact, case-sensitive — same as Go).

Migration `create_user_tokens`:
- `user_tokens`: `id`, `user_id` (FK, `on_delete: :delete_all`), `token_hash` (binary, not null), `inserted_at`. `unique_index(:user_tokens, [:token_hash])`, `index(:user_tokens, [:user_id])`.

### `Helios.Accounts` context
- `User` schema; `UserToken` schema.
- Name normalisation/validation (`User.name_changeset/2`): `String.trim/1`; required; length 1..24. Messages: "Name can't be empty!", "Name must be at most 24 characters".
- `check_login(access_token, name) :: :ok | {:error, :invalid_access_token | :invalid_name, message}`
  1. `access_token` empty → `{:error, :invalid_access_token, "Access token can't be empty!"}`; mismatch with `Application.fetch_env!(:helios, :access_token)` (constant-time compare via `Plug.Crypto.secure_compare/2`) → `{:error, :invalid_access_token, "Wrong access token"}`.
  2. Invalid name per changeset → `{:error, :invalid_name, msg}`.
  3. Name belongs to a user currently online (`HeliosWeb.Presence` topic `"users:online"` has that user id) → `{:error, :invalid_name, "Name is already taken"}`.
- `login(access_token, name) :: {:ok, user, token} | {:error, reason, message}` — re-runs `check_login`, then in a transaction: get-or-insert user by name; delete that user's existing tokens (collecting their hashes); insert a new token. After commit, broadcast `"disconnect"` to each revoked token's `live_socket_id` so stale sessions elsewhere drop. Returns the raw token (32 random bytes).
- `get_user_by_session_token(token) :: User.t() | nil` — valid if the hash exists and `inserted_at` is within 60 days.
- `delete_session_token(token) :: :ok`.
- `Helios.Accounts.Scope` struct `%Scope{user: User.t() | nil}` with `for_user/1` (Phoenix 1.8 scope convention).

Error atoms mirror the legacy contract: `:invalid_access_token`, `:invalid_name`, `:unauthorized` (no session), `:invalid_user` (session token unknown/revoked/expired). `:invalid_game_id` is introduced in Phase 2.

### Web layer
- `HeliosWeb.UserAuth` (plug + LiveView hooks):
  - `fetch_current_scope` plug: reads `:user_token` from session, assigns `current_scope`.
  - `log_in_user(conn, user, token)`: renew session, `put_session(:user_token, token)`, `put_session(:live_socket_id, "users_sessions:" <> Base.url_encode64(token_hash))`, redirect to `/`.
  - `log_out_user(conn)`: delete token, broadcast `"disconnect"` on the live_socket_id, clear session, redirect `/login`.
  - `on_mount(:require_user, ...)`: no token → redirect `/login`, flash "You must log in to access this page." (`:unauthorized`); token present but user not found → redirect `/login`, flash "Your session has expired, please log in again." (`:invalid_user`), and the session is cleared on the next HTTP request via the plug. On success assign `current_scope`; if `connected?(socket)`, `HeliosWeb.Presence.track(self(), "users:online", user.id, %{name: user.name})`.
  - `on_mount(:redirect_if_user, ...)`: logged in → redirect `/`.
- `HeliosWeb.Presence` (`use Phoenix.Presence, otp_app: :helios, pubsub_server: Helios.PubSub`) added to the supervision tree.
- `HeliosWeb.UserSessionController`: `create/2` (POST `/session`, params `%{"login" => %{"access_token", "name"}}`) → `Accounts.login` → `UserAuth.log_in_user`, or on error redirect `/login` with the message as flash; `delete/2` (DELETE `/session`) → `UserAuth.log_out_user`.
- `LoginLive` (`/login`): form `to_form(%{"access_token" => "", "name" => ""}, as: :login)`; `phx-change="validate"` clears errors; `phx-submit="login"` calls `Accounts.check_login/2`; on `:ok` sets `trigger_submit: true` so the form (`action={~p"/session"}` `method="post"` `phx-trigger-action={@trigger_submit}`) posts to the controller; on error shows the message under the form. Inputs: Access Token (`type="password"`, `autocomplete="off"`), Name (`autocomplete="username"`).
- Routes:
  - `live_session :guest, on_mount: [{UserAuth, :redirect_if_user}]` → `live "/login", LoginLive`.
  - `live_session :authenticated, on_mount: [{UserAuth, :require_user}]` → `live "/lobby", LobbyStubLive` (Phase 1 only: greeting "Welcome, <name>" — replaced in Phase 2).
  - `get "/", PageController, :home` → redirect to `/lobby` if logged in, else `/login`. (Phase 2 changes the target to `/lobby/:game_id`.)
  - `post "/session"`, `delete "/session"`.
  - Remove the default `page_html/home.html.heex`.
- `HeliosWeb.Layouts.app/1`: full-bleed `<main>` (no `max-w-2xl`); renders `<.site_header current_scope={@current_scope} />` when a user is present: rounded teal→blue gradient bar, username on the left, "Logout" button (`<.link href={~p"/session"} method="delete">`) on the right. Remove `theme_toggle` and the root-layout theme script (single light theme).

### Design base
- `assets/css/app.css`: remove the daisyUI plugin and both `daisyui-theme` blocks; delete `assets/vendor/daisyui*.js`. Add `@theme` tokens: `--color-antique: #faebd7`, `--color-header-from: rgb(18 147 150)`, `--color-header-to: rgb(18 147 255)`, `--color-disconnected: #d6d9dc`, `--font-sans: "Source Sans Pro", "Trebuchet MS", "Lucida Grande", "Helvetica Neue", sans-serif`. Utility-first; no `@apply`.
- `core_components.ex`: restyle `button`, `input`, `flash`, `header`, `table` with plain Tailwind (dark buttons `bg-zinc-900 text-white hover:bg-zinc-700 transition`), removing all daisyUI class names (`btn`, `input`, `alert`, `toast`, `table-zebra`, …).
- LoginLive look: `7_wonders.jpg` full-screen cover background, "7 WONDERS" heading, antique-white card with the form.

### Config
- `config/config.exs`: `config :helios, access_token: "dev"`.
- `config/test.exs`: `config :helios, access_token: "test"`.
- `config/runtime.exs` (prod): `access_token: System.get_env("ACCESS_TOKEN") || raise "ACCESS_TOKEN is missing"`. In dev, `ACCESS_TOKEN` env overrides the default if set.
- `config/e2e.exs` (new `MIX_ENV=e2e`): imports dev-like endpoint config without watchers/live reload; `server: true`, port from `PORT` (default 4004), database `helios_e2e.db`, `access_token: "e2e"`, logger level `:warning`, `code_reloader: false`, `debug_errors: false`. `helios/.gitignore` already ignores `*.db`.

### `e2e/` package (repo root)
- `e2e/package.json`: devDependencies `@playwright/test`, `typescript`; scripts `test`, `test:ui`.
- `e2e/playwright.config.ts`: `baseURL: "http://localhost:4004"`, Chromium project, `fullyParallel: true`, `webServer: { command: "cd ../helios && MIX_ENV=e2e mix do ecto.reset + assets.build + phx.server", url: "http://localhost:4004/login", reuseExistingServer: !process.env.CI, timeout: 180_000, env: { PORT: "4004" } }`.
- `e2e/tests/support/auth.ts`: `uniqueName(prefix)` (random suffix, ≤ 24 chars), `login(page, name, token = "e2e")`, `logout(page)`.
- `e2e/tests/auth.spec.ts`:
  1. Login succeeds → lands on `/lobby`, header shows the name.
  2. Wrong access token → "Wrong access token" shown, still on `/login`.
  3. Empty access token / empty name → respective messages.
  4. Name taken: context A logged in and on `/lobby`; context B logs in with the same name → "Name is already taken".
  5. Re-enter: A logs out; B logs in with the same name → succeeds.
  6. Guest visiting `/lobby` → redirected to `/login`.
  7. Logged-in user visiting `/login` → redirected to `/lobby`.
  8. Logout → `/login`; revisiting `/lobby` → `/login`.
- `mise.toml`: add `node = "<current LTS>"`.
- `e2e/.gitignore`: `node_modules/`, `test-results/`, `playwright-report/`.

### CI
New `e2e` job: checkout → `jdx/mise-action` → hex/rebar → `cd helios && mix deps.get` → `cd e2e && npm ci && npx playwright install --with-deps chromium && npx playwright test`. Upload `playwright-report/` on failure.

## Testing (ExUnit)
- `Accounts`: check_login order and messages; name trimming/length; login creates user; login re-enters existing un-held user and revokes old tokens; login rejects held name (track a Presence entry in the test); token expiry (60 days) via inserting an old token; delete_session_token.
- `UserAuth`: on_mount redirects for guest and invalid token; `:redirect_if_user`.
- `LoginLive`: renders; validation messages; successful check sets `phx-trigger-action`.
- `UserSessionController`: create success sets session + redirects; failure redirects with flash; delete clears session.

## Acceptance criteria
- `mix precommit` green; `cd e2e && npx playwright test` green locally and in CI.
- No daisyUI references remain (`grep -ri daisy helios/assets helios/lib` empty).

## Amendments from planning (2026-09-23)
- Presence keys are strings in `Presence.list/1` and diffs: track `to_string(user.id)`; `HeliosWeb.Presence` exposes `online_topic/0`, `track_user/2`, `user_online?/1`.
- `config/runtime.exs` sets the port for every env; it must default to 4004 when `MIX_ENV=e2e`.
- Playwright `webServer` order is `assets.build + ecto.reset + phx.server` (seeds would start the endpoint before assets exist otherwise). CI pre-compiles the e2e env before Playwright to stay under the server timeout.
- `Scope.for_user(nil)` returns `nil`; guests have `current_scope == nil`.

## Known shortcomings (found during implementation, 2026-09-24)

### SQLite `Database busy` under concurrent writes

**Symptom.** With the Playwright suite running at default parallelism (~6 workers), logins in parallel hit
`Exqlite.Error) Database busy` on `INSERT INTO "users" ... ON CONFLICT ("name") DO NOTHING` inside
`Accounts.get_or_insert_user!/1` → `start_session/1` → `login/2` (`Ecto.Repo.transact`). The `POST /session`
request 500s (no redirect to `/lobby`). Observed over five default-parallel runs: `7/2`, `6/3`, `7/2`,
`8/1`, `7/2` passed/failed — never fully green; `npx playwright test --workers=1` passes `9/9`.

**Root cause.** SQLite has a single writer; Ecto holds `pool_size: 5` connections and `login/2` runs its
writes in a transaction started with the default *deferred* mode. Under concurrent logins the deferred
transactions conflict when upgrading to a write lock, and the driver's `busy_timeout` does not retry that
upgrade — the statement fails with `SQLITE_BUSY` instead of waiting. This is app-side contention, not a
test-harness bug: the scenarios are independent and serial execution proves the logic is correct.

**Current mitigation.** `e2e/playwright.config.ts` sets `workers: 1` (with a comment). This is a recorded
deviation from this plan's Global Constraint `fullyParallel: true`, approved because the alternative was
a flaky suite (evidence and decision trail: commits `6c5233f`, `eabe363`). **Raising workers again will
reintroduce the flakiness.**

**Impact beyond e2e.** Any runtime path with concurrent writers can 500 the same way — a burst of logins
today; Phase 2+ lobby/invite writes and concurrent game actions add more writers. At 3–7 players on one
node the exposure is small but unproven; it is currently unmitigated outside the test harness.

**Options for a follow-up fix (none chosen yet):**
1. Single-writer discipline for SQLite (lower `pool_size`, or serialize writes through one process) — simplest, probably sufficient at this scale.
2. Start write transactions in immediate mode (`BEGIN IMMEDIATE`) so writers queue on the file lock instead of upgrading — requires checking what `ecto_sqlite3` exposes for transaction begin.
3. Application-level retry-on-busy around write transactions.
4. Set `journal_mode=WAL` and an explicit `busy_timeout` pragma — good hygiene, but **not sufficient alone** for the deferred-upgrade case.

**Acceptance for the eventual fix:** default-parallel `npx playwright test` is green repeatedly (≥5 runs)
with `workers: 1` removed from `e2e/playwright.config.ts`.
