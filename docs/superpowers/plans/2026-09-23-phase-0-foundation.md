# Phase 0 — Foundation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce a committed, building, CI-verified baseline: Helios in git, a modernized `seven_wonders_core` crate loaded by Helios through a native-terms Rustler NIF (`Helios.Core.Native` / `Helios.Core`), and the deprecated `backend/` and `proto/` removed.

**Architecture:** First bootstrap the toolchain (mise: Rust, Hex/Rebar), remove the hardcoded Tidewave token, and commit the untracked Helios app as-is. Then fix the LoginLive placeholder. Next, modernize `core/` in two steps: dependencies, edition, the `core`→`engine` rename and a plain-Rust `api` module (tested by `cargo test`), followed by fmt/clippy cleanup. After that, put a thin rustler 0.38 NIF layer in `core/src/lib.rs` on top of `api` and wrap it in Elixir (`Helios.Core.Native` + `Helios.Core`, tested by ExUnit against the real NIF). Finally, rewrite CI around `jdx/mise-action` and delete `backend/` + `proto/`.

**Tech Stack:** Rust 1.98.1 (edition 2021), rustler 0.38.0, derive_more 2.1.1, strum 0.28, rand 0.10.3, itertools 0.15, serde_json; Elixir 1.19.5-otp-28 / Erlang 28, Phoenix 1.8, LiveView 1.1, Hex `rustler ~> 0.38.0`; GitHub Actions with `jdx/mise-action@v4`.

**Spec:** `docs/superpowers/specs/2026-09-23-phase-0-foundation-design.md` (cross-cutting decisions: `docs/superpowers/specs/2026-09-23-migration-overview-design.md`; downstream consumers: `2026-09-23-phase-1-auth-design.md`, `2026-09-23-phase-3-engine-design.md`).

## Global Constraints

- Toolchain: `mise.toml` pins `erlang = "28"`, `elixir = "1.19.5-otp-28"` (unchanged) and adds `rust = { version = "1.98.1", components = "clippy,rustfmt" }` (1.98.1 was current stable on 2026-09-23).
- Crate versions (latest stable on crates.io on 2026-09-23; use exactly these): `rustler = "0.38.0"`, `derive_more = { version = "2.1.1", features = ["display"] }`, `strum = "0.28.0"`, `strum_macros = "0.28.0"`, `rand = "0.10.3"`, `itertools = "0.15.0"`, `lazy_static = "1.5.0"`, `maplit = "1.0.2"`, `serde = { version = "1.0.229", features = ["derive"] }`, `serde_json = "1.0.151"`. `protobuf` is removed. Hex: `{:rustler, "~> 0.38.0", runtime: false}`.
- Crate identity: package and lib name `seven_wonders_core`, `edition = "2021"`, `crate-type = ["cdylib", "rlib"]` (in that order; see Task 5 for why the order matters). The internal module formerly named `core` is `engine` (`core/src/engine/`). NIF module name: `"Elixir.Helios.Core.Native"`.
- **No rule changes** in this phase. Legacy engine code is only edited mechanically (renames, dependency API changes, lint fixes). If a legacy test fails for a reason other than an API rename, STOP and report. Do not change game logic to make it pass.
- Gates: `cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test` passes after Task 4 and again after Task 5. `cd helios && mix precommit` passes after Task 2 and again after Task 5.
- Starting state: the user committed `helios/`, `mise.toml`, the `Login.elm` edit and a `*.DS_Store` ignore rule in `b283ab3` ("Save version before migration") and pushed it. Execute on branch `game_ui` (or a worktree branched from it); the working tree starts clean.
- Commands: all paths are relative to the repo root. Every tool runs through mise: `(cd core && mise exec -- cargo test)`, `(cd helios && mise exec -- mix test)`. `sed` is BSD sed on macOS (`sed -i ''`).
- Git hygiene: stage with explicit paths only. Never use `git add -A`, `git add .`, `git add -u` or `git commit -a`.
- Never `rm -rf` any untracked leftover without explicit human confirmation in the conversation (Task 7). Untracked `backend/.env` and `backend/config/prod.secret.exs` may contain secrets.
- No secrets in committed source. Never write the old Tidewave token value anywhere, including commit messages and this plan's follow-ups. The old token is already in the public history (`b283ab3`); Task 1 removes it from HEAD and the human revokes it. Do not rewrite git history unless the human asks.
- CI actions use current major versions: `actions/checkout@v7`, `actions/cache@v6`, `jdx/mise-action@v4`.
- Every commit message ends with the line `Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>`.

## Review Focus

1. **Accented wonder names crossing the NIF** (`"Rhódos"`, `"Éphesos"`, `"Olympía"`, `"Halikarnassós"`, all NFC in `data.rs`). A caller that passes back the names `game_settings/0` returned, or types them as UTF-8 literals, should start a game. The ASCII `"Rhodos"` should still be rejected as an unknown wonder. Pinned in Task 3 (`test_start_game_accepts_accented_wonder_names`) and Task 5 (`accepts accented wonder names exactly as game_settings/0 returns them`).
2. **Malformed Elixir input to the NIF** (atom player names, a non-boolean side, a reference that is not a game). This should raise `ArgumentError` in the calling process and never crash the BEAM or panic. Pinned in Task 5 (`raises ArgumentError ...` tests).
3. **Boundary player counts.** 3 and 7 players should be accepted, and a 7-player random game should use each of the 7 wonders exactly once. 2 and 8 players should give `{:error, {:invalid_players_number, n}}`. Pinned in Task 3 (`test_start_game_with_seven_random_wonders_uses_each_wonder_once`, `test_start_game_with_invalid_players_number`) and Task 5.
4. **Login form with blank or whitespace-only fields, and live validation.** Both fields should show their error message and the form should never crash on `phx-change`. Pinned in Task 2 (`shows an error per blank field...`, `validate keeps the typed values...`).
5. **`TIDEWAVE_TOKEN` set or unset after the endpoint was compiled.** The dev server should honour the current environment without a forced recompile, and an unset token should mean plain `plug Tidewave`. Handled by reading the env var at request time in Task 1, and smoke-tested there with both values.

Known limitations deliberately **not** changed here (they are rule/validation changes owned by Phase 3):
- Duplicate player names silently collapse, because `player_states` is keyed by name.
- Duplicate explicit wonders are accepted.
- `debug_game` uses `try_lock`, so concurrent calls can return `{:error, :lock_fail}`.

---

## File Structure

| Path | Action | Responsibility |
|---|---|---|
| `.gitignore` | Modify (append) | Ignore `.DS_Store`, `erl_crash.dump` repo-wide |
| `mise.toml` | Modify, then commit (untracked today) | Pin Rust alongside Erlang/Elixir |
| `helios/**` | Commit (untracked today) | Phoenix app |
| `helios/lib/helios_web/endpoint.ex` | Modify L30-32 | Tidewave token from `TIDEWAVE_TOKEN` at request time |
| `helios/lib/helios_web/live/login_live.ex` | Rewrite | Namespaced `:login` form, no custom Enter hook |
| `helios/test/helios_web/live/login_live_test.exs` | Rewrite | LoginLive behaviour tests |
| `core/Cargo.toml` | Rewrite | Crate identity + modern deps |
| `core/Cargo.lock` | Regenerated | Lockfile |
| `core/Makefile.toml`, `core/.cargo/config` | Delete | protobuf codegen task; x86-only macOS link args that rustler ≥ 0.36 no longer needs |
| `core/src/core/` → `core/src/engine/` | `git mv` | Engine data/deck/init/trading (renamed to avoid the built-in `core` crate name) |
| `core/src/engine/deck.rs`, `core/src/engine/game_init.rs` | Modify | rand 0.10 API, `std::iter::zip`, `crate::engine` paths |
| `core/src/domain/{structure,wonder}.rs`, `core/src/domain/supply/resources/types.rs` | Modify | derive_more 2 `#[display("...")]` syntax |
| `core/src/api/{ping,game_settings,start_game}.rs` | Delete | protobuf-generated code |
| `core/src/api/mod.rs` | Rewrite | Plain-Rust API: `GameSettings`, `WonderSideChoice`, `ErrorType`, `game_settings()`, `start_game()` |
| `core/src/tests/api.rs` | Rewrite | Tests for the plain-Rust API |
| `core/src/tests/{deck,trading,game_effects}.rs` | Modify L1-2 | `crate::engine` paths |
| `core/src/lib.rs` | Rewrite (Task 3), extend (Task 5) | Module tree, then the rustler NIF layer |
| `core/README.md` | Rewrite | Accurate crate docs |
| `helios/mix.exs` | Modify deps | Add `:rustler` |
| `helios/lib/helios/core/native.ex` | Create | `Helios.Core.Native` NIF stubs |
| `helios/lib/helios/core.ex` | Create | `Helios.Core` public wrapper with specs/docs |
| `helios/test/helios/core_test.exs` | Create | ExUnit tests against the real NIF |
| `helios/.gitignore` | Modify (append) | Ignore `/priv/native/` |
| `.github/workflows/ci.yml` | Rewrite | `core` + `helios` jobs; keep `old_backend`, `frontend`, `integration-tests` verbatim; drop `backend` |
| `backend/`, `proto/` | `git rm -r` | Deprecated |
| `README.md` | Modify | Drop "Backend new" and protobuf sections |

---

### Task 1: Repo hygiene, toolchain bootstrap, remove the hardcoded Tidewave token

**Files:**
- Modify: `.gitignore` (append at end; the file currently has no trailing newline)
- Modify: `helios/lib/helios_web/endpoint.ex:30-32`
- Modify: `mise.toml`
- Commit: `helios/lib/helios_web/endpoint.ex`, `mise.toml`, `.gitignore`

**Interfaces:**
- Consumes: nothing.
- Produces:
  - A working toolchain for all later tasks: `mise exec -- cargo|rustc|mix` resolve to Rust 1.98.1 (with clippy + rustfmt) and Elixir 1.19.5-otp-28 with Hex/Rebar.
  - The dev endpoint plugs Tidewave through the private function plug `tidewave/2` (no literal token in source).

- [ ] **Step 1: Preflight — confirm location and working-tree state**

Run:
```bash
cd /Users/arnoldszederjesi/Projects/seven_wonders && git rev-parse --show-toplevel && git branch --show-current && git status --short
```
Expected:
- The top level is `/Users/arnoldszederjesi/Projects/seven_wonders` and the branch is `game_ui`.
- Status is empty, or lists only untracked `erl_crash.dump` files (`.DS_Store` is ignored since `b283ab3`).
- `git ls-files mise.toml helios/mix.exs` prints both paths (already tracked).

If the branch is neither `game_ui` nor a branch created from it, or tracked files are modified, STOP and ask the human.

- [ ] **Step 2: Ignore macOS and BEAM crash artifacts repo-wide**

`.gitignore` ends with `# End of https://www.gitignore.io/api/go,git,elm,node,intellij,visualstudiocode` and **no trailing newline**, so start the append with a newline:
```bash
printf '\n\n# Local OS / BEAM artifacts\n.DS_Store\nerl_crash.dump\n' >> .gitignore
tail -5 .gitignore
```
Expected: the last three lines are `# Local OS / BEAM artifacts`, `.DS_Store`, `erl_crash.dump`, and the `# End of ...` line is intact above them.

- [ ] **Step 3: Replace the hardcoded Tidewave token (read at request time)**

In `helios/lib/helios_web/endpoint.ex`, lines 30-32 are the block that starts with `if Mix.env() == :dev do`, contains `plug Tidewave, team: [id: "octoscreen", token: "<literal>"]` and ends with `end`. Do not copy the literal anywhere. Replace the whole three-line block with:

```elixir
  if Mix.env() == :dev do
    # Tidewave team config is read at request time so changing TIDEWAVE_TOKEN
    # never requires recompiling the endpoint. When unset this is plain
    # `plug Tidewave`. It must stay before the code_reloading? block.
    plug :tidewave

    defp tidewave(conn, _opts) do
      opts =
        case System.get_env("TIDEWAVE_TOKEN") do
          nil -> []
          "" -> []
          token -> [team: [id: "octoscreen", token: token]]
        end

      Tidewave.call(conn, Tidewave.init(opts))
    end
  end
```
(`Tidewave.init/1` in the locked tidewave 0.5.6 is a pure keyword-to-map conversion, so calling it per request is cheap and side-effect free.)

- [ ] **Step 4: Verify no literal token remains**

Run:
```bash
grep -rnE 'token: "[A-Za-z0-9]{16,}"' helios/lib helios/config ; echo "exit=$?"
grep -rn "token:" helios/lib
```
Expected:
- The first command prints only `exit=1` (no matches).
- The second prints only the `token -> [team: [id: "octoscreen", token: token]]` line.

- [ ] **Step 5: Pin Rust in `mise.toml`**

Replace the whole content of `mise.toml` with:
```toml
[tools]
elixir = "1.19.5-otp-28"
erlang = "28"
rust = { version = "1.98.1", components = "clippy,rustfmt" }
```

- [ ] **Step 6: Install the toolchain**

Run:
```bash
mise install
mise exec -- rustc --version
mise exec -- cargo clippy --version
mise exec -- cargo fmt --version
```
Expected:
- `mise install` installs rustup (into `~/.rustup` / `~/.cargo`) and the `1.98.1` toolchain; Erlang/Elixir are already installed.
- `rustc 1.98.1 (48a229cea 2026-09-01)`.
- A `clippy 0.1.98 ...` line.
- A `rustfmt 1.x ...` line.

If mise reports the config is untrusted, run `mise trust` once and retry.

- [ ] **Step 7: Install Hex and Rebar for the mise Elixir**

Run:
```bash
(cd helios && mise exec -- mix local.hex --force && mise exec -- mix local.rebar --force)
```
Expected: `* creating .../archives/hex-2.x.y` and `* creating .../elixir/1-19-otp-28/rebar3` (paths may differ). No errors.

- [ ] **Step 8: Fetch deps and compile Helios (dev)**

Run:
```bash
(cd helios && mise exec -- mix deps.get && mise exec -- mix compile)
```
Expected: `mix compile` finishes with `Generated helios app` and no `Could not find an SCM for dependency :tidewave` error.

- [ ] **Step 9: Smoke-test the endpoint with the token unset and set**

Run (dev env, no HTTP server; this calls the endpoint plug pipeline directly):
```bash
(cd helios && env -u TIDEWAVE_TOKEN mise exec -- mix run -e 'conn = HeliosWeb.Endpoint.call(Plug.Test.conn(:get, "/"), HeliosWeb.Endpoint.init([])); IO.puts("status=#{conn.status}")')
(cd helios && TIDEWAVE_TOKEN=dummy-not-a-secret mise exec -- mix run -e 'conn = HeliosWeb.Endpoint.call(Plug.Test.conn(:get, "/"), HeliosWeb.Endpoint.init([])); IO.puts("status=#{conn.status}")')
```
Expected: both print `status=200` with no Tidewave exception. If it fails only because of the dev DB, run `(cd helios && mise exec -- mix ecto.create)` once and retry.

If it fails inside `Phoenix.CodeReloader`/`Phoenix.LiveReloader` (not Tidewave) because `mix run` does not host the reloader, use the real server instead:
1. Start `(cd helios && env -u TIDEWAVE_TOKEN mise exec -- mix phx.server)` in the background.
2. Poll `curl -s -o /dev/null -w '%{http_code}\n' http://localhost:4000/` until it prints `200`.
3. Stop the server.
4. Repeat with `TIDEWAVE_TOKEN=dummy-not-a-secret`.

- [ ] **Step 10: Run the Helios test suite to record the baseline**

Run:
```bash
(cd helios && mise exec -- mix test)
```
Expected: `6 tests, 1 failure`. The failure is `test handles login attempt (HeliosWeb.LoginLiveTest)`, which the spec says is broken; Task 2 fixes it.

- [ ] **Step 11: Stage explicitly and inspect before committing**

Run:
```bash
git add .gitignore mise.toml helios/lib/helios_web/endpoint.ex
git diff --cached --name-only | grep -v '^helios/'
git diff --cached --name-only | grep -E '\.db|erl_crash|DS_Store|priv/static/assets|/deps/|/_build/' ; echo "forbidden-matches-exit=$?"
```
Expected:
- The first `grep` prints exactly `.gitignore`, `mise.toml`.
- The second prints only `forbidden-matches-exit=1`.

The only staged `helios/` path is `helios/lib/helios_web/endpoint.ex`.

- [ ] **Step 12: Commit**

```bash
git commit -m "$(cat <<'EOF'
chore: drop hardcoded Tidewave token, pin Rust in mise, ignore crash dumps

- Read the Tidewave team token from TIDEWAVE_TOKEN at request time
  instead of a hardcoded literal; plain `plug Tidewave` when unset.
- Pin rust 1.98.1 (clippy, rustfmt) in mise.toml.
- Ignore .DS_Store and erl_crash.dump repo-wide.

Known failing test: LoginLiveTest "handles login attempt" (fixed next).

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>
EOF
)"
```
Expected: the commit succeeds and `git status --short` is empty (apart from untracked crash dumps).

---

### Task 2: Fix the LoginLive placeholder

**Files:**
- Modify: `helios/lib/helios_web/live/login_live.ex` (full rewrite below)
- Test: `helios/test/helios_web/live/login_live_test.exs` (full rewrite below)

**Interfaces:**
- Consumes: Helios baseline from Task 1.
- Produces:
  - `HeliosWeb.LoginLive` at `live "/"`. It renders `<.form id="login_form" phx-change="validate" phx-submit="login">` for `to_form(params, as: :login)`, so inputs are named `login[access_token]` / `login[name]` with ids `login_access_token` / `login_name`.
  - Errors are rendered inside `#login_errors`.
  - Events: `"validate"` with `%{"login" => params}` and `"login"` with `%{"login" => %{"name" => _, "access_token" => _}}`.
  - No `phx-hook`. Behaviour stays placeholder (flash only). Phase 1 replaces it.

- [ ] **Step 1: Write the failing tests**

Replace `helios/test/helios_web/live/login_live_test.exs` with:

```elixir
defmodule HeliosWeb.LoginLiveTest do
  use HeliosWeb.ConnCase
  import Phoenix.LiveViewTest

  test "renders login page", %{conn: conn} do
    {:ok, _view, html} = live(conn, ~p"/")

    assert html =~ "7 WONDERS"
    assert html =~ "Access Token"
    assert html =~ "Name"
  end

  test "namespaces fields under login and attaches no custom Enter hook", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    assert has_element?(view, "#login_form input[name='login[access_token]']")
    assert has_element?(view, "#login_form input[name='login[name]']")
    refute has_element?(view, "#login_form[phx-hook]")
  end

  test "handles login attempt", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    result =
      view
      |> form("#login_form", login: %{access_token: "test_token", name: "test_user"})
      |> render_submit()

    assert result =~ "Login successful for test_user!"
  end

  test "shows an error per blank field, treating whitespace as blank", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> form("#login_form", login: %{access_token: "   ", name: ""})
    |> render_submit()

    assert has_element?(view, "#login_errors", "Name can't be empty!")
    assert has_element?(view, "#login_errors", "Access token can't be empty!")
    refute render(view) =~ "Login successful"
  end

  test "validate keeps the typed values in the form", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> form("#login_form", login: %{access_token: "", name: "arnold"})
    |> render_change()

    assert has_element?(view, "#login_name[value='arnold']")
  end
end
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `(cd helios && mise exec -- mix test test/helios_web/live/login_live_test.exs)`

Expected: `5 tests, 4 failures`. `renders login page` passes. The other four fail:
- the `login[...]` input selectors do not match;
- `form/3` raises because fields `login[access_token]` / `login[name]` do not exist in the form;
- `render_change` requires a `phx-change` on the form.

- [ ] **Step 3: Write the implementation**

Replace `helios/lib/helios_web/live/login_live.ex` with:

```elixir
defmodule HeliosWeb.LoginLive do
  use HeliosWeb, :live_view

  @empty_login %{"name" => "", "access_token" => ""}

  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       form: to_form(@empty_login, as: :login),
       errors: [],
       trigger_submit: false
     )}
  end

  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash}>
      <div
        class="min-h-full flex flex-col items-center justify-center py-12 px-6 lg:px-8 w-full h-screen"
        style="background-image: url('/images/7_wonders.jpg'); background-size: cover; background-position: center;"
      >
        <h1 class="text-5xl font-bold text-center text-yellow-400 p-5">7 WONDERS</h1>
        <div class="mt-8 sm:mx-auto sm:w-full sm:max-w-md">
          <div class="bg-white py-8 px-4 shadow sm:rounded-lg sm:px-10">
            <.form
              for={@form}
              id="login_form"
              phx-change="validate"
              phx-submit="login"
              class="space-y-6"
            >
              <div class="md-form md-2">
                <.input
                  field={@form[:access_token]}
                  type="text"
                  label="Access Token"
                  autocomplete="off"
                />
              </div>
              <div class="md-form mt-2">
                <.input
                  field={@form[:name]}
                  type="text"
                  label="Name"
                  autocomplete="username"
                />
              </div>
              <div class="flex justify-center mt-2">
                <.button class="btn btn-outline-dark">
                  Submit
                </.button>
              </div>
            </.form>
            <div id="login_errors" class="mt-2 text-red-600 text-center">
              <div :for={error <- @errors}>{error}</div>
            </div>
          </div>
        </div>
      </div>
    </Layouts.app>
    """
  end

  def handle_event("validate", %{"login" => login_params}, socket) do
    {:noreply, assign(socket, form: to_form(login_params, as: :login))}
  end

  def handle_event(
        "login",
        %{"login" => %{"name" => name, "access_token" => access_token} = login_params},
        socket
      ) do
    # Placeholder: Phase 1 replaces this with Accounts.check_login/2 + session.
    errors = []
    errors = if String.trim(name) == "", do: ["Name can't be empty!" | errors], else: errors

    errors =
      if String.trim(access_token) == "",
        do: ["Access token can't be empty!" | errors],
        else: errors

    if Enum.empty?(errors) do
      {:noreply,
       socket
       |> put_flash(:info, "Login successful for #{name}!")
       |> assign(form: to_form(@empty_login, as: :login))
       |> assign(errors: [])}
    else
      {:noreply, assign(socket, form: to_form(login_params, as: :login), errors: errors)}
    end
  end
end
```

Notes:
- The colocated `.LoginEnter` hook and `phx-hook="LoginEnter"` are removed. A form with a submit button submits on Enter natively.
- `phx-change` moved from the two inputs to the form so `"validate"` always receives the full `%{"login" => ...}` map.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `(cd helios && mise exec -- mix test test/helios_web/live/login_live_test.exs)`
Expected: `5 tests, 0 failures`.

- [ ] **Step 5: Run the project gate**

Run: `(cd helios && mise exec -- mix precommit)`
Expected:
- The compile step (`--warnings-as-errors`) passes with no warnings.
- `format` succeeds.
- `9 tests, 0 failures`.

Then run `git status --short helios`. Expected: only the two files above are modified. If `mix format` rewrote either of them, that is fine: they are the files being committed.

- [ ] **Step 6: Commit**

```bash
git add helios/lib/helios_web/live/login_live.ex helios/test/helios_web/live/login_live_test.exs
git commit -m "$(cat <<'EOF'
fix(helios): namespace LoginLive form params and drop broken Enter hook

to_form/1 without :as produced flat params that never matched the
"login_form" handlers, and phx-hook="LoginEnter" did not match the
colocated ".LoginEnter" hook. Use to_form(params, as: :login), move
phx-change to the form, and rely on native Enter-to-submit.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>
EOF
)"
```
Expected: the commit succeeds.

---

### Task 3: Modernize the `core` crate (manifest, `engine` rename, dependency APIs, plain-Rust API)

**Files:**
- Rewrite: `core/Cargo.toml`
- Regenerate: `core/Cargo.lock`
- Delete: `core/Makefile.toml`, `core/.cargo/config`, `core/src/api/ping.rs`, `core/src/api/game_settings.rs`, `core/src/api/start_game.rs`
- Rename: `core/src/core/` → `core/src/engine/`
- Modify:
  - `core/src/engine/deck.rs` (full content below)
  - `core/src/engine/game_init.rs` (full content below)
  - `core/src/tests/deck.rs:1`, `core/src/tests/trading.rs:2`, `core/src/tests/game_effects.rs:2`
  - `core/src/domain/structure.rs:8`, `core/src/domain/wonder.rs:8`, `core/src/domain/supply/resources/types.rs:27`
- Rewrite: `core/src/lib.rs`, `core/src/api/mod.rs`
- Test: `core/src/tests/api.rs` (full rewrite)

**Interfaces:**
- Consumes: Rust 1.98.1 from Task 1.
- Produces (Rust, crate `seven_wonders_core`; Task 5 and Phase 3 rely on these exact names):
  ```rust
  // core/src/lib.rs
  pub mod api; mod common; pub mod domain; pub mod engine;

  // core/src/api/mod.rs
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct GameSettings { pub version: String, pub wonders: Vec<String> }
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct WonderSideChoice { pub wonder_name: String, pub side_b: bool }
  #[derive(Debug, PartialEq, Eq)]
  pub enum ErrorType {
      InvalidPlayersNumber(usize),
      InvalidPlayersAndWonderSideLength(String), // e.g. "3 != 1"
      InvalidWonder(String),                     // the unknown name
  }
  pub type SafeGameState = std::sync::Mutex<crate::domain::GameState>;
  pub fn game_settings() -> GameSettings;       // version = CARGO_PKG_VERSION, wonders = WONDER_NAMES order
  pub fn start_game(players: Vec<String>, wonder_sides: Vec<WonderSideChoice>)
      -> Result<SafeGameState, ErrorType>;      // same validation as today
  ```
  Engine paths are `crate::engine::{data, deck, game_init, trading}`.

The modules become `pub` (previously private). The crate is now also an `rlib` whose engine code is mostly unused by the NIF layer. With private modules, every unused `pub fn` would be a `dead_code` warning and `-D warnings` would fail. Exporting them is also the shape Phase 3 needs.

- [ ] **Step 1: Write the failing tests (new plain-Rust API)**

Replace `core/src/tests/api.rs` with:

```rust
use crate::api::{self, ErrorType, WonderSideChoice};
use crate::domain::{
    BattleTokens, Cards, Events, GameState, MilitarySymbolCount, Player, PlayerState,
    ResourceType::*, WonderStagesBuilt,
};
use crate::engine::data::WONDER_NAMES;

fn players(names: &[&str]) -> Vec<String> {
    names.iter().map(|name| name.to_string()).collect()
}

fn choice(wonder_name: &str, side_b: bool) -> WonderSideChoice {
    WonderSideChoice {
        wonder_name: wonder_name.to_string(),
        side_b,
    }
}

#[test]
fn test_game_settings() {
    let game_settings = api::game_settings();
    assert_eq!(game_settings.version, env!("CARGO_PKG_VERSION"));
    assert_eq!(game_settings.wonders, *WONDER_NAMES);
}

#[test]
fn test_start_game_with_random_wonders() {
    let safe_game_state = api::start_game(players(&["a", "b", "c"]), vec![]).unwrap();
    let game_state = safe_game_state.lock().unwrap();
    test_game_state(&game_state);

    for name in ["a", "b", "c"] {
        let player_state = game_state.player_states.get(name).unwrap();
        assert_eq!(player_state.player, Player(name.to_string()));
        test_player_state(player_state);
    }
}

#[test]
fn test_start_game_with_seven_random_wonders_uses_each_wonder_once() {
    let safe_game_state =
        api::start_game(players(&["a", "b", "c", "d", "e", "f", "g"]), vec![]).unwrap();
    let game_state = safe_game_state.lock().unwrap();
    let mut wonders: Vec<String> = game_state
        .player_states
        .values()
        .map(|player_state| {
            player_state
                .wonder
                .name()
                .split(" - ")
                .next()
                .unwrap()
                .to_string()
        })
        .collect();
    wonders.sort();
    let mut expected = WONDER_NAMES.clone();
    expected.sort();
    assert_eq!(wonders, expected);
}

#[test]
fn test_start_game_with_specific_wonders() {
    let wonder_sides = vec![
        choice("Gizah", false),
        choice("Alexandria", false),
        choice("Babylon", true),
    ];
    let safe_game_state = api::start_game(players(&["a", "b", "c"]), wonder_sides).unwrap();
    let game_state = safe_game_state.lock().unwrap();
    test_game_state(&game_state);

    {
        let player_state_a = game_state.player_states.get("a").unwrap();
        assert_eq!(player_state_a.player, Player("a".to_string()));
        test_player_state(player_state_a);
        assert_eq!(player_state_a.wonder.name(), "Gizah - A");
        assert_eq!(
            *player_state_a
                .resources_produced
                .single_resources
                .get(&Stone)
                .unwrap(),
            1
        );
    }
    {
        let player_state_b = game_state.player_states.get("b").unwrap();
        assert_eq!(player_state_b.player, Player("b".to_string()));
        test_player_state(player_state_b);
        assert_eq!(player_state_b.wonder.name(), "Alexandria - A");
        assert_eq!(
            *player_state_b
                .resources_produced
                .single_resources
                .get(&Glass)
                .unwrap(),
            1
        );
    }
    {
        let player_state_c = game_state.player_states.get("c").unwrap();
        assert_eq!(player_state_c.player, Player("c".to_string()));
        test_player_state(player_state_c);
        assert_eq!(player_state_c.wonder.name(), "Babylon - B");
        assert_eq!(
            *player_state_c
                .resources_produced
                .single_resources
                .get(&Clay)
                .unwrap(),
            1
        );
    }
}

#[test]
fn test_start_game_accepts_accented_wonder_names() {
    let wonder_sides = vec![
        choice("Rhódos", true),
        choice("Éphesos", false),
        choice("Halikarnassós", true),
    ];
    let safe_game_state = api::start_game(players(&["a", "b", "c"]), wonder_sides).unwrap();
    let game_state = safe_game_state.lock().unwrap();
    assert_eq!(
        game_state.player_states.get("a").unwrap().wonder.name(),
        "Rhódos - B"
    );
    assert_eq!(
        game_state.player_states.get("b").unwrap().wonder.name(),
        "Éphesos - A"
    );
    assert_eq!(
        game_state.player_states.get("c").unwrap().wonder.name(),
        "Halikarnassós - B"
    );
}

#[test]
fn test_start_game_with_invalid_players_number() {
    assert_eq!(
        api::start_game(players(&["a", "b"]), vec![]).unwrap_err(),
        ErrorType::InvalidPlayersNumber(2)
    );
    assert_eq!(
        api::start_game(players(&["a", "b", "c", "d", "e", "f", "g", "h"]), vec![]).unwrap_err(),
        ErrorType::InvalidPlayersNumber(8)
    );
}

#[test]
fn test_start_game_with_invalid_length() {
    let error_type =
        api::start_game(players(&["a", "b", "c"]), vec![choice("Gizah", false)]).unwrap_err();
    assert_eq!(
        error_type,
        ErrorType::InvalidPlayersAndWonderSideLength("3 != 1".to_string())
    );
}

#[test]
fn test_start_game_with_invalid_wonder() {
    let wonder_sides = vec![
        choice("Gizah", false),
        choice("Alexandria", false),
        choice("Rhodos", false),
    ];
    let error_type = api::start_game(players(&["a", "b", "c"]), wonder_sides).unwrap_err();
    assert_eq!(error_type, ErrorType::InvalidWonder("Rhodos".to_string()));
}

fn test_game_state(game_state: &GameState) {
    assert_eq!(game_state.cards_discarded, Cards::default());
    assert_eq!(game_state.current_age, Default::default());
    assert_eq!(game_state.current_age_cards, Default::default());
    assert_eq!(game_state.events, Events::default());
}

fn test_player_state(player_state: &PlayerState) {
    assert_eq!(
        player_state.wonder_stages_built,
        WonderStagesBuilt::default()
    );
    assert_eq!(player_state.coins, 3);
    assert_eq!(
        player_state.military_symbols,
        MilitarySymbolCount::default()
    );
    assert_eq!(player_state.battle_tokens, BattleTokens::default());
    assert_eq!(player_state.scientific_symbols_produced, Default::default());
    assert_ne!(player_state.resources_produced, Default::default());
    assert_eq!(player_state.structure_builder, Default::default());
    assert_eq!(player_state.point_actions.len(), 0);
    assert_eq!(player_state.trade_actions.len(), 1);
    assert!(!player_state.can_play_last_card);
    assert!(!player_state.can_copy_guild);
}
```

This keeps the 5 legacy api tests (same assertions, new API) and adds 3 new ones (seven random wonders, accented names, invalid player count). The crate total goes from 37 to 40 tests.

- [ ] **Step 2: Rewrite the manifest and delete protobuf/codegen leftovers**

Replace `core/Cargo.toml` with:
```toml
[package]
name = "seven_wonders_core"
version = "0.1.0"
authors = []
edition = "2021"

[lib]
name = "seven_wonders_core"
path = "src/lib.rs"
# cdylib MUST stay first: Rustler's mix compiler copies the artifact's first filename.
crate-type = ["cdylib", "rlib"]

[dependencies]
lazy_static = "1.5.0"
derive_more = { version = "2.1.1", features = ["display"] }
strum = "0.28.0"
strum_macros = "0.28.0"
rand = "0.10.3"
itertools = "0.15.0"
maplit = "1.0.2"
serde = { version = "1.0.229", features = ["derive"] }
serde_json = "1.0.151"
```
(`rustler` is added in Task 5 together with the NIF layer that uses it.)

Then run:
```bash
git rm -q core/Makefile.toml core/.cargo/config core/src/api/ping.rs core/src/api/game_settings.rs core/src/api/start_game.rs
```
`core/Makefile.toml` only contained the `gen_proto` task. `core/.cargo/config` only held `x86_64-apple-darwin` `-undefined dynamic_lookup` link args. rustler ≥ 0.36 loads the NIF API dynamically and no longer needs them, and newer cargo warns that the `.cargo/config` filename is deprecated.

- [ ] **Step 3: Rename `mod core` → `mod engine` and update every `crate::core::` path**

```bash
git mv core/src/core core/src/engine
sed -i '' 's/crate::core::/crate::engine::/g' core/src/tests/deck.rs core/src/tests/trading.rs core/src/tests/game_effects.rs
```
The remaining `crate::core::` users are `core/src/engine/deck.rs`, `core/src/engine/game_init.rs` (rewritten in Step 5) and `core/src/api/mod.rs` (rewritten in Step 8).

- [ ] **Step 4: Rewrite `core/src/lib.rs` as a plain module tree (NIF layer comes in Task 5)**

Replace `core/src/lib.rs` with:
```rust
//! Seven Wonders game engine (`seven_wonders_core`), loaded into Helios as a Rustler NIF.

pub mod api;
mod common;
pub mod domain;
pub mod engine;

#[cfg(test)]
mod tests {
    pub mod api;
    pub mod deck;
    pub mod game_effects;
    pub mod helpers;
    pub mod points;
    pub mod resources;
    pub mod trading;
}
```
(`common/mod.rs` is an empty file and is kept as-is.)

- [ ] **Step 5: Port `engine/deck.rs` and `engine/game_init.rs` to rand 0.10 / std `zip`**

Replace `core/src/engine/deck.rs` with (only the imports and the `thread_rng()` → `rand::rng()` calls change):
```rust
use crate::domain::{Card, Cards, Deck, Effect, Structure};
use crate::engine::data::{
    AGE_III_STRUCTURES, AGE_II_STRUCTURES, AGE_I_STRUCTURES, GUILD_STRUCTURES,
};

use rand::seq::SliceRandom;

pub fn generate_deck(players_count: usize) -> Deck {
    let mut age1 = generate_age_deck(players_count, AGE_I_STRUCTURES.iter());
    age1.shuffle(&mut rand::rng());
    let mut age2 = generate_age_deck(players_count, AGE_II_STRUCTURES.iter());
    age2.shuffle(&mut rand::rng());
    let mut age3 = generate_age_deck(players_count, AGE_III_STRUCTURES.iter());
    age3.append(&mut generate_guild_cards(players_count));
    age3.shuffle(&mut rand::rng());
    (age1, age2, age3)
}

fn generate_age_deck(
    players_count: usize,
    structures: impl Iterator<Item = &'static Structure<'static, Effect>>,
) -> Cards {
    let mut cards = vec![];
    for structure in structures {
        for _ in structure
            .thresholds()
            .iter()
            .filter(|threshold| **threshold <= players_count as u8)
        {
            cards.push(Card(structure));
        }
    }
    cards
}

fn generate_guild_cards(players_count: usize) -> Cards {
    let mut cards: Cards = GUILD_STRUCTURES.iter().map(Card).collect();
    cards.shuffle(&mut rand::rng());
    cards.truncate(players_count + 2);
    cards
}
```

Replace `core/src/engine/game_init.rs` with:
```rust
use crate::domain::{Effect, GameState, Player, PlayersWithWonders, Wonder};
use crate::engine::data::WONDERS;
use crate::engine::deck::generate_deck;
use rand::seq::SliceRandom;
use std::iter::zip;

pub fn init_with_random_wonders(players: Vec<Player>) -> GameState {
    let players_count = players.len();
    let mut wonders: Vec<&'static Wonder<'static, Effect>> = WONDERS.iter().collect();
    wonders.shuffle(&mut rand::rng());
    wonders.truncate(players_count);
    let mut players_with_wonders: PlayersWithWonders = vec![];
    for (player, wonder) in zip(players, wonders) {
        if rand::random::<bool>() {
            players_with_wonders.push((player, &wonder.1));
        } else {
            players_with_wonders.push((player, &wonder.2));
        }
    }
    init(players_with_wonders)
}

pub fn init(players_with_wonders: PlayersWithWonders) -> GameState {
    let players_count = players_with_wonders.len();
    let deck = generate_deck(players_count);
    let mut game_state = GameState::new(deck, players_with_wonders);
    game_state.init();
    game_state
}
```
rand 0.10 facts: `thread_rng()` is gone and replaced by `rand::rng()` (feature `thread_rng`, on by default). `rand::random` still exists. `SliceRandom::shuffle<R: Rng + ?Sized>` still lives in `rand::seq`. `itertools::zip` is deprecated since 0.10.4; `std::iter::zip` replaces it.

- [ ] **Step 6: Port derive_more attributes to 2.x syntax**

derive_more ≥ 1 replaced `#[display(fmt = "...", args)]` with `#[display("...")]`. It also **does not infer trait bounds for trailing positional args**, so interpolate the fields inline. Make exactly these three edits:

- `core/src/domain/structure.rs:8`: `#[display(fmt = "Structure({})", _0)]` → `#[display("Structure({_0})")]`
- `core/src/domain/wonder.rs:8`: `#[display(fmt = "Wonder({}, {}, {})", _0, _1, _2)]` → `#[display("Wonder({_0}, {_1}, {_2})")]`
- `core/src/domain/supply/resources/types.rs:27`: `#[display(fmt = "ResourceCost({}, {})", _0, _1)]` → `#[display("ResourceCost({_0}, {_1})")]`

The bare `#[derive(Display)]` uses on `Player`, `Category`, `Age`, `ScientificSymbol` and `ResourceType` need no change: newtypes forward, and unit variants print their name.

If `Wonder` fails to compile with "`WonderSide<'a, T>` doesn't implement `Display`", remove `Display` from its derive list, delete its `#[display(...)]` line, and remove the now-unused `use derive_more::Display;` in `wonder.rs`. `WonderSide` never implements `Display`, so `Wonder`'s `Display` impl could never be used. If anything did format a `Wonder`, the build fails loudly, so this cannot hide a behaviour change.

- [ ] **Step 7: Run the tests to verify they fail for the expected reason**

Run: `(cd core && mise exec -- cargo test 2>&1 | grep -E "^error" | sort | uniq -c)`

Expected: compilation FAILS only in `src/api/mod.rs` / `src/tests/api.rs`. Typical errors are `unresolved import protobuf`, `unresolved import crate::core`, `cannot find type WonderSideChoice in module crate::api`, and `no field version on type ...`.

If errors appear in any **other** file, they are dependency-upgrade fallout. Fix them mechanically before continuing:
- `thread_rng` → `rand::rng()`
- `display(fmt = ...)` → Step 6
- `itertools::zip` → `std::iter::zip`
- `crate::core::` → `crate::engine::`

- [ ] **Step 8: Write the plain-Rust API**

Replace `core/src/api/mod.rs` with:
```rust
//! Plain-Rust API consumed by the NIF layer in `lib.rs`. No rustler types here.

use crate::domain::{GameState, Player, PlayersWithWonders};
use crate::engine::data::{WONDERS_BY_NAME, WONDER_NAMES};
use crate::engine::game_init;
use std::sync::Mutex;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GameSettings {
    pub version: String,
    pub wonders: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WonderSideChoice {
    pub wonder_name: String,
    pub side_b: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorType {
    InvalidPlayersNumber(usize),
    InvalidPlayersAndWonderSideLength(String),
    InvalidWonder(String),
}

pub type SafeGameState = Mutex<GameState>;

pub fn game_settings() -> GameSettings {
    GameSettings {
        version: VERSION.to_string(),
        wonders: WONDER_NAMES.to_vec(),
    }
}

/// Starts a game for 3..=7 players. An empty `wonder_sides` assigns random
/// wonders and sides; otherwise it must have one entry per player, in seat order.
pub fn start_game(
    players: Vec<String>,
    wonder_sides: Vec<WonderSideChoice>,
) -> Result<SafeGameState, ErrorType> {
    if !(3..=7).contains(&players.len()) {
        return Err(ErrorType::InvalidPlayersNumber(players.len()));
    }
    let game_state = if wonder_sides.is_empty() {
        game_init::init_with_random_wonders(players.into_iter().map(Player).collect())
    } else if players.len() == wonder_sides.len() {
        let mut players_with_wonders: PlayersWithWonders = Vec::with_capacity(players.len());
        for (player_name, choice) in players.into_iter().zip(wonder_sides) {
            let wonder = WONDERS_BY_NAME
                .get(&choice.wonder_name)
                .ok_or_else(|| ErrorType::InvalidWonder(choice.wonder_name.clone()))?;
            let wonder_side = if choice.side_b { &wonder.2 } else { &wonder.1 };
            players_with_wonders.push((Player(player_name), wonder_side));
        }
        game_init::init(players_with_wonders)
    } else {
        return Err(ErrorType::InvalidPlayersAndWonderSideLength(format!(
            "{} != {}",
            players.len(),
            wonder_sides.len()
        )));
    };
    Ok(Mutex::new(game_state))
}
```
This has the same validation and order as the protobuf version:
1. Player count.
2. Empty sides → random.
3. Equal length → explicit, where an unknown name fails.
4. Otherwise → length mismatch.

`ping` is removed.

- [ ] **Step 9: Run the tests to verify they pass**

Run:
```bash
(cd core && mise exec -- cargo test 2>&1 | tail -5)
grep -rn "crate::core\|protobuf\|RepeatedField" core/src ; echo "exit=$?"
```
Expected:
- `test result: ok. 40 passed; 0 failed`. Warnings are allowed at this step; Task 4 removes them.
- The grep prints only `exit=1`.

If a legacy (non-`api`) test fails, STOP and report the name and output. Do not change engine logic.

- [ ] **Step 10: Commit**

```bash
git add core/Cargo.toml core/Cargo.lock core/src
git diff --cached --name-only | grep -v '^core/' ; echo "non-core-staged-exit=$?"
git commit -m "$(cat <<'EOF'
refactor(core): modernize crate as seven_wonders_core with plain-Rust API

- edition 2021, package/lib seven_wonders_core, crate-type cdylib+rlib
- drop protobuf (generated api/*.rs, Makefile.toml gen_proto task)
- bump derive_more 2.1, strum 0.28, rand 0.10, itertools 0.15
- rename internal mod core -> engine (avoids the built-in core crate)
- api: GameSettings/WonderSideChoice/ErrorType + game_settings/start_game
- remove obsolete x86-only .cargo/config link args
No rule changes; 37 legacy tests + 3 new api tests pass.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>
EOF
)"
```
Expected: `non-core-staged-exit=1` (only `core/` paths are staged), and the commit succeeds.

---

### Task 4: Make `core` fmt- and clippy-clean

**Files:**
- Modify: any `core/src/**/*.rs` flagged by rustfmt/clippy. The known ones are listed in Step 4.

**Interfaces:**
- Consumes: the crate from Task 3 (40 passing tests).
- Produces: a crate where `cargo fmt --check` and `cargo clippy --all-targets -- -D warnings` pass. Public names and behaviour are unchanged, so Task 3's Interfaces still hold.

- [ ] **Step 1: Run the gate to see it fail**

Run:
```bash
(cd core && mise exec -- cargo fmt --check ; echo "fmt-exit=$?")
(cd core && mise exec -- cargo clippy --all-targets -- -D warnings 2>&1 | grep -E "^(error|warning)" | sort | uniq -c | sort -rn)
```
Expected: `fmt-exit` is likely 1, and clippy reports a list of lints (errors under `-D warnings`). Keep this list; it is the checklist for Step 4.

- [ ] **Step 2: Apply formatting**

Run: `(cd core && mise exec -- cargo fmt && mise exec -- cargo fmt --check ; echo "fmt-exit=$?")`
Expected: `fmt-exit=0`.

- [ ] **Step 3: Apply machine-applicable fixes**

Run (`--allow-dirty` lets `cargo fix` run with this task's uncommitted edits; it only rewrites files of this crate):
```bash
(cd core && mise exec -- cargo clippy --fix --allow-dirty --allow-staged --all-targets && mise exec -- cargo fmt)
(cd core && mise exec -- cargo clippy --all-targets -- -D warnings 2>&1 | grep -E "^(error|warning)" | sort | uniq -c)
git status --short -- . ':!core' ':!backend'
```
Expected: the remaining list is shorter, and the last command prints nothing (nothing outside `core/` changed; `backend/` is excluded because untracked leftovers such as `.env` live there until Task 7).

This typically auto-fixes:
- `needless_borrow` / `needless_borrows_for_generic_args` (e.g. `&player_name` in `domain/player.rs:44`, `&effect_directions` / `&categories` in `domain/game_state.rs`)
- `needless_lifetimes` (`domain/player.rs:37` `<'a>`)
- `useless_conversion` (`domain/player.rs:88,92,96` `.into_iter()` inside `HashSet::from_iter`)
- `redundant_closure`
- rustc's `mismatched_lifetime_syntaxes` (e.g. `domain/structure.rs` `fn name(&self) -> SName` → `SName<'_>`, `fn dependencies(&self) -> Dependencies` → `Dependencies<'_>`)

- [ ] **Step 4: Fix the remaining findings by hand**

These findings were identified by reading the code. The toolchain was not available when the plan was written, so treat the Step 1/3 output as authoritative and apply whichever of these it still reports:

1. `clippy::derivable_impls` at `core/src/domain/structure.rs:95-99` (`impl Default for Age`). Replace the manual impl with a derive:
   ```rust
   #[derive(Display, Debug, Clone, Copy, PartialEq, Eq, Hash, Default, serde::Serialize)]
   pub enum Age {
       #[default]
       None,
       I,
       II,
       III,
   }
   ```
   Then delete the `impl Default for Age { ... }` block.
2. `clippy::new_without_default` at `core/src/domain/point.rs:25` (`PointsMap::new`). Add below the `impl PointsMap` block:
   ```rust
   impl Default for PointsMap {
       fn default() -> Self {
           Self::new()
       }
   }
   ```
3. `clippy::ptr_arg` at `core/src/domain/supply/scientific.rs:103` and `:131`. In both `apply_any_symbols` and `combination_of_symbols_and_points`, change `any_symbols: &Vec<ScientificSymbols<'static>>` to `any_symbols: &[ScientificSymbols<'static>]`. Callers passing `&self.any_symbols` coerce automatically.
4. `clippy::type_complexity` on a field or signature, if reported: introduce a `type` alias next to the item, as `domain/game_state.rs` already does with `Actions<T>`.
5. Anything else: apply the fix clippy suggests in its `help:` line, keeping behaviour identical.

Use `#[allow(clippy::<lint>)]` only on the smallest item, and only for code generated by a macro or a fix that would change game rules. Put a comment on the line above it starting with `// Justification:`. Never use a crate-level `allow`.

- [ ] **Step 5: Run the full gate**

Run:
```bash
(cd core && mise exec -- cargo fmt --check && mise exec -- cargo clippy --all-targets -- -D warnings && mise exec -- cargo test 2>&1 | tail -3)
```
Expected: no clippy output beyond `Finished`, and `test result: ok. 40 passed; 0 failed`.

- [ ] **Step 6: Commit**

```bash
git add core/src
git diff --cached --name-only | grep -v '^core/src/' ; echo "non-core-staged-exit=$?"
git commit -m "$(cat <<'EOF'
style(core): rustfmt and clippy -D warnings clean on Rust 1.98

Mechanical lint fixes only (derivable Default for Age, Default for
PointsMap, slice params, elided lifetimes, needless borrows); no rule
changes.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>
EOF
)"
```
Expected: `non-core-staged-exit=1`, and the commit succeeds.

---

### Task 5: Rustler NIF layer and the `Helios.Core` bridge

**Files:**
- Modify: `core/Cargo.toml` (add `rustler`), `core/Cargo.lock`
- Rewrite: `core/src/lib.rs` (NIF layer inserted between the module declarations and the trailing `#[cfg(test)] mod tests`)
- Rewrite: `core/README.md`
- Modify: `helios/mix.exs` (deps), `helios/mix.lock`, `helios/.gitignore`
- Create: `helios/lib/helios/core/native.ex`, `helios/lib/helios/core.ex`
- Test: `helios/test/helios/core_test.exs`

**Interfaces:**
- Consumes (from Task 3): `seven_wonders_core::api::{game_settings, start_game, GameSettings, WonderSideChoice, ErrorType}` and `seven_wonders_core::domain::GameState`.
- Produces:
  - Rust (`core/src/lib.rs`):
    - `pub struct GameResource(Mutex<GameState>)` with `#[rustler::resource_impl] impl rustler::Resource`.
    - NIFs `game_settings/0`, `start_game/2`, `debug_game/1` registered by `rustler::init!("Elixir.Helios.Core.Native")`.
  - Elixir `Helios.Core.Native` (raw NIFs):
    - `game_settings() :: %{version: String.t(), wonders: [String.t()]}`
    - `start_game([String.t()], [%{wonder_name: String.t(), side_b: boolean()}]) :: {:ok, reference()} | {:error, {atom(), non_neg_integer() | String.t()}}`
    - `debug_game(reference()) :: {:ok, String.t()} | {:error, :lock_fail}`
    - Malformed arguments raise `ArgumentError`.
  - Elixir `Helios.Core` (public API for Phases 2–4):
    - `game_settings/0`
    - `start_game(players :: [String.t()], wonder_sides :: [{String.t(), boolean()}]) :: {:ok, game} | {:error, {atom(), term()}}`
    - `debug_game(game) :: {:ok, map()} | {:error, atom()}`
    - `@type game :: reference()`
  - Error reasons: `:invalid_players_number` (detail = player count), `:invalid_players_and_wonder_side_length` (detail = `"<players> != <sides>"`), `:invalid_wonder` (detail = the unknown name).

- [ ] **Step 1: Write the failing ExUnit tests**

Create `helios/test/helios/core_test.exs`:
```elixir
defmodule Helios.CoreTest do
  use ExUnit.Case, async: true

  alias Helios.Core

  @wonders ["Rhódos", "Alexandria", "Éphesos", "Babylon", "Olympía", "Halikarnassós", "Gizah"]

  describe "game_settings/0" do
    test "returns the crate version and the seven base-game wonders in data order" do
      settings = Core.game_settings()

      assert settings.version =~ ~r/^\d+\.\d+\.\d+/
      assert settings.wonders == @wonders
    end
  end

  describe "start_game/2" do
    test "starts a 3-player game with random wonders" do
      assert {:ok, game} = Core.start_game(["a", "b", "c"], [])
      assert is_reference(game)
    end

    test "starts a 7-player game with random wonders, using each wonder exactly once" do
      assert {:ok, game} = Core.start_game(~w(a b c d e f g), [])
      assert {:ok, %{"player_states" => player_states}} = Core.debug_game(game)

      wonders =
        for {_name, player_state} <- player_states do
          player_state["wonder"] |> hd() |> String.split(" - ") |> hd()
        end

      assert Enum.sort(wonders) == Enum.sort(@wonders)
    end

    test "starts a game with explicit wonder sides in seat order" do
      sides = [{"Gizah", false}, {"Alexandria", false}, {"Babylon", true}]

      assert {:ok, game} = Core.start_game(["a", "b", "c"], sides)
      assert {:ok, %{"player_states" => player_states}} = Core.debug_game(game)
      assert player_states["a"]["wonder"] == ["Gizah - A"]
      assert player_states["b"]["wonder"] == ["Alexandria - A"]
      assert player_states["c"]["wonder"] == ["Babylon - B"]
    end

    test "accepts accented wonder names exactly as game_settings/0 returns them" do
      %{wonders: wonders} = Core.game_settings()
      sides = Enum.map(wonders, &{&1, true})

      assert {:ok, game} = Core.start_game(~w(a b c d e f g), sides)
      assert {:ok, %{"player_states" => player_states}} = Core.debug_game(game)
      assert player_states["a"]["wonder"] == ["Rhódos - B"]
      assert player_states["f"]["wonder"] == ["Halikarnassós - B"]
    end

    test "rejects fewer than 3 players" do
      assert Core.start_game(["a", "b"], []) == {:error, {:invalid_players_number, 2}}
    end

    test "rejects more than 7 players" do
      assert Core.start_game(~w(a b c d e f g h), []) == {:error, {:invalid_players_number, 8}}
    end

    test "rejects a wonder list whose length differs from the player list" do
      assert Core.start_game(["a", "b", "c"], [{"Gizah", false}]) ==
               {:error, {:invalid_players_and_wonder_side_length, "3 != 1"}}
    end

    test "rejects an unknown wonder (the ASCII spelling of an accented name included)" do
      sides = [{"Gizah", false}, {"Alexandria", false}, {"Rhodos", false}]

      assert Core.start_game(["a", "b", "c"], sides) == {:error, {:invalid_wonder, "Rhodos"}}
    end

    test "raises ArgumentError instead of crashing the VM on malformed input" do
      assert_raise ArgumentError, fn -> Core.start_game([:a, :b, :c], []) end

      assert_raise ArgumentError, fn ->
        Core.start_game(["a", "b", "c"], [{"Gizah", :b}, {"Alexandria", false}, {"Babylon", true}])
      end
    end
  end

  describe "debug_game/1" do
    test "returns the decoded state with one player_states entry per player" do
      {:ok, game} = Core.start_game(["a", "b", "c"], [])

      assert {:ok, %{"player_states" => player_states}} = Core.debug_game(game)
      assert player_states |> Map.keys() |> Enum.sort() == ["a", "b", "c"]

      for {name, player_state} <- player_states do
        assert player_state["name"] == name
        assert player_state["coins"] == 3
      end
    end

    test "raises ArgumentError for a reference that is not a game" do
      assert_raise ArgumentError, fn -> Core.debug_game(make_ref()) end
    end
  end
end
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `(cd helios && mise exec -- mix test test/helios/core_test.exs)`
Expected: `12 tests, 12 failures`, each with `** (UndefinedFunctionError) function Helios.Core.game_settings/0 is undefined (module Helios.Core is not available)` or the equivalent for `start_game/2` / `debug_game/1`.

- [ ] **Step 3: Add rustler to the crate and write the NIF layer**

In `core/Cargo.toml` under `[dependencies]`, add as the first line:
```toml
rustler = "0.38.0"
```

Replace `core/src/lib.rs` with the content below. The NIF layer goes **between** the module declarations and the `#[cfg(test)] mod tests` block, which must stay last. Clippy's default `items_after_test_module` lint rejects items declared after the test module.
```rust
//! Seven Wonders game engine (`seven_wonders_core`), loaded into Helios as a Rustler NIF.

pub mod api;
mod common;
pub mod domain;
pub mod engine;

// ---------------------------------------------------------------------------
// NIF layer: native-term DTOs + thin wrappers over `api`. No logic lives here.
// ---------------------------------------------------------------------------

use rustler::{Atom, NifMap, NifUntaggedEnum, ResourceArc};
use std::sync::Mutex;

mod atoms {
    rustler::atoms! {
        invalid_players_number,
        invalid_players_and_wonder_side_length,
        invalid_wonder,
        lock_fail,
    }
}

pub struct GameResource(Mutex<domain::GameState>);

#[rustler::resource_impl]
impl rustler::Resource for GameResource {}

#[derive(NifMap)]
struct GameSettings {
    version: String,
    wonders: Vec<String>,
}

impl From<api::GameSettings> for GameSettings {
    fn from(settings: api::GameSettings) -> Self {
        Self {
            version: settings.version,
            wonders: settings.wonders,
        }
    }
}

#[derive(NifMap)]
struct WonderSideChoice {
    wonder_name: String,
    side_b: bool,
}

impl From<WonderSideChoice> for api::WonderSideChoice {
    fn from(choice: WonderSideChoice) -> Self {
        Self {
            wonder_name: choice.wonder_name,
            side_b: choice.side_b,
        }
    }
}

/// Second element of `{:error, {reason, detail}}`: an integer or a string.
#[derive(NifUntaggedEnum)]
enum ErrorDetail {
    Count(usize),
    Text(String),
}

fn nif_error(error_type: api::ErrorType) -> (Atom, ErrorDetail) {
    match error_type {
        api::ErrorType::InvalidPlayersNumber(count) => {
            (atoms::invalid_players_number(), ErrorDetail::Count(count))
        }
        api::ErrorType::InvalidPlayersAndWonderSideLength(message) => (
            atoms::invalid_players_and_wonder_side_length(),
            ErrorDetail::Text(message),
        ),
        api::ErrorType::InvalidWonder(wonder_name) => {
            (atoms::invalid_wonder(), ErrorDetail::Text(wonder_name))
        }
    }
}

#[rustler::nif]
fn game_settings() -> GameSettings {
    api::game_settings().into()
}

#[rustler::nif]
fn start_game(
    players: Vec<String>,
    wonder_sides: Vec<WonderSideChoice>,
) -> Result<ResourceArc<GameResource>, (Atom, ErrorDetail)> {
    let wonder_sides = wonder_sides.into_iter().map(Into::into).collect();
    api::start_game(players, wonder_sides)
        .map(|game_state| ResourceArc::new(GameResource(game_state)))
        .map_err(nif_error)
}

#[rustler::nif]
fn debug_game(game: ResourceArc<GameResource>) -> Result<String, Atom> {
    let game_state = game.0.try_lock().map_err(|_| atoms::lock_fail())?;
    Ok(serde_json::to_string(&*game_state).expect("GameState serializes to JSON"))
}

rustler::init!("Elixir.Helios.Core.Native");

#[cfg(test)]
mod tests {
    pub mod api;
    pub mod deck;
    pub mod game_effects;
    pub mod helpers;
    pub mod points;
    pub mod resources;
    pub mod trading;
}
```

Deliberate differences from the spec's sketch:
- The `atoms!` list omits `ok`/`error`. Rustler encodes `Result` as `{:ok, _}` / `{:error, _}` itself, and unused atom functions would be `dead_code` errors under `-D warnings`.
- The error detail is a `NifUntaggedEnum` instead of a raw `Term`, so no `Env` lifetime is needed. The Elixir-side encoding is identical: `{:error, {reason_atom, integer_or_string}}`.

- [ ] **Step 4: Verify the crate still passes its gate**

Run:
```bash
(cd core && mise exec -- cargo fmt --check && mise exec -- cargo clippy --all-targets -- -D warnings && mise exec -- cargo test 2>&1 | tail -3)
```
Expected: clean clippy, and `test result: ok. 40 passed; 0 failed`. The test binary links without a BEAM because rustler ≥ 0.30 resolves `enif_*` at load time.

If clippy flags code expanded from `#[rustler::nif]`/`rustler::atoms!`, add the narrowest item-level `#[allow(...)]` with a `// Justification: rustler macro expansion` comment.

- [ ] **Step 5: Add Rustler to Helios**

In `helios/mix.exs` `deps/0`, add after `{:jason, "~> 1.2"},`:
```elixir
      {:rustler, "~> 0.38.0", runtime: false},
```

Append to `helios/.gitignore`:
```
# Rustler-built NIF libraries
/priv/native/
```

Create `helios/lib/helios/core/native.ex`:
```elixir
defmodule Helios.Core.Native do
  @moduledoc false
  # Raw NIF bindings to the `seven_wonders_core` crate in ../core.
  # Use `Helios.Core` instead of calling these directly.
  use Rustler, otp_app: :helios, crate: "seven_wonders_core", path: "../core"

  def game_settings, do: :erlang.nif_error(:nif_not_loaded)
  def start_game(_players, _wonder_sides), do: :erlang.nif_error(:nif_not_loaded)
  def debug_game(_game), do: :erlang.nif_error(:nif_not_loaded)
end
```

Create `helios/lib/helios/core.ex`:
```elixir
defmodule Helios.Core do
  @moduledoc """
  Elixir entry point to the Rust 7 Wonders engine (`core/`, crate `seven_wonders_core`).

  Game handles are opaque NIF resources (they satisfy `is_reference/1`). Game rules
  live in Rust; this module only converts arguments and results.
  Malformed arguments (for example non-string player names) raise `ArgumentError`.
  """

  alias Helios.Core.Native

  @typedoc "Opaque handle to a running engine game."
  @type game :: reference()

  @typedoc "A wonder name as returned by `game_settings/0` and whether side B is used."
  @type wonder_side :: {String.t(), boolean()}

  @type start_error ::
          {:invalid_players_number, non_neg_integer()}
          | {:invalid_players_and_wonder_side_length, String.t()}
          | {:invalid_wonder, String.t()}

  @doc "Engine version and the names of the supported wonders."
  @spec game_settings() :: %{version: String.t(), wonders: [String.t()]}
  def game_settings, do: Native.game_settings()

  @doc """
  Starts a game for 3 to 7 players, in seat order.

  Pass `[]` as `wonder_sides` for random wonders and sides. Otherwise pass one
  `{wonder_name, side_b?}` per player, in the same order.
  """
  @spec start_game([String.t()], [wonder_side()]) :: {:ok, game()} | {:error, start_error()}
  def start_game(players, wonder_sides) do
    wonder_sides =
      Enum.map(wonder_sides, fn {wonder_name, side_b} ->
        %{wonder_name: wonder_name, side_b: side_b}
      end)

    Native.start_game(players, wonder_sides)
  end

  @doc "Full internal engine state as a decoded JSON map. Debug and test use only."
  @spec debug_game(game()) :: {:ok, map()} | {:error, atom()}
  def debug_game(game) do
    with {:ok, json} <- Native.debug_game(game) do
      {:ok, Jason.decode!(json)}
    end
  end
end
```

- [ ] **Step 6: Fetch, compile, and check the built NIF artifact**

Run:
```bash
(cd helios && mise exec -- mix deps.get && MIX_ENV=test mise exec -- mix compile --warnings-as-errors)
file helios/priv/native/seven_wonders_core.so
```
Expected:
- The compile output includes a Rustler line similar to `Compiling crate seven_wonders_core in release mode (../core)` and `Copying .../core/target/release/libseven_wonders_core.dylib to priv/native/seven_wonders_core.so`.
- `file` reports `Mach-O 64-bit dynamically linked shared library arm64` (Linux CI: `ELF 64-bit LSB shared object`).

If `file` reports `current ar archive` (the `.rlib` was copied because cargo listed it first), change `crate-type` in `core/Cargo.toml` to `["cdylib"]`. `cargo test` only runs in-crate unit tests, which do not need the rlib. Then re-run this step and `cargo test`.

- [ ] **Step 7: Run the tests to verify they pass**

Run: `(cd helios && mise exec -- mix test test/helios/core_test.exs)`
Expected: `12 tests, 0 failures`.

- [ ] **Step 8: Rewrite `core/README.md`**

Replace `core/README.md` with:
````markdown
# seven_wonders_core

Rust 7 Wonders engine. Helios loads it as the Rustler NIF `Helios.Core.Native`
(`helios/lib/helios/core/native.ex`); use the `Helios.Core` wrapper from Elixir.

## Development

The toolchain is pinned in the repo-root `mise.toml`. From this folder:

```shell
mise exec -- cargo fmt --check
mise exec -- cargo clippy --all-targets -- -D warnings
mise exec -- cargo test
```

Helios compiles this crate automatically on `mix compile` (`path: "../core"`).
````

- [ ] **Step 9: Run both project gates**

Run:
```bash
(cd core && mise exec -- cargo fmt --check && mise exec -- cargo clippy --all-targets -- -D warnings && mise exec -- cargo test 2>&1 | tail -3)
(cd helios && mise exec -- mix precommit)
```
Expected:
- Rust: `40 passed`.
- Helios: no compile warnings, and `21 tests, 0 failures` (2 error_json + 2 error_html + 5 login + 12 core).

- [ ] **Step 10: Commit**

```bash
git add core/Cargo.toml core/Cargo.lock core/src/lib.rs core/README.md helios/mix.exs helios/mix.lock helios/.gitignore helios/lib/helios/core.ex helios/lib/helios/core/native.ex helios/test/helios/core_test.exs
git diff --cached --name-only | grep -E 'priv/native|Login.elm' ; echo "forbidden-exit=$?"
git commit -m "$(cat <<'EOF'
feat: load seven_wonders_core into Helios via a native-terms Rustler NIF

- core: rustler 0.38 NIF layer (GameResource, NifMap DTOs,
  game_settings/start_game/debug_game) registered as
  Elixir.Helios.Core.Native; errors are {:error, {reason, detail}}
- helios: Helios.Core.Native bindings + Helios.Core wrapper with specs,
  ExUnit tests against the real NIF, /priv/native/ ignored

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>
EOF
)"
```
Expected: `forbidden-exit=1`, and the commit succeeds.

---

### Task 6: Rewrite CI around mise

**Files:**
- Rewrite: `.github/workflows/ci.yml`

**Interfaces:**
- Consumes: the gates from Tasks 4–5 (`cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test`; Helios compile/format/test) and `mise.toml` from Task 1.
- Produces:
  - CI jobs `core` and `helios`, which Phase 1 extends with `e2e` and Phase 5 with `docker`.
  - Legacy jobs `old_backend`, `frontend` and `integration-tests`, unchanged (Phase 2 deletes them).
  - The `backend` job is removed.

- [ ] **Step 1: Write the new workflow**

Replace `.github/workflows/ci.yml` with the content below. The three legacy jobs are copied verbatim from the current file; only a trailing newline is added at the end of the file.
```yaml
name: CI

on:
  push:
    branches: [ master ]
  pull_request:
    branches: [ master ]

env:
  CARGO_TERM_COLOR: always

jobs:
  core:
    name: Core
    runs-on: ubuntu-latest
    defaults:
      run:
        working-directory: core
    steps:
    - uses: actions/checkout@v7
    - name: Install Rust (mise.toml)
      uses: jdx/mise-action@v4
      with:
        install_args: rust
    - name: Cache cargo registry and target
      uses: actions/cache@v6
      with:
        path: |
          ~/.cargo/registry
          ~/.cargo/git
          core/target
        key: ${{ runner.os }}-core-${{ hashFiles('mise.toml', 'core/Cargo.lock') }}
        restore-keys: ${{ runner.os }}-core-
    - name: Check formatting
      run: cargo fmt --check
    - name: Clippy
      run: cargo clippy --all-targets -- -D warnings
    - name: Test
      run: cargo test
  helios:
    name: Helios
    runs-on: ubuntu-latest
    env:
      MIX_ENV: test
    defaults:
      run:
        working-directory: helios
    steps:
    - uses: actions/checkout@v7
    - name: Install Erlang, Elixir and Rust (mise.toml)
      uses: jdx/mise-action@v4
    - name: Cache deps, build and NIF target
      uses: actions/cache@v6
      with:
        path: |
          ~/.cargo/registry
          ~/.cargo/git
          helios/deps
          helios/_build
          core/target
        key: ${{ runner.os }}-helios-${{ hashFiles('mise.toml', 'helios/mix.lock', 'core/Cargo.lock') }}
        restore-keys: ${{ runner.os }}-helios-
    - name: Install Hex and Rebar
      run: mix local.hex --force && mix local.rebar --force
    - name: Install dependencies
      run: mix deps.get
    - name: Check for unused dependencies
      run: mix deps.unlock --check-unused
    - name: Compile (builds the NIF)
      run: mix compile --warnings-as-errors
    - name: Check formatting
      run: mix format --check-formatted
    - name: Test
      run: mix test
  old_backend:
    name: Old Backend
    runs-on: ubuntu-latest
    env:
      ACCESS_TOKEN: "TEST"
      JWT_SECRET: "test"
    defaults:
      run:
        working-directory: backend_old
    steps:
    - name: Install Go
      uses: actions/setup-go@v2
      with:
        go-version: 1.16.x
    - name: Checkout code
      uses: actions/checkout@v2
    - name: Test
      run: go test ./...
  frontend:
    name: Frontend
    runs-on: ubuntu-latest
    defaults:
      run:
        working-directory: frontend
    steps:
    - uses: jorelali/setup-elm@v3
      with:
        elm-version: 0.19.1
    - uses: actions/checkout@v2
    - run: npm install
    - run: sudo npm install -g elm-github-install create-elm-app@4.2.16 --unsafe-perm=true
    - run: elm-app build
    - run: elm-app test
  integration-tests:
    name: Integration Tests
    runs-on: ubuntu-latest
    env:
      ACCESS_TOKEN: "TEST"
      JWT_SECRET: "test"
      LOG_LEVEL: "error"
    defaults:
      run:
        working-directory: integration-tests
    needs: [old_backend]
    steps:
      - uses: actions/checkout@v2
      - run: cd ../backend_old && go build && cd ../integration-tests
      - run: npm install
      - run: ../backend_old/backend_old & npm test
```

Decisions:
- The cache covers `~/.cargo/registry` and `~/.cargo/git`, not all of `~/.cargo`. This avoids restoring stale rustup proxies over the ones mise just installed.
- The `core` job installs only `rust` via `install_args`, so it never builds Erlang.
- `mix deps.unlock --check-unused` is the CI (non-mutating) form of the `deps.unlock --unused` step in `mix precommit`.

- [ ] **Step 2: Validate the YAML, the job set, and that legacy jobs are unchanged**

Run (compares the parsed legacy jobs against the committed version):
```bash
ruby -e 'require "yaml"; old = YAML.load(`git show HEAD:.github/workflows/ci.yml`); new = YAML.load_file(".github/workflows/ci.yml"); puts new["jobs"].keys.sort.join(","); %w[old_backend frontend integration-tests].each { |j| puts "#{j}: #{old["jobs"][j] == new["jobs"][j] ? "unchanged" : "CHANGED"}" }; puts "top-level on/env unchanged: #{old["on"] == new["on"] && old["env"] == new["env"]}"'
```
Expected:
```
core,frontend,helios,integration-tests,old_backend
old_backend: unchanged
frontend: unchanged
integration-tests: unchanged
top-level on/env unchanged: true
```
(Ruby's YAML parses the `on:` key as the string `"on"`, so the comparison is valid.)

- [ ] **Step 3: Replay both CI jobs locally**

Run:
```bash
(cd core && mise exec -- cargo fmt --check && mise exec -- cargo clippy --all-targets -- -D warnings && mise exec -- cargo test 2>&1 | tail -1)
(cd helios && MIX_ENV=test mise exec -- mix deps.unlock --check-unused && MIX_ENV=test mise exec -- mix compile --warnings-as-errors && mise exec -- mix format --check-formatted && mise exec -- mix test 2>&1 | tail -3)
```
Expected: `test result: ok. 40 passed ...` and `21 tests, 0 failures`.

- [ ] **Step 4: Commit**

```bash
git add .github/workflows/ci.yml
git commit -m "$(cat <<'EOF'
ci: add mise-based core and helios jobs, drop backend job

core: fmt --check, clippy -D warnings, cargo test (Rust from mise.toml).
helios: hex/rebar, deps.get, compile --warnings-as-errors (builds the
NIF), format --check-formatted, mix test. Legacy old_backend, frontend
and integration-tests jobs unchanged until Phase 2.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 7: Delete `backend/` and `proto/`, update README, final acceptance

**Files:**
- Delete (tracked files only): `backend/**` (64 tracked files), `proto/game_settings.proto`, `proto/ping.proto`, `proto/start_game.proto`
- Modify: `README.md` (remove the "Backend new" section and the "Generate Protobuf modules" section)

**Interfaces:**
- Consumes: green gates from Tasks 5–6. Nothing in `core/`, `helios/` or CI references `backend/` or `proto/` any more.
- Produces: a repo without `backend/` and `proto/` in git. Phase 2 then deletes `backend_old/`, `websocket-client/` and `integration-tests/`.

- [ ] **Step 1: STOP — confirm with the human before touching `backend/`**

Do not run any command in this task until the human answers. Ask verbatim:

> "Before I remove `backend/`: it contains untracked files git cannot recover (e.g. `backend/.env`, `backend/config/prod.secret.exs`). Have you saved anything you want to keep? I will only run `git rm -r backend proto`, which removes tracked files only. I will not delete any untracked leftovers without your explicit go-ahead."

Continue only after an explicit "yes, proceed".

- [ ] **Step 2: Remove tracked files only**

Run:
```bash
git rm -r -q backend proto
git status --short | grep -vE '^D  (backend|proto)/' 
```
Expected: `git status` shows only the staged `D` deletions plus possibly `?? backend/...` leftovers.

- [ ] **Step 3: Inspect untracked leftovers — do not delete them**

Run:
```bash
ls -la backend 2>&1 ; find backend -maxdepth 3 -not -path 'backend/deps/*' -not -path 'backend/_build/*' 2>/dev/null | head -50
ls proto 2>&1
```
Expected:
- `proto`: `No such file or directory`.
- `backend/` probably still exists with ignored/untracked leftovers: `deps/`, `_build/` or `priv/`, `.env`, `config/prod.secret.exs`, `erl_crash.dump`, and `.DS_Store` files.

Show this listing to the human and ask:

> "These untracked/ignored files remain in `backend/` (listing above). May I delete them with `rm -rf backend`? Note that `.env` and `config/prod.secret.exs` may contain secrets you want to keep."

Run `rm -rf backend` **only** after an explicit yes. Otherwise leave `backend/` on disk. Git no longer tracks it, which satisfies the spec.

- [ ] **Step 4: Update README.md**

Using the Edit tool on `README.md`, delete this block (including the blank line after it):
````markdown
### Backend new

In backend folder:
```shell script
iex -S mix
```

Run tests:
```shell script
ACCESS_TOKEN="TEST" JWT_SECRET="test" go test -v -race ./...
```

````
Then delete everything from the line `## Generate Protobuf modules` to the end of the file. That covers the whole section: the intro sentence, `### Backend` with `mix gen_proto ping.proto`, and `### Core` with `cargo make gen_proto start_game.proto`. The file then ends with the `## Deployment` code block.

Run:
```bash
grep -n -i "protobuf\|gen_proto\|Backend new\|Core.Api" README.md ; echo "exit=$?"
```
Expected: `exit=1`.

- [ ] **Step 5: Acceptance sweep**

Run:
```bash
git ls-files backend proto | wc -l
git grep -n -i -E "Core\.Api|protobuf" -- core helios README.md .github ; echo "refs-exit=$?"
grep -rnE 'token: "[A-Za-z0-9]{16,}"' helios/lib ; echo "secrets-exit=$?"
grep -rn "token:" helios/lib
(cd core && mise exec -- cargo fmt --check && mise exec -- cargo clippy --all-targets -- -D warnings && mise exec -- cargo test 2>&1 | tail -1)
(cd helios && mise exec -- mix precommit 2>&1 | tail -3)
git status --short
```
Expected:
- `0` tracked files under `backend`/`proto`.
- `refs-exit=1`.
- `secrets-exit=1`.
- `token:` appears only on the `token -> [team: [id: "octoscreen", token: token]]` line.
- `40 passed` and `21 tests, 0 failures`.
- `git status` shows the staged `D` lines, ` M README.md`, and possibly `?? backend/` if the human kept the leftovers.

The references check is scoped to `core`, `helios`, `README.md` and `.github`. `backend_old/` and `websocket-client/` legitimately use protobuf until Phase 2 deletes them, and `docs/` specs mention it historically.

- [ ] **Step 6: Commit**

```bash
git add README.md
git diff --cached --name-only | grep -vE '^(backend|proto)/|^README\.md$' ; echo "unexpected-staged-exit=$?"
git commit -m "$(cat <<'EOF'
chore: remove deprecated Elixir backend and protobuf definitions

Helios + seven_wonders_core (native-term NIF) replace backend/ and the
proto/ contract. README drops the "Backend new" and protobuf codegen
sections.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>
EOF
)"
```
Expected: `unexpected-staged-exit=1` and the commit succeeds.

- [ ] **Step 7: STOP — ask the human how to get CI green**

CI only runs on pushes to `master` and on pull requests targeting `master`. Do not push on your own initiative. Ask:

> "Phase 0 is committed locally on `game_ui` (Tasks 1–7), and both CI jobs pass when replayed locally. Should I push `game_ui` and open a PR against `master` so the `core` and `helios` jobs run? I'll watch them with `gh run watch`."

Only with the human's approval, run:
```bash
git push -u origin game_ui
gh pr create --base master --head game_ui --title "Phase 0: foundation (Helios + seven_wonders_core NIF)" --body "$(cat <<'EOF'
Implements docs/superpowers/specs/2026-09-23-phase-0-foundation-design.md
(plan: docs/superpowers/plans/2026-09-23-phase-0-foundation.md).

- Helios committed; Tidewave token from TIDEWAVE_TOKEN
- core -> seven_wonders_core (edition 2021, rustler 0.38, no protobuf)
- Helios.Core / Helios.Core.Native bridge with ExUnit tests on the real NIF
- mise-based CI jobs core + helios
- backend/ and proto/ removed

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
gh run watch
```
Expected: the `Core` and `Helios` jobs are green. `Old Backend`, `Frontend` and `Integration Tests` behave as before this phase; they are pre-existing legacy jobs and Phase 2 deletes them.

---

## Self-Review

**Spec coverage:**

| Spec item | Where |
|---|---|
| 0.1 `.gitignore` | Task 1 Step 2 |
| 0.1 Tidewave token | Task 1 Steps 3–4, 9 |
| 0.1 mise rust | Task 1 Step 5 |
| 0.1 token removed from HEAD, Rust pinned (helios/mise.toml already committed in b283ab3) | Task 1 Steps 11–12 |
| 0.2 `mise install`, hex/rebar, `mix deps.get && mix compile` | Task 1 Steps 6–8 |
| 0.3 `to_form(..., as: :login)`, `%{"login" => ...}`, hook dropped, tests updated, `mix precommit` | Task 2 |
| 0.4 Cargo.toml (edition, name, crate-type, deps, no protobuf) | Task 3 Step 2 |
| 0.4 derive_more syntax | Task 3 Step 6 |
| 0.4 rand API | Task 3 Step 5 |
| 0.4 `mod engine` | Task 3 Step 3 |
| 0.4 delete generated api + Makefile.toml | Task 3 Step 2 |
| 0.4 plain-Rust `api` | Task 3 Step 8 |
| 0.4 tests updated, all pass | Task 3 Steps 1, 9 |
| 0.4 fmt/clippy | Task 4 |
| 0.4 NIF layer (atoms, `GameResource`, NifMap DTOs, three NIFs, `init!`, ping removed) | Task 5 Step 3 |
| 0.5 mix dep | Task 5 Step 5 |
| 0.5 `native.ex` (spec code verbatim) and `core.ex` (specs, docs, tuple→map, `Jason.decode!`) | Task 5 Step 5 |
| 0.5 `core_test.exs` (all listed cases) | Task 5 Step 1 |
| 0.5 `/priv/native/` | Task 5 Step 5 |
| 0.6 CI jobs, caches, action versions, legacy jobs kept, backend job removed | Task 6 |
| 0.7 delete backend/proto + README | Task 7 |
| Acceptance criteria | Task 7 Steps 5, 7 |

**Placeholder scan:** Every code step has full content. Task 4 Step 4 is necessarily driven by real clippy output, but it gives concrete fixes for every finding that could be predicted from the code, plus a precise rule for the rest.

**Type consistency:**
- `api::WonderSideChoice { wonder_name, side_b }` (Task 3) matches the NIF DTO (Task 5) and the Elixir map keys `%{wonder_name:, side_b:}` built by `Helios.Core.start_game/2`.
- `api::ErrorType` variants match `nif_error/1` and the ExUnit error assertions.
- `GameSettings { version, wonders }` matches `%{version:, wonders:}`.
- The NIF names `game_settings/0`, `start_game/2` and `debug_game/1` match the `Helios.Core.Native` stubs.

**Review Focus:** each of the five lines has a pinned test (or smoke check, for item 5) in the task that owns the code.
