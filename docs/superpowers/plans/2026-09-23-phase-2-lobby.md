# Phase 2 — Lobby Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Port the Go lobby model (one table per user, owner invites, invitee accepts/declines, global online presence with a grace period, per-lobby connected presence) to Helios LiveView + PubSub + Presence with invites persisted in SQLite, cover all 12 legacy scenarios with Playwright, then delete `backend_old/`, `websocket-client/`, `integration-tests/` and their CI jobs.

**Architecture:** A new `Helios.Lobbies` context (schemas `Lobby`, `Invite`) owns all rules and broadcasts on `"user:<id>"` / `"lobby:<id>"`. The web layer adds a pure `HeliosWeb.OnlineTracker` (debounced online/offline over the Phase 1 `"users:online"` presence topic), a `HeliosWeb.Notifications` on_mount hook (per-user invite/simple notifications rendered by `Layouts.app`) and `HeliosWeb.LobbyLive` at `/lobby/:game_id`. **This plan is written against the Phase 1 end state as described in `docs/superpowers/specs/2026-09-23-phase-1-auth-design.md`; Phase 1 files do not exist at plan-writing time.** Where a task edits a Phase 1 file whose exact contents are unknown, the step says precisely what to find and what to put there.

**Tech Stack:** Elixir 1.19 / Phoenix 1.8 / LiveView 1.1, Ecto + `ecto_sqlite3`, Phoenix.PubSub, Phoenix.Presence, Tailwind v4 (no daisyUI), ExUnit + `Phoenix.LiveViewTest` + `LazyHTML`, Playwright (`e2e/`, TypeScript).

**Spec:** `docs/superpowers/specs/2026-09-23-phase-2-lobby-design.md` (overview: `docs/superpowers/specs/2026-09-23-migration-overview-design.md`; starting state: `docs/superpowers/specs/2026-09-23-phase-1-auth-design.md`; downstream consumer: `docs/superpowers/specs/2026-09-23-phase-4-game-design.md`).

## Global Constraints

- **Phase 1 interfaces assumed (exact names):** `Helios.Accounts` (`login(access_token, name) :: {:ok, user, token} | {:error, reason, message}`), `Helios.Accounts.User` (integer `id`, `name`), `Helios.Accounts.Scope` (`%Scope{user: User.t() | nil}`, `Scope.for_user/1`), `HeliosWeb.UserAuth` (plug `fetch_current_scope` reading session `:user_token`; `on_mount` `:require_user` / `:redirect_if_user`), `HeliosWeb.Presence` tracking `"users:online"` keyed by user id with meta `%{name: name}` (tracked by `:require_user` on connected mount), `HeliosWeb.Layouts.app/1` with `flash` + `current_scope` attrs rendering `<.site_header current_scope={@current_scope} />`, `LobbyStubLive` at `/lobby`, `PageController.home` at `/`, `e2e/` Playwright package with `e2e/tests/support/auth.ts` exporting `uniqueName(prefix)`, `login(page, name, token = "e2e")`, `logout(page)`, `config/e2e.exs` (`MIX_ENV=e2e`, port 4004, `access_token: "e2e"`), test access token `"test"`.
- **Presence keys are strings when read back.** `Phoenix.Presence.list/1` and `presence_diff` payloads stringify keys (`to_string(key)` in `Phoenix.Presence.group/1`), so user id `5` comes back as `"5"`. Always convert with `HeliosWeb.OnlineTracker.to_user_id/1`.
- Capacity: **7** players per table (owner + invites ≥ 7 → `:lobby_full`).
- Presence grace: **5 s** default, configurable via `config :helios, :presence_grace_ms`; the e2e env sets **2000**.
- Simple notifications auto-expire after **8 s**; at most **5** shown, newest first; approve notifications are never evicted by simple ones.
- PubSub (server `Helios.PubSub`) — exact messages: `"user:<invitee_id>"`: `{:invited, invite}`, `{:uninvited, lobby_id}`, `{:invite_resolved, lobby_id}`; `"lobby:<id>"`: `{:members_changed}`, `{:declined, user}`. Presence topic `"lobby:<id>"` keyed by user id, meta `%{name: name}`.
- Error atoms → messages (single source: `Helios.Lobbies.error_message/1`): `:not_leader` → "Only the table leader can do that"; `:unauthorized` → "Only invited players can join this table"; `:invalid_game_id` → "That table does not exist"; `:self_invite` → "You can't invite yourself"; `:already_invited` → "That player is already invited"; `:lobby_full` → "The table is full (7 players max)"; `:invalid_user` → "That player does not exist"; `:not_invited` → "That invitation is no longer valid".
- User-facing copy (verbatim): "You are expected on table <owner>", "User <X> got online!", "User <X> got offline!", "User <X> declined your invitation!", "You were removed from <owner>'s table", select prompt "Select username", button "Invite", table headers "Username" / "Delete", padding rows "FREE", header link "My table", notification buttons "Accept" / "Decline" / "OK".
- Follow `helios/AGENTS.md`: every LiveView template starts with `<Layouts.app flash={@flash} current_scope={@current_scope} ...>`; forms via `to_form/2` + `<.form for=...>` + `<.input>`; icons via `<.icon>` (exception: the crown, which Heroicons does not ship, is a small inline SVG function component); Tailwind v4 utilities only — no daisyUI class names, no `@apply`, no inline `<script>`; no `String.to_atom/1` on input; no `Process.sleep/1` in ExUnit; unique DOM ids on key elements; tests use `has_element?/2,3` and element ids.
- **Streams are deliberately not used**: the members list is bounded (≤ 7) and recomputed as a whole (connected filter, FREE padding), and the notification list is bounded (≤ 5 visible); streams cannot count/filter, which both views need.
- Ecto code stays portable (no SQLite-only SQL). Migrations are created with `mix ecto.gen.migration <name>` (AGENTS.md).
- No new dependencies (Elixir or npm).
- Each task ends green on `cd helios && mix precommit` (Playwright tasks additionally on `cd e2e && npx playwright test`).
- Git: stage files **by explicit path only** — never `git add -A` / `git add .`. Every commit message ends with the line `Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>`.

## Review Focus

1. **Tampered LiveView events** (`invite` with `user_id` `"abc"`/`""`/own id/missing, `uninvite` with `"abc"`, `accept_invite`/`decline_invite` with a non-UUID or no `id`, `select_invitee` with no params) — expected: an error flash (or silent no-op for param-less notification events), never a crashed LiveView. Pinned in Task 4 (`invite`/`uninvite`/`select_invitee`) and Task 5 (notification events).
2. **Same user in two tabs, one tab closes** — expected: no "User X got offline!" because the user is still present when the grace check fires. Pinned in Task 3 (`confirm_offline` with the user still in the presence list).
3. **Stale grace timer** (a `{:confirm_offline, ...}` message that was already in the mailbox when a re-join cancelled it) — expected: ignored, the user stays online. Pinned in Task 3 (token mismatch test).
4. **Duplicate / overflowing notifications** (same invite delivered twice via pending-on-mount + broadcast, 6+ simple notifications, 5 invites + a simple one) — expected: one approve notification per table, at most 5 visible, approve never evicted. Pinned in Task 5 (`add/2` tests).
5. **Invite resolved in another tab** (accept in tab 1 while tab 2 shows the same Accept/Decline) — expected: the notification disappears in tab 2 as well. Pinned in Task 5.

---

## File Structure

**Create (helios):**
- `helios/priv/repo/migrations/<ts>_create_lobbies.exs` — `lobbies` table.
- `helios/priv/repo/migrations/<ts>_create_lobby_invites.exs` — `lobby_invites` table.
- `helios/lib/helios/lobbies/lobby.ex` — `Helios.Lobbies.Lobby` schema.
- `helios/lib/helios/lobbies/invite.ex` — `Helios.Lobbies.Invite` schema + changeset.
- `helios/lib/helios/lobbies.ex` — `Helios.Lobbies` context (rules, queries, broadcasts, error messages).
- `helios/lib/helios_web/online_tracker.ex` — `HeliosWeb.OnlineTracker` pure debounced online tracker.
- `helios/lib/helios_web/notifications.ex` — `HeliosWeb.Notifications` on_mount hook + pure notification list functions.
- `helios/lib/helios_web/live/lobby_live.ex` — `HeliosWeb.LobbyLive`.
- `helios/test/support/fixtures/lobbies_fixtures.ex` — `Helios.LobbiesFixtures`.
- `helios/test/helios/lobbies_test.exs`, `helios/test/helios_web/online_tracker_test.exs`, `helios/test/helios_web/live/lobby_live_test.exs`, `helios/test/helios_web/notifications_test.exs`.

**Modify (helios, Phase 1 files):**
- `helios/config/config.exs` (grace default), `helios/config/e2e.exs` (grace 2000).
- `helios/lib/helios_web/router.ex` (route + on_mount list), `helios/lib/helios_web/controllers/page_controller.ex` (`/` → own lobby), `helios/lib/helios_web/components/layouts.ex` ("My table" link, `notifications` attr + component).
- `helios/test/helios_web/controllers/page_controller_test.exs` (replaced).
- Any Phase 1 test that references `/lobby` (found by grep in Task 4).

**Delete (helios):** `helios/lib/helios_web/live/lobby_stub_live.ex` (+ its test if present).

**e2e:**
- Modify `e2e/tests/support/auth.ts` (add `LOBBY_URL`), `e2e/tests/auth.spec.ts` (`/lobby` → `/lobby/<uuid>`).
- Create `e2e/tests/support/lobby.ts`, `e2e/tests/presence.spec.ts`, `e2e/tests/lobby.spec.ts`.

**Repo root (final task):** `git rm -r backend_old websocket-client integration-tests`; modify `.github/workflows/ci.yml` (drop `old_backend`, `frontend`, `integration-tests` jobs).

---

### Task 1: Lobby data model and read-side context

**Files:**
- Create: `helios/priv/repo/migrations/<ts>_create_lobbies.exs`, `helios/priv/repo/migrations/<ts>_create_lobby_invites.exs`
- Create: `helios/lib/helios/lobbies/lobby.ex`, `helios/lib/helios/lobbies/invite.ex`, `helios/lib/helios/lobbies.ex`
- Create: `helios/test/support/fixtures/lobbies_fixtures.ex`
- Test: `helios/test/helios/lobbies_test.exs`

**Interfaces:**
- Consumes: `Helios.Accounts.login/2 :: {:ok, %User{}, token :: binary()}` (Phase 1), `Helios.Accounts.User`, `Helios.Repo`.
- Produces:
  - `Helios.Lobbies.Lobby` — `%Lobby{id: Ecto.UUID.t() (binary_id, the public game_id), owner_id: integer(), owner: User.t() | NotLoaded}`.
  - `Helios.Lobbies.Invite` — `%Invite{id, lobby_id, user_id, status: "pending" | "accepted", lobby, user}`; `Invite.create_changeset/1`.
  - `Lobbies.max_players() :: 7`
  - `Lobbies.lobby_topic(lobby_id) :: "lobby:<id>"`, `Lobbies.user_topic(user_id) :: "user:<id>"`
  - `Lobbies.get_or_create_own_lobby(%User{}) :: %Lobby{}`
  - `Lobbies.fetch_lobby(game_id :: term()) :: {:ok, %Lobby{}} | {:error, :invalid_game_id}`
  - `Lobbies.authorize(%Lobby{}, %User{}) :: :ok | {:error, :unauthorized}`
  - `Lobbies.members(%Lobby{}) :: [%{user: %User{}, leader?: boolean()}]` (owner first, then invitees by `inserted_at`, `id`)
  - `Lobbies.error_message(atom()) :: String.t()`
  - Fixtures (`Helios.LobbiesFixtures`): `unique_player_name/0`, `player_fixture/0,1 :: %User{}`, `player_with_token_fixture/0,1 :: {%User{}, token}`, `lobby_fixture(%User{}) :: %Lobby{}`.

- [ ] **Step 1: Write the fixtures module**

Create `helios/test/support/fixtures/lobbies_fixtures.ex` (it is compiled in the test env because `elixirc_paths(:test)` includes `test/support`). Users are created through Phase 1's public `Accounts.login/2` so the plan does not depend on any Phase 1 fixture module; the `player_` prefix avoids clashing with a Phase 1 `user_fixture`.

```elixir
defmodule Helios.LobbiesFixtures do
  @moduledoc """
  Test helpers for lobby tests. Players are created through
  `Helios.Accounts.login/2`, exactly like a real login.
  """

  alias Helios.Accounts
  alias Helios.Lobbies

  def unique_player_name, do: "p#{System.unique_integer([:positive])}"

  def player_with_token_fixture(name \\ unique_player_name()) do
    {:ok, user, token} = Accounts.login(Application.fetch_env!(:helios, :access_token), name)
    {user, token}
  end

  def player_fixture(name \\ unique_player_name()) do
    {user, _token} = player_with_token_fixture(name)
    user
  end

  def lobby_fixture(owner), do: Lobbies.get_or_create_own_lobby(owner)
end
```

- [ ] **Step 2: Write the failing read-side tests**

Create `helios/test/helios/lobbies_test.exs`:

```elixir
defmodule Helios.LobbiesTest do
  use Helios.DataCase, async: false

  import Helios.LobbiesFixtures

  alias Helios.Accounts.User
  alias Helios.Lobbies
  alias Helios.Lobbies.Invite
  alias Helios.Lobbies.Lobby

  describe "topics" do
    test "lobby and user topics" do
      assert Lobbies.lobby_topic("abc") == "lobby:abc"
      assert Lobbies.user_topic(5) == "user:5"
    end
  end

  describe "get_or_create_own_lobby/1" do
    test "creates the lobby once and returns the same lobby afterwards" do
      user = player_fixture()

      lobby = Lobbies.get_or_create_own_lobby(user)
      assert %Lobby{owner_id: owner_id} = lobby
      assert owner_id == user.id
      assert Lobbies.get_or_create_own_lobby(user).id == lobby.id
      assert Repo.aggregate(from(l in Lobby, where: l.owner_id == ^user.id), :count) == 1
    end

    test "every user gets a distinct lobby" do
      a = player_fixture()
      b = player_fixture()

      refute Lobbies.get_or_create_own_lobby(a).id == Lobbies.get_or_create_own_lobby(b).id
    end
  end

  describe "fetch_lobby/1" do
    test "returns the lobby for its game id" do
      lobby = lobby_fixture(player_fixture())
      lobby_id = lobby.id

      assert {:ok, %Lobby{id: ^lobby_id}} = Lobbies.fetch_lobby(lobby_id)
    end

    test "malformed ids are :invalid_game_id" do
      for id <- ["", "nope", "123", "not-a-uuid-at-all-really", nil, 42, %{}] do
        assert Lobbies.fetch_lobby(id) == {:error, :invalid_game_id}, "for #{inspect(id)}"
      end
    end

    test "an unknown uuid is :invalid_game_id" do
      assert Lobbies.fetch_lobby(Ecto.UUID.generate()) == {:error, :invalid_game_id}
    end
  end

  describe "authorize/2" do
    setup do
      owner = player_fixture()
      %{owner: owner, lobby: lobby_fixture(owner)}
    end

    test "the owner is authorized", %{owner: owner, lobby: lobby} do
      assert Lobbies.authorize(lobby, owner) == :ok
    end

    test "a stranger is not", %{lobby: lobby} do
      assert Lobbies.authorize(lobby, player_fixture()) == {:error, :unauthorized}
    end

    test "pending and accepted invitees are authorized", %{lobby: lobby} do
      pending = player_fixture()
      accepted = player_fixture()
      Repo.insert!(%Invite{lobby_id: lobby.id, user_id: pending.id})
      Repo.insert!(%Invite{lobby_id: lobby.id, user_id: accepted.id, status: "accepted"})

      assert Lobbies.authorize(lobby, pending) == :ok
      assert Lobbies.authorize(lobby, accepted) == :ok
    end
  end

  describe "members/1" do
    test "owner alone is the leader" do
      owner = player_fixture()
      owner_id = owner.id

      assert [%{user: %User{id: ^owner_id}, leader?: true}] =
               Lobbies.members(lobby_fixture(owner))
    end

    test "owner first, then invitees in invitation order" do
      owner = player_fixture()
      lobby = lobby_fixture(owner)
      b = player_fixture()
      c = player_fixture()
      Repo.insert!(%Invite{lobby_id: lobby.id, user_id: b.id})
      Repo.insert!(%Invite{lobby_id: lobby.id, user_id: c.id})

      assert Enum.map(Lobbies.members(lobby), &{&1.user.id, &1.leader?}) ==
               [{owner.id, true}, {b.id, false}, {c.id, false}]
    end
  end

  describe "error_message/1" do
    test "human messages for every error atom" do
      assert Lobbies.error_message(:not_leader) == "Only the table leader can do that"
      assert Lobbies.error_message(:unauthorized) == "Only invited players can join this table"
      assert Lobbies.error_message(:invalid_game_id) == "That table does not exist"
      assert Lobbies.error_message(:self_invite) == "You can't invite yourself"
      assert Lobbies.error_message(:already_invited) == "That player is already invited"
      assert Lobbies.error_message(:lobby_full) == "The table is full (7 players max)"
      assert Lobbies.error_message(:invalid_user) == "That player does not exist"
      assert Lobbies.error_message(:not_invited) == "That invitation is no longer valid"
    end
  end

  test "max_players/0 is 7" do
    assert Lobbies.max_players() == 7
  end
end
```

- [ ] **Step 3: Run the tests to verify they fail**

Run: `cd helios && mix test test/helios/lobbies_test.exs`
Expected: FAIL — `module Helios.Lobbies.Invite is not available` / `Helios.Lobbies.Lobby.__struct__/1 is undefined` (compile error in the test file).

- [ ] **Step 4: Generate and write the migrations**

Run (from `helios/`, one after the other so timestamps differ):

```bash
mix ecto.gen.migration create_lobbies
mix ecto.gen.migration create_lobby_invites
```

Replace the body of the generated `priv/repo/migrations/<ts>_create_lobbies.exs`:

```elixir
defmodule Helios.Repo.Migrations.CreateLobbies do
  use Ecto.Migration

  def change do
    create table(:lobbies, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :owner_id, references(:users, on_delete: :delete_all), null: false

      timestamps(type: :utc_datetime)
    end

    create unique_index(:lobbies, [:owner_id])
  end
end
```

Replace the body of the generated `priv/repo/migrations/<ts>_create_lobby_invites.exs`:

```elixir
defmodule Helios.Repo.Migrations.CreateLobbyInvites do
  use Ecto.Migration

  def change do
    create table(:lobby_invites) do
      add :lobby_id, references(:lobbies, type: :binary_id, on_delete: :delete_all), null: false
      add :user_id, references(:users, on_delete: :delete_all), null: false
      add :status, :string, null: false, default: "pending"

      timestamps(type: :utc_datetime)
    end

    create unique_index(:lobby_invites, [:lobby_id, :user_id])
    create index(:lobby_invites, [:user_id])
  end
end
```

- [ ] **Step 5: Write the schemas**

Create `helios/lib/helios/lobbies/lobby.ex`:

```elixir
defmodule Helios.Lobbies.Lobby do
  @moduledoc """
  A table. Its `id` is the public `game_id`; its owner is its leader forever.
  """
  use Ecto.Schema

  alias Helios.Accounts.User

  @type t :: %__MODULE__{}

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "lobbies" do
    belongs_to :owner, User

    timestamps(type: :utc_datetime)
  end
end
```

Create `helios/lib/helios/lobbies/invite.ex`:

```elixir
defmodule Helios.Lobbies.Invite do
  @moduledoc """
  An invitation to a table. Any row (whatever its status) authorizes the
  invitee to open the table; `"pending"` rows drive invite notifications.
  """
  use Ecto.Schema

  import Ecto.Changeset

  alias Helios.Accounts.User
  alias Helios.Lobbies.Lobby

  @type t :: %__MODULE__{}

  schema "lobby_invites" do
    belongs_to :lobby, Lobby, type: :binary_id
    belongs_to :user, User
    field :status, :string, default: "pending"

    timestamps(type: :utc_datetime)
  end

  @doc """
  Changeset for inserting an invite whose `lobby_id` and `user_id` were set
  programmatically. Maps the unique index violation to a changeset error.
  """
  def create_changeset(%__MODULE__{} = invite) do
    invite
    |> change()
    |> unique_constraint([:lobby_id, :user_id])
  end
end
```

- [ ] **Step 6: Write the read side of the context**

Create `helios/lib/helios/lobbies.ex`:

```elixir
defmodule Helios.Lobbies do
  @moduledoc """
  Tables ("lobbies").

  Every user owns exactly one lobby, created lazily; its owner is its leader
  forever. The owner invites other users; an invite row (any status) makes the
  invitee *authorized* to open the lobby. Leaving a lobby never removes
  authorization.

  PubSub messages (server `Helios.PubSub`):

    * `"user:<user_id>"` — `{:invited, %Invite{lobby: %Lobby{owner: %User{}}}}`,
      `{:uninvited, lobby_id}`, `{:invite_resolved, lobby_id}`
    * `"lobby:<lobby_id>"` — `{:members_changed}`, `{:declined, %User{}}`
  """

  import Ecto.Query, warn: false

  alias Helios.Accounts.User
  alias Helios.Lobbies.Invite
  alias Helios.Lobbies.Lobby
  alias Helios.Repo

  @max_players 7

  @type member :: %{user: User.t(), leader?: boolean()}

  @doc "Maximum number of players at a table, owner included."
  @spec max_players() :: pos_integer()
  def max_players, do: @max_players

  @spec lobby_topic(Ecto.UUID.t()) :: String.t()
  def lobby_topic(lobby_id), do: "lobby:#{lobby_id}"

  @spec user_topic(integer()) :: String.t()
  def user_topic(user_id), do: "user:#{user_id}"

  @doc """
  Returns the user's own lobby, creating it on first use. Race-safe: a
  concurrent insert is ignored through the unique owner index and re-fetched.
  """
  @spec get_or_create_own_lobby(User.t()) :: Lobby.t()
  def get_or_create_own_lobby(%User{id: user_id}) do
    case Repo.get_by(Lobby, owner_id: user_id) do
      %Lobby{} = lobby ->
        lobby

      nil ->
        Repo.insert!(%Lobby{owner_id: user_id}, on_conflict: :nothing, conflict_target: [:owner_id])
        Repo.get_by!(Lobby, owner_id: user_id)
    end
  end

  @doc "Looks a lobby up by its public game id."
  @spec fetch_lobby(term()) :: {:ok, Lobby.t()} | {:error, :invalid_game_id}
  def fetch_lobby(game_id) do
    with {:ok, id} <- cast_lobby_id(game_id),
         %Lobby{} = lobby <- Repo.get(Lobby, id) do
      {:ok, lobby}
    else
      _ -> {:error, :invalid_game_id}
    end
  end

  @doc "The owner and every invited user (any status) may open the lobby."
  @spec authorize(Lobby.t(), User.t()) :: :ok | {:error, :unauthorized}
  def authorize(%Lobby{owner_id: owner_id}, %User{id: owner_id}), do: :ok

  def authorize(%Lobby{id: lobby_id}, %User{id: user_id}) do
    if Repo.exists?(from i in Invite, where: i.lobby_id == ^lobby_id and i.user_id == ^user_id) do
      :ok
    else
      {:error, :unauthorized}
    end
  end

  @doc "Owner first (leader), then invitees in invitation order."
  @spec members(Lobby.t()) :: [member()]
  def members(%Lobby{} = lobby) do
    owner = Repo.get!(User, lobby.owner_id)

    invitees =
      Repo.all(
        from i in Invite,
          join: u in assoc(i, :user),
          where: i.lobby_id == ^lobby.id,
          order_by: [asc: i.inserted_at, asc: i.id],
          select: u
      )

    [%{user: owner, leader?: true} | Enum.map(invitees, &%{user: &1, leader?: false})]
  end

  @doc "Human message for every lobby error atom."
  @spec error_message(atom()) :: String.t()
  def error_message(:not_leader), do: "Only the table leader can do that"
  def error_message(:unauthorized), do: "Only invited players can join this table"
  def error_message(:invalid_game_id), do: "That table does not exist"
  def error_message(:self_invite), do: "You can't invite yourself"
  def error_message(:already_invited), do: "That player is already invited"
  def error_message(:lobby_full), do: "The table is full (#{@max_players} players max)"
  def error_message(:invalid_user), do: "That player does not exist"
  def error_message(:not_invited), do: "That invitation is no longer valid"

  defp cast_lobby_id(id) when is_binary(id), do: Ecto.UUID.cast(id)
  defp cast_lobby_id(_id), do: :error
end
```

- [ ] **Step 7: Run the tests to verify they pass**

Run: `cd helios && mix test test/helios/lobbies_test.exs`
Expected: PASS (all tests green; `mix test` runs the new migrations automatically via the `test` alias).

- [ ] **Step 8: Run precommit**

Run: `cd helios && mix precommit`
Expected: compile with no warnings, formatter applied, all tests pass.

- [ ] **Step 9: Commit**

```bash
git add helios/priv/repo/migrations/*_create_lobbies.exs helios/priv/repo/migrations/*_create_lobby_invites.exs \
  helios/lib/helios/lobbies.ex helios/lib/helios/lobbies/lobby.ex helios/lib/helios/lobbies/invite.ex \
  helios/test/support/fixtures/lobbies_fixtures.ex helios/test/helios/lobbies_test.exs
git status --short
git commit -m "feat(lobby): add lobbies/invites schema and read-side context

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: Invite / uninvite / accept / decline with broadcasts

**Files:**
- Modify: `helios/lib/helios/lobbies.ex`
- Modify: `helios/test/support/fixtures/lobbies_fixtures.ex`
- Test: `helios/test/helios/lobbies_test.exs`

**Interfaces:**
- Consumes: everything produced by Task 1; `Helios.Accounts.Scope` (`%Scope{user: %User{}}`, `Scope.for_user/1`).
- Produces:
  - `Lobbies.invite(%Scope{}, %Lobby{}, invitee_id :: integer() | String.t()) :: {:ok, %Invite{lobby: %Lobby{owner: %User{}}, user: %User{}}} | {:error, :not_leader | :self_invite | :already_invited | :lobby_full | :invalid_user}` — check order: `:not_leader`, id parse (`:invalid_user`), `:self_invite`, `:invalid_user` (no such user), `:already_invited`, `:lobby_full`. Broadcasts `{:invited, invite}` on `"user:<invitee_id>"` and `{:members_changed}` on `"lobby:<id>"`.
  - `Lobbies.uninvite(%Scope{}, %Lobby{}, user_id :: integer() | String.t()) :: :ok | {:error, :not_leader | :not_invited}` — broadcasts `{:uninvited, lobby_id}` on `"user:<user_id>"` and `{:members_changed}` on `"lobby:<id>"`.
  - `Lobbies.accept(%Scope{}, lobby_id :: String.t()) :: :ok | {:error, :not_invited}` — idempotent for already-accepted rows; broadcasts `{:invite_resolved, lobby_id}` on the invitee's own topic.
  - `Lobbies.decline(%Scope{}, lobby_id :: String.t()) :: :ok | {:error, :not_invited}` — only `"pending"` rows; deletes the row; broadcasts `{:declined, %User{}}` on `"lobby:<id>"` and `{:invite_resolved, lobby_id}` on `"user:<id>"`.
  - `Lobbies.pending_invites(%User{}) :: [%{lobby_id: String.t(), owner_name: String.t()}]` — newest first.
  - `Lobbies.owner_name(lobby_id) :: String.t() | nil`.
  - Fixture `invite_fixture(%Lobby{}, owner :: %User{}, invitee :: %User{}) :: %Invite{}`.

- [ ] **Step 1: Add the invite fixture**

Append inside `Helios.LobbiesFixtures` (after `lobby_fixture/1`):

```elixir
  def invite_fixture(lobby, owner, invitee) do
    {:ok, invite} = Lobbies.invite(Helios.Accounts.Scope.for_user(owner), lobby, invitee.id)
    invite
  end
```

- [ ] **Step 2: Write the failing mutation tests**

Append these `describe` blocks inside `Helios.LobbiesTest` (before the final `end`), and add `alias Helios.Accounts.Scope` next to the other aliases at the top of the module:

```elixir
  defp owner_and_lobby do
    owner = player_fixture()
    %{owner: owner, lobby: lobby_fixture(owner), scope: Scope.for_user(owner)}
  end

  describe "invite/3" do
    test "creates a pending invite that authorizes the invitee" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      guest = player_fixture()

      assert {:ok, %Invite{status: "pending"}} = Lobbies.invite(scope, lobby, guest.id)
      assert Lobbies.authorize(lobby, guest) == :ok
    end

    test "accepts the invitee id as a string (form params)" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      guest = player_fixture()

      assert {:ok, _} = Lobbies.invite(scope, lobby, Integer.to_string(guest.id))
    end

    test "broadcasts to the invitee (with the owner preloaded) and to the lobby" do
      %{owner: owner, lobby: lobby, scope: scope} = owner_and_lobby()
      guest = player_fixture()
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(guest.id))
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      {:ok, _invite} = Lobbies.invite(scope, lobby, guest.id)

      lobby_id = lobby.id
      owner_name = owner.name

      assert_receive {:invited,
                      %Invite{lobby_id: ^lobby_id, lobby: %Lobby{owner: %User{name: ^owner_name}}}}

      assert_receive {:members_changed}
    end

    test "only the owner may invite" do
      %{lobby: lobby} = owner_and_lobby()
      other = player_fixture()

      assert Lobbies.invite(Scope.for_user(other), lobby, player_fixture().id) ==
               {:error, :not_leader}
    end

    test "the owner cannot invite themselves" do
      %{owner: owner, lobby: lobby, scope: scope} = owner_and_lobby()

      assert Lobbies.invite(scope, lobby, owner.id) == {:error, :self_invite}
    end

    test "inviting twice is :already_invited, whatever the status" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      guest = player_fixture()
      {:ok, _} = Lobbies.invite(scope, lobby, guest.id)

      assert Lobbies.invite(scope, lobby, guest.id) == {:error, :already_invited}

      :ok = Lobbies.accept(Scope.for_user(guest), lobby.id)
      assert Lobbies.invite(scope, lobby, guest.id) == {:error, :already_invited}
    end

    test "unknown or malformed user ids are :invalid_user" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()

      for id <- [0, -1, 9_999_999, "abc", "", "12abc", nil, 1.5] do
        assert Lobbies.invite(scope, lobby, id) == {:error, :invalid_user}, "for #{inspect(id)}"
      end
    end

    test "a table holds at most 7 players, owner included" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()

      for _ <- 1..6 do
        assert {:ok, _} = Lobbies.invite(scope, lobby, player_fixture().id)
      end

      assert Lobbies.invite(scope, lobby, player_fixture().id) == {:error, :lobby_full}
      assert length(Lobbies.members(lobby)) == 7
    end
  end

  describe "uninvite/3" do
    test "removes authorization and broadcasts" do
      %{owner: owner, lobby: lobby, scope: scope} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(guest.id))
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      assert Lobbies.uninvite(scope, lobby, Integer.to_string(guest.id)) == :ok
      assert Lobbies.authorize(lobby, guest) == {:error, :unauthorized}

      lobby_id = lobby.id
      assert_receive {:uninvited, ^lobby_id}
      assert_receive {:members_changed}
    end

    test "only the owner may uninvite" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)

      assert Lobbies.uninvite(Scope.for_user(guest), lobby, guest.id) == {:error, :not_leader}
      assert Lobbies.authorize(lobby, guest) == :ok
    end

    test "uninviting someone who is not invited is :not_invited" do
      %{owner: owner, lobby: lobby, scope: scope} = owner_and_lobby()

      assert Lobbies.uninvite(scope, lobby, player_fixture().id) == {:error, :not_invited}
      assert Lobbies.uninvite(scope, lobby, owner.id) == {:error, :not_invited}
      assert Lobbies.uninvite(scope, lobby, "abc") == {:error, :not_invited}
    end
  end

  describe "accept/2" do
    test "marks the invite accepted, keeps authorization, broadcasts :invite_resolved" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(guest.id))

      assert Lobbies.accept(Scope.for_user(guest), lobby.id) == :ok

      lobby_id = lobby.id
      assert_receive {:invite_resolved, ^lobby_id}
      assert Repo.get_by!(Invite, lobby_id: lobby.id, user_id: guest.id).status == "accepted"
      assert Lobbies.authorize(lobby, guest) == :ok
      assert Lobbies.pending_invites(guest) == []
    end

    test "is idempotent" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)

      assert Lobbies.accept(Scope.for_user(guest), lobby.id) == :ok
      assert Lobbies.accept(Scope.for_user(guest), lobby.id) == :ok
    end

    test "without an invite or with a malformed id it is :not_invited" do
      %{lobby: lobby} = owner_and_lobby()
      stranger = Scope.for_user(player_fixture())

      assert Lobbies.accept(stranger, lobby.id) == {:error, :not_invited}
      assert Lobbies.accept(stranger, "not-a-uuid") == {:error, :not_invited}
      assert Lobbies.accept(stranger, nil) == {:error, :not_invited}
    end
  end

  describe "decline/2" do
    test "deletes the invite and broadcasts to the lobby and the invitee" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(guest.id))
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      assert Lobbies.decline(Scope.for_user(guest), lobby.id) == :ok
      assert Lobbies.authorize(lobby, guest) == {:error, :unauthorized}

      lobby_id = lobby.id
      guest_id = guest.id
      assert_receive {:declined, %User{id: ^guest_id}}
      assert_receive {:invite_resolved, ^lobby_id}
    end

    test "an accepted invite can no longer be declined" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      :ok = Lobbies.accept(Scope.for_user(guest), lobby.id)

      assert Lobbies.decline(Scope.for_user(guest), lobby.id) == {:error, :not_invited}
      assert Lobbies.authorize(lobby, guest) == :ok
    end

    test "without an invite or with a malformed id it is :not_invited" do
      %{lobby: lobby} = owner_and_lobby()
      stranger = Scope.for_user(player_fixture())

      assert Lobbies.decline(stranger, lobby.id) == {:error, :not_invited}
      assert Lobbies.decline(stranger, "garbage") == {:error, :not_invited}
    end
  end

  describe "pending_invites/1 and owner_name/1" do
    test "lists pending invites newest first with the owner's name" do
      guest = player_fixture()
      first = owner_and_lobby()
      second = owner_and_lobby()
      invite_fixture(first.lobby, first.owner, guest)
      invite_fixture(second.lobby, second.owner, guest)

      assert Lobbies.pending_invites(guest) == [
               %{lobby_id: second.lobby.id, owner_name: second.owner.name},
               %{lobby_id: first.lobby.id, owner_name: first.owner.name}
             ]
    end

    test "owner_name/1" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()

      assert Lobbies.owner_name(lobby.id) == owner.name
      assert Lobbies.owner_name(Ecto.UUID.generate()) == nil
      assert Lobbies.owner_name("nope") == nil
    end
  end
```

- [ ] **Step 3: Run the tests to verify they fail**

Run: `cd helios && mix test test/helios/lobbies_test.exs`
Expected: FAIL — `UndefinedFunctionError: function Helios.Lobbies.invite/3 is undefined` (and the same for `uninvite/3`, `accept/2`, `decline/2`, `pending_invites/1`, `owner_name/1`).

- [ ] **Step 4: Implement the mutations**

In `helios/lib/helios/lobbies.ex`, add `alias Helios.Accounts.Scope` next to the other aliases, then insert the following public functions directly after `members/1` (before `error_message/1`):

```elixir
  @doc """
  The owner invites `invitee_id` (integer or numeric string).
  """
  @spec invite(Scope.t(), Lobby.t(), term()) ::
          {:ok, Invite.t()}
          | {:error, :not_leader | :self_invite | :already_invited | :lobby_full | :invalid_user}
  def invite(%Scope{user: %User{} = user}, %Lobby{} = lobby, invitee_id) do
    with :ok <- ensure_leader(lobby, user),
         {:ok, invitee_id} <- parse_user_id(invitee_id, :invalid_user),
         :ok <- ensure_not_self(lobby, invitee_id),
         {:ok, invitee} <- fetch_user(invitee_id),
         :ok <- ensure_not_invited(lobby, invitee.id),
         :ok <- ensure_capacity(lobby),
         {:ok, invite} <- insert_invite(lobby, invitee) do
      invite = %{invite | lobby: %{lobby | owner: user}, user: invitee}
      broadcast(user_topic(invitee.id), {:invited, invite})
      broadcast(lobby_topic(lobby.id), {:members_changed})
      {:ok, invite}
    end
  end

  @doc "The owner removes an invitee (pending or accepted)."
  @spec uninvite(Scope.t(), Lobby.t(), term()) :: :ok | {:error, :not_leader | :not_invited}
  def uninvite(%Scope{user: %User{} = user}, %Lobby{} = lobby, user_id) do
    with :ok <- ensure_leader(lobby, user),
         {:ok, user_id} <- parse_user_id(user_id, :not_invited),
         {1, _} <-
           Repo.delete_all(
             from i in Invite, where: i.lobby_id == ^lobby.id and i.user_id == ^user_id
           ) do
      broadcast(user_topic(user_id), {:uninvited, lobby.id})
      broadcast(lobby_topic(lobby.id), {:members_changed})
      :ok
    else
      {0, _} -> {:error, :not_invited}
      {:error, _reason} = error -> error
    end
  end

  @doc "The invitee accepts; idempotent for an already accepted invite."
  @spec accept(Scope.t(), term()) :: :ok | {:error, :not_invited}
  def accept(%Scope{user: %User{} = user}, lobby_id) do
    with {:ok, lobby_id} <- cast_lobby_id(lobby_id),
         {1, _} <-
           Repo.update_all(
             from(i in Invite, where: i.lobby_id == ^lobby_id and i.user_id == ^user.id),
             set: [status: "accepted", updated_at: now()]
           ) do
      broadcast(user_topic(user.id), {:invite_resolved, lobby_id})
      :ok
    else
      _ -> {:error, :not_invited}
    end
  end

  @doc "The invitee declines a pending invite and loses authorization."
  @spec decline(Scope.t(), term()) :: :ok | {:error, :not_invited}
  def decline(%Scope{user: %User{} = user}, lobby_id) do
    with {:ok, lobby_id} <- cast_lobby_id(lobby_id),
         {1, _} <-
           Repo.delete_all(
             from i in Invite,
               where:
                 i.lobby_id == ^lobby_id and i.user_id == ^user.id and i.status == "pending"
           ) do
      broadcast(lobby_topic(lobby_id), {:declined, user})
      broadcast(user_topic(user.id), {:invite_resolved, lobby_id})
      :ok
    else
      _ -> {:error, :not_invited}
    end
  end

  @doc "Pending invites for `user`, newest first."
  @spec pending_invites(User.t()) :: [%{lobby_id: Ecto.UUID.t(), owner_name: String.t()}]
  def pending_invites(%User{id: user_id}) do
    Repo.all(
      from i in Invite,
        join: l in assoc(i, :lobby),
        join: o in assoc(l, :owner),
        where: i.user_id == ^user_id and i.status == "pending",
        order_by: [desc: i.inserted_at, desc: i.id],
        select: %{lobby_id: l.id, owner_name: o.name}
    )
  end

  @doc "Name of the lobby's owner, or nil for an unknown/malformed id."
  @spec owner_name(term()) :: String.t() | nil
  def owner_name(lobby_id) do
    case cast_lobby_id(lobby_id) do
      {:ok, id} ->
        Repo.one(from l in Lobby, join: o in assoc(l, :owner), where: l.id == ^id, select: o.name)

      :error ->
        nil
    end
  end
```

Then add these private helpers at the bottom of the module (next to `cast_lobby_id/1`):

```elixir
  defp ensure_leader(%Lobby{owner_id: owner_id}, %User{id: owner_id}), do: :ok
  defp ensure_leader(_lobby, _user), do: {:error, :not_leader}

  defp parse_user_id(id, _error) when is_integer(id) and id > 0, do: {:ok, id}

  defp parse_user_id(id, error) when is_binary(id) do
    case Integer.parse(id) do
      {int, ""} when int > 0 -> {:ok, int}
      _ -> {:error, error}
    end
  end

  defp parse_user_id(_id, error), do: {:error, error}

  defp ensure_not_self(%Lobby{owner_id: owner_id}, owner_id), do: {:error, :self_invite}
  defp ensure_not_self(_lobby, _user_id), do: :ok

  defp fetch_user(user_id) do
    case Repo.get(User, user_id) do
      %User{} = user -> {:ok, user}
      nil -> {:error, :invalid_user}
    end
  end

  defp ensure_not_invited(%Lobby{id: lobby_id}, user_id) do
    if Repo.exists?(from i in Invite, where: i.lobby_id == ^lobby_id and i.user_id == ^user_id) do
      {:error, :already_invited}
    else
      :ok
    end
  end

  defp ensure_capacity(%Lobby{id: lobby_id}) do
    invites = Repo.aggregate(from(i in Invite, where: i.lobby_id == ^lobby_id), :count)

    if invites + 1 >= @max_players, do: {:error, :lobby_full}, else: :ok
  end

  defp insert_invite(%Lobby{id: lobby_id}, %User{id: user_id}) do
    %Invite{lobby_id: lobby_id, user_id: user_id}
    |> Invite.create_changeset()
    |> Repo.insert()
    |> case do
      {:ok, invite} -> {:ok, invite}
      {:error, %Ecto.Changeset{}} -> {:error, :already_invited}
    end
  end

  defp broadcast(topic, message), do: Phoenix.PubSub.broadcast(Helios.PubSub, topic, message)

  defp now, do: DateTime.utc_now() |> DateTime.truncate(:second)
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cd helios && mix test test/helios/lobbies_test.exs`
Expected: PASS.

- [ ] **Step 6: Run precommit**

Run: `cd helios && mix precommit`
Expected: PASS, no warnings.

- [ ] **Step 7: Commit**

```bash
git add helios/lib/helios/lobbies.ex helios/test/support/fixtures/lobbies_fixtures.ex helios/test/helios/lobbies_test.exs
git status --short
git commit -m "feat(lobby): invite, uninvite, accept, decline with PubSub broadcasts

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 3: OnlineTracker (debounced global online/offline)

**Files:**
- Create: `helios/lib/helios_web/online_tracker.ex`
- Modify: `helios/config/config.exs`, `helios/config/e2e.exs`
- Test: `helios/test/helios_web/online_tracker_test.exs`

**Interfaces:**
- Consumes: presence maps shaped like `HeliosWeb.Presence.list/1` output: `%{"<user_id>" => %{metas: [%{name: String.t()} | _]}}`; `presence_diff` payloads `%{joins: presence_map, leaves: presence_map}`.
- Produces (`HeliosWeb.OnlineTracker`):
  - `%OnlineTracker{online: %{user_id => name}, pending: %{user_id => {token :: reference(), timer :: reference()}}, grace_ms: non_neg_integer()}`
  - `topic() :: "users:online"`
  - `grace_ms() :: non_neg_integer()` — `Application.get_env(:helios, :presence_grace_ms, 5_000)`
  - `new(presences :: map(), opts :: [grace_ms: non_neg_integer()]) :: t()`
  - `handle_diff(t(), %{joins: map(), leaves: map()}) :: {t(), [event()]}` — schedules `{:confirm_offline, user_id, token}` to `self()` after `grace_ms` for each leave of an online user.
  - `confirm_offline(t(), user_id, token, presences :: map()) :: {t(), [event()]}`
  - `online_users(t()) :: [{user_id, name}]`
  - `to_user_id(String.t() | integer()) :: integer()`
  - `event :: {:came_online, user_id, name} | {:went_offline, user_id, name}`

- [ ] **Step 1: Write the failing tests**

Create `helios/test/helios_web/online_tracker_test.exs`:

```elixir
defmodule HeliosWeb.OnlineTrackerTest do
  use ExUnit.Case, async: true

  alias HeliosWeb.OnlineTracker

  defp presence(entries) do
    Map.new(entries, fn {id, name} -> {to_string(id), %{metas: [%{name: name, phx_ref: "r"}]}} end)
  end

  defp diff(joins, leaves), do: %{joins: presence(joins), leaves: presence(leaves)}

  test "topic/0 and to_user_id/1" do
    assert OnlineTracker.topic() == "users:online"
    assert OnlineTracker.to_user_id("42") == 42
    assert OnlineTracker.to_user_id(42) == 42
  end

  test "new/2 builds the online set from a presence list" do
    tracker = OnlineTracker.new(presence([{1, "ann"}, {2, "bob"}]), grace_ms: 10)

    assert Enum.sort(OnlineTracker.online_users(tracker)) == [{1, "ann"}, {2, "bob"}]
  end

  test "grace_ms defaults to the application config" do
    assert OnlineTracker.new(%{}).grace_ms == OnlineTracker.grace_ms()
  end

  test "a fresh join emits :came_online" do
    tracker = OnlineTracker.new(%{}, grace_ms: 10)

    {tracker, events} = OnlineTracker.handle_diff(tracker, diff([{3, "cid"}], []))

    assert events == [{:came_online, 3, "cid"}]
    assert OnlineTracker.online_users(tracker) == [{3, "cid"}]
  end

  test "a join for an already online user (second tab) emits nothing" do
    tracker = OnlineTracker.new(presence([{3, "cid"}]), grace_ms: 10)

    assert {_tracker, []} = OnlineTracker.handle_diff(tracker, diff([{3, "cid"}], []))
  end

  test "leave then confirm while absent emits :went_offline" do
    tracker = OnlineTracker.new(presence([{3, "cid"}]), grace_ms: 0)

    {tracker, []} = OnlineTracker.handle_diff(tracker, diff([], [{3, "cid"}]))
    assert_receive {:confirm_offline, 3, token}

    {tracker, events} = OnlineTracker.confirm_offline(tracker, 3, token, %{})

    assert events == [{:went_offline, 3, "cid"}]
    assert OnlineTracker.online_users(tracker) == []
  end

  test "leave then join within the grace period emits nothing and cancels the check" do
    tracker = OnlineTracker.new(presence([{3, "cid"}]), grace_ms: 50)

    {tracker, []} = OnlineTracker.handle_diff(tracker, diff([], [{3, "cid"}]))
    {tracker, events} = OnlineTracker.handle_diff(tracker, diff([{3, "cid"}], []))

    assert events == []
    assert tracker.pending == %{}
    assert OnlineTracker.online_users(tracker) == [{3, "cid"}]
    refute_receive {:confirm_offline, 3, _}, 100
  end

  test "leave and join in the same diff (live navigation) emits nothing" do
    tracker = OnlineTracker.new(presence([{3, "cid"}]), grace_ms: 50)

    assert {%OnlineTracker{pending: pending}, []} =
             OnlineTracker.handle_diff(tracker, diff([{3, "cid"}], [{3, "cid"}]))

    assert pending == %{}
  end

  # Review Focus 2: same user, two tabs, one closes.
  test "confirm while the user is still present (other tab) emits nothing" do
    tracker = OnlineTracker.new(presence([{3, "cid"}]), grace_ms: 0)

    {tracker, []} = OnlineTracker.handle_diff(tracker, diff([], [{3, "cid"}]))
    assert_receive {:confirm_offline, 3, token}

    {tracker, events} = OnlineTracker.confirm_offline(tracker, 3, token, presence([{3, "cid"}]))

    assert events == []
    assert OnlineTracker.online_users(tracker) == [{3, "cid"}]
    assert tracker.pending == %{}
  end

  # Review Focus 3: stale timer message after a re-join cancelled the check.
  test "a stale confirm message (token no longer pending) is ignored" do
    tracker = OnlineTracker.new(presence([{3, "cid"}]), grace_ms: 0)

    {tracker, []} = OnlineTracker.handle_diff(tracker, diff([], [{3, "cid"}]))
    assert_receive {:confirm_offline, 3, stale_token}
    {tracker, []} = OnlineTracker.handle_diff(tracker, diff([{3, "cid"}], []))

    assert {^tracker, []} = OnlineTracker.confirm_offline(tracker, 3, stale_token, %{})
  end

  test "a leave for a user that was never online is ignored" do
    tracker = OnlineTracker.new(%{}, grace_ms: 0)

    assert {%OnlineTracker{pending: pending}, []} =
             OnlineTracker.handle_diff(tracker, diff([], [{9, "zed"}]))

    assert pending == %{}
    refute_receive {:confirm_offline, 9, _}, 20
  end
end
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cd helios && mix test test/helios_web/online_tracker_test.exs`
Expected: FAIL — `module HeliosWeb.OnlineTracker is not available`.

- [ ] **Step 3: Implement OnlineTracker**

Create `helios/lib/helios_web/online_tracker.ex`:

```elixir
defmodule HeliosWeb.OnlineTracker do
  @moduledoc """
  Debounced global online/offline tracking over the `"users:online"` presence
  topic, kept in a LiveView's assigns.

  * A join for a user not previously online emits `{:came_online, id, name}`.
  * A leave schedules `{:confirm_offline, id, token}` to `self()` after the
    grace period (`config :helios, :presence_grace_ms`, default 5 s).
  * A join while a check is pending cancels it and emits nothing (reconnects
    and reloads do not flap).
  * On confirm, a user still absent from the presence list emits
    `{:went_offline, id, name}`; a stale token is ignored.

  Presence keys are stringified by Phoenix, so ids are normalised with
  `to_user_id/1`.
  """

  @topic "users:online"
  @default_grace_ms 5_000

  defstruct online: %{}, pending: %{}, grace_ms: @default_grace_ms

  @type user_id :: integer()
  @type event :: {:came_online, user_id(), String.t()} | {:went_offline, user_id(), String.t()}
  @type t :: %__MODULE__{
          online: %{user_id() => String.t()},
          pending: %{user_id() => {reference(), reference()}},
          grace_ms: non_neg_integer()
        }

  @spec topic() :: String.t()
  def topic, do: @topic

  @spec grace_ms() :: non_neg_integer()
  def grace_ms, do: Application.get_env(:helios, :presence_grace_ms, @default_grace_ms)

  @spec new(map(), keyword()) :: t()
  def new(presences, opts \\ []) do
    %__MODULE__{
      online: names(presences),
      grace_ms: Keyword.get_lazy(opts, :grace_ms, &grace_ms/0)
    }
  end

  @spec online_users(t()) :: [{user_id(), String.t()}]
  def online_users(%__MODULE__{online: online}), do: Map.to_list(online)

  @spec to_user_id(String.t() | integer()) :: user_id()
  def to_user_id(id) when is_integer(id), do: id
  def to_user_id(id) when is_binary(id), do: String.to_integer(id)

  @spec handle_diff(t(), %{joins: map(), leaves: map()}) :: {t(), [event()]}
  def handle_diff(%__MODULE__{} = tracker, %{joins: joins, leaves: leaves}) do
    tracker =
      leaves
      |> names()
      |> Enum.reduce(tracker, fn {id, _name}, acc -> schedule_check(acc, id) end)

    {tracker, events} =
      joins
      |> names()
      |> Enum.reduce({tracker, []}, fn {id, name}, {acc, events} ->
        case join(acc, id, name) do
          {acc, nil} -> {acc, events}
          {acc, event} -> {acc, [event | events]}
        end
      end)

    {tracker, Enum.reverse(events)}
  end

  @spec confirm_offline(t(), user_id(), reference(), map()) :: {t(), [event()]}
  def confirm_offline(%__MODULE__{} = tracker, user_id, token, presences) do
    case Map.fetch(tracker.pending, user_id) do
      {:ok, {^token, _timer}} ->
        tracker = %{tracker | pending: Map.delete(tracker.pending, user_id)}

        if Map.has_key?(names(presences), user_id) do
          {tracker, []}
        else
          {name, online} = Map.pop(tracker.online, user_id)
          {%{tracker | online: online}, [{:went_offline, user_id, name}]}
        end

      _ ->
        {tracker, []}
    end
  end

  defp join(tracker, id, name) do
    cond do
      Map.has_key?(tracker.pending, id) -> {cancel_check(tracker, id), nil}
      Map.has_key?(tracker.online, id) -> {tracker, nil}
      true -> {%{tracker | online: Map.put(tracker.online, id, name)}, {:came_online, id, name}}
    end
  end

  defp schedule_check(tracker, id) do
    if Map.has_key?(tracker.online, id) do
      tracker = cancel_check(tracker, id)
      token = make_ref()
      timer = Process.send_after(self(), {:confirm_offline, id, token}, tracker.grace_ms)
      %{tracker | pending: Map.put(tracker.pending, id, {token, timer})}
    else
      tracker
    end
  end

  defp cancel_check(tracker, id) do
    case Map.pop(tracker.pending, id) do
      {nil, _pending} ->
        tracker

      {{_token, timer}, pending} ->
        Process.cancel_timer(timer)
        %{tracker | pending: pending}
    end
  end

  defp names(presences) do
    Map.new(presences, fn {key, %{metas: [meta | _]}} -> {to_user_id(key), meta.name} end)
  end
end
```

- [ ] **Step 4: Add the configuration**

In `helios/config/config.exs`, directly after the existing `config :helios, ecto_repos: ..., generators: ...` block, add:

```elixir
# Grace period before a disconnected user is reported offline
# (see HeliosWeb.OnlineTracker). The e2e env shortens it.
config :helios, presence_grace_ms: 5_000
```

In `helios/config/e2e.exs` (created by Phase 1), add at the end:

```elixir
config :helios, presence_grace_ms: 2_000
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cd helios && mix test test/helios_web/online_tracker_test.exs`
Expected: PASS.

- [ ] **Step 6: Run precommit**

Run: `cd helios && mix precommit`
Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add helios/lib/helios_web/online_tracker.ex helios/test/helios_web/online_tracker_test.exs helios/config/config.exs helios/config/e2e.exs
git status --short
git commit -m "feat(lobby): add debounced OnlineTracker with configurable grace period

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 4: LobbyLive, routing and `/` redirect

**Files:**
- Create: `helios/lib/helios_web/live/lobby_live.ex`
- Modify: `helios/lib/helios_web/router.ex`, `helios/lib/helios_web/controllers/page_controller.ex`, `helios/lib/helios_web/components/layouts.ex`
- Modify: `helios/test/support/fixtures/lobbies_fixtures.ex`
- Replace: `helios/test/helios_web/controllers/page_controller_test.exs`
- Delete: `helios/lib/helios_web/live/lobby_stub_live.ex` (+ `helios/test/helios_web/live/lobby_stub_live_test.exs` if it exists)
- Modify: `e2e/tests/support/auth.ts`, `e2e/tests/auth.spec.ts`, and any Phase 1 ExUnit test referencing `/lobby`
- Test: `helios/test/helios_web/live/lobby_live_test.exs`

**Interfaces:**
- Consumes: Task 1–3 functions; Phase 1 `HeliosWeb.UserAuth` `:require_user`, `HeliosWeb.Presence` (`track/4`, `list/1`), `Layouts.app` (`flash`, `current_scope`), `site_header/1`.
- Produces:
  - Route `live "/lobby/:game_id", LobbyLive, :show` in the `:authenticated` live_session.
  - `HeliosWeb.LobbyLive` assigns: `lobby :: %Lobby{}`, `owner? :: boolean()`, `members :: [Lobbies.member()]`, `connected_ids :: MapSet.t(integer())`, `online :: %OnlineTracker{}`, `invite_form :: Phoenix.HTML.Form.t()` (as `:invite`, field `user_id`), `invite_options :: [{name, id}]`, `rows`, `free_slots`, `page_title`. Phase 4 adds Start/Rejoin to this module and relies on `handle_info({:members_changed}, socket)` reloading `members`.
  - LobbyLive tracks `HeliosWeb.Presence` on `"lobby:<id>"`, key `user.id`, meta `%{name: user.name}` (read back as string keys).
  - DOM contract (used by ExUnit and Playwright): `#lobby`, `#lobby-title`, `#invite-form`, select `#invite_user_id` (prompt option `value=""`), `#invite-button`, `#members-table`, member rows `tr#member-<user_id>[data-member][data-name][data-leader][data-self][data-connected]` (`"true"|"false"`), uninvite buttons `#uninvite-<user_id>` with `aria-label="Remove <name>"`, FREE rows `tr#free-slot-<n>[data-free-slot]`, header link `#my-table-link` (text "My table", `href="/"`).
  - `GET /` → `/lobby/<own game_id>` for logged-in users, `/login` for guests.
  - Fixture `log_in_player(conn, name \\ unique_player_name()) :: {conn, %User{}}`.
  - e2e: `LOBBY_URL` regex exported from `e2e/tests/support/auth.ts`.

- [ ] **Step 1: Add the log-in fixture**

Append inside `Helios.LobbiesFixtures`:

```elixir
  @doc "Creates a player and returns a conn whose session is logged in as them."
  def log_in_player(conn, name \\ unique_player_name()) do
    {user, token} = player_with_token_fixture(name)
    {Plug.Test.init_test_session(conn, %{user_token: token}), user}
  end
```

- [ ] **Step 2: Write the failing LobbyLive tests**

Create `helios/test/helios_web/live/lobby_live_test.exs`:

```elixir
defmodule HeliosWeb.LobbyLiveTest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.LobbiesFixtures

  alias Helios.Lobbies
  alias Phoenix.Socket.Broadcast

  defp count(view, selector) do
    view |> render() |> LazyHTML.from_fragment() |> LazyHTML.query(selector) |> Enum.count()
  end

  defp player_with_lobby(conn) do
    {conn, user} = log_in_player(conn)
    %{conn: conn, user: user, lobby: Lobbies.get_or_create_own_lobby(user)}
  end

  describe "owner view" do
    test "the owner is the only, connected leader with 6 FREE seats", %{conn: conn} do
      %{conn: conn, user: me, lobby: lobby} = player_with_lobby(conn)

      {:ok, view, _html} = live(conn, ~p"/lobby/#{lobby.id}")

      assert has_element?(view, "#lobby-title", "My table")
      assert count(view, "#members-table tr[data-member]") == 1

      assert has_element?(
               view,
               "#member-#{me.id}[data-leader='true'][data-self='true'][data-connected='true']",
               me.name
             )

      assert count(view, "#members-table tr[data-free-slot]") == 6
      assert has_element?(view, "#invite-form #invite_user_id")
      assert has_element?(view, "#invite-button[disabled]")
      refute has_element?(view, "#uninvite-#{me.id}")
      assert has_element?(view, "#my-table-link[href='/']", "My table")
    end

    test "owner invites an online player, who then joins the table", %{conn: conn} do
      a = player_with_lobby(conn)
      b = player_with_lobby(build_conn())

      # B online first, so A's initial presence list already contains B.
      {:ok, _b_own_view, _} = live(b.conn, ~p"/lobby/#{b.lobby.id}")
      {:ok, view_a, _} = live(a.conn, ~p"/lobby/#{a.lobby.id}")

      assert has_element?(view_a, "#invite_user_id option[value='#{b.user.id}']", b.user.name)
      refute has_element?(view_a, "#invite_user_id option[value='#{a.user.id}']")

      view_a
      |> form("#invite-form", invite: %{user_id: to_string(b.user.id)})
      |> render_change()

      refute has_element?(view_a, "#invite-button[disabled]")

      view_a
      |> form("#invite-form", invite: %{user_id: to_string(b.user.id)})
      |> render_submit()

      assert has_element?(
               view_a,
               "#member-#{b.user.id}[data-leader='false'][data-connected='false']",
               b.user.name
             )

      refute has_element?(view_a, "#invite_user_id option[value='#{b.user.id}']")
      assert has_element?(view_a, "#invite-button[disabled]")
      assert count(view_a, "#members-table tr[data-free-slot]") == 5
      assert Lobbies.authorize(a.lobby, b.user) == :ok

      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(a.lobby.id))
      {:ok, view_b, _} = live(b.conn, ~p"/lobby/#{a.lobby.id}")
      b_key = to_string(b.user.id)
      assert_receive %Broadcast{event: "presence_diff", payload: %{joins: %{^b_key => _}}}

      assert has_element?(view_a, "#member-#{b.user.id}[data-connected='true']")

      # Guest view: connected members only, no controls.
      assert has_element?(view_b, "#lobby-title", "#{a.user.name}'s table")
      refute has_element?(view_b, "#invite-form")
      refute has_element?(view_b, "button[id^='uninvite-']")
      assert count(view_b, "#members-table tr[data-free-slot]") == 0

      assert has_element?(
               view_b,
               "#member-#{a.user.id}[data-leader='true'][data-connected='true']"
             )

      assert has_element?(view_b, "#member-#{b.user.id}[data-self='true']")
    end

    test "owner removes an invitee", %{conn: conn} do
      a = player_with_lobby(conn)
      b = player_fixture()
      invite_fixture(a.lobby, a.user, b)

      {:ok, view, _} = live(a.conn, ~p"/lobby/#{a.lobby.id}")
      assert has_element?(view, "#uninvite-#{b.id}[aria-label='Remove #{b.name}']")

      view |> element("#uninvite-#{b.id}") |> render_click()

      refute has_element?(view, "#member-#{b.id}")
      assert Lobbies.authorize(a.lobby, b) == {:error, :unauthorized}
    end

    # Review Focus 1: tampered params never crash the LiveView.
    test "tampered invite/uninvite params show an error flash", %{conn: conn} do
      a = player_with_lobby(conn)
      {:ok, view, _} = live(a.conn, ~p"/lobby/#{a.lobby.id}")

      render_hook(view, "invite", %{"invite" => %{"user_id" => "abc"}})
      assert has_element?(view, "#flash-error", "That player does not exist")

      render_hook(view, "invite", %{"invite" => %{"user_id" => to_string(a.user.id)}})
      assert has_element?(view, "#flash-error", "You can't invite yourself")

      render_hook(view, "invite", %{})
      assert has_element?(view, "#flash-error", "That player does not exist")

      render_hook(view, "uninvite", %{"id" => "abc"})
      assert has_element?(view, "#flash-error", "That invitation is no longer valid")

      render_hook(view, "select_invitee", %{})
      assert has_element?(view, "#lobby")
    end
  end

  describe "guest view" do
    test "lists only connected members", %{conn: conn} do
      a = player_with_lobby(build_conn())
      b = player_with_lobby(conn)
      c = player_fixture()
      invite_fixture(a.lobby, a.user, b.user)
      invite_fixture(a.lobby, a.user, c)

      {:ok, view_b, _} = live(b.conn, ~p"/lobby/#{a.lobby.id}")

      assert count(view_b, "#members-table tr[data-member]") == 1
      assert has_element?(view_b, "#member-#{b.user.id}[data-self='true'][data-connected='true']")
      refute has_element?(view_b, "#member-#{a.user.id}")
      refute has_element?(view_b, "#member-#{c.id}")
    end

    test "non-owner invite and uninvite are rejected server-side", %{conn: conn} do
      a = player_with_lobby(build_conn())
      b = player_with_lobby(conn)
      c = player_fixture()
      invite_fixture(a.lobby, a.user, b.user)

      {:ok, view_b, _} = live(b.conn, ~p"/lobby/#{a.lobby.id}")

      render_hook(view_b, "invite", %{"invite" => %{"user_id" => to_string(c.id)}})
      assert has_element?(view_b, "#flash-error", "Only the table leader can do that")
      assert Lobbies.authorize(a.lobby, c) == {:error, :unauthorized}

      render_hook(view_b, "uninvite", %{"id" => to_string(b.user.id)})
      assert has_element?(view_b, "#flash-error", "Only the table leader can do that")
      assert Lobbies.authorize(a.lobby, b.user) == :ok
    end
  end

  describe "access" do
    test "a malformed game id redirects to the own lobby", %{conn: conn} do
      %{conn: conn, lobby: own} = player_with_lobby(conn)
      own_path = ~p"/lobby/#{own.id}"

      assert {:error, {_kind, %{to: ^own_path, flash: %{"error" => "That table does not exist"}}}} =
               live(conn, ~p"/lobby/not-a-uuid")
    end

    test "an unknown game id redirects to the own lobby", %{conn: conn} do
      %{conn: conn, lobby: own} = player_with_lobby(conn)
      own_path = ~p"/lobby/#{own.id}"

      assert {:error, {_kind, %{to: ^own_path, flash: %{"error" => "That table does not exist"}}}} =
               live(conn, ~p"/lobby/#{Ecto.UUID.generate()}")
    end

    test "an uninvited user is redirected to their own lobby", %{conn: conn} do
      a = player_with_lobby(build_conn())
      %{conn: conn, lobby: own} = player_with_lobby(conn)
      own_path = ~p"/lobby/#{own.id}"

      assert {:error,
              {_kind,
               %{to: ^own_path, flash: %{"error" => "Only invited players can join this table"}}}} =
               live(conn, ~p"/lobby/#{a.lobby.id}")
    end
  end
end
```

Replace `helios/test/helios_web/controllers/page_controller_test.exs` entirely with:

```elixir
defmodule HeliosWeb.PageControllerTest do
  use HeliosWeb.ConnCase, async: false

  import Helios.LobbiesFixtures

  alias Helios.Lobbies

  test "GET / redirects guests to /login", %{conn: conn} do
    conn = get(conn, ~p"/")

    assert redirected_to(conn) == ~p"/login"
  end

  test "GET / redirects a logged-in user to their own table", %{conn: conn} do
    {conn, user} = log_in_player(conn)

    conn = get(conn, ~p"/")

    lobby = Lobbies.get_or_create_own_lobby(user)
    assert redirected_to(conn) == ~p"/lobby/#{lobby.id}"
  end
end
```

- [ ] **Step 3: Run the tests to verify they fail**

Run: `cd helios && mix test test/helios_web/live/lobby_live_test.exs test/helios_web/controllers/page_controller_test.exs`
Expected: FAIL — `no route found for GET /lobby/...` (Phoenix.Router.NoRouteError) and the page controller redirecting to `/lobby` instead of `/lobby/<uuid>`.

- [ ] **Step 4: Write LobbyLive**

Create `helios/lib/helios_web/live/lobby_live.ex`:

```elixir
defmodule HeliosWeb.LobbyLive do
  @moduledoc """
  A table ("lobby"). The owner invites online players and removes invitees;
  invited players see who is currently connected.
  """
  use HeliosWeb, :live_view

  alias Helios.Lobbies
  alias HeliosWeb.OnlineTracker
  alias HeliosWeb.Presence
  alias Phoenix.Socket.Broadcast

  @online_topic "users:online"

  @impl true
  def mount(%{"game_id" => game_id}, _session, socket) do
    user = socket.assigns.current_scope.user

    with {:ok, lobby} <- Lobbies.fetch_lobby(game_id),
         :ok <- Lobbies.authorize(lobby, user) do
      {:ok, setup(socket, lobby, user)}
    else
      {:error, reason} ->
        own = Lobbies.get_or_create_own_lobby(user)

        {:ok,
         socket
         |> put_flash(:error, Lobbies.error_message(reason))
         |> push_navigate(to: ~p"/lobby/#{own.id}")}
    end
  end

  defp setup(socket, lobby, user) do
    topic = Lobbies.lobby_topic(lobby.id)

    if connected?(socket) do
      Phoenix.PubSub.subscribe(Helios.PubSub, topic)
      Phoenix.PubSub.subscribe(Helios.PubSub, @online_topic)
      {:ok, _ref} = Presence.track(self(), topic, user.id, %{name: user.name})
    end

    socket
    |> assign(:lobby, lobby)
    |> assign(:owner?, lobby.owner_id == user.id)
    |> assign(:online, OnlineTracker.new(Presence.list(@online_topic)))
    |> assign(:invite_form, invite_form(""))
    |> assign(:connected_ids, MapSet.new([user.id]))
    |> load_members()
    |> load_connected()
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <section id="lobby" class="mx-auto w-full max-w-3xl space-y-6 px-4 py-10">
        <h1 id="lobby-title" class="text-2xl font-semibold tracking-tight text-zinc-900">
          {@page_title}
        </h1>

        <.form
          :if={@owner?}
          for={@invite_form}
          id="invite-form"
          phx-change="select_invitee"
          phx-submit="invite"
          class="flex items-end gap-3"
        >
          <div class="flex-1">
            <.input
              field={@invite_form[:user_id]}
              type="select"
              label="Invite a player"
              options={@invite_options}
              prompt="Select username"
            />
          </div>
          <.button
            id="invite-button"
            type="submit"
            disabled={@invite_form[:user_id].value in [nil, ""]}
            class="mb-2 rounded-lg bg-zinc-900 px-5 py-2 font-semibold text-white shadow-sm transition hover:bg-zinc-700 disabled:cursor-not-allowed disabled:opacity-40"
          >
            Invite
          </.button>
        </.form>

        <div class="overflow-hidden rounded-xl bg-antique shadow-md ring-1 ring-zinc-900/10">
          <table id="members-table" class="w-full text-left">
            <thead class="bg-zinc-900 text-xs uppercase tracking-wider text-white">
              <tr>
                <th class="px-4 py-3">Username</th>
                <th :if={@owner?} class="w-24 px-4 py-3 text-right">Delete</th>
              </tr>
            </thead>
            <tbody>
              <tr
                :for={row <- @rows}
                id={"member-#{row.user.id}"}
                data-member
                data-name={row.user.name}
                data-leader={to_string(row.leader?)}
                data-self={to_string(row.self?)}
                data-connected={to_string(row.connected?)}
                class={[
                  "border-t border-zinc-900/10 transition-colors",
                  if(row.connected?, do: "text-zinc-900", else: "bg-disconnected text-zinc-500")
                ]}
              >
                <td class="px-4 py-3">
                  <span class="inline-flex items-center gap-2 font-medium">
                    <.icon :if={row.self?} name="hero-chevron-double-right" class="size-4" />
                    <.crown :if={row.leader?} />
                    {row.user.name}
                  </span>
                </td>
                <td :if={@owner?} class="px-4 py-3 text-right">
                  <button
                    :if={!row.self?}
                    id={"uninvite-#{row.user.id}"}
                    type="button"
                    phx-click="uninvite"
                    phx-value-id={row.user.id}
                    aria-label={"Remove #{row.user.name}"}
                    class="inline-flex size-8 items-center justify-center rounded-md bg-zinc-900 text-white transition hover:bg-red-700"
                  >
                    <.icon name="hero-x-mark" class="size-4" />
                  </button>
                </td>
              </tr>
              <tr
                :for={n <- 1..@free_slots//1}
                id={"free-slot-#{n}"}
                data-free-slot
                class="border-t border-zinc-900/10 text-zinc-400"
              >
                <td colspan="2" class="px-4 py-3 text-sm font-semibold uppercase tracking-widest">
                  FREE
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </section>
    </Layouts.app>
    """
  end

  # Heroicons has no crown; small inline SVG in the same style.
  defp crown(assigns) do
    ~H"""
    <svg
      data-icon="crown"
      role="img"
      aria-label="Leader"
      viewBox="0 0 24 24"
      fill="currentColor"
      class="size-4 text-amber-500"
    >
      <path d="M3 7l4.5 4L12 4l4.5 7L21 7l-2 11H5L3 7zm2 13h14v2H5v-2z" />
    </svg>
    """
  end

  @impl true
  def handle_event("select_invitee", %{"invite" => %{} = params}, socket) do
    {:noreply, assign(socket, :invite_form, invite_form(params["user_id"] || ""))}
  end

  def handle_event("select_invitee", _params, socket), do: {:noreply, socket}

  def handle_event("invite", %{"invite" => %{"user_id" => user_id}}, socket) do
    %{current_scope: scope, lobby: lobby} = socket.assigns

    case Lobbies.invite(scope, lobby, user_id) do
      {:ok, _invite} ->
        {:noreply, socket |> assign(:invite_form, invite_form("")) |> load_members()}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, Lobbies.error_message(reason))}
    end
  end

  def handle_event("invite", _params, socket) do
    {:noreply, put_flash(socket, :error, Lobbies.error_message(:invalid_user))}
  end

  def handle_event("uninvite", %{"id" => user_id}, socket) do
    %{current_scope: scope, lobby: lobby} = socket.assigns

    case Lobbies.uninvite(scope, lobby, user_id) do
      :ok -> {:noreply, load_members(socket)}
      {:error, reason} -> {:noreply, put_flash(socket, :error, Lobbies.error_message(reason))}
    end
  end

  def handle_event("uninvite", _params, socket) do
    {:noreply, put_flash(socket, :error, Lobbies.error_message(:not_invited))}
  end

  @impl true
  def handle_info(%Broadcast{event: "presence_diff", topic: @online_topic, payload: diff}, socket) do
    {online, _events} = OnlineTracker.handle_diff(socket.assigns.online, diff)
    {:noreply, socket |> assign(:online, online) |> assign_derived()}
  end

  def handle_info(%Broadcast{event: "presence_diff"}, socket) do
    {:noreply, load_connected(socket)}
  end

  def handle_info({:confirm_offline, user_id, token}, socket) do
    {online, _events} =
      OnlineTracker.confirm_offline(
        socket.assigns.online,
        user_id,
        token,
        Presence.list(@online_topic)
      )

    {:noreply, socket |> assign(:online, online) |> assign_derived()}
  end

  def handle_info({:members_changed}, socket), do: {:noreply, load_members(socket)}

  def handle_info({:declined, _user}, socket), do: {:noreply, load_members(socket)}

  defp invite_form(user_id), do: to_form(%{"user_id" => user_id}, as: :invite)

  defp load_members(socket) do
    members = Lobbies.members(socket.assigns.lobby)
    [%{user: owner} | _] = members
    title = if socket.assigns.owner?, do: "My table", else: "#{owner.name}'s table"

    socket
    |> assign(:members, members)
    |> assign(:page_title, title)
    |> assign_derived()
  end

  defp load_connected(socket) do
    %{lobby: lobby, current_scope: %{user: me}} = socket.assigns

    connected_ids =
      lobby.id
      |> Lobbies.lobby_topic()
      |> Presence.list()
      |> Map.keys()
      |> MapSet.new(&OnlineTracker.to_user_id/1)
      # Whoever renders this page is at the table.
      |> MapSet.put(me.id)

    socket |> assign(:connected_ids, connected_ids) |> assign_derived()
  end

  defp assign_derived(socket) do
    %{
      members: members,
      connected_ids: connected_ids,
      owner?: owner?,
      online: online,
      invite_form: form,
      current_scope: %{user: me}
    } = socket.assigns

    rows =
      members
      |> Enum.map(fn %{user: user, leader?: leader?} ->
        %{
          user: user,
          leader?: leader?,
          connected?: MapSet.member?(connected_ids, user.id),
          self?: user.id == me.id
        }
      end)
      |> Enum.filter(fn row -> owner? or row.connected? end)

    member_ids = MapSet.new(members, & &1.user.id)

    invite_options =
      online
      |> OnlineTracker.online_users()
      |> Enum.reject(fn {id, _name} -> id == me.id or MapSet.member?(member_ids, id) end)
      |> Enum.sort_by(fn {_id, name} -> name end)
      |> Enum.map(fn {id, name} -> {name, id} end)

    selected = form[:user_id].value

    form =
      if Enum.any?(invite_options, fn {_name, id} -> to_string(id) == selected end),
        do: form,
        else: invite_form("")

    socket
    |> assign(:rows, rows)
    |> assign(:free_slots, if(owner?, do: max(Lobbies.max_players() - length(members), 0), else: 0))
    |> assign(:invite_options, invite_options)
    |> assign(:invite_form, form)
  end
end
```

- [ ] **Step 5: Route, `/` redirect, header link, remove the stub**

In `helios/lib/helios_web/router.ex`, inside the Phase 1 `live_session :authenticated, on_mount: [{HeliosWeb.UserAuth, :require_user}] do ... end` block, replace the line `live "/lobby", LobbyStubLive` (whatever its action suffix) with:

```elixir
      live "/lobby/:game_id", LobbyLive, :show
```

Replace `helios/lib/helios_web/controllers/page_controller.ex` entirely with:

```elixir
defmodule HeliosWeb.PageController do
  use HeliosWeb, :controller

  alias Helios.Accounts.Scope
  alias Helios.Lobbies

  def home(conn, _params) do
    case conn.assigns[:current_scope] do
      %Scope{user: %{} = user} ->
        lobby = Lobbies.get_or_create_own_lobby(user)
        redirect(conn, to: ~p"/lobby/#{lobby.id}")

      _ ->
        redirect(conn, to: ~p"/login")
    end
  end
end
```

In `helios/lib/helios_web/components/layouts.ex`, inside Phase 1's `site_header/1` template, insert this link immediately before the "Logout" link (it goes to `/`, which redirects to the user's own table):

```heex
<.link
  href={~p"/"}
  id="my-table-link"
  class="rounded-md px-3 py-1.5 text-sm font-semibold text-white/90 transition hover:bg-white/15 hover:text-white"
>
  My table
</.link>
```

Remove the stub (tracked files only):

```bash
git rm helios/lib/helios_web/live/lobby_stub_live.ex
git rm --ignore-unmatch helios/test/helios_web/live/lobby_stub_live_test.exs
```

- [ ] **Step 6: Update remaining Phase 1 references to `/lobby`**

Run: `cd helios && grep -rn -E '~p"/lobby"|"/lobby"|LobbyStubLive' lib test`
For every hit (Phase 1 tests such as UserAuth/session-controller tests), apply exactly one of:
- A guest being redirected away from a protected page: use `~p"/lobby/#{Ecto.UUID.generate()}"` (`:require_user` redirects before `LobbyLive.mount/3` runs).
- A logged-in user expected to land on their table after login or after `:redirect_if_user`: the redirect target is `~p"/"` (unchanged Phase 1 behaviour; `/` then redirects to the table). If the test followed that redirect and asserted `"/lobby"`, assert instead `redirected_to(conn) =~ ~r"^/lobby/[0-9a-f-]{36}$"`.
Re-run the grep; expected: no output.

- [ ] **Step 7: Run the ExUnit tests to verify they pass**

Run: `cd helios && mix test test/helios_web/live/lobby_live_test.exs test/helios_web/controllers/page_controller_test.exs`
Expected: PASS. Then run `cd helios && mix precommit`; expected: PASS (whole suite, no warnings).

- [ ] **Step 8: Update the Phase 1 e2e assertions**

In `e2e/tests/support/auth.ts` add (import `expect` from `@playwright/test` if the file does not already):

```ts
/** Matches `/lobby/<uuid>` — every logged-in user lands on their own table. */
export const LOBBY_URL =
  /\/lobby\/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/;
```

If `login()` waits for the lobby URL after submitting (e.g. `waitForURL("**/lobby")` or `toHaveURL(/\/lobby$/)`), replace that wait with `await expect(page).toHaveURL(LOBBY_URL);`. If it does not wait for a URL, leave `login()` unchanged.

In `e2e/tests/auth.spec.ts` (run `grep -n "lobby" e2e/tests/auth.spec.ts`):
- Every assertion that the user is on `/lobby` → `await expect(page).toHaveURL(LOBBY_URL);` (import `LOBBY_URL` from `./support/auth`).
- Every `page.goto("/lobby")` (guest redirect scenario 6, post-logout scenario 8) → ``page.goto(`/lobby/${crypto.randomUUID()}`)`` (the `/login` assertion that follows stays as is).

Re-run `grep -n '"/lobby"\|/lobby\$\|\*\*/lobby' e2e/tests/auth.spec.ts e2e/tests/support/auth.ts`; expected: no output.

- [ ] **Step 9: Run the e2e suite**

Run: `cd e2e && npx playwright test`
Expected: PASS (Phase 1 `auth.spec.ts`, now landing on `/lobby/<uuid>`).

- [ ] **Step 10: Commit**

```bash
git add helios/lib/helios_web/live/lobby_live.ex helios/lib/helios_web/router.ex \
  helios/lib/helios_web/controllers/page_controller.ex helios/lib/helios_web/components/layouts.ex \
  helios/test/support/fixtures/lobbies_fixtures.ex helios/test/helios_web/live/lobby_live_test.exs \
  helios/test/helios_web/controllers/page_controller_test.exs \
  e2e/tests/support/auth.ts e2e/tests/auth.spec.ts
# plus, by explicit path, every Phase 1 test file edited in Step 6 (see `git status --short`)
git status --short   # the stub deletions are already staged by git rm
git commit -m "feat(lobby): add LobbyLive at /lobby/:game_id and redirect / to own table

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 5: Notifications hook, component, and lobby notifications

**Files:**
- Create: `helios/lib/helios_web/notifications.ex`
- Modify: `helios/lib/helios_web/components/layouts.ex`, `helios/lib/helios_web/router.ex`, `helios/lib/helios_web/live/lobby_live.ex`
- Test: `helios/test/helios_web/notifications_test.exs`

**Interfaces:**
- Consumes: `Lobbies.pending_invites/1`, `accept/2`, `decline/2`, `owner_name/1`, `get_or_create_own_lobby/1`, `user_topic/1`, `error_message/1`; `OnlineTracker` events `{:came_online | :went_offline, id, name}`.
- Produces:
  - `HeliosWeb.Notifications.on_mount(:default, params, session, socket)` — added to the `:authenticated` live_session after `{HeliosWeb.UserAuth, :require_user}`; assigns `:notifications`; subscribes to `"user:<id>"` when connected; handles `{:invited, invite}`, `{:uninvited, lobby_id}`, `{:invite_resolved, lobby_id}`, `{:expire_notification, id}` (all halted) and events `"accept_invite"`, `"decline_invite"` (param `"id"` = lobby id), `"dismiss_notification"` (param `"id"` = notification id). On `{:uninvited, lobby_id}` while the current LiveView's `@lobby.id == lobby_id`, it flashes `:info` "You were removed from <owner>'s table" and `push_navigate`s to the user's own table.
  - `Notifications.push_simple(socket, message) :: socket` — adds a simple notification and schedules its expiry (8 s).
  - Pure: `approve(%{lobby_id, owner_name}) :: notification`, `simple(message) :: notification`, `add([notification], notification) :: [notification]`, `remove([notification], id) :: [notification]`, `visible([notification]) :: [notification]` (first 5); `notification :: %{id: String.t(), kind: :approve | :simple, message: String.t(), lobby_id: String.t() | nil}`; approve ids are `"invite-<lobby_id>"`.
  - `Layouts.app` gains `attr :notifications, :list, default: []` and renders `Layouts.notifications/1` (`<.notifications items={@notifications} />`). **Every LiveView in the `:authenticated` session must pass `notifications={@notifications}` to `<Layouts.app>`** (Phase 4 GameLive included).
  - DOM contract: `#notifications`, items `#notification-<id>[data-notification="approve"|"simple"]`, buttons `#accept-invite-<lobby_id>` ("Accept"), `#decline-invite-<lobby_id>` ("Decline"), `#dismiss-<id>` ("OK").

- [ ] **Step 1: Write the failing tests**

Create `helios/test/helios_web/notifications_test.exs`:

```elixir
defmodule HeliosWeb.NotificationsTest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.LobbiesFixtures

  alias Helios.Accounts.Scope
  alias Helios.Lobbies
  alias HeliosWeb.Notifications
  alias Phoenix.Socket.Broadcast

  describe "add/2 (pure)" do
    test "newest first" do
      a = Notifications.approve(%{lobby_id: "l1", owner_name: "Ann"})
      s = Notifications.simple("hello")

      assert [] |> Notifications.add(a) |> Notifications.add(s) == [s, a]
      assert a.message == "You are expected on table Ann"
      assert a.id == "invite-l1"
    end

    # Review Focus 4: the same invite delivered twice yields one notification.
    test "de-duplicates by id, moving the notification to the front" do
      a = Notifications.approve(%{lobby_id: "l1", owner_name: "Ann"})
      s = Notifications.simple("hello")

      assert [] |> Notifications.add(a) |> Notifications.add(s) |> Notifications.add(a) == [a, s]
    end

    test "keeps at most 5 by evicting the oldest simple notifications" do
      list =
        Enum.reduce(1..6, [], fn i, acc -> Notifications.add(acc, Notifications.simple("s#{i}")) end)

      assert Enum.map(list, & &1.message) == ["s6", "s5", "s4", "s3", "s2"]
    end

    test "never evicts approve notifications to make room for simple ones" do
      approves =
        for i <- 1..5, do: Notifications.approve(%{lobby_id: "l#{i}", owner_name: "O#{i}"})

      list =
        approves
        |> Enum.reduce([], &Notifications.add(&2, &1))
        |> Notifications.add(Notifications.simple("late"))

      assert length(list) == 5
      assert Enum.all?(list, &(&1.kind == :approve))
    end

    test "a mix evicts simple before approve" do
      list =
        [
          Notifications.approve(%{lobby_id: "l1", owner_name: "O1"}),
          Notifications.simple("s1"),
          Notifications.approve(%{lobby_id: "l2", owner_name: "O2"}),
          Notifications.simple("s2"),
          Notifications.approve(%{lobby_id: "l3", owner_name: "O3"}),
          Notifications.simple("s3")
        ]
        |> Enum.reduce([], &Notifications.add(&2, &1))

      assert Enum.map(list, & &1.id) |> Enum.filter(&String.starts_with?(&1, "invite-")) ==
               ["invite-l3", "invite-l2", "invite-l1"]

      assert Enum.map(list, & &1.message) |> Enum.reject(&String.starts_with?(&1, "You")) ==
               ["s3", "s2"]
    end

    test "remove/2 and visible/1" do
      items = for i <- 1..7, do: Notifications.approve(%{lobby_id: "l#{i}", owner_name: "O"})

      assert length(Notifications.visible(items)) == 5
      assert Notifications.remove(items, "invite-l3") |> Enum.map(& &1.id) |> Enum.member?("invite-l3") == false
    end
  end

  defp two_players(conn) do
    {conn_a, a} = log_in_player(conn)
    {conn_b, b} = log_in_player(build_conn())

    %{
      conn_a: conn_a,
      a: a,
      lobby_a: Lobbies.get_or_create_own_lobby(a),
      conn_b: conn_b,
      b: b,
      lobby_b: Lobbies.get_or_create_own_lobby(b)
    }
  end

  describe "hook" do
    test "pending invites are rendered on mount", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)

      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      assert has_element?(
               view_b,
               "#notification-invite-#{ctx.lobby_a.id}[data-notification='approve']",
               "You are expected on table #{ctx.a.name}"
             )

      assert has_element?(view_b, "#accept-invite-#{ctx.lobby_a.id}", "Accept")
      assert has_element?(view_b, "#decline-invite-#{ctx.lobby_a.id}", "Decline")
    end

    test "an invite arriving while mounted is shown once", %{conn: conn} do
      ctx = two_players(conn)
      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)

      assert has_element?(view_b, "#notification-invite-#{ctx.lobby_a.id}")
      assert render(view_b) |> LazyHTML.from_fragment() |> LazyHTML.query("#notifications [data-notification]") |> Enum.count() == 1
    end

    test "accept marks the invite accepted and navigates to the table", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      {:ok, view_on_a, _} =
        view_b
        |> element("#accept-invite-#{ctx.lobby_a.id}")
        |> render_click()
        |> follow_redirect(ctx.conn_b, ~p"/lobby/#{ctx.lobby_a.id}")

      assert has_element?(view_on_a, "#members-table")
      refute has_element?(view_on_a, "#notification-invite-#{ctx.lobby_a.id}")
      assert Lobbies.pending_invites(ctx.b) == []
      assert Lobbies.authorize(ctx.lobby_a, ctx.b) == :ok
    end

    test "decline removes the notification and the authorization; the owner is told", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      {:ok, view_a, _} = live(ctx.conn_a, ~p"/lobby/#{ctx.lobby_a.id}")
      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      view_b |> element("#decline-invite-#{ctx.lobby_a.id}") |> render_click()

      refute has_element?(view_b, "#notification-invite-#{ctx.lobby_a.id}")
      assert Lobbies.authorize(ctx.lobby_a, ctx.b) == {:error, :unauthorized}

      assert has_element?(
               view_a,
               "#notifications [data-notification='simple']",
               "User #{ctx.b.name} declined your invitation!"
             )

      refute has_element?(view_a, "#member-#{ctx.b.id}")
    end

    test "simple notifications can be dismissed and expire", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      {:ok, view_a, _} = live(ctx.conn_a, ~p"/lobby/#{ctx.lobby_a.id}")
      :ok = Lobbies.decline(Scope.for_user(ctx.b), ctx.lobby_a.id)

      [dom_id] =
        view_a
        |> render()
        |> LazyHTML.from_fragment()
        |> LazyHTML.query("#notifications [data-notification='simple']")
        |> LazyHTML.attribute("id")

      "notification-" <> id = dom_id
      send(view_a.pid, {:expire_notification, id})
      refute has_element?(view_a, "#notifications [data-notification='simple']")

      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      :ok = Lobbies.decline(Scope.for_user(ctx.b), ctx.lobby_a.id)
      assert has_element?(view_a, "#notifications [data-notification='simple'] button", "OK")
      view_a |> element("#notifications [data-notification='simple'] button") |> render_click()
      refute has_element?(view_a, "#notifications [data-notification='simple']")
    end

    # Review Focus 5: resolved in another tab.
    test "accepting in one tab clears the notification in the other", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      {:ok, tab1, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")
      {:ok, tab2, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      tab1 |> element("#accept-invite-#{ctx.lobby_a.id}") |> render_click()

      refute has_element?(tab2, "#notification-invite-#{ctx.lobby_a.id}")
    end

    test "being uninvited while at the table sends the user home with a flash", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      :ok = Lobbies.accept(Scope.for_user(ctx.b), ctx.lobby_a.id)
      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_a.id}")

      :ok = Lobbies.uninvite(Scope.for_user(ctx.a), ctx.lobby_a, ctx.b.id)

      flash = assert_redirect(view_b, ~p"/lobby/#{ctx.lobby_b.id}")
      assert flash["info"] == "You were removed from #{ctx.a.name}'s table"
    end

    test "being uninvited elsewhere replaces the invite with a simple notification", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      :ok = Lobbies.uninvite(Scope.for_user(ctx.a), ctx.lobby_a, ctx.b.id)

      refute has_element?(view_b, "#notification-invite-#{ctx.lobby_a.id}")

      assert has_element?(
               view_b,
               "#notifications [data-notification='simple']",
               "You were removed from #{ctx.a.name}'s table"
             )
    end

    test "a player coming online is announced to others", %{conn: conn} do
      ctx = two_players(conn)
      {:ok, view_a, _} = live(ctx.conn_a, ~p"/lobby/#{ctx.lobby_a.id}")
      Phoenix.PubSub.subscribe(Helios.PubSub, "users:online")

      {:ok, _view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      b_key = to_string(ctx.b.id)
      assert_receive %Broadcast{event: "presence_diff", payload: %{joins: %{^b_key => _}}}

      assert has_element?(
               view_a,
               "#notifications [data-notification='simple']",
               "User #{ctx.b.name} got online!"
             )

      assert has_element?(view_a, "#invite_user_id option[value='#{ctx.b.id}']")
    end

    # Review Focus 1: tampered notification events never crash.
    test "tampered notification events are handled", %{conn: conn} do
      ctx = two_players(conn)
      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      render_hook(view_b, "accept_invite", %{"id" => "not-a-uuid"})
      assert has_element?(view_b, "#flash-error", "That invitation is no longer valid")

      render_hook(view_b, "decline_invite", %{"id" => Ecto.UUID.generate()})
      assert has_element?(view_b, "#flash-error", "That invitation is no longer valid")

      render_hook(view_b, "accept_invite", %{})
      render_hook(view_b, "decline_invite", %{})
      render_hook(view_b, "dismiss_notification", %{})
      assert has_element?(view_b, "#lobby")
    end
  end
end
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cd helios && mix test test/helios_web/notifications_test.exs`
Expected: FAIL — `module HeliosWeb.Notifications is not available`.

- [ ] **Step 3: Implement the hook and pure functions**

Create `helios/lib/helios_web/notifications.ex`:

```elixir
defmodule HeliosWeb.Notifications do
  @moduledoc """
  Per-user notification strip for every authenticated LiveView.

  Two kinds:

    * `:approve` — a pending table invite ("You are expected on table X"),
      persistent until accepted, declined or withdrawn.
    * `:simple` — transient messages with an OK button, auto-expiring after 8 s.

  At most 5 are shown, newest first; approve notifications are never evicted
  to make room for simple ones.
  """
  import Phoenix.Component, only: [assign: 3, update: 3]
  import Phoenix.LiveView

  use HeliosWeb, :verified_routes

  alias Helios.Lobbies

  @max_visible 5
  @simple_ttl_ms 8_000
  @events ~w(accept_invite decline_invite dismiss_notification)

  @type notification :: %{
          id: String.t(),
          kind: :approve | :simple,
          message: String.t(),
          lobby_id: String.t() | nil
        }

  def on_mount(:default, _params, _session, socket) do
    user = socket.assigns.current_scope.user

    if connected?(socket) do
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(user.id))
    end

    notifications = user |> Lobbies.pending_invites() |> Enum.map(&approve/1)

    {:cont,
     socket
     |> assign(:notifications, notifications)
     |> attach_hook(:notifications, :handle_info, &handle_info/2)
     |> attach_hook(:notifications, :handle_event, &handle_event/3)}
  end

  @doc "Adds a transient notification to the socket and schedules its expiry."
  def push_simple(socket, message) do
    notification = simple(message)
    Process.send_after(self(), {:expire_notification, notification.id}, @simple_ttl_ms)
    update(socket, :notifications, &add(&1, notification))
  end

  @spec approve(%{lobby_id: String.t(), owner_name: String.t()}) :: notification()
  def approve(%{lobby_id: lobby_id, owner_name: owner_name}) do
    %{
      id: approve_id(lobby_id),
      kind: :approve,
      message: "You are expected on table #{owner_name}",
      lobby_id: lobby_id
    }
  end

  @spec simple(String.t()) :: notification()
  def simple(message) do
    %{
      id: "simple-#{System.unique_integer([:positive, :monotonic])}",
      kind: :simple,
      message: message,
      lobby_id: nil
    }
  end

  @doc "Prepends (de-duplicating by id), then evicts the oldest simple ones beyond 5."
  @spec add([notification()], notification()) :: [notification()]
  def add(notifications, %{id: id} = notification) do
    evict_simple([notification | Enum.reject(notifications, &(&1.id == id))])
  end

  @spec remove([notification()], String.t()) :: [notification()]
  def remove(notifications, id), do: Enum.reject(notifications, &(&1.id == id))

  @spec visible([notification()]) :: [notification()]
  def visible(notifications), do: Enum.take(notifications, @max_visible)

  defp evict_simple(list) when length(list) <= @max_visible, do: list

  defp evict_simple(list) do
    case list |> Enum.reverse() |> Enum.find_index(&(&1.kind == :simple)) do
      nil -> list
      reverse_index -> list |> List.delete_at(length(list) - 1 - reverse_index) |> evict_simple()
    end
  end

  defp approve_id(lobby_id), do: "invite-#{lobby_id}"

  defp handle_info({:invited, invite}, socket) do
    notification = approve(%{lobby_id: invite.lobby_id, owner_name: invite.lobby.owner.name})
    {:halt, update(socket, :notifications, &add(&1, notification))}
  end

  defp handle_info({:uninvited, lobby_id}, socket) do
    socket = update(socket, :notifications, &remove(&1, approve_id(lobby_id)))
    message = removed_message(Lobbies.owner_name(lobby_id))

    case socket.assigns do
      %{lobby: %{id: ^lobby_id}} ->
        own = Lobbies.get_or_create_own_lobby(socket.assigns.current_scope.user)
        {:halt, socket |> put_flash(:info, message) |> push_navigate(to: ~p"/lobby/#{own.id}")}

      _ ->
        {:halt, push_simple(socket, message)}
    end
  end

  defp handle_info({:invite_resolved, lobby_id}, socket) do
    {:halt, update(socket, :notifications, &remove(&1, approve_id(lobby_id)))}
  end

  defp handle_info({:expire_notification, id}, socket) do
    {:halt, update(socket, :notifications, &remove(&1, id))}
  end

  defp handle_info(_message, socket), do: {:cont, socket}

  defp handle_event("accept_invite", %{"id" => lobby_id}, socket) when is_binary(lobby_id) do
    socket = update(socket, :notifications, &remove(&1, approve_id(lobby_id)))

    case Lobbies.accept(socket.assigns.current_scope, lobby_id) do
      :ok -> {:halt, push_navigate(socket, to: ~p"/lobby/#{lobby_id}")}
      {:error, reason} -> {:halt, put_flash(socket, :error, Lobbies.error_message(reason))}
    end
  end

  defp handle_event("decline_invite", %{"id" => lobby_id}, socket) when is_binary(lobby_id) do
    socket = update(socket, :notifications, &remove(&1, approve_id(lobby_id)))

    case Lobbies.decline(socket.assigns.current_scope, lobby_id) do
      :ok -> {:halt, socket}
      {:error, reason} -> {:halt, put_flash(socket, :error, Lobbies.error_message(reason))}
    end
  end

  defp handle_event("dismiss_notification", %{"id" => id}, socket) when is_binary(id) do
    {:halt, update(socket, :notifications, &remove(&1, id))}
  end

  # Malformed params for our own events: swallow, never crash the LiveView.
  defp handle_event(event, _params, socket) when event in @events, do: {:halt, socket}

  defp handle_event(_event, _params, socket), do: {:cont, socket}

  defp removed_message(nil), do: "You were removed from a table"
  defp removed_message(owner_name), do: "You were removed from #{owner_name}'s table"
end
```

- [ ] **Step 4: Render the strip from `Layouts.app`**

In `helios/lib/helios_web/components/layouts.ex`:

1. Next to the existing `attr :current_scope, ...` of `app/1`, add:

```elixir
  attr :notifications, :list,
    default: [],
    doc: "notifications from HeliosWeb.Notifications (authenticated pages)"
```

2. Inside the `app/1` template, directly after `<.site_header ... />` (before `<main>`), add:

```heex
<.notifications items={@notifications} />
```

3. Add this function component to the module (below `app/1`):

```elixir
  @doc """
  Teal notification strip. Approve notifications carry Accept/Decline; simple
  ones an OK button. Events are handled by `HeliosWeb.Notifications`.
  """
  attr :items, :list, required: true

  def notifications(assigns) do
    ~H"""
    <div
      id="notifications"
      aria-live="polite"
      class="pointer-events-none fixed inset-x-0 top-4 z-50 flex flex-col items-center gap-2 px-4"
    >
      <div
        :for={n <- HeliosWeb.Notifications.visible(@items)}
        id={"notification-#{n.id}"}
        data-notification={Atom.to_string(n.kind)}
        class="pointer-events-auto flex w-full max-w-xl items-center justify-between gap-4 rounded-lg bg-teal-600 px-4 py-2 text-white shadow-lg ring-1 ring-teal-900/30 transition"
      >
        <span class="text-sm font-medium">{n.message}</span>
        <div class="flex shrink-0 gap-2">
          <%= if n.kind == :approve do %>
            <button
              id={"accept-invite-#{n.lobby_id}"}
              type="button"
              phx-click="accept_invite"
              phx-value-id={n.lobby_id}
              class="rounded-md bg-white px-3 py-1 text-sm font-semibold text-teal-800 transition hover:bg-teal-50"
            >
              Accept
            </button>
            <button
              id={"decline-invite-#{n.lobby_id}"}
              type="button"
              phx-click="decline_invite"
              phx-value-id={n.lobby_id}
              class="rounded-md bg-zinc-900 px-3 py-1 text-sm font-semibold text-white transition hover:bg-zinc-700"
            >
              Decline
            </button>
          <% else %>
            <button
              id={"dismiss-#{n.id}"}
              type="button"
              phx-click="dismiss_notification"
              phx-value-id={n.id}
              class="rounded-md bg-zinc-900 px-3 py-1 text-sm font-semibold text-white transition hover:bg-zinc-700"
            >
              OK
            </button>
          <% end %>
        </div>
      </div>
    </div>
    """
  end
```

- [ ] **Step 5: Wire the hook into the router**

In `helios/lib/helios_web/router.ex`, change the authenticated live_session header to:

```elixir
    live_session :authenticated,
      on_mount: [{HeliosWeb.UserAuth, :require_user}, HeliosWeb.Notifications] do
```

- [ ] **Step 6: Emit lobby notifications from LobbyLive**

In `helios/lib/helios_web/live/lobby_live.ex`:

1. Add `alias HeliosWeb.Notifications` next to the other aliases.
2. Change the first template line to:

```heex
    <Layouts.app flash={@flash} current_scope={@current_scope} notifications={@notifications}>
```

3. Replace the three `handle_info` clauses for the online presence diff, `{:confirm_offline, ...}` and `{:declined, _user}` with:

```elixir
  def handle_info(%Broadcast{event: "presence_diff", topic: @online_topic, payload: diff}, socket) do
    {online, events} = OnlineTracker.handle_diff(socket.assigns.online, diff)
    {:noreply, socket |> assign(:online, online) |> notify_presence(events) |> assign_derived()}
  end
```

```elixir
  def handle_info({:confirm_offline, user_id, token}, socket) do
    {online, events} =
      OnlineTracker.confirm_offline(
        socket.assigns.online,
        user_id,
        token,
        Presence.list(@online_topic)
      )

    {:noreply, socket |> assign(:online, online) |> notify_presence(events) |> assign_derived()}
  end
```

```elixir
  def handle_info({:declined, user}, socket) do
    socket = load_members(socket)

    socket =
      if socket.assigns.owner?,
        do: Notifications.push_simple(socket, "User #{user.name} declined your invitation!"),
        else: socket

    {:noreply, socket}
  end
```

(Keep the lobby-topic `presence_diff` clause between the online-diff clause and the confirm clause, unchanged.)

4. Add this private helper below `invite_form/1`:

```elixir
  defp notify_presence(socket, events) do
    me = socket.assigns.current_scope.user.id

    Enum.reduce(events, socket, fn
      {_kind, ^me, _name}, acc -> acc
      {:came_online, _id, name}, acc -> Notifications.push_simple(acc, "User #{name} got online!")
      {:went_offline, _id, name}, acc -> Notifications.push_simple(acc, "User #{name} got offline!")
    end)
  end
```

- [ ] **Step 7: Run the tests to verify they pass**

Run: `cd helios && mix test test/helios_web/notifications_test.exs test/helios_web/live/lobby_live_test.exs`
Expected: PASS.

- [ ] **Step 8: Run precommit**

Run: `cd helios && mix precommit`
Expected: PASS (whole suite, no warnings; `grep -rn daisy helios/lib` still empty).

- [ ] **Step 9: Commit**

```bash
git add helios/lib/helios_web/notifications.ex helios/lib/helios_web/components/layouts.ex \
  helios/lib/helios_web/router.ex helios/lib/helios_web/live/lobby_live.ex \
  helios/test/helios_web/notifications_test.exs
git status --short
git commit -m "feat(lobby): add notifications hook for invites and presence events

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 6: Playwright lobby helpers and presence scenarios (2–4)

**Files:**
- Create: `e2e/tests/support/lobby.ts`
- Test: `e2e/tests/presence.spec.ts`

**Interfaces:**
- Consumes: `uniqueName`, `login`, `logout`, `LOBBY_URL` from `e2e/tests/support/auth.ts`; the DOM contract of Tasks 4–5; e2e grace 2000 ms (Task 3).
- Produces (`e2e/tests/support/lobby.ts`): `GRACE_MS = 2_000`; `type Player = { context, page, name, lobbyUrl }`; Playwright `test` extended with fixture `player(prefix) => Promise<Player>` (contexts closed after each test); re-exported `expect`; helpers `waitForLiveView(page)`, `memberRows(page)`, `memberRow(page, name)`, `freeRows(page)`, `inviteOption(page, name)`, `invite(page, name)`, `notification(page, text)`, `acceptInvite(page, ownerName)`, `declineInvite(page, ownerName)`, `removeMember(page, name)`.

- [ ] **Step 1: Write the helpers**

Create `e2e/tests/support/lobby.ts`:

```ts
import {
  test as base,
  expect,
  type BrowserContext,
  type Locator,
  type Page,
} from "@playwright/test";
import { LOBBY_URL, login, uniqueName } from "./auth";

export { expect };

/** Must match `config :helios, presence_grace_ms` in helios/config/e2e.exs. */
export const GRACE_MS = 2_000;

export type Player = {
  context: BrowserContext;
  page: Page;
  name: string;
  lobbyUrl: string;
};

function exact(text: string): RegExp {
  return new RegExp(`^\\s*${text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\s*$`);
}

export async function waitForLiveView(page: Page): Promise<void> {
  await expect(page.locator("[data-phx-main].phx-connected")).toHaveCount(1);
}

export const test = base.extend<{ player: (prefix: string) => Promise<Player> }>({
  player: async ({ browser }, use) => {
    const created: Player[] = [];

    await use(async (prefix: string) => {
      const context = await browser.newContext();
      const page = await context.newPage();
      const name = uniqueName(prefix);
      await login(page, name);
      await expect(page).toHaveURL(LOBBY_URL);
      await waitForLiveView(page);
      const player = { context, page, name, lobbyUrl: page.url() };
      created.push(player);
      return player;
    });

    for (const { context } of created) {
      await context.close().catch(() => undefined);
    }
  },
});

export function memberRows(page: Page): Locator {
  return page.locator("#members-table tbody tr[data-member]");
}

export function memberRow(page: Page, name: string): Locator {
  return page.locator(`#members-table tbody tr[data-member][data-name="${name}"]`);
}

export function freeRows(page: Page): Locator {
  return page.locator("#members-table tbody tr[data-free-slot]");
}

export function inviteOption(page: Page, name: string): Locator {
  return page.locator("#invite_user_id option", { hasText: exact(name) });
}

export async function invite(page: Page, name: string): Promise<void> {
  await page.locator("#invite_user_id").selectOption({ label: name });
  await expect(page.locator("#invite-button")).toBeEnabled();
  await page.locator("#invite-button").click();
  await expect(memberRow(page, name)).toHaveCount(1);
}

export function notification(page: Page, text: string): Locator {
  return page.locator("#notifications [data-notification]", { hasText: text });
}

export async function acceptInvite(page: Page, ownerName: string): Promise<void> {
  await notification(page, `You are expected on table ${ownerName}`)
    .getByRole("button", { name: "Accept" })
    .click();
}

export async function declineInvite(page: Page, ownerName: string): Promise<void> {
  await notification(page, `You are expected on table ${ownerName}`)
    .getByRole("button", { name: "Decline" })
    .click();
}

export async function removeMember(page: Page, name: string): Promise<void> {
  await page.getByRole("button", { name: `Remove ${name}` }).click();
}
```

- [ ] **Step 2: Write the presence scenarios**

Create `e2e/tests/presence.spec.ts`:

```ts
import { GRACE_MS, expect, inviteOption, notification, test, waitForLiveView } from "./support/lobby";

test("2. online users: A sees B come online, both can invite each other", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");

  await expect(notification(a.page, `User ${b.name} got online!`)).toBeVisible();
  await expect(inviteOption(a.page, b.name)).toHaveCount(1);
  await expect(inviteOption(b.page, a.name)).toHaveCount(1);
});

test("3. offline users: B's browser closes, A is told after the grace period", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");
  await expect(inviteOption(a.page, b.name)).toHaveCount(1);

  await b.context.close();

  await expect(notification(a.page, `User ${b.name} got offline!`)).toBeVisible({
    timeout: GRACE_MS + 5_000,
  });
  await expect(inviteOption(a.page, b.name)).toHaveCount(0);
});

test("4. reconnect: B reloads, A gets no online/offline notification", async ({ player }) => {
  // B first, so A starts with B already online and gets no initial "got online".
  const b = await player("b");
  const a = await player("a");
  await expect(inviteOption(a.page, b.name)).toHaveCount(1);

  await b.page.reload();
  await waitForLiveView(b.page);
  await a.page.waitForTimeout(2 * GRACE_MS);

  await expect(notification(a.page, b.name)).toHaveCount(0);
  await expect(inviteOption(a.page, b.name)).toHaveCount(1);
});
```

- [ ] **Step 3: Run the presence scenarios**

Run: `cd e2e && npx playwright test tests/presence.spec.ts`
Expected: 3 passed. (The behaviour was built in Tasks 3–5; a failure here is an application bug — debug the app, do not weaken the assertion.)

- [ ] **Step 4: Commit**

```bash
git add e2e/tests/support/lobby.ts e2e/tests/presence.spec.ts
git status --short
git commit -m "test(e2e): add lobby helpers and presence scenarios

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 7: Playwright lobby scenarios (1, 5–12)

**Files:**
- Test: `e2e/tests/lobby.spec.ts`

**Interfaces:**
- Consumes: everything exported by `e2e/tests/support/lobby.ts` (Task 6) and `login`, `logout`, `LOBBY_URL` from `e2e/tests/support/auth.ts`.
- Produces: nothing downstream.

- [ ] **Step 1: Write the lobby scenarios**

Create `e2e/tests/lobby.spec.ts`:

```ts
import { LOBBY_URL, login, logout } from "./support/auth";
import {
  acceptInvite,
  declineInvite,
  expect,
  freeRows,
  invite,
  inviteOption,
  memberRow,
  memberRows,
  notification,
  removeMember,
  test,
  type Player,
} from "./support/lobby";

async function joinTable(owner: Player, guest: Player): Promise<void> {
  await invite(owner.page, guest.name);
  await acceptInvite(guest.page, owner.name);
  await expect(guest.page).toHaveURL(owner.lobbyUrl);
  await expect(memberRow(owner.page, guest.name)).toHaveAttribute("data-connected", "true");
}

test("1. lobby connection: own table shows only me, leader and connected", async ({ player }) => {
  const a = await player("a");

  await expect(memberRows(a.page)).toHaveCount(1);
  const me = memberRow(a.page, a.name);
  await expect(me).toHaveAttribute("data-leader", "true");
  await expect(me).toHaveAttribute("data-self", "true");
  await expect(me).toHaveAttribute("data-connected", "true");
  await expect(freeRows(a.page)).toHaveCount(6);
});

test("5. invite users", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");

  await invite(a.page, b.name);

  await expect(memberRows(a.page)).toHaveCount(2);
  await expect(memberRow(a.page, a.name)).toHaveAttribute("data-leader", "true");
  await expect(memberRow(a.page, b.name)).toHaveAttribute("data-leader", "false");
  await expect(memberRow(a.page, b.name)).toHaveAttribute("data-connected", "false");
  await expect(notification(b.page, `You are expected on table ${a.name}`)).toBeVisible();
  await expect(memberRows(b.page)).toHaveCount(1);
  await expect(memberRow(b.page, b.name)).toHaveAttribute("data-leader", "true");
});

test("6. accept invitation", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");

  await invite(a.page, b.name);
  await acceptInvite(b.page, a.name);

  await expect(b.page).toHaveURL(a.lobbyUrl);
  await expect(memberRow(a.page, b.name)).toHaveAttribute("data-connected", "true");
  await expect(memberRows(b.page)).toHaveCount(2);
  await expect(memberRow(b.page, a.name)).toHaveAttribute("data-leader", "true");
  await expect(memberRow(b.page, b.name)).toHaveAttribute("data-self", "true");
  await expect(notification(b.page, `You are expected on table ${a.name}`)).toHaveCount(0);
});

test("7. decline invitation", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");

  await invite(a.page, b.name);
  await declineInvite(b.page, a.name);

  await expect(notification(a.page, `User ${b.name} declined your invitation!`)).toBeVisible();
  await expect(memberRows(a.page)).toHaveCount(1);
  await expect(notification(b.page, `You are expected on table ${a.name}`)).toHaveCount(0);
});

test("8. unauthorized invite: a guest sees no invite controls", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");
  await joinTable(a, b);

  await expect(b.page.locator("#invite-form")).toHaveCount(0);
  await expect(b.page.getByRole("button", { name: /^Remove / })).toHaveCount(0);
});

test("9. return to own lobby", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");
  await joinTable(a, b);

  await b.page.getByRole("link", { name: "My table" }).click();

  await expect(b.page).toHaveURL(b.lobbyUrl);
  await expect(memberRows(b.page)).toHaveCount(1);
  await expect(memberRow(b.page, b.name)).toHaveAttribute("data-leader", "true");
  await expect(memberRow(b.page, b.name)).toHaveAttribute("data-connected", "true");
  // B left A's table but is still invited (leaving never removes authorization)...
  await expect(memberRow(a.page, b.name)).toHaveAttribute("data-connected", "false");
  // ...and still online: once removed from the table, B is offered in A's invite select.
  await removeMember(a.page, b.name);
  await expect(inviteOption(a.page, b.name)).toHaveCount(1);
  await expect(notification(a.page, `User ${b.name} got offline!`)).toHaveCount(0);
});

test("10. direct URL without invite redirects to own lobby", async ({ player }) => {
  const a = await player("a");
  const c = await player("c");

  await c.page.goto(a.lobbyUrl);

  await expect(c.page).toHaveURL(c.lobbyUrl);
  await expect(c.page.getByText("Only invited players can join this table")).toBeVisible();
});

test("11. uninvite while connected sends the guest home", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");
  await joinTable(a, b);

  await removeMember(a.page, b.name);

  await expect(b.page).toHaveURL(b.lobbyUrl);
  await expect(b.page.getByText(`You were removed from ${a.name}'s table`)).toBeVisible();
  await expect(memberRows(a.page)).toHaveCount(1);
});

test("12. an invite sent while logged out is shown at next login", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");
  await expect(inviteOption(a.page, b.name)).toHaveCount(1);

  await logout(b.page);
  // B is gone; A still lists B during the presence grace period, so invite now.
  await invite(a.page, b.name);
  await expect(memberRow(a.page, b.name)).toHaveAttribute("data-connected", "false");

  await login(b.page, b.name);
  await expect(b.page).toHaveURL(LOBBY_URL);
  await expect(notification(b.page, `You are expected on table ${a.name}`)).toBeVisible();
});
```

- [ ] **Step 2: Run the lobby scenarios**

Run: `cd e2e && npx playwright test tests/lobby.spec.ts`
Expected: 9 passed. (A failure here is an application bug — debug the app, do not weaken the assertion.)

- [ ] **Step 3: Run the full e2e suite**

Run: `cd e2e && npx playwright test`
Expected: all specs pass (`auth.spec.ts`, `presence.spec.ts`, `lobby.spec.ts`).

- [ ] **Step 4: Commit**

```bash
git add e2e/tests/lobby.spec.ts
git status --short
git commit -m "test(e2e): port legacy lobby scenarios to Playwright

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

### Task 8: Delete the legacy Go backend, websocket client, Jest tests and their CI jobs

**Files:**
- Delete (tracked files): `backend_old/`, `websocket-client/`, `integration-tests/`
- Modify: `.github/workflows/ci.yml`

**Interfaces:**
- Consumes: green `mix precommit` and green Playwright suite (Tasks 1–7).
- Produces: repository without the legacy lobby stack; CI jobs `core`, `helios`, `e2e` only (`frontend/` stays on disk until Phase 4 as the asset source, but no longer builds in CI).

- [ ] **Step 1: Verify everything is green before deleting**

Run: `cd helios && mix precommit`
Expected: PASS.
Run: `cd e2e && npx playwright test`
Expected: all passed.

- [ ] **Step 2: Remove the tracked legacy directories**

Run from the repository root:

```bash
git rm -r -q backend_old websocket-client integration-tests
```

Expected: exits 0. Only tracked files are removed; any ignored leftovers on disk (e.g. `node_modules/`, `websocket-client/build`) are left alone — do not `rm -rf` them.

- [ ] **Step 3: Remove the legacy CI jobs**

In `.github/workflows/ci.yml`, delete the three job entries under `jobs:` in full — each spans from its 2-space-indented key line up to (not including) the next 2-space-indented key or the end of the file:
- `old_backend:` (name "Old Backend", `working-directory: backend_old`, `go test ./...`)
- `frontend:` (name "Frontend", `working-directory: frontend`, `elm-app build` / `elm-app test`)
- `integration-tests:` (name "Integration Tests", `working-directory: integration-tests`, `needs: [old_backend]`)

Leave `core`, `helios` and `e2e` untouched.

Run: `grep -nE "old_backend|integration-tests|backend_old|websocket-client|elm|working-directory: frontend" .github/workflows/ci.yml`
Expected: no output.
Run: `grep -nE "^  [a-z_-]+:$" .github/workflows/ci.yml`
Expected: exactly the `core:`, `helios:` and `e2e:` job keys.

- [ ] **Step 4: Check nothing still depends on the deleted directories**

Run: `grep -rnE "backend_old|websocket-client|integration-tests" helios/lib helios/test helios/config helios/mix.exs e2e --exclude-dir=node_modules`
Expected: no output. (Remaining mentions in `README.md`, `.gitignore`, `frontend/` and `docs/` are cleaned up by Phases 4–5.)

- [ ] **Step 5: Stage the CI change and verify the index**

```bash
git add .github/workflows/ci.yml
git diff --cached --name-only | grep -vE '^(backend_old|websocket-client|integration-tests)/|^\.github/workflows/ci\.yml$'
```

Expected: the second command prints nothing (only the deletions and `ci.yml` are staged). `git status --short` shows the untracked files untouched.

- [ ] **Step 6: Commit**

```bash
git commit -m "chore: remove legacy Go backend, websocket client and Jest tests

The lobby is now served by Helios (LobbyLive + PubSub + Presence) and covered
by Playwright. Drop the old_backend, integration-tests and frontend CI jobs;
frontend/ stays on disk as the asset source until Phase 4.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>"
```

---

## Self-Review Notes

- **Spec coverage:** data model (Task 1), full context incl. every error atom, capacity, ordering and broadcasts (Tasks 1–2), OnlineTracker with configurable grace (Task 3), LobbyLive owner/guest views, events, presence, "My table", `/` redirect, stub removal, Phase 1 e2e URL updates (Task 4), Notifications hook/component, approve/simple kinds, 5-cap, 8 s expiry, declined/online/offline/removed messages, uninvite navigation (Task 5), all 12 Playwright scenarios (Tasks 6–7), deletions + CI (Task 8).
- **Deliberate resolutions of spec ambiguities** (also listed in the hand-off summary):
  1. Scenario 9 says "A's invite select still contains B (online)", but the select excludes members and B stays a member after leaving. The test proves B is still online by removing B from A's table and then asserting B appears in A's select, plus no "got offline" notification.
  2. `{:uninvited, lobby_id}` is handled entirely by the Notifications hook (halted); it navigates when the current LiveView's `@lobby.id` matches, so non-lobby LiveViews (Phase 4 GameLive) never need a clause for it.
  3. The removal message needs the owner's name, which `{:uninvited, lobby_id}` does not carry → `Lobbies.owner_name/1` added.
  4. The grace timer message is `{:confirm_offline, user_id, token}` (token = `make_ref()`), not `{:confirm_offline, user_id}`, so stale timers are ignored; events carry `{kind, user_id, name}`.
  5. Scenario 12: the UI can only invite users in the online list, so the test invites B during the presence grace window right after B logs out.
  6. `accept/2` is idempotent for accepted rows; `decline/2` only affects pending rows (a stale Decline cannot kick a player who already accepted in another tab).
  7. FREE padding only in the owner view (guest view: connected members only, no padding). The self row shows both the double-chevron and the crown when self is the leader (scenario 1 expects the crown on the owner's own row).
  8. "My table" links to `/` (which redirects to the own table) so the header needs no DB lookup.
