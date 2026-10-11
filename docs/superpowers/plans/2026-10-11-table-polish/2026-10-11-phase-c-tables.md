# Table Polish — Phase C: Recent Tables and Invite Auto-Accept Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A "Recent tables" header menu lists up to 5 friends' tables the user can still enter, and opening a table (or a game played at it) with a pending invite accepts that invite and clears its notification in every tab.

**Architecture:** One migration adds `lobby_invites.last_visited_at`. `Helios.Lobbies` gains `touch_visit/2`, `accept_pending/2` (delegates to `accept/2` only when the invite is pending) and `recent_tables/1` (a portable query with a grouped subquery over the user's games; the max-of-two, ordering and limit are done in Elixir). `HeliosWeb.Notifications` — the `on_mount` that already loads notifications for every authenticated LiveView — also assigns `recent_tables` and refreshes it on `{:invite_resolved, _}` / `{:uninvited, _}`; every LiveView passes it to `Layouts.app` exactly like `notifications`. `LobbyLive` (connected, non-owner) and `GameLive` (connected) "arrive": they call `accept_pending/2`, whose existing `{:invite_resolved, lobby_id}` broadcast removes the banner through the existing Notifications hook.

**Tech Stack:** Phoenix 1.8 / LiveView 1.1, Ecto (SQLite, portable queries), Tailwind 4.1, ExUnit + LazyHTML, Playwright.

**Spec:** `docs/superpowers/specs/2026-10-11-table-polish/2026-10-11-table-polish-design.md` (sections "E — Recent tables menu" and "F — Arriving at a table accepts its invite").

## Global Constraints

- Read `helios/AGENTS.md` before touching `helios/`.
- Exactly one migration in this phase, created with `mix ecto.gen.migration` (never hand-named); nullable column, no backfill.
- Ecto queries stay portable: no SQLite-only SQL, no `fragment/1`. Only one active game per lobby exists (unique partial index): tests that put several games in one lobby must mark the earlier ones `finished` first.
- Hidden information is unaffected: the menu shows only the owner's name and the lobby id, which the user already sees in the invite banner.
- Kept ids: `#site-header`, `#current-user-name`, `#past-games-link`, `#my-table-link`, `#logout-link`, `#notifications`, `#notification-invite-<lobby_id>`, `#accept-invite-<lobby_id>`, `#decline-invite-<lobby_id>`. New: `#recent-tables` (wrapper), `#recent-tables-button`, `#recent-tables-menu`, `#recent-table-<lobby_id>`.
- No accept and no `touch_visit` on the disconnected (dead) render — only `connected?(socket)` mounts write.
- Decline is unchanged. Accepting an already-accepted invite again must not happen on arrival (no repeated `{:invite_resolved}` broadcasts).
- Touch targets ≥ 44×44 CSS px; no horizontal scroll at ≥ 360px; `responsive.spec.ts` must stay green.
- Stage explicit paths only; never stage `helios/config/config.exs` / `helios/config/dev.exs`; never `git add -A` / `git add .`.
- `cd helios && mix precommit` before each commit.

## Review Focus

- Opening a table whose invite is already accepted (or the own table) must not call `accept/2` again: no `{:invite_resolved}` broadcast, no write except `last_visited_at`. (Tests in Task 1 and Task 4.)
- The disconnected HTTP render of `/lobby/:id` and `/game/:id` must leave a pending invite pending and `last_visited_at` nil. (Test in Task 4.)
- `recent_tables/1` lists only lobbies with an *accepted* invite (pending and uninvited excluded, the own lobby excluded even when the user played there), and a game the user was not seated in never bumps `last_active_at`. (Tests in Task 2.)
- Arriving via `LobbyLive` and via `GameLive` removes `#accept-invite-<id>` both in the arriving LiveView and in another already-open tab of the same user. (Tests in Task 4 and Task 5.)
- The Recent tables button is absent when the list is empty and on a 360px touch phone the header keeps `#recent-tables-button`, `#my-table-link` and `#logout-link` fully in the viewport with ≥ 44px targets. (Tests in Task 3 and Task 6.)

---

### Task 1: `last_visited_at`, `touch_visit/2`, `accept_pending/2`

**Files:**
- Create: `helios/priv/repo/migrations/<timestamp>_add_last_visited_at_to_lobby_invites.exs` (generated)
- Modify: `helios/lib/helios/lobbies/invite.ex`, `helios/lib/helios/lobbies.ex`
- Test: `helios/test/helios/lobbies_test.exs`

**Interfaces:**
- Produces: column `lobby_invites.last_visited_at :utc_datetime` (nullable); schema field `Invite.last_visited_at`.
- Produces: `Lobbies.touch_visit(Scope.t(), term()) :: :ok` — stamps `last_visited_at = now` on the scope user's invite for that lobby; `:ok` also for a malformed id or a missing invite (no-op).
- Produces: `Lobbies.accept_pending(Scope.t(), term()) :: :ok | :none` — when the user's invite for that lobby is `"pending"`, calls `accept/2` (which broadcasts `{:invite_resolved, lobby_id}` on `user:<id>`) and returns `:ok`; otherwise (accepted, missing, malformed id) returns `:none` and broadcasts nothing.

- [ ] **Step 1: Write the failing tests**

Append to `helios/test/helios/lobbies_test.exs`, before the final `end`:

```elixir
  describe "touch_visit/2" do
    test "stamps last_visited_at on the user's invite for that lobby only" do
      guest = player_fixture()
      first = owner_and_lobby()
      second = owner_and_lobby()
      invite_fixture(first.lobby, first.owner, guest)
      invite_fixture(second.lobby, second.owner, guest)

      assert Repo.get_by!(Invite, lobby_id: first.lobby.id, user_id: guest.id).last_visited_at ==
               nil

      assert Lobbies.touch_visit(Scope.for_user(guest), first.lobby.id) == :ok

      assert %DateTime{} =
               Repo.get_by!(Invite, lobby_id: first.lobby.id, user_id: guest.id).last_visited_at

      assert Repo.get_by!(Invite, lobby_id: second.lobby.id, user_id: guest.id).last_visited_at ==
               nil
    end

    test "is a no-op without an invite or with a malformed id" do
      %{lobby: lobby, scope: owner_scope} = owner_and_lobby()
      stranger = Scope.for_user(player_fixture())

      assert Lobbies.touch_visit(stranger, lobby.id) == :ok
      assert Lobbies.touch_visit(owner_scope, lobby.id) == :ok
      assert Lobbies.touch_visit(stranger, "not-a-uuid") == :ok
      assert Lobbies.touch_visit(stranger, nil) == :ok
    end
  end

  describe "accept_pending/2" do
    test "accepts a pending invite and broadcasts :invite_resolved" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(guest.id))

      assert Lobbies.accept_pending(Scope.for_user(guest), lobby.id) == :ok

      lobby_id = lobby.id
      assert_receive {:invite_resolved, ^lobby_id}
      assert Repo.get_by!(Invite, lobby_id: lobby.id, user_id: guest.id).status == "accepted"
    end

    # Review Focus 1: an accepted invite is never re-accepted on arrival.
    test "does nothing for an accepted, missing or malformed invite" do
      %{owner: owner, lobby: lobby, scope: owner_scope} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      :ok = Lobbies.accept(Scope.for_user(guest), lobby.id)
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(guest.id))
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(owner.id))

      assert Lobbies.accept_pending(Scope.for_user(guest), lobby.id) == :none
      assert Lobbies.accept_pending(owner_scope, lobby.id) == :none
      assert Lobbies.accept_pending(Scope.for_user(guest), "not-a-uuid") == :none
      assert Lobbies.accept_pending(Scope.for_user(guest), nil) == :none
      refute_receive {:invite_resolved, _}
    end
  end
```

- [ ] **Step 2: Run to verify they fail**

```bash
cd helios && mix test test/helios/lobbies_test.exs
```

Expected: FAIL — `Invite` has no `last_visited_at` field / `Lobbies.touch_visit/2` and `Lobbies.accept_pending/2` are undefined.

- [ ] **Step 3: Generate and fill the migration**

```bash
cd helios && mix ecto.gen.migration add_last_visited_at_to_lobby_invites
```

Expected: `* creating priv/repo/migrations/<timestamp>_add_last_visited_at_to_lobby_invites.exs`. Replace the generated `change/0` body so the file reads:

```elixir
defmodule Helios.Repo.Migrations.AddLastVisitedAtToLobbyInvites do
  use Ecto.Migration

  def change do
    alter table(:lobby_invites) do
      add :last_visited_at, :utc_datetime
    end
  end
end
```

Then migrate the dev database (the `test` alias migrates the test database itself):

```bash
cd helios && mix ecto.migrate
```

- [ ] **Step 4: Schema field**

In `helios/lib/helios/lobbies/invite.ex`, add the field after `status`:

```elixir
  schema "lobby_invites" do
    belongs_to :lobby, Lobby, type: :binary_id
    belongs_to :user, User
    field :status, :string, default: "pending"
    field :last_visited_at, :utc_datetime

    timestamps(type: :utc_datetime)
  end
```

- [ ] **Step 5: `touch_visit/2` and `accept_pending/2`**

In `helios/lib/helios/lobbies.ex`, directly after `accept/2`:

```elixir
  @doc """
  Accepts the invite only while it is pending (opening the table counts as
  accepting it). Already accepted, missing or malformed: `:none`, no broadcast.
  """
  @spec accept_pending(Scope.t(), term()) :: :ok | :none
  def accept_pending(%Scope{user: %User{id: user_id}} = scope, lobby_id) do
    with {:ok, lobby_id} <- cast_lobby_id(lobby_id),
         true <-
           Repo.exists?(
             from i in Invite,
               where:
                 i.lobby_id == ^lobby_id and i.user_id == ^user_id and i.status == "pending"
           ),
         :ok <- accept(scope, lobby_id) do
      :ok
    else
      _ -> :none
    end
  end

  @doc "Records that the invitee opened the table (feeds `recent_tables/1`). Always `:ok`."
  @spec touch_visit(Scope.t(), term()) :: :ok
  def touch_visit(%Scope{user: %User{id: user_id}}, lobby_id) do
    with {:ok, lobby_id} <- cast_lobby_id(lobby_id) do
      Repo.update_all(
        from(i in Invite, where: i.lobby_id == ^lobby_id and i.user_id == ^user_id),
        set: [last_visited_at: now()]
      )
    end

    :ok
  end
```

- [ ] **Step 6: Run and commit**

```bash
cd helios && mix test test/helios/lobbies_test.exs && mix precommit
```

Expected: PASS.

```bash
git add helios/priv/repo/migrations/*_add_last_visited_at_to_lobby_invites.exs helios/lib/helios/lobbies/invite.ex helios/lib/helios/lobbies.ex helios/test/helios/lobbies_test.exs
git commit -m "feat(lobbies): record table visits and accept pending invites on arrival" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 2: `Lobbies.recent_tables/1`

**Files:**
- Modify: `helios/lib/helios/lobbies.ex`
- Test: `helios/test/helios/lobbies_test.exs`

**Interfaces:**
- Consumes: `Invite.last_visited_at` (Task 1); `Helios.Games.Game` (`lobby_id`, `inserted_at`), `Helios.Games.GamePlayer` (`game_id`, `user_id`); test helper `Helios.GamesFixtures.game_fixture/2` (called fully qualified — importing it would clash with `LobbiesFixtures.player_fixture/1`).
- Produces: `Lobbies.recent_tables(User.t()) :: [%{lobby_id: Ecto.UUID.t(), owner_name: String.t(), last_active_at: DateTime.t() | nil}]` — lobbies where the user holds an `"accepted"` invite, own lobby excluded; `last_active_at` = the later of `last_visited_at` and the latest `games.inserted_at` among games in that lobby with a `game_players` row for the user; ordered by `last_active_at` desc, nils last, ties by newest invite first; at most 5.

- [ ] **Step 1: Write the failing tests**

Add `alias Helios.Games.Game` to the alias block at the top of `helios/test/helios/lobbies_test.exs`, then append before the final `end`:

```elixir
  describe "recent_tables/1" do
    defp table_for(guest, accept? \\ true) do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      invite_fixture(lobby, owner, guest)
      if accept?, do: :ok = Lobbies.accept(Scope.for_user(guest), lobby.id)
      %{owner: owner, lobby: lobby}
    end

    defp visited(%{lobby: lobby}, guest, at) do
      Repo.update_all(
        from(i in Invite, where: i.lobby_id == ^lobby.id and i.user_id == ^guest.id),
        set: [last_visited_at: at]
      )
    end

    # Finished, so several games may share one lobby.
    defp played(%{lobby: lobby}, users, at) do
      game = Helios.GamesFixtures.game_fixture(users, lobby: lobby)

      Repo.update_all(from(g in Game, where: g.id == ^game.id),
        set: [inserted_at: at, status: "finished"]
      )
    end

    test "orders by the later of last visit and last game played, never-active last" do
      guest = player_fixture()
      visit_only = table_for(guest)
      play_only = table_for(guest)
      both = table_for(guest)
      idle = table_for(guest)

      visited(visit_only, guest, ~U[2026-10-01 10:00:00Z])
      played(play_only, [play_only.owner, guest], ~U[2026-10-03 10:00:00Z])
      visited(both, guest, ~U[2026-09-01 10:00:00Z])
      played(both, [both.owner, guest], ~U[2026-09-15 10:00:00Z])
      played(both, [both.owner, guest], ~U[2026-10-02 10:00:00Z])

      assert Lobbies.recent_tables(guest) == [
               %{
                 lobby_id: play_only.lobby.id,
                 owner_name: play_only.owner.name,
                 last_active_at: ~U[2026-10-03 10:00:00Z]
               },
               %{
                 lobby_id: both.lobby.id,
                 owner_name: both.owner.name,
                 last_active_at: ~U[2026-10-02 10:00:00Z]
               },
               %{
                 lobby_id: visit_only.lobby.id,
                 owner_name: visit_only.owner.name,
                 last_active_at: ~U[2026-10-01 10:00:00Z]
               },
               %{lobby_id: idle.lobby.id, owner_name: idle.owner.name, last_active_at: nil}
             ]
    end

    # Review Focus 3.
    test "pending, uninvited and own tables are excluded" do
      guest = player_fixture()
      _pending = table_for(guest, false)
      removed = table_for(guest)
      :ok = Lobbies.uninvite(Scope.for_user(removed.owner), removed.lobby, guest.id)
      own = %{lobby: lobby_fixture(guest)}
      played(own, [guest, player_fixture()], ~U[2026-10-05 10:00:00Z])

      assert Lobbies.recent_tables(guest) == []
    end

    # Review Focus 3.
    test "a game the user was not seated in does not count" do
      guest = player_fixture()
      table = table_for(guest)
      visited(table, guest, ~U[2026-10-01 10:00:00Z])
      played(table, [table.owner, player_fixture()], ~U[2026-10-09 10:00:00Z])

      assert [%{last_active_at: ~U[2026-10-01 10:00:00Z]}] = Lobbies.recent_tables(guest)
    end

    test "keeps the 5 most recent" do
      guest = player_fixture()

      tables =
        for day <- 1..6 do
          table = table_for(guest)
          visited(table, guest, DateTime.new!(Date.new!(2026, 10, day), ~T[10:00:00], "Etc/UTC"))
          table
        end

      expected = tables |> Enum.reverse() |> Enum.take(5) |> Enum.map(& &1.lobby.id)
      assert Enum.map(Lobbies.recent_tables(guest), & &1.lobby_id) == expected
    end
  end
```

- [ ] **Step 2: Run to verify they fail**

```bash
cd helios && mix test test/helios/lobbies_test.exs
```

Expected: FAIL — `Lobbies.recent_tables/1` is undefined.

- [ ] **Step 3: Implement**

In `helios/lib/helios/lobbies.ex`, extend the aliases:

```elixir
  alias Helios.Accounts.Scope
  alias Helios.Accounts.User
  alias Helios.Games.Game
  alias Helios.Games.GamePlayer
  alias Helios.Lobbies.Invite
  alias Helios.Lobbies.Lobby
  alias Helios.Repo
```

Add `@recent_limit 5` under `@max_players 7`, and after `pending_invites/1`:

```elixir
  @doc """
  Up to 5 tables the user can still enter (accepted invites; the own table has
  "My table"), most recently active first. Activity is the later of the user's
  last visit and the latest game there in which the user was seated; tables
  with neither come last.
  """
  @spec recent_tables(User.t()) :: [
          %{lobby_id: Ecto.UUID.t(), owner_name: String.t(), last_active_at: DateTime.t() | nil}
        ]
  def recent_tables(%User{id: user_id}) do
    played =
      from g in Game,
        join: p in GamePlayer,
        on: p.game_id == g.id and p.user_id == ^user_id,
        group_by: g.lobby_id,
        select: %{lobby_id: g.lobby_id, played_at: type(max(g.inserted_at), :utc_datetime)}

    from(i in Invite,
      join: l in assoc(i, :lobby),
      join: o in assoc(l, :owner),
      left_join: g in subquery(played),
      on: g.lobby_id == l.id,
      where: i.user_id == ^user_id and i.status == "accepted" and l.owner_id != ^user_id,
      select: %{
        invite_id: i.id,
        lobby_id: l.id,
        owner_name: o.name,
        visited_at: i.last_visited_at,
        played_at: g.played_at
      }
    )
    |> Repo.all()
    |> Enum.map(fn row -> Map.put(row, :last_active_at, latest(row.visited_at, row.played_at)) end)
    |> Enum.sort_by(&recent_key/1)
    |> Enum.take(@recent_limit)
    |> Enum.map(&Map.take(&1, [:lobby_id, :owner_name, :last_active_at]))
  end

  defp latest(nil, other), do: other
  defp latest(one, nil), do: one
  defp latest(one, other), do: if(DateTime.compare(one, other) == :lt, do: other, else: one)

  # Ascending sort: active rows first (newest first), then never-active rows; ties by newest invite.
  defp recent_key(%{last_active_at: nil, invite_id: id}), do: {1, 0, -id}
  defp recent_key(%{last_active_at: at, invite_id: id}), do: {0, -DateTime.to_unix(at), -id}
```

- [ ] **Step 4: Run and commit**

```bash
cd helios && mix test test/helios/lobbies_test.exs && mix precommit
```

Expected: PASS.

```bash
git add helios/lib/helios/lobbies.ex helios/test/helios/lobbies_test.exs
git commit -m "feat(lobbies): list the user's recent tables" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 3: Recent tables header menu

**Files:**
- Modify: `helios/lib/helios_web/notifications.ex`, `helios/lib/helios_web/components/layouts.ex`, `helios/lib/helios_web/live/lobby_live.ex`, `helios/lib/helios_web/live/game_live.ex`, `helios/lib/helios_web/live/past_games_live.ex`
- Test: `helios/test/helios_web/components/layouts_test.exs`, `helios/test/helios_web/notifications_test.exs`

**Interfaces:**
- Consumes: `Lobbies.recent_tables/1` (Task 2).
- Produces: socket assign `:recent_tables` on every LiveView of the `:authenticated` live_session (set in `HeliosWeb.Notifications.on_mount/4`), refreshed on `{:invite_resolved, _}` and `{:uninvited, _}`.
- Produces: `HeliosWeb.Notifications.refresh_recent_tables(socket) :: socket`.
- Produces: `Layouts.app/1` attr `recent_tables` (list, default `[]`), forwarded to `Layouts.site_header/1` attr `recent_tables`. Markup: `div#recent-tables` (hidden when the list is empty; carries `phx-click-away` + Escape), `button#recent-tables-button` (`aria-haspopup="menu"`, `aria-controls="recent-tables-menu"`, `aria-expanded`), `ul#recent-tables-menu` (`role="menu"`, `hidden` by default), `a#recent-table-<lobby_id>` → `/lobby/<lobby_id>`, text "‹Owner›'s table".
- The three LiveViews that pass `notifications={@notifications}` today — `LobbyLive`, `GameLive`, `PastGamesLive` (the only ones in the `:authenticated` live_session) — pass `recent_tables={@recent_tables}` too.

- [ ] **Step 1: Write the failing tests**

`helios/test/helios_web/components/layouts_test.exs` — add a helper below `render_app/2` and two tests:

```elixir
  defp render_with_tables(tables) do
    assigns = %{scope: Scope.for_user(%User{id: 1, name: "alice"}), tables: tables}

    rendered_to_string(~H"""
    <Layouts.app flash={%{}} current_scope={@scope} recent_tables={@tables}>
      <p id="content">Hello</p>
    </Layouts.app>
    """)
    |> LazyHTML.from_fragment()
  end

  test "the Recent tables menu links each table and starts closed" do
    id = Ecto.UUID.generate()
    doc = render_with_tables([%{lobby_id: id, owner_name: "bob", last_active_at: nil}])

    button = LazyHTML.query(doc, "#site-header #recent-tables-button")
    assert LazyHTML.attribute(button, "aria-controls") == ["recent-tables-menu"]
    assert LazyHTML.attribute(button, "aria-haspopup") == ["menu"]
    assert LazyHTML.attribute(button, "aria-expanded") == ["false"]
    assert [js] = LazyHTML.attribute(button, "phx-click")
    assert js =~ "#recent-tables-menu"

    # Phones: icon only, the label stays for screen readers.
    assert doc |> LazyHTML.query("#recent-tables-button .hero-user-group") |> Enum.count() == 1
    assert doc |> LazyHTML.query("#recent-tables-button .sr-only") |> LazyHTML.text() =~
             "Recent tables"

    wrapper = LazyHTML.query(doc, "#recent-tables")
    assert [_] = LazyHTML.attribute(wrapper, "phx-click-away")
    assert LazyHTML.attribute(wrapper, "phx-key") == ["Escape"]

    [menu_class] = doc |> LazyHTML.query("#recent-tables-menu") |> LazyHTML.attribute("class")
    assert "hidden" in String.split(menu_class)

    link = LazyHTML.query(doc, "#recent-tables-menu #recent-table-#{id}")
    assert LazyHTML.attribute(link, "href") == ["/lobby/#{id}"]
    assert link |> LazyHTML.text() |> String.trim() == "bob's table"
  end

  # Review Focus 5.
  test "no Recent tables button when there are none" do
    doc = render_with_tables([])
    assert doc |> LazyHTML.query("#recent-tables, #recent-tables-button") |> Enum.empty?()
    assert doc |> LazyHTML.query("#site-header #my-table-link") |> Enum.count() == 1
  end
```

`helios/test/helios_web/notifications_test.exs` — inside `describe "hook"`:

```elixir
    test "the Recent tables menu follows accept and uninvite in an open tab", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")
      refute has_element?(view_b, "#recent-tables-button")

      :ok = Lobbies.accept(Scope.for_user(ctx.b), ctx.lobby_a.id)

      assert has_element?(
               view_b,
               "#recent-tables-menu #recent-table-#{ctx.lobby_a.id}",
               "#{ctx.a.name}'s table"
             )

      :ok = Lobbies.uninvite(Scope.for_user(ctx.a), ctx.lobby_a, ctx.b.id)

      refute has_element?(view_b, "#recent-tables-button")
    end
```

- [ ] **Step 2: Run to verify they fail**

```bash
cd helios && mix test test/helios_web/components/layouts_test.exs test/helios_web/notifications_test.exs
```

Expected: FAIL — `Layouts.app` warns about the unknown `recent_tables` attr and renders no `#recent-tables-button`; the hook test finds no menu.

- [ ] **Step 3: Notifications assigns and refreshes `recent_tables`**

In `helios/lib/helios_web/notifications.ex`, extend the moduledoc's first paragraph:

```elixir
  @moduledoc """
  Per-user notification strip for every authenticated LiveView. Also owns the
  header's `recent_tables` assign (`Lobbies.recent_tables/1`), which changes on
  the same `user:<id>` messages (`{:invite_resolved, _}`, `{:uninvited, _}`).
```

(Keep the rest of the moduledoc.) Change `on_mount/4` to:

```elixir
  def on_mount(:default, _params, _session, socket) do
    user = socket.assigns.current_scope.user

    if connected?(socket) do
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(user.id))
    end

    notifications = user |> Lobbies.pending_invites() |> Enum.map(&approve/1)

    {:cont,
     socket
     |> assign(:notifications, notifications)
     |> assign(:recent_tables, Lobbies.recent_tables(user))
     |> attach_hook(:notifications, :handle_info, &handle_info/2)
     |> attach_hook(:notifications, :handle_event, &handle_event/3)}
  end
```

Add after `push_simple/2`:

```elixir
  @doc "Reloads the header's Recent tables menu for the current user."
  def refresh_recent_tables(socket) do
    assign(socket, :recent_tables, Lobbies.recent_tables(socket.assigns.current_scope.user))
  end
```

Change the two handlers:

```elixir
  defp handle_info({:uninvited, lobby_id}, socket) do
    socket =
      socket
      |> update(:notifications, &remove(&1, approve_id(lobby_id)))
      |> refresh_recent_tables()

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
    {:halt,
     socket
     |> update(:notifications, &remove(&1, approve_id(lobby_id)))
     |> refresh_recent_tables()}
  end
```

- [ ] **Step 4: `Layouts.app` and `site_header`**

In `helios/lib/helios_web/components/layouts.ex`, add the attr after `:notifications`:

```elixir
  attr :recent_tables, :list,
    default: [],
    doc: "tables from Lobbies.recent_tables/1 for the header menu (authenticated pages)"
```

and forward it in `app/1`:

```elixir
        <.site_header current_scope={@current_scope} recent_tables={@recent_tables} />
```

Replace `site_header/1` (doc, attrs and template) with the version below. Besides the menu it tightens the phone gap to `gap-1` and shortens the phone label of `#past-games-link` to "Games": with a 24-character name truncated to nothing, the 360px touch header needs ~335px for Past games + My table + Logout + a 44px icon button at `gap-2`, but has only 320px (360 − `px-2`×2 − `px-3`×2). (Predicted; Task 6's e2e check is the arbiter.)

```elixir
  @doc """
  The rounded teal→blue header bar: username on the left; Recent tables (when
  there are any), Past games, My table and Logout on the right.
  """
  attr :current_scope, :map, required: true
  attr :recent_tables, :list, default: []

  def site_header(assigns) do
    ~H"""
    <header
      id="site-header"
      class="flex items-center justify-between gap-1 rounded-2xl bg-linear-to-b from-header-from to-header-to px-3 py-2 text-white shadow-md sm:gap-2 sm:px-6 sm:py-3"
    >
      <span
        id="current-user-name"
        class="min-w-0 truncate text-base font-semibold tracking-wide sm:text-lg"
      >
        {@current_scope.user.name}
      </span>
      <.recent_tables :if={@recent_tables != []} tables={@recent_tables} />
      <.link
        navigate={~p"/games"}
        id="past-games-link"
        class="inline-flex shrink-0 items-center justify-center rounded-md px-3 py-1.5 text-sm font-semibold text-white/90 transition hover:bg-white/15 hover:text-white active:bg-white/25 pointer-coarse:min-h-11"
      >
        <span class="sm:hidden">Games</span><span class="hidden sm:inline">Past games</span>
      </.link>
      <.link
        href={~p"/"}
        id="my-table-link"
        class="inline-flex shrink-0 items-center justify-center rounded-md px-3 py-1.5 text-sm font-semibold text-white/90 transition hover:bg-white/15 hover:text-white active:bg-white/25 pointer-coarse:min-h-11"
      >
        My table
      </.link>
      <.link
        id="logout-link"
        href={~p"/session"}
        method="delete"
        class="inline-flex shrink-0 items-center justify-center rounded-lg px-3 py-1.5 font-medium text-white/90 transition hover:bg-white/15 hover:text-white active:bg-white/25 pointer-coarse:min-h-11"
      >
        Logout
      </.link>
    </header>
    """
  end

  attr :tables, :list, required: true

  # Click-away and Escape sit on the wrapper, not the menu: a click on the
  # button is then not "away", so the button's toggle can also close the menu.
  defp recent_tables(assigns) do
    ~H"""
    <div
      id="recent-tables"
      class="relative shrink-0"
      phx-click-away={close_recent_tables()}
      phx-window-keydown={close_recent_tables()}
      phx-key="Escape"
    >
      <button
        id="recent-tables-button"
        type="button"
        aria-haspopup="menu"
        aria-controls="recent-tables-menu"
        aria-expanded="false"
        phx-click={
          JS.toggle(to: "#recent-tables-menu")
          |> JS.toggle_attribute({"aria-expanded", "true", "false"})
        }
        class="inline-flex items-center justify-center rounded-md px-2 py-1.5 text-sm font-semibold text-white/90 transition hover:bg-white/15 hover:text-white active:bg-white/25 sm:px-3 pointer-coarse:min-h-11 pointer-coarse:min-w-11"
      >
        <.icon name="hero-user-group" class="size-5 sm:hidden" />
        <span class="sr-only sm:not-sr-only">Recent tables</span>
      </button>
      <ul
        id="recent-tables-menu"
        role="menu"
        aria-label="Recent tables"
        class="absolute right-0 top-full z-50 mt-2 hidden w-56 max-w-[calc(100vw-1rem)] overflow-hidden rounded-xl bg-white py-1 text-zinc-800 shadow-2xl ring-1 ring-black/10"
      >
        <li :for={table <- @tables} role="none">
          <.link
            navigate={~p"/lobby/#{table.lobby_id}"}
            id={"recent-table-#{table.lobby_id}"}
            role="menuitem"
            class="flex items-center truncate px-4 py-2 text-sm font-medium transition hover:bg-zinc-100 active:bg-zinc-200 pointer-coarse:min-h-11"
          >
            {table.owner_name}'s table
          </.link>
        </li>
      </ul>
    </div>
    """
  end

  defp close_recent_tables do
    JS.hide(to: "#recent-tables-menu")
    |> JS.set_attribute({"aria-expanded", "false"}, to: "#recent-tables-button")
  end
```

- [ ] **Step 5: Every LiveView passes the assign**

`helios/lib/helios_web/live/lobby_live.ex`:

```elixir
    <Layouts.app
      flash={@flash}
      current_scope={@current_scope}
      notifications={@notifications}
      recent_tables={@recent_tables}
    >
```

`helios/lib/helios_web/live/game_live.ex` and `helios/lib/helios_web/live/past_games_live.ex` — add one line after `notifications={@notifications}` in their `<Layouts.app ...>` call (keep `background=` and everything else):

```elixir
      recent_tables={@recent_tables}
```

Verify none is missed:

```bash
cd helios && grep -rn "notifications={@notifications}" lib | wc -l && grep -rn "recent_tables={@recent_tables}" lib | wc -l
```

Expected: `3` and `3`.

- [ ] **Step 6: Run and commit**

```bash
cd helios && mix test test/helios_web/components/layouts_test.exs test/helios_web/notifications_test.exs && mix precommit
```

Expected: PASS.

```bash
git add helios/lib/helios_web/notifications.ex helios/lib/helios_web/components/layouts.ex helios/lib/helios_web/live/lobby_live.ex helios/lib/helios_web/live/game_live.ex helios/lib/helios_web/live/past_games_live.ex helios/test/helios_web/components/layouts_test.exs helios/test/helios_web/notifications_test.exs
git commit -m "feat(web): Recent tables menu in the header" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 4: Arriving at a table accepts its invite (`LobbyLive`)

**Files:**
- Modify: `helios/lib/helios_web/live/lobby_live.ex`
- Test: `helios/test/helios_web/notifications_test.exs`, `helios/test/helios_web/live/lobby_live_test.exs`

**Interfaces:**
- Consumes: `Lobbies.accept_pending/2`, `Lobbies.touch_visit/2` (Task 1); `Notifications.refresh_recent_tables/1` (Task 3).
- Produces: `LobbyLive.mount/3`, on a connected, authorized, non-owner mount: `accept_pending` → `touch_visit` → `refresh_recent_tables`. Nothing on the dead render or for the owner.

Why the banner disappears in the arriving tab: `HeliosWeb.Notifications` is listed in the `:authenticated` live_session's `on_mount` (router), so it runs before `LobbyLive.mount/3` in the same process and has already subscribed to `user:<id>` and assigned the pending banner. `accept/2` broadcasts `{:invite_resolved, lobby_id}` from inside `mount`; local `Phoenix.PubSub` delivery is a synchronous `send`, so the message sits in the LiveView's own mailbox before `mount` returns and is handled by the Notifications hook right after the first connected render. In tests any later `has_element?/render` call is queued behind it, so `refute has_element?` is deterministic. Other tabs receive the same broadcast.

- [ ] **Step 1: Write the failing tests**

`helios/test/helios_web/notifications_test.exs` — inside `describe "hook"`:

```elixir
    # Review Focus 4.
    test "opening the inviting table accepts the invite and clears the banner in every tab", %{
      conn: conn
    } do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      {:ok, other_tab, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")
      assert has_element?(other_tab, "#accept-invite-#{ctx.lobby_a.id}")

      {:ok, at_table, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_a.id}")

      refute has_element?(at_table, "#accept-invite-#{ctx.lobby_a.id}")
      refute has_element?(other_tab, "#accept-invite-#{ctx.lobby_a.id}")
      assert Lobbies.pending_invites(ctx.b) == []
      assert has_element?(at_table, "#recent-table-#{ctx.lobby_a.id}")
      assert has_element?(other_tab, "#recent-table-#{ctx.lobby_a.id}")
    end

    # Review Focus 1.
    test "re-opening a table whose invite is accepted broadcasts nothing", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      :ok = Lobbies.accept(Scope.for_user(ctx.b), ctx.lobby_a.id)
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(ctx.b.id))
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(ctx.a.id))

      {:ok, _at_table, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_a.id}")
      {:ok, _own, _} = live(ctx.conn_a, ~p"/lobby/#{ctx.lobby_a.id}")

      refute_receive {:invite_resolved, _}
    end

    # Review Focus 2.
    test "the disconnected render accepts nothing and records no visit", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)

      assert ctx.conn_b |> get(~p"/lobby/#{ctx.lobby_a.id}") |> html_response(200) =~
               "accept-invite-#{ctx.lobby_a.id}"

      assert Lobbies.pending_invites(ctx.b) == [
               %{lobby_id: ctx.lobby_a.id, owner_name: ctx.a.name}
             ]

      assert Helios.Repo.get_by!(Helios.Lobbies.Invite, lobby_id: ctx.lobby_a.id, user_id: ctx.b.id).last_visited_at ==
               nil
    end
```

`helios/test/helios_web/live/lobby_live_test.exs` — add `alias Helios.Lobbies.Invite` and `alias Helios.Repo` to the aliases, then inside `describe "guest view"`:

```elixir
    test "a guest's visit is recorded; the owner's is not", %{conn: conn} do
      a = player_with_lobby(build_conn())
      b = player_with_lobby(conn)
      invite_fixture(a.lobby, a.user, b.user)
      :ok = Lobbies.accept(Helios.Accounts.Scope.for_user(b.user), a.lobby.id)

      {:ok, _owner_view, _} = live(a.conn, ~p"/lobby/#{a.lobby.id}")
      assert Repo.get_by!(Invite, lobby_id: a.lobby.id, user_id: b.user.id).last_visited_at == nil

      {:ok, view_b, _} = live(b.conn, ~p"/lobby/#{a.lobby.id}")

      assert %DateTime{} =
               Repo.get_by!(Invite, lobby_id: a.lobby.id, user_id: b.user.id).last_visited_at

      assert has_element?(view_b, "#recent-table-#{a.lobby.id}", "#{a.user.name}'s table")
    end
```

- [ ] **Step 2: Run to verify they fail**

```bash
cd helios && mix test test/helios_web/notifications_test.exs test/helios_web/live/lobby_live_test.exs
```

Expected: FAIL — the banner is still there after opening the table, the invite stays pending, `last_visited_at` stays nil. ("re-opening … broadcasts nothing" and "the disconnected render …" already pass; they guard the implementation.)

- [ ] **Step 3: Implement**

In `helios/lib/helios_web/live/lobby_live.ex`, change the success branch of `mount/3`:

```elixir
    with {:ok, lobby} <- Lobbies.fetch_lobby(game_id),
         :ok <- Lobbies.authorize(lobby, user) do
      {:ok, socket |> setup(lobby, user) |> arrive(lobby, user) |> assign_game_state()}
```

and add below `setup/3`:

```elixir
  # Opening a table you were invited to counts as accepting the invite; the
  # `{:invite_resolved, _}` broadcast clears the banner here and in other tabs
  # (HeliosWeb.Notifications subscribed in on_mount, before this mount).
  defp arrive(socket, lobby, user) do
    if connected?(socket) and lobby.owner_id != user.id do
      scope = socket.assigns.current_scope
      Lobbies.accept_pending(scope, lobby.id)
      :ok = Lobbies.touch_visit(scope, lobby.id)
      Notifications.refresh_recent_tables(socket)
    else
      socket
    end
  end
```

- [ ] **Step 4: Run everything and commit**

```bash
cd helios && mix test test/helios_web/notifications_test.exs test/helios_web/live/lobby_live_test.exs && mix precommit
```

Expected: PASS, including the existing suites (`lobby_live_test` "owner invites an online player, who then joins the table" now auto-accepts B's invite on arrival; none of its assertions depend on the status).

```bash
git add helios/lib/helios_web/live/lobby_live.ex helios/test/helios_web/notifications_test.exs helios/test/helios_web/live/lobby_live_test.exs
git commit -m "feat(web): opening an inviting table accepts the invite" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 5: Opening a game accepts its table's invite (`GameLive`)

**Files:**
- Modify: `helios/lib/helios_web/live/game_live.ex`
- Test: `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Consumes: `Lobbies.accept_pending/2` (Task 1); test helpers `user_with_token_fixture/0`, `invite_all/3`, `game_fixture/2`, `log_in_conn/2` (`Helios.GamesFixtures`, already imported) and the file's `open/2`.
- Produces: `GameLive` calls `Lobbies.accept_pending(scope, game.lobby_id)` on a connected, authorized (seated) mount — active and finished games alike. No `touch_visit` (seated games already count through `game_players`).

- [ ] **Step 1: Write the failing test**

Append to `helios/test/helios_web/live/game_live_test.exs`, before the final `end`:

```elixir
  # Review Focus 4.
  test "opening a game accepts a pending invite to its table in every tab" do
    [{a, _ta}, {b, tb}, {c, _tc}] = for _ <- 1..3, do: user_with_token_fixture()
    lobby = Helios.Lobbies.get_or_create_own_lobby(a)
    :ok = invite_all(a, lobby, [b, c])
    game = game_fixture([a, b, c], lobby: lobby)

    {:ok, other_tab, _} = live(log_in_conn(build_conn(), tb), ~p"/games")
    assert has_element?(other_tab, "#accept-invite-#{lobby.id}")

    {:ok, view, _html} = open(tb, game.id)

    refute has_element?(view, "#accept-invite-#{lobby.id}")
    refute has_element?(other_tab, "#accept-invite-#{lobby.id}")
    assert Helios.Lobbies.pending_invites(b) == []
    assert has_element?(view, "#recent-table-#{lobby.id}")
  end
```

- [ ] **Step 2: Run to verify it fails**

```bash
cd helios && mix test test/helios_web/live/game_live_test.exs
```

Expected: FAIL — `#accept-invite-<id>` is still rendered on the game page.

- [ ] **Step 3: Implement**

In `helios/lib/helios_web/live/game_live.ex`, make `arrive/2` the first call of `mount_authorized/3` (if Phase B changed the rest of the function body, keep it as is):

```elixir
  defp mount_authorized(socket, game, user) do
    arrive(socket, game)

    if game.status == "finished" do
      mount_finished(socket, game, user)
    else
      mount_active(socket, game, user)
    end
  end

  # A seated player opening a game straight from a table they were invited to
  # "arrives" there too; the `{:invite_resolved, _}` broadcast clears the banner.
  defp arrive(socket, game) do
    if connected?(socket),
      do: Lobbies.accept_pending(socket.assigns.current_scope, game.lobby_id)

    :ok
  end
```

- [ ] **Step 4: Run and commit**

```bash
cd helios && mix test test/helios_web/live/game_live_test.exs && mix precommit
```

Expected: PASS.

```bash
git add helios/lib/helios_web/live/game_live.ex helios/test/helios_web/live/game_live_test.exs
git commit -m "feat(web): opening a game accepts its table's pending invite" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 6: e2e — arrival by URL, Recent tables, phone header; phase gate

**Files:**
- Modify: `e2e/tests/lobby.spec.ts`, `e2e/tests/responsive.spec.ts`
- Possibly modify: `helios/lib/helios_web/components/layouts.ex` (only if the phone header check fails)

**Interfaces:**
- Consumes: ids from Tasks 3–4; e2e helpers `invite`, `notification`, `memberRow`, `waitForLiveView` (`./support/lobby`), `setupTable`, `expectTapTargets`, `expectNoHorizontalScroll` (already imported in `responsive.spec.ts`).

- [ ] **Step 1: Add the e2e checks**

`e2e/tests/lobby.spec.ts` — add `waitForLiveView` to the import from `./support/lobby`, then append:

```ts
test("13. opening the table by URL accepts the invite; Recent tables leads back", async ({
  player,
}) => {
  const a = await player("a");
  const b = await player("b");

  await invite(a.page, b.name);
  await expect(notification(b.page, `You are expected on table ${a.name}`)).toBeVisible();

  const tab = await b.context.newPage();
  await tab.goto(a.lobbyUrl);
  await waitForLiveView(tab);

  await expect(memberRow(a.page, b.name)).toHaveAttribute("data-connected", "true");
  await expect(notification(tab, `You are expected on table ${a.name}`)).toHaveCount(0);
  await expect(notification(b.page, `You are expected on table ${a.name}`)).toHaveCount(0);

  // The first tab (B's own table) got the menu through the same broadcast.
  await b.page.locator("#recent-tables-button").click();
  await b.page.locator("#recent-tables-menu a", { hasText: `${a.name}'s table` }).click();
  await expect(b.page).toHaveURL(a.lobbyUrl);
});
```

`e2e/tests/responsive.spec.ts`, test "lobby controls fit" — before `if (vp.touch) {` add (the guest accepted the owner's invite in `setupTable`, so their header has the menu):

```ts
      // Review Focus 5: a guest's header with the Recent tables button still fits.
      const guestPage = players[1].page;
      await expect(guestPage.locator("#recent-tables-button")).toBeVisible();
      await expectNoHorizontalScroll(guestPage);
      for (const id of ["#recent-tables-button", "#my-table-link", "#logout-link"]) {
        await expect(guestPage.locator(id)).toBeInViewport({ ratio: 1 });
      }
      if (vp.touch) await expectTapTargets(guestPage.locator("#recent-tables-button"));
```

- [ ] **Step 2: Run the e2e suite**

```bash
cd e2e && npm ci && npx playwright install chromium && npx playwright test tests/lobby.spec.ts tests/responsive.spec.ts
```

Expected: PASS. If "lobby controls fit" fails on `small-android` because `#logout-link` leaves the viewport, the predicted width budget in Task 3 was too optimistic: drop the phone padding of the header links from `px-3` to `px-2` (`px-2 sm:px-3`) on `#past-games-link`, `#my-table-link` and `#logout-link` in `site_header/1`, rerun, and include `helios/lib/helios_web/components/layouts.ex` in the commit.

- [ ] **Step 3: Phase gate**

```bash
cd helios && mix precommit
cd ../e2e && npx playwright test
```

Expected: all green (ExUnit and every Playwright spec, including `game.spec.ts`, which clicks `#past-games-link` by id).

- [ ] **Step 4: Commit**

```bash
git add e2e/tests/lobby.spec.ts e2e/tests/responsive.spec.ts
git commit -m "test(e2e): invite auto-accept by URL and the Recent tables menu" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

(Add `helios/lib/helios_web/components/layouts.ex` to the `git add` only if Step 2 required the padding fix.)
