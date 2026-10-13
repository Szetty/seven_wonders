# AI Players — Phase C: Seats and AI Players Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Table owners can seat AI players. A game started with AI seats plays itself on those seats: one provider call per decision, a random legal move on timeout or error, and every move persisted with its source so random moves can be shown and replayed.

**Architecture:**
- **Schema.** A migration makes `game_players.user_id` and `game_actions.user_id` nullable. It adds `game_players.ai_model`, plus `game_actions.seat` and `game_actions.source`. SQLite can't alter nullability, so both tables are rebuilt with portable SQL.
- **Actors.** `Helios.Games.Actor` names who acts: a user id or `{:ai, seat}`. Engine ids are `"<user_id>"` and `"ai-<seat>"`.
- **Lobby.** The lobby stores AI seats in `lobby_ai_seats`, and `Games.start_game/2` seats them after the connected humans.
- **GameServer.** `GameServer` takes `source` on every submit. It records random moves as history-shaped keys (`Helios.Games.RandomMoves`), and `Replay` rebuilds the same set from the log.
- **AI players.** `GameServer.init/1` starts one `Helios.AI.Player` per AI seat. Each player monitors the server, re-fetches its own view on every `{:game_updated, _}`, and calls its provider under the model deadline in a `Task.Supervisor` task.
- **Setup.** During setup the owner acts for AI seats through `Games.submit_for_ai_seat/4`.

**Tech Stack:** Ecto (SQLite, portable SQL), OTP (GenServer, DynamicSupervisor, Task.Supervisor), Phoenix PubSub, ExUnit.

**Spec:** `docs/superpowers/specs/2026-10-13-ai-players/2026-10-13-ai-players-design.md` (sections "Data model", "Lobby", "AI seat process", "Orchestration changes", "Testing"). Cross-phase contract: `docs/superpowers/plans/2026-10-13-ai-players/README.md`.

## Global Constraints

- **Prerequisites:** phases A and B are merged: `Helios.AI.Models`, `Helios.AI.Readiness`, `Helios.AI.Prompt` (`build/1`, `to_choice/1`), the `Helios.AI.Provider` adapters, `Helios.AI.TaskSupervisor` and `Helios.AI.Supervisor`. If any of these differ from the README, stop and ask.
- Read `helios/AGENTS.md` before touching `helios/`.
- No engine change. Elixir never re-implements rules: AI moves come only from `Prompt.build/1`'s legal ids, and the engine still validates every submit.
- PubSub broadcasts carry no game state. AI players fetch their own view like a browser.
- Queries and migrations stay portable (no SQLite-only SQL). Table rebuilds use `CREATE` / `INSERT … SELECT` / `DROP` / `ALTER TABLE … RENAME`.
- Existing callers keep working. `Games.submit/3`, `submit_choice/3` and `view/2` still accept integer user ids. `snapshot/2` and `replay/2` only *add* the `random` key.
- One provider call per decision at most, and no call after the deadline.
- Tests: `start_supervised!/1`, no `Process.sleep/1`. Tests that seat AI players and drive them by hand set `put_ai_config(players: false)`.
- Stage explicit paths only; never stage `helios/config/dev.exs`.
- Run `cd helios && mix precommit` before each commit. Commit messages end with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.

## Review Focus

- Existing games, whose rows have `user_id` set and default `source`, must replay identically after the migration. Existing human-only tests must pass unchanged. (Test in Task 1, "existing rows survive"; plus the full suite.)
- The history `{:turn, event}` fields of Babylon's play-last-card and Halicarnassus's build-from-discard turns must equal the phase recorded just before the submit. Otherwise "random" tags would never match. (Test in Task 4, "random keys match history"; stop and ask if it fails.)
- A GameServer restart must neither lose random marks nor make an AI seat call its provider again for a decision it already submitted. (Tests in Tasks 4 and 5.)
- A non-owner, or the owner targeting a *human* seat or a card action, must get `{:error, :not_leader}` / `{:error, :invalid_choice}` from `submit_for_ai_seat/4`, with nothing logged. (Test in Task 4.)
- When a provider never answers (`fake-timeout`), the random move lands within deadline + slack, and the hung task is killed rather than leaked. (Test in Task 5.)

---

### Task 1: Migration, schemas, `Helios.Games.Actor`

**Files:**
- Create: `helios/priv/repo/migrations/<timestamp>_add_ai_players.exs` (generated)
- Create: `helios/lib/helios/games/actor.ex`, `helios/lib/helios/lobbies/ai_seat.ex`
- Modify: `helios/lib/helios/games/game_player.ex`, `helios/lib/helios/games/game_action.ex`, `helios/lib/helios/games/replay.ex` (`player_ids/1`, new `seats/1`)
- Modify: `helios/test/support/games_fixtures.ex` (`game_fixture/2` accepts `{:ai, model_id}`, new `put_ai_config/1`)
- Test: `helios/test/helios/games/actor_test.exs`, `helios/test/helios/games/replay_test.exs`

**Interfaces:**
- Produces: the `Helios.Games.Actor` functions
  - `player_id(actor) :: String.t()`
  - `from_player_id(String.t()) :: actor`
  - `of_seat(GamePlayer.t()) :: actor`
  - `of_row(GameAction.t()) :: actor`
  - `row_fields(actor) :: %{user_id: integer | nil, seat: integer | nil}`
- Produces: `GamePlayer` field `ai_model`; `GameAction` fields `seat` and `source` (default `"human"`); schema `Helios.Lobbies.AISeat` (`lobby_id`, `model_id`, `inserted_at`).
- Produces: `Replay.seats(game_id) :: [GamePlayer.t()]` (seat order). `Replay.player_ids/1` now returns `"ai-<seat>"` for AI seats.
- Produces: `GamesFixtures.game_fixture(users_or_ai, opts)`, where entries may be `{:ai, model_id}` (the first entry must be a user); `GamesFixtures.put_ai_config(overrides)`.

- [ ] **Step 1: Write the failing tests**

Create `helios/test/helios/games/actor_test.exs`:

```elixir
defmodule Helios.Games.ActorTest do
  use ExUnit.Case, async: true

  alias Helios.Games.{Actor, GameAction, GamePlayer}

  test "engine ids round-trip" do
    assert Actor.player_id(42) == "42"
    assert Actor.player_id({:ai, 3}) == "ai-3"
    assert Actor.from_player_id("42") == 42
    assert Actor.from_player_id("ai-3") == {:ai, 3}
  end

  test "seats and log rows name their actor" do
    assert Actor.of_seat(%GamePlayer{user_id: 7, seat: 0, ai_model: nil}) == 7
    assert Actor.of_seat(%GamePlayer{user_id: nil, seat: 2, ai_model: "fake-first"}) == {:ai, 2}
    assert Actor.of_row(%GameAction{user_id: 7, seat: nil}) == 7
    assert Actor.of_row(%GameAction{user_id: nil, seat: 2}) == {:ai, 2}
  end

  test "row fields set exactly one of user_id / seat" do
    assert Actor.row_fields(7) == %{user_id: 7, seat: nil}
    assert Actor.row_fields({:ai, 2}) == %{user_id: nil, seat: 2}
  end
end
```

Add to `helios/test/helios/games/replay_test.exs`, inside the module (it already uses `Helios.DataCase` and imports `Helios.GamesFixtures`; add `alias Helios.Games.{GameAction, Replay}` if they aren't aliased yet):

```elixir
  describe "AI seats" do
    test "player ids name AI seats ai-<seat>, in seat order" do
      [a, b] = for _ <- 1..2, do: player_fixture()
      game = game_fixture([a, b, {:ai, "fake-first"}])

      assert Replay.player_ids(game.id) == [to_string(a.id), to_string(b.id), "ai-2"]
      assert [%{ai_model: nil}, %{ai_model: nil}, %{ai_model: "fake-first", user_id: nil}] =
               Replay.seats(game.id)
    end

    test "existing rows survive with source human and replay" do
      users = for _ <- 1..3, do: player_fixture()
      game = game_fixture(users)
      [a | _] = users
      {:ok, view} = Helios.Core.view(elem(Replay.rebuild(game, [], fn _ -> nil end, fn _, _, acc -> acc end), 1), to_string(a.id))

      Repo.insert!(%GameAction{
        game_id: game.id,
        seq: 1,
        user_id: a.id,
        action: Helios.Games.ActionCodec.encode({:discard, hd(view.hand).name})
      })

      assert [%GameAction{source: "human", seat: nil}] = Replay.actions(game.id)
      assert {:ok, _ref, nil} = Replay.rebuild(game, Replay.actions(game.id), fn _ -> nil end, fn _, _, acc -> acc end)
    end
  end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios/games/actor_test.exs test/helios/games/replay_test.exs`
Expected: FAIL (`Helios.Games.Actor` is undefined; `game_fixture` rejects `{:ai, _}`; `ai_model` / `source` are unknown fields).

- [ ] **Step 3: Migration**

Run: `cd helios && mix ecto.gen.migration add_ai_players`

Replace the generated module body with:

```elixir
defmodule Helios.Repo.Migrations.AddAiPlayers do
  use Ecto.Migration

  # SQLite cannot change a column's nullability, so game_players and
  # game_actions are rebuilt (create / copy / drop / rename) in SQL that
  # Postgres accepts too.
  def up do
    create table(:lobby_ai_seats) do
      add :lobby_id, references(:lobbies, type: :binary_id, on_delete: :delete_all), null: false
      add :model_id, :string, null: false
      timestamps(type: :utc_datetime, updated_at: false)
    end

    create index(:lobby_ai_seats, [:lobby_id])

    create table(:game_players_new) do
      add :game_id, references(:games, type: :binary_id, on_delete: :delete_all), null: false
      add :user_id, references(:users, on_delete: :nothing)
      add :seat, :integer, null: false
      add :ai_model, :string
    end

    execute "INSERT INTO game_players_new (id, game_id, user_id, seat) SELECT id, game_id, user_id, seat FROM game_players"
    drop table(:game_players)
    rename table(:game_players_new), to: table(:game_players)
    create unique_index(:game_players, [:game_id, :seat])
    create unique_index(:game_players, [:game_id, :user_id])
    create index(:game_players, [:user_id])

    create table(:game_actions_new) do
      add :game_id, references(:games, type: :binary_id, on_delete: :delete_all), null: false
      add :seq, :integer, null: false
      add :user_id, references(:users, on_delete: :nothing)
      add :seat, :integer
      add :source, :string, null: false, default: "human"
      add :action, :map, null: false
      timestamps(type: :utc_datetime, updated_at: false)
    end

    execute "INSERT INTO game_actions_new (id, game_id, seq, user_id, action, inserted_at) SELECT id, game_id, seq, user_id, action, inserted_at FROM game_actions"
    drop table(:game_actions)
    rename table(:game_actions_new), to: table(:game_actions)
    create unique_index(:game_actions, [:game_id, :seq])
  end

  # Back to human-only tables: AI seats and their actions are dropped.
  def down do
    drop table(:lobby_ai_seats)

    create table(:game_players_old) do
      add :game_id, references(:games, type: :binary_id, on_delete: :delete_all), null: false
      add :user_id, references(:users, on_delete: :nothing), null: false
      add :seat, :integer, null: false
    end

    execute "INSERT INTO game_players_old (id, game_id, user_id, seat) SELECT id, game_id, user_id, seat FROM game_players WHERE user_id IS NOT NULL"
    drop table(:game_players)
    rename table(:game_players_old), to: table(:game_players)
    create unique_index(:game_players, [:game_id, :seat])
    create unique_index(:game_players, [:game_id, :user_id])
    create index(:game_players, [:user_id])

    create table(:game_actions_old) do
      add :game_id, references(:games, type: :binary_id, on_delete: :delete_all), null: false
      add :seq, :integer, null: false
      add :user_id, references(:users, on_delete: :nothing), null: false
      add :action, :map, null: false
      timestamps(type: :utc_datetime, updated_at: false)
    end

    execute "INSERT INTO game_actions_old (id, game_id, seq, user_id, action, inserted_at) SELECT id, game_id, seq, user_id, action, inserted_at FROM game_actions WHERE user_id IS NOT NULL"
    drop table(:game_actions)
    rename table(:game_actions_old), to: table(:game_actions)
    create unique_index(:game_actions, [:game_id, :seq])
  end
end
```

Run: `cd helios && mix ecto.migrate && mix ecto.rollback && mix ecto.migrate`
Expected: all three succeed. If `drop table` fails on a foreign key, report the error. Don't disable foreign keys without asking.

- [ ] **Step 4: Schemas and `Actor`**

`helios/lib/helios/games/game_player.ex`: update the moduledoc and add the field:

```elixir
  @moduledoc """
  A seat at a game. Seat order is the engine's player order. A human seat has
  `user_id`; an AI seat has `ai_model` (a `Helios.AI.Models` id) and no user.
  """
```

```elixir
  schema "game_players" do
    field :seat, :integer
    field :ai_model, :string
    belongs_to :game, Helios.Games.Game, type: :binary_id
    belongs_to :user, Helios.Accounts.User
  end
```

`helios/lib/helios/games/game_action.ex`:

```elixir
  @moduledoc """
  One accepted engine action, in submission order (`seq` starts at 1). The
  actor is `user_id` (a human) or `seat` (an AI seat; owner setup choices for
  AI seats too). `source` is `"human"`, `"ai"` or `"random"` (an AI fallback).
  """
```

```elixir
  schema "game_actions" do
    field :seq, :integer
    field :action, :map
    field :seat, :integer
    field :source, :string, default: "human"
    belongs_to :game, Helios.Games.Game, type: :binary_id
    belongs_to :user, Helios.Accounts.User

    timestamps(type: :utc_datetime, updated_at: false)
  end
```

Create `helios/lib/helios/games/actor.ex`:

```elixir
defmodule Helios.Games.Actor do
  @moduledoc """
  Who acts in a game: a user id (integer) or `{:ai, seat}`. The engine knows
  players by string id: `"<user_id>"` or `"ai-<seat>"` (a hyphen, so ids stay
  valid in DOM ids and CSS selectors).
  """

  alias Helios.Games.{GameAction, GamePlayer}

  @type t :: integer() | {:ai, non_neg_integer()}

  @spec player_id(t()) :: String.t()
  def player_id({:ai, seat}) when is_integer(seat), do: "ai-#{seat}"
  def player_id(user_id) when is_integer(user_id), do: Integer.to_string(user_id)

  @spec from_player_id(String.t()) :: t()
  def from_player_id("ai-" <> seat), do: {:ai, String.to_integer(seat)}
  def from_player_id(id) when is_binary(id), do: String.to_integer(id)

  @spec of_seat(GamePlayer.t()) :: t()
  def of_seat(%GamePlayer{ai_model: nil, user_id: user_id}), do: user_id
  def of_seat(%GamePlayer{seat: seat}), do: {:ai, seat}

  @spec of_row(GameAction.t()) :: t()
  def of_row(%GameAction{user_id: nil, seat: seat}), do: {:ai, seat}
  def of_row(%GameAction{user_id: user_id}), do: user_id

  @spec row_fields(t()) :: %{user_id: integer() | nil, seat: non_neg_integer() | nil}
  def row_fields({:ai, seat}), do: %{user_id: nil, seat: seat}
  def row_fields(user_id) when is_integer(user_id), do: %{user_id: user_id, seat: nil}
end
```

Create `helios/lib/helios/lobbies/ai_seat.ex`:

```elixir
defmodule Helios.Lobbies.AISeat do
  @moduledoc "An AI player seated at a lobby's table (persists across games until removed)."
  use Ecto.Schema

  @type t :: %__MODULE__{}

  schema "lobby_ai_seats" do
    field :model_id, :string
    belongs_to :lobby, Helios.Lobbies.Lobby, type: :binary_id

    timestamps(type: :utc_datetime, updated_at: false)
  end
end
```

In `helios/lib/helios/games/replay.ex`, add `Actor` to the alias line (`alias Helios.Games.{ActionCodec, Actor, Game, GameAction, GamePlayer}`) and replace `player_ids/1` with:

```elixir
  @spec seats(Ecto.UUID.t()) :: [GamePlayer.t()]
  def seats(game_id),
    do: Repo.all(from p in GamePlayer, where: p.game_id == ^game_id, order_by: p.seat)

  @spec player_ids(Ecto.UUID.t()) :: [String.t()]
  def player_ids(game_id),
    do: game_id |> seats() |> Enum.map(&(&1 |> Actor.of_seat() |> Actor.player_id()))
```

In `rebuild/4`, replace `Core.submit(ref, to_string(row.user_id), action)` with `Core.submit(ref, row |> Actor.of_row() |> Actor.player_id(), action)`.

- [ ] **Step 5: Fixtures**

In `helios/test/support/games_fixtures.ex`, replace the seat-inserting `Enum.each` in `game_fixture/2` with:

```elixir
    users
    |> Enum.with_index()
    |> Enum.each(fn
      {{:ai, model_id}, seat} ->
        Repo.insert!(%GamePlayer{game_id: game.id, user_id: nil, ai_model: model_id, seat: seat})

      {user, seat} ->
        Repo.insert!(%GamePlayer{game_id: game.id, user_id: user.id, seat: seat})
    end)
```

Update its `@doc` to mention that `users` may contain `{:ai, model_id}` entries (any position except the first). Add after `put_games_config/1`:

```elixir
  @doc "Merges `overrides` into `config :helios, Helios.AI` for the current test (e.g. `players: false`)."
  def put_ai_config(overrides) do
    original = Application.get_env(:helios, Helios.AI, [])
    Application.put_env(:helios, Helios.AI, Keyword.merge(original, overrides))
    ExUnit.Callbacks.on_exit(fn -> Application.put_env(:helios, Helios.AI, original) end)
  end
```

- [ ] **Step 6: Run the tests**

Run: `cd helios && mix test test/helios/games/actor_test.exs test/helios/games/replay_test.exs`
Expected: PASS.

Run: `cd helios && mix test`
Expected: the whole suite passes. Human-only games are unaffected.

- [ ] **Step 7: Precommit and commit**

```bash
git add helios/priv/repo/migrations/*_add_ai_players.exs helios/lib/helios/games/actor.ex \
  helios/lib/helios/lobbies/ai_seat.ex helios/lib/helios/games/game_player.ex \
  helios/lib/helios/games/game_action.ex helios/lib/helios/games/replay.ex \
  helios/test/support/games_fixtures.ex helios/test/helios/games/actor_test.exs \
  helios/test/helios/games/replay_test.exs
git commit -m "feat(games): AI seats in the schema, actors and engine ids

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 2: Lobby AI seats

**Files:**
- Modify: `helios/lib/helios/lobbies.ex`
- Test: `helios/test/helios/lobbies_test.exs`

**Interfaces:**
- Consumes: `Helios.AI.Readiness.ready?/1`; `Helios.Lobbies.AISeat`.
- Produces:
  - `Lobbies.ai_seats(lobby) :: [AISeat.t()]` (insertion order)
  - `Lobbies.add_ai_seat(scope, lobby, model_id) :: {:ok, AISeat.t()} | {:error, :not_leader | :model_unavailable | :lobby_full}`
  - `Lobbies.remove_ai_seat(scope, lobby, seat_id) :: :ok | {:error, :not_leader | :no_ai_seat}`
  - both mutations broadcast `{:members_changed}` on `"lobby:<id>"`
  - `error_message/1` gets clauses for `:model_unavailable` and `:no_ai_seat`
- Changes: the invite capacity check counts AI seats too, so invites + AI seats + owner ≤ 7.

- [ ] **Step 1: Write the failing tests**

Add to `helios/test/helios/lobbies_test.exs` (it already has the `owner_and_lobby/0` helper and imports the fixtures; alias `Helios.Lobbies.AISeat` if needed):

```elixir
  describe "AI seats" do
    test "the owner adds ready models; seats are listed in order and broadcast" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      assert {:ok, %AISeat{model_id: "fake-first"}} = Lobbies.add_ai_seat(scope, lobby, "fake-first")
      assert_receive {:members_changed}
      assert {:ok, _} = Lobbies.add_ai_seat(scope, lobby, "fake-first")
      assert {:ok, _} = Lobbies.add_ai_seat(scope, lobby, "fake-last")

      assert Enum.map(Lobbies.ai_seats(lobby), & &1.model_id) == ~w(fake-first fake-first fake-last)
    end

    test "unknown or unready models are rejected" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()

      for model <- ["fake-down", "not-configured", nil, 3] do
        assert Lobbies.add_ai_seat(scope, lobby, model) == {:error, :model_unavailable}
      end

      assert Lobbies.ai_seats(lobby) == []
    end

    test "only the owner adds or removes" do
      %{owner: owner, lobby: lobby, scope: scope} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      {:ok, seat} = Lobbies.add_ai_seat(scope, lobby, "fake-first")

      assert {:error, :not_leader} = Lobbies.add_ai_seat(Scope.for_user(guest), lobby, "fake-first")
      assert {:error, :not_leader} = Lobbies.remove_ai_seat(Scope.for_user(guest), lobby, seat.id)
      assert length(Lobbies.ai_seats(lobby)) == 1
    end

    test "remove deletes the seat; a second remove is :no_ai_seat" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      {:ok, seat} = Lobbies.add_ai_seat(scope, lobby, "fake-first")
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      assert :ok = Lobbies.remove_ai_seat(scope, lobby, to_string(seat.id))
      assert_receive {:members_changed}
      assert Lobbies.ai_seats(lobby) == []
      assert {:error, :no_ai_seat} = Lobbies.remove_ai_seat(scope, lobby, seat.id)
      assert {:error, :no_ai_seat} = Lobbies.remove_ai_seat(scope, lobby, "nope")
    end

    test "AI seats and invites share the 7 seats" do
      %{owner: owner, lobby: lobby, scope: scope} = owner_and_lobby()
      for _ <- 1..4, do: invite_fixture(lobby, owner, player_fixture())
      assert {:ok, _} = Lobbies.add_ai_seat(scope, lobby, "fake-first")
      assert {:ok, _} = Lobbies.add_ai_seat(scope, lobby, "fake-first")
      assert {:error, :lobby_full} = Lobbies.add_ai_seat(scope, lobby, "fake-first")
      assert {:error, :lobby_full} = Lobbies.invite(scope, lobby, player_fixture().id)
    end

    test "error messages" do
      assert Lobbies.error_message(:model_unavailable) == "That AI model is not available right now"
      assert Lobbies.error_message(:no_ai_seat) == "That AI player is no longer at the table"
    end
  end
```

If `invite_fixture/3` is named differently in the test file, use the existing helper the `set_wonder_mode/3` tests use.

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios/lobbies_test.exs`
Expected: FAIL (the functions are undefined).

- [ ] **Step 3: Implement**

In `helios/lib/helios/lobbies.ex`:
- add `alias Helios.Lobbies.AISeat`;
- add `{:members_changed}` for AI seat changes to the moduledoc's PubSub list wording ("members or AI seats changed");
- add after `set_wonder_mode/3`:

```elixir
  @doc "The lobby's AI seats, in the order they were added."
  @spec ai_seats(Lobby.t()) :: [AISeat.t()]
  def ai_seats(%Lobby{id: lobby_id}) do
    Repo.all(
      from s in AISeat, where: s.lobby_id == ^lobby_id, order_by: [asc: s.inserted_at, asc: s.id]
    )
  end

  @doc """
  The owner seats an AI player using `model_id`, which must be ready
  (`Helios.AI.Readiness`). AI seats and invites share the #{@max_players} seats.
  """
  @spec add_ai_seat(Scope.t(), Lobby.t(), term()) ::
          {:ok, AISeat.t()} | {:error, :not_leader | :model_unavailable | :lobby_full}
  def add_ai_seat(%Scope{user: %User{} = user}, %Lobby{} = lobby, model_id) do
    with :ok <- ensure_leader(lobby, user),
         :ok <- ensure_ready(model_id),
         {:ok, seat} <- insert_ai_seat(lobby, model_id) do
      broadcast(lobby_topic(lobby.id), {:members_changed})
      {:ok, seat}
    end
  end

  @doc "The owner removes an AI seat (`seat_id` integer or numeric string)."
  @spec remove_ai_seat(Scope.t(), Lobby.t(), term()) :: :ok | {:error, :not_leader | :no_ai_seat}
  def remove_ai_seat(%Scope{user: %User{} = user}, %Lobby{} = lobby, seat_id) do
    with :ok <- ensure_leader(lobby, user),
         {:ok, seat_id} <- parse_user_id(seat_id, :no_ai_seat),
         {1, _} <-
           Repo.delete_all(from s in AISeat, where: s.lobby_id == ^lobby.id and s.id == ^seat_id) do
      broadcast(lobby_topic(lobby.id), {:members_changed})
      :ok
    else
      {0, _} -> {:error, :no_ai_seat}
      {:error, _reason} = error -> error
    end
  end
```

Add the error messages next to the others:

```elixir
  def error_message(:model_unavailable), do: "That AI model is not available right now"
  def error_message(:no_ai_seat), do: "That AI player is no longer at the table"
```

Add the private helpers next to `ensure_capacity/1`, and change `ensure_capacity/1` so it counts AI seats too:

```elixir
  defp ensure_ready(model_id) when is_binary(model_id) do
    if Helios.AI.Readiness.ready?(model_id), do: :ok, else: {:error, :model_unavailable}
  end

  defp ensure_ready(_model_id), do: {:error, :model_unavailable}

  # Owner + invitees + AI seats must stay within @max_players.
  defp ensure_capacity(%Lobby{id: lobby_id}) do
    invites = Repo.aggregate(from(i in Invite, where: i.lobby_id == ^lobby_id), :count)
    ai = Repo.aggregate(from(s in AISeat, where: s.lobby_id == ^lobby_id), :count)

    if invites + ai + 1 >= @max_players, do: {:error, :lobby_full}, else: :ok
  end

  defp insert_ai_seat(lobby, model_id) do
    Repo.transaction(fn ->
      with :ok <- ensure_capacity(lobby) do
        Repo.insert!(%AISeat{lobby_id: lobby.id, model_id: model_id})
      else
        {:error, reason} -> Repo.rollback(reason)
      end
    end)
  end
```

`ensure_capacity/1` returns `:lobby_full` when *adding one more* would exceed the limit, so it's checked before the insert in both `insert_invite/2` and `insert_ai_seat/2`. That matches the existing `>=` check.

- [ ] **Step 4: Run the tests**

Run: `cd helios && mix test test/helios/lobbies_test.exs`
Expected: PASS.

- [ ] **Step 5: Precommit and commit**

```bash
git add helios/lib/helios/lobbies.ex helios/test/helios/lobbies_test.exs
git commit -m "feat(lobbies): owner-managed AI seats sharing the seven seats

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 3: Starting games with AI seats; names and ids

**Files:**
- Modify: `helios/lib/helios/games.ex`
- Test: `helios/test/helios/games_test.exs`

**Interfaces:**
- Consumes: `Lobbies.ai_seats/1`, `Helios.AI.Models.seat_labels/1`, `Helios.Games.Actor`.
- Produces:
  - `Games.seat_plan(lobby) :: [{:user, User.t()} | {:ai, model_id}]`: connected humans in members order, then AI seats.
  - `Games.player_id(GamePlayer.t()) :: String.t()`.
  - `Games.display_names([GamePlayer.t()]) :: %{player_id => String.t()}`; players must have `:user` preloaded, as `Games.players/1` does.
  - `Games.ai_player_ids([GamePlayer.t()]) :: MapSet.t(String.t())`.
- Changes: `start_game/2` seats `seat_plan/1`, so AI seats count towards the 3–7 limits.

- [ ] **Step 1: Write the failing tests**

Add to `helios/test/helios/games_test.exs` (it uses the fixtures and `connect_to_lobby/2`; alias `Helios.Accounts.Scope` and `Helios.Lobbies` if they aren't aliased yet):

```elixir
  describe "AI seats" do
    setup do
      on_exit(&stop_game_servers/0)
      put_ai_config(players: false)
      :ok
    end

    test "seat_plan lists connected humans, then AI seats" do
      [owner, b] = for _ <- 1..2, do: player_fixture()
      lobby = Lobbies.get_or_create_own_lobby(owner)
      invite_all(owner, lobby, [b])
      {:ok, _} = Lobbies.add_ai_seat(Scope.for_user(owner), lobby, "fake-first")

      plan = fn ->
        Enum.map(Games.seat_plan(lobby), fn
          {:user, user} -> {:user, user.id}
          ai -> ai
        end)
      end

      assert plan.() == [{:user, owner.id}, {:ai, "fake-first"}]
      connect_to_lobby(lobby, [b])
      assert plan.() == [{:user, owner.id}, {:user, b.id}, {:ai, "fake-first"}]
    end

    test "the owner starts with AI seats filling the table" do
      owner = player_fixture()
      lobby = Lobbies.get_or_create_own_lobby(owner)
      scope = Scope.for_user(owner)
      {:ok, _} = Lobbies.add_ai_seat(scope, lobby, "fake-first")
      assert {:error, :not_enough_players} = Games.start_game(scope, lobby)

      {:ok, _} = Lobbies.add_ai_seat(scope, lobby, "fake-last")
      assert {:ok, game} = Games.start_game(scope, lobby)

      players = Games.players(game)
      assert Enum.map(players, &{&1.seat, &1.user_id, &1.ai_model}) ==
               [{0, owner.id, nil}, {1, nil, "fake-first"}, {2, nil, "fake-last"}]

      assert Enum.map(players, &Games.player_id/1) == [to_string(owner.id), "ai-1", "ai-2"]
      assert {:ok, %{me: "ai-1"}} = Games.view(game.id, {:ai, 1})
      assert Games.ai_player_ids(players) == MapSet.new(["ai-1", "ai-2"])
    end

    test "display names: user names for humans, numbered labels for AI" do
      owner = player_fixture()
      game = game_fixture([owner, {:ai, "fake-first"}, {:ai, "fake-first"}, {:ai, "gone"}])

      assert Games.display_names(Games.players(game)) == %{
               to_string(owner.id) => owner.name,
               "ai-1" => "Fake First",
               "ai-2" => "Fake First #2",
               "ai-3" => "gone"
             }
    end
  end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios/games_test.exs`
Expected: FAIL (`seat_plan/1` etc. are undefined).

- [ ] **Step 3: Implement**

In `helios/lib/helios/games.ex`:
- change the alias line to `alias Helios.Games.{ActionCodec, Actor, Game, GamePlayer, GameServer, Replay}`;
- add after `eligible_players/1`:

```elixir
  @doc "Who would be seated if the game started now: connected humans (members order), then the lobby's AI seats."
  @spec seat_plan(Lobbies.Lobby.t()) :: [{:user, Helios.Accounts.User.t()} | {:ai, String.t()}]
  def seat_plan(lobby) do
    Enum.map(eligible_players(lobby), &{:user, &1}) ++
      Enum.map(Lobbies.ai_seats(lobby), &{:ai, &1.model_id})
  end

  @doc "The engine id of a seat."
  @spec player_id(GamePlayer.t()) :: String.t()
  def player_id(%GamePlayer{} = player), do: player |> Actor.of_seat() |> Actor.player_id()

  @doc "Engine id => display name; AI seats are their model label, numbered when repeated."
  @spec display_names([GamePlayer.t()]) :: %{String.t() => String.t()}
  def display_names(players) do
    {ai, humans} = Enum.split_with(players, &is_binary(&1.ai_model))
    labels = Helios.AI.Models.seat_labels(Enum.map(ai, & &1.ai_model))

    Map.new(
      Enum.map(humans, &{player_id(&1), &1.user.name}) ++
        Enum.zip(Enum.map(ai, &player_id/1), labels)
    )
  end

  @spec ai_player_ids([GamePlayer.t()]) :: MapSet.t(String.t())
  def ai_player_ids(players),
    do: for(%GamePlayer{ai_model: model} = p <- players, is_binary(model), into: MapSet.new(), do: player_id(p))
```

- replace the body of `start_game/2` with:

```elixir
  def start_game(%Scope{user: user}, lobby) do
    seats = seat_plan(lobby)

    with :ok <- check_leader(lobby, user),
         :ok <- check_player_count(length(seats)),
         :ok <- check_no_active_game(lobby),
         player_ids = seat_player_ids(seats),
         wonders = configured_wonders(lobby, length(seats)),
         seed = new_seed(),
         :ok <- validate_setup(player_ids, wonders, seed),
         {:ok, game} <- insert_game(lobby, seats, seed, wonders),
         {:ok, game} <- boot(game) do
      Phoenix.PubSub.broadcast(Helios.PubSub, "lobby:#{lobby.id}", {:game_started, game.id})
      {:ok, game}
    end
  end
```

- add the private helper and change the `Multi.insert_all` rows in `insert_game/4` (rename its `players` parameter to `seats`):

```elixir
  defp seat_player_ids(seats) do
    seats
    |> Enum.with_index()
    |> Enum.map(fn
      {{:user, user}, _seat} -> Actor.player_id(user.id)
      {{:ai, _model}, seat} -> Actor.player_id({:ai, seat})
    end)
  end
```

```elixir
    |> Multi.insert_all(:players, GamePlayer, fn %{game: game} ->
      Enum.with_index(seats, fn
        {:user, user}, seat -> %{game_id: game.id, user_id: user.id, ai_model: nil, seat: seat}
        {:ai, model}, seat -> %{game_id: game.id, user_id: nil, ai_model: model, seat: seat}
      end)
    end)
```

In `helios/lib/helios_web/live/lobby_live.ex` `assign_game_state/1`, replace `length(Games.eligible_players(lobby))` with `length(Games.seat_plan(lobby))`.

- [ ] **Step 4: Run the tests**

Run: `cd helios && mix test test/helios/games_test.exs test/helios_web/live/lobby_live_game_test.exs`
Expected: PASS.

- [ ] **Step 5: Precommit and commit**

```bash
git add helios/lib/helios/games.ex helios/lib/helios_web/live/lobby_live.ex helios/test/helios/games_test.exs
git commit -m "feat(games): start games with AI seats; display names and engine ids

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 4: Sources, random-move keys, owner setup for AI seats

**Files:**
- Create: `helios/lib/helios/games/random_moves.ex`
- Modify: `helios/lib/helios/games.ex`, `helios/lib/helios/games/game_server.ex`, `helios/lib/helios/games/replay.ex`, `helios/test/support/game_driver.ex`
- Test: `helios/test/helios/games/random_moves_test.exs`, `helios/test/helios/games/game_server_test.exs`, `helios/test/helios/games_test.exs`

**Interfaces:**
- Consumes: `Helios.Games.Actor`.
- Produces:
  - `RandomMoves.new/0`, `record(set, source, phase, player_id)`, `key(phase, player_id)`, and `random?(set, turn_event, player_id) :: boolean`.
  - `Games.submit(game_id, actor, action, source \\ :human)` and `Games.submit_choice(game_id, actor, choice, source \\ :human)`. Source is `:human | :ai | :random`; anything else gives `{:error, :invalid_source}`.
  - `Games.view(game_id, actor)`. Snapshot is `{:ok, %{view, history, random}}`, and replay is `{:ok, %{view, history, frames, random}}`.
  - `Games.submit_for_ai_seat(scope, game_id, seat, action) :: :ok | {:error, :not_leader | :invalid_choice | term()}`.
  - `GameServer.view(pid, actor)` and `GameServer.submit_choice(pid, actor, choice, source)` (direct calls).
  - `Replay.rebuild_with_random(game, actions, init, step) :: {:ok, ref, acc, random} | {:error, term()}`.
  - `GameDriver.games_funs/1` accepts AI engine ids.

- [ ] **Step 1: Write the failing tests**

Create `helios/test/helios/games/random_moves_test.exs`:

```elixir
defmodule Helios.Games.RandomMovesTest do
  use ExUnit.Case, async: true

  alias Helios.{Core, GameDriver}
  alias Helios.Games.RandomMoves

  test "record keeps only random sources" do
    phase = %{kind: :choosing_cards, age: 2, turn: 4, extra_turn_kind: nil}
    set = RandomMoves.new() |> RandomMoves.record(:ai, phase, "ai-1") |> RandomMoves.record(:random, phase, "ai-2")
    assert set == MapSet.new([{"ai-2", 2, 4, :regular}])
    assert RandomMoves.random?(set, %{age: 2, turn: 4, kind: :regular}, "ai-2")
    refute RandomMoves.random?(set, %{age: 2, turn: 4, kind: :regular}, "ai-1")
    assert RandomMoves.record(set, :random, %{kind: :setup, age: 1, turn: 1}, "ai-1") == set
  end

  # The key is taken from the phase just before the submit; it must equal the
  # fields of the history event that turn produces, for extra turns too.
  for {kind, wonder} <- [build_from_discard: {"Halikarnassós", :b}, play_last_card: {"Babylon", :b}] do
    test "random keys match history for #{kind}" do
      ids = ["1", "2", "3"]
      wonders = [unquote(Macro.escape(wonder)), {"Gizah", :a}, {"Rhódos", :a}]
      strategies = %{"1" => :wonder, "2" => :build, "3" => :build}
      stop? = &match?(%{phase: %{kind: :extra_turn, extra_turn_kind: unquote(kind)}}, &1)
      seed = GameDriver.find_seed(ids, wonders, strategies, stop?)
      {:ok, ref} = Core.new_game(ids, {:explicit, wonders}, seed)
      {view_fun, submit_fun} = GameDriver.core_funs(ref)
      {:reached, view} = GameDriver.play_until(ids, strategies, view_fun, submit_fun, stop?)

      player = view.phase.extra_turn_player
      key = RandomMoves.key(view_fun.(player).phase, player)
      action = GameDriver.choose(view_fun.(player), :build) || GameDriver.choose(view_fun.(player), :discard)
      :ok = submit_fun.(player, action)

      {:ok, history} = Core.history(ref)
      {:turn, event} = history |> Enum.filter(&match?({:turn, _}, &1)) |> List.last()
      assert key == {player, event.age, event.turn, event.kind}
      assert Enum.any?(event.plays, &(&1.player == player))
    end
  end
end
```

Add to `helios/test/helios/games/game_server_test.exs`:

```elixir
  describe "AI seats and sources" do
    setup do
      put_ai_config(players: false)
      [human] = [player_fixture()]
      game = game_fixture([human, {:ai, "fake-first"}, {:ai, "fake-first"}])
      %{ai_game: game, human: human}
    end

    test "AI actors submit with a source; random ones are marked", %{ai_game: game, human: human} do
      {:ok, view} = Games.view(game.id, {:ai, 1})
      assert view.me == "ai-1"
      card = hd(view.hand).name

      assert :ok = Games.submit_choice(game.id, {:ai, 1}, %{card: card, kind: "discard", option: 0}, :random)
      {:ok, view2} = Games.view(game.id, {:ai, 2})
      assert :ok = Games.submit(game.id, {:ai, 2}, {:discard, hd(view2.hand).name}, :ai)

      assert [%GameAction{seat: 1, user_id: nil, source: "random"}, %GameAction{seat: 2, source: "ai"}] =
               Repo.all(from a in GameAction, where: a.game_id == ^game.id, order_by: a.seq)

      assert {:ok, %{random: random}} = Games.snapshot(game.id, human.id)
      assert random == MapSet.new([{"ai-1", 1, 1, :regular}])
      assert {:error, :invalid_source} = Games.submit(game.id, human.id, {:discard, "x"}, :bogus)
    end

    test "random marks survive a restart", %{ai_game: game, human: human} do
      {:ok, view} = Games.view(game.id, {:ai, 1})
      :ok = Games.submit(game.id, {:ai, 1}, {:discard, hd(view.hand).name}, :random)
      stop_game_servers()

      assert {:ok, %{random: random}} = Games.snapshot(game.id, human.id)
      assert MapSet.member?(random, {"ai-1", 1, 1, :regular})
    end
  end
```

Add to `helios/test/helios/games_test.exs`:

```elixir
  describe "submit_for_ai_seat/4" do
    setup do
      on_exit(&stop_game_servers/0)
      put_ai_config(players: false)
      [owner, b] = for _ <- 1..2, do: player_fixture()
      game = game_fixture([owner, b, {:ai, "fake-first"}], selection: :choose_side)
      %{game: game, owner: owner, b: b}
    end

    test "the owner claims and confirms for an AI seat", %{game: game, owner: owner} do
      {:ok, %{setup: setup}} = Games.view(game.id, owner.id)
      wonder = Enum.find(setup.seats, &(&1.player == "ai-2")).wonder
      scope = Scope.for_user(owner)

      assert :ok = Games.submit_for_ai_seat(scope, game.id, 2, {:claim_wonder, %{wonder: wonder, side: :b}})
      assert :ok = Games.submit_for_ai_seat(scope, game.id, 2, :confirm_wonder)

      {:ok, %{setup: setup}} = Games.view(game.id, owner.id)
      assert %{side: :b, confirmed: true} = Enum.find(setup.seats, &(&1.player == "ai-2"))
      assert [%{seat: 2, user_id: nil, source: "human"} | _] =
               Repo.all(from a in Helios.Games.GameAction, where: a.game_id == ^game.id, order_by: a.seq)
    end

    test "non-owners, human seats and card actions are rejected", %{game: game, owner: owner, b: b} do
      claim = {:claim_wonder, %{wonder: "Gizah", side: :a}}
      assert {:error, :not_leader} = Games.submit_for_ai_seat(Scope.for_user(b), game.id, 2, claim)
      assert {:error, :not_leader} = Games.submit_for_ai_seat(Scope.for_user(owner), game.id, 1, claim)
      assert {:error, :not_leader} = Games.submit_for_ai_seat(Scope.for_user(owner), game.id, 9, claim)
      assert {:error, :invalid_choice} = Games.submit_for_ai_seat(Scope.for_user(owner), game.id, 2, {:discard, "Altar"})
      assert Repo.aggregate(from(a in Helios.Games.GameAction, where: a.game_id == ^game.id), :count) == 0
    end
  end
```

Add to `helios/test/helios/games/replay_test.exs` (inside `describe "AI seats"`):

```elixir
    test "replay returns the random marks of a finished game" do
      put_ai_config(players: false)
      on_exit(&stop_game_servers/0)
      [a, b] = for _ <- 1..2, do: player_fixture()
      game = game_fixture([a, b, {:ai, "fake-first"}])
      ids = [to_string(a.id), to_string(b.id), "ai-2"]
      {view_fun, _submit} = Helios.GameDriver.games_funs(game.id)

      submit = fn
        "ai-2", action -> Games.submit(game.id, {:ai, 2}, action, :random)
        player, action -> Games.submit(game.id, String.to_integer(player), action)
      end

      {:game_over, _} = Helios.GameDriver.play_until(ids, %{}, view_fun, submit, fn _ -> false end)

      assert {:ok, %{random: random, history: history}} = Games.replay(Repo.get!(Game, game.id), a.id)
      turns = for {:turn, event} <- history, Enum.any?(event.plays, &(&1.player == "ai-2")), do: event
      assert MapSet.size(random) == length(turns)
      assert Enum.all?(turns, &Helios.Games.RandomMoves.random?(random, &1, "ai-2"))
    end
```

Alias `Helios.Games` and `Helios.Games.Game` in that file if they aren't aliased yet.

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios/games`
Expected: FAIL (`RandomMoves` is undefined; `Games.view(game_id, {:ai, 1})` raises in `to_string/1`; `submit/4` is undefined).

- [ ] **Step 3: `RandomMoves`**

Create `helios/lib/helios/games/random_moves.ex`:

```elixir
defmodule Helios.Games.RandomMoves do
  @moduledoc """
  Which plays were AI random fallbacks: a set of `{player_id, age, turn,
  turn_kind}` keys — exactly the fields of a history `{:turn, event}`
  (`turn_kind` is `:regular`, `:play_last_card` or `:build_from_discard`). A
  key is taken from the phase just before the submit; setup has no key.
  """

  @type key :: {String.t(), pos_integer(), pos_integer(), atom()}
  @type t :: MapSet.t(key())

  @spec new() :: t()
  def new, do: MapSet.new()

  @spec record(t(), atom(), map(), String.t()) :: t()
  def record(set, :random, phase, player) do
    case key(phase, player) do
      nil -> set
      key -> MapSet.put(set, key)
    end
  end

  def record(set, _source, _phase, _player), do: set

  @spec key(map(), String.t()) :: key() | nil
  def key(%{kind: :choosing_cards, age: age, turn: turn}, player), do: {player, age, turn, :regular}

  def key(%{kind: :extra_turn, age: age, turn: turn, extra_turn_kind: kind}, player),
    do: {player, age, turn, kind}

  def key(_phase, _player), do: nil

  @doc "Whether `player`'s play in the history turn `event` was random."
  @spec random?(t(), map(), String.t()) :: boolean()
  def random?(set, %{age: age, turn: turn, kind: kind}, player),
    do: MapSet.member?(set, {player, age, turn, kind})
end
```

- [ ] **Step 4: `Replay.rebuild_with_random/4`**

In `helios/lib/helios/games/replay.ex`, add `RandomMoves` to the aliases and replace `rebuild/4` with:

```elixir
  @doc """
  A fresh engine with every row of `actions` submitted in order. `init` sees
  the fresh engine and returns the accumulator; `step` sees each row with the
  engine just after it was applied.
  """
  @spec rebuild(Game.t(), [GameAction.t()], (Core.game() -> acc), (GameAction.t(),
                                                                   Core.game(),
                                                                   acc ->
                                                                     acc)) ::
          {:ok, Core.game(), acc} | {:error, term()}
        when acc: term()
  def rebuild(%Game{} = game, actions, init, step) do
    with {:ok, ref, acc, _random} <- rebuild_with_random(game, actions, init, step),
         do: {:ok, ref, acc}
  end

  @doc "`rebuild/4` that also returns the `Helios.Games.RandomMoves` set of the log."
  @spec rebuild_with_random(Game.t(), [GameAction.t()], (Core.game() -> acc), (GameAction.t(),
                                                                              Core.game(),
                                                                              acc ->
                                                                                acc)) ::
          {:ok, Core.game(), acc, RandomMoves.t()} | {:error, term()}
        when acc: term()
  def rebuild_with_random(%Game{} = game, actions, init, step) do
    with {:ok, wonders} <- ActionCodec.decode_wonders(game.wonders),
         {:ok, ref} <- Core.new_game(player_ids(game.id), wonders, game.seed) do
      actions
      |> Enum.reduce_while({:ok, init.(ref), RandomMoves.new()}, fn %GameAction{} = row,
                                                                     {:ok, acc, random} ->
        player = row |> Actor.of_row() |> Actor.player_id()

        with {:ok, action} <- ActionCodec.decode(row.action),
             {:ok, %{phase: phase}} <- Core.view(ref, player),
             :ok <- Core.submit(ref, player, action) do
          random = RandomMoves.record(random, source(row), phase, player)
          {:cont, {:ok, step.(row, ref, acc), random}}
        else
          {:error, reason} -> {:halt, {:error, {:replay_failed, row.seq, reason}}}
        end
      end)
      |> case do
        {:ok, acc, random} -> {:ok, ref, acc, random}
        error -> error
      end
    end
  end

  defp source(%GameAction{source: "random"}), do: :random
  defp source(%GameAction{}), do: :human
```

- [ ] **Step 5: `GameServer`**

In `helios/lib/helios/games/game_server.ex`:
- change the alias line to `alias Helios.Games.{ActionCodec, Actor, Choice, Game, GameAction, Replay}`;
- in the moduledoc, add: "Submissions carry a `source` (`:human | :ai | :random`); random ones are kept as `Helios.Games.RandomMoves` keys, rebuilt from the log on init and returned by `:snapshot`.";
- in `init/1`, replace the `case Replay.rebuild(…)` head and the `{:ok, ref, nil}` clause with:

```elixir
    case Replay.rebuild_with_random(game, actions, fn _ref -> nil end, fn _row, _ref, acc -> acc end) do
      {:ok, ref, nil, random} ->
        state = %{
          game_id: game.id,
          lobby_id: game.lobby_id,
          ref: ref,
          seq: last_seq(actions),
          players: Replay.player_ids(game.id),
          random: random,
          status: game.status,
          idle_timeout: idle_timeout
        }

        state = maybe_finish(state)
        {:ok, state, state.idle_timeout}
```

- add the public direct-call functions after `start_link/1`:

```elixir
  @doc "Direct call for processes holding the server pid (AI players): no ensure_started, no retry."
  def view(server, actor), do: GenServer.call(server, {:view, actor})

  @doc "Direct call for processes holding the server pid (AI players)."
  def submit_choice(server, actor, choice, source),
    do: GenServer.call(server, {:submit_choice, actor, choice, source})
```

- replace every `handle_call/3` clause and `do_submit/3` with:

```elixir
  @impl true
  def handle_call({:submit, actor, action, source}, _from, state) do
    do_submit(state, actor, action, source)
  end

  def handle_call({:submit_choice, actor, choice, source}, _from, state) do
    with {:ok, view} <- Core.view(state.ref, Actor.player_id(actor)),
         {:ok, action} <- Choice.resolve(view, choice) do
      do_submit(state, actor, action, source)
    else
      {:error, reason} -> {:reply, {:error, reason}, state, state.idle_timeout}
    end
  end

  def handle_call({:view, actor}, _from, state) do
    {:reply, Core.view(state.ref, Actor.player_id(actor)), state, state.idle_timeout}
  end

  def handle_call(:history, _from, state) do
    {:reply, Core.history(state.ref), state, state.idle_timeout}
  end

  def handle_call({:snapshot, actor}, _from, state) do
    with {:ok, view} <- Core.view(state.ref, Actor.player_id(actor)),
         {:ok, history} <- Core.history(state.ref) do
      {:reply, {:ok, %{view: view, history: history, random: state.random}}, state,
       state.idle_timeout}
    else
      {:error, reason} -> {:reply, {:error, reason}, state, state.idle_timeout}
    end
  end
```

```elixir
  defp do_submit(state, actor, action, source) do
    player = Actor.player_id(actor)

    with {:ok, %{phase: phase}} <- Core.view(state.ref, player),
         :ok <- Core.submit(state.ref, player, action) do
      seq = state.seq + 1

      # Raises on failure -> crash -> restart + replay without this action.
      Repo.insert!(
        struct(
          GameAction,
          Map.merge(Actor.row_fields(actor), %{
            game_id: state.game_id,
            seq: seq,
            source: Atom.to_string(source),
            action: ActionCodec.encode(action)
          })
        )
      )

      random = Helios.Games.RandomMoves.record(state.random, source, phase, player)
      state = maybe_finish(%{state | seq: seq, random: random})
      Phoenix.PubSub.broadcast(Helios.PubSub, Games.topic(state.game_id), {:game_updated, seq})
      {:reply, :ok, state, state.idle_timeout}
    else
      {:error, reason} -> {:reply, {:error, reason}, state, state.idle_timeout}
    end
  end
```

- [ ] **Step 6: `Games` API**

In `helios/lib/helios/games.ex`:
- add `@sources [:human, :ai, :random]` at the top of the module;
- replace `submit/3`, `submit_choice/3`, `view/2` and `snapshot/2` with:

```elixir
  @doc "`actor` is a user id or `{:ai, seat}`; `source` is `:human`, `:ai` or `:random`."
  @spec submit(term(), Actor.t(), term(), atom()) :: :ok | {:error, term()}
  def submit(game_id, actor, action, source \\ :human)

  def submit(game_id, actor, action, source) when source in @sources,
    do: call(game_id, {:submit, actor, action, source})

  def submit(_game_id, _actor, _action, _source), do: {:error, :invalid_source}

  @spec submit_choice(term(), Actor.t(), Helios.Games.Choice.t(), atom()) :: :ok | {:error, term()}
  def submit_choice(game_id, actor, choice, source \\ :human)

  def submit_choice(game_id, actor, %{} = choice, source) when source in @sources,
    do: call(game_id, {:submit_choice, actor, choice, source})

  def submit_choice(_game_id, _actor, _choice, _source), do: {:error, :invalid_source}

  @spec view(term(), Actor.t()) :: {:ok, map()} | {:error, term()}
  def view(game_id, actor), do: call(game_id, {:view, actor})
```

```elixir
  @doc """
  The viewer's view, the public history and the random-move marks
  (`Helios.Games.RandomMoves`) read in one `GameServer` call, so they can
  never be split by a concurrent submit.
  """
  @spec snapshot(term(), Actor.t()) ::
          {:ok, %{view: map(), history: [term()], random: MapSet.t()}} | {:error, term()}
  def snapshot(game_id, actor), do: call(game_id, {:snapshot, actor})
```

- add `submit_for_ai_seat/4` after `submit_choice/4`:

```elixir
  @doc """
  The lobby owner's setup choice (claim or confirm a board) for one of the
  game's AI seats. Logged under the AI seat with source `:human`.
  """
  @spec submit_for_ai_seat(Scope.t(), term(), term(), term()) :: :ok | {:error, term()}
  def submit_for_ai_seat(%Scope{user: user}, game_id, seat, action) do
    with :ok <- setup_action(action),
         {:ok, game} <- fetch_game(game_id),
         :ok <- check_ai_controller(game, user, seat) do
      submit(game.id, {:ai, seat}, action, :human)
    end
  end

  defp setup_action({:claim_wonder, %{wonder: _, side: _}}), do: :ok
  defp setup_action(:confirm_wonder), do: :ok
  defp setup_action(_action), do: {:error, :invalid_choice}

  defp check_ai_controller(game, user, seat) when is_integer(seat) do
    owner? =
      Repo.exists?(
        from l in Lobbies.Lobby, where: l.id == ^game.lobby_id and l.owner_id == ^user.id
      )

    ai? =
      Repo.exists?(
        from p in GamePlayer,
          where: p.game_id == ^game.id and p.seat == ^seat and not is_nil(p.ai_model)
      )

    if owner? and ai?, do: :ok, else: {:error, :not_leader}
  end

  defp check_ai_controller(_game, _user, _seat), do: {:error, :not_leader}
```

- in `replay/2`, use `rebuild_with_random` and return `random`:

```elixir
    with {:ok, ref, acc, random} <-
           Replay.rebuild_with_random(game, Replay.actions(game.id), init, step),
         {:ok, %{phase: %{kind: :game_over}} = view} <- Core.view(ref, me),
         {:ok, history} <- Core.history(ref) do
      {:ok,
       %{
         view: view,
         history: history,
         frames: Enum.reverse(acc.done, [acc.current]),
         random: random
       }}
    else
```

Also update its `@spec` to include `random: MapSet.t()`.

- in `frame_step/4`, replace `if to_string(row.user_id) == me do` with `if row |> Actor.of_row() |> Actor.player_id() == me do`.

- [ ] **Step 7: `GameDriver`**

In `helios/test/support/game_driver.ex`, replace `games_funs/1` with:

```elixir
  def games_funs(game_id) do
    {fn player ->
       {:ok, view} = Games.view(game_id, Helios.Games.Actor.from_player_id(player))
       view
     end,
     fn player, action ->
       Games.submit(game_id, Helios.Games.Actor.from_player_id(player), action)
     end}
  end
```

- [ ] **Step 8: Run the tests**

Run: `cd helios && mix test test/helios/games test/helios/games_test.exs`
Expected: PASS. If a "random keys match history" test fails because the history event's `turn` or `kind` differs from the phase, **stop and ask** (README human checkpoint). Don't change `RandomMoves.key/2` to fit without agreement.

Run: `cd helios && mix test`
Expected: the whole suite passes.

- [ ] **Step 9: Precommit and commit**

```bash
git add helios/lib/helios/games/random_moves.ex helios/lib/helios/games.ex helios/lib/helios/games/game_server.ex \
  helios/lib/helios/games/replay.ex helios/test/support/game_driver.ex \
  helios/test/helios/games/random_moves_test.exs helios/test/helios/games/game_server_test.exs \
  helios/test/helios/games_test.exs helios/test/helios/games/replay_test.exs
git commit -m "feat(games): move sources, random-move marks and owner setup for AI seats

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 5: `Helios.AI.Player`

**Files:**
- Create: `helios/lib/helios/ai/player.ex`
- Modify: `helios/lib/helios/games/game_server.ex` (start AI players in `init/1`)
- Test: `helios/test/helios/ai/player_test.exs`, `helios/test/helios/ai/full_game_test.exs`

**Interfaces:**
- Consumes:
  - `GameServer.view/2` and `submit_choice/4`;
  - `Helios.AI.Prompt.build/1` and `to_choice/1`;
  - `Helios.AI.Models.fetch/1` and `provider_module/1`;
  - `Helios.AI.Readiness.ready?/1`;
  - `Helios.AI.TaskSupervisor` and `Helios.AI.Supervisor`;
  - `Replay.seats/1`.
- Produces:
  - `Helios.AI.Player.start_link(game_id:, seat:, model_id:, server:)`.
  - `Player.decision_key(view) :: {age, turn, turn_kind} | nil` (public for tests).
  - Telemetry event `[:helios, :ai, :decision]`, with measurements `%{latency_ms, input_tokens, output_tokens}` and metadata `%{game_id, model_id, source, reason}`.
  - Config flag `config :helios, Helios.AI, players: false` disables starting AI players (tests only).

- [ ] **Step 1: Write the failing tests**

Create `helios/test/helios/ai/player_test.exs`:

```elixir
defmodule Helios.AI.PlayerTest do
  use Helios.DataCase, async: false

  import Helios.GamesFixtures

  alias Helios.AI.Player
  alias Helios.{Games, SampleViews}
  alias Helios.Games.GameAction

  setup do
    Application.put_env(:helios, :fake_ai_listener, self())
    on_exit(fn -> Application.delete_env(:helios, :fake_ai_listener) end)
    on_exit(&stop_game_servers/0)
    :ok
  end

  defp start_game(models, opts \\ []) do
    human = player_fixture()
    game = game_fixture([human | Enum.map(models, &{:ai, &1})], opts)
    Phoenix.PubSub.subscribe(Helios.PubSub, Games.topic(game.id))
    {:ok, _pid} = Games.ensure_started(game.id)
    {game, human}
  end

  defp rows(game),
    do: Repo.all(from a in GameAction, where: a.game_id == ^game.id, order_by: a.seq)

  describe "decision_key/1" do
    test "a pending pick, an own extra turn; nothing otherwise" do
      view = SampleViews.view(%{submitted: [{"1", false}, {"2", true}]})
      assert Player.decision_key(view) == {2, 3, :regular}
      assert Player.decision_key(SampleViews.view()) == nil

      extra = fn player ->
        SampleViews.view(%{
          phase: %{kind: :extra_turn, age: 1, turn: 6, direction: :west,
                   extra_turn_player: player, extra_turn_kind: :play_last_card}
        })
      end

      assert Player.decision_key(extra.("1")) == {1, 6, :play_last_card}
      assert Player.decision_key(extra.("2")) == nil
      assert Player.decision_key(SampleViews.setup_view()) == nil
    end
  end

  test "AI seats decide at once and submit as ai, once per decision" do
    {game, human} = start_game(["fake-first", "fake-first"])

    assert_receive {:fake_ai_decide, "fake-first", %{moves: [_, _ | _]}}
    assert_receive {:fake_ai_decide, "fake-first", _}
    assert_receive {:game_updated, 2}, 2_000
    assert [%{source: "ai", user_id: nil}, %{source: "ai", user_id: nil}] = rows(game)
    assert Enum.sort(Enum.map(rows(game), & &1.seat)) == [1, 2]

    for _ <- 1..3, do: Phoenix.PubSub.broadcast(Helios.PubSub, Games.topic(game.id), {:game_updated, 99})
    refute_receive {:fake_ai_decide, _, _}, 200

    {:ok, view} = Games.view(game.id, human.id)
    :ok = Games.submit(game.id, human.id, {:discard, hd(view.hand).name})
    assert_receive {:fake_ai_decide, "fake-first", _}
    assert_receive {:fake_ai_decide, "fake-first", _}
    assert_receive {:game_updated, 5}, 2_000
  end

  test "a provider that never answers falls back to random after the deadline" do
    {game, human} = start_game(["fake-timeout", "fake-first"])

    assert_receive {:game_updated, 2}, 2_000
    by_seat = Map.new(rows(game), &{&1.seat, &1.source})
    assert by_seat == %{1 => "random", 2 => "ai"}
    # The hung provider task was killed before the random move was submitted.
    assert Task.Supervisor.children(Helios.AI.TaskSupervisor) == []

    assert {:ok, %{random: random}} = Games.snapshot(game.id, human.id)
    assert random == MapSet.new([{"ai-1", 1, 1, :regular}])
  end

  test "errors and invalid answers fall back to random" do
    {game, _human} = start_game(["fake-error", "fake-invalid"])
    assert_receive {:game_updated, 2}, 2_000
    assert Enum.map(rows(game), & &1.source) == ["random", "random"]
  end

  test "an unready or unknown model plays random without calling anyone" do
    {game, _human} = start_game(["fake-down", "removed-model"])
    assert_receive {:game_updated, 2}, 2_000
    refute_received {:fake_ai_decide, _, _}
    assert Enum.map(rows(game), & &1.source) == ["random", "random"]
  end

  test "AI seats wait during setup (the owner chooses for them)" do
    {game, _human} = start_game(["fake-first", "fake-first"], selection: :choose_side)
    refute_receive {:fake_ai_decide, _, _}, 200
    assert rows(game) == []
  end

  test "AI players stop with their server and come back with it, without repeating a decision" do
    {game, human} = start_game(["fake-first", "fake-first"])
    assert_receive {:game_updated, 2}, 2_000

    refs =
      for {_, pid, _, _} <- DynamicSupervisor.which_children(Helios.AI.Supervisor),
          do: Process.monitor(pid)

    stop_game_servers()
    for ref <- refs, do: assert_receive({:DOWN, ^ref, :process, _, _})

    {:ok, _pid} = Games.ensure_started(game.id)
    refute_receive {:fake_ai_decide, _, _}, 200

    {:ok, view} = Games.view(game.id, human.id)
    :ok = Games.submit(game.id, human.id, {:discard, hd(view.hand).name})
    assert_receive {:fake_ai_decide, _, _}
    assert_receive {:fake_ai_decide, _, _}
  end

  test "a disabled config starts no AI players" do
    put_ai_config(players: false)
    {_game, _human} = start_game(["fake-first", "fake-first"])
    refute_receive {:fake_ai_decide, _, _}, 200
  end
end
```

Create `helios/test/helios/ai/full_game_test.exs`:

```elixir
defmodule Helios.AI.FullGameTest do
  use Helios.DataCase, async: false

  import Helios.GamesFixtures

  alias Helios.{GameDriver, Games}

  @moduletag timeout: 180_000

  setup do
    on_exit(&stop_game_servers/0)
    :ok
  end

  test "one human and two AI seats play a whole game" do
    human = player_fixture()
    wonders = [{"Gizah", :a}, {"Babylon", :b}, {"Halikarnassós", :b}]
    game = game_fixture([human, {:ai, "fake-first"}, {:ai, "fake-last"}], wonders: wonders)
    Phoenix.PubSub.subscribe(Helios.PubSub, Games.topic(game.id))
    {:ok, _pid} = Games.ensure_started(game.id)

    assert %{phase: %{kind: :game_over}} = drive(game.id, human.id, 600)
  end

  # Plays the human (discard, or a build from the discard pile) whenever they
  # must act; otherwise waits for the AI seats to move.
  defp drive(_game_id, _user_id, 0), do: flunk("the game did not finish")

  defp drive(game_id, user_id, steps) do
    {:ok, view} = Games.view(game_id, user_id)

    cond do
      view.phase.kind == :game_over ->
        view

      must_act?(view) ->
        :ok = Games.submit(game_id, user_id, GameDriver.choose(view, :discard))
        drive(game_id, user_id, steps - 1)

      true ->
        assert_receive {:game_updated, _}, 5_000
        drive(game_id, user_id, steps - 1)
    end
  end

  defp must_act?(%{phase: %{kind: :choosing_cards}, me: me, submitted: submitted}),
    do: List.keyfind(submitted, me, 0) == {me, false}

  defp must_act?(%{phase: %{kind: :extra_turn, extra_turn_player: me}, me: me}), do: true
  defp must_act?(_view), do: false
end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios/ai/player_test.exs test/helios/ai/full_game_test.exs`
Expected: FAIL (`Helios.AI.Player` is undefined; no AI players are started).

- [ ] **Step 3: Implement the player**

Create `helios/lib/helios/ai/player.ex`:

```elixir
defmodule Helios.AI.Player do
  @moduledoc """
  One AI seat of one running game. Its `GameServer` starts it and it stops
  with the server (monitor). It behaves like a browser: after every
  `{:game_updated, _}` it fetches its own view and, when a decision is
  pending that it has not handled yet, asks its model under the model's
  `deadline_ms` (in a `Helios.AI.TaskSupervisor` task, hard stop at deadline +
  #{250} ms). A valid answer is submitted as `:ai`; a timeout, error, invalid
  answer or unready model becomes a uniformly random legal move submitted as
  `:random`. One provider call per decision at most; nothing during setup
  (the owner chooses boards for AI seats).
  """
  use GenServer, restart: :temporary

  require Logger

  alias Helios.AI.{Models, Prompt, Readiness}
  alias Helios.Games
  alias Helios.Games.GameServer

  @slack_ms 250

  def start_link(opts), do: GenServer.start_link(__MODULE__, opts)

  @doc "The pending decision of the view's player, or nil."
  @spec decision_key(map()) :: {pos_integer(), pos_integer(), atom()} | nil
  def decision_key(%{phase: %{kind: :choosing_cards} = phase, me: me, submitted: submitted}) do
    if List.keyfind(submitted, me, 0) == {me, false}, do: {phase.age, phase.turn, :regular}
  end

  def decision_key(%{phase: %{kind: :extra_turn, extra_turn_player: me} = phase, me: me}),
    do: {phase.age, phase.turn, phase.extra_turn_kind}

  def decision_key(_view), do: nil

  @impl true
  def init(opts) do
    server = Keyword.fetch!(opts, :server)
    game_id = Keyword.fetch!(opts, :game_id)
    Process.monitor(server)
    Phoenix.PubSub.subscribe(Helios.PubSub, Games.topic(game_id))

    state = %{
      game_id: game_id,
      server: server,
      actor: {:ai, Keyword.fetch!(opts, :seat)},
      model_id: Keyword.fetch!(opts, :model_id),
      done: nil,
      pending: nil
    }

    {:ok, state, {:continue, :look}}
  end

  @impl true
  def handle_continue(:look, state), do: look(state)

  @impl true
  def handle_info({:game_updated, _seq}, state), do: look(state)
  def handle_info({:game_finished}, state), do: stop(state)
  def handle_info({:game_aborted}, state), do: stop(state)

  def handle_info({:DOWN, _ref, :process, server, _reason}, %{server: server} = state),
    do: stop(state)

  def handle_info({ref, result}, %{pending: %{task: %Task{ref: ref}}} = state) do
    Process.demonitor(ref, [:flush])
    {:noreply, answered(state, result)}
  end

  def handle_info({:DOWN, ref, :process, _pid, reason}, %{pending: %{task: %Task{ref: ref}}} = state),
    do: {:noreply, answered(state, {:error, {:crashed, reason}})}

  def handle_info({:deadline, ref}, %{pending: %{task: %Task{ref: ref} = task}} = state) do
    Task.shutdown(task, :brutal_kill)
    {:noreply, answered(state, {:error, :timeout})}
  end

  # Late replies, deadlines and exits of decisions already settled.
  def handle_info(_message, state), do: {:noreply, state}

  defp look(%{pending: %{}} = state), do: {:noreply, state}

  defp look(state) do
    case call(fn -> GameServer.view(state.server, state.actor) end) do
      {:ok, %{phase: %{kind: :game_over}}} -> stop(state)
      {:ok, view} -> {:noreply, maybe_decide(state, view)}
      {:error, _reason} -> {:noreply, state}
    end
  end

  defp maybe_decide(state, view) do
    case decision_key(view) do
      nil -> state
      key when key == state.done -> state
      key -> decide(state, view, key)
    end
  end

  defp decide(state, view, key) do
    prompt = Prompt.build(view)
    started = now()
    meta = %{started: started, usage: %{}}

    case {prompt.moves, ready_model(state.model_id)} do
      {[], _model} ->
        Logger.warning("AI #{state.model_id} in game #{state.game_id} has no legal move")
        %{state | done: key}

      {[only], _model} ->
        submit(state, key, prompt.moves, only, :ai, Map.put(meta, :reason, :single_move))

      {moves, nil} ->
        submit(state, key, moves, Enum.random(moves), :random, Map.put(meta, :reason, :model_unavailable))

      {_moves, model} ->
        provider = Models.provider_module(model)
        deadline = started + model.deadline_ms

        task =
          Task.Supervisor.async_nolink(Helios.AI.TaskSupervisor, fn ->
            provider.decide(model, prompt, deadline)
          end)

        Process.send_after(self(), {:deadline, task.ref}, model.deadline_ms + @slack_ms)
        %{state | pending: %{task: task, key: key, moves: prompt.moves, meta: meta}}
    end
  end

  defp answered(%{pending: pending} = state, result) do
    state = %{state | pending: nil}
    %{key: key, moves: moves, meta: meta} = pending

    with {:ok, id, usage} <- result,
         %{} = move <- Enum.find(moves, &(&1.id == id)) do
      submit(state, key, moves, move, :ai, %{meta | usage: usage} |> Map.put(:reason, nil))
    else
      nil -> submit(state, key, moves, Enum.random(moves), :random, Map.put(meta, :reason, :invalid_move))
      {:error, reason} -> submit(state, key, moves, Enum.random(moves), :random, Map.put(meta, :reason, reason))
    end
  end

  # An AI move the server rejects is replaced by one random move; a rejected
  # random move is logged and the decision is given up (never a loop).
  defp submit(state, key, moves, move, source, meta) do
    choice = Prompt.to_choice(move)
    result = call(fn -> GameServer.submit_choice(state.server, state.actor, choice, source) end)
    report(state, source, meta, result)

    case {result, source} do
      {:ok, _source} ->
        %{state | done: key}

      {{:error, reason}, :ai} ->
        submit(state, key, moves, Enum.random(moves), :random, Map.put(meta, :reason, {:rejected, reason}))

      {{:error, reason}, :random} ->
        Logger.error("AI #{state.model_id} in game #{state.game_id}: random move rejected (#{inspect(reason)})")
        %{state | done: key}
    end
  end

  defp report(state, source, meta, result) do
    latency = now() - meta.started

    :telemetry.execute(
      [:helios, :ai, :decision],
      %{
        latency_ms: latency,
        input_tokens: Map.get(meta.usage, :input_tokens, 0),
        output_tokens: Map.get(meta.usage, :output_tokens, 0)
      },
      %{game_id: state.game_id, model_id: state.model_id, source: source, reason: meta[:reason]}
    )

    Logger.info(
      "AI #{state.model_id} in game #{state.game_id}: #{source} move in #{latency} ms " <>
        "(reason #{inspect(meta[:reason])}, tokens #{inspect(meta.usage)}, #{inspect(result)})"
    )
  end

  defp ready_model(model_id) do
    with {:ok, model} <- Models.fetch(model_id),
         true <- Readiness.ready?(model_id) do
      model
    else
      _ -> nil
    end
  end

  defp call(fun) do
    fun.()
  catch
    :exit, reason -> {:error, {:server_exit, reason}}
  end

  defp stop(%{pending: %{task: task}} = state) do
    Task.shutdown(task, :brutal_kill)
    {:stop, :normal, %{state | pending: nil}}
  end

  defp stop(state), do: {:stop, :normal, state}

  defp now, do: System.monotonic_time(:millisecond)
end
```

- [ ] **Step 4: Start AI players from `GameServer.init/1`**

In `helios/lib/helios/games/game_server.ex`, in `init/1`, replace:

```elixir
        state = maybe_finish(state)
        {:ok, state, state.idle_timeout}
```

with:

```elixir
        state = maybe_finish(state)
        if state.status == "active", do: start_ai_players(game.id)
        {:ok, state, state.idle_timeout}
```

and add the private function:

```elixir
  # One Helios.AI.Player per AI seat; each monitors this server and stops with it.
  defp start_ai_players(game_id) do
    if Keyword.get(Application.get_env(:helios, Helios.AI, []), :players, true) do
      for %{ai_model: model, seat: seat} <- Replay.seats(game_id), is_binary(model) do
        {:ok, _pid} =
          DynamicSupervisor.start_child(
            Helios.AI.Supervisor,
            {Helios.AI.Player, game_id: game_id, seat: seat, model_id: model, server: self()}
          )
      end
    end

    :ok
  end
```

Also mention in the moduledoc: "Starts one `Helios.AI.Player` per AI seat of an active game."

- [ ] **Step 5: Run the tests**

Run: `cd helios && mix test test/helios/ai`
Expected: PASS, including the full game in under about 60 s. If the full game times out, report how far it got (turn and phase) rather than raising the timeout.

Run: `cd helios && mix test`
Expected: the whole suite passes.

- [ ] **Step 6: Precommit and commit**

```bash
git add helios/lib/helios/ai/player.ex helios/lib/helios/games/game_server.ex \
  helios/test/helios/ai/player_test.exs helios/test/helios/ai/full_game_test.exs
git commit -m "feat(ai): AI seat players with deadline, random fallback and telemetry

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```
