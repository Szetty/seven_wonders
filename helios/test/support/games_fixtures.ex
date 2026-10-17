defmodule Helios.GamesFixtures do
  @moduledoc "Test helpers for games: users with session tokens, lobbies, games, config overrides."

  alias Helios.{Accounts, Lobbies, Repo}
  alias Helios.Accounts.Scope
  alias Helios.Games.{ActionCodec, Game, GamePlayer}

  @default_wonders [
    {"Gizah", :a},
    {"Rhódos", :a},
    {"Éphesos", :a},
    {"Alexandria", :a},
    {"Babylon", :a},
    {"Olympía", :a},
    {"Halikarnassós", :a}
  ]

  def default_wonders, do: @default_wonders

  def user_with_token_fixture(prefix \\ "p") do
    name = "#{prefix}#{System.unique_integer([:positive])}"
    {:ok, user, token} = Accounts.login(Application.fetch_env!(:helios, :access_token), name)
    {user, token}
  end

  def player_fixture(prefix \\ "p") do
    {user, _token} = user_with_token_fixture(prefix)
    user
  end

  def invite_all(owner, lobby, guests) do
    Enum.each(guests, fn guest ->
      {:ok, _invite} = Lobbies.invite(Scope.for_user(owner), lobby, guest.id)
    end)
  end

  @doc "Marks users as connected to the lobby (tracked by the calling test process)."
  def connect_to_lobby(lobby, users) do
    Enum.each(users, fn user ->
      {:ok, _ref} =
        HeliosWeb.Presence.track(self(), "lobby:#{lobby.id}", user.id, %{name: user.name})
    end)
  end

  @doc """
  Inserts an active game directly (no GameServer is started). `users` may
  contain `{:ai, model_id}` entries (any position except the first). opts:
  :lobby, :wonders, :seed, :selection (:choose_board | :choose_side | {:explicit, …}).
  """
  def game_fixture(users, opts \\ []) do
    lobby = Keyword.get_lazy(opts, :lobby, fn -> Lobbies.get_or_create_own_lobby(hd(users)) end)
    wonders = opts |> Keyword.get(:wonders, @default_wonders) |> Enum.take(length(users))
    selection = Keyword.get(opts, :selection, {:explicit, wonders})

    game =
      Repo.insert!(%Game{
        lobby_id: lobby.id,
        seed: Keyword.get(opts, :seed, 1),
        engine_version: Helios.Core.game_settings().engine_version,
        status: "active",
        wonders: ActionCodec.encode_wonders(selection)
      })

    users
    |> Enum.with_index()
    |> Enum.each(fn
      {{:ai, model_id}, seat} ->
        Repo.insert!(%GamePlayer{game_id: game.id, user_id: nil, ai_model: model_id, seat: seat})

      {user, seat} ->
        Repo.insert!(%GamePlayer{game_id: game.id, user_id: user.id, seat: seat})
    end)

    game
  end

  @discard_wonders [{"Halikarnassós", :b}, {"Gizah", :a}, {"Rhódos", :a}]

  @doc """
  A finished 3-player game in which the first user (Halikarnassós B) built
  from the discard pile at least once. The seed is searched in a throwaway
  engine, then the game is persisted and played to the end through
  `Helios.Games`, so its action log replays like any real game.
  """
  def discard_build_game_fixture([_, _, _] = users) do
    [first | rest] = ids = Enum.map(users, &to_string(&1.id))
    strategies = Map.new([{first, :wonder} | Enum.map(rest, &{&1, :build})])
    stop? = &match?(%{phase: %{kind: :extra_turn, extra_turn_kind: :build_from_discard}}, &1)
    seed = Helios.GameDriver.find_seed(ids, @discard_wonders, strategies, stop?)
    game = game_fixture(users, seed: seed, wonders: @discard_wonders)
    {view_fun, submit_fun} = Helios.GameDriver.games_funs(game.id)

    {:game_over, _view} =
      Helios.GameDriver.play_until(ids, strategies, view_fun, submit_fun, fn _ -> false end)

    Repo.get!(Game, game.id)
  end

  @doc "Terminates every GameServer. Register with `on_exit/1` so it runs before the sandbox stops."
  def stop_game_servers do
    Helios.Games.Supervisor
    |> DynamicSupervisor.which_children()
    |> Enum.each(fn
      {_, pid, _, _} when is_pid(pid) ->
        DynamicSupervisor.terminate_child(Helios.Games.Supervisor, pid)

      _other ->
        :ok
    end)
  end

  @doc "Merges `overrides` into `config :helios, Helios.Games` for the current test."
  def put_games_config(overrides) do
    original = Application.get_env(:helios, Helios.Games, [])
    Application.put_env(:helios, Helios.Games, Keyword.merge(original, overrides))
    ExUnit.Callbacks.on_exit(fn -> Application.put_env(:helios, Helios.Games, original) end)
  end

  @doc "Merges `overrides` into `config :helios, Helios.AI` for the current test (e.g. `players: false`)."
  def put_ai_config(overrides) do
    original = Application.get_env(:helios, Helios.AI, [])
    Application.put_env(:helios, Helios.AI, Keyword.merge(original, overrides))
    ExUnit.Callbacks.on_exit(fn -> Application.put_env(:helios, Helios.AI, original) end)
  end

  def log_in_conn(conn, token), do: Phoenix.ConnTest.init_test_session(conn, %{user_token: token})
end
