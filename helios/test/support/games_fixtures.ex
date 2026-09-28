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

  @doc "Inserts an active game directly (no GameServer is started)."
  def game_fixture(users, opts \\ []) do
    lobby = Keyword.get_lazy(opts, :lobby, fn -> Lobbies.get_or_create_own_lobby(hd(users)) end)
    wonders = opts |> Keyword.get(:wonders, @default_wonders) |> Enum.take(length(users))

    game =
      Repo.insert!(%Game{
        lobby_id: lobby.id,
        seed: Keyword.get(opts, :seed, 1),
        engine_version: Helios.Core.game_settings().engine_version,
        status: "active",
        wonders: ActionCodec.encode_wonders({:explicit, wonders})
      })

    users
    |> Enum.with_index()
    |> Enum.each(fn {user, seat} ->
      Repo.insert!(%GamePlayer{game_id: game.id, user_id: user.id, seat: seat})
    end)

    game
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

  def log_in_conn(conn, token), do: Phoenix.ConnTest.init_test_session(conn, %{user_token: token})
end
