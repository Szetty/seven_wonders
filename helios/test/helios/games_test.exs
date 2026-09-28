defmodule Helios.GamesTest do
  use Helios.DataCase, async: false

  import Helios.GamesFixtures

  alias Helios.{Games, Lobbies}
  alias Helios.Accounts.Scope
  alias Helios.Games.Game

  setup do
    on_exit(&stop_game_servers/0)
    put_games_config(fixed_seed: 42, wonders: nil)
    owner = player_fixture("own")
    guests = for _ <- 1..3, do: player_fixture("g")
    lobby = Lobbies.get_or_create_own_lobby(owner)
    :ok = invite_all(owner, lobby, guests)
    %{owner: owner, guests: guests, lobby: lobby}
  end

  describe "start_game/2" do
    test "only the lobby owner can start", %{guests: [b, c | _], lobby: lobby} do
      connect_to_lobby(lobby, [b, c])
      assert {:error, :not_leader} = Games.start_game(Scope.for_user(b), lobby)
    end

    test "needs at least 3 connected players", %{owner: owner, guests: [b | _], lobby: lobby} do
      connect_to_lobby(lobby, [b])
      assert {:error, :not_enough_players} = Games.start_game(Scope.for_user(owner), lobby)
      assert Games.active_game_for_lobby(lobby.id) == nil
    end

    test "seats the owner and connected invitees in members order and broadcasts",
         %{owner: owner, guests: [b, c, d], lobby: lobby} do
      connect_to_lobby(lobby, [b, c])
      Phoenix.PubSub.subscribe(Helios.PubSub, "lobby:#{lobby.id}")

      assert {:ok, %Game{} = game} = Games.start_game(Scope.for_user(owner), lobby)
      game_id = game.id
      assert_receive {:game_started, ^game_id}

      expected =
        lobby
        |> Lobbies.members()
        |> Enum.map(& &1.user.id)
        |> Enum.filter(&(&1 in [owner.id, b.id, c.id]))

      assert Enum.map(Games.players(game), & &1.user_id) == expected
      assert Enum.map(Games.players(game), & &1.seat) == [0, 1, 2]
      refute Games.seated?(game, d.id)

      assert game.seed == 42
      assert game.status == "active"
      assert game.wonders == nil
      assert game.engine_version == Helios.Core.game_settings().engine_version
      assert Games.active_game_for_lobby(lobby.id).id == game.id
      assert {:ok, _view} = Games.view(game.id, owner.id)
    end

    test "allows only one active game per lobby", %{
      owner: owner,
      guests: [b, c | _],
      lobby: lobby
    } do
      connect_to_lobby(lobby, [b, c])
      assert {:ok, game} = Games.start_game(Scope.for_user(owner), lobby)
      assert {:error, :game_in_progress} = Games.start_game(Scope.for_user(owner), lobby)

      stop_game_servers()
      Repo.update!(Game.finish_changeset(game, %{"scores" => []}))
      assert {:ok, _next} = Games.start_game(Scope.for_user(owner), lobby)
    end

    test "uses the configured explicit wonders and persists them", %{
      owner: owner,
      guests: [b, c | _],
      lobby: lobby
    } do
      put_games_config(
        wonders: [{"Gizah", :b}, {"Rhódos", :a}, {"Éphesos", :b}, {"Alexandria", :a}]
      )

      connect_to_lobby(lobby, [b, c])

      assert {:ok, game} = Games.start_game(Scope.for_user(owner), lobby)
      assert game.wonders == %{"explicit" => [["Gizah", "b"], ["Rhódos", "a"], ["Éphesos", "b"]]}

      {:ok, view} = Games.view(game.id, owner.id)

      assert Enum.map(view.players, &{&1.wonder, &1.side}) == [
               {"Gizah", :b},
               {"Rhódos", :a},
               {"Éphesos", :b}
             ]
    end

    test "an engine setup rejection leaves no game behind", %{
      owner: owner,
      guests: [b, c | _],
      lobby: lobby
    } do
      put_games_config(wonders: [{"Atlantis", :a}, {"Rhódos", :a}, {"Éphesos", :a}])
      connect_to_lobby(lobby, [b, c])

      assert {:error, {:setup_failed, _reason}} = Games.start_game(Scope.for_user(owner), lobby)
      assert Games.active_game_for_lobby(lobby.id) == nil
    end
  end

  describe "eligible_players/1" do
    test "the owner always counts; invitees only while connected", %{
      owner: owner,
      guests: [b | _],
      lobby: lobby
    } do
      assert Enum.map(Games.eligible_players(lobby), & &1.id) == [owner.id]
      connect_to_lobby(lobby, [b])
      assert Enum.map(Games.eligible_players(lobby), & &1.id) == [owner.id, b.id]
    end
  end

  describe "start_blocker/2" do
    test "explains why a game cannot start" do
      assert Games.start_blocker(nil, 2) == "Need at least 3 connected players"
      assert Games.start_blocker(nil, 8) == "At most 7 players can play"
      assert Games.start_blocker(%Game{}, 3) == "A game is already running at this table"
      assert Games.start_blocker(nil, 3) == nil
      assert Games.start_blocker(nil, 7) == nil
    end
  end

  describe "error_message/1" do
    test "has specific copy for every engine and context error" do
      fallback = Games.error_message(:something_else)

      for reason <- [
            :unknown_player,
            :not_your_turn,
            :game_over,
            :card_not_in_hand,
            :card_not_in_discard,
            :already_built,
            :cannot_afford,
            :invalid_payment,
            :no_wonder_stage_left,
            :free_build_unavailable,
            :action_not_allowed_now,
            :not_leader,
            :not_enough_players,
            :too_many_players,
            :game_in_progress,
            :not_found,
            :not_active,
            :aborted,
            :unauthorized,
            :server_restarted,
            :invalid_choice,
            {:setup_failed, :anything}
          ] do
        message = Games.error_message(reason)
        assert is_binary(message) and message != fallback, "no copy for #{inspect(reason)}"
      end

      assert Games.error_message(:cannot_afford) == "You can't afford that"
      assert Games.error_message(:invalid_payment) == "That payment is no longer valid"
      assert Games.error_message(:game_in_progress) == "A game is already running at this table"
    end
  end
end
