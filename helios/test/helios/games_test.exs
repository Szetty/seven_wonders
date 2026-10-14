defmodule Helios.GamesTest do
  use Helios.DataCase, async: false

  import Helios.GamesFixtures

  alias Helios.{Games, Lobbies}
  alias Helios.Accounts.Scope
  alias Helios.Games.{Game, Replay}

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
      assert lobby.wonder_mode == "random"

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

    test "choose_side starts in setup with drawn boards and ignores the wonders override", %{
      owner: owner,
      guests: [b, c | _],
      lobby: lobby
    } do
      put_games_config(wonders: default_wonders())
      connect_to_lobby(lobby, [b, c])
      {:ok, lobby} = Lobbies.set_wonder_mode(Scope.for_user(owner), lobby, "choose_side")

      assert {:ok, game} = Games.start_game(Scope.for_user(owner), lobby)
      assert game.wonders == %{"mode" => "choose_side"}

      {:ok, view} = Games.view(game.id, owner.id)
      assert %{phase: %{kind: :setup}, players: [], setup: %{mode: :choose_side}} = view
      assert view.setup.available == []
      assert length(view.setup.seats) == 3

      assert Enum.all?(view.setup.seats, fn seat ->
               is_binary(seat.wonder) and is_nil(seat.side) and not seat.confirmed
             end)
    end

    test "choose_board starts in setup with every board free", %{
      owner: owner,
      guests: [b, c | _],
      lobby: lobby
    } do
      put_games_config(wonders: default_wonders())
      connect_to_lobby(lobby, [b, c])
      {:ok, lobby} = Lobbies.set_wonder_mode(Scope.for_user(owner), lobby, "choose_board")

      assert {:ok, game} = Games.start_game(Scope.for_user(owner), lobby)
      assert game.wonders == %{"mode" => "choose_board"}

      {:ok, view} = Games.view(game.id, owner.id)
      assert %{phase: %{kind: :setup}, setup: %{mode: :choose_board}} = view
      assert length(view.setup.available) == 7
      assert Enum.all?(view.setup.seats, &is_nil(&1.wonder))
    end

    test "setup actions are logged and survive a server restart", %{
      owner: owner,
      guests: [b, c | _],
      lobby: lobby
    } do
      connect_to_lobby(lobby, [b, c])
      {:ok, lobby} = Lobbies.set_wonder_mode(Scope.for_user(owner), lobby, "choose_board")
      {:ok, game} = Games.start_game(Scope.for_user(owner), lobby)

      assert :ok =
               Games.submit(game.id, owner.id, {:claim_wonder, %{wonder: "Babylon", side: :b}})

      assert {:error, :wonder_taken} =
               Games.submit(game.id, b.id, {:claim_wonder, %{wonder: "Babylon", side: :a}})

      assert :ok = Games.submit(game.id, owner.id, :confirm_wonder)

      assert [
               %{"type" => "claim_wonder", "wonder" => "Babylon", "side" => "b"},
               %{"type" => "confirm_wonder"}
             ] = Enum.map(Replay.actions(game.id), & &1.action)

      stop_game_servers()
      {:ok, view} = Games.view(game.id, b.id)
      owner_id = to_string(owner.id)

      assert %{wonder: "Babylon", side: :b, confirmed: true} =
               Enum.find(view.setup.seats, &(&1.player == owner_id))

      refute "Babylon" in view.setup.available
    end

    test "once every seat confirms, Age I is dealt", %{
      owner: owner,
      guests: [b, c | _],
      lobby: lobby
    } do
      connect_to_lobby(lobby, [b, c])
      {:ok, lobby} = Lobbies.set_wonder_mode(Scope.for_user(owner), lobby, "choose_side")
      {:ok, game} = Games.start_game(Scope.for_user(owner), lobby)
      ids = Enum.map([owner, b, c], &to_string(&1.id))
      {view_fun, submit_fun} = Helios.GameDriver.games_funs(game.id)

      assert :ok = Helios.GameDriver.finish_setup(ids, view_fun, submit_fun)

      view = view_fun.(hd(ids))
      assert %{phase: %{kind: :choosing_cards, age: 1, turn: 1}, setup: nil} = view
      assert length(view.hand) == 7
      assert Enum.all?(view.players, &(&1.side == :a))
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

  describe "AI seats" do
    setup do
      on_exit(&stop_game_servers/0)
      put_ai_config(players: false)
      :ok
    end

    test "put_ai_config(players: false) is in effect during these tests" do
      config = Helios.AI.Models.config()
      assert Keyword.get(config, :players) == false
      assert Keyword.get(config, :models) != []
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
            :unknown_wonder,
            :wonder_taken,
            :not_your_wonder,
            :no_claim,
            :already_confirmed,
            {:setup_failed, :anything}
          ] do
        message = Games.error_message(reason)
        assert is_binary(message) and message != fallback, "no copy for #{inspect(reason)}"
      end

      assert Games.error_message(:cannot_afford) == "You can't afford that"
      assert Games.error_message(:invalid_payment) == "That payment is no longer valid"
      assert Games.error_message(:game_in_progress) == "A game is already running at this table"
      assert Games.error_message(:not_your_wonder) == "That is not your board"
      assert Games.error_message(:no_claim) == "Pick a board first"
    end
  end

  describe "finished_games_for/2" do
    test "lists only my finished games, newest first" do
      [me, b, c] = users = for _ <- 1..3, do: player_fixture()
      lobby = Helios.Lobbies.get_or_create_own_lobby(me)

      finish = fn finished_at ->
        game = game_fixture(users, lobby: lobby)

        game
        |> Ecto.Changeset.change(
          status: "finished",
          finished_at: finished_at,
          final_scores: %{"scores" => []}
        )
        |> Repo.update!()
      end

      old = finish.(~U[2026-01-01 10:00:00Z])
      new = finish.(~U[2026-02-01 10:00:00Z])

      aborted =
        game_fixture(users, lobby: lobby)
        |> Ecto.Changeset.change(status: "aborted")
        |> Repo.update!()

      _active = game_fixture(users, lobby: lobby)

      [x, y, z] = for _ <- 1..3, do: player_fixture()
      other_lobby = Helios.Lobbies.get_or_create_own_lobby(x)

      game_fixture([x, y, z], lobby: other_lobby)
      |> Ecto.Changeset.change(status: "finished", finished_at: ~U[2026-03-01 10:00:00Z])
      |> Repo.update!()

      assert Enum.map(Games.finished_games_for(me), & &1.id) == [new.id, old.id]
      refute aborted.id in Enum.map(Games.finished_games_for(me), & &1.id)
      assert [%{players: [%{user: %{name: _}} | _]} | _] = Games.finished_games_for(me)
      assert length(Games.finished_games_for(me, 1)) == 1
      assert Games.finished_games_for(b) |> length() == 2
      assert Games.finished_games_for(c) |> length() == 2
    end
  end
end
