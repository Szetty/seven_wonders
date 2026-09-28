defmodule Helios.Games.GameServerTest do
  use Helios.DataCase, async: false

  import ExUnit.CaptureLog
  import Helios.GamesFixtures

  alias Helios.{GameDriver, Games}
  alias Helios.Games.{ActionCodec, Game, GameAction}

  setup do
    on_exit(&stop_game_servers/0)
    users = for _ <- 1..3, do: player_fixture()
    game = game_fixture(users)
    %{game: game, users: users, ids: Enum.map(users, &to_string(&1.id))}
  end

  defp action_count(game),
    do: Repo.aggregate(from(a in GameAction, where: a.game_id == ^game.id), :count)

  defp hand_names(game, user) do
    {:ok, view} = Games.view(game.id, user.id)
    Enum.map(view.hand, & &1.name)
  end

  defp views(game, users) do
    Map.new(users, fn user ->
      {:ok, view} = Games.view(game.id, user.id)
      {user.id, view}
    end)
  end

  describe "submit/3" do
    test "persists accepted actions with increasing seq and broadcasts each", %{game: game, users: users} do
      Phoenix.PubSub.subscribe(Helios.PubSub, Games.topic(game.id))

      for user <- users do
        [card | _] = hand_names(game, user)
        assert :ok = Games.submit(game.id, user.id, {:discard, card})
      end

      assert_receive {:game_updated, 1}
      assert_receive {:game_updated, 2}
      assert_receive {:game_updated, 3}

      rows = Repo.all(from a in GameAction, where: a.game_id == ^game.id, order_by: a.seq)
      assert Enum.map(rows, & &1.seq) == [1, 2, 3]
      assert Enum.map(rows, & &1.user_id) == Enum.map(users, & &1.id)
      assert Enum.all?(rows, &match?(%{"type" => "discard", "card" => _}, &1.action))

      {:ok, view} = Games.view(game.id, hd(users).id)
      assert view.phase.turn == 2
    end

    test "engine errors are returned and nothing is persisted", %{game: game, users: [a | _]} do
      assert {:error, :card_not_in_hand} = Games.submit(game.id, a.id, {:discard, "Not A Card"})
      assert action_count(game) == 0
    end

    test "users who are not seated are unknown to the engine", %{game: game} do
      outsider = player_fixture()
      assert {:error, :unknown_player} = Games.view(game.id, outsider.id)
    end
  end

  describe "submit_choice/3" do
    test "resolves the choice against the server's current view", %{game: game, users: [a | _]} do
      [card | _] = hand_names(game, a)
      assert :ok = Games.submit_choice(game.id, a.id, %{card: card, kind: "discard", option: 0})
      assert {:ok, %{my_pending: {:discard, ^card}}} = Games.view(game.id, a.id)

      assert {:error, :card_not_in_hand} =
               Games.submit_choice(game.id, a.id, %{card: "Not A Card", kind: "build", option: 0})

      assert action_count(game) == 1
    end
  end

  describe "ensure_started/1" do
    test "reports missing, finished and malformed games", %{game: game} do
      assert {:error, :not_found} = Games.ensure_started(Ecto.UUID.generate())
      assert {:error, :not_found} = Games.ensure_started("not-a-uuid")

      Repo.update!(Game.finish_changeset(game, %{"scores" => []}))
      assert {:error, :not_active} = Games.ensure_started(game.id)
    end

    test "returns the running process on repeated calls", %{game: game} do
      assert {:ok, pid} = Games.ensure_started(game.id)
      assert {:ok, ^pid} = Games.ensure_started(game.id)
      assert {:ok, ^pid} = Games.ensure_started(String.upcase(game.id))
    end
  end

  describe "durability" do
    test "kill-and-replay rebuilds identical views and ignores later config changes",
         %{game: game, users: [a | _] = users, ids: ids} do
      {view_fun, submit_fun} = GameDriver.games_funs(game.id)
      strategies = Map.new(ids, &{&1, :build})
      for _ <- 1..4, do: GameDriver.step(ids, strategies, view_fun, submit_fun)

      # A pending choice that is changed must survive the replay too.
      [first | _] = names = hand_names(game, a)
      assert :ok = Games.submit(game.id, a.id, {:discard, first})
      assert :ok = Games.submit(game.id, a.id, {:discard, List.last(names)})
      before = views(game, users)

      put_games_config(fixed_seed: 999, wonders: [{"Olympía", :b}, {"Babylon", :b}, {"Alexandria", :b}])

      {:ok, pid} = Games.ensure_started(game.id)
      ref = Process.monitor(pid)
      Process.exit(pid, :kill)
      assert_receive {:DOWN, ^ref, :process, ^pid, :killed}

      assert {:ok, new_pid} = Games.ensure_started(game.id)
      assert new_pid != pid
      assert views(game, users) == before
    end

    @tag :capture_log
    test "a failed insert crashes the server and the log stays authoritative", %{game: game, users: [a | _]} do
      [card | _] = names = hand_names(game, a)
      {:ok, pid} = Games.ensure_started(game.id)

      # Occupy seq 1 behind the server's back so its own insert of seq 1 fails.
      Repo.insert!(%GameAction{game_id: game.id, seq: 1, user_id: a.id, action: ActionCodec.encode({:discard, card})})
      ref = Process.monitor(pid)

      assert {:error, :server_restarted} = Games.submit(game.id, a.id, {:discard, List.last(names)})
      assert_receive {:DOWN, ^ref, :process, ^pid, _reason}

      assert {:ok, %{my_pending: {:discard, ^card}}} = Games.view(game.id, a.id)
    end

    test "stops after the idle timeout and restarts lazily", %{game: game, users: [a | _]} do
      put_games_config(idle_timeout: 50)
      {:ok, pid} = Games.ensure_started(game.id)
      ref = Process.monitor(pid)
      assert_receive {:DOWN, ^ref, :process, ^pid, :normal}, 1_000

      assert {:ok, %{me: me}} = Games.view(game.id, a.id)
      assert me == to_string(a.id)
    end

    @tag :capture_log
    test "a replay error marks the game aborted and notifies", %{game: game, users: [a | _]} do
      Repo.insert!(%GameAction{game_id: game.id, seq: 1, user_id: a.id, action: %{"type" => "discard", "card" => "Not A Card"}})
      Phoenix.PubSub.subscribe(Helios.PubSub, Games.topic(game.id))
      Phoenix.PubSub.subscribe(Helios.PubSub, "lobby:#{game.lobby_id}")

      assert {:error, :aborted} = Games.ensure_started(game.id)
      assert_receive {:game_aborted}
      assert_receive {:members_changed}
      assert %Game{status: "aborted"} = Repo.get!(Game, game.id)
      assert {:error, :aborted} = Games.view(game.id, a.id)
    end

    @tag :capture_log
    test "an undecodable stored action also aborts", %{game: game, users: [a | _]} do
      Repo.insert!(%GameAction{game_id: game.id, seq: 1, user_id: a.id, action: %{"type" => "bogus"}})
      assert {:error, :aborted} = Games.ensure_started(game.id)
    end

    test "replays games from another engine version with a warning", %{game: game} do
      Repo.update!(Ecto.Changeset.change(game, engine_version: 999_999))
      log = capture_log(fn -> assert {:ok, _pid} = Games.ensure_started(game.id) end)
      assert log =~ "engine version"
    end
  end

  test "a completed game is marked finished with final scores", %{game: game, users: [a | _], ids: ids} do
    Phoenix.PubSub.subscribe(Helios.PubSub, Games.topic(game.id))
    Phoenix.PubSub.subscribe(Helios.PubSub, "lobby:#{game.lobby_id}")
    {view_fun, submit_fun} = GameDriver.games_funs(game.id)

    assert {:reached, view} =
             GameDriver.play_until(ids, %{}, view_fun, submit_fun, &(&1.phase.kind == :game_over))

    assert length(view.scores) == 3
    assert_receive {:game_finished}
    assert_receive {:members_changed}

    finished = Repo.get!(Game, game.id)
    assert finished.status == "finished"
    assert %DateTime{} = finished.finished_at
    assert %{"scores" => [_, _, _] = scores} = finished.final_scores
    assert Enum.all?(scores, &(is_integer(&1["total"]) and is_binary(&1["player"])))

    assert {:error, :game_over} = Games.submit(game.id, a.id, {:discard, "Altar"})
  end
end
