defmodule Helios.Games.ReplayTest do
  use Helios.DataCase, async: false

  import Helios.GamesFixtures

  alias Helios.{GameDriver, Games, Repo}
  alias Helios.Games.{Game, GameAction, Replay}

  setup do
    on_exit(&stop_game_servers/0)
    users = for _ <- 1..3, do: player_fixture()
    game = game_fixture(users)
    ids = Enum.map(users, &to_string(&1.id))
    {view_fun, submit_fun} = GameDriver.games_funs(game.id)
    {:game_over, live} = GameDriver.play_until(ids, %{}, view_fun, submit_fun, fn _ -> false end)
    %{game: Repo.get!(Game, game.id), users: users, ids: ids, live: live}
  end

  test "rebuild replays every row and reports each step", %{game: game} do
    rows = Replay.actions(game.id)

    assert {:ok, _ref, count} =
             Replay.rebuild(game, rows, fn _ref -> 0 end, fn _row, _ref, n -> n + 1 end)

    assert count == length(rows)
  end

  test "replay/2 rebuilds the viewer's final view without a GameServer", %{
    game: game,
    users: [a | _],
    live: live
  } do
    stop_game_servers()
    assert game.status == "finished"
    assert {:ok, %{view: view, history: [{:turn, _} | _] = history}} = Games.replay(game, a.id)
    assert {:battles, %{age: 3}} = List.last(history)
    assert view.phase.kind == :game_over
    assert view.me == to_string(a.id)
    assert view.scores == live.scores
    assert Enum.map(view.players, & &1.built) == Enum.map(live.players, & &1.built)
    assert Registry.lookup(Helios.Games.Registry, game.id) == []
  end

  test "a log that no longer replays is an error and leaves the game untouched", %{
    game: game,
    users: [a | _]
  } do
    Repo.insert!(%GameAction{
      game_id: game.id,
      seq: 10_000,
      user_id: a.id,
      action: %{"type" => "discard", "card" => "Not A Card"}
    })

    assert {:error, {:replay_failed, 10_000, _}} = Games.replay(game, a.id)
    assert Repo.get!(Game, game.id).status == "finished"
  end

  test "frames: one per turn plus the final table, with only my hand and my choice", %{
    game: game,
    users: [a | _],
    ids: [ia | _]
  } do
    assert {:ok, %{frames: frames, history: history}} = Games.replay(game, a.id)
    turns = Enum.count(history, &match?({:turn, _}, &1))
    assert length(frames) == turns + 1

    [first | _] = frames
    assert %{phase: %{age: 1, turn: 1, kind: :choosing_cards}, event_index: 0} = first
    assert length(first.hand) == 7
    assert first.my_choice == {:discard, hd(first.hand)}

    final = List.last(frames)
    assert %{phase: %{kind: :game_over}, event_index: nil, my_choice: nil, hand: []} = final

    for frame <- frames, frame.phase.kind == :choosing_cards do
      assert length(frame.hand) == 8 - frame.phase.turn
    end

    for frame <- Enum.drop(frames, -1) do
      assert {:turn, %{}} = Enum.at(history, frame.event_index)
    end

    assert Enum.all?(frames, fn frame -> Enum.any?(frame.players, &(&1.name == ia)) end)
  end

  describe "a game where Halikarnassós builds from the discard pile" do
    setup do
      users = for _ <- 1..3, do: player_fixture()
      %{discard_game: discard_build_game_fixture(users), discard_users: users}
    end

    test "only the chooser's own discard frame carries the discard pile", %{
      discard_game: game,
      discard_users: [chooser | others]
    } do
      assert {:ok, %{frames: frames, view: final}} = Games.replay(game, chooser.id)

      discard_frames = Enum.filter(frames, &is_list(&1.discard_pile))
      assert discard_frames != []

      for frame <- discard_frames do
        assert %{kind: :extra_turn, extra_turn_kind: :build_from_discard} = frame.phase
        assert frame.phase.extra_turn_player == to_string(chooser.id)
        assert frame.discard_pile != []
        assert frame.discard_pile == Enum.sort(Enum.uniq(frame.discard_pile))
        assert {:build_from_discard, card} = frame.my_choice
        assert card in frame.discard_pile
        assert frame.hand == []
      end

      for frame <- frames, frame.phase.kind != :extra_turn do
        assert frame.discard_pile == nil
      end

      assert Enum.all?(frames, &(is_integer(&1.discard_count) and &1.discard_count >= 0))
      assert hd(frames).discard_count == 0
      assert List.last(frames).discard_count == final.discard_count

      for other <- others do
        assert {:ok, %{frames: other_frames}} = Games.replay(game, other.id)
        assert length(other_frames) == length(frames)
        assert Enum.all?(other_frames, &is_nil(&1.discard_pile))
        assert Enum.map(other_frames, & &1.discard_count) == Enum.map(frames, & &1.discard_count)
      end
    end
  end

  test "a choose_board game's frames start at Age I turn 1; setup is nobody's choice" do
    users = for _ <- 1..3, do: player_fixture()
    [a | _] = users
    game = game_fixture(users, selection: :choose_board)
    ids = Enum.map(users, &to_string(&1.id))
    {view_fun, submit_fun} = GameDriver.games_funs(game.id)

    :ok = GameDriver.finish_setup(ids, view_fun, submit_fun)
    {:game_over, _} = GameDriver.play_until(ids, %{}, view_fun, submit_fun, fn _ -> false end)
    game = Repo.get!(Game, game.id)
    stop_game_servers()

    assert {:ok, %{frames: frames, history: history}} = Games.replay(game, a.id)
    turns = Enum.count(history, &match?({:turn, _}, &1))
    assert length(frames) == turns + 1

    [first | _] = frames
    assert %{phase: %{kind: :choosing_cards, age: 1, turn: 1}, event_index: 0} = first
    assert length(first.hand) == 7
    assert Enum.all?(first.players, &(&1.side == :a))
    assert first.my_choice == {:discard, hd(first.hand)}

    refute Enum.any?(frames, &(&1.phase.kind == :setup))

    refute Enum.any?(frames, fn frame ->
             match?({:claim_wonder, _}, frame.my_choice) or frame.my_choice == :confirm_wonder
           end)
  end
end
