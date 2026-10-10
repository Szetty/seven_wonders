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
end
