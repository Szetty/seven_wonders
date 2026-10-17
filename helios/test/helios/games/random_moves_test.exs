defmodule Helios.Games.RandomMovesTest do
  use ExUnit.Case, async: true

  alias Helios.{Core, GameDriver}
  alias Helios.Games.RandomMoves

  test "record keeps only random sources" do
    phase = %{kind: :choosing_cards, age: 2, turn: 4, extra_turn_kind: nil}

    set =
      RandomMoves.new()
      |> RandomMoves.record(:ai, phase, "ai-1")
      |> RandomMoves.record(:random, phase, "ai-2")

    assert set == MapSet.new([{"ai-2", 2, 4, :regular}])
    assert RandomMoves.random?(set, %{age: 2, turn: 4, kind: :regular}, "ai-2")
    refute RandomMoves.random?(set, %{age: 2, turn: 4, kind: :regular}, "ai-1")
    assert RandomMoves.record(set, :random, %{kind: :setup, age: 1, turn: 1}, "ai-1") == set
  end

  # The key is taken from the phase just before the submit; it must equal the
  # fields of the history event that turn produces, for extra turns too.
  for {kind, wonder} <- [
        build_from_discard: {"Halikarnassós", :b},
        play_last_card: {"Babylon", :b}
      ] do
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

      action =
        GameDriver.choose(view_fun.(player), :build) ||
          GameDriver.choose(view_fun.(player), :discard)

      :ok = submit_fun.(player, action)

      {:ok, history} = Core.history(ref)
      {:turn, event} = history |> Enum.filter(&match?({:turn, _}, &1)) |> List.last()
      assert key == {player, event.age, event.turn, event.kind}
      assert Enum.any?(event.plays, &(&1.player == player))
    end
  end
end
