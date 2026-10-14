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
          phase: %{
            kind: :extra_turn,
            age: 1,
            turn: 6,
            direction: :west,
            extra_turn_player: player,
            extra_turn_kind: :play_last_card
          }
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

    for _ <- 1..3,
        do: Phoenix.PubSub.broadcast(Helios.PubSub, Games.topic(game.id), {:game_updated, 99})

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
    # Drain the first turn's provider calls; the refute below must only see new ones.
    assert_receive {:fake_ai_decide, _, _}
    assert_receive {:fake_ai_decide, _, _}

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
