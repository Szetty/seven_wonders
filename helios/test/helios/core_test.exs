defmodule Helios.CoreTest do
  use ExUnit.Case, async: true

  alias Helios.Core

  @three ["a", "b", "c"]
  @plain_wonders {:explicit, [{"Gizah", :a}, {"Rhódos", :a}, {"Éphesos", :a}]}
  @empty_payment %{west: [], east: []}

  describe "game_settings/0" do
    test "exposes the engine version, every wonder and every card" do
      settings = Core.game_settings()
      assert settings.engine_version == 1
      assert length(settings.wonders) == 7
      assert %{name: "Halikarnassós", sides: [:a, :b]} in settings.wonders
      assert length(settings.cards) == 78
      assert %{name: "Altar", category: :civilian, age: 1} in settings.cards
      assert %{name: "Loom", category: :manufactured_good, age: 2} in settings.cards
    end
  end

  describe "new_game/3" do
    test "starts a game with random wonders" do
      assert {:ok, game} = Core.new_game(@three, :random, 42)
      assert is_reference(game)
    end

    test "starts a game with explicit wonders in seat order" do
      {:ok, game} = Core.new_game(@three, @plain_wonders, 1)
      {:ok, view} = Core.view(game, "a")

      assert Enum.map(view.players, &{&1.name, &1.wonder, &1.side}) ==
               [{"a", "Gizah", :a}, {"b", "Rhódos", :a}, {"c", "Éphesos", :a}]
    end

    test "accepts the full u64 seed range and rejects anything outside it" do
      assert {:ok, _} = Core.new_game(@three, :random, 0)
      assert {:ok, _} = Core.new_game(@three, :random, 18_446_744_073_709_551_615)

      assert_raise FunctionClauseError, fn ->
        Core.new_game(@three, :random, 18_446_744_073_709_551_616)
      end

      assert_raise FunctionClauseError, fn -> Core.new_game(@three, :random, -1) end
    end

    test "reports setup errors" do
      assert Core.new_game(["a", "b"], :random, 1) == {:error, :invalid_players_number}

      assert Core.new_game(Enum.map(1..8, &"p#{&1}"), :random, 1) ==
               {:error, :invalid_players_number}

      assert Core.new_game(["a", "b", "a"], :random, 1) == {:error, {:duplicate_player, "a"}}

      assert Core.new_game(@three, {:explicit, [{"Gizah", :a}]}, 1) ==
               {:error, {:wonders_length_mismatch, %{players: 3, wonders: 1}}}

      assert Core.new_game(
               @three,
               {:explicit, [{"Gizah", :a}, {"Rhodos", :a}, {"Éphesos", :a}]},
               1
             ) ==
               {:error, {:invalid_wonder, "Rhodos"}}

      assert Core.new_game(
               @three,
               {:explicit, [{"Gizah", :a}, {"Gizah", :b}, {"Éphesos", :a}]},
               1
             ) ==
               {:error, {:duplicate_wonder, "Gizah"}}
    end
  end

  describe "submit/3 and view/2" do
    setup do
      {:ok, game} = Core.new_game(@three, @plain_wonders, 7)
      %{game: game}
    end

    test "the initial view has the documented shape", %{game: game} do
      assert {:ok, view} = Core.view(game, "a")
      assert view.me == "a"
      assert view.west == "c"
      assert view.east == "b"

      assert view.phase == %{
               kind: :choosing_cards,
               age: 1,
               turn: 1,
               direction: :west,
               extra_turn_player: nil,
               extra_turn_kind: nil
             }

      assert length(view.hand) == 7
      assert view.discard_pile == nil
      assert view.discard_count == 0
      assert view.submitted == [{"a", false}, {"b", false}, {"c", false}]
      assert view.my_pending == nil
      assert view.scores == nil

      assert [%{name: _, category: _, age: 1, build: _, wonder_stage: _, free_build: false} | _] =
               view.hand

      assert [
               %{
                 name: "a",
                 wonder: "Gizah",
                 side: :a,
                 stages_built: 0,
                 stages_total: 3,
                 built: [],
                 coins: 3,
                 shields: 0,
                 military_tokens: [],
                 free_build_available: false
               }
               | _
             ] = view.players
    end

    test "everyone discarding advances the turn", %{game: game} do
      for player <- @three do
        {:ok, %{hand: [card | _]}} = Core.view(game, player)
        assert Core.submit(game, player, {:discard, card.name}) == :ok
      end

      {:ok, view} = Core.view(game, "a")
      assert %{kind: :choosing_cards, turn: 2} = view.phase
      assert length(view.hand) == 6
      assert view.discard_count == 3
      assert Enum.all?(view.players, &(&1.coins == 6))
    end

    test "a pending choice is visible to its owner only", %{game: game} do
      {:ok, %{hand: [card | _]}} = Core.view(game, "a")
      :ok = Core.submit(game, "a", {:discard, card.name})

      assert {:ok, %{my_pending: {:discard, name}}} = Core.view(game, "a")
      assert name == card.name
      assert {:ok, %{my_pending: nil, submitted: [{"a", true} | _]}} = Core.view(game, "b")
    end

    test "errors come back as atoms", %{game: game} do
      assert Core.view(game, "zed") == {:error, :unknown_player}
      assert Core.submit(game, "zed", {:discard, "Altar"}) == {:error, :unknown_player}
      assert Core.submit(game, "a", {:discard, "Not A Card"}) == {:error, :card_not_in_hand}

      assert Core.submit(game, "a", {:build_from_discard, "Altar"}) ==
               {:error, :action_not_allowed_now}

      assert Core.submit(game, "a", {:build_free, "Altar"}) == {:error, :free_build_unavailable}
    end

    test "malformed action terms raise instead of crashing the VM", %{game: game} do
      assert_raise ArgumentError, fn -> Core.submit(game, "a", {:build, "Altar"}) end

      assert_raise ArgumentError, fn ->
        Core.submit(
          game,
          "a",
          {:build, %{card: "Altar", payment: %{west: [{:gold, 1}], east: []}}}
        )
      end

      assert_raise ArgumentError, fn -> Core.submit(game, "a", :discard) end
      # The game is still usable afterwards.
      assert {:ok, %{phase: %{turn: 1}}} = Core.view(game, "a")
    end

    test "debug_game/1 returns decoded JSON", %{game: game} do
      assert {:ok, %{"seats" => ["a", "b", "c"], "state" => %{"player_states" => states}}} =
               Core.debug_game(game)

      assert map_size(states) == 3
    end
  end

  describe "full random games through the NIF" do
    for players <- [3, 7] do
      @players players
      test "#{players} players reach game over" do
        names = Enum.map(1..@players, &"p#{&1}")
        {:ok, game} = Core.new_game(names, :random, 1_000 + @players)
        :rand.seed(:exsss, {@players, 2, 3})

        view = play_until_over(game, names, 0)

        assert length(view.scores) == @players
        assert Enum.sort(Enum.map(view.scores, & &1.player)) == Enum.sort(names)
        assert Enum.any?(view.scores, &(&1.rank == 1))
      end
    end
  end

  defp play_until_over(_game, _names, rounds) when rounds > 1_000,
    do: flunk("game did not finish")

  defp play_until_over(game, [first | _] = names, rounds) do
    {:ok, view} = Core.view(game, first)

    if view.phase.kind == :game_over do
      view
    else
      for {player, false} <- view.submitted do
        {:ok, player_view} = Core.view(game, player)
        action = player_view |> available_actions() |> Enum.random()
        assert Core.submit(game, player, action) == :ok
      end

      play_until_over(game, names, rounds + 1)
    end
  end

  defp available_actions(view) do
    from_discard = for name <- view.discard_pile || [], do: {:build_from_discard, name}

    from_hand =
      Enum.flat_map(view.hand, fn card ->
        options(card.build, &{:build, %{card: card.name, payment: &1}}) ++
          options(card.wonder_stage, &{:build_wonder_stage, %{card: card.name, payment: &1}}) ++
          if(card.free_build, do: [{:build_free, card.name}], else: []) ++
          [{:discard, card.name}]
      end)

    from_discard ++ from_hand
  end

  defp options({:unavailable, _reason}, _make), do: []
  defp options(:free, make), do: [make.(@empty_payment)]
  defp options({:coins, _}, make), do: [make.(@empty_payment)]
  defp options({:trade, choices}, make), do: Enum.map(choices, &make.(&1.payment))
end
