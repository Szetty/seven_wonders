defmodule Helios.CoreShapesTest do
  use ExUnit.Case, async: true

  alias Helios.Core

  @players ["1", "2", "3"]
  @wonders {:explicit, [{"Gizah", :a}, {"Rhódos", :a}, {"Éphesos", :a}]}

  defp build_option?(:free), do: true
  defp build_option?({:coins, n}) when is_integer(n) and n > 0, do: true
  defp build_option?({:unavailable, reason}) when is_atom(reason), do: true

  defp build_option?({:trade, options}) when is_list(options) and options != [] do
    Enum.all?(
      options,
      &match?(
        %{payment: %{west: w, east: e}, west_coins: _, east_coins: _, bank_coins: _}
        when is_list(w) and is_list(e),
        &1
      )
    )
  end

  defp build_option?({:chain, from}) when is_binary(from), do: true

  defp build_option?(_), do: false

  test "game_settings/0" do
    settings = Core.game_settings()
    assert is_integer(settings.engine_version)
    assert length(settings.wonders) == 7

    assert Enum.all?(
             settings.wonders,
             &match?(%{name: name, sides: [:a, :b]} when is_binary(name), &1)
           )

    assert %{name: name, category: category, age: age} = hd(settings.cards)
    assert is_binary(name) and is_atom(category) and age in 1..3

    altar = Enum.find(settings.cards, &(&1.name == "Altar"))
    assert %{cost: %{coins: 0, resources: []}, chains_from: [], chains_to: ["Temple"]} = altar
    assert [{:points, %{category: :civilian, points: 2}}] = altar.effects

    forum = Enum.find(settings.cards, &(&1.name == "Forum"))

    assert [{:produces, %{resources: [:loom, :glass, :papyrus], choice: true, tradable: false}}] =
             forum.effects

    gizah = Enum.find(settings.wonders, &(&1.name == "Gizah"))
    assert [%{side: :a, stages: [_, _, _]}, %{side: :b, stages: [_, _, _, _]}] = gizah.boards

    assert %{
             cost: %{coins: 0, resources: [{:wood, 3}]},
             effects: [{:points, %{category: :wonder, points: 5}}]
           } =
             Enum.at(hd(gizah.boards).stages, 1)

    babylon_b = Enum.find(settings.wonders, &(&1.name == "Babylon")).boards |> Enum.at(1)
    assert [:play_last_card] = Enum.at(babylon_b.stages, 1).effects
  end

  test "view/2 at the start of a game" do
    {:ok, ref} = Core.new_game(@players, @wonders, 1)
    assert {:ok, view} = Core.view(ref, "1")

    assert %{me: "1", west: "3", east: "2", discard_pile: nil, discard_count: 0} = view
    assert %{my_pending: nil, scores: nil} = view

    assert %{
             kind: :choosing_cards,
             age: 1,
             turn: 1,
             direction: :west,
             extra_turn_player: nil,
             extra_turn_kind: nil
           } = view.phase

    assert [{"1", false}, {"2", false}, {"3", false}] = view.submitted

    assert [
             %{
               name: "1",
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

    assert length(view.hand) == 7

    for card <- view.hand do
      assert %{name: name, category: category, age: 1, free_build: false} = card
      assert is_binary(name) and is_atom(category)
      assert build_option?(card.build), "unexpected build option #{inspect(card.build)}"

      assert build_option?(card.wonder_stage),
             "unexpected wonder option #{inspect(card.wonder_stage)}"
    end
  end

  test "submit/3 records my pending action and rejects bad cards" do
    {:ok, ref} = Core.new_game(@players, @wonders, 1)
    {:ok, view} = Core.view(ref, "1")
    card = hd(view.hand).name

    assert :ok = Core.submit(ref, "1", {:discard, card})

    assert {:ok, %{my_pending: {:discard, ^card}, submitted: [{"1", true} | _]}} =
             Core.view(ref, "1")

    assert {:error, :card_not_in_hand} = Core.submit(ref, "1", {:discard, "Not A Card"})
    assert {:error, :unknown_player} = Core.view(ref, "9")
  end

  test "a discard-only game reaches game over with final scores" do
    {:ok, ref} = Core.new_game(@players, @wonders, 1)

    final =
      Enum.reduce_while(1..40, nil, fn _, _ ->
        {:ok, view} = Core.view(ref, "1")

        if view.phase.kind == :game_over do
          {:halt, view}
        else
          Enum.each(@players, fn player ->
            {:ok, player_view} = Core.view(ref, player)

            case player_view.hand do
              [card | _] -> :ok = Core.submit(ref, player, {:discard, card.name})
              [] -> :ok
            end
          end)

          {:cont, nil}
        end
      end)

    assert %{phase: %{kind: :game_over}, scores: [_, _, _] = scores} = final

    for score <- scores do
      assert %{
               player: _,
               military: _,
               treasury: _,
               wonder: _,
               civilian: _,
               scientific: _,
               commercial: _,
               guild: _,
               total: _,
               coins: _,
               rank: _
             } = score
    end
  end

  test "history/1 lists public turn and battle events" do
    {:ok, ref} = Core.new_game(@players, @wonders, 1)
    assert {:ok, []} = Core.history(ref)

    for _turn <- 1..6, player <- @players do
      {:ok, view} = Core.view(ref, player)
      :ok = Core.submit(ref, player, {:discard, hd(view.hand).name})
    end

    assert {:ok, events} = Core.history(ref)
    assert [{:turn, first} | _] = events

    assert %{
             age: 1,
             turn: 1,
             kind: :regular,
             plays: [play | _],
             coins: [%{player: "1", delta: 3} | _]
           } = first

    assert %{player: "1", action: :discarded, coins_to_west: 0, coins_to_east: 0} = play
    assert {:battles, %{age: 1, battles: [battle, _, _]}} = List.last(events)

    assert %{
             player: "1",
             rival: "2",
             player_strength: 0,
             rival_strength: 0,
             player_token: nil,
             rival_token: nil
           } = battle
  end
end
