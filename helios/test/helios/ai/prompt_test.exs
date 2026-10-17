defmodule Helios.AI.PromptTest do
  use ExUnit.Case, async: true

  alias Helios.AI.Prompt
  alias Helios.SampleViews

  defp ids(prompt), do: Enum.map(prompt.moves, & &1.id)
  defp move(prompt, id), do: Enum.find(prompt.moves, &(&1.id == id))

  describe "moves" do
    test "only legal moves, per hand card: build, wonder stage, free build, discard" do
      prompt = Prompt.build(SampleViews.view())

      # Baths: build unavailable, stage by trade. Tavern: free + Olympia free build.
      # Tree Farm: 1 coin. Discard is always legal.
      assert ids(prompt) == ~w(w0 d0 b1 f1 d1 b2 d2)

      assert move(prompt, "w0") == %{
               id: "w0",
               label: "build wonder stage 2 with Baths: trade, from 2 coins",
               card: "Baths",
               kind: "wonder_stage",
               trade_options: 2
             }

      assert move(prompt, "b1").label == "build Tavern: free"

      assert move(prompt, "f1") |> Map.take([:kind, :label]) ==
               %{kind: "build_free", label: "build Tavern free (once per age)"}

      assert move(prompt, "b2").label == "build Tree Farm: 1 coin"

      assert move(prompt, "d2") |> Map.take([:kind, :label]) ==
               %{kind: "discard", label: "discard Tree Farm: +3 coins"}
    end

    test "the trade label shows the cheapest total, never payments" do
      options = [
        %{payment: %{west: [{:ore, 2}], east: []}, west_coins: 4, east_coins: 0, bank_coins: 1},
        %{payment: %{west: [], east: [{:ore, 2}]}, west_coins: 0, east_coins: 2, bank_coins: 1}
      ]

      [baths | rest] = SampleViews.hand()
      view = SampleViews.view(%{hand: [%{baths | wonder_stage: {:trade, options}} | rest]})
      prompt = Prompt.build(view)

      assert move(prompt, "w0").label =~ "trade, from 3 coins"
      encoded = Jason.encode!(prompt)
      refute encoded =~ "west_coins"
      refute encoded =~ "payment"
    end

    test "chain builds say so" do
      [baths | rest] = SampleViews.hand()
      view = SampleViews.view(%{hand: [%{baths | build: {:chain, "Altar"}} | rest]})
      assert move(Prompt.build(view), "b0").label == "build Baths: free (chain from Altar)"
    end

    test "build from discard offers only the pile, free" do
      view =
        SampleViews.view(%{
          phase: %{
            kind: :extra_turn,
            age: 1,
            turn: 6,
            direction: :west,
            extra_turn_player: "1",
            extra_turn_kind: :build_from_discard
          },
          hand: [],
          discard_pile: ["Altar", "Baths"]
        })

      prompt = Prompt.build(view)
      assert ids(prompt) == ~w(x0 x1)

      assert move(prompt, "x1") == %{
               id: "x1",
               label: "build Baths from the discard pile, free",
               card: "Baths",
               kind: "build_from_discard",
               trade_options: 0
             }

      assert Enum.map(prompt.state.discard_pile, & &1.name) == ["Altar", "Baths"]
    end

    test "play last card offers the single card's moves" do
      [_baths, tavern, _farm] = SampleViews.hand()

      view =
        SampleViews.view(%{
          phase: %{
            kind: :extra_turn,
            age: 1,
            turn: 6,
            direction: :west,
            extra_turn_player: "1",
            extra_turn_kind: :play_last_card
          },
          hand: [tavern]
        })

      assert ids(Prompt.build(view)) == ~w(b0 f0 d0)
    end
  end

  describe "to_choice/1" do
    test "non-trade moves use option 0" do
      prompt = Prompt.build(SampleViews.view())

      assert Prompt.to_choice(move(prompt, "b2")) ==
               %{card: "Tree Farm", kind: "build", option: 0}

      assert Prompt.to_choice(move(prompt, "d0")) == %{card: "Baths", kind: "discard", option: 0}
    end

    test "trades pick a random payment index inside the engine's list" do
      trade = move(Prompt.build(SampleViews.view()), "w0")
      options = for _ <- 1..60, into: MapSet.new(), do: Prompt.to_choice(trade).option
      assert options == MapSet.new([0, 1])
    end
  end

  describe "state" do
    test "compact boards for me and my neighbours, summaries for the others" do
      state = Prompt.build(SampleViews.view()).state

      assert %{age: 2, turn: 3, pass: "east", discard_count: 2} = state
      assert state.me.wonder == "Gizah A"
      assert state.me.stages == "1/3"
      assert state.me.coins == 4
      assert state.me.built == ["Lumber Yard", "Altar", "Stockade"]
      assert "wood" in state.me.produces
      assert state.me.shields == 1
      assert state.me.military_tokens == [1, -1]
      assert is_binary(state.me.next_stage)
      assert state.east.wonder == "Rhódos B"
      assert state.west.wonder == "Halikarnassós B"

      assert [%{wonder: "Éphesos A", stages: "1/3", coins: 4, shields: 1, cards: cards}] =
               state.others

      assert cards == "1 blue, 1 brown, 1 red"
    end

    test "hand cards are flat rows: colour, effect text and chains as strings" do
      [baths, tavern, farm] = Prompt.build(SampleViews.view()).state.hand
      assert %{name: "Baths", colour: "blue", effects: baths_text, chains_to: chains} = baths
      assert baths_text =~ "victory point"
      assert is_binary(chains)
      assert tavern.colour == "yellow"
      assert farm.colour == "brown"

      assert Enum.all?(
               [baths, tavern, farm],
               &(Map.keys(&1) |> Enum.sort() == [:chains_to, :colour, :effects, :name])
             )
    end

    test "is JSON and small" do
      json = Jason.encode!(Prompt.build(SampleViews.view()).state)
      assert byte_size(json) < 3_000
    end

    test "omits extra_turn outside extra turns" do
      refute Map.has_key?(Prompt.build(SampleViews.view()).state, :extra_turn)
    end
  end

  describe "user_message/2" do
    test "lists the moves after the state, in either format" do
      prompt = Prompt.build(SampleViews.view())

      for format <- [:json, :toon] do
        assert Prompt.user_message(prompt, format) =~
                 "\nMoves:\nw0: build wonder stage 2 with Baths: trade, from 2 coins\nd0: discard Baths: +3 coins\n"
      end
    end

    test "TOON writes the hand as a table and is shorter than JSON" do
      prompt = Prompt.build(SampleViews.view())
      toon = Prompt.user_message(prompt, :toon)
      json = Prompt.user_message(prompt, :json)

      assert "State (TOON):\n" <> _ = toon
      assert toon =~ ~r/hand\[3\]\{[a-z_,]+\}:/
      assert "State (JSON):\n" <> _ = json
      assert byte_size(toon) < byte_size(json)
    end

    test "TOON round-trips" do
      prompt = Prompt.build(SampleViews.view())
      "State (TOON):\n" <> rest = Prompt.user_message(prompt, :toon)
      [body, _moves] = String.split(rest, "\nMoves:\n", parts: 2)
      assert Toon.decode!(body) == prompt.state |> Jason.encode!() |> Jason.decode!()
    end
  end

  test "the primer is a short fixed persona" do
    assert Prompt.primer() =~ "7 Wonders"
    assert String.length(Prompt.primer()) < 400
    assert Prompt.build(SampleViews.view()).primer == Prompt.primer()
  end
end
