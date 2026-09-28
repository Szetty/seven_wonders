defmodule Helios.Games.ChoiceTest do
  use ExUnit.Case, async: true

  alias Helios.Games.Choice
  alias Helios.SampleViews

  @empty %{west: [], east: []}

  setup do
    %{view: SampleViews.view()}
  end

  describe "parse/1" do
    test "accepts known kinds with an integer option (default 0)" do
      assert Choice.parse(%{"card" => "Baths", "kind" => "wonder_stage", "option" => "1"}) ==
               {:ok, %{card: "Baths", kind: "wonder_stage", option: 1}}

      assert Choice.parse(%{"card" => "Baths", "kind" => "discard", "value" => ""}) ==
               {:ok, %{card: "Baths", kind: "discard", option: 0}}
    end

    test "rejects unknown kinds, negative or non-numeric options and missing cards" do
      for params <- [
            %{"card" => "Baths", "kind" => "steal"},
            %{"card" => "Baths", "kind" => "build", "option" => "-1"},
            %{"card" => "Baths", "kind" => "build", "option" => "1x"},
            %{"kind" => "build"},
            %{}
          ] do
        assert Choice.parse(params) == {:error, :invalid_choice}, "accepted #{inspect(params)}"
      end
    end
  end

  describe "resolve/2" do
    test "a trade option index resolves to the server-side payment", %{view: view} do
      assert Choice.resolve(view, %{card: "Baths", kind: "wonder_stage", option: 1}) ==
               {:ok, {:build_wonder_stage, %{card: "Baths", payment: %{west: [], east: [{:stone, 1}]}}}}
    end

    test "free and coin-only builds use an empty payment", %{view: view} do
      assert Choice.resolve(view, %{card: "Tavern", kind: "build", option: 0}) ==
               {:ok, {:build, %{card: "Tavern", payment: @empty}}}

      assert Choice.resolve(view, %{card: "Tree Farm", kind: "build", option: 3}) ==
               {:ok, {:build, %{card: "Tree Farm", payment: @empty}}}
    end

    test "unavailable options return the engine's reason", %{view: view} do
      assert Choice.resolve(view, %{card: "Baths", kind: "build", option: 0}) == {:error, :cannot_afford}

      assert Choice.resolve(view, %{card: "Tavern", kind: "wonder_stage", option: 0}) ==
               {:error, :no_wonder_stage_left}
    end

    test "an out-of-range option is an invalid payment", %{view: view} do
      assert Choice.resolve(view, %{card: "Baths", kind: "wonder_stage", option: 5}) ==
               {:error, :invalid_payment}
    end

    test "paid builds of cards not in hand are rejected", %{view: view} do
      assert Choice.resolve(view, %{card: "Palace", kind: "build", option: 0}) == {:error, :card_not_in_hand}
    end

    test "discard, free builds and discard-pile builds pass the card to the engine", %{view: view} do
      assert Choice.resolve(view, %{card: "Baths", kind: "discard", option: 0}) == {:ok, {:discard, "Baths"}}
      assert Choice.resolve(view, %{card: "Tavern", kind: "build_free", option: 0}) == {:ok, {:build_free, "Tavern"}}

      assert Choice.resolve(view, %{card: "Library", kind: "build_from_discard", option: 0}) ==
               {:ok, {:build_from_discard, "Library"}}
    end

    test "unknown kinds are invalid", %{view: view} do
      assert Choice.resolve(view, %{card: "Baths", kind: "steal", option: 0}) == {:error, :invalid_choice}
    end
  end

  test "available?/1" do
    assert Choice.available?(:free)
    assert Choice.available?({:coins, 2})
    assert Choice.available?({:trade, [%{payment: @empty, west_coins: 0, east_coins: 0, bank_coins: 0}]})
    refute Choice.available?({:trade, []})
    refute Choice.available?({:unavailable, :cannot_afford})
  end
end
