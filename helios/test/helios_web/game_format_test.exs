defmodule HeliosWeb.GameFormatTest do
  use ExUnit.Case, async: true

  alias Helios.SampleViews
  alias HeliosWeb.GameFormat

  test "labels" do
    assert Enum.map(1..3, &GameFormat.roman/1) == ["I", "II", "III"]
    assert GameFormat.direction_label(:west) == "west"
    assert GameFormat.direction_icon(:east) == "hero-arrow-right"
    assert GameFormat.side_label(:b) == "B"
    assert GameFormat.player_name(%{"1" => "Ann"}, "1") == "Ann"
    assert GameFormat.player_name(%{}, "9") == "9"
  end

  test "groups built cards by colour in a fixed order, skipping empty colours" do
    built = SampleViews.player("1").built

    assert [{:raw_material, [_]}, {:civilian, [_]}, {:military, [_]}] =
             GameFormat.group_by_category(built)

    assert GameFormat.category_counts(built) == [raw_material: 1, civilian: 1, military: 1]
    assert GameFormat.category_class(:scientific) == "bg-green-600"
    assert GameFormat.category_label(:manufactured_good) == "Manufactured goods"
  end

  test "option labels name the neighbours and skip the bank" do
    neighbours = %{west: "Bob", east: "Cid"}
    assert GameFormat.option_label(:free, neighbours) == "Free"
    assert GameFormat.option_label({:chain, "Altar"}, neighbours) == "Free (you have Altar)"
    assert GameFormat.option_label({:coins, 1}, neighbours) == "Pay 1 coin"
    assert GameFormat.option_label({:coins, 3}, neighbours) == "Pay 3 coins"

    assert GameFormat.option_label(
             %{west_coins: 2, east_coins: 1, bank_coins: 1, payment: %{}},
             neighbours
           ) == "Bob 2 · Cid 1"

    assert GameFormat.option_label(
             %{west_coins: 0, east_coins: 2, bank_coins: 0, payment: %{}},
             neighbours
           ) == "Cid 2"
  end

  test "cheapest way to build" do
    trade =
      {:trade,
       [
         %{west_coins: 2, east_coins: 2, bank_coins: 1, payment: %{}},
         %{west_coins: 1, east_coins: 0, bank_coins: 1, payment: %{}}
       ]}

    assert GameFormat.cheapest(:free) == :free
    assert GameFormat.cheapest({:chain, "Altar"}) == {:chain, "Altar"}
    assert GameFormat.cheapest({:coins, 1}) == {:coins, 1}
    assert GameFormat.cheapest(trade) == {:coins, 2}
    assert GameFormat.cheapest({:unavailable, :cannot_afford}) == {:unavailable, :cannot_afford}

    assert GameFormat.cheapest_label({:chain, "Altar"}) == "Free (you have Altar)"
    assert GameFormat.cheapest_label(trade) == "2 coins"
    assert GameFormat.cheapest_label({:coins, 1}) == "1 coin"

    assert GameFormat.cheapest_label({:unavailable, :already_built}) ==
             "You already built that structure"
  end

  test "card cost is the bank part of a trade, shown once" do
    assert GameFormat.card_cost({:trade, [%{bank_coins: 1}, %{bank_coins: 1}]}) == 1
    assert GameFormat.card_cost({:trade, [%{bank_coins: 0}]}) == nil
    assert GameFormat.card_cost(:free) == nil
    assert GameFormat.card_cost({:coins, 2}) == nil
  end

  test "describes pending actions" do
    assert GameFormat.describe_action({:discard, "Baths"}) == "Discard Baths"

    assert GameFormat.describe_action({:build, %{card: "Altar", payment: %{west: [], east: []}}}) ==
             "Build Altar"

    assert GameFormat.describe_action(
             {:build_wonder_stage, %{card: "Altar", payment: %{west: [], east: []}}}
           ) ==
             "Build a wonder stage with Altar"

    assert GameFormat.action_card({:build, %{card: "Altar", payment: %{west: [], east: []}}}) ==
             "Altar"

    assert GameFormat.action_card({:discard, "Baths"}) == "Baths"
    assert GameFormat.action_card(nil) == nil
  end

  test "waiting_for lists unsubmitted players, or only the extra-turn player" do
    names = SampleViews.names()
    assert GameFormat.waiting_for(SampleViews.view(), names) == ["Bob", "Dee"]

    extra =
      SampleViews.view(%{
        phase: %{
          SampleViews.view().phase
          | kind: :extra_turn,
            extra_turn_player: "3",
            extra_turn_kind: :build_from_discard
        }
      })

    assert GameFormat.waiting_for(extra, names) == ["Cid"]

    over = SampleViews.view(%{phase: %{SampleViews.view().phase | kind: :game_over}})
    assert GameFormat.waiting_for(over, names) == []
  end

  test "show_hand? hides the hand during someone else's extra turn" do
    view = SampleViews.view()
    assert GameFormat.show_hand?(view)
    refute GameFormat.show_hand?(%{view | hand: []})

    refute GameFormat.show_hand?(%{
             view
             | phase: %{view.phase | kind: :extra_turn, extra_turn_player: "2"}
           })

    assert GameFormat.show_hand?(%{
             view
             | phase: %{view.phase | kind: :extra_turn, extra_turn_player: "1"}
           })
  end

  test "dock? shows the dock for a hand or an extra turn, never at game over" do
    view = SampleViews.view()
    assert GameFormat.dock?(view)
    refute GameFormat.dock?(%{view | hand: []})

    assert GameFormat.dock?(%{
             view
             | hand: [],
               phase: %{view.phase | kind: :extra_turn, extra_turn_player: "2"}
           })

    refute GameFormat.dock?(%{view | hand: [], phase: %{view.phase | kind: :game_over}})
  end

  describe "player_status/2" do
    test "choosing cards: chosen when submitted, thinking otherwise" do
      view = Helios.SampleViews.view()
      assert GameFormat.player_status(view, "1") == :chosen
      assert GameFormat.player_status(view, "2") == :thinking
      assert GameFormat.player_status(view, "9") == nil
    end

    test "extra turn: only the acting player is playing" do
      view = Helios.SampleViews.view()
      extra = %{view | phase: %{view.phase | kind: :extra_turn, extra_turn_player: "3"}}
      assert GameFormat.player_status(extra, "3") == :playing
      assert GameFormat.player_status(extra, "1") == nil
    end

    test "game over: nobody has a status" do
      view = Helios.SampleViews.view()
      over = %{view | phase: %{view.phase | kind: :game_over}}
      assert GameFormat.statuses(over) == %{"1" => nil, "2" => nil, "3" => nil, "4" => nil}
    end
  end
end
