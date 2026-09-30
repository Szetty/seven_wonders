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

  test "option labels" do
    assert GameFormat.option_label(:free) == "Free"
    assert GameFormat.option_label({:coins, 1}) == "Pay 1 coin"
    assert GameFormat.option_label({:coins, 3}) == "Pay 3 coins"

    assert GameFormat.option_label(%{
             payment: %{west: [], east: []},
             west_coins: 2,
             east_coins: 1,
             bank_coins: 0
           }) ==
             "West 2 · East 1 · Bank 0"
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
end
