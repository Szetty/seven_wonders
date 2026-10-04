defmodule HeliosWeb.CardTextTest do
  use ExUnit.Case, async: true

  alias Helios.Catalogue
  alias HeliosWeb.CardText

  test "single effects" do
    assert CardText.effect(
             {:produces, %{resources: [:wood, :wood], choice: false, tradable: true}},
             %{}
           ) ==
             "Produces 2 wood"

    assert CardText.effect(
             {:produces, %{resources: [:wood, :clay], choice: true, tradable: true}},
             %{}
           ) ==
             "Produces wood or clay (one per turn)"

    assert CardText.effect(
             {:produces, %{resources: [:loom, :glass, :papyrus], choice: true, tradable: false}},
             %{}
           ) ==
             "Produces loom, glass or papyrus (one per turn, only for you)"

    assert CardText.effect({:points, %{category: :civilian, points: 3}}, %{}) ==
             "Worth 3 victory points"

    assert CardText.effect({:military, 2}, %{}) == "+2 military strength"

    assert CardText.effect({:science, %{symbols: [:gears], choice: false}}, %{}) ==
             "Science symbol: gears"

    assert CardText.effect({:science, %{symbols: [:compass, :gears, :tablet], choice: true}}, %{}) ==
             "One science symbol of your choice: compass, gears or tablet"

    assert CardText.effect({:coins, 5}, %{}) == "Gain 5 coins (once, when built)"

    assert CardText.effect(
             {:trade_discount, %{directions: [:east], resources: [:wood, :stone, :ore, :clay]}},
             %{east: "Cid"}
           ) == "Buy wood, stone, ore or clay from Cid for 1 coin"

    assert CardText.effect(
             {:trade_discount,
              %{directions: [:east, :west], resources: [:loom, :glass, :papyrus]}},
             %{}
           ) == "Buy loom, glass or papyrus from both neighbours for 1 coin"

    assert CardText.effect(
             {:coins_per_card,
              %{directions: [:east, :west, :own], categories: [:raw_material], coins: 1}},
             %{}
           ) ==
             "Gain 1 coin per brown card in your city and your neighbours' cities (once, when built)"

    assert CardText.effect(
             {:points_per_card,
              %{
                category: :guild,
                directions: [:own],
                categories: [:raw_material, :manufactured_good, :guild],
                points: 1
              }},
             %{}
           ) == "1 victory point per brown, grey or purple card in your city (at game end)"

    assert CardText.effect(
             {:points_per_defeat, %{category: :guild, directions: [:east, :west], points: 1}},
             %{}
           ) ==
             "1 victory point per defeat token in your neighbours' cities (at game end)"

    assert CardText.effect(:play_last_card, %{}) ==
             "Play the last card of each age instead of discarding it"
  end

  test "costs" do
    assert CardText.cost(%{coins: 0, resources: []}) == "Free"
    assert CardText.cost(%{coins: 1, resources: []}) == "1 coin"
    assert CardText.cost(%{coins: 0, resources: [{:wood, 1}, {:clay, 2}]}) == "1 wood, 2 clay"
  end

  test "every card and every wonder stage in the catalogue renders" do
    for card <- Catalogue.cards() do
      lines = CardText.effects(card.effects, %{})
      assert lines != [] and Enum.all?(lines, &(is_binary(&1) and &1 != "")), card.name
      assert is_binary(CardText.cost(card.cost))
    end

    for {{wonder, side}, board} <- Catalogue.boards(), stage <- board.stages do
      assert [_ | _] = CardText.effects(stage.effects, %{}), "#{wonder} #{side}"
    end
  end
end
