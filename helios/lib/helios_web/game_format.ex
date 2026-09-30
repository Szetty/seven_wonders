defmodule HeliosWeb.GameFormat do
  @moduledoc "Pure presentation helpers for the game table: labels, colours, grouping."

  alias Helios.Games.Choice

  @category_order [
    :raw_material,
    :manufactured_good,
    :civilian,
    :commercial,
    :military,
    :scientific,
    :guild
  ]

  def category_order, do: @category_order

  def roman(1), do: "I"
  def roman(2), do: "II"
  def roman(3), do: "III"

  def direction_label(:west), do: "west"
  def direction_label(:east), do: "east"

  def direction_icon(:west), do: "hero-arrow-left"
  def direction_icon(:east), do: "hero-arrow-right"

  def side_label(:a), do: "A"
  def side_label(:b), do: "B"

  def player_name(names, id), do: Map.get(names, id, id)

  def category_class(:raw_material), do: "bg-amber-800"
  def category_class(:manufactured_good), do: "bg-zinc-400"
  def category_class(:civilian), do: "bg-blue-600"
  def category_class(:commercial), do: "bg-yellow-500"
  def category_class(:military), do: "bg-red-600"
  def category_class(:scientific), do: "bg-green-600"
  def category_class(:guild), do: "bg-purple-700"

  def category_ring(:raw_material), do: "ring-amber-800"
  def category_ring(:manufactured_good), do: "ring-zinc-400"
  def category_ring(:civilian), do: "ring-blue-600"
  def category_ring(:commercial), do: "ring-yellow-500"
  def category_ring(:military), do: "ring-red-600"
  def category_ring(:scientific), do: "ring-green-600"
  def category_ring(:guild), do: "ring-purple-700"

  def category_label(:raw_material), do: "Raw materials"
  def category_label(:manufactured_good), do: "Manufactured goods"
  def category_label(:civilian), do: "Civilian"
  def category_label(:commercial), do: "Commercial"
  def category_label(:military), do: "Military"
  def category_label(:scientific), do: "Scientific"
  def category_label(:guild), do: "Guilds"

  def group_by_category(built) do
    groups = Enum.group_by(built, & &1.category)

    @category_order
    |> Enum.map(&{&1, Map.get(groups, &1, [])})
    |> Enum.reject(fn {_category, cards} -> cards == [] end)
  end

  def category_counts(built) do
    built
    |> group_by_category()
    |> Enum.map(fn {category, cards} -> {category, length(cards)} end)
  end

  defdelegate available?(option), to: Choice

  def option_label(:free), do: "Free"
  def option_label({:coins, 1}), do: "Pay 1 coin"
  def option_label({:coins, n}), do: "Pay #{n} coins"

  def option_label(%{west_coins: west, east_coins: east, bank_coins: bank}),
    do: "West #{west} · East #{east} · Bank #{bank}"

  def action_card(nil), do: nil
  def action_card({_type, %{card: card}}), do: card
  def action_card({_type, card}) when is_binary(card), do: card

  def describe_action({:build, %{card: card}}), do: "Build #{card}"

  def describe_action({:build_wonder_stage, %{card: card}}),
    do: "Build a wonder stage with #{card}"

  def describe_action({:discard, card}), do: "Discard #{card}"
  def describe_action({:build_free, card}), do: "Build #{card} for free"
  def describe_action({:build_from_discard, card}), do: "Build #{card} from the discard pile"

  def extra_turn_label(:build_from_discard), do: "building from the discard pile"
  def extra_turn_label(:play_last_card), do: "playing their last card"

  def waiting_for(%{phase: %{kind: :game_over}}, _names), do: []

  def waiting_for(%{phase: %{kind: :extra_turn, extra_turn_player: player}}, names),
    do: [player_name(names, player)]

  def waiting_for(view, names), do: for({id, false} <- view.submitted, do: player_name(names, id))

  def show_hand?(%{hand: []}), do: false

  def show_hand?(%{phase: %{kind: :extra_turn, extra_turn_player: player}, me: me}),
    do: player == me

  def show_hand?(_view), do: true

  def dock?(%{phase: %{kind: :extra_turn}}), do: true
  def dock?(view), do: show_hand?(view)
end
